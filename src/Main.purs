module Main where

import Prelude

import Cheerio (attr, find)
import Cheerio as Cheerio
import Cheerio.Static (load, loadRoot, select)
import Control.Monad.Except (class MonadError)
import Data.Array (catMaybes, filter)
import Data.Array as Array
import Data.Either (Either(..), either)
import Data.Foldable (foldMap)
import Data.Maybe (Maybe(..), maybe)
import Data.Options (Options, (:=))
import Data.String (Pattern(..))
import Data.String as String
import Effect (Effect)
import Effect.Aff (Aff, Canceler(..), error, launchAff_, makeAff, throwError)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Effect.Exception (Error)
import Effect.Ref as Ref
import Foreign.Object (singleton, Object)
import Node.Encoding (Encoding(..))
import Node.HTTP.Client (RequestHeaders(..), RequestOptions, headers, hostname, method, path, port, protocol, request, requestAsStream, responseAsStream)
import Node.Stream (Read, Readable, Stream, end)
import Node.Stream as S
import Simple.JSON (readJSON)

main :: Effect Unit
main = do
  launchAff_ do
    url <- getFeedUrl
    log url

chromeAgent :: String
chromeAgent = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/81.0.4044.138 Safari/537.36"


options :: String -> Options RequestOptions
options path_ =
  protocol := "https:" <>
  hostname := "www.stuff.co.nz" <>
  path := path_ <>
  port := 443 <>
  method := "GET" <>
  headers := RequestHeaders (singleton "User-Agent" chromeAgent)

log :: String -> Aff Unit
log = liftEffect <<< Console.log

getFeedUrl ::  Aff String
getFeedUrl =
  getFeedUrlAtEndpoint "/national/quizzes?label=Quizzes"

readToEnd :: forall r. Encoding -> Readable r -> Aff String
readToEnd enc stream = makeAff go
  where
  go cb = do
    result <- Ref.new ""
    S.onDataString stream enc (\s -> Ref.modify_ (_ <> s) result)
    S.onError stream (\e -> cb (Left e))
    S.onEnd stream (do r <- Ref.read result
                       cb (Right r))
    pure mempty
    
getFeedUrlAtEndpoint :: String -> Aff String
getFeedUrlAtEndpoint endpoint = do
  log $  "Fetching the quizzes page from: " <> endpoint
  stream <- requestStream (options endpoint)
  html <- readToEnd UTF8 stream
  
  quizUrl <- html # readFeedUrlFromHtml # maybe mempty pure
  quizPage <- requestStream (options quizUrl) >>= readToEnd UTF8
  log $  "Fetching todays quiz page from: " <> quizUrl
  
  tag <- findTheScriptTag' quizPage
  let assets = tag.news
               # foldMap (_.news.display_assets)
               -- TODO: this is all a bit yuck
               # catMaybes <<< map (\o -> do
                               e <- o.embedCode
                               pure e.embed)
               # filter (String.contains (Pattern "riddle"))
               # Array.head
               # map decodeHTML
               >>= findTheIframeSrc

  log $ "We found the goddamn iframe"
               
  case assets of
    Just s -> pure s
    Nothing -> throwError (error "shit")

findTheIframeSrc :: String -> Maybe String
findTheIframeSrc = load
                   >>> select "iframe"
                   >>> attr "src"
  

readFeedUrlFromHtml :: String -> Maybe String
readFeedUrlFromHtml = load
                      >>> select "div.main_article a[href]" -- >>> select "a[href]"
                      >>> attr "href"

foreign import decodeHTML :: String -> String
                      
findTheScriptTag' :: forall m. MonadError Error m => String -> m StuffBlob
findTheScriptTag' s =
  case Array.head (findTheScriptTag s) of
    Just v -> pure v
    Nothing -> throwError (error "Can't find the gd tag")

findTheScriptTag :: String -> Array StuffBlob
findTheScriptTag src = do
  tag <- loadRoot src # find "script" # Cheerio.toArray
  o <- maybe mempty pure $ do
          txt <- Cheerio.html tag
          json <- txt
                  # String.trim
                  # String.stripPrefix (Pattern "window.__INITIAL_STATE__ = ")
          obj <- read json
          pure obj
  pure o
      
read :: forall m. Monoid (m StuffBlob) =>  Applicative m => String -> m StuffBlob
read s = either (\_ -> mempty) pure do
      json :: String <- readJSON s
      blob :: StuffBlob <- readJSON json
      pure blob

type StuffItem = { news :: { display_assets :: Array { embedCode :: Maybe { embed :: String } } } }
type StuffBlob = { news :: Object StuffItem}

requestStream :: forall t. Options RequestOptions -> Aff (Stream (read :: Read | t))
requestStream opt = makeAff go
  where
    go f = do
          req <- request opt (\response -> do
                                     let stream = responseAsStream response
                                     -- setEncoding stream UTF8
                                     f (Right stream))
          end (requestAsStream req) (pure unit)
          pure $ Canceler (\_ -> pure unit)
