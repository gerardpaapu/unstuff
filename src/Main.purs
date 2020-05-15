module Main where

import Prelude

import Control.Monad.Except (class MonadError)
import Data.Array (catMaybes, filter)
import Data.Array as Array
import Data.Either (Either(..), either, hush)
import Data.Foldable (findMap, foldMap, foldr)
import Data.List (List(..), (:))
import Data.List as List
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
import LenientHtmlParser as H
import Node.Encoding (Encoding(..))
import Node.HTTP.Client (RequestHeaders(..), RequestOptions, headers, hostname, method, path, port, protocol, request, requestAsStream, responseAsStream)
import Node.Stream (Read, Readable, Stream, end)
import Node.Stream as S
import Simple.JSON (readJSON)
import Unstuff.Html as H
import Control.Promise (Promise)
import Control.Promise as Promise

type Payload =
  { statusCode :: Number
  , body :: String
  }
  
handler :: Effect (Promise Payload)
handler = Promise.fromAff $ do
  url <- getFeedUrl
  pure { statusCode: 200.0
       , body: url }
  
main :: Effect Unit
main = do
  launchAff_ do
    url <- getFeedUrl
    log url

chromeAgent :: String
chromeAgent = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/81.0.4044.138 Safari/537.36"

mobileSafari :: String
mobileSafari = "Mozilla/5.0 (iPad; CPU OS 13_4 like Mac OS X) AppleWebKit/605.1.15 (KHTML, like Gecko) CriOS/81.0.4044.124 Mobile/15E148 Safari/604.1"


options :: String -> Options RequestOptions
options path_ =
  protocol := "https:" <>
  hostname := "i.stuff.co.nz" <>
  path := path_ <>
  port := 443 <>
  method := "GET" <>
  headers := RequestHeaders (singleton "User-Agent" chromeAgent)

log :: String -> Aff Unit
log = liftEffect <<< Console.log

getFeedUrl ::  Aff String
getFeedUrl =
  getFeedUrlAtEndpoint "/national/quizzes"

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
  quizUrl <- html # readFeedUrlFromHtml # maybe (throwError (error "fuck")) pure
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
  let iframeSrc = assets >>= findTheIframeSrc
  case iframeSrc of
    Just s -> pure s
    Nothing -> throwError (error "shit")

findTheIframeSrc :: String -> Maybe String
findTheIframeSrc html = do
  dom <- hush $ H.parseTags html
  tag <- dom # List.find (H.match' "<iframe>")
  H.getAttr "src" tag

readFeedUrlFromHtml :: String -> Maybe _
readFeedUrlFromHtml html = do
  dom <- H.parseTags html # hush
  d <- dom
    # List.dropWhile (not <<< H.match' "<li class=\"story-list__item js-adfliction__target--all\">")
    # List.dropWhile (not <<< H.match' "<a>")
    # List.head

  H.getAttr "href" d

foreign import decodeHTML :: String -> String
                      
findTheScriptTag' :: forall m. MonadError Error m => String -> m StuffBlob
findTheScriptTag' s =
  case (findTheScript s) of
    Just v -> pure v
    Nothing -> throwError (error "Can't find the gd tag")

  
findTheScript :: String -> Maybe StuffBlob
findTheScript src = do
  html <- hush $ H.parseTags src
  s <- html # List.findMap takeState
  json <- hush $ readJSON s
  obj <- hush $ readJSON json
  pure obj
  where
    takeState = case _ of
      H.TScript _ s -> s
                       # String.trim
                       # String.stripPrefix (Pattern "window.__INITIAL_STATE__ = ")
      _ -> Nothing
  
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
