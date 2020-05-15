module Main where

import Prelude
import Control.Monad.Except (class MonadError)
import Control.Promise (Promise)
import Control.Promise as Promise
import Data.Array (catMaybes)
import Data.Array as Array
import Data.Either (either, hush)
import Data.Foldable (foldMap)
import Data.List as List
import Data.Maybe (Maybe(..), maybe)
import Data.String (Pattern(..))
import Data.String as String
import Effect (Effect)
import Effect.Aff (Aff, error, launchAff_, throwError)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Effect.Exception (Error)
import Foreign.Object (Object)
import LenientHtmlParser as H
import Simple.JSON (readJSON)
import Unstuff.Html as Html
import Unstuff.Http as Http

type Payload
  = { statusCode :: Number
    , body :: String
    }

handler :: Effect (Promise Payload)
handler =
  Promise.fromAff
    $ do
        url <- getFeedUrl
        pure
          { statusCode: 200.0
          , body: url
          }

main :: Effect Unit
main = do
  launchAff_ do
    url <- getFeedUrl
    log url

log :: String -> Aff Unit
log = liftEffect <<< Console.log

getFeedUrl :: Aff String
getFeedUrl = getFeedUrlAtEndpoint "/national/quizzes"

getFeedUrlAtEndpoint :: String -> Aff String
getFeedUrlAtEndpoint endpoint = do
  log $ "Fetching the quizzes page from: " <> endpoint
  html <- Http.getString endpoint
  quizUrl <- html # readFeedUrlFromHtml # maybe (throwError (error "fuck")) pure
  quizPage <- Http.getString quizUrl
  log $ "Fetching todays quiz page from: " <> quizUrl
  tag <- findTheScriptTag' quizPage
  case urlFromBlob tag of
    Just s -> pure s
    Nothing -> throwError (error "shit")

urlFromBlob :: StuffBlob -> Maybe String
urlFromBlob tag =
    tag.news
    # foldMap (_.news.display_assets)
    # map (_.embedCode)
    # catMaybes
    # map (_.embed)
    # Array.filter (String.contains (Pattern "riddle"))
    # Array.head
    >>= findTheIframeSrc

findTheIframeSrc :: String -> Maybe String
findTheIframeSrc str = do
  let html = decodeHTML str
  dom <- hush $ H.parseTags html
  tag <- dom # List.find (Html.match' "<iframe>")
  Html.getAttr "src" tag

readFeedUrlFromHtml :: String -> Maybe String
readFeedUrlFromHtml html = do
  dom <- H.parseTags html # hush
  d <-
    dom
      # List.dropWhile (not <<< Html.match' "<li class=\"story-list__item js-adfliction__target--all\">")
      # List.dropWhile (not <<< Html.match' "<a>")
      # List.head
  Html.getAttr "href" d

foreign import decodeHTML :: String -> String

findTheScriptTag' :: forall m. MonadError Error m => String -> m StuffBlob
findTheScriptTag' s = case (findTheScript s) of
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
    H.TScript _ s ->
      s
        # String.trim
        # String.stripPrefix (Pattern "window.__INITIAL_STATE__ = ")
    _ -> Nothing

read :: forall m. Monoid (m StuffBlob) => Applicative m => String -> m StuffBlob
read s =
  either (\_ -> mempty) pure do
    json :: String <- readJSON s
    blob :: StuffBlob <- readJSON json
    pure blob

type StuffItem
  = { news :: { display_assets :: Array { embedCode :: Maybe { embed :: String } } } }

type StuffBlob
  = { news :: Object StuffItem }
