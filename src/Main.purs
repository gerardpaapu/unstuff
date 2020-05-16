module Main where

import Prelude

import Control.Monad.Except (class MonadError)
import Control.Promise (Promise)
import Control.Promise as Promise
import Data.Array as Array
import Data.Either (hush)
import Data.Filterable (filterMap)
import Data.Foldable (foldMap)
import Data.List as List
import Data.Maybe (Maybe(..))
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

  log $ "Finding the url for the latest quiz"
  quizUrl <- findLatestQuizUrl' html

  log $ "Fetching todays quiz page from: " <> quizUrl
  quizPage <- Http.getString quizUrl

  log $ "looking for the __INIT_STATE__ blob"
  blob <- findTheScriptTag' quizPage

  log $ "looking for the iframe src in the blob"
  url <- urlFromBlob' blob
  
  pure url
  
urlFromBlob' :: forall m. MonadError Error m => StuffBlob -> m String
urlFromBlob' blob = do
  let url = Array.head $ urlFromBlob blob
  case url of
    Just result -> pure result
    _ -> throwError (error "Can't find the url in the __INIT_STATE__ blob")

urlFromBlob :: StuffBlob -> Array String
urlFromBlob tag =
  allEmbeds tag # filterMap findTheIframeSrc

allEmbeds :: forall a. StuffBlob' a -> Array a
allEmbeds  =
  _.news
  >>> foldMap (_.news.display_assets)
  >>> foldMap (_.embedCode >>> t)
  where
    t = case _ of
      Just { embed } -> pure embed
      _ -> mempty

findTheIframeSrc :: String -> Maybe String
findTheIframeSrc str = do
  let html = decodeHTML str
  dom <- hush $ H.parseTags html
  tag <- dom # List.find (Html.match' "<iframe>")
  Html.getAttr "src" tag

findLatestQuizUrl' :: forall m. MonadError Error m => String -> m String
findLatestQuizUrl' = findLatestQuizUrl >>> case _ of
  Just s -> pure s
  Nothing -> throwError (error "Can't find quiz url")

findLatestQuizUrl :: String -> Maybe String
findLatestQuizUrl html = do
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
  obj <- hush $ readJSON s >>= readJSON
  pure obj
  where
  takeState = case _ of
    H.TScript _ s ->
      s
        # String.trim
        # String.stripPrefix (Pattern "window.__INITIAL_STATE__ = ")
    _ -> Nothing

type StuffBlob' a
  = { news :: Object (StuffItem a) }

type StuffItem a
  = { news :: { display_assets :: Array { embedCode :: Maybe { embed :: a} } } }

type StuffBlob
  = StuffBlob' String
