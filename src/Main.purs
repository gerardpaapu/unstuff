module Main where

import Prelude

import Control.Monad.Except (class MonadError)
import Control.Promise (Promise)
import Control.Promise as Promise
import Data.Array as Array
import Data.Date as D
import Data.Either (hush)
import Data.Enum (fromEnum)
import Data.Filterable (filterMap)
import Data.Foldable (foldMap)
import Data.List (List)
import Data.List as List
import Data.Maybe (Maybe(..), maybe)
import Data.String (Pattern(..))
import Data.String as String
import Data.Traversable (for, for_)
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Data.Unfoldable (class Unfoldable, unfoldr)
import Effect (Effect)
import Effect.Aff (Aff, error, launchAff_, throwError)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Effect.Exception (Error)
import Effect.Now (nowDate)
import Foreign.Object (Object)
import LenientHtmlParser (Tag)
import LenientHtmlParser as H
import Simple.JSON (readJSON, writeJSON)
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
        body <- getFeedUrl
        pure
          { statusCode: 200.0
          , body: writeJSON body
          }

main :: Effect Unit
main = do
  launchAff_ do
    urls <- getFeedUrl
    log "---------------"
    for_ urls $ \url ->
      log $ show url

log :: String -> Aff Unit
log = liftEffect <<< Console.log

getFeedUrl :: Aff (Array { title :: String , url :: String })
getFeedUrl = getFeedUrlAtEndpoint "/national/quizzes"

getFeedUrlAtEndpoint :: String -> Aff (Array { url :: String, title :: String })
getFeedUrlAtEndpoint endpoint = do
  log $ "Fetching the quizzes page from: " <> endpoint
  html <- Http.getString endpoint
  log $ "Finding the url for the latest quiz"

  urls <- allQuizUrls html
  timestamp <- todayTimestamp
  log $ "timestamp " <> timestamp
  
  let urls' = urls # Array.filter (_.text >>> isMatchingTitle timestamp)
  for urls' $ \({ href, text }) -> do
    log $ "Fetching the quiz page for:  " <> text
    log $ "Fetching the quiz page from: " <> href
    quizPage <- Http.getString href
    log $ "Looking for the title"
    title <- findPageTitle' quizPage
    log $ "looking for the __INIT_STATE__ blob"
    blob <- findTheScriptTag' quizPage
    log $ "looking for the iframe src in the blob"
    url <- urlFromBlob' blob
    log $ "found!"
    pure { url, title }


isMatchingTitle :: String -> String -> Boolean
isMatchingTitle timestamp title = do
  let notSports = not $ String.contains (Pattern "Sport") title
  let isToday = String.contains (Pattern timestamp) title
  isToday && notSports

todayTimestamp :: Aff String
todayTimestamp = liftEffect $ do
  today <- nowDate
  let dayN = D.day today # fromEnum # show
  pure $ show (D.month today) <> " " <>  dayN


urlFromBlob' :: forall m. MonadError Error m => StuffBlob -> m String
urlFromBlob' blob = do
  let
    url = Array.head $ urlFromBlob blob
  case url of
    Just result -> pure result
    _ -> throwError (error "Can't find the url in the __INIT_STATE__ blob")

urlFromBlob :: StuffBlob -> Array String
urlFromBlob tag = allEmbeds tag # filterMap findTheIframeSrc

allEmbeds :: forall a. StuffBlob' a -> Array a
allEmbeds =
  _.news
    >>> foldMap (_.news.display_assets)
    >>> foldMap (_.embedCode >>> getEmbed)
  where
  getEmbed = case _ of
    Just { embed } -> pure embed
    _ -> mempty

findTheIframeSrc :: String -> Maybe String
findTheIframeSrc str = do
  let
    html = decodeHTML str
  dom <- hush $ H.parseTags html
  tag <- dom # List.find (Html.match' "<iframe>")
  Html.getAttr "src" tag

findLatestQuizUrl' :: forall m. MonadError Error m => String -> m String
findLatestQuizUrl' =
  findLatestQuizUrl
    >>> case _ of
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

allQuizzes' :: (List Tag) -> Maybe (Tuple { href :: String , text :: String} (List Tag))                  
allQuizzes' dom =
    dom
    # List.dropWhile (not <<< Html.match' "<li class=\"story-list__item js-adfliction__target--all\">")
    # List.dropWhile (not <<< Html.match' "<a>")
    # case _ of
      List.Nil -> Nothing
      List.Cons x xs -> do
        href <- Html.getAttr "href" x
        text <- getTitle xs
        pure ({ href, text } /\ xs)
    where
      getTitle = 
        List.dropWhile (not <<< Html.match' "<h3>")
        >>> List.drop 1
        >>> case _ of
          List.Cons (H.TNode txt) _ -> pure (String.trim txt)
          _ -> Nothing
        

allQuizUrls :: forall m u. MonadError Error m =>  Unfoldable u => String -> m (u _)
allQuizUrls html = do
  case H.parseTags html # hush of
    Nothing -> throwError (error "can't parse html")
    Just dom -> pure $ unfoldr allQuizzes' dom
      

findPageTitle' :: String -> Aff String
findPageTitle' =
  findPageTitle
    >>> maybe (throwError (error "No page title")) pure

findPageTitle :: String -> Maybe String
findPageTitle html = do
  dom <- H.parseTags html # hush
  tag <-
    dom
      # List.dropWhile (not <<< Html.match' "<title>")
      # List.drop 1
      # List.head
  case tag of
    (H.TNode text) -> Just text
    _ -> Nothing

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
  = { news :: { display_assets :: Array { embedCode :: Maybe { embed :: a } } } }

type StuffBlob
  = StuffBlob' String
