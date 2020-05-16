module Unstuff.Html where

import Prelude

import Data.Either (Either(..))
import Data.Foldable (all)
import Data.List (elem)
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import LenientHtmlParser as H
import Text.Parsing.StringParser (ParseError)

class ProbablyTag m where
  asTag :: m -> Either ParseError H.Tag

instance tagProbablyTag :: ProbablyTag H.Tag where
  asTag = Right

instance stringProbablyTag :: ProbablyTag String where
  asTag = H.parse H.tag
  
match :: H.Tag -> H.Tag -> Boolean
match example needle =
  case Tuple example needle of
    Tuple (H.TagOpen l attrL) (H.TagOpen r attrR ) ->
      l == r && all (\k -> elem k attrR) attrL

    Tuple (H.TagSingle l attrL) (H.TagSingle r attrR ) ->
      l == r && all (\k -> elem k attrR) attrL

    Tuple (H.TScript attrL b) (H.TScript attrR d) ->
      all (\k -> elem k attrR) attrL
      
    Tuple (H.TagClose l) (H.TagClose r) ->
      l == r

    _ -> false
    
match' :: forall a b. ProbablyTag a => ProbablyTag b => a -> b -> Boolean
match' a b =
  case Tuple (asTag a) (asTag b) of
    Tuple (Right l) (Right r) -> match l r
    Tuple (Left e) _ -> false
    Tuple _ (Left e) -> false

getAttr :: String -> H.Tag -> Maybe String
getAttr name = case _ of
  H.TagSingle _ attr -> getAttr' name attr
  H.TagOpen _ attr -> getAttr' name attr
  _ -> Nothing
  
getAttr' :: String -> H.Attributes -> Maybe String
getAttr' name xs = getValue <$> List.find matchName xs
  where
    matchName (H.Attribute (H.Name name') _) = name == name'
    getValue (H.Attribute _ (H.Value x )) = x

