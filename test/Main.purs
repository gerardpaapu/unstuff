module Test.Main where

import Prelude

import Data.Either (hush)
import Effect (Effect)
import Effect.Class.Console (log)
import LenientHtmlParser as H
import Unstuff.Html as H
import Data.List as List

main :: Effect Unit
main = do
  let p = do
        ls <- hush $ H.parseTags "<iframe src=\"poop\" />"
        iframe <- ls # List.find (H.match' "<iframe>")
        pure iframe
  log $ show p
