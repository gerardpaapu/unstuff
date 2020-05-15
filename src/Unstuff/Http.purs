module Unstuff.Http where

import Prelude

import Data.Either (Either(..))
import Data.Options (Options, (:=))
import Effect.Aff (Aff, Canceler(..), makeAff)
import Effect.Ref as Ref
import Foreign.Object (singleton)
import Node.Encoding (Encoding(..))
import Node.HTTP.Client (RequestHeaders(..), RequestOptions, headers, hostname, method, path, port, protocol, request, requestAsStream, responseAsStream)
import Node.Stream (Read, Readable, Stream, end)
import Node.Stream as S

mobileSafari :: String
mobileSafari = "Mozilla/5.0 (iPad; CPU OS 13_4 like Mac OS X) AppleWebKit/605.1.15 (KHTML, like Gecko) CriOS/81.0.4044.124 Mobile/15E148 Safari/604.1"

options :: String -> Options RequestOptions
options path_ =
  protocol := "https:" <>
  hostname := "i.stuff.co.nz" <>
  path := path_ <>
  port := 443 <>
  method := "GET" <>
  headers := RequestHeaders (singleton "User-Agent" mobileSafari)

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

requestStream :: forall t. Options RequestOptions -> Aff (Stream (read :: Read | t))
requestStream opt = makeAff go
  where
    go f = do
          req <- request opt (\response -> do
                                     let stream = responseAsStream response
                                     f (Right stream))
          end (requestAsStream req) (pure unit)
          pure $ Canceler (\_ -> pure unit)

getString :: String -> Aff String
getString path = requestStream (options path) >>= readToEnd UTF8
