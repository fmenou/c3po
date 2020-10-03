{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Example
  ( example
  ) where

import           Example.Locales (MyLocale (EnglishUK, FrenchFrance), locales)

import           Data.Text.IO    as TIO (putStrLn)
import           Named

[locales|
  hello "en_UK" "Hello!"
  hello "fr_FR" "Bonjour !"
  trip  "en_UK" "To {destination}, from {origin}."
  trip  "fr_FR" "Au départ de {origin}, à destination de {destination}."
|]

speak :: MyLocale -> IO ()
speak loc =
  let message = trip ! #origin      "Paris"
                     ! #destination "Berlin"
  in do
       TIO.putStrLn (hello   loc)
       TIO.putStrLn (message loc)

example :: IO ()
example = do
  speak FrenchFrance
  speak EnglishUK
