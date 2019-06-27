{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Example
  ( example
  ) where

import           Example.Locales

import           C3PO            (c3po)

import           Data.Text.IO    as TIO (putStrLn)
import           Named

c3po ''MyLocale allLocales $
  unlines
    [ "hello fr_FR \"Bonjour !\""
    , "hello en_UK \"Hello!\""
    , "trip fr_FR \"Au départ de {origin}, à destination de {destination}.\""
    , "trip en_UK \"To {destination}, from {origin}.\""
    ]

example :: IO ()
example = do
  speak FrenchFrance
  speak EnglishUK

speak :: MyLocale -> IO ()
speak loc =
  let message = trip ! #origin      "Paris"
                     ! #destination "Berlin"
  in do
       TIO.putStrLn (message loc)
       TIO.putStrLn (hello   loc)
