module Main where

import System.Environment
import Data.Either
import Control.Monad

import Text.Parsec
import Text.Parsec.Text
import qualified Data.Text as Text

import Data.Predicate


main :: IO ()
main = do
    formula <- parse (do {w <- wff; eof; return w}) "" . Text.pack . concat <$> getArgs
    either (const $ putStrLn "Not a well formed formula") formulaInfo formula
    print formula
        where
            formulaInfo w = do
                putStr "Formula scopes: " >> print (scopes w)
                putStr "Major connective part: " >> print (majorConnective w)
