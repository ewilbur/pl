{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
module Data.Predicate where

import Data.Text (Text)

import Text.Parsec
import Text.Parsec.Text
import Text.Parsec.Token
import Text.ParserCombinators.Parsec.Combinator
import Text.ParserCombinators.Parsec.Char

import Data.Char
import Data.Either
import Data.Functor.Identity

data WFF a
    = Atomic a
    | Negate (WFF a)
    | And (WFF a) (WFF a)
    | Or (WFF a) (WFF a)
    | Implies (WFF a) (WFF a)
    | Equivalent (WFF a) (WFF a)
    deriving Functor

instance Show a => Show (WFF a) where
    show (Atomic a) = "[" ++ [head $ dropWhile (not . isAlpha) (show a)] ++ "]"
    show (Negate wff) = "-" ++ show wff
    show (And wff wff') = "(" ++ show wff ++ "&" ++ show wff' ++ ")"
    show (Or wff wff') = "(" ++ show wff ++ "v" ++ show wff' ++ ")"
    show (Implies wff wff') = "(" ++ show wff ++ "->" ++ show wff' ++ ")"
    show (Equivalent wff wff') = "(" ++ show wff ++ "<->" ++  show wff' ++ ")"

type WFF' = WFF Char

data Hole

wffDefinition :: Stream s m Char => GenLanguageDef s () m
wffDefinition
  = LanguageDef { commentStart    = "/*"
                , commentEnd      = "*/"
                , commentLine     = ""
                , nestedComments  = False
                , identStart      = upper
                , identLetter     = satisfy (const False) -- ^ Only allow single character for a variable name
                , opStart         = satisfy (const False)
                , opLetter        = satisfy (const False)
                , reservedNames   = []
                , reservedOpNames = ["&", "v", "->", "=>", "<->", "<=>"]
                , caseSensitive   = True
                }

wffTokenParser :: Stream s m Char => GenTokenParser s () m
wffTokenParser = makeTokenParser wffDefinition

wffAtomic :: Parser (WFF Char)
wffAtomic = (Atomic . head) <$> identifier wffTokenParser 

wff :: Parser (WFF Char)
wff = try wffAtomic <|> try wffAnd <|> try wffOr <|> try wffImplies <|> try wffEquivalent <|> try wffNegate

wffNegate :: Parser (WFF Char)
wffNegate = Negate <$> (symbol wffTokenParser "-" >> wff)

wffBinaryOperation op = parens wffTokenParser (binop op)
    where
        binop x = do
            wff1 <- wff
            reservedOp wffTokenParser x
            wff2 <- wff
            return (wff1, wff2)

wffAnd :: Parser (WFF Char)
wffAnd = uncurry And <$> wffBinaryOperation "&"

wffOr :: Parser (WFF Char)
wffOr = uncurry Or <$> wffBinaryOperation "v"

wffImplies :: Parser (WFF Char)
wffImplies = uncurry Implies <$> wffBinaryOperation "->"

wffEquivalent :: Parser (WFF Char)
wffEquivalent = uncurry Equivalent <$> wffBinaryOperation "<->"

isWFF :: Text -> Bool
isWFF w = isRight $ parse wff "" w

isAtomic :: Text -> Bool
isAtomic w = case parse wff "" w of
               Right (Atomic _) -> True
               _ -> False

reduce :: WFF Bool -> Bool
reduce (Atomic a) = a
reduce (Negate wff) = not . reduce $ wff
reduce (And wff wff') = reduce wff && reduce wff'
reduce (Or wff wff') = reduce wff || reduce wff'
reduce (Implies wff wff') = reduce wff' || reduce (Negate wff) 
reduce (Equivalent wff wff') = reduce wff == reduce wff'

evaluate :: WFF a -> (a -> Bool) -> Bool
evaluate wff f = reduce $ fmap f wff

onWFF :: (WFF Char -> b) -> Text -> Either ParseError b
onWFF f w = f <$> (parse wff "" w)

scopes :: Show a => WFF a -> [(String, String, String)]
scopes (Atomic _) = []
scopes (Negate w) = ("-", show w, "") : scopes w
scopes (And wff wff') = ("&", show wff, show wff') : (scopes wff ++ scopes wff')
scopes (Or wff wff') = ("v", show wff, show wff') : (scopes wff ++ scopes wff')
scopes (Implies wff wff') = ("->", show wff, show wff') : (scopes wff ++ scopes wff')
scopes (Equivalent wff wff') = ("<->", show wff, show wff') : (scopes wff ++ scopes wff')

majorConnective :: Show a => WFF a -> Maybe (String, String, String)
majorConnective (Atomic _) = Nothing
majorConnective w = Just $ head . scopes $ w
