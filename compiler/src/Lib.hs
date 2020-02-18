{-# LANGUAGE OverloadedStrings #-}
module Lib (parse) where
import Control.Applicative
import Control.Monad
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import Text.Megaparsec(Parsec, between, choice, parseTest, eof)
import Text.Megaparsec.Char(alphaNumChar, string, space1)
import qualified Text.Megaparsec.Char.Lexer as L


type Parser = Parsec Void Text

data Function = Function { funcName :: Text
                         , funcArgs :: [Text]
                         , funcBody :: Term -- different in pattern-matching
                         } deriving (Show)

data Term = Expression { callFunc :: Term
                       , callArgs :: [Term]
                       } | Reference Text deriving (Show)

sc :: Parser ()
sc = L.space
  space1
  (L.skipLineComment "--")
  (L.skipBlockComment "{-" "-}")

symbol :: Text -> Parser Text
symbol = L.symbol sc

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

reference :: Parser Text
reference = lexeme $ T.pack <$> some alphaNumChar

arguments :: Parser [Text]
arguments = many reference

term :: Parser Term
term = choice
  [ parentesizedExpression
  , reference >>= (return . Reference)
  ]

parentesizedExpression :: Parser Term
parentesizedExpression = between (symbol "(") (symbol ")") expression

expression :: Parser Term
expression = do
  func <- term
  args <- many term
  return $ Expression func args

function :: Parser Function
function = do
  name <- reference
  args <- arguments
  void (symbol "=")
  body <- term
  return $ Function name args body


functions :: Parser [Function]
functions = many function <* eof

parse :: Text -> IO ()
parse = parseTest functions
