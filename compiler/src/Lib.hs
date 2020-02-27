{-# LANGUAGE OverloadedStrings #-}
module Lib (parse) where
import Control.Applicative
import Control.Monad
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.IO (putStrLn)
import Data.Void
import Text.Megaparsec(Parsec, between, choice, eof, sepBy, runParser, errorBundlePretty)
import Text.Megaparsec.Char(alphaNumChar, space1)
import qualified Text.Megaparsec.Char.Lexer as L


type Parser = Parsec Void Text

data Function = Function { funcName :: Text
                         , funcArgs :: [Text]
                         , funcBody :: [Branch]
                         } deriving (Show)

data Term = Expression { callFunc :: Term
                       , callArgs :: [Term]
                       }
          | Reference Text
          | Symbol Text
          | List [Term]
          deriving (Show)

data Branch = Branch { branchCond :: Maybe Term
                     , branchBody :: Term
                     } deriving (Show)

sc :: Parser ()
sc = L.space
  space1
  (L.skipLineComment "--")
  (L.skipBlockComment "{-" "-}")

symbol :: Text -> Parser Text
symbol = L.symbol sc

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

token :: Parser Text
token = T.pack <$> some alphaNumChar

lexToken :: Parser Text
lexToken = lexeme token

reference :: Parser Term
reference = lexeme $ Reference <$> token

literalSymbol :: Parser Term
literalSymbol = do
  void "#"
  lexeme $ Symbol <$> token

literalList :: Parser Term
literalList = do
  terms <- between (symbol "[") (symbol "]") (term `sepBy` symbol ",")
  return $ List terms

arguments :: Parser [Text]
arguments = many lexToken

term :: Parser Term
term = choice
  [ parentesizedExpression
  , reference
  , literalSymbol
  , literalList
  ]

parentesizedExpression :: Parser Term
parentesizedExpression = between (symbol "(") (symbol ")") expression

expression :: Parser Term
expression = do
  func <- term
  args <- many term
  return $ Expression func args

termAsBranch :: Parser [Branch]
termAsBranch = do
  body <- term
  return [Branch Nothing body]

otherwiseNothing :: Parser (Maybe Term)
otherwiseNothing = do
  void (symbol "otherwise")
  return Nothing

justTerm :: Parser (Maybe Term)
justTerm = Just <$> term

branch :: Parser Branch
branch = do
  void (symbol "|")
  br <- otherwiseNothing <|> justTerm
  void (symbol "->")
  body <- term
  return $ Branch br body

branches :: Parser [Branch]
branches = some branch

function :: Parser Function
function = do
  name <- lexToken
  args <- arguments
  void (symbol "=")
  body <- branches <|> termAsBranch
  return $ Function name args body


functions :: Parser [Function]
functions = many function <* eof

translateArgs :: [Text] -> Text
translateArgs [] = ""
translateArgs args = T.concat ["<", T.intercalate ", " args, ">"]

translateTerm :: Term -> Text
translateTerm (Reference ref) = ref
translateTerm (Symbol sym) = T.concat ["\"", sym, "\""]
translateTerm (List xs) = T.concat ["[", T.intercalate ", " (map translateTerm xs), "]"]
translateTerm (Expression call args) =
  T.concat [ translateTerm call
           , "<"
           , T.intercalate ", " $ map translateTerm args
           , ">"
           ]

translateFuncBody :: [Branch] -> Text
translateFuncBody [] = ""
translateFuncBody ((Branch Nothing t):[]) = translateTerm t
translateFuncBody bs = T.concat ["{\n", (translateFuncCases bs 0), "}", "[", (translateFuncCond bs 0), "]"]

translateFuncCases :: [Branch] -> Int -> Text
translateFuncCases [] _ = ""
translateFuncCases ((Branch _ t):bs) idx = T.concat ["  '", T.pack (show idx), "': ", (translateTerm t), ",\n", translateFuncCases bs (idx + 1)]

translateFuncCond :: [Branch] -> Int -> Text
translateFuncCond [] _ = ""
translateFuncCond ((Branch (Just cond) _):bs) idx = T.concat [translateTerm cond, " extends true ? ", "'", T.pack (show idx), "'", " : ", translateFuncCond bs (idx + 1)]
translateFuncCond ((Branch Nothing _):_) idx = T.concat ["'", T.pack (show idx), "'"]

translateFunction :: Function -> Text
translateFunction fn =
  T.concat [ "type "
           , funcName fn
           , translateArgs $ funcArgs fn
           , " = "
           , translateFuncBody $ funcBody fn
           , "\n"
           ]

translate :: [Function] -> Text
translate fns =
  T.concat $ map translateFunction fns

parse :: Text -> IO ()
parse input =
  let parsed = runParser functions "input" input in
    case parsed of
      Right fns -> Data.Text.IO.putStrLn $ translate fns
      Left errorsBundle -> Prelude.putStrLn $ errorBundlePretty errorsBundle
