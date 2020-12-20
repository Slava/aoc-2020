{-# LANGUAGE OverloadedStrings #-}
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as IO
import Data.Void
import Text.Megaparsec(Parsec, between, choice, eof, runParser, errorBundlePretty)
import Text.Megaparsec.Char(space1)
import Control.Monad.Combinators.Expr(makeExprParser, Operator(..))
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

data Expr
  = Int Int
  | Sum      Expr Expr
  | Product  Expr Expr
  deriving (Eq, Ord, Show)

sc :: Parser ()
sc = L.space
  space1
  (L.skipLineComment "--")
  (L.skipBlockComment "{-" "-}")

symbol :: Text -> Parser Text
symbol = L.symbol sc

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

pInteger :: Parser Expr
pInteger = Int <$> lexeme L.decimal

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

pTerm :: Parser Expr
pTerm = choice [parens pExpr, pInteger]

pExpr :: Parser Expr
pExpr = makeExprParser pTerm operatorTable

pProgram :: Parser Expr
pProgram = pExpr <* eof

operatorTable :: [[Operator Parser Expr]]
operatorTable = [[binary "+" Sum], [binary "*" Product]]

binary :: Text -> (Expr -> Expr -> Expr) -> Operator Parser Expr
binary name f = InfixL (f <$ symbol name)

eval :: Expr -> Int
eval (Int x) = x
eval (Sum l r) = (eval l) + (eval r)
eval (Product l r) = (eval l) * (eval r)

process :: Text -> IO Int
process input = do
  let parsed = runParser pProgram "input" input in
    case parsed of
      Right expr -> return $ eval expr
      Left errorsBundle -> do
        putStrLn $ errorBundlePretty errorsBundle
        return (0)

main :: IO ()
main = do
  input <- IO.getContents
  processed <- mapM process $ T.lines input
  print $ sum processed
