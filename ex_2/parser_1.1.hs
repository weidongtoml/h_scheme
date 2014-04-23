import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad

data LispVal = Atom String
             | Bool Bool
             | String String
             | Number Integer
             | List [LispVal]
             | DottedList [LispVal] LispVal

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany space

parseAtom :: Parser LispVal
parseAtom = do
                first <- letter <|> symbol
                rest <- many (letter <|> digit <|> symbol)
                let atom = first : rest
                return $ case atom of
                            "#t" -> Bool True
                            "#f" -> Bool False
                            _    -> Atom atom

parseString :: Parser LispVal
parseString = do
                char '"'
                x <- many (noneOf "\"")
                char '"'
                return $ String x

-- Exercise 1.1, rewrite parseNumber without liftM, use do-notation instead.
parseNumber :: Parser LispVal
-- parseNumber = liftM (Number . read) $ many1 digit
parseNumber = do
                number_str <- many1 digit
                let number = read number_str :: Integer
                return $ Number number

parseExpr :: Parser LispVal
parseExpr = parseAtom 
        <|> parseString
        <|> parseNumber

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value "

main :: IO ()
main = do
    args <- getArgs
    putStrLn (readExpr (args !! 0))