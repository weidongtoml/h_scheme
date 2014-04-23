import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad

data LispVal = Atom String
             | Bool Bool
             | String String
             | Number Integer
             | List [LispVal]
             | DottedList [LispVal] LispVal

instance Show LispVal where show = showVal

showVal :: LispVal -> String
showVal (Atom name) = name
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Number number) = show number



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

-- Rewrite parseString to be R5RS compliant.
parseString :: Parser LispVal
parseString = do
                char '"'
                x <- many $ (char '\\' >> char '\"') <|> (noneOf "\"")
                char '"'
                return $ String x

parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit

parseExpr :: Parser LispVal
parseExpr = parseAtom 
        <|> parseString
        <|> parseNumber

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value: " ++ show val

main :: IO ()
main = do
    args <- getArgs
    putStrLn (readExpr (args !! 0))