import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad
import Numeric

data LispVal = Atom String
             | Bool Bool
             | String String
             | Number Integer
             | Character Char
             | List [LispVal]
             | DottedList [LispVal] LispVal

instance Show LispVal where show = showVal

showVal :: LispVal -> String
showVal (Atom name) = name -- ++ "[Atom]"
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (String contents) = "\"" ++ contents ++ "\"" -- ++ "[String]"
showVal (Number number) = show number -- ++ "[Number]"
showVal (Character char) = show char -- ++ "[Character]"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

zero_or_more_spaces :: Parser ()
zero_or_more_spaces = skipMany space

parseAtom :: Parser LispVal
parseAtom = do
    first <- letter <|> symbol
    rest <- many (letter <|> digit <|> symbol)
    let atom = first : rest
    return $ case atom of
        "#t" -> Bool True
        "#f" -> Bool False
        ('#' : '\\' : alphas) -> Character $ f alphas --TODO(weidoliang): readExpr "#\\a" does not work, need to debug.
                                    where f a | length a == 1     = a !! 0
                                              | a == "space"      = ' '
                                              | a == "newline"    = '\n'
        ('#' : 'x' : digits) -> Number $ fst (readHex digits !! 0)
        ('#' : 'd' : digits) -> Number $ fst (readDec digits !! 0)
        ('#' : 'o' : digits) -> Number $ fst (readOct digits !! 0)
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

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
    head <- endBy parseExpr spaces
    tail <- char '.' >> spaces >> parseExpr
    return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]

parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseString
        <|> parseNumber
        <|> parseQuoted
        <|> do  char '('
                zero_or_more_spaces
                x <- try parseList <|> parseDottedList
                zero_or_more_spaces
                char ')'
                return x

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value: " ++ show val

main :: IO ()
main = do
    args <- getArgs
    putStrLn (readExpr (args !! 0))