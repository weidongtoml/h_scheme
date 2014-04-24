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

-- Parser section.

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

readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err -> String $ "No match: " ++ show err
    Right val -> val

-- Evaluator section.
eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Bool _)   = val
eval (List [Atom "quote", val]) = val
eval (List (Atom func : args)) = apply func $ map eval args

apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) $ lookup func primitives

primitives :: [(String, [LispVal] -> LispVal)]
primitives = [  ("+",               numericBinop (+)),
                ("-",               numericBinop (-)),
                ("*",               numericBinop (*)),
                ("/",               numericBinop div),
                ("mod",             numericBinop mod),
                ("quotient",        numericBinop quot),
                ("remainder",       numericBinop rem),
                ("symbol?",         booleanTest isSymbol),
                ("string?",         booleanTest isString),
                ("number?",         booleanTest isNumber),
                ("list?",           booleanTest isList),
                ("symbol->string",  symbolToString . (!! 0) ),
                ("string->symbol",  stringToSymbol . (!! 0) )]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = Number $ foldl1 op $map unpackNum params

unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
unpackNum (String n) = let parsed = reads n :: [(Integer, String)] in
                            if null parsed
                                then 0
                                else fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum _          = 0

booleanTest :: (LispVal -> Bool) -> [LispVal] -> LispVal
booleanTest op params = Bool $ foldl (&&) True $ map op params

isSymbol :: LispVal -> Bool
isSymbol (Atom _) = True
isSymbol _        = False

isString :: LispVal -> Bool
isString (String _) = True
isString _          = False

isNumber :: LispVal -> Bool
isNumber (Number _) = True
isNumber _          = False

isList :: LispVal -> Bool
isList (List _)     = True
isList _            = False

symbolToString :: LispVal -> LispVal
symbolToString (Atom a) = String a
symbolToString _        = String ""

stringToSymbol :: LispVal -> LispVal
stringToSymbol (String a) = Atom a
                
main :: IO ()
main = getArgs >>= print . eval . readExpr . head