import Control.Monad
import Control.Monad.Error
import Numeric
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)

-- For Parser Tree

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

-- For Error Reporting
data LispError  = NumArgs Integer [LispVal]
                | TypeMismatch String LispVal
                | Parser ParseError
                | BadSpecialForm String LispVal
                | NotFunction String String
                | UnboundVar String String
                | Default String

showError :: LispError -> String
showError (NumArgs expected found) = "Expected " ++ show expected ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected ++ ", found " ++ show found
showError (Parser parseErr) = "Parse error at " ++ show parseErr
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ show func
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (Default message) = message

instance Show LispError where show = showError

instance Error LispError where 
    noMsg = Default "An error has occurred"
    strMsg = Default

type ThrowsError = Either LispError

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

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

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err -> throwError $ Parser err
    Right val -> return val

-- Evaluator section.
eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _)   = return val
eval (List [Atom "quote", val]) = return val
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognised special form" badForm

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function args" func)
                        ($ args)
                        (lookup func primitives)

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [  ("+",               numericBinop (+)),
                ("-",               numericBinop (-)),
                ("*",               numericBinop (*)),
                ("/",               numericBinop div),
                ("mod",             numericBinop mod),
                ("quotient",        numericBinop quot),
                ("remainder",       numericBinop rem),
                ("symbol?",         applyToSingle isSymbol),
                ("string?",         applyToSingle isString),
                ("number?",         applyToSingle isNumber),
                ("list?",           applyToSingle isList),
                ("symbol->string",  symbolToString),
                ("string->symbol",  stringToSymbol)]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op         []      = throwError $ NumArgs 2 []
numericBinop op singleVal@[_]   = throwError $ NumArgs 2 singleVal
numericBinop op params = mapM unpackNum params >>= return . Number . foldl1 op

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n :: [(Integer, String)] in
                            if null parsed
                                then throwError $ TypeMismatch "number" $ String n
                                else return $ fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum notNum  = throwError $ TypeMismatch "number" notNum

applyToSingle :: (LispVal -> LispVal) -> [LispVal] -> ThrowsError LispVal
applyToSingle op params = if length params == 1
                            then return $ op $ params !! 0
                            else throwError $ NumArgs 1 params

isSymbol :: LispVal -> LispVal
isSymbol (Atom _) = Bool True
isSymbol _        = Bool False

isString :: LispVal -> LispVal
isString (String _) = Bool True
isString _          = Bool False

isNumber :: LispVal -> LispVal
isNumber (Number _) = Bool True
isNumber _          = Bool False

isList :: LispVal -> LispVal
isList (List _)     = Bool True
isList _            = Bool False

symbolToString :: [LispVal] -> ThrowsError LispVal
symbolToString [(Atom a)]   = return $ String a
symbolToString param        = throwError $ TypeMismatch "symbol" $ String $ show param

stringToSymbol :: [LispVal] -> ThrowsError LispVal
stringToSymbol [(String a)] = return $ Atom a --TODO(weidoliang): add space filter to flag "This is a string" as error.
stringToSymbol param        = throwError $ TypeMismatch "string" $ String $ show param
                
main :: IO ()
main = do
    args <- getArgs
    evaled <- return $ liftM show $ readExpr (args !! 0) >>= eval
    putStrLn $ extractValue $ trapError evaled