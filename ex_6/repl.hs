import Control.Monad
import Control.Monad.Error
import Numeric
import System.Environment
import System.IO
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
eval (List [Atom "if", pred, conseq, alt]) = do
    result <- eval pred
    case result of
        Bool False -> eval alt
        otherwise  -> eval conseq
eval (List (Atom func : args)) = mapM eval args >>= apply func


eval badForm = throwError $ BadSpecialForm "Unrecognised special form" badForm

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognised primitive function args" func)
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
                ("string->symbol",  stringToSymbol),
                ("=",               numBoolBinop (==)),
                ("<",               numBoolBinop (<)),
                (">",               numBoolBinop (>)),
                ("/=",              numBoolBinop (/=)),
                (">=",              numBoolBinop (>=)),
                ("<=",              numBoolBinop (<=)),
                ("&&",              boolBoolBinop (&&)),
                ("||",              boolBoolBinop (||)),
                ("string=?",        strBoolBinop (==)),
                ("string<?",        strBoolBinop (<)),
                ("string>?",        strBoolBinop (>)),
                ("string<=?",       strBoolBinop (<=)),
                ("string>=?",       strBoolBinop (>=)),
                ("car",             car),
                ("cdr",             cdr),
                ("cons",            cons),
                ("eq?",             eqv),
                ("eqv?",            eqv)]

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

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2
                                then throwError $ NumArgs 2 args
                                else do 
                                    left <- unpacker $ args !! 0
                                    right <- unpacker $ args !! 1
                                    return $ Bool $ left `op` right

numBoolBinop = boolBinop unpackNum
strBoolBinop = boolBinop unpackStr
boolBoolBinop = boolBinop unpackBool

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s)    = return s
unpackStr (Number s)    = return $ show s
unpackStr (Bool s)      = return $ show s
unpackStr notString      = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b)     = return b
unpackBool notBool      = throwError $ TypeMismatch "boolean" notBool

car :: [LispVal] -> ThrowsError LispVal
car [List (x:xs)]           = return x
car [DottedList (x:xs) _]   = return x
car [badArg]                = throwError $ TypeMismatch "pair" badArg
car badArgList              = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (x : xs) ]        = return $ List xs
cdr [DottedList [_] x]      = return x
cdr [DottedList (_ : xs) x] = return $ DottedList xs x
cdr [badArg]                = throwError $ TypeMismatch "pair" badArg
cdr badArgList              = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x1, List []]              = return $ List [x1]
cons [x, List xs]               = return $ List $ x : xs
cons [x, DottedList xs xlast]   = return $ DottedList (x : xs) xlast
cons [x1, x2]                   = return $ DottedList [x1] x2
cons badArgList                 = throwError $ NumArgs 2 badArgList

eqv :: [LispVal] -> ThrowsError LispVal
eqv [(Bool arg1), (Bool arg2)]      = return $ Bool $ arg1 == arg2
eqv [(Number arg1), (Number arg2)]  = return $ Bool $ arg1 == arg2
eqv [(String arg1), (String arg2)]  = return $ Bool $ arg1 == arg2
eqv [(Atom arg1), (Atom arg2)]      = return $ Bool $ arg1 == arg2
eqv [(DottedList xs x), (DottedList ys y)]   = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [(List arg1), (List arg2)]      = return $ Bool $ (length arg1 == length arg2) && (all eqvPair $ zip arg1 arg2)
    where   eqvPair (x1, x2)    = case eqv [x1, x2] of
                                    Left err -> False
                                    Right (Bool val) -> val
eqv [_,_]   = return $ Bool False
eqv badArgList = throwError $ NumArgs 2 badArgList
                                                        
-- For REPL
flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: String -> IO String
evalString expr = return $ extractValue $ trapError (liftM show $ readExpr expr >>= eval)

evalAndPrint :: String -> IO ()
evalAndPrint expr = evalString expr >>= putStrLn

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m()) -> m ()
until_ pred prompt action = do
    result <- prompt
    if pred result
        then return ()
        else action result >> until_ pred prompt action

runRepl :: IO ()
runRepl = until_ (== "quit") (readPrompt "Lisp>>> ") evalAndPrint

main :: IO ()
main = do
    args <- getArgs
    case length args of
        0 -> runRepl
        1 -> evalAndPrint $ args !! 0
        otherwise -> putStrLn "Program takes only 0 or 1 argument"
