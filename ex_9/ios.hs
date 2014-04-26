import Control.Monad
import Control.Monad.Error
import Data.IORef
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
             | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
             | Func {params :: [String]       -- names of the parameters
                    ,vararg :: (Maybe String)  -- whehter function accepts varaible-length list of arguments
                    ,body :: [LispVal]        -- function body as list of expressions
                    ,closure :: Env }         -- the environment the function was created in
             | IOFunc ([LispVal] -> IOThrowsError LispVal)
             | Port Handle

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
showVal (PrimitiveFunc _) = "<primitive>"
showVal (Func {params = args, vararg = varargs, body = body, closure = env}) =
    "(lambda (" ++ unwords (map show args) ++
        (case varargs of
            Nothing -> ""
            Just arg -> " . " ++ arg) ++ ") ...)"
showVal (Port _) = "<IO port>"
showVal (IOFunc _) = "<IO primitive>"
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

-- Make LispError and instance of show so that it maybe printed.
instance Show LispError where show = showError

showError :: LispError -> String
showError (NumArgs expected found) = "Expected " ++ show expected ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected ++ ", found " ++ show found
showError (Parser parseErr) = "Parse error at " ++ show parseErr
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ show func
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (Default message) = message

-- Make LispError an instance of error so that throwError and catchError maybe used with it.
instance Error LispError where 
    noMsg = Default "An error has occurred"
    strMsg = Default

{--
Definition of Error:

class Error a where
    noMsg :: a              -- creates an exception without a message
    strMsg :: String -> a   -- creates an exception with a message.

sample instances:
    Error String    - throwing a string as an error
    Error IOError   - IO related error
    Error LispError - our usage of error.

The error monad:

class Monad m => MonadError e m | m -> e where
    throwError :: e -> m a                  -- used within a monadic computation to begin exception processing.
    catchError :: m a -> (e -> m a) -> m a  -- a handler function to handle previous errors and return to normal execution.
                                            -- e.g. do { action_1; action_2; action_3 } `catchError` handler

sample instances:
    Error e => MonadError e (Either e)
--}

-- Catch the LispError that has been previous generated using throwError, and convert it to string.
trapError action = catchError action (return . show)

type ThrowsError = Either LispError

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

readOrThrow :: Parser a -> String -> ThrowsError a
readOrThrow parser input = case parse parser "lisp" input of
    Left err -> throwError $ Parser err
    Right val -> return val
    
readExpr = readOrThrow parseExpr
readExprList = readOrThrow (endBy parseExpr spaces)


-- Evaluator section.

{--
data IORef a
a mutable variable in the IO monad

newIORef :: a -> IO (IORef a)   -- build a new IORef
readIORef :: IORef a -> IO a    -- read the value of IORef
writeIORef :: IORef a -> a -> IO () -- write a new value into IORef
emodifyIORef :: IORef a -> (a -> a) -> IO () -- mutable the contents of an IORef
--}
type Env = IORef [(String, IORef LispVal)]

-- Creates a new empty environment.
nullEnv :: IO Env
nullEnv = newIORef []

-- ErrorT: the error monad transformer which can be used to add error handling to other monads.
-- In this case, we are adding LispError to the IO monad.
-- Usage: newtype ErrorT e m a
-- where: e - error type, m - inner monnad
-- Constructor: runErrorT :: m (Either e a)
-- partial type, the the following usage, it would be :
--  IOThrowsError String
--  IOThrowsError LispVal
type IOThrowsError = ErrorT LispError IO

-- Converts a ThrowsError to IOThrowsError
liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err)   = throwError err
liftThrows (Right val)  = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runErrorT (trapError action) >>= return . extractValue

-- | Attempt to look up var in the given environment, if found, return True
-- otherwise, return False.
isBound :: Env -> String -> IO Bool
isBound envRef var = readIORef envRef >>= return . maybe False (const True) . lookup var
-- lookup :: Eq a => a -> [(a, b)] -> Maybe b
-- Lookup the value of a key in a map.

-- maybe :: b -> (a -> b) -> Maybe a -> b
-- maybe n _ Nothing = n
-- maybe _ f (Just x) = f x
-- maybe default_value function maybe_value =
--      if maybe_value is Nothing, then return default_value
--      else return function(maybe_value)

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var = do
    env <- liftIO $ readIORef envRef
    maybe   (throwError $ UnboundVar "Getting an unbound variable" var)
            (liftIO . readIORef)
            (lookup var env)

setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value = do
    env <- liftIO $ readIORef envRef
    maybe   (throwError $ UnboundVar  "Setting an unbound variable" var)
            (liftIO . (flip writeIORef value)) -- writeIORef :: IORef LispVal -> LispVal -> IO ()
            (lookup var env) -- IORef LispVal
    return value
-- flip :: (a -> b -> c) -> b -> a -> c
-- flip f b a = f a b
-- writeIORef :: IORef a -> a -> IO () -- write a new value into IORef

-- | define a new variable with the given value, if the variable is around bound,
-- then just update its value; otherwise, create a new variable and assign the value.
defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = do
    alreadyDefined <- liftIO $ isBound envRef var
    if alreadyDefined
        then setVar envRef var value >> return value
        else liftIO $ do
            valueRef <- newIORef value -- creates a new IORef LispVal
            env <- readIORef envRef  -- returns ()?
            writeIORef envRef ((var, valueRef) : env)
            return value
-- readIORef :: IORef a -> IO a -- read the value of IORef
-- writeIORef :: IORef a -> a -> IO () -- write a new value into IORef

-- Creates and Env from the lisp of (String, LispVal).
bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
    where   extendEnv binds env = liftM (++ env) (mapM addBinding bindings)
            addBinding (var, value) = do    ref <- newIORef value
                                            return (var, ref)

eval :: Env -> LispVal -> IOThrowsError LispVal
eval env val@(String _) = return val
eval env val@(Number _) = return val
eval env val@(Bool _)   = return val
eval env (Atom id) = getVar env id
eval env (List [Atom "quote", val]) = return val
eval env (List [Atom "if", pred, conseq, alt]) = do
    result <- eval env pred
    case result of
        Bool False -> eval env alt
        otherwise  -> eval env conseq
eval env (List [Atom "set!", Atom var, form]) = eval env form >>= setVar env var
eval env (List [Atom "define", Atom var, form]) = eval env form >>= defineVar env var
eval env (List (Atom "define" : List (Atom var : params) : body)) = 
    makeNormalFunc env params body >>= defineVar env var
eval env (List (Atom "define" : DottedList (Atom var : params) varargs : body)) =
    makeVarargs varargs env params body >>= defineVar env var
eval env (List (Atom "lambda" : List params : body)) =
    makeNormalFunc env params body
eval env (List (Atom "lambda" : DottedList params varargs : body)) =
    makeVarargs varargs env params body
eval env (List (Atom "lambda" : varargs@(Atom _) : body)) =
    makeVarargs varargs env [] body
eval env (List [Atom "load", String filename]) =
    load filename >>= liftM last . mapM (eval env)
eval env (List (function : args)) = do
    func <- eval env function
    argVals <- mapM (eval env) args
    apply func argVals
eval env badForm = throwError $ BadSpecialForm "Unrecognised special form" badForm


apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc func) args = liftThrows $ func args
apply (Func params varargs body closure) args =
    if num params /= num args && varargs == Nothing
        then throwError $ NumArgs (num params) args
        else (liftIO $ bindVars closure $ zip params args) >>= bindVarArgs varargs >>= evalBody
    where
        remainingArgs = drop (length params) args
        num = toInteger . length
        evalBody env = liftM last $ mapM (eval env) body
        bindVarArgs arg env = case arg of
            Just argName -> liftIO $ bindVars env [(argName, List $ remainingArgs)]
            Nothing -> return env
apply (IOFunc func) args = func args

makeFunc varargs env params body = return $ Func (map showVal params) varargs body env
makeNormalFunc = makeFunc Nothing
makeVarargs = makeFunc . Just . showVal

applyProc :: [LispVal] -> IOThrowsError LispVal
applyProc [func, List args] = apply func args
applyProc (func : args)     = apply func args

makePort :: IOMode -> [LispVal] -> IOThrowsError LispVal
makePort mode [String filename] = liftM Port $ liftIO $ openFile filename mode

closePort :: [LispVal] -> IOThrowsError LispVal
closePort [Port port]   = liftIO $ hClose port >> (return $ Bool True)
closePort _             = return $ Bool False

readProc :: [LispVal] -> IOThrowsError LispVal
readProc []             = readProc [Port stdin]
readProc [Port port]    = (liftIO $ hGetLine port) >>= liftThrows . readExpr

writeProc :: [LispVal] -> IOThrowsError LispVal
writeProc [obj]             = writeProc [obj, Port stdout]
writeProc [obj, Port port]  = liftIO $ hPrint port obj >> (return $ Bool True)

readContents :: [LispVal] -> IOThrowsError LispVal
readContents [String filename] = liftM String $ liftIO $ readFile filename

load :: String -> IOThrowsError [LispVal]
load filename = (liftIO $ readFile filename) >>= liftThrows . readExprList

readAll :: [LispVal] -> IOThrowsError LispVal
readAll [String filename] = liftM List $ load filename

-- Creates and iniitial environment with built-in functions. 
primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= (flip bindVars $ map (makeFunc IOFunc) ioPrimitives
                                                ++ map (makeFunc PrimitiveFunc) primitives)
    where
        makeFunc constructor (var, func) = (var, constructor func)


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

-- IO operations
ioPrimitives :: [(String, [LispVal] -> IOThrowsError LispVal)]
ioPrimitives = [("apply", applyProc),
                ("open-input-file", makePort ReadMode),
                ("open-output-file", makePort WriteMode),
                ("close-input-port", closePort),
                ("close-output-port", closePort),
                ("read", readProc),
                ("write", writeProc),
                ("read-contents", readContents),
                ("read-all", readAll)]
                

-- For REPL
flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ liftM show $ ( (liftThrows $ readExpr expr) >>= eval env )

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m()) -> m ()
until_ pred prompt action = do
    result <- prompt
    if pred result
        then return ()
        else action result >> until_ pred prompt action

runOne :: [String] -> IO ()
runOne args = do
    env <- primitiveBindings >>= flip bindVars [("args", List $ map String $ drop 1 args)]
    (runIOThrows $ liftM show $ eval env (List [Atom "load", String (args !! 0)])) >>= hPutStrLn stderr 

runRepl :: IO ()
runRepl = primitiveBindings >>= ( until_ (== "quit") (readPrompt "Lisp>>> ") ) . evalAndPrint
-- here, the action has become the curried function (evalAndPrint env), and all the subsequent
-- evaluation within until_, the futher modified env will be passed along.

main :: IO ()
main = do
    args <- getArgs
    if null args then runRepl else runOne $ args
