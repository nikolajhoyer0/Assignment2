module Parser.Impl where

import SubsAst
import Text.ParserCombinators.ReadP

{------------------}
{- Parsing errors -}
{------------------}

data ParseError = ParseError String
                deriving (Show, Eq)

ambiguousError :: (Program, String) -> (Program, String) -> [(Program, String)]
                  -> String
ambiguousError x y zs = "Ambiguous parse. Found " ++ show (length zs + 2) ++
  " results." ++ " First two were: " ++ show x ++ " and " ++ show y

noresultError :: String
noresultError = "No parse results found"

remainderError :: String -> String
remainderError s = "Found result, but could not parse remainder: " ++ s

{--------------------}
{- Global variables -}
{--------------------}

alphabet :: String
alphabet = '_' : ['A'..'Z'] ++ ['a'..'z']

digits :: String
digits = ['0'..'9']

alphaNum :: String
alphaNum = alphabet ++ digits

{---------------}
{- Main parser -}
{---------------}

parseString :: String -> Either ParseError Program
parseString s =
  let parse = readP_to_S (pStms <* (skipSpaces >> eof)) s
  in case parse of
    []          -> Left $ ParseError noresultError
    x:y:zs      -> Left $ ParseError $ ambiguousError x y zs
    [(_, x:xs)] -> Left $ ParseError $ remainderError (x:xs)
    [(x, "")]   -> Right x

parseFile :: FilePath -> IO (Either ParseError Program)
parseFile path = parseString <$> readFile path

{----------------------}
{- Individual parsers -}
{----------------------}

{- Statements -}
pStms :: ReadP Program
pStms = option e s
  where e = Prog []
        s = do stms <- pStm
               return $ Prog stms

pStm :: ReadP [Stm]
pStm = endBy (pVarDec +++ pExprStm) (token $ char ';')

{- Parser for Ident tokens -}
pIdent :: ReadP Ident
pIdent = do
  i  <- satisfy (`elem` alphabet)
  is <- many $ satisfy (`elem` alphaNum)
  if notReserved (i:is) then return (i:is) else pfail

{- Variable declaration. Note: does not support empty assignments -}
pVarDec :: ReadP Stm
pVarDec = do
  _   <- token $ string "var "
  s   <- pIdent
  ao  <- pAssignOpt
  return $ VarDecl s (Just $ Assign s ao)

pAssignOpt :: ReadP Expr
pAssignOpt = option empty nonempty
  where empty = Undefined
        nonempty = do
          _ <- token $ char '='
          pExpr1

{- Top level expression parser -}
pExprStm :: ReadP Stm
pExprStm = do
  e <- pExpr
  return $ ExprAsStm e

pExpr :: ReadP Expr
pExpr = pComma

pComma :: ReadP Expr
pComma = chainl1 pExpr1 dlim
  where
    dlim :: ReadP (Expr -> Expr -> Expr)
    dlim = do
      _ <- token $ char ','
      return $ \ e1 e2 -> Comma e1 e2

pExpr1 :: ReadP Expr
pExpr1 = pExpr2 +++ pAfterident

pExpr2 :: ReadP Expr
pExpr2 = chainl1 pExpr3 $ opToFunS "===" "==="

pExpr3 :: ReadP Expr
pExpr3 = chainl1 pExpr4 $ opToFun '<' "<"

pExpr4 :: ReadP Expr
pExpr4 = chainl1 pExpr5 $ opToFun '+' "+" <++ opToFun '-' "-"

pExpr5 :: ReadP Expr
pExpr5 = chainl1 pExpr6 $ opToFun '*' "*" <++ opToFun '%' "%"

pExpr6 :: ReadP Expr
pExpr6 = pString +++ pNumber +++ pTrue +++ pFalse +++ pUndefined +++ pParens

{- Parser for Number tokens -}
pNumber :: ReadP Expr
pNumber = token $ do
  d  <- satisfy (\x -> x `elem` '-' : digits)
  ds <- many $ satisfy (`elem` digits)
  let n = trimZeros $ d : ds
  if numValid n then return $ Number $ read n else pfail

{- Parser for String tokens -}
pString :: ReadP Expr
pString = token $ do
  let dlim q = q == '\''
  s <- between (satisfy dlim) (satisfy dlim) (many $ satisfy $ fmap not dlim)
  return $ String s

{- True terminal -}
pTrue :: ReadP Expr
pTrue = token $ do
  _ <- string "true"
  return TrueConst

{- False terminal -}
pFalse :: ReadP Expr
pFalse = token $ do
  _ <- string "false"
  return FalseConst

{- Undefined Terminal -}
pUndefined :: ReadP Expr
pUndefined = token $ do
  _ <- string "undefined"
  return Undefined

{- Parser for Expressions inside square brackets -}
pSquare :: ReadP Expr
pSquare = exprInBetween '[' ']' pExprs

{- Parser for array comprehensions -}
pArrayComprExpr :: ReadP Expr
pArrayComprExpr = undefined

{- Parser for Expressions inside parentheses -}
pParens :: ReadP Expr
pParens = exprInBetween '(' ')' pExpr

pAfterident :: ReadP Expr
pAfterident = pAssignOpt +++ pFunCall

pFunCall :: ReadP Expr
pFunCall = call +++ exprs
  where call = do
          _   <- char '.'
          i   <- pIdent
          fc  <- pFunCall
          return $ Call i [fc]
        exprs = exprInBetween '(' ')' pExprs

pExprs :: ReadP Expr
pExprs = do
  e  <- pExpr1
  ce <- pCommaExprs
  return $ Comma e ce

pCommaExprs :: ReadP Expr
pCommaExprs = do
  _ <- token $ char ','
  pExprs

pArrayCompr :: ReadP Expr
pArrayCompr = undefined

{--------------------}
{- Helper functions -}
{--------------------}

{- Helper function for tokenizing -}
token :: ReadP a -> ReadP a
token p = skipSpaces >> p

{- Checks that a string is not a reserved keyword -}
notReserved :: String -> Bool
notReserved s =
  let res = ["var", "true", "false", "undefined", "for", "of", "or", "if"]
  in notElem s res

{- Checks for proper length of a number, given in the assignment text -}
numValid :: String -> Bool
numValid []     = False
numValid [_]    = True
numValid (s:ss) = if s == '-' then length ss <= 8 else length ss <= 7

{- Checks for leading zeros in a number and then strips them -}
trimZeros :: String -> String
trimZeros "0"      = "0"
trimZeros ('0':ss) = trimZeros ss
trimZeros ('-':ss) = '-' : trimZeros ss
trimZeros s        = s

{- Helper to convert character operations to functions in the language -}
opToFun :: Char -> String -> ReadP (Expr -> Expr -> Expr)
opToFun op fun = do
  _ <- token $ char op
  return $ \ e1 e2 -> Call fun [e1, e2]

{- Helper to convert string operations to functions in the language -}
opToFunS :: String -> String -> ReadP (Expr -> Expr -> Expr)
opToFunS op fun = do
  _ <- token $ string op
  return $ \ e1 e2 -> Call fun [e1, e2]

{- Helper function for matching expressions in between brackets -}
exprInBetween :: Char -> Char -> ReadP Expr -> ReadP Expr
exprInBetween start end expr = do
  let open   p = p == start
      close  p = p == end
  between (token $ satisfy open) (token $ satisfy close) expr

-- FOR TESTING IN GHCI!!!
--  readP_to_S (pNumber <* (skipSpaces >> eof)) "123 fdsafsda fdsa"

{- end -}
