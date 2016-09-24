module Parser.Impl where

import SubsAst
import Text.ParserCombinators.ReadP

data ParseError = ParseError String
                deriving (Show, Eq)

{--------------------}
{- Global variables -}
{--------------------}

alphabet :: String
alphabet = '_' : ['A'..'Z'] ++ ['a'..'z']

alphaNum :: String
alphaNum = alphabet ++ ['0'..'9']

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

{---------------}
{- Main parser -}
{---------------}
parseString :: String -> Either ParseError Program
parseString = undefined

parseFile :: FilePath -> IO (Either ParseError Program)
parseFile path = parseString <$> readFile path

{----------------------}
{- Individual parsers -}
{----------------------}

{- Statements -}
pStms :: ReadP Program
pStms = do stms <- pStm
           return $ Prog stms

pStm :: ReadP [Stm]
pStm = endBy (pVarDec +++ pExpr) dlim
  where dlim :: ReadP Char
        dlim = do skipSpaces; d <- char ';'; skipSpaces
                  return d

{- Variable declaration -}
pVarDec :: ReadP Stm
pVarDec = token $ do
  var <- string "var"
  s   <- pIdent
  ao  <- pAssignOpt
  return $ VarDecl s (Just $ Assign s ao)

pAssignOpt :: ReadP Expr
pAssignOpt = token $ do
  char '='
  pExpr1

{- Top level expression parser -}
pExpr :: ReadP Stm
pExpr = do expr <- pExpr1 +++ pComma
           return $ ExprAsStm expr

pComma :: ReadP Expr
pComma = chainl1 pExpr1 dlim
  where
    dlim :: ReadP (Expr -> Expr -> Expr)
    dlim = do skipSpaces; _ <- char ','; skipSpaces  -- Consume ';' and spaces
              return $ \ e1 e2 -> Comma e1 e2

{- -}
pExpr1 :: ReadP Expr
pExpr1 = pExpr2  -- Still need to implement Ident AfterIdent

{- -}
pExpr2 :: ReadP Expr
pExpr2 = pExpr3 +++ pEq

{- -}
pEq :: ReadP Expr
pEq = chainl1 pExpr3 eqOp
  where
    eqOp :: ReadP (Expr -> Expr -> Expr)
    eqOp = do skipSpaces; _ <- string "==="; skipSpaces  -- Consume op and spaces
              return $ \ e1 e2 -> Call "===" [e1, e2]

{- -}
pExpr3 :: ReadP Expr
pExpr3 = pExpr4 +++ pLess

{- -}
pLess :: ReadP Expr
pLess = chainl1 pExpr4 lessOp
  where
    lessOp :: ReadP (Expr -> Expr -> Expr)
    lessOp = do skipSpaces; _ <- char '<'; skipSpaces  -- Consume op and spaces
                return $ \ e1 e2 -> Call "<" [e1, e2]

{- -}
pExpr4 :: ReadP Expr
pExpr4 = pExpr5 +++ pAddSub

{- -}
pAddSub :: ReadP Expr
pAddSub = chainl1 pExpr5 (addOp +++ subOp)
  where
    addOp :: ReadP (Expr -> Expr -> Expr)
    addOp = do skipSpaces; _ <- char '+'; skipSpaces  -- Consume op and spaces
               return $ \ e1 e2 -> Call "+" [e1, e2]
    subOp :: ReadP (Expr -> Expr -> Expr)
    subOp = do skipSpaces; _ <- char '-'; skipSpaces  -- Consume op and spaces
               return $ \ e1 e2 -> Call "-" [e1, e2]

{- -}
pExpr5 :: ReadP Expr
pExpr5 = pExpr6 +++ pMulMod


{- -}
pMulMod :: ReadP Expr
pMulMod = chainl1 pExpr6 (mulOp +++ modOp)
  where
    mulOp :: ReadP (Expr -> Expr -> Expr)
    mulOp = do skipSpaces; _ <- char '*'; skipSpaces  -- Consume op and spaces
               return $ \ e1 e2 -> Call "*" [e1, e2]
    modOp :: ReadP (Expr -> Expr -> Expr)
    modOp = do skipSpaces; _ <- char '%'; skipSpaces  -- Consume op and spaces
               return $ \ e1 e2 -> Call "%" [e1, e2]

{- -}
pExpr6 :: ReadP Expr
pExpr6 = pString +++ pNumber +++ pTrue +++ pFalse +++ pUndefined +++ pParens

{- Parser for Number tokens -}
pNumber :: ReadP Expr
pNumber = token $ do
  d  <- satisfy (\x -> x `elem` '-' : ['0'..'9'])
  ds <- many $ satisfy (\y -> y `elem` ['0'..'9'])
  let n = trimZeros $ d : ds
  if numValid n then return $ Number $ read n else pfail

{- Parser for String tokens -}
pString :: ReadP Expr
pString = token $ do
  let dlim q = q == '\''
  s <- between (satisfy dlim) (satisfy dlim) (many $ satisfy $ fmap not dlim)
  return $ String s

{- Parser for Expressions inside brackets -}
pParens :: ReadP Expr
pParens = token $ do
  let open   p = p == '('
      close  p = p == ')'
  between (satisfy open) (satisfy close) pExpr1

{- Parser for Ident tokens -}
pIdent :: ReadP Ident
pIdent = token $ do
  i  <- satisfy (`elem` alphabet)
  is <- many $ satisfy (`elem` alphaNum)
  if notReserved (i:is) then return (i:is) else pfail

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


-- FOR TESTING IN GHCI!!!
--  readP_to_S (pNumber <* (skipSpaces >> eof)) "123 fdsafsda fdsa"

{- end -}
