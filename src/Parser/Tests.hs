module Parser.Tests where

import Data.List
import Parser.Impl
import SubsAst
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Text.ParserCombinators.ReadP



main :: IO ()
main = defaultMain tests


tests :: TestTree
tests = testGroup "Tests" [unitTests]


unitTests :: TestTree
unitTests = testGroup "Unit Tests"
  [ --
    -- pNumber unit tests
    --

    testCase "pNumber parses (+) 1-digit prime" $
      last (readP_to_S pNumber "7") @?=
        (Number 7, ""),

    testCase "pNumber parses (-) 1-digit prime" $
      last (readP_to_S pNumber "-3") @?=
        (Number (-3), ""),

    testCase "pNumber parses (+) 1-digit prime w/leading zeros" $
      last (readP_to_S pNumber "007") @?=
        (Number 7, ""),

    testCase "pNumber parses (-) 1-digit prime w/leading zeros" $
      last (readP_to_S pNumber "-0000003") @?=
        (Number (-3), ""),

    testCase "pNumber parses (+) zero" $
      last (readP_to_S pNumber "0") @?=
        (Number 0, ""),

    testCase "pNumber parses (-) zero" $
      last (readP_to_S pNumber "-0") @?=
        (Number (-0), ""),

    testCase "pNumber parses (+) zero w/leading zeros" $
      last (readP_to_S pNumber "000000000") @?=
        (Number 0, ""),

    testCase "pNumber parses (-) zero w/leading zeros" $
      last (readP_to_S pNumber "-000000000") @?=
        (Number (-0), ""),

    testCase "pNumber parses (+) 8-digit number" $
      last (readP_to_S pNumber "12345678") @?=
        (Number 12345678, ""),

    testCase "pNumber parses (-) 8-digit number" $
      last (readP_to_S pNumber "-12345678") @?=
        (Number (-12345678), ""),

    testCase "pNumber parses (+) 8-digit number w/leading zeros" $
      last (readP_to_S pNumber "00012345678") @?=
        (Number 12345678, ""),

    testCase "pNumber parses (-) 8-digit number w/leading zeros" $
      last (readP_to_S pNumber "-000012345678") @?=
        (Number (-12345678), ""),

    testCase "pNumber parses (+) 9-digit number fails" $
      last (readP_to_S pNumber "123456789") == (Number 123456789, "") @?=
        False,

    testCase "pNumber parses (-) 9-digit number fails" $
      last (readP_to_S pNumber "-123456789") == (Number (-123456789), "") @?=
        False,

    --
    -- pString unit tests
    --

    testCase "pString parses empty string" $
      last (readP_to_S pString "''") @?= (String "",""),

    testCase "pString parses string w/single quote fails (1)" $
      last (readP_to_S pString "'''") == (String "'","") @?= False,

    testCase "pString parses string w/single quote fails (2)" $
      last (readP_to_S pString "'abc'abc'") == (String "'","") @?= False,

    testCase "pString parses numeric as expected" $
      last (readP_to_S pString "' 1 2 3 4 5 6 7 8 9 0 '") @?=
        (String " 1 2 3 4 5 6 7 8 9 0 ",""),

    testCase "pString parses lower case as expected" $
      last (readP_to_S pString "' abcdefghijklmnopqrstuvwxyz '") @?=
        (String " abcdefghijklmnopqrstuvwxyz ",""),

    testCase "pString parses upper case as expected" $
      last (readP_to_S pString "' ABCDEFGHIJKLMNOPQRSTUVWXYZ '") @?=
        (String " ABCDEFGHIJKLMNOPQRSTUVWXYZ ",""),

    testCase "pString parses other chars as expected" $
      last (readP_to_S pString "' !@#$%^&*()-=_+[]{}|;:,.<>/?~`æøå '") @?=
        (String " !@#$%^&*()-=_+[]{}|;:,.<>/?~`æøå ",""),

    --
    -- pTrue and pFalse unit tests
    --

    testCase "pFalse parses 'false' as token" $
      (readP_to_S pFalse "false") @?= [(FalseConst,"")],

    testCase "pFalse parses 'true' as token fails" $
      (readP_to_S pFalse "true") @?= (readP_to_S pfail "true"),

    testCase "pTrue parses 'true' as token" $
      (readP_to_S pTrue "true") @?= [(TrueConst,"")],

    testCase "pTrue parses 'false' as token fails" $
      (readP_to_S pTrue "false") @?= (readP_to_S pfail "false"),

    --
    -- pUndefined
    --

    testCase "pUndefined parses 'undefined' as token" $
      (readP_to_S pUndefined "undefined") @?= [(Undefined,"")],


    --
    -- pExpr parser unit tests
    --

    testCase "pExpr parses basic addition" $
      last (readP_to_S pExpr "-1 + 2") @?=
        (Call "+" [Number (-1),Number 2],""),

    testCase "pExpr parses multiple additions w/LAG" $
      last (readP_to_S pExpr "1 + 2 + 3") @?=
        (Call "+" [Call "+" [Number 1,Number 2], Number 3],""),


    testCase "pExpr parses basic subtraction" $
      last (readP_to_S pExpr "-1 - 2") @?=
        (Call "-" [Number (-1),Number 2],""),

    testCase "pExpr parses multiple subtractions w/LAG" $
      last (readP_to_S pExpr "1 - 2 - 3") @?=
        (Call "-" [Call "-" [Number 1,Number 2], Number 3],""),


    testCase "pExpr parses basic multiplication" $
      last (readP_to_S pExpr "-1 * 2") @?=
        (Call "*" [Number (-1),Number 2],""),

    testCase "pExpr parses multiple multiplications w/LAG" $
      last (readP_to_S pExpr "1 * 2 * 3") @?=
        (Call "*" [Call "*" [Number 1,Number 2], Number 3],""),


    testCase "pExpr parses basic modulo" $
      last (readP_to_S pExpr "-1 % 2") @?=
        (Call "%" [Number (-1),Number 2],""),

    testCase "pExpr parses multiple modulos w/LAG" $
      last (readP_to_S pExpr "1 % 2 % 3") @?=
        (Call "%" [Call "%" [Number 1,Number 2], Number 3],""),


    testCase "pExpr parses basic less than" $
      last (readP_to_S pExpr "-1 < 2") @?=
        (Call "<" [Number (-1),Number 2],""),

    testCase "pExpr parses multiple less thans w/LAG" $
      last (readP_to_S pExpr "1 < 2 < 3") @?=
        (Call "<" [Call "<" [Number 1,Number 2], Number 3],""),


    testCase "pExpr parses basic equality" $
      last (readP_to_S pExpr "-1 === 2") @?=
        (Call "===" [Number (-1),Number 2],""),

    testCase "pExpr parses multiple equalities w/LAG" $
      last (readP_to_S pExpr "1 === 2 === 3") @?=
        (Call "===" [Call "===" [Number 1,Number 2], Number 3],""),


    testCase "pExpr correct precedence for addition vs multiplication" $
      last (readP_to_S pExpr "1 + 2 * 3") @?=
        (Call "+" [Number 1, Call "*" [Number 2,Number 3]],""),

    testCase "pExpr correct precedence for addition vs modulo" $
      last (readP_to_S pExpr "1 + 2 % 3") @?=
        (Call "+" [Number 1, Call "%" [Number 2,Number 3]],""),


    testCase "pExpr correct precedence for subtraction vs multiplication" $
      last (readP_to_S pExpr "1 - 2 * 3") @?=
        (Call "-" [Number 1, Call "*" [Number 2,Number 3]],""),

    testCase "pExpr correct precedence for subtraction vs modulo" $
      last (readP_to_S pExpr "1 - 2 % 3") @?=
        (Call "-" [Number 1, Call "%" [Number 2,Number 3]],""),


    testCase "pExpr correct precedence for parens (1)" $
      last (readP_to_S pExpr "(1 + 2) * 3") @?=
        (Call "*" [Call "+" [Number 1,Number 2], Number 3],""),

    testCase "pExpr correct precedence for parens (2)" $
      last (readP_to_S pExpr "(1 + 2) * (3 - 4)") @?=
        (Call "*" [Call "+" [Number 1, Number 2],
                   Call "-" [Number 3, Number 4]],""),


    testCase "pExpr parses comma separated exprs" $
      last (readP_to_S pExpr "1,2,3,4+5") @?=
        ((Comma (Comma (Comma (Number 1) (Number 2)) (Number 3))
          (Call "+" [Number 4, Number 5])),""),


    --
    -- pString parser unit tests
    --

    --
    -- pString parser unit tests
    --

    --
    -- pString parser unit tests
    --

    --
    -- pString parser unit tests
    --

    --
    -- pString parser unit tests
    --

    --
    -- pString parser unit tests
    --


    testCase "-" $
      True @?=
        True
  ]




