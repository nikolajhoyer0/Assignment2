-- | This module defines a simple command line interface for the SubScript
-- interpreter.  If your solution is correct, this module should just
-- work.
module Subs
       (main)
where
import SubsAst
import SubsParser
import SubsInterpreter

import Control.Monad(forM_)
import Data.List(intercalate)
import qualified Data.Map as Map
import System.Environment(getArgs)


-- | nice display of JavaScript values
nice :: Value -> String
nice (IntVal v) = show v
nice TrueVal = "true"
nice FalseVal = "false"
nice (StringVal s) = show s
nice UndefinedVal = "undefined"
nice (ArrayVal vs) = "["++ intercalate ", " (map nice vs) ++"]"

run :: Program -> IO ()
run p =
  case runProg p of
    Left e -> error $ show e
    Right res ->
      forM_ (Map.toList res) (\(n,v) -> putStrLn $ n ++ " = " ++ nice v)


main :: IO ()
main = do args <- getArgs
          case args of
            ["-i", file] -> do
              s <- readFile file
              run $ read s
            ["-p", file] -> do
              p <- parseFile file
              putStrLn $ show p
            [file] -> do
              pp <- parseFile file
              case pp of
                Left e -> error $ show e
                Right p -> run p
            _ ->
              error "Invalid arguments!"
