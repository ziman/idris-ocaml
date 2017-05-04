module Main where

import Idris.AbsSyntax
import Idris.Core.TT
import Idris.ElabDecls
import Idris.Main
import Idris.ModeCommon
import Idris.REPL

import IRTS.CodegenOCaml
import IRTS.Compiler

import System.Environment
import System.Exit

import Paths_idris_ocaml

data Opts = Opts { inputs :: [FilePath],
                   output :: FilePath }

showUsage = do putStrLn "Usage: idris-ocaml <ibc-files> [-o <output-file>]"
               exitWith ExitSuccess

getOpts :: IO Opts
getOpts = do xs <- getArgs
             return $ process (Opts [] "a.ml") xs
  where
    process opts ("-o":o:xs) = process (opts { output = o }) xs
    process opts ("--yes-really":xs) = process opts xs -- TODO
    process opts (x:xs) = process (opts { inputs = x:inputs opts }) xs
    process opts [] = opts

c_main :: Opts -> Idris ()
c_main opts = do elabPrims
                 loadInputs (inputs opts) Nothing
                 mainProg <- elabMain
                 ir <- compile (Via IBCFormat "ocaml") (output opts) (Just mainProg)
                 runIO $ codegenOCaml ir

main :: IO ()
main = do opts <- getOpts
          if (null (inputs opts))
             then showUsage
             else runMain (c_main opts)
