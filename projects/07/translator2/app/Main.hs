{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.State (State, evalState, get, modify)
import Data.Text (Text, pack, strip)
import qualified Data.Text as T (lines, takeWhile, unlines, unwords, words)
import qualified Data.Text.IO as IO
import Fmt ((+|), (|+))
import System.Environment (getArgs)
import System.FilePath (replaceExtension, takeBaseName)

parseTemp :: Text -> Text
parseTemp "0" = "R5"
parseTemp "1" = "R6"
parseTemp "2" = "R7"
parseTemp "3" = "R8"
parseTemp "4" = "R9"
parseTemp "5" = "R10"
parseTemp "6" = "R11"
parseTemp "7" = "R12"
parseTemp offset = error $ "Uknown temp location: " +| offset |+ ""

arithmetic :: Text -> Text
arithmetic str = "@SP\nAM=M-1\nD=M\nA=A-1\nM=M" +| str |+ "D"

bit :: Text -> Text
bit str = "@SP\nA=M-1\nM=" +| str |+ "M"

{- ORMOLU_DISABLE -}
logic :: Text -> Text -> State Int Text
logic fileName str = do
  counter <- get
  modify (+ 1)
  return $
    "@SP\nAM=M-1\nD=M\nA=A-1\nD=M-D\nM=0\n\
    \@TRUTHY." +| fileName |+ "." +| counter |+ "\n\
    \D;" +| str |+ "\n\
    \@DONE." +| fileName |+ "." +| counter |+ "\n\
    \0;JMP\n\
    \(TRUTHY." +| fileName |+ "." +| counter |+ ")\n\
    \@SP\nA=M-1\nM=-1\n\
    \(DONE." +| fileName |+ "." +| counter |+ ")"
{- ORMOLU_ENABLE -}

pushBody = "@SP\nM=M+1\nA=M-1\nM=D"

push :: Text -> Text -> Text -> Text
push _ "constant" offset = "@" +| offset |+ "\nD=A\n" +| pushBody
push _ "pointer" "0" = "@THIS\nD=M\n" +| pushBody
push _ "pointer" "1" = "@THAT\nD=M\n" +| pushBody
push _ "temp" offset = "@" +| parseTemp offset |+ "\nD=M\n" +| pushBody
push fileName "static" offset = "@" +| fileName |+ "." +| offset |+ "\nD=M\n" +| pushBody
push fileName "local" offset = "@LCL\nD=M\n@" +| offset |+ "\nA=D+A\nD=M\n" +| pushBody
push fileName "argument" offset = "@ARG\nD=M\n@" +| offset |+ "\nA=D+A\nD=M\n" +| pushBody
push fileName "this" offset = "@THIS\nD=M\n@" +| offset |+ "\nA=D+A\nD=M\n" +| pushBody
push fileName "that" offset = "@THAT\nD=M\n@" +| offset |+ "\nA=D+A\nD=M\n" +| pushBody
push _ t offset = error $ "push " +| t |+ " " +| offset |+ " not implemented"

popBody = "D=A+D\n@R13\nM=D\n@SP\nAM=M-1\nD=M\n@R13\nA=M\nM=D"

pop :: Text -> Text -> Text -> Text
pop _ "pointer" "0" = "@SP\nAM=M-1\nD=M\n@THIS\nM=D"
pop _ "pointer" "1" = "@SP\nAM=M-1\nD=M\n@THAT\nM=D"
pop _ "temp" offset = "@SP\nAM=M-1\nD=M\n@" +| parseTemp offset |+ "\nM=D"
pop _ "local" offset = "@LCL\nD=M\n@" +| offset |+ "\n" +| popBody
pop _ "argument" offset = "@ARG\nD=M\n@" +| offset |+ "\n" +| popBody
pop _ "this" offset = "@THIS\nD=M\n@" +| offset |+ "\n" +| popBody
pop _ "that" offset = "@THAT\nD=M\n@" +| offset |+ "\n" +| popBody
pop fileName "static" offset = "@SP\nAM=M-1\nD=M\n@" +| fileName |+ "." +| offset |+ "\nM=D"
pop _ t offset = error $ "pop " +| t |+ " " +| offset |+ " not implemented"

processItem :: Text -> [Text] -> State Int Text
processItem fileName ["push", location, dest] = return $ push fileName location dest
processItem fileName ["pop", location, dest] = return $ pop fileName location dest
processItem _ ["add"] = return $ arithmetic "+"
processItem _ ["sub"] = return $ arithmetic "-"
processItem _ ["and"] = return $ arithmetic "&"
processItem _ ["or"] = return $ arithmetic "|"
processItem _ ["neg"] = return $ bit "-"
processItem _ ["not"] = return $ bit "!"
processItem fileName ["eq"] = logic fileName "JEQ"
processItem fileName ["lt"] = logic fileName "JLT"
processItem fileName ["gt"] = logic fileName "JGT"
processItem _ a = error $ "Unknown command: " +| T.unwords a |+ ""

parseFile :: Text -> [Text]
parseFile = filter (/= "") . map (strip . T.takeWhile (/= '/')) . T.lines

main :: IO ()
main = do
  args <- getArgs
  let fileName = head args
  file <- IO.readFile fileName

  let fileLines = parseFile file
  let parser = processItem (pack $ takeBaseName fileName) . T.words
  let output = T.unlines $ evalState (mapM parser fileLines) 0

  IO.writeFile (replaceExtension fileName "asm") output
