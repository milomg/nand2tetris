import BasicPrelude
import Control.Monad.State (State, evalState, get, put, runState)
import qualified Data.List
import Data.Text (pack, strip, unpack)
import qualified Data.Text as T (takeWhile)
import Data.Text.Read (decimal)
import System.Directory (getDirectoryContents)
import System.FilePath (addExtension, dropTrailingPathSeparator, replaceExtension, takeBaseName, takeFileName)

{-

**
A text rendition of xkcd.com/1296 for your viewing pleasure.
**

-------|-------------comment-----------------|-----date-------
*       created main loop & timing control	   14 hours ago
|
*       enabled config file parsing	           9 hours ago
|
*       misc bugfixes	                         5 hours ago
|
*       code additions/edits                   4 hours ago
|
*       more code	                             4 hours ago
|\
| *     here have code	                       4 hours ago
| |
| *     aaaaaaaa                               3 hours ago
|/
*       adkfjslkdfjsdklfj                      2 hours ago
|
*       my hands are typing words              2 hours ago
|
*       haaaaaaaaands                          2 hours ago

            As a project drags on, my commit
            messages get less and less informative.

-}

data MyState = MyState
  { counter :: Int,
    functionName :: Text
  }

-- Parse the offset for a temp file, if it is larger than 7 thrown an error
parseTemp :: Text -> Text
parseTemp offset =
  case decimal offset of
    Right (b, _) | b <= 7 -> "R" ++ tshow (5 + b :: Int)
    _ -> error (unpack $ "Uknown temp location: " ++ offset)

-- The string for an arithmetic command like add, sub, and, or.
arithmetic :: Text -> Text
arithmetic str = "@SP\nAM=M-1\nD=M\nA=A-1\nM=M" ++ str ++ "D"

-- The string for a one bit command like neg, not.
--    > Two bytes meet.  The first byte asks, “Are you ill?”
--    > The second byte replies, “No, just feeling a bit off.”
bit :: Text -> Text
bit str = "@SP\nA=M-1\nM=" ++ str ++ "M"

-- The string for a logic command like eq, gt, lt.
logic :: Text -> Text -> State MyState Text
logic fileName str = do
  state <- get
  let mycount = tshow $ counter state
  put state {counter = counter state + 1}
  return $
    unlines
      [ "@SP\nAM=M-1\nD=M\nA=A-1\nD=M-D\nM=0",
        "@TRUTHY." ++ fileName ++ "." ++ mycount,
        "D;" ++ str,
        "@DONE." ++ fileName ++ "." ++ mycount,
        "0;JMP",
        "(TRUTHY." ++ fileName ++ "." ++ mycount ++ ")",
        "@SP\nA=M-1\nM=-1",
        "(DONE." ++ fileName ++ "." ++ mycount ++ ")"
      ]

pushBody :: Text
pushBody = "@SP\nM=M+1\nA=M-1\nM=D"

-- The string for a push command, handles push {constant pointer temp static local argument this that}, otherwise throws an error
push :: Text -> Text -> Text -> Text
push _ "constant" offset = "@" ++ offset ++ "\nD=A\n" ++ pushBody
push _ "pointer" "0" = "@THIS\nD=M\n" ++ pushBody
push _ "pointer" "1" = "@THAT\nD=M\n" ++ pushBody
push _ "temp" offset = "@" ++ parseTemp offset ++ "\nD=M\n" ++ pushBody
push fileName "static" offset = "@" ++ fileName ++ "." ++ offset ++ "\nD=M\n" ++ pushBody
push _ "local" offset = "@LCL\nD=M\n@" ++ offset ++ "\nA=D+A\nD=M\n" ++ pushBody
push _ "argument" offset = "@ARG\nD=M\n@" ++ offset ++ "\nA=D+A\nD=M\n" ++ pushBody
push _ "this" offset = "@THIS\nD=M\n@" ++ offset ++ "\nA=D+A\nD=M\n" ++ pushBody
push _ "that" offset = "@THAT\nD=M\n@" ++ offset ++ "\nA=D+A\nD=M\n" ++ pushBody
push _ t offset = error (unpack $ "push " ++ t ++ " " ++ offset ++ " not implemented")

popBody :: Text
popBody = "D=A+D\n@R13\nM=D\n@SP\nAM=M-1\nD=M\n@R13\nA=M\nM=D"

-- The string for a pop command, handles pop {constant pointer temp static local argument this that}, otherwise throws an error
pop :: Text -> Text -> Text -> Text
pop _ "pointer" "0" = "@SP\nAM=M-1\nD=M\n@THIS\nM=D"
pop _ "pointer" "1" = "@SP\nAM=M-1\nD=M\n@THAT\nM=D"
pop _ "temp" offset = "@SP\nAM=M-1\nD=M\n@" ++ parseTemp offset ++ "\nM=D"
pop _ "local" offset = "@LCL\nD=M\n@" ++ offset ++ "\n" ++ popBody
pop _ "argument" offset = "@ARG\nD=M\n@" ++ offset ++ "\n" ++ popBody
pop _ "this" offset = "@THIS\nD=M\n@" ++ offset ++ "\n" ++ popBody
pop _ "that" offset = "@THAT\nD=M\n@" ++ offset ++ "\n" ++ popBody
pop fileName "static" offset = "@SP\nAM=M-1\nD=M\n@" ++ fileName ++ "." ++ offset ++ "\nM=D"
pop _ t offset = error (unpack $ "pop " ++ t ++ " " ++ offset ++ " not implemented")

labelT :: Text -> State MyState Text
labelT name = do
  state <- get
  return $ "(" ++ (functionName state) ++ "$" ++ name ++ ")"

gotoT :: Text -> State MyState Text
gotoT name = do
  state <- get
  return $ "@" ++ (functionName state) ++ "$" ++ name ++ "\n0;JMP"

ifgoto :: Text -> State MyState Text
ifgoto name = do
  state <- get
  return $ "@SP\nAM=M-1\nD=M\n@" ++ (functionName state) ++ "$" ++ name ++ "\nD;JNE"

parseArgs :: Text -> Int
parseArgs args =
  case decimal args of
    Right (b, _) -> b
    _ -> error (unpack $ "Uknown args: " ++ args)

functionT :: Text -> Text -> State MyState Text
functionT name n = do
  state <- get
  put state {functionName = name}
  return $ intercalate "\n" $ ("(" ++ name ++ ")") : (replicate (parseArgs n) ("@SP\nM=M+1\nA=M-1\nM=0"))

callT :: Text -> Text -> State MyState Text
callT name n = do
  state <- get
  let mycount = tshow $ counter state
  put state {counter = counter state + 1}
  return $
    intercalate
      "\n"
      [ "@RETURN." ++ mycount,
        "D=A\n@SP\nM=M+1\nA=M-1\nM=D",
        "@LCL\nD=M\n@SP\nM=M+1\nA=M-1\nM=D",
        "@ARG\nD=M\n@SP\nM=M+1\nA=M-1\nM=D",
        "@THIS\nD=M\n@SP\nM=M+1\nA=M-1\nM=D",
        "@THAT\nD=M\n@SP\nM=M+1\nA=M-1\nM=D",
        "@SP\nD=M\n@LCL\nM=D\n@" ++ n ++ "\nD=D-A\n@5\nD=D-A\n@ARG\nM=D",
        "@" ++ name,
        "0;JMP",
        "(RETURN." ++ mycount ++ ")"
      ]

returnT :: State MyState Text
returnT =
  return $
    "@LCL\nD=M\n@5\nA=D-A\nD=M\n@R13\nM=D\n"
      ++ "@SP\nA=M-1\nD=M\n@ARG\nA=M\nM=D\n"
      ++ "D=A+1\n@SP\nM=D\n"
      ++ "@LCL\nAM=M-1\nD=M\n@THAT\nM=D\n"
      ++ "@LCL\nAM=M-1\nD=M\n@THIS\nM=D\n"
      ++ "@LCL\nAM=M-1\nD=M\n@ARG\nM=D\n"
      ++ "@LCL\nA=M-1\nD=M\n@LCL\nM=D\n"
      ++ "@R13\nA=M\n0;JMP"

processItem :: Text -> [Text] -> State MyState Text
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
processItem _ ["label", name] = labelT name
processItem _ ["goto", name] = gotoT name
processItem _ ["if-goto", name] = ifgoto name
processItem _ ["function", name, n] = functionT name n
processItem _ ["call", name, n] = callT name n
processItem _ ["return"] = returnT
processItem _ a = error (unpack $ "Unknown command: " ++ unwords a)

parseFile :: Text -> [Text]
parseFile = filter (/= "") . map (strip . T.takeWhile (/= '/')) . lines

runFile :: FilePath -> IO (State MyState [Text])
runFile file = do
  contents <- readFile file
  let parsed = (parseFile contents)
  let mapper = processItem (pack $ takeBaseName file)
  let magic = \x -> do
        out <- mapper (words x)
        return $ "//" ++ x ++ "\n" ++ out
  return $ mapM magic parsed

main :: IO ()
main = do
  args <- getArgs

  let fileName = unpack $ head args
  let isFolder = not (".vm" `isSuffixOf` fileName)

  let zeroState = MyState {counter = 0, functionName = ""} -- The initial state is just a zeroed counter and a blank function name
  let (bootstrap, firstState) =
        -- We return our true first state that might be changed by calling a function in bootstrap
        if isFolder
          then do
            let (callSysInit, incCounter) = runState (callT "Sys.init" "0") zeroState
            ("// Bootstrap code\n@256\nD=A\n@SP\nM=D\n" ++ (callSysInit) ++ "\n", incCounter)
          else ("", zeroState)

  files <-
    if isFolder
      then do
        allFiles <- getDirectoryContents fileName
        -- Some people, when confronted with a problem, think, 'I know, I'll use threads' - and then two they hav erpoblesms.
        mapM (runFile . (fileName </>)) (filter (".vm" `isSuffixOf`) allFiles)
      else do
        file <- runFile fileName
        return [file]

  let mapped = evalState (mapM id files) firstState
  let output = bootstrap ++ unlines (Data.List.concat mapped)
  let outfile =
        if isFolder
          then fileName </> addExtension (takeFileName (dropTrailingPathSeparator fileName)) "asm"
          else (replaceExtension fileName "asm")

  writeFile outfile output
