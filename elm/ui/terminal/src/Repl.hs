{-# LANGUAGE OverloadedStrings #-}
module Repl
  ( Flags(..)
  , run
  )
  where


import Prelude hiding (lines, read)
import Control.Applicative ((<|>))
import Control.Monad.RWS (lift, liftIO)
import qualified Control.Monad.RWS as RWS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as LBS
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Monoid ((<>))
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified System.Console.Haskeline as Repl
import qualified System.Directory as Dir
import qualified System.Exit as Exit
import System.FilePath ((</>))
import qualified System.IO as IO
import qualified System.Process as Proc

import qualified Elm.Compiler as Elm (version)
import qualified Elm.Name as N
import qualified Elm.Project as Project
import qualified Elm.Package as Pkg
import qualified Elm.PerUserCache as PerUserCache
import qualified Parse.Repl as Elm
import Reporting.Doc ((<+>))
import qualified Reporting.Doc as D
import qualified Reporting.Task as Task
import qualified Reporting.Progress.Repl as Repl
import qualified Reporting.Render.Type.Localizer as L



-- RUN


data Flags =
  Flags
    { _interpreter :: Maybe FilePath
    , _noColors :: Bool
    }


run :: () -> Flags -> IO ()
run () (Flags maybeAlternateInterpreter noColors) =
  do  interpreter <- getInterpreter maybeAlternateInterpreter
      _ <- Project.getRootWithReplFallback
      printWelcomeMessage
      settings <- initSettings
      let manager = Repl.runInputT settings (Repl.withInterrupt loop)
      (exitCode,_,_) <- RWS.runRWST manager (interpreter, noColors) initialState
      Exit.exitWith exitCode


type Runner =
  RWS.RWST (FilePath, Bool) () State IO



-- LOOP


loop :: Repl.InputT Runner Exit.ExitCode
loop =
  do  input <- Repl.handleInterrupt (return Skip) read
      maybe loop return =<< lift (eval input)



-- READ


read :: Repl.InputT Runner Input
read =
  readHelp "> " []


readHelp :: String -> [String] -> Repl.InputT Runner Input
readHelp starter lines =
  do  input <- Repl.getInputLine starter
      case input of
        Nothing ->
          return Exit

        Just line ->
          if not (null line) && last line == '\\'
            then readHelp "|   " (init line : lines)
            else return $ toInput $ List.intercalate "\n  " $ reverse (line : lines)



-- EVAL


eval :: Input -> Runner (Maybe Exit.ExitCode)
eval input =
  case input of
    Skip ->
      return Nothing

    Exit ->
      return (Just Exit.ExitSuccess)

    Reset ->
      do  RWS.put initialState
          report "<reset>"

    Help info ->
      report (helpMessage info)

    Code entry ->
      Repl.handleInterrupt (report "<cancelled>") $
        do  state@(State _ imports types defs tricks localizer) <- RWS.get
            case entry of
              Elm.Import name alias exposing src ->
                interpret None $
                  state
                    { _imports = Map.insert name src imports
                    , _localizer = L.replAdd name alias exposing localizer
                    }

              Elm.Type name src ->
                interpret None $ state { _types = Map.insert name src types }

              Elm.Def (Just name) src ->
                interpret (Def name) $ state { _defs = Map.insert name src defs }

              Elm.Def Nothing src ->
                interpret None $ state { _tricks = src : tricks }

              Elm.Other src ->
                interpret (Expr src) state

              Elm.Annotation ->
                report "I cannot handle type annotations."

              Elm.Port ->
                report "I cannot handle port declarations."


report :: String -> Runner (Maybe a)
report msg =
  do  liftIO (putStrLn msg)
      return Nothing


data Output = None | Def N.Name | Expr Text.Text


interpret :: Output -> State -> Runner (Maybe a)
interpret def state =
  do  noColors <- RWS.asks snd
      result <- liftIO $ Task.try Repl.reporter $ compile noColors def state

      case result of
        Nothing ->
          return ()

        Just maybePath ->
          do  RWS.put (state { _count = _count state + 1 })
              maybe (return ()) interpretHelp maybePath

      return Nothing


interpretHelp :: FilePath -> Runner ()
interpretHelp path =
  do  interpreter <- RWS.asks fst
      let proc = (Proc.proc interpreter [path]) { Proc.std_in = Proc.CreatePipe }
      liftIO $ Proc.withCreateProcess proc $ \_ _ _ handle ->
        do  _ <- Proc.waitForProcess handle
            Dir.removeFile path



-- INPUT


data Input
  = Skip
  | Exit
  | Reset
  | Help (Maybe String)
  | Code Elm.Entry


toInput :: String -> Input
toInput string =
  case dropWhile (==' ') string of
    ":exit" ->
      Exit

    ":help" ->
      Help Nothing

    ":reset" ->
      Reset

    ':' : rest ->
      Help (Just (takeWhile (/=' ') rest))

    "" ->
      Skip

    _ ->
      Code (Elm.parseEntry string)



-- STATE


data State =
  State
    { _count :: !Int
    , _imports :: Map.Map N.Name Text.Text
    , _types :: Map.Map N.Name Text.Text
    , _defs :: Map.Map N.Name Text.Text
    , _tricks :: [Text.Text]
    , _localizer :: L.Localizer
    }


initialState :: State
initialState =
  State 0 Map.empty Map.empty Map.empty [] L.replEmpty


compile :: Bool -> Output -> State -> Task.Task (Maybe FilePath)
compile noColors output (State count imports types defs tricks localizer) =
  let
    header =
      "module " <> N.toBuilder N.replModule <> " exposing (..)\n\n"

    elmSourceCode =
      (<>) header $
        addLines imports $ addLines types $ addLines defs $
          List.foldr (<>) mempty (map Text.encodeUtf8Builder tricks)

    compileForRepl builder maybeName =
      Project.compileForRepl noColors localizer (LBS.toStrict (B.toLazyByteString builder)) maybeName
  in
  case output of
    None ->
      if Map.null types && Map.null defs && null tricks then
        compileForRepl (elmSourceCode <> "unit = ()") Nothing
      else
        compileForRepl elmSourceCode Nothing

    Def name ->
      compileForRepl elmSourceCode (Just name)

    Expr expr ->
      let
        name = N.addIndex "repl_value_" count
        text = Text.replace "\n" "\n  " expr
        def = N.toBuilder name <> " =\n  " <> Text.encodeUtf8Builder text <> "\n"
      in
      compileForRepl (elmSourceCode <> def) (Just name)


addLines :: Map.Map N.Name Text.Text -> B.Builder -> B.Builder
addLines dict builder =
  Map.foldr (\text b -> Text.encodeUtf8Builder text <> "\n" <> b) builder dict



-- SETTINGS


initSettings :: IO (Repl.Settings Runner)
initSettings =
  do  cache <- PerUserCache.getReplRoot
      return $
        Repl.Settings
          { Repl.historyFile = Just (cache </> "history")
          , Repl.autoAddHistory = True
          , Repl.complete = Repl.completeWord Nothing " \n" lookupCompletions
          }


lookupCompletions :: String -> Runner [Repl.Completion]
lookupCompletions string =
    do  (State _ imports types defs _ _) <- RWS.get
        return $
          addMatches string False defs $
          addMatches string False types $
          addMatches string True imports $
          addMatches string False commands []


commands :: Map.Map N.Name ()
commands =
  Map.fromList [ (":exit", ()), (":quit", ()), (":reset", ()), (":help", ()) ]


addMatches :: String -> Bool -> Map.Map N.Name v -> [Repl.Completion] -> [Repl.Completion]
addMatches string isFinished dict completions =
  Map.foldrWithKey (addMatch string isFinished) completions dict


addMatch :: String -> Bool -> N.Name -> v -> [Repl.Completion] -> [Repl.Completion]
addMatch string isFinished name _ completions =
  let suggestion = N.toString name in
  if List.isPrefixOf string suggestion then
    Repl.Completion suggestion suggestion isFinished : completions
  else
    completions



-- WELCOME


printWelcomeMessage :: IO ()
printWelcomeMessage =
  let
    title = "Elm" <+> D.fromString elmVersion
    dashes = replicate (70 - length elmVersion) '-'
  in
  D.toAnsi IO.stdout $
    D.vcat
      [ D.black "----" <+> D.dullcyan title <+> D.black (D.fromString dashes)
      , D.black $ D.fromString $ "Read " <> D.makeLink "repl" <> " to learn more: exit, help, imports, etc."
      , D.black "--------------------------------------------------------------------------------"
      , D.empty
      ]


elmVersion :: String
elmVersion =
  Pkg.versionToString Elm.version



-- HELP MESSAGES


helpMessage :: Maybe String -> String
helpMessage maybeBadCommand =
  case maybeBadCommand of
    Nothing ->
      genericHelpMessage

    Just command ->
      "I do not recognize the :" ++ command ++ " command. " ++ genericHelpMessage


genericHelpMessage :: String
genericHelpMessage =
  "Valid commands include:\n\
  \\n\
  \  :exit    Exit the REPL\n\
  \  :help    Show this information\n\
  \  :reset   Clear all previous imports and definitions\n\
  \\n\
  \More info at " ++ D.makeLink "repl" ++ "\n"



-- GET INTERPRETER


getInterpreter :: Maybe String -> IO FilePath
getInterpreter maybeName =
  case maybeName of
    Just name ->
      getInterpreterHelp name (Dir.findExecutable name)

    Nothing ->
      getInterpreterHelp "node` or `nodejs" $
        do  exe1 <- Dir.findExecutable "node"
            exe2 <- Dir.findExecutable "nodejs"
            return (exe1 <|> exe2)


getInterpreterHelp :: String -> IO (Maybe FilePath) -> IO FilePath
getInterpreterHelp name findExe =
  do  maybePath <- findExe
      case maybePath of
        Just path ->
          return path

        Nothing ->
          do  IO.hPutStrLn IO.stderr (exeNotFound name)
              Exit.exitFailure


exeNotFound :: String -> String
exeNotFound name =
  "The REPL relies on node.js to execute JavaScript code outside the browser.\n"
  ++ "I could not find executable `" ++ name ++ "` on your PATH though!\n\n"
  ++ "You can install node.js from <http://nodejs.org/>. If it is already installed\n"
  ++ "but has a different name, use the --interpreter flag."
