{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import           Prelude             hiding (init)

import           Control.Lens        ((^.))
import           Data.Extensible
import           Data.Monoid         ((<>))
import           Data.SessType
import           Data.Text           (Text, pack, unpack)
import qualified Data.Text.IO        as T
import           Options.Applicative
import           System.IO           (hFlush, stdout)

main :: IO ()
main = do
  opts <- execParser $
    options `withInfo` "A command-line interface to the sesstype mini language"
  input <-
    case opts ^. #input of
      Just file -> T.readFile (unpack file)
      Nothing   -> T.getContents
  case readGlobalType input of
    Nothing -> T.putStrLn "cannot parse."
    Just gt -> exec opts gt

exec :: Options -> GlobalType -> IO ()
exec opts gt =
  case opts ^. #subcmd of
    ParseCmd         -> outputs opts $ pretty gt
    ProjectCmd opts' -> outputs opts $ pretty (projectCmd opts' gt)
    ReplCmd -> do
      T.putStrLn "input action: `P,Q!message` or `P,Q?message` or float (e.g. 1.0)"
      T.putStrLn (pretty gt)
      repl run (init gt)

outputs :: Options -> Text -> IO ()
outputs opts =
  case opts ^. #output of
    Just file -> T.writeFile (unpack file)
    Nothing   -> T.putStrLn

repl :: (Text -> a -> Maybe (a, Text)) -> a -> IO ()
repl f st = do
  T.putStr ">> "
  hFlush stdout
  input <- T.getLine
  case f input st of
    Nothing            -> T.putStrLn "Leaving."
    Just (st', output) -> T.putStrLn output *> repl f st'

run :: (Transition a, Pretty a) => Text -> ST a -> Maybe (ST a, Text)
run ":q" _ = Nothing
run act st =
  case maybe (Left "cannot parse.") (`transition` st) $ readAction act of
    Left err  -> pure (st, err)
    Right st' -> pure (st', pretty $ peel st')

projectCmd :: ProjectCmdOpts -> GlobalType -> Text
projectCmd opts =
  case opts ^. #role of
    (Just p) -> either eshow pretty . projection p
    Nothing  -> pretty . projectionAll

eshow :: ProjectionError -> Text
eshow err = mconcat [err ^. #message, " on: ", pretty $ err ^. #target]

type Options = Record
  '[ "subcmd" >: Cmd
   , "output" >: Maybe Text
   , "input"  >: Maybe Text
   ]

data Cmd
  = ParseCmd
  | ProjectCmd ProjectCmdOpts
  | ReplCmd

type ProjectCmdOpts = Record '[ "role" >: Maybe Participant ]

options :: Parser Options
options = hsequence
   $ #subcmd <@=> cmdParser
  <: #output <@=> outputParser
  <: #input  <@=> inputParser
  <: nil

cmdParser :: Parser Cmd
cmdParser = subparser
   $ command "parse" (pure ParseCmd `withInfo` "Parse a sesstype file")
  <> command "project"
      (ProjectCmd <$> projectParser `withInfo` "Perform endpoint projection on the given session type")
  <> command "repl" (pure ReplCmd `withInfo` "REPL for LTS of session type")

projectParser :: Parser ProjectCmdOpts
projectParser = hsequence
   $ #role <@=> roleParser
  <: nil

roleParser :: Parser (Maybe Participant)
roleParser = option (maybeReader $ Just . Just . pack)
    $ long "role"
   <> short 'r'
   <> metavar "ROLE"
   <> value Nothing
   <> help "Name of role to project for"

outputParser :: Parser (Maybe Text)
outputParser = option (maybeReader $ Just . Just . pack)
    $ long "output"
   <> short 'o'
   <> metavar "FILE"
   <> value Nothing
   <> help "Output to FILE"

inputParser :: Parser (Maybe Text)
inputParser = argument (maybeReader $ Just . Just . pack)
    $ metavar "FILE"
   <> value Nothing
   <> help "Input to FILE"

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts = info (helper <*> opts) . progDesc
