{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import           Control.Lens        ((&), (^.))
import           Data.Extensible
import           Data.Monoid         ((<>))
import           Data.SessType
import           Data.Text           (Text, pack, unpack)
import qualified Data.Text.IO        as T
import           Options.Applicative

main :: IO ()
main = do
  opts <- execParser $
    options `withInfo` "A command-line interface to the sesstype mini language"
  txt <-
    case opts ^. #input of
      Just file -> T.readFile (unpack file)
      Nothing   -> T.getContents
  exec (opts ^. #subcmd) txt &
    case opts ^. #output of
      Just file -> T.writeFile (unpack file)
      Nothing   -> T.putStrLn

exec :: Cmd -> Text -> Text
exec ParseCmd = maybe "cannot parse." pretty . readGlobalType
exec (ProjectCmd opts) =
  maybe "cannot parse." (projectCmd opts) . readGlobalType

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
