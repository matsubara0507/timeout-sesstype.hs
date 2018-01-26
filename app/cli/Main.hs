{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import           Control.Lens        ((^.))
import           Data.Extensible
import           Data.Monoid         ((<>))
import           Data.SessType
import           Data.Text           (Text, pack)
import qualified Data.Text.IO        as T
import           Options.Applicative

main :: IO ()
main = do
  opts <- execParser $
    options `withInfo` "A command-line interface to the sesstype mini language"
  T.putStrLn $ exec (opts ^. #subcmd) (opts ^. #input)

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
eshow (emessage, g) = mconcat [emessage, " on: ", pretty g]

type Options = Record
  '[ "subcmd" >: Cmd
   , "input"  >: Text
   ]

data Cmd
  = ParseCmd
  | ProjectCmd ProjectCmdOpts

type ProjectCmdOpts = Record '[ "role" >: Maybe Participant ]

options :: Parser Options
options = hsequence
   $ #subcmd <@=> cmdParser
  <: #input  <@=> textArgument (metavar "inputs" <> help "Input global type")
  <: nil

cmdParser :: Parser Cmd
cmdParser = subparser
   $ command "parser" (pure ParseCmd `withInfo` "Parse a sesstype file")
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
   <> help "Name of role to project for (if nothing, project all participants)"

textArgument :: Mod ArgumentFields Text -> Parser Text
textArgument = argument (eitherReader $ Right . pack)

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts = info (helper <*> opts) . progDesc
