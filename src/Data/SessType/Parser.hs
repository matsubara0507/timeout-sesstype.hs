{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}

module Data.SessType.Parser where

import           Prelude                    hiding (LT)

import           Data.Extensible
import           Data.SessType.Syntax
import           Data.Text                  (Text, pack)
import           Text.Megaparsec
import           Text.Megaparsec.Char       (alphaNumChar, char, lowerChar,
                                             space, string, upperChar)
import           Text.Megaparsec.Char.Lexer (float)

readGlobalType :: Text -> Maybe GlobalType
readGlobalType = parseMaybe globalType

type Parser = Parsec Error Text

type Error = ErrorItem Char

globalTypeParser :: Parser GlobalType
globalTypeParser = globalType

globalType :: Parser GlobalType
globalType
    = try commEnd
  <|> try recVar
  <|> try recursion
  <|> try communication
  <|> timeout

globalTypeS :: Parser GlobalType
globalTypeS = try commEnd <|> communication

commEnd :: Parser GlobalType
commEnd = keyword "end" *> pure CommEnd

recVar :: Parser GlobalType
recVar = RVar <$> (space *> variable <* space)

recursion :: Parser GlobalType
recursion = do
  v <- char '*' *> variable
  keyword "."
  Rec v <$> globalType

communication :: Parser GlobalType
communication = do
  p1 <- participant
  keyword "->"
  p2 <- participant
  keyword ":"
  ms <- message
  keyword "."
  Comm (#from @= p1 <: #to @= p2 <: #message @= ms <: nil) <$> globalType

timeout :: Parser GlobalType
timeout = do
  p <- participant <* char '@'
  (tc, g1) <- betweenTuple '[' timeConstraint "," globalTypeS ']'
  keyword "."
  (g2, g3) <- betweenTuple '(' globalType "," globalType ')'
  let
    meta = #owner @= p <: #delta @= tc <: #normal @= g2 <: #abend @= g3 <: nil
  return $ Timeout meta g1

timeConstraint :: Parser TConstraint
timeConstraint = do
  clock <- variable
  op <- try le <|> lt
  op clock <$> float
  where
    le = keyword "<=" *> pure LE
    lt = keyword "<"  *> pure LT

participant :: Parser Participant
participant = fmap pack $ (:) <$> upperChar <*> many alphaNumChar

message :: Parser Message
message = fmap pack $ (:) <$> lowerChar <*> many alphaNumChar

variable :: Parser Var
variable = message

keyword :: (MonadParsec e s m, Token s ~ Char) => Tokens s -> m ()
keyword s = (space *> string s <* space) *> pure ()

betweenTuple ::
  (MonadParsec e s m, Token s ~ Char)
  => Token s
  -> m a
  -> Tokens s
  -> m b
  -> Token s
  -> m (a, b)
betweenTuple open fstP sep sndP close = do
  a <- char open *> space *> fstP
  keyword sep
  b <- sndP <* space <* char close
  return (a, b)
