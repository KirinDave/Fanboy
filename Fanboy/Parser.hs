{-# LANGUAGE OverloadedStrings #-}
module Fanboy.Parser where
import qualified Data.Irc as IRC
import           Data.Irc (Message)
import qualified Data.Attoparsec.Char8 as P8
import           Data.Attoparsec as P

import Data.Word (Word8)
import qualified Data.ByteString as B hiding (pack)
import qualified Data.ByteString.Char8 as B hiding (map)
import qualified Data.ByteString       as B (map)
import Control.Applicative
import Control.Monad (replicateM)


message :: Parser Message
message = IRC.Message <$> prefix <*> command <*> params <*> body <* eol

prefix :: Parser B.ByteString
prefix = (P8.char ':' *> getPrefix <* whitespace) <|> return B.empty
         where getPrefix = takeWhile1 (notInClass "\n\r ")

command :: Parser B.ByteString
command = (takeN 3 P8.digit <|> P8.takeWhile (P8.notInClass ": ")) <* whitespace <?> "Bad command"

params :: Parser [B.ByteString]
params = P.many param <?> "Bad Params"

param :: Parser B.ByteString
param = takeWhile1 (not <$> inClass "\n\r: ") <* whitespace

body :: Parser B.ByteString
body = (P8.char ':' *> takeTill (inClass "\n\r")) <|> return B.empty <?> "Bad Body"

eol :: Parser ()
eol = string "\r\n" *> return () <?> "Fail finding eol."

whitespace :: Parser ()
whitespace = skipWhile (inClass " ") >> return ()

takeN :: Int -> Parser Char -> Parser B.ByteString
takeN count parser = B.pack <$> replicateM count parser
