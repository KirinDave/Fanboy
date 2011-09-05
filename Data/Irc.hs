module Data.Irc where
import qualified Data.ByteString as B

data Message =
  Message { prefix   :: B.ByteString
          , command  :: B.ByteString
          , params   :: [B.ByteString]
          , body     :: B.ByteString
          } deriving (Show)
