{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings #-}
module Fanboy.State (
  Fanboy,
  FanboyState(..),
  FanboyConfig(..),
  runFanboy,
  getHandle,
  getConfig,
  buildHandle,
  getState,
  upState,
  sayLine) where
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import Data.Set (Set)
import qualified Data.Set as S
import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import System.IO
import Network

data FanboyState =
  FanboyState { nickname :: ByteString
              , channels :: Set ByteString
              , handle   :: Handle
              } deriving (Show)

data FanboyConfig =
  FanboyConfig { server            :: ByteString
               , port              :: Int
               , preferredNick     :: ByteString
               , preferredRealname :: ByteString
               , defaultChannels   :: Set ByteString
               , admins            :: Set ByteString}

newtype Fanboy a = MyApp {
  runApp :: ReaderT FanboyConfig (StateT FanboyState IO) a
} deriving (Monad, MonadIO,
            MonadReader FanboyConfig,
            MonadState FanboyState)

runFanboy :: Fanboy a -> FanboyConfig -> Handle -> IO (a, FanboyState)
runFanboy k conf newHandle = do
  let state = FanboyState B.empty S.empty newHandle in
    runStateT (runReaderT (runApp k) conf) state

buildHandle :: FanboyConfig -> IO Handle
buildHandle conf = do
  newHandle <- connectTo (B8.unpack $ server conf) (PortNumber (fromIntegral (port conf)))
  hSetBuffering newHandle NoBuffering
  return newHandle

getConfig :: Fanboy FanboyConfig
getConfig = ask

getState :: Fanboy FanboyState
getState = get

upState :: (FanboyState -> FanboyState) -> Fanboy ()
upState f = modify f >> return ()

getHandle :: Fanboy Handle
getHandle = get >>= (return . handle)

sayLine :: ByteString -> Fanboy ()
sayLine lineText = do
  h <- getHandle
  liftIO $ mapM_ (B.hPutStr h) [lineText, "\r\n"]
