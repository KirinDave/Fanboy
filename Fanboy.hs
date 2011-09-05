{-# LANGUAGE OverloadedStrings #-}
module Fanboy where
import Data.Irc
import Fanboy.Parser
import Fanboy.Runloop
import Fanboy.State
import Control.Monad.Trans (liftIO)
import qualified Data.ByteString as B
import qualified Data.Set as S

main :: IO ()
main = do
  putStrLn "Gonna try get anna get to freenode. Brb."
  newHandle <- buildHandle fakeConfig
  runFanboy (kickstart derp) fakeConfig newHandle
  return ()

fakeConfig = FanboyConfig { server = "irc.freenode.net"
                            , port   = 6667
                            , preferredNick = "Brototyping"
                            , preferredRealname = "Sup"
                            , defaultChannels = (S.fromList ["#leanintoit redacted"])
                            , admins = (S.fromList ["KirinDave"]) }
kickstart :: (Message -> Fanboy ()) -> Fanboy ()
kickstart k = do
  conf <- getConfig
  handle <- getHandle
  sayLine "NICK Brototyping"
  sayLine "USER Sup 0 * :Brototyping my tutorial bot"
  sayLine "JOIN #leanintoit redacted"
  runLoop handle k

derp :: Message -> Fanboy ()
derp m = (liftIO $ putStrLn (show m)) >> return ()
