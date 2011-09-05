{-# LANGUAGE NoMonomorphismRestriction #-}
module Fanboy.Runloop where
import           Data.Irc
import           Fanboy.State
import qualified Fanboy.Parser as Parser
import           Data.Enumerator as Enum
import           Data.Enumerator.Binary (enumHandle)
import           Data.Attoparsec.Enumerator
import           Data.ByteString (ByteString)
import           System.IO
import qualified Control.Monad as M
import Debug.Trace

type Runloop a b = Iteratee a Fanboy b

parseRunloop :: Enumeratee ByteString Message Fanboy ()
parseRunloop = Enum.sequence (iterParser Parser.message)

buildConsumer :: (Message -> Fanboy ()) -> Runloop Message ()
buildConsumer k = continue step where
  step (Chunks []) = continue step
  step (Chunks xs) = traceShow xs $ (return (mapM_ k xs)) >> continue step
  step EOF         = Enum.yield () EOF

buildRunloop :: (Message -> Fanboy ()) -> Runloop ByteString ()
buildRunloop k = joinI (parseRunloop $$ buildConsumer k)

runLoop :: Handle -> (Message -> Fanboy ()) -> Fanboy ()
runLoop handle processor = run_ $ (enumHandle 128 handle $$ buildRunloop processor)
