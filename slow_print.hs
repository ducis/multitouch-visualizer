#!runghc
import Control.Concurrent
import System.Environment
import GHC.IO.Handle
import GHC.IO.Handle.FD
main = do
	[t]<-getArgs
	getContents>>=mapM_ (\l->(putStrLn l>>hFlush stdout>>threadDelay (read t*1000))).lines
