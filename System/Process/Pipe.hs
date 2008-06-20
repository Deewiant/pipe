-- File created: 2008-02-11 12:55:34
{-# LANGUAGE CPP #-}
module System.Process.Pipe (pipe) where

import Control.Monad         (forM)
import Data.Maybe            (fromJust)
import System.Directory      (getCurrentDirectory)
import System.FilePath       ((</>), isAbsolute, dropFileName)
import System.IO             ( withBinaryFile, IOMode (ReadMode, WriteMode)
                             , Handle, hClose)
import System.Process        ( CreateProcess(..), createProcess
                             , CmdSpec (RawCommand)
                             , StdStream (CreatePipe, Inherit)
                             , ProcessHandle, waitForProcess)

#if mingw32_HOST_OS
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
#else
import Control.Monad         (when)
import Data.Word             (Word8)
import Foreign.Ptr           (Ptr)
import Foreign.Marshal.Alloc (allocaBytes)
import System.IO             ( hIsEOF, hIsOpen
                             , hGetBuf, hGetBufNonBlocking, hPutBuf)
import System.IO.Error       (isFullError)
import System.Posix.Signals  ( Signal, openEndedPipe
                             , Handler (Ignore), installHandler)

whenM :: (Monad m) => m Bool -> m a -> m ()
whenM cond body = cond >>= \c -> when c (body >> return ())

whileM, untilM :: (Monad m) => m Bool -> m a -> m ()
whileM cond body = whenM cond (body >> whileM cond body)

untilM cond = whileM (liftM not cond)
#endif

-- | Pipes the contents of the first file to the second file through all the
-- programs named. Giving the full path (as in "./foo" instead of just "foo")
-- is recommended for portability. Appending ".exe" to the name is attempted if
-- starting a process in the given path doesn't work.
--
-- The working directory used is the location of the first file.
--
-- Note to Windows users: since 'hGetBufNonBlocking' doesn't work on Windows
-- (it blocks despite its name, see
-- http://hackage.haskell.org/trac/ghc/ticket/806), this pipeline uses a
-- non-constant amount of space. The amount used is linear in the amount of
-- data used at any point in the pipeline. So if you want to pipe 20 gibioctets
-- of data to a program, you better make sure you have at least said amount of
-- memory available. (In fact, ByteStrings are used, and their documentation
-- suggests that you might want twice that, just in case.)
--
-- If you want to do something about the above, ideally fix the GHC ticket and
-- let me know so that I can activate the better code for Windows as well.
-- Alternatively, feel free to code an implementation of this which works on
-- Windows.
pipe :: [(FilePath, [String])] -> FilePath -> FilePath -> IO ()
pipe progs infile outfile = do
   absFile <- toAbsolutePath infile
   let wdir = dropFileName absFile
       proc = CreateProcess
                 { cmdspec   = undefined
                 , cwd       = Just wdir
                 , env       = Nothing
                 , std_in    = CreatePipe
                 , std_out   = CreatePipe
                 , std_err   = Inherit
                 , close_fds = True }

   withBinaryFile outfile WriteMode $ \outhdl ->
    withBinaryFile infile ReadMode  $ \inhdl ->
#if mingw32_HOST_OS
     do
        ps <- forM progs $ \(pr, args) -> do
           (i,o,_,p) <- createProcess proc { cmdspec = RawCommand pr args }
                           `catch` const (createProcess proc
                              { cmdspec = RawCommand (pr ++ ".exe") args })
           return (fromJust i, fromJust o, p)

        dat <- BS.hGetContents inhdl
        pipeline outhdl ps dat
#else
     allocaBytes bufSize            $ \buf -> do
        ps <- forM progs $ \(pr, args) -> do
           (i,o,_,p) <- createProcess proc { cmdspec = RawCommand pr args }
           return (fromJust i, fromJust o, p)

        untilM (hIsEOF inhdl) $ do
           sz <- hGetBuf inhdl buf bufSize
           status <- pipeline outhdl ps buf sz

           when (isNeed status) . whenM (hIsEOF inhdl) $
              -- The first process wants more input, but there's no more to
              -- give. Hence what we do is enter the final pipeline: have the
              -- process close its stdin and deal with any leftover output.
              finalPipeline outhdl ps buf
#endif

toAbsolutePath :: FilePath -> IO FilePath
toAbsolutePath p =
   if isAbsolute p
      then return p
      else getCurrentDirectory >>= return . (</> p)

type Proc = (Handle, Handle, ProcessHandle)

#if mingw32_HOST_OS
-- hGetBufNonBlocking doesn't work on Windows (see
-- http://hackage.haskell.org/trac/ghc/ticket/806). I can't think of a way of
-- doing a robust constant-space pipeline without it. Hence we use this silly
-- implementation instead, which is crap but does the job. (This is how
-- pipelines worked in Windows pre-NT anyway, AFAIK. (At least in DOS.))
--
-- Basically we just wait for the previous command to complete, gathering up
-- all its output, and then give it all at once to the next one. And yes, this
-- means that if some process outputs (or the input file contains) an infinite
-- amount of data or more than fits in memory, you're screwed.
pipeline :: Handle -> [Proc] -> ByteString -> IO ()
pipeline hdl ((i,o,p):ps) dat = do
   BS.hPut i dat
   hClose i
   dat' <- BS.hGetContents o
   waitForProcess p
   pipeline hdl ps dat'

pipeline hdl [] dat = BS.hPut hdl dat

#else

type Buf    = Ptr Word8
data Status = Done | Need

isNeed :: Status -> Bool
isNeed Need = True
isNeed _    = False

-- This matches more than just EPIPE but it's the best we can do.
onEPIPE :: IO a -> IO a -> IO a
a `onEPIPE` b = a `catch` \e -> if isFullError e then b else ioError e

pipeline, shoveDownPipeline :: Handle -> [Proc] -> Buf -> Int -> IO Status
pipeline hdl []                   buf sz = pipelineReachedFile hdl buf sz
pipeline hdl ps@((inp, out, _):_) buf sz = do
   -- Put the given data to the stdin of this process.
   --
   -- We have to be careful with SIGPIPEs here. If hPutBuf fails, that's
   -- because the input pipe is either closed or broken. We try to close it,
   -- and if that fails then we know the pipe is broken.
   --
   -- Simply piping infinite output to a process which takes only a bit of
   -- input ("yes | head", for instance) triggers both of the onEPIPE cases, as
   -- well as the one in finalize.
   brokenPipe <-
      withIgnoringSignal openEndedPipe $
         (hPutBuf inp buf sz >> return False)
            `onEPIPE` (hClose inp >> return False)
            `onEPIPE` return True

   -- We can't block here, lest it be the case where the program has output
   -- e.g. bufSize-1 bytes, but is waiting on input. If we block here then
   -- we're waiting for more output while the process is waiting for more
   -- input---deadlock!
   sz' <- hGetBufNonBlocking out buf bufSize
   if sz' == 0
      -- We got no output from this process. If the process's stdin is open,
      -- we request more input for it from the process above us. Otherwise
      -- we block, waiting for the process to complete its computation or
      -- whatever it's doing. We can do this safely now since the stdin is
      -- closed: there's no way to reach the deadlock outlined above.
      then do
         wantsMore <- hIsOpen inp
         if wantsMore && not brokenPipe
            then return Need
            else blockingPipeline hdl ps buf
      else shoveDownPipeline hdl ps buf sz'

shoveDownPipeline hdl []          buf sz = pipelineReachedFile hdl buf sz
shoveDownPipeline hdl ps@(p:rest) buf sz = do
   -- Pipe the output from the process above to the next process in the
   -- pipeline.
   below <- pipeline hdl rest buf sz
   case below of
        -- The process below wants more data: go back and try a nonblocking get
        -- from this process.
        Need -> pipeline hdl ps buf 0
        -- The process below us says it wants nothing more ever again. Since it
        -- wants nothing from us, we have nothing to do either: finish up here
        -- and bubble the information up.
        Done -> finalize p >> return Done

blockingPipeline, finalPipeline :: Handle -> [Proc] -> Buf -> IO Status
blockingPipeline hdl []                    buf = pipelineReachedFile hdl buf 0
blockingPipeline hdl ps@(p@(_,out,_):rest) buf = do
   sz <- hGetBuf out buf bufSize
   if sz == 0
      -- The blocking call returned 0: this means we've hit EOF, i.e. the
      -- process is done and will no longer output anything. Go down the
      -- pipeline, closing the stdin of the process below us and pushing any
      -- last output further down. After that, finish up here and tell the ones
      -- above to do the same.
      then do
         finalPipeline hdl rest buf
         finalize p
         return Done
      else shoveDownPipeline hdl ps buf sz

finalPipeline hdl []               buf = pipelineReachedFile hdl buf 0
finalPipeline hdl ps@((inp,_,_):_) buf = do
   hClose inp
   -- Since the stdin is closed, we can jump straight to the blocking version
   -- of the pipeline: while we could go by way of the non-blocking one there's
   -- no need to do so.
   blockingPipeline hdl ps buf

-- Some data found its way all the way down the pipeline, so we put it in the
-- output handle and let the processes know that we're ready for more.
--
-- The special case for 0, while handled in hPutBuf, is my little hint to the
-- optimizer that it should inline these calls where the 0 is given explicitly
-- above.
pipelineReachedFile :: Handle -> Buf -> Int -> IO Status
pipelineReachedFile _   _   0  =                       return Need
pipelineReachedFile hdl buf sz = hPutBuf hdl buf sz >> return Need

finalize :: Proc -> IO ()
finalize (i,o,p) = do
   -- Once again the stdin pipe may be broken so we sidestep a possible
   -- SIGPIPE.
   withIgnoringSignal openEndedPipe (hClose i `onEPIPE` return ())

   -- We close the stdout as soon as we no longer need to read anything from
   -- it, so that we don't have to wait for the process to finish outputting.
   -- With infinite-output processes (and possibly even just with buffered
   -- output) we'd be stuck forever in waitForProcess if we don't do this.
   --
   -- This is also the reason why withIgnoringSignal openEndedPipe isn't
   -- wrapped around this whole thing already in the pipe function: we want the
   -- processes in the pipeline to handle (or die of) SIGPIPE (which this
   -- hClose causes on their end) by themselves.
   hClose o
   waitForProcess p
   return ()

bufSize :: Int
bufSize = 48*1024

withIgnoringSignal :: Signal -> IO a -> IO a
withIgnoringSignal sig mx = do
   old <- installHandler sig Ignore Nothing
   x <- mx
   installHandler sig old Nothing
   return x
#endif
