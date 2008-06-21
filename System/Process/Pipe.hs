-- File created: 2008-02-11 12:55:34
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fglasgow-exts -frewrite-rules #-} -- for the rewrite rule
-------------------------------------------------------------------------------
-- |
-- Module    : System.Process.Pipe
-- Copyright : (c) Matti Niemenmaa 2008
-- License   : BSD (see LICENSE.txt)
--
-- Maintainer  : Matti Niemenmaa <matti.niemenmaa+web@iki.fi>
-- Stability   : experimental
-- Portability : portable
--
-- Operations for piping data through multiple processes.
--
-- 'pipe' is the most general function, with 'filePipe' provided for
-- convenience purposes. For the common case of piping 'String's, the
-- 'word8ToString' and 'stringToWord8' helpers are included.
--
-- Whenever specifying a path to a process, explicitly specifying the current
-- directory is recommended for portability. That is: use \".\/foo\" instead of
-- \"foo\", for instance.
--
-- On Windows, appending \".exe\" to process paths is attempted if the
-- invocation fails.
-------------------------------------------------------------------------------
module System.Process.Pipe
   ( filePipe
   , Tap(..), Sink(..), bufferSize
   , pipe
   , word8ToString, stringToWord8
   ) where

import Control.Monad         (forM)
import Data.Char             (chr, ord)
import Data.Maybe            (fromJust)
import Data.Word             (Word8)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Marshal.Array (peekArray, pokeArray)
import Foreign.Ptr           (Ptr)
import System.FilePath       (dropFileName)
import System.IO             ( withBinaryFile, IOMode (ReadMode, WriteMode)
                             , Handle, hClose
                             , hGetContents, hPutStr)
import System.Process        ( CreateProcess(..), createProcess
                             , CmdSpec (RawCommand)
                             , StdStream (CreatePipe, Inherit, UseHandle)
                             , ProcessHandle, waitForProcess)

import System.Process.Pipe.Plumbing

#if mingw32_HOST_OS
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Foreign.Ptr           (castPtr)
#else
import System.IO             (hIsOpen, hGetBuf, hGetBufNonBlocking, hPutBuf)
import System.IO.Error       (isFullError)
import System.Posix.Signals  ( Signal, openEndedPipe
                             , Handler (Ignore), installHandler)
#endif

type Proc  = (      Handle,       Handle, ProcessHandle)
type MProc = (Maybe Handle, Maybe Handle, ProcessHandle)

createProc :: FilePath -> StdStream -> StdStream -> (FilePath,[String])
           -> IO MProc
createProc wdir inp out (p,args) = do
   let proc = CreateProcess
         { cmdspec   = undefined
         , cwd       = Just wdir
         , env       = Nothing
         , std_in    = inp
         , std_out   = out
         , std_err   = Inherit
         , close_fds = True }

   (i,o,_,pid) <-
      createProcess proc { cmdspec = RawCommand p args }
#if mingw32_HOST_OS
         `catch` const (createProcess proc
                         { cmdspec = RawCommand (p ++ ".exe") args })
#endif
   return (i,o,pid)

-- | Pipes the contents of the first file to the second file through all the
-- programs named.
--
-- The working directory used is the directory component of the path to the
-- first file.
filePipe :: [(FilePath,[String])] -> FilePath -> FilePath -> IO ()
filePipe progs infile outfile = do
   withBinaryFile outfile WriteMode $ \outhdl ->
    withBinaryFile infile ReadMode  $ \inhdl ->
      pipe (dropFileName infile) progs inhdl outhdl
   return ()

-- | Pipes data from the 'Tap' to the 'Sink' through all the commands named, in
-- the given working directory.
--
-- Be careful! All IO is at the byte level: this means that piping even a
-- String such as \"foo\" will result in the raw UTF-32 moving: the bytes (in
-- my case; I believe this is implementation-dependent) in question are not the
-- ASCII @[102, 111, 111]@ but rather @[102, 0, 0, 0, 111, 0, 0, 0, 111, 0, 0,
-- 0]@.
--
-- Note to Windows users: since 'hGetBufNonBlocking' doesn't work on Windows
-- (it blocks despite its name, see
-- <http://hackage.haskell.org/trac/ghc/ticket/806>), this pipeline uses a
-- non-constant amount of space. The amount used is linear in the amount of
-- data used at any point in the pipeline. So if you want to pipe 20 gibioctets
-- of data to a program, you better make sure you have at least said amount of
-- memory available. (In fact, ByteStrings are used, and their documentation
-- suggests that you might want twice that, just in case.)
--
-- In addition, the 'Tap' and 'Sink' classes are meant for the POSIX code:
-- having to move data through the 'Ptr' 'Word8' types, 'bufferSize' bytes at a
-- time, results in extra complexity.
--
-- If you want to do something about the above, ideally fix the GHC ticket
-- (probably nontrivial) and let me know so that I can activate the better code
-- for Windows as well. Alternatively, feel free to code an implementation of
-- this which works on Windows.
pipe :: (Tap t, Sink s) => FilePath -> [(FilePath,[String])] -> t -> s
                        -> IO (t,s)
pipe wdir progs otap osink = do

   let cp = createProc wdir CreatePipe CreatePipe

   allocaBytes bufferSize $ \buf -> do
      ps <- forM progs $ \pr -> do
         (i,o,pid) <- cp pr
         return (fromJust i, fromJust o, pid)

-- See 'pipeline' comment below for why this needs to be done differently.
#if mingw32_HOST_OS
      -- Gather up all data from the tap until it's exhausted.
      let loop tap s = do
             exh <- exhausted tap
             if exh
                then return (tap,s)
                else do
                   (tap',sz) <- flowOut tap buf bufferSize
                   xs <- peekArray sz (castPtr buf)
                   loop tap' (s `BS.append` BS.pack xs)

      (tap, s) <- loop otap BS.empty
      sink <- pipeline osink buf ps s
      return (tap,sink)
#else
      let loop :: (Sink s, Tap t) => t -> s -> IO (t,s)
          loop tap sink = do
             exh <- exhausted tap
             if exh
                then return (tap,sink)
                else do
                   (tap' , sz)     <- flowOut tap buf bufferSize
                   (sink', status) <- pipeline sink ps buf sz

                   exh' <- exhausted tap'
                   if isNeed status && exh'
                      then do
                         -- The first process wants more input, but there's no
                         -- more to give. Hence what we do is enter the final
                         -- pipeline: have the process close its stdin and deal
                         -- with any leftover output.
                         (sink'',_) <- finalPipeline sink' ps buf
                         return (tap',sink'')
                      else
                         loop tap' sink'
      loop otap osink
#endif

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
pipeline :: Sink s => s -> Ptr Word8 -> [Proc] -> ByteString -> IO s
pipeline sink buf ((i,o,p):ps) dat = do
   BS.hPut i dat
   hClose i
   dat' <- BS.hGetContents o
   waitForProcess p
   pipeline sink buf ps dat'

pipeline osink buf [] dat = do
   let loop sink s =
          if BS.null s
             then return sink
             else do
                let (xs,ys) = BS.splitAt bufferSize s
                pokeArray (castPtr buf) (BS.unpack xs)
                sink' <- flowIn sink buf (BS.length xs)
                loop sink' ys
   loop osink dat

#else

data Need = Done | Need

isNeed :: Need -> Bool
isNeed Need = True
isNeed _    = False

-- This matches more than just EPIPE but it's the best we can do.
onEPIPE :: IO a -> IO a -> IO a
a `onEPIPE` b = a `catch` \e -> if isFullError e then b else ioError e

pipeline, shoveDown :: Sink s => s -> [Proc] -> Ptr Word8 -> Int -> IO (s,Need)
pipeline sink []                   buf sz = toSink sink buf sz
pipeline sink ps@((inp, out, _):_) buf sz = do
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
   -- e.g. bufferSize-1 bytes, but is waiting on input. If we block here then
   -- we're waiting for more output while the process is waiting for more
   -- input---deadlock!
   sz' <- hGetBufNonBlocking out buf bufferSize
   if sz' == 0
      -- We got no output from this process. If the process's stdin is open,
      -- we request more input for it from the process above us. Otherwise
      -- we block, waiting for the process to complete its computation or
      -- whatever it's doing. We can do this safely now since the stdin is
      -- closed: there's no way to reach the deadlock outlined above.
      then do
         wantsMore <- hIsOpen inp
         if wantsMore && not brokenPipe
            then return (sink, Need)
            else blockingPipeline sink ps buf
      else shoveDown sink ps buf sz'

shoveDown sink []          buf sz = toSink sink buf sz
shoveDown sink ps@(p:rest) buf sz = do
   -- Pipe the output from the process above to the next process in the
   -- pipeline.
   (sink', below) <- pipeline sink rest buf sz
   case below of
        -- The process below wants more data: go back and try a nonblocking get
        -- from this process.
        Need -> pipeline sink' ps buf 0
        -- The process below us says it wants nothing more ever again. Since it
        -- wants nothing from us, we have nothing to do either: finish up here
        -- and bubble the information up.
        Done -> finalize p >> return (sink', Done)

blockingPipeline,
 finalPipeline    :: Sink s => s -> [Proc] -> Ptr Word8 -> IO (s, Need)
blockingPipeline sink []                    buf = toSink sink buf 0
blockingPipeline sink ps@(p@(_,out,_):rest) buf = do
   sz <- hGetBuf out buf bufferSize
   if sz == 0
      -- The blocking call returned 0: this means we've hit EOF, i.e. the
      -- process is done and will no longer output anything. Go down the
      -- pipeline, closing the stdin of the process below us and pushing any
      -- last output further down. After that, finish up here and tell the ones
      -- above to do the same.
      then do
         (sink', _) <- finalPipeline sink rest buf
         finalize p
         return (sink', Done)
      else shoveDown sink ps buf sz

finalPipeline sink []               buf = toSink sink buf 0
finalPipeline sink ps@((inp,_,_):_) buf = do
   hClose inp
   -- Since the stdin is closed, we can jump straight to the blocking version
   -- of the pipeline: while we could go by way of the non-blocking one there's
   -- no need to do so.
   blockingPipeline sink ps buf

-- Some data found its way all the way down the pipeline, so we put it in the
-- output handle and let the processes know that we're ready for more.
--
-- The special case for 0, while handled in hPutBuf, is my little hint to the
-- optimizer that it should inline these calls where the 0 is given explicitly
-- above.
toSink :: Sink s => s -> Ptr Word8 -> Int -> IO (s, Need)
toSink sink _   0  =                                  return (sink , Need)
toSink sink buf sz = flowIn sink buf sz >>= \sink' -> return (sink', Need)

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

withIgnoringSignal :: Signal -> IO a -> IO a
withIgnoringSignal sig mx = do
   old <- installHandler sig Ignore Nothing
   x <- mx
   installHandler sig old Nothing
   return x

#endif

{-# RULES "pipe->handlePipe" pipe = handlePipe #-}

-- Smarter way of piping Handle-to-Handle
handlePipe :: FilePath -> [(FilePath,[String])] -> Handle -> Handle
           -> IO (Handle, Handle)

handlePipe _    []     inhdl outhdl = do
   hGetContents inhdl >>= hPutStr outhdl
   return (inhdl, outhdl)

handlePipe wdir (p:ps) inhdl outhdl = do
   let cp = createProc wdir

       f pids out []       = return (out, pids)
       f pids out (pr:prs) = do
          (i,_,pid) <- cp CreatePipe out pr
          f (pid:pids) (UseHandle . fromJust $ i) prs

   (inp, pids) <- f [] (UseHandle outhdl) (reverse ps)
   (_,_,pid)   <- cp   (UseHandle inhdl) inp p

   mapM_ waitForProcess (pid:pids)

   return (inhdl, outhdl)

-- | A helper function which converts a @['Word8']@ to a 'String' by mapping
-- 'chr' over the octets.
--
-- In most cases, when you wish to pipe data to a String, you do not want to
-- interpret the results as the raw byte pattern of 'Char's, so you use
-- @['Word8']@ as the 'Sink' type. This function handles the common case of
-- ASCII data simply&#8212;if you're dealing with non-ASCII data you probably need
-- to handle the results in a different way.
word8ToString :: [Word8] -> String
word8ToString = map (chr.fromIntegral)

-- | The inverse of 'word8ToString'. Any 'Char's greater than 255 are
-- truncated: once again, be careful with non-ASCII.
stringToWord8 :: String -> [Word8]
stringToWord8 = map (fromIntegral.ord)
