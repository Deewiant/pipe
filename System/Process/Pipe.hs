-- File created: 2008-02-11 12:55:34
{-# LANGUAGE CPP #-}
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
-- 'pipe' is the most general function, with 'pipe\'' and 'pipeString' provided
-- for convenience purposes.
--
-- 'handlePipe', 'filePipe', and 'filePipe\'' are for the common file-to-file
-- case and behave somewhat differently.
--
-- Whenever specifying a path to a process, explicitly specifying the current
-- directory is recommended for portability. That is: use \".\/foo\" instead of
-- \"foo\", for instance.
--
-- On Windows, appending \".exe\" to process paths is attempted if the
-- invocation fails.
-------------------------------------------------------------------------------
module System.Process.Pipe
   ( pipe, pipe'
   , pipeString
   , handlePipe
   , filePipe, filePipe'
   ) where

import Control.Concurrent    (forkIO)
import Control.Exception     (try, IOException)
import Control.Monad         (mplus)
import System.FilePath       (dropFileName)
import System.IO             ( withBinaryFile, IOMode (ReadMode, WriteMode)
                             , Handle, hGetContents, hPutStr, hClose)
import System.Process        ( CreateProcess(..), createProcess
                             , CmdSpec (RawCommand)
                             , StdStream (CreatePipe, Inherit, UseHandle)
                             , ProcessHandle, waitForProcess)

#if !mingw32_HOST_OS
import Control.Exception    (bracket)
import System.Posix.Signals (installHandler, sigPIPE, Handler(Ignore))
#endif

createProc :: FilePath -> StdStream -> StdStream -> (FilePath,[String])
           -> IO (Maybe Handle, Maybe Handle, ProcessHandle)
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

pipeline :: FilePath -> StdStream -> StdStream -> [(FilePath,[String])]
         -> IO (Maybe Handle, Maybe Handle, [ProcessHandle])

pipeline _    _   _   []    = ioError.userError$ "Pipe :: null pipeline"
pipeline wdir inp out progs = f [] Nothing inp progs
 where
   f pids firstI i [p] = do
      (i',o,pid) <- createProc wdir i out p
      return (firstI `mplus` i', o, reverse (pid:pids))

   f pids firstI i (p:ps) = do
      (i',Just o,pid) <- createProc wdir i CreatePipe p
      f (pid:pids) (firstI `mplus` i') (UseHandle o) ps

-- | Pipes the input, using the given writer and reader functions, through all
-- the commands named, in the given working directory. Returns the result.
--
-- An exception is thrown if the list of programs is empty.
--
-- The writer function is called in a 'forkIO'\'d thread, allowing this to be
-- lazy.
--
-- SIGPIPE is ignored in the writer thread. Likewise, any IOExceptions are
-- caught and ignored.
pipe :: (Handle -> a -> IO ()) -> (Handle -> IO b)
     -> FilePath -> [(FilePath,[String])]
     -> a -> IO b

pipe writer reader wdir progs dat = do
   (Just inp, Just out, pids) <- pipeline wdir CreatePipe CreatePipe progs

   forkIO $ do
#if !mingw32_HOST_OS
      bracket
         (         installHandler sigPIPE Ignore Nothing)
         (\orig -> installHandler sigPIPE orig   Nothing)
         $ \_   -> do
#endif
            try (writer inp dat) :: IO (Either IOException ())
            hClose inp `catch` const (return ())
            mapM_ waitForProcess pids
   reader out

-- | A convenience function for when you don't care about the working
-- directory, 'pipe\'' uses ".".
pipe' :: (Handle -> a -> IO ()) -> (Handle -> IO b)
      -> [(FilePath,[String])]
      -> a -> IO b
pipe' r w = pipe r w "."

-- | A convenience function for the common case of piping from a 'String' to a
-- 'String'.
pipeString :: [(FilePath, [String])] -> String -> IO String
pipeString = pipe' hPutStr hGetContents

-- | A function for the common case of piping from a 'Handle' to a 'Handle'.
--
-- Note that this is not a convenient frontend for 'pipe' and is fundamentally
-- different in the following ways:
--
-- * A null list of programs is allowed, in which case the contents of the
--   input Handle are simply written to the output Handle.
--
-- * This function is not lazy and returns only when the writing has been
--   completed.
handlePipe :: FilePath -> [(FilePath,[String])] -> Handle -> Handle -> IO ()
handlePipe _    []    inhdl outhdl = hGetContents inhdl >>= hPutStr outhdl
handlePipe wdir progs inhdl outhdl = do
   (_, _, pids) <- pipeline wdir (UseHandle inhdl) (UseHandle outhdl) progs
   mapM_ waitForProcess pids

-- | A convenience function for 'handlePipe'. Opens the given files in binary
-- mode.
filePipe :: FilePath -> [(FilePath,[String])] -> FilePath -> FilePath -> IO ()
filePipe wdir progs infile outfile = do
   withBinaryFile outfile WriteMode $ \outhdl ->
    withBinaryFile infile ReadMode  $ \inhdl ->
      handlePipe wdir progs inhdl outhdl

-- | Like 'filePipe', but the working directory used is the directory component
-- of the path to the first file.
filePipe' :: [(FilePath,[String])] -> FilePath -> FilePath -> IO ()
filePipe' progs infile = filePipe (dropFileName infile) progs infile
