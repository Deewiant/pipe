-- File created: 2008-06-20 14:51:20

{-# LANGUAGE FlexibleInstances, ScopedTypeVariables #-}

module System.Process.Pipe.Plumbing
   ( Accessible(..), Tap(..), Sink(..)
   , bufferSize
   ) where

import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BL
import Data.Foldable         (toList)
import Data.IORef            (IORef, readIORef, writeIORef, modifyIORef)
import Data.Sequence         (Seq)
import qualified Data.Sequence as S
import Data.Word             (Word8)
import Foreign.Marshal.Array (peekArray, pokeArray)
import Foreign.Ptr           (Ptr, castPtr)
import Foreign.Storable      (Storable, sizeOf)
import System.IO             (Handle, hClose, hGetBuf, hPutBuf, hIsEOF)

-- | An Accessible is anything which can be opened or closed. By default both
-- operations do nothing.
class Accessible a where
   open, close :: a -> IO ()
   open  _ = return ()
   close _ = return ()

-- | From a Tap, data up to the requested amount flows into a Ptr. The exact
-- amount of Word8's that flowed is returned. The requested amount is
-- guaranteed to be no greater than 'bufferSize'.
class Accessible a => Tap a where
   flowOut   :: a -> Ptr Word8 -> Int -> IO Int
   exhausted :: a -> IO Bool

-- | To a Sink, the requested amount of Word8's flows from a Ptr. The requested
-- amount is guaranteed to be no greater than 'bufferSize'.
class Accessible a => Sink a where
   flowIn :: a -> Ptr Word8 -> Int -> IO ()

-- | The size of one chunk of data. A Ptr Word8 given to a 'Tap' or 'Sink' is
-- guaranteed to have room for this many Word8's, but no more.
bufferSize :: Int
bufferSize = 48*1024

-- Instances
------------

-- Handle

instance Accessible Handle where close   = hClose
instance Tap        Handle where flowOut = hGetBuf; exhausted = hIsEOF
instance Sink       Handle where flowIn  = hPutBuf

-- Storable a => IORef [a]

instance Storable a => Accessible (IORef [a])

instance (Show a, Storable a) => Tap (IORef [a]) where
   exhausted = fmap null . readIORef

   flowOut x buf sz = do
      dat <- readIORef x

      let size     = sizeOf (undefined :: a)
          (xs, ys) = splitAt (sz `div` size) dat
          sz'      = size * if null ys then length xs else sz

      writeIORef x ys
      pokeArray (castPtr buf) xs

      return sz'

instance (Show a, Storable a) => Sink (IORef [a]) where
   flowIn x buf sz = do
      xs <- peekArray (sz `div` sizeOf (undefined :: a)) (castPtr buf)
      modifyIORef x (++ xs)

-- Storable a => IORef (Seq a)

instance Storable a => Accessible (IORef (Seq a))

instance Storable a => Tap (IORef (Seq a)) where
   exhausted = fmap S.null . readIORef

   flowOut x buf sz = do
      s <- readIORef x
      let size     = sizeOf (undefined :: a)
          (xs, ys) = S.splitAt (sz `div` size) s
      writeIORef x ys
      pokeArray (castPtr buf) (toList xs)
      return (size * S.length xs)

instance Storable a => Sink (IORef (Seq a)) where
   flowIn x buf sz = do
      xs <- peekArray (sz `div` sizeOf (undefined :: a)) (castPtr buf)
      modifyIORef x (S.>< S.fromList xs)

-- IORef ByteString (both strict and lazy)

-- We cheat and know in advance that ByteStrings contain octets and thus we
-- don't need all the messing about with sizeOf.

instance Accessible (IORef BS.ByteString)
instance Accessible (IORef BL.ByteString)

instance Tap (IORef BS.ByteString) where
   exhausted = fmap BS.null . readIORef

   flowOut x buf sz = do
      s <- readIORef x

      let (xs, ys) = BS.splitAt sz s
      writeIORef x ys
      pokeArray (castPtr buf) (BS.unpack xs)
      return (BS.length xs)

instance Tap (IORef BL.ByteString) where
   exhausted = fmap BL.null . readIORef

   flowOut x buf sz = do
      s <- readIORef x
      let (xs, ys) = BL.splitAt (fromIntegral sz) s
      writeIORef x ys
      pokeArray (castPtr buf) (BL.unpack xs)
      return . fromIntegral . BL.length $ xs

instance Sink (IORef BS.ByteString) where
   flowIn x buf sz = do
      xs <- peekArray sz (castPtr buf)
      modifyIORef x (`BS.append` BS.pack xs)

instance Sink (IORef BL.ByteString) where
   flowIn x buf sz = do
      xs <- peekArray sz (castPtr buf)
      modifyIORef x (`BL.append` BL.pack xs)

