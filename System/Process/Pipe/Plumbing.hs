-- File created: 2008-06-20 14:51:20

module System.Process.Pipe.Plumbing
   ( Tap(..), Sink(..)
   , bufferSize
   ) where

import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BL
import Data.Foldable         (toList)
import Data.Sequence         (Seq)
import qualified Data.Sequence as S
import Data.Word             (Word8)
import Foreign.Marshal.Array (peekArray, pokeArray)
import Foreign.Ptr           (Ptr, castPtr)
import Foreign.Storable      (Storable, sizeOf)
import System.IO             (Handle, hGetBuf, hPutBuf, hIsEOF)

-- | From a 'Tap', data up to the requested amount flows into a 'Ptr'. The
-- exact amount of 'Word8'\'s that flowed is returned. The requested amount is
-- guaranteed to be no greater than 'bufferSize'.
class Tap a where
   flowOut   :: a -> Ptr Word8 -> Int -> IO (a, Int)
   exhausted :: a -> IO Bool

-- | To a 'Sink', the requested amount of 'Word8'\'s flows from a 'Ptr'. The
-- requested amount is guaranteed to be no greater than 'bufferSize'.
class Sink a where
   flowIn :: a -> Ptr Word8 -> Int -> IO a

-- | The size of one chunk of data. A 'Ptr' 'Word8' given to a 'Tap' or 'Sink'
-- is guaranteed to have room for this many 'Word8'\'s, but no more.
bufferSize :: Int
bufferSize = 32*1024

-- Instances
------------

-- Handle

instance Tap  Handle where flowOut h b s = hGetBuf h b s >>= return . (,) h
                           exhausted     = hIsEOF
instance Sink Handle where flowIn  h b s = hPutBuf h b s >>  return       h

-- Storable a => [a]

instance Storable a => Tap [a] where
   exhausted = return . null

   flowOut x buf sz = do
      let size     = sizeOf (head x)
          (xs, ys) = splitAt (sz `div` size) x
          -- avoid expensive call to 'length' if possible
          sz'      = if null ys then size * length xs else sz

      pokeArray (castPtr buf) xs
      return (ys, sz')

instance Storable a => Sink [a] where
   flowIn x buf sz = do
      xs <- peekArray (sz `div` sizeOf (head x)) (castPtr buf)
      return (x ++ xs)

-- Storable a => Seq a

instance Storable a => Tap (Seq a) where
   exhausted = return . S.null

   flowOut x buf sz = do
      let size     = sizeOf (S.index x 0)
          (xs, ys) = S.splitAt (sz `div` size) x

      pokeArray (castPtr buf) (toList xs)
      return (ys, size * S.length xs)

instance Storable a => Sink (Seq a) where
   flowIn x buf sz = do
      xs <- peekArray (sz `div` sizeOf (S.index x 0)) (castPtr buf)
      return (x S.>< S.fromList xs)

-- ByteString (both strict and lazy)

-- We cheat and know in advance that ByteStrings contain octets and thus we
-- don't need all the messing about with sizeOf.

instance Tap BS.ByteString where
   exhausted = return . BS.null

   flowOut x buf sz = do
      let (xs, ys) = BS.splitAt sz x

      pokeArray (castPtr buf) (BS.unpack xs)
      return (ys, BS.length xs)

instance Tap BL.ByteString where
   exhausted = return . BL.null

   flowOut x buf sz = do
      let (xs, ys) = BL.splitAt (fromIntegral sz) x

      pokeArray (castPtr buf) (BL.unpack xs)
      return (ys, fromIntegral . BL.length $ xs)

instance Sink BS.ByteString where
   flowIn x buf sz = do
      xs <- peekArray sz (castPtr buf)
      return (x `BS.append` BS.pack xs)

instance Sink BL.ByteString where
   flowIn x buf sz = do
      xs <- peekArray sz (castPtr buf)
      return (x `BL.append` BL.pack xs)
