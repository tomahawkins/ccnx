module Network.CCNx.CCNb
  ( Block (..)
  , printCCNb
  , parseCCNb
  ) where

import Data.Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Char
import Data.Word

data Block
  = BLOB [Word8]
  | UDATA String
  | TAG String [Block]
  | ATTR String String
  | DTAG Int [Block]
  | DATTR Int String
  | EXT Int
  deriving Show

data BlockType = BLOB' | UDATA' | TAG' | ATTR' | DTAG' | DATTR' | EXT'

printCCNb :: Block -> ByteString
printCCNb = B.pack . formatCCNb

formatCCNb :: Block -> [Word8]
formatCCNb a = case a of
  BLOB a    -> formatHeader BLOB'  (length a) ++ a
  UDATA a   -> formatHeader UDATA' (length a) ++ map (fromIntegral . ord) a
  TAG a b   -> formatHeader TAG'   (length a - 1) ++ concatMap formatCCNb b ++ [0x00]
  ATTR a b  -> formatHeader ATTR'  (length a - 1) ++ formatCCNb (UDATA b)
  DTAG a b  -> formatHeader DTAG'  a ++ concatMap formatCCNb b ++ [0x00]
  DATTR a b -> formatHeader DATTR' a ++ formatCCNb (UDATA b)  --XXX Is this right, or does it need to close with 00?
  EXT a     -> formatHeader EXT' a

blockTypeCode :: BlockType -> Word8
blockTypeCode a = case a of
  BLOB'  -> 5
  UDATA' -> 6
  TAG'   -> 1
  ATTR'  -> 3
  DTAG'  -> 2
  DATTR' -> 4
  EXT'   -> 0

formatHeader :: BlockType -> Int -> [Word8]
formatHeader _ a | a < 0 = error "number is not positive"
formatHeader t a = f1 a
  where
  f1 a = f2 (shiftR a 4) ++ [fromIntegral (a .&. 0xF) .|. 0x80 .|. blockTypeCode t]
  f2 0 = []
  f2 a = f2 (shiftR a 7) ++ [fromIntegral (a .&. 0x7F)]

parseCCNb :: ByteString -> Block
parseCCNb = undefined


