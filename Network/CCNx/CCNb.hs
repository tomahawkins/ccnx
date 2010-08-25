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
import Text.Printf

data Block
  = BLOB [Word8]
  | UDATA String
  | TAG String [Block]
  | ATTR String String
  | DTAG Int [Block]
  | DATTR Int String
  | EXT Int
  deriving Show

data BlockType = BLOB' | UDATA' | TAG' | ATTR' | DTAG' | DATTR' | EXT' deriving Show

printCCNb :: Block -> ByteString
printCCNb = B.pack . formatCCNb

formatCCNb :: Block -> [Word8]
formatCCNb a = case a of
  BLOB a    -> formatHeader BLOB'  (length a) ++ a
  UDATA a   -> formatHeader UDATA' (length a) ++ map (fromIntegral . ord) a
  TAG a b   -> formatHeader TAG'   (length a - 1) ++ map (fromIntegral . ord) a ++ concatMap formatCCNb b ++ [0x00]
  ATTR a b  -> formatHeader ATTR'  (length a - 1) ++ map (fromIntegral . ord) a ++ formatCCNb (UDATA b)
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

blockType :: Word8 -> BlockType
blockType a = case a .&. 0x7 of
  5 -> BLOB'
  6 -> UDATA'
  1 -> TAG'
  3 -> ATTR'
  2 -> DTAG'
  4 -> DATTR'
  0 -> EXT'
  a -> error $ "unknown block type : " ++ show a

formatHeader :: BlockType -> Int -> [Word8]
formatHeader _ a | a < 0 = error "number is not positive"
formatHeader t a = f (shiftR a 4) ++ [shiftL (fromIntegral $ a .&. 0xF) 3 .|. 0x80 .|. blockTypeCode t]
  where
  f 0 = []
  f a = f (shiftR a 7) ++ [fromIntegral (a .&. 0x7F)]

parseCCNb :: ByteString -> Block
parseCCNb = fst . parseBlock . B.unpack

parseBlock :: [Word8] -> (Block, [Word8])
parseBlock a = case tt of
  BLOB'  -> (BLOB $ take n rest, drop n rest)
  UDATA' -> (UDATA udata, drop n rest)
    where
    udata = map (chr . fromIntegral) $ take n rest
  TAG'   -> (TAG (map (chr . fromIntegral) $ take (n + 1) rest) blocks, rest')
    where
    (blocks, rest') = parseBlocks $ drop (n + 1) rest
  ATTR'  -> (ATTR (map (chr . fromIntegral) $ take (n + 1) rest) a, rest')
    where
    (a, rest') = case parseBlock $ drop (n + 1) rest of
      (UDATA a, b) -> (a, b)
      _ -> error "expecting UDATA after ATTR"
  DTAG'  -> (DTAG n blocks, rest')
    where
    (blocks, rest') = parseBlocks rest
  DATTR' -> (DATTR n a, rest')
    where
    (a, rest') = case parseBlock rest of
      (UDATA a, b) -> (a, b)
      _ -> error "expecting UDATA after DATTR"
  EXT'   -> (EXT n, rest)
  where
  ((tt, n), rest) = parseHeader a

parseBlocks :: [Word8] -> ([Block], [Word8])
parseBlocks (0x00 : rest) = ([], rest)
parseBlocks a = (block : blocks, rest')
  where
  (block, rest) = parseBlock a
  (blocks, rest') = parseBlocks rest

parseHeader :: [Word8] -> ((BlockType, Int), [Word8])
parseHeader a = ((tt, n), b)
  where
  tt = blockType a2
  (a1, a2 : b) = case span (not . flip testBit 7) a of
    (_, []) -> error $ "parseHeader: " ++ concat [ printf "%02x" a | a <- a ]
    a -> a
  n = f (fromIntegral $ shiftR a2 3 .&. 0xF) 4 $ reverse a1
  f n _ [] = n
  f n s (a : b) = f (n .|. shiftL (fromIntegral a) s) (s + 7) b
  --header = "header: " ++ concat [ printf "%02x" a | a <- take (length a - length b) a ] ++ ": " ++ show tt ++ "  " ++ show n



