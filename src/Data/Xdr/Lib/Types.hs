module Data.Xdr.Lib.Types
  ( SignedInt
  , UnsignedInt
  , SignedHyper
  , UnsignedHyper
  ) where

import           Data.Int  (Int32, Int64)
import           Data.Word (Word32, Word64)

-- | An XDR signed integer is a 32-bit datum that encodes an integer in
--  the range [-2147483648,2147483647].  The integer is represented in
--  two’s complement notation.  The most and least significant bytes are
--  0 and 3, respectively.
type SignedInt = Int32

-- | An XDR unsigned integer is a 32-bit datum that encodes a non-negative
-- integer in the range [0,4294967295].  It is represented by an
-- unsigned binary number whose most and least significant bytes are 0
-- and 3, respectively.
type UnsignedInt = Word32

-- | The standard also defines 64-bit (8-byte) numbers called hyper
-- integers and unsigned hyper integers.  Their representations are the
-- obvious extensions of integer and unsigned integer defined above.
-- They are represented in two’s complement notation.  The most and
-- least significant bytes are 0 and 7, respectively.
type SignedHyper = Int64
type UnsignedHyper = Word64
