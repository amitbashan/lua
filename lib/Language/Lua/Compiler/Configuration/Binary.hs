module Language.Lua.Compiler.Configuration.Binary where

import Data.ByteString
import Data.ByteString.UTF8
import Data.Word
import qualified Language.Lua.Compiler.Configuration.Size as Configuration.Size

data Version
  = Official
  deriving (Enum)

data Endianness
  = Big
  | Little
  deriving (Enum)

data IntegralFlag
  = FloatingPoint
  | Integral
  deriving (Enum)

signature :: ByteString
signature = fromString "\x1BLua"

version :: Version
version = Official

versionNumber :: Word8
versionNumber = 0x51

endianness :: Endianness
endianness = Little

integralFlag :: IntegralFlag
integralFlag = FloatingPoint

header :: ByteString
header =
  signature
    <> pack
      [ versionNumber,
        fromIntegral $ fromEnum version,
        fromIntegral $ fromEnum endianness,
        Configuration.Size.integer,
        Configuration.Size.sizeT,
        Configuration.Size.instruction,
        Configuration.Size.number,
        fromIntegral $ fromEnum integralFlag
      ]
