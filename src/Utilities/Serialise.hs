module Utilities.Serialise
  ( validatorToHexString
  , closedTermToHexString
  ) where

import Data.ByteString.Short qualified as SBS
import Data.Word                          (Word8)
import Numeric
import Plutarch
import Plutarch.Script                    (serialiseScript)

validatorToHexString :: SBS.ShortByteString -> String
validatorToHexString = concatMap byteToHex . SBS.unpack

byteToHex :: Word8 -> String
byteToHex b = padToLen 2 '0' (showHex b "")

padToLen :: Int -> Char -> String -> String 
padToLen len c w = replicate (len - length w) c <> w

closedTermToHexString :: forall (p :: PType). ClosedTerm p -> Config -> Maybe String
closedTermToHexString t config = do 
    case compile config t of 
      Left _ -> Nothing 
      Right script -> 
        Just $ concatMap byteToHex $ SBS.unpack $ serialiseScript (script :: Script)

