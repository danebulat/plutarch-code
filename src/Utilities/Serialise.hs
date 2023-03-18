module Utilities.Serialise
  ( validatorToHexString
  , closedTermToHexString
  , writeMatchV
  , writeVestingV
  , writeCheckSigV
  , writeNftMp
  ) where

import Data.ByteString.Short     qualified as SBS
import Data.Word                 (Word8)
import Numeric
import Plutarch
import Plutarch.Prelude
import Plutarch.Api.V2
import Plutarch.Script           (serialiseScript)
import Ply.Plutarch              (writeTypedScript)
import System.FilePath           ((</>))

import Validators.Guess          (matchGuess)
import Validators.Vesting        (checkVesting)
import Validators.CheckSignature (checkSignatory)
import MintingPolicies.NFT       (nftMp)

-- ---------------------------------------------------------------------- 
-- Ply
-- ---------------------------------------------------------------------- 

-- Match guess validator
matchGuessV :: ClosedTerm PValidator
matchGuessV = plam $ \dat red ctx -> matchGuess # dat # red # pdata ctx

writeMatchV :: IO () 
writeMatchV = 
    writeTypedScript 
      (Config NoTracing) 
      "Match guess validator" 
      ("data" </> "match-guess.plutus")
      matchGuessV

-- Vesting validator
vestingV :: ClosedTerm PValidator
vestingV = plam $ \dat red ctx -> checkVesting # dat # red # pdata ctx 

writeVestingV :: IO () 
writeVestingV = 
    writeTypedScript 
      (Config NoTracing) 
      "Vesting validator" 
      ("data" </> "vesting.plutus")
      vestingV 

-- Check signature validator
parameterizedCheckSigV :: ClosedTerm (PPubKeyHash :--> PValidator)
parameterizedCheckSigV = plam $ \pkh dat red ctx -> 
    checkSignatory # pkh # dat # red # pdata ctx 

writeCheckSigV :: IO () 
writeCheckSigV = 
    writeTypedScript 
      (Config NoTracing) 
      "Check signatory validator" 
      ("data" </> "check-signatory.plutus")
      parameterizedCheckSigV 

-- NFT minting policy
writeNftMp :: IO () 
writeNftMp =
    writeTypedScript 
      (Config {tracingMode=DoTracing})
      "NFT Minting Policy (DoTracing)"
      ("data" </> "nftMp.plutus")
      nftMp

-- ---------------------------------------------------------------------- 
-- Manual serialisation from bytestring to hex 
-- ---------------------------------------------------------------------- 

-- Not verified on testnet, use ply library to serialise 
-- scripts to CBOR hex and raw hex.
--
-- Example usage:
-- serialiseToHex :: String
-- serialiseToHex = validatorToHexString $ serialiseScript eval'

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

