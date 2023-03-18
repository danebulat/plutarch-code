module MintingPolicies.NFTOffChain where

import System.FilePath ((</>))
import PlutusLedgerApi.V2
import Ply
import UntypedPlutusCore

newtype MintingPolicy = MintingPolicy (Program DeBruijn DefaultUni DefaultFun ())

toMintingPolicy :: TypedScript 'MintingPolicyRole '[] -> MintingPolicy
toMintingPolicy (TypedScript _ s) = MintingPolicy s

usePolicy :: MintingPolicy -> IO ()
usePolicy _ = putStrLn "Pretending to build and submit a transaction"

txId :: TxId 
txId = "89192bb6713811ed483e7b6c9efd168c32c6050b76c33a9a9e1e9d500308cab8"

main :: IO ()
main = do 
    nftMp <- readTypedScript $ "data" </> "nftMp.plutus"

    -- Print the Plutus ledger version used by the minting policy.
    putStr "NFT Minting Policy version: "
    print $ getPlutusVersion nftMp

    let policy =
          toMintingPolicy $
            nftMp 
              # TxOutRef {txOutRefId = txId, txOutRefIdx = 1}
              # ("A" :: TokenName)

    usePolicy policy
