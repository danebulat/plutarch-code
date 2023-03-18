{-# LANGUAGE ImportQualifiedPost #-}

module MintingPolicies.NFT where

import Plutarch.Api.V2
import Plutarch.Prelude
import Plutarch.Api.V1.Value as PValue
import Plutarch.Extra.TermCont 

nftMp' :: ClosedTerm (PTxOutRef :--> PTokenName :--> PMintingPolicy)
nftMp' = plam $ \ref tn _ ctx' -> popaque $
    unTermCont $ do
      ctx <- tcont $ pletFields @'["txInfo", "purpose"] ctx'
      PMinting mintF <- tcont . pmatch $ getField @"purpose" ctx
      let ownSym = pfield @"_0" # mintF
      txInfo <- tcont $ pletFields @'["inputs", "mint"] $ getField @"txInfo" ctx
      pguardC "UTxO not consumed" $
        pany # plam (\x -> pfield @"outRef" # x #== pdata ref) #$ pfromData $ 
          getField @"inputs" txInfo
      pguardC "Wrong NFT mint amount" $
        pvalueOf # getField @"mint" txInfo # ownSym # tn #== 1
      pure $ pconstant ()

nftMp :: ClosedTerm (PTxOutRef :--> PTokenName :--> PMintingPolicy)
nftMp = plam $ \ref tn _ ctx' -> popaque $
    unTermCont $ do 
      ctx <- pletFieldsC @'["txInfo", "purpose"] ctx'
      PMinting mintF <- pmatchC $ ctx.purpose
      let ownSym = pfield @"_0" # mintF
      txInfo <- pletFieldsC @'["inputs", "mint"] ctx.txInfo

      pguardC "UTxO not consumed" $
        pany # plam (\x -> pfield @"outRef" # x #== pdata ref) #$ pfromData txInfo.inputs
      pguardC "Wrong NFT mint amount" $
        pvalueOf # txInfo.mint # ownSym # tn #== 1
      pure $ pconstant ()



