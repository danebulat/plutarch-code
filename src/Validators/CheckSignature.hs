{-# LANGUAGE QualifiedDo          #-}
{-# LANGUAGE OverloadedRecordDot  #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE UndecidableInstances #-}

module Validators.CheckSignature where

import Plutarch.Prelude
import Plutarch.Api.V2
import Plutarch.Monadic            qualified as P
import PlutusTx                    qualified
import PlutusLedgerApi.V1.Interval
import Plutarch.Builtin            (pforgetData)
import Plutarch.Extra.TermCont 

import Data.Default                (def)
import Data.String                 (fromString)
import Data.Text                   (Text)
import Data.Text                   qualified as Text
import PlutusLedgerApi.V2          qualified as PL
import Plutarch                    (compile)
import Plutarch.Evaluate
import Plutarch.Script

-- ---------------------------------------------------------------------- 
--  Match on the script purpose to see if its actually for spending.
--
--  Get the signatories field from `txInfo` (the 7th field), and check
--  if the given pub key hash is present within the signatories.
-- ---------------------------------------------------------------------- 

{-
 - TermCont version
 -}

checkSignatory
    :: Term s (
         PPubKeyHash
    :--> PData
    :--> PData
    :--> PAsData PScriptContext
    :--> POpaque)
checkSignatory = plam $ \ph _ _ ctx' -> popaque $ unTermCont $ do
  ctx         <- pletFieldsC @["txInfo", "purpose"] ctx'
  PSpending _ <- pmatchC ctx.purpose
  let 
    signatories = pfield @"signatories" # ctx.txInfo

  pguardC "not signed by beneficiary" 
    (pelem # pdata ph # pfromData signatories)

  pure $ pforgetData $ pdata $ pcon PUnit

{-
 - Qualified do version
 -}

checkSignatory'
    :: Term s (
         PPubKeyHash
    :--> PData
    :--> PData
    :--> PAsData PScriptContext
    :--> POpaque)
checkSignatory' = plam $ \ph _ _ ctx' -> P.do
  ctx         <- pletFields @["txInfo", "purpose"] ctx'
  PSpending _ <- pmatch ctx.purpose
  let
    signatories = pfield @"signatories" # ctx.txInfo
  pif
    (pelem # pdata ph # pfromData signatories)    
      (popaque $ pconstant ())
      perror

-- ----------------------------------------------------------------------
-- Mock Functions 
-- ----------------------------------------------------------------------

hashStr :: PL.PubKeyHash
hashStr = "abce0f123e"

pkh :: Term s PPubKeyHash
pkh = pconstant hashStr

mockCtx :: PL.ScriptContext
mockCtx =
  PL.ScriptContext
    (PL.TxInfo
      mempty               -- inputs
      mempty               -- referenceInputs
      mempty               -- outputs
      mempty               -- fee
      mempty               -- mint
      mempty               -- dcert
      (PL.fromList [])     -- wdrl
      (interval (PL.POSIXTime 1) (PL.POSIXTime 2))   -- validRange
      [fromString (show hashStr), "f013", "ab45"]    -- signatories
      (PL.fromList [])     -- redeemers
      (PL.fromList [])     -- datums
      ""                   -- id
    )
    (PL.Spending (PL.TxOutRef "" 1))

-- ----------------------------------------------------------------------
-- Evaluation
-- ----------------------------------------------------------------------

appliedValidator :: Term s POpaque
appliedValidator = checkSignatory # pkh # pdat # pred # pctx
  where
    pdat = pforgetData $ pdata $ pconstant $ PL.Datum (PlutusTx.toBuiltinData ())
    pred = pforgetData $ pdata $ pconstant $ PL.Redeemer (PlutusTx.toBuiltinData ())
    pctx = pdata $ pconstant mockCtx

-- evaluate a fully applied script
runValidator :: (Either EvalError Script, PL.ExBudget, [Text])
runValidator = case compile def appliedValidator of
    Right script -> evalScript script
    Left  err    -> error (Text.unpack err)

-- compile script + output script and budget
eval :: (Either EvalError Script, PL.ExBudget, [Text])
eval = case compile def checkSignatory of
    Right script -> evalScript script
    Left  err    -> error (Text.unpack err)

-- compile and output script
eval' :: Script
eval' = case compile def checkSignatory of
  Left err     -> error (Text.unpack err)
  Right script -> let (s, _, _) = evalScript script
                  in case s of
                       Right s -> s
                       _       -> error "error"

-- output script
eval'' :: Either EvalError Script
eval'' = case compile def checkSignatory of
  Right script -> let (s, _, _) = evalScript script in s
  Left err     -> error (Text.unpack err)
