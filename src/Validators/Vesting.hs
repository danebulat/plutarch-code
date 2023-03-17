{-# LANGUAGE QualifiedDo          #-}
{-# LANGUAGE OverloadedRecordDot  #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE UndecidableInstances #-}

module Validators.Vesting where

import Plutarch.Prelude
import Plutarch.Api.V2
import Plutarch.Monadic            qualified as P
import Plutarch.DataRepr           (PDataFields)
import PlutusTx                    qualified
import PlutusTx.AssocMap           qualified as AssocMap
import PlutusLedgerApi.V1.Interval
import Plutarch.Builtin            (pforgetData)
import Plutarch.Script             qualified as PS

-- Extra 
import Plutarch.Extra.Interval
import Plutarch.Extra.TermCont 

-- Evaluating
import Data.ByteString.Short       qualified as SBS
import Data.Default                (def)
import Data.Text                   (Text)
import Data.Text                   qualified as Text
import PlutusLedgerApi.V2          qualified as PL
import Plutarch                    (compile)
import Plutarch.Evaluate
import Plutarch.Script

-- ----------------------------------------------------------------------
-- ptxSignedBy
-- ----------------------------------------------------------------------

ptxSignedBy :: forall (s :: S). 
    Term s (PBuiltinList (PAsData PPubKeyHash) :--> PAsData PPubKeyHash :--> PBool)
ptxSignedBy = phoistAcyclic $
  plam $
    \sigs sig -> pelem # sig # sigs

-- ----------------------------------------------------------------------
-- Datum Term
-- ----------------------------------------------------------------------

newtype PDat (s :: S) = PDat
    (Term s (
        PDataRecord
          '[
             "beneficiary" ':= PByteString  -- key authorised to claim the script funds
           , "deadline"    ':= PInteger     -- claim can be made when deadline passed
           ]
        )
    )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields, PEq, PTryFrom PData)

instance DerivePlutusType PDat where 
  type DPTStrat _ = PlutusTypeData

-- ----------------------------------------------------------------------
-- Validator Script
-- ----------------------------------------------------------------------

{- 
 - TermCont monad version 
 -}
checkVesting
    :: Term s (
       PData 
  :--> PData
  :--> PAsData PScriptContext
  :--> POpaque)
checkVesting = plam $ \dat' _ ctx' -> popaque $ unTermCont do 
    -- coerce dat
    dat <- fst <$> ptryFromC @PDat @PData dat'

    -- confirm script is for spending
    ctx         <- pletFieldsC @'["purpose", "txInfo"] ctx'
    PSpending _ <- pmatchC ctx.purpose

    -- fields 
    datF         <- pletFieldsC @'["beneficiary", "deadline"] dat
    validRangeF  <- pletFieldsC @'["validRange"] ctx.txInfo
    signatoriesF <- pletFieldsC @'["signatories"] ctx.txInfo

    -- lets
    deadline    <- pletC $ pdata $ pcon $ PPOSIXTime $ pfromData datF.deadline
    beneficiary <- pletC $ pdata $ pcon $ PPubKeyHash datF.beneficiary
    signatories <- pletC $ signatoriesF.signatories  
    validRange  <- pletC $ validRangeF.validRange

    -- checks
    pguardC "deadline passed" 
      (pcontains # (pfrom # deadline) # pfromData validRange)

    pguardC "signed by beneficiary" 
      (ptxSignedBy # signatories # beneficiary) 

    -- return unit
    pure $ pforgetData $ pdata $ pcon PUnit

{- 
 - Qualified do version 
 -}
checkVesting'
    :: Term s (
       PData
  :--> PData
  :--> PAsData PScriptContext
  :--> POpaque)
checkVesting' = plam $ \dat' _ ctx' -> P.do 
    -- coerce dat
    (dat, _) <- ptryFrom @PDat @PData dat'

    -- confirm script is for spending
    ctx         <- pletFields @'["purpose", "txInfo"] ctx'
    PSpending _ <- pmatch ctx.purpose

    -- fields 
    datF         <- pletFields @'["beneficiary", "deadline"] dat
    validRangeF  <- pletFields @'["validRange"] ctx.txInfo
    signatoriesF <- pletFields @'["signatories"] ctx.txInfo

    -- lets
    deadline    <- plet $ pdata $ pcon $ PPOSIXTime $ pfromData datF.deadline
    beneficiary <- plet $ pdata $ pcon $ PPubKeyHash datF.beneficiary
    signatories <- plet $ signatoriesF.signatories  
    validRange  <- plet $ validRangeF.validRange

    -- checks
    pif 
      (pcontains # (pfrom # deadline) # pfromData validRange #&& 
       ptxSignedBy # signatories # beneficiary)
         (popaque $ pconstant ())
         perror

-- ----------------------------------------------------------------------
-- Mock Functions 
-- ----------------------------------------------------------------------

-- Mock data for test datum
pkh :: Term s PByteString 
pkh = pconstant "f013" 

deadline :: Term s PInteger 
deadline = 10 

-- Mock context
mockCtx :: PL.ScriptContext
mockCtx =
  PL.ScriptContext
    (PL.TxInfo
      mempty              -- inputs
      mempty              -- referenceInputs
      mempty              -- outputs
      mempty              -- fee
      mempty              -- mint 
      mempty              -- dcert 
      AssocMap.empty      -- wdrl 
      (interval (PL.POSIXTime 20) (PL.POSIXTime 30))  -- validRange
      [PL.PubKeyHash "f013", PL.PubKeyHash "f014"]    -- signatories
      AssocMap.empty      -- redeemers
      AssocMap.empty      -- datums
      ""                  -- id
    )
    (PL.Spending (PL.TxOutRef "" 1))

-- ----------------------------------------------------------------------
-- Evaluation
-- ----------------------------------------------------------------------
 
appliedValidator :: Term s POpaque 
appliedValidator = checkVesting # dat # pred # pctx
  where
    dat = pforgetData $ pdata $ pcon $
            PDat $ pdcons # pdata pkh 
                #$ pdcons # pdata deadline # pdnil
    pred = pforgetData $ pdata $ pconstant $ PL.Redeemer (PlutusTx.toBuiltinData ())
    pctx = pdata $ pconstant mockCtx

-- evaluate a fully applied script
runValidator :: (Either EvalError Script, PL.ExBudget, [Text])
runValidator = case compile def appliedValidator of
    Right script -> evalScript script
    Left  err    -> error (Text.unpack err)

-- compile script + output script and budget
eval :: (Either EvalError Script, PL.ExBudget, [Text])
eval = case compile def checkVesting' of
    Right script -> evalScript script
    Left  err    -> error (Text.unpack err)

-- compile and output script
eval' :: Script
eval' = case compile def checkVesting' of
  Left err     -> error (Text.unpack err)
  Right script -> let (s, _, _) = evalScript script
                  in case s of
                       Right s -> s
                       _       -> error "error"

-- output vesting script
eval'' :: Either EvalError Script
eval'' = case compile def checkVesting' of
  Right script -> let (s, _, _) = evalScript script in s
  Left err     -> error (Text.unpack err)

-- ----------------------------------------------------------------------
-- Serialise
-- ----------------------------------------------------------------------

serialise' :: SBS.ShortByteString
serialise' = serialiseScript eval'
