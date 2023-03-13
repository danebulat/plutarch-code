{-# LANGUAGE QualifiedDo          #-}
{-# LANGUAGE OverloadedRecordDot  #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ImportQualifiedPost  #-}

module Validators.Guess where

import Plutarch.Prelude
import Plutarch.Api.V2.Contexts
import Plutarch.Monadic            qualified as P
import Plutarch.DataRepr           (PDataFields)

import Data.ByteString.Short       qualified as SBS       
import Data.Default                (def)
import Data.Text                   (Text, unpack)

import Plutarch
import Plutarch.Evaluate
import Plutarch.Builtin            (pforgetData)
import Plutarch.Script             qualified as PS
import PlutusLedgerApi.V2          qualified as PL
import PlutusLedgerApi.V1.Interval

-- ----------------------------------------------------------------------
-- Redeemer Term
-- ----------------------------------------------------------------------

newtype PRedeem (s :: S)
    = PRedeem 
        (Term s (PDataRecord '["_0" ':= PInteger]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields, PEq, PTryFrom PData)

instance DerivePlutusType PRedeem where
  type DPTStrat _ = PlutusTypeData

-- ----------------------------------------------------------------------
-- Datum Term
-- ----------------------------------------------------------------------

newtype PDat (s :: S) 
    = PDat 
        (Term s (PDataRecord '["_0" ':= PInteger]))
  deriving stock Generic
  deriving anyclass (PlutusType, PIsData, PDataFields, PEq, PTryFrom PData)

instance DerivePlutusType PDat where
  type DPTStrat _ = PlutusTypeData

-- ----------------------------------------------------------------------
-- Validator script
-- ----------------------------------------------------------------------
 
matchGuess
    :: Term s (
       PData
  :--> PData
  :--> PAsData PScriptContext
  :--> POpaque)
matchGuess = plam $ \dat red ctx' -> P.do
  -- confirm script is for spending
  ctx         <- pletFields @'["purpose"] ctx'
  PSpending _ <- pmatch ctx.purpose

  -- coerce datum and redeemer
  (dat, _) <- ptryFrom @PDat @PData dat
  (red, _) <- ptryFrom @PRedeem @PData red

  -- get datum and redeemer fields
  d <- pletFields @'["_0"] dat
  r <- pletFields @'["_0"] red

  -- check if datum matches redeemer
  pif (pfromData d._0 #== pfromData r._0)
    (popaque $ pconstant ())
    (ptraceError $ pconstant "wrong guess")

-- ----------------------------------------------------------------------
-- Mock
-- ----------------------------------------------------------------------

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
      [PL.PubKeyHash "f013", PL.PubKeyHash "ab45"]   -- signatories
      (PL.fromList [])     -- redeemers
      (PL.fromList [])     -- datums
      ""                   -- id
    )
    (PL.Spending (PL.TxOutRef "" 1))

-- ----------------------------------------------------------------------
-- Evaluate
-- ----------------------------------------------------------------------

appliedValidator :: Term s POpaque
appliedValidator = matchGuess # datum # red # ctx
  where
    -- represents guess in datum and the provided redeemer
    num :: Integer -> Term s (PAsData PInteger)
    num x = pdata $ pconstant x
  
    datum :: Term s PData
    datum = pforgetData $ pdata $ pcon $ PDat datumFields
     
    datumFields :: Term _ (PDataRecord '["_0" ':= PInteger])
    datumFields = pdcons # num 22 # pdnil
    
    red :: Term s PData 
    red = pforgetData $ pdata $ pcon $ PRedeem 
        $ pdcons # num 22 # pdnil
     
    ctx = pdata $ pconstant mockCtx

-- evaluate a fully applied script
eval :: (Either EvalError Script, PL.ExBudget, [Text])
eval = case compile def appliedValidator of
    Right script -> evalScript script
    Left  err    -> error (unpack err)

-- compile guess script
eval' :: Script
eval' = case compile def matchGuess of
  Left err     -> error (unpack err)
  Right script -> let (s, _, _) = evalScript script
                  in case s of
                       Right s -> s
                       _       -> error "error"

-- compile and output guess script or an error
eval'' :: Either EvalError Script
eval'' = case compile def matchGuess of
  Right script -> let (s, _, _) = evalScript script in s
  Left err     -> error (unpack err)

-- ----------------------------------------------------------------------
-- Serialise
-- ----------------------------------------------------------------------

serialise' :: SBS.ShortByteString
serialise' = PS.serialiseScript eval'
