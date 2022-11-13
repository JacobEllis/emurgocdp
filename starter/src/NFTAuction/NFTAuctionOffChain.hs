
{-# LANGUAGE DataKinds #-} --Makes the kind system extensible
{-# LANGUAGE TemplateHaskell #-} --allows you to do type-safe compile-time meta-programming
{-# LANGUAGE NoImplicitPrelude   #-} --PlutusTx prelude has priority over Haskell prelude
{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE TypeApplications    #-} --required to use custo datatypes
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE RecordWildCards #-} -- To allow notation like GrabParams {..}

{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}


module NFTAuction.NFTAuctionOffChain where
-- Sections of a Plutus contract

-- Haskell imports
import qualified Control.Monad            as Monad (void)
import qualified GHC.Generics                        as GHCGenerics (Generic)
import qualified Data.Aeson                          as DataAeson (ToJSON, FromJSON)
import qualified Data.OpenApi.Schema                 as DataOpenApiSchema (ToSchema)
import qualified Prelude                  as P
import qualified Data.Void                as Void (Void)
import qualified Data.Map                 as Map
import qualified Data.Text                           as DataText (Text)


--2 Imports
import PlutusTx
import PlutusTx.Prelude                                          hiding (Semigroup(..), unless)
import qualified Ledger.Address                                  as V1LAddress
import qualified Plutus.V2.Ledger.Api                            as V2LedgerApi
import qualified Plutus.V2.Ledger.Contexts                       as Contexts
import qualified Plutus.V1.Ledger.Contexts                       as ContextsV1
import qualified Plutus.V1.Ledger.Value                          as ValueV1
import qualified Plutus.Script.Utils.V1.Scripts                  as V1UtlisScripts
import qualified Plutus.Script.Utils.V2.Typed.Scripts.Validators as V2UtilsTypeScripts
import qualified Prelude                                         as P 
import qualified Ledger                                          (PaymentPubKeyHash, unPaymentPubKeyHash, AssetClass, Datum)
import qualified Plutus.V1.Ledger.Interval                       as LedgerIntervalV1
import qualified Ledger.Ada                                      as Ada
import qualified Ledger.Tx                                       as LTX
import qualified Plutus.Contract                                 as PlutusContract
import qualified Ledger.Constraints                              as Constraints
import qualified Plutus.V1.Ledger.Scripts                        as ScriptsLedger
import qualified Ledger.Typed.Scripts                            as Scripts
import qualified Text.Printf                                     as TextPrintf (printf)
import qualified NFTAuction.NFTAuctionOnChain                    as OnChain
import Data.ByteString as S (ByteString, unpack)
import Data.ByteString.Char8 as C8 (pack)
import Data.Char (chr)


type NFT = Ledger.AssetClass


strToBS :: P.String -> S.ByteString
strToBS = C8.pack

bsToStr :: S.ByteString -> P.String
bsToStr = P.map (chr . P.fromEnum) . S.unpack

{-# INLINABLE mkPolicy #-}
mkPolicy :: ContextsV1.TxOutRef -> V2LedgerApi.TokenName -> () -> ContextsV1.ScriptContext -> Bool
mkPolicy oref tn () ctx = traceIfFalse "UTxO not consumed"   hasUTxO           &&
                          traceIfFalse "wrong amount minted" checkMintedAmount
  where
    info :: ContextsV1.TxInfo
    info = ContextsV1.scriptContextTxInfo ctx

    hasUTxO :: Bool
    hasUTxO = any (\i -> ContextsV1.txInInfoOutRef i == oref) $ ContextsV1.txInfoInputs info

    checkMintedAmount :: Bool
    checkMintedAmount = case ValueV1.flattenValue (ContextsV1.txInfoMint info) of
        [(_, tn', amt)] -> tn' == tn && amt == 1
        _               -> False

policy :: ContextsV1.TxOutRef -> V2LedgerApi.TokenName -> Scripts.MintingPolicy
policy oref tn = ScriptsLedger.mkMintingPolicyScript $
    $$(PlutusTx.compile [|| \oref' tn' -> Scripts.mkUntypedMintingPolicy $ mkPolicy oref' tn' ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode oref
    `PlutusTx.applyCode`
    PlutusTx.liftCode tn

curSymbol :: ContextsV1.TxOutRef -> V2LedgerApi.TokenName -> V2LedgerApi.CurrencySymbol
curSymbol oref tn = V1UtlisScripts.scriptCurrencySymbol $ policy oref tn


--endpoints (contract monads)

data StartParams = StartParams 
    {
        spSeller :: !Ledger.PaymentPubKeyHash,
        spToken   :: !V2LedgerApi.TokenName,
        spAddress :: !V1LAddress.Address,
        spDeadline :: !V2LedgerApi.POSIXTime,
        spMinBid :: !Integer
    } deriving (GHCGenerics.Generic, DataAeson.ToJSON, DataAeson.FromJSON, DataOpenApiSchema.ToSchema)

data PlaceBidParams = PlaceBidParams
    {
        pbNFT :: !NFT,
        pbBid :: !Integer
    } deriving (GHCGenerics.Generic, DataAeson.ToJSON, DataAeson.FromJSON, DataOpenApiSchema.ToSchema)


data CloseAuctionParams = CloseAuctionParams
    {
        caNFT :: !NFT
    } deriving (GHCGenerics.Generic, DataAeson.ToJSON, DataAeson.FromJSON, DataOpenApiSchema.ToSchema)

data NFTParams = NFTParams
    { npToken   :: !V2LedgerApi.TokenName
    , npAddress :: !V1LAddress.Address
    } deriving (GHCGenerics.Generic, DataAeson.ToJSON, DataAeson.FromJSON, DataOpenApiSchema.ToSchema)

type NFTAucctionSchema = 
    PlutusContract.Endpoint "start" StartParams
    PlutusContract..\/ PlutusContract.Endpoint "bid" PlaceBidParams
    PlutusContract..\/ PlutusContract.Endpoint "close" CloseAuctionParams
    PlutusContract..\/ PlutusContract.Endpoint "mint" NFTParams


start :: PlutusContract.AsContractError e => StartParams -> PlutusContract.Contract w s e ()
start sp = do
    utxos <- PlutusContract.utxosAt $ spAddress sp
    case Map.keys utxos of
        []       -> PlutusContract.logError @P.String "no utxo found"
        oref : _ -> do
            let tn      = spToken sp   
            let d = OnChain.Dat
                    {
                        OnChain.dSeller = spSeller sp,
                        OnChain.dNFT = (ValueV1.assetClass (curSymbol oref tn) tn),
                        OnChain.dDeadline = spDeadline sp,
                        OnChain.dMinBid = spMinBid sp,
                        OnChain.dCurrentHighestBid = Nothing,
                        OnChain.dCurrentHighestBidder = Nothing
                    }
                v = (ValueV1.singleton (curSymbol oref tn) tn 1) P.<> Ada.lovelaceValueOf 50000000
                tx = Constraints.mustPayToOtherScript OnChain.validatorHash (ScriptsLedger.Datum $ PlutusTx.toBuiltinData d) v
                lookups = Constraints.plutusV2OtherScript OnChain.validator
                dsp = bsToStr (V2LedgerApi.fromBuiltin (V2LedgerApi.unCurrencySymbol (curSymbol oref tn)))   
            submittedTx <- PlutusContract.submitTxConstraintsWith @Void.Void lookups tx
            Monad.void $ PlutusContract.awaitTxConfirmed $ LTX.getCardanoTxId submittedTx
            PlutusContract.logInfo @P.String $ "created contract"


-- bid :: PlaceBidParams -> PlutusContract.Contract w s DataText.Text ()
-- bid pbp = do
--     (oref, o, )

mint :: NFTParams -> PlutusContract.Contract w NFTAucctionSchema DataText.Text ()
mint np = do
    utxos <- PlutusContract.utxosAt $ npAddress np
    case Map.keys utxos of
        []       -> PlutusContract.logError @P.String "no utxo found"
        oref : _ -> do
            let tn      = npToken np
            let val     = ValueV1.singleton (curSymbol oref tn) tn 1
                lookups = Constraints.plutusV1MintingPolicy (policy oref tn) P.<> Constraints.unspentOutputs utxos
                tx      = Constraints.mustMintValue val P.<> Constraints.mustSpendPubKeyOutput oref
                dsp = bsToStr (V2LedgerApi.fromBuiltin (V2LedgerApi.unCurrencySymbol (curSymbol oref tn)))
            ledgerTx <- PlutusContract.submitTxConstraintsWith @Void.Void lookups tx
            Monad.void $ PlutusContract.awaitTxConfirmed $ LTX.getCardanoTxId ledgerTx
            PlutusContract.logInfo @P.String $ dsp
            


endpoints :: PlutusContract.Contract () NFTAucctionSchema DataText.Text ()
endpoints = PlutusContract.awaitPromise (start' `PlutusContract.select` mint') >> endpoints
    where 
        start' = PlutusContract.endpoint @"start" start
        mint' = PlutusContract.endpoint @"mint" mint
        --grab' = PlutusContract.endpoint @"grab" $ grab

-- data Dat = Dat
--     {
--     dSeller :: Ledger.PaymentPubKeyHash,
--     dNFT :: NFT,
--     dDeadline :: V2LedgerApi.POSIXTime,
--     dMinBid :: Integer,
--     dCurrentHighestBidder :: Maybe Ledger.PaymentPubKeyHash,
--     dCurrentHighestBid :: Maybe Integer
--     } deriving P.Show

-- instance Eq Dat where 
--     {-# INLINABLE (==) #-}
--     a == b = (dSeller a == dSeller b) &&
--              (dDeadline a == dDeadline b) &&
--              (dNFT a == dNFT b) &&
--              (dMinBid a == dMinBid b) &&
--              (dCurrentHighestBidder a == dCurrentHighestBidder b) &&
--              (dCurrentHighestBid a == dCurrentHighestBid b)

-- PlutusTx.unstableMakeIsData ''Dat
-- PlutusTx.makeLift ''Dat

-- data Redeem = EndAuction | PlaceBid Ledger.PaymentPubKeyHash Integer
--     deriving P.Show

-- PlutusTx.unstableMakeIsData ''Redeem -- This is to instantiate the IsData class
-- PlutusTx.makeLift ''Redeem

-- {-# INLINABLE mainValidator #-}
-- --Actual validator logic
-- mainValidator :: Dat -> Redeem -> Contexts.ScriptContext -> Bool
-- mainValidator d r context =
--     case r of
--         EndAuction -> validateEndAuctionAction d context
--         PlaceBid bidder bid -> validatePlaceBidAction bidder bid d context

-- {-# INLINABLE validateEndAuctionAction #-}
-- validateEndAuctionAction :: Dat -> Contexts.ScriptContext -> Bool
-- validateEndAuctionAction d context = 
--     traceIfFalse "Only the seller or buyer can close the auction." (signedBySeller || signedByBuyer) &&
--     traceIfFalse "Deadline not yet reached"  deadlinepassed &&
--     case hasHighestBid of 
--         False -> traceIfFalse "NFT must return to seller" (getsOutputNFT (dSeller d) (dNFT d)) --logic for nft to seller in output
--         True -> traceIfFalse "Winning Bidder must get token." (case dCurrentHighestBidder d of
--                                     Nothing -> False
--                                     Just bbidder -> (getsOutputNFT (bbidder) (dNFT d))) &&
--                 traceIfFalse "Seller must get paid." (getsOutputBid (dSeller d))

--     where 
--         txinfo :: Contexts.TxInfo
--         txinfo = Contexts.scriptContextTxInfo context
        
--         getsOutputNFT :: Ledger.PaymentPubKeyHash -> Ledger.AssetClass -> Bool
--         getsOutputNFT ppkh n =
--             let
--                 [outtxs] = [ outtxs' | outtxs' <- Contexts.txInfoOutputs txinfo, Contexts.txOutValue outtxs' == (ValueV1.assetClassValue n 1) ]
--             in 
--                 Contexts.txOutAddress outtxs == V1LAddress.pubKeyHashAddress ppkh Nothing            

--         getsOutputBid :: Ledger.PaymentPubKeyHash -> Bool
--         getsOutputBid ppkh =
--             case dCurrentHighestBid d of
--                 Nothing -> False
--                 Just b ->
--                         let
--                             [outtxs] = [ outtxs' | outtxs' <- Contexts.txInfoOutputs txinfo, Contexts.txOutValue outtxs' == (Ada.lovelaceValueOf $ b) ]
--                         in 
--                             Contexts.txOutAddress outtxs == V1LAddress.pubKeyHashAddress ppkh Nothing  

--         signedBySeller :: Bool
--         signedBySeller = Contexts.txSignedBy txinfo $ Ledger.unPaymentPubKeyHash (dSeller d)

--         signedByBuyer :: Bool
--         signedByBuyer = 
--             case dCurrentHighestBidder d of
--                 Nothing -> False
--                 Just b -> Contexts.txSignedBy txinfo $ Ledger.unPaymentPubKeyHash b
            
--         hasHighestBid :: Bool
--         hasHighestBid =
--             case dCurrentHighestBidder d of
--                 Nothing -> False 
--                 Just _ -> case dCurrentHighestBid d of
--                     Nothing -> False
--                     Just _ -> True

--         deadlinepassed :: Bool
--         deadlinepassed = LedgerIntervalV1.contains (LedgerIntervalV1.from (dDeadline d)) (Contexts.txInfoValidRange txinfo)


-- --     dSeller :: Ledger.PaymentPubKeyHash,
-- --     dNFT :: NFT,
-- --     dDeadline :: V2LedgerApi.POSIXTime,
-- --     dMinBid :: Integer,


-- {-# INLINABLE validatePlaceBidAction #-}
-- validatePlaceBidAction :: Ledger.PaymentPubKeyHash -> Integer -> Dat -> Contexts.ScriptContext -> Bool
-- validatePlaceBidAction bidder bid d context = traceIfFalse "Amount sent less than the NFT price" checkIfPaymentIsCorrect &&
--                                               traceIfFalse "Incorrect Datum Output"  ((dSeller outputDatum == dSeller d) 
--                                                                                      && (dNFT outputDatum == dNFT d) 
--                                                                                      && (dDeadline outputDatum == dDeadline d)
--                                                                                      && (dMinBid outputDatum == dMinBid d)) &&
--                                               traceIfFalse "Incorrect Output Value" (Contexts.txOutValue ownOutput == (ValueV1.assetClassValue (dNFT d) 1) <> Ada.lovelaceValueOf (bid)) &&
--                                               traceIfFalse "Deadline Has Passed" deadlinenotpassed &&
--                                               traceIfFalse "Incorrect refund of previous bidder" checkRefund
--     where 
--         txinfo :: Contexts.TxInfo
--         txinfo = Contexts.scriptContextTxInfo context

--         checkIfPaymentIsCorrect :: Bool
--         checkIfPaymentIsCorrect =
--             let 
--                 valuepay = Contexts.valuePaidTo txinfo $ Ledger.unPaymentPubKeyHash (dSeller d)
--             in
--                 valuepay `ValueV1.gt` (Ada.lovelaceValueOf $ dMinBid d)

        
--         ownOutput   :: Contexts.TxOut
--         outputDatum :: Dat
--         (ownOutput, outputDatum) = case Contexts.getContinuingOutputs context of
--             [o] -> case V2LedgerApi.txOutDatum o of
--                 V2LedgerApi.NoOutputDatum   -> traceError "No Datum"
--                 V2LedgerApi.OutputDatumHash h -> case Contexts.findDatum h txinfo of
--                     Nothing        -> traceError "No Datum"
--                     Just (V2LedgerApi.Datum dd) ->  case PlutusTx.fromBuiltinData dd of
--                         Just ad' -> (o, ad')
--                         Nothing  -> traceError "error decoding data"
--                 V2LedgerApi.OutputDatum dt   -> case PlutusTx.fromBuiltinData (PlutusTx.toBuiltinData dt) of
--                         Just ad' -> (o, ad')
--                         Nothing  -> traceError "error decoding data"
--             _   -> traceError "expected exactly one continuing output"

--         deadlinenotpassed :: Bool
--         deadlinenotpassed = LedgerIntervalV1.contains (LedgerIntervalV1.to (dDeadline d)) (Contexts.txInfoValidRange txinfo)

--         checkRefund :: Bool
--         checkRefund = case dCurrentHighestBid d of
--             Nothing -> (case dCurrentHighestBidder d of
--                 Nothing -> True
--                 Just _  -> False)
--             Just bbd -> case dCurrentHighestBidder d of
--                 Nothing -> False
--                 Just bbdr ->      
--                     let
--                         os = [ o
--                             | o <- Contexts.txInfoOutputs txinfo
--                             , Contexts.txOutAddress o == V1LAddress.pubKeyHashAddress bbdr Nothing
--                             ]
--                     in
--                         case os of
--                             [o] -> Contexts.txOutValue o == Ada.lovelaceValueOf bbd
--                             _   -> traceError "expected exactly one refund output"


-- data Auction
-- instance V2UtilsTypeScripts.ValidatorTypes Auction where
--     type instance RedeemerType Auction = Redeem
--     type instance DatumType Auction = Dat


-- --Boilerplate
-- auctionTypeV :: V2UtilsTypeScripts.TypedValidator Auction
-- auctionTypeV = V2UtilsTypeScripts.mkTypedValidator @Auction 
--     $$(compile [|| mainValidator ||])
--     $$(compile [|| wrap ||]) where
--         wrap = V2UtilsTypeScripts.mkUntypedValidator @Dat @Redeem

-- validator :: V2LedgerApi.Validator
-- validator = V2UtilsTypeScripts.validatorScript auctionTypeV

-- validatorHash :: V2LedgerApi.ValidatorHash
-- validatorHash = V2UtilsTypeScripts.validatorHash auctionTypeV