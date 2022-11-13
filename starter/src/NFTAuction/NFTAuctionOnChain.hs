-- Jacob Ellis
-- NFT Auction Contract

-- Lock an NFT into a contract with a time limit and minimum bid amount. The bidder with the highest bid that is over the minimum amount and the time is up gets the NFT otherwise funds are returned.

--1 Extensions
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}

--This is to work not only with Strings
{-# LANGUAGE OverloadedStrings   #-}

-- required to use custom data types
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE MultiParamTypeClasses      #-} 


module NFTAuction.NFTAuctionOnChain where
-- Sections of a Plutus contract


--2 Imports
import PlutusTx
import PlutusTx.Prelude
import qualified Ledger.Address                                  as V1LAddress
import qualified Plutus.V2.Ledger.Api                            as V2LedgerApi
import qualified Plutus.V2.Ledger.Contexts                       as Contexts
import qualified Plutus.V1.Ledger.Value                          as ValueV1
import qualified Plutus.Script.Utils.V2.Typed.Scripts.Validators as V2UtilsTypeScripts
import qualified Prelude                                         as P
import qualified Ledger                                          (PaymentPubKeyHash, unPaymentPubKeyHash, AssetClass, Datum)
import qualified Plutus.V1.Ledger.Interval                       as LedgerIntervalV1
import qualified Ledger.Ada                                      as Ada
import qualified Ledger.Tx                                       as LTX

--3 Onchain code

type NFT = Ledger.AssetClass

data Dat = Dat
    {
    dSeller :: Ledger.PaymentPubKeyHash,
    dNFT :: NFT,
    dDeadline :: V2LedgerApi.POSIXTime,
    dMinBid :: Integer,
    dCurrentHighestBidder :: Maybe Ledger.PaymentPubKeyHash,
    dCurrentHighestBid :: Maybe Integer
    } deriving P.Show

instance Eq Dat where 
    {-# INLINABLE (==) #-}
    a == b = (dSeller a == dSeller b) &&
             (dDeadline a == dDeadline b) &&
             (dNFT a == dNFT b) &&
             (dMinBid a == dMinBid b) &&
             (dCurrentHighestBidder a == dCurrentHighestBidder b) &&
             (dCurrentHighestBid a == dCurrentHighestBid b)

PlutusTx.unstableMakeIsData ''Dat
PlutusTx.makeLift ''Dat

data Redeem = EndAuction | PlaceBid Ledger.PaymentPubKeyHash Integer
    deriving P.Show

PlutusTx.unstableMakeIsData ''Redeem -- This is to instantiate the IsData class
PlutusTx.makeLift ''Redeem

{-# INLINABLE mainValidator #-}
--Actual validator logic
mainValidator :: Dat -> Redeem -> Contexts.ScriptContext -> Bool
mainValidator d r context =
    case r of
        EndAuction -> validateEndAuctionAction d context
        PlaceBid bidder bid -> validatePlaceBidAction bidder bid d context

{-# INLINABLE validateEndAuctionAction #-}
validateEndAuctionAction :: Dat -> Contexts.ScriptContext -> Bool
validateEndAuctionAction d context = 
    traceIfFalse "Only the seller or buyer can close the auction." (signedBySeller || signedByBuyer) &&
    traceIfFalse "Deadline not yet reached"  deadlinepassed &&
    case hasHighestBid of 
        False -> traceIfFalse "NFT must return to seller" (getsOutputNFT (dSeller d) (dNFT d)) --logic for nft to seller in output
        True -> traceIfFalse "Winning Bidder must get token." (case dCurrentHighestBidder d of
                                    Nothing -> False
                                    Just bbidder -> (getsOutputNFT (bbidder) (dNFT d))) &&
                traceIfFalse "Seller must get paid." (getsOutputBid (dSeller d))

    where 
        txinfo :: Contexts.TxInfo
        txinfo = Contexts.scriptContextTxInfo context
        
        getsOutputNFT :: Ledger.PaymentPubKeyHash -> Ledger.AssetClass -> Bool
        getsOutputNFT ppkh n =
            let
                [outtxs] = [ outtxs' | outtxs' <- Contexts.txInfoOutputs txinfo, Contexts.txOutValue outtxs' == (ValueV1.assetClassValue n 1) ]
            in 
                Contexts.txOutAddress outtxs == V1LAddress.pubKeyHashAddress ppkh Nothing            

        getsOutputBid :: Ledger.PaymentPubKeyHash -> Bool
        getsOutputBid ppkh =
            case dCurrentHighestBid d of
                Nothing -> False
                Just b ->
                        let
                            [outtxs] = [ outtxs' | outtxs' <- Contexts.txInfoOutputs txinfo, Contexts.txOutValue outtxs' == (Ada.lovelaceValueOf $ b) ]
                        in 
                            Contexts.txOutAddress outtxs == V1LAddress.pubKeyHashAddress ppkh Nothing  

        signedBySeller :: Bool
        signedBySeller = Contexts.txSignedBy txinfo $ Ledger.unPaymentPubKeyHash (dSeller d)

        signedByBuyer :: Bool
        signedByBuyer = 
            case dCurrentHighestBidder d of
                Nothing -> False
                Just b -> Contexts.txSignedBy txinfo $ Ledger.unPaymentPubKeyHash b
            
        hasHighestBid :: Bool
        hasHighestBid =
            case dCurrentHighestBidder d of
                Nothing -> False 
                Just _ -> case dCurrentHighestBid d of
                    Nothing -> False
                    Just _ -> True

        deadlinepassed :: Bool
        deadlinepassed = LedgerIntervalV1.contains (LedgerIntervalV1.from (dDeadline d)) (Contexts.txInfoValidRange txinfo)


--     dSeller :: Ledger.PaymentPubKeyHash,
--     dNFT :: NFT,
--     dDeadline :: V2LedgerApi.POSIXTime,
--     dMinBid :: Integer,


{-# INLINABLE validatePlaceBidAction #-}
validatePlaceBidAction :: Ledger.PaymentPubKeyHash -> Integer -> Dat -> Contexts.ScriptContext -> Bool
validatePlaceBidAction bidder bid d context = traceIfFalse "Amount sent less than the NFT price" checkIfPaymentIsCorrect &&
                                              traceIfFalse "Incorrect Datum Output"  ((dSeller outputDatum == dSeller d) 
                                                                                     && (dNFT outputDatum == dNFT d) 
                                                                                     && (dDeadline outputDatum == dDeadline d)
                                                                                     && (dMinBid outputDatum == dMinBid d)) &&
                                              traceIfFalse "Incorrect Output Value" (Contexts.txOutValue ownOutput == (ValueV1.assetClassValue (dNFT d) 1) <> Ada.lovelaceValueOf (bid)) &&
                                              traceIfFalse "Deadline Has Passed" deadlinenotpassed &&
                                              traceIfFalse "Incorrect refund of previous bidder" checkRefund
    where 
        txinfo :: Contexts.TxInfo
        txinfo = Contexts.scriptContextTxInfo context

        checkIfPaymentIsCorrect :: Bool
        checkIfPaymentIsCorrect =
            let 
                valuepay = Contexts.valuePaidTo txinfo $ Ledger.unPaymentPubKeyHash (dSeller d)
            in
                valuepay `ValueV1.gt` (Ada.lovelaceValueOf $ dMinBid d)

        
        ownOutput   :: Contexts.TxOut
        outputDatum :: Dat
        (ownOutput, outputDatum) = case Contexts.getContinuingOutputs context of
            [o] -> case V2LedgerApi.txOutDatum o of
                V2LedgerApi.NoOutputDatum   -> traceError "No Datum"
                V2LedgerApi.OutputDatumHash h -> case Contexts.findDatum h txinfo of
                    Nothing        -> traceError "No Datum"
                    Just (V2LedgerApi.Datum dd) ->  case PlutusTx.fromBuiltinData dd of
                        Just ad' -> (o, ad')
                        Nothing  -> traceError "error decoding data"
                V2LedgerApi.OutputDatum dt   -> case PlutusTx.fromBuiltinData (PlutusTx.toBuiltinData dt) of
                        Just ad' -> (o, ad')
                        Nothing  -> traceError "error decoding data"
            _   -> traceError "expected exactly one continuing output"

        deadlinenotpassed :: Bool
        deadlinenotpassed = LedgerIntervalV1.contains (LedgerIntervalV1.to (dDeadline d)) (Contexts.txInfoValidRange txinfo)

        checkRefund :: Bool
        checkRefund = case dCurrentHighestBid d of
            Nothing -> (case dCurrentHighestBidder d of
                Nothing -> True
                Just _  -> False)
            Just bbd -> case dCurrentHighestBidder d of
                Nothing -> False
                Just bbdr ->      
                    let
                        os = [ o
                            | o <- Contexts.txInfoOutputs txinfo
                            , Contexts.txOutAddress o == V1LAddress.pubKeyHashAddress bbdr Nothing
                            ]
                    in
                        case os of
                            [o] -> Contexts.txOutValue o == Ada.lovelaceValueOf bbd
                            _   -> traceError "expected exactly one refund output"


data Auction
instance V2UtilsTypeScripts.ValidatorTypes Auction where
    type instance RedeemerType Auction = Redeem
    type instance DatumType Auction = Dat


--Boilerplate
auctionTypeV :: V2UtilsTypeScripts.TypedValidator Auction
auctionTypeV = V2UtilsTypeScripts.mkTypedValidator @Auction 
    $$(compile [|| mainValidator ||])
    $$(compile [|| wrap ||]) where
        wrap = V2UtilsTypeScripts.mkUntypedValidator @Dat @Redeem

validator :: V2LedgerApi.Validator
validator = V2UtilsTypeScripts.validatorScript auctionTypeV

validatorHash :: V2LedgerApi.ValidatorHash
validatorHash = V2UtilsTypeScripts.validatorHash auctionTypeV