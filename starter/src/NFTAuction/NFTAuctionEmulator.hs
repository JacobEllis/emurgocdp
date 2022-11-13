
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds        #-}

module NFTAuction.NFTAuctionEmulator where

--Plutus modules
import qualified Plutus.Trace.Emulator      as Emulator
import Data.Default                         (Default (..))
import Control.Monad.Freer.Extras           as Extras
import Data.Functor                         (void)
import Plutus.Trace
import qualified Wallet.Emulator.Wallet     as Wallet
import qualified Ledger.TimeSlot            as TimeSlot
import qualified Ledger                     (AssetClass)
import qualified Plutus.V1.Ledger.Value     as ValueV1
import Data.ByteString as S (ByteString, unpack)
import Data.ByteString.Char8 as C8 (pack)
import Data.Char (chr)



-- Our offchain code
import qualified NFTAuction.NFTAuctionOffChain as OffChain

type NFT = Ledger.AssetClass


strToBS :: String -> S.ByteString
strToBS = C8.pack

bsToStr :: S.ByteString -> String
bsToStr = map (chr . fromEnum) . S.unpack


test :: IO ()
test = Emulator.runEmulatorTraceIO trace1

trace1 :: Emulator.EmulatorTrace ()
trace1 = do
    h1 <- Emulator.activateContractWallet (Wallet.knownWallet 1) OffChain.endpoints

    -- Emulator.callEndpoint @"mint" h1 $ OffChain.NFTParams
    --     { OffChain.npToken   = (ValueV1.tokenName (strToBS "JNFT"))
    --     , OffChain.npAddress = Wallet.mockWalletAddress (Wallet.knownWallet 1)
    --     }

    void $ waitNSlots 2
    Emulator.callEndpoint @"start" h1 $ OffChain.StartParams {
        OffChain.spSeller = Wallet.mockWalletPaymentPubKeyHash $ Wallet.knownWallet 1,
        OffChain.spMinBid = 12000000,
        OffChain.spDeadline = TimeSlot.slotToBeginPOSIXTime def 30,
       -- OffChain.spNFT = ValueV1.assetClass (ValueV1.currencySymbol (strToBS "\"3_\\130|\\254\\210\\fe\\215t\\DC2\\150\\SI'\\195\\137:\\182=\\165\\228\\230v\\203}\\240\\190\\DC4\"")) (ValueV1.tokenName (strToBS "JNFT"))
       OffChain.spToken = (ValueV1.tokenName (strToBS "Jacob NFT")),
       OffChain.spAddress = Wallet.mockWalletAddress (Wallet.knownWallet 1)
    }
    void $ waitNSlots 2

    s <- waitNSlots 2
    Extras.logInfo $ "reached " ++ show s







