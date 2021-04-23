{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | User-friendly pretty-printing for textual user interfaces (TUI)
module Cardano.CLI.Run.Friendly (friendlyTxBodyBS) where

import           Cardano.Prelude

import           Data.Aeson (Value (..), object, toJSON, (.=))
import qualified Data.Aeson as Aeson
import           Data.Yaml (array)
import           Data.Yaml.Pretty (defConfig, encodePretty, setConfCompare)

import           Cardano.Api as Api
import           Cardano.Api.Byron (Lovelace (..))
import           Cardano.Api.Shelley (Address (ShelleyAddress), Certificate (..), StakeAddress (..),
                   StakeCredential (..), StakePoolParameters (..), fromShelleyPaymentCredential,
                   fromShelleyStakeCredential, fromShelleyStakeReference)
import           Shelley.Spec.Ledger.API (MIRPot (ReservesMIR, TreasuryMIR))

import           Cardano.CLI.Helpers (textShow)

friendlyTxBodyBS :: CardanoEra era -> TxBody era -> ByteString
friendlyTxBodyBS era =
  encodePretty (setConfCompare compare defConfig) . friendlyTxBody era

friendlyTxBody :: CardanoEra era -> TxBody era -> Aeson.Value
friendlyTxBody
  era
  (TxBody
    TxBodyContent
      { txIns
      , txOuts
      , txFee
      , txValidityRange
      , txMetadata
      , txAuxScripts
      , txWithdrawals
      , txCertificates
      , txUpdateProposal
      , txMintValue
      }) =
  object
    $   [ "era"     .= era
        , "fee"     .= friendlyFee txFee
        , "inputs"  .= friendlyInputs txIns
        , "outputs" .= map friendlyTxOut txOuts
        ]
    ++  [ "auxiliary scripts" .= friendlyAuxScripts txAuxScripts
        | Just _ <- [auxScriptsSupportedInEra era]
        ]
    ++  [ "certificates" .= friendlyCertificates txCertificates
        | Just _ <- [certificatesSupportedInEra era]
        ]
    ++  [ "metadata" .= friendlyMetadata txMetadata
        | Just _ <- [txMetadataSupportedInEra era]
        ]
    ++  [ "mint" .= friendlyMintValue txMintValue
        | Right _ <- [multiAssetSupportedInEra era]
        ]
    ++  [ "update proposal" .= friendlyUpdateProposal txUpdateProposal
        | Just _ <- [updateProposalSupportedInEra era]
        ]
    ++  friendlyValidityRange era txValidityRange
    ++  [ "withdrawals" .= friendlyWithdrawals txWithdrawals
        | Just _ <- [withdrawalsSupportedInEra era]
        ]

friendlyValidityRange
  :: CardanoEra era
  -> (TxValidityLowerBound era, TxValidityUpperBound era)
  -> [(Text, Aeson.Value)]
friendlyValidityRange era = \case
  ( TxValidityNoLowerBound,
    TxValidityUpperBound ValidityUpperBoundInShelleyEra ttl
    ) ->
      -- special case: in Shelley, upper bound is TTL, and no lower bound
      ["time to live" .= ttl]
  (lowerBound, upperBound) ->
    [ "validity range" .=
        object
          ( [ "lower bound" .=
                case lowerBound of
                  TxValidityNoLowerBound   -> Null
                  TxValidityLowerBound _ s -> toJSON s
            | isLowerBoundSupported
            ]
          ++
            [ "upper bound" .=
                case upperBound of
                  TxValidityNoUpperBound _ -> Null
                  TxValidityUpperBound _ s -> toJSON s
            | isUpperBoundSupported
            ]
          )
    | isLowerBoundSupported || isUpperBoundSupported
    ]
  where
    isLowerBoundSupported = isJust $ validityLowerBoundSupportedInEra era
    isUpperBoundSupported = isJust $ validityUpperBoundSupportedInEra era

friendlyWithdrawals :: TxWithdrawals ViewTx era -> Aeson.Value
friendlyWithdrawals TxWithdrawalsNone = Null
friendlyWithdrawals (TxWithdrawals _ withdrawals) =
  array
    [ object $
        "address" .= serialiseAddress addr   :
        "amount"  .= friendlyLovelace amount :
        friendlyStakeAddress addr
    | (addr, amount, _) <- withdrawals
    ]

friendlyStakeAddress :: StakeAddress -> [(Text, Aeson.Value)]
friendlyStakeAddress (StakeAddress net cred) =
  [ "network" .= net
  , friendlyStakeCredential "" $ fromShelleyStakeCredential cred
  ]

friendlyTxOut :: TxOut era -> Aeson.Value
friendlyTxOut (TxOut addr amount mdatum) =
  object $
  case addr of
    AddressInEra ByronAddressInAnyEra _ ->
      common "Byron"
    AddressInEra (ShelleyAddressInEra _) (ShelleyAddress net cred stake) ->
      "network" .= net                                              :
      friendlyPaymentCredential (fromShelleyPaymentCredential cred) :
      friendlyStakeReference (fromShelleyStakeReference stake)      :
      common "Shelley"                                              ++
      ["datum" .= datum | TxOutDatumHash _ datum <- [mdatum]]
  where
    common :: Text -> [(Text, Aeson.Value)]
    common addressEra =
      [ "address" .=
          (serialiseAddressForTxOut addr <> " (" <> addressEra <> ")")
      , "amount"  .= friendlyTxOutValue amount
      ]

friendlyStakeReference :: StakeAddressReference -> (Text, Aeson.Value)
friendlyStakeReference = \case
  NoStakeAddress            -> "stake reference" .= Null
  StakeAddressByPointer ptr -> "stake reference" .= toJSON ptr
  StakeAddressByValue cred  -> friendlyStakeCredential "reference" cred

friendlyUpdateProposal :: TxUpdateProposal era -> Aeson.Value
friendlyUpdateProposal = \case
  TxUpdateProposalNone -> Null
  TxUpdateProposal _ p -> String $ textShow p

friendlyCertificates :: TxCertificates ViewTx era -> Aeson.Value
friendlyCertificates = \case
  TxCertificatesNone    -> Null
  TxCertificates _ cs _ -> array $ map friendlyCertificate cs

friendlyCertificate :: Certificate -> Aeson.Value
friendlyCertificate =
  object . (:[]) .
  \case
    -- Stake address certificates
    StakeAddressRegistrationCertificate credential ->
      "stake address registration" .=
        object [friendlyStakeCredential "" credential]
    StakeAddressDeregistrationCertificate credential ->
      "stake address deregistration" .=
        object [friendlyStakeCredential "" credential]
    StakeAddressDelegationCertificate credential poolId ->
      "stake address delegation" .=
        object [friendlyStakeCredential "" credential, "pool" .= poolId]

    -- Stake pool certificates
    StakePoolRegistrationCertificate parameters ->
      "stake pool registration" .= friendlyStakePoolParameters parameters
    StakePoolRetirementCertificate poolId epochNo ->
      "stake pool retirement" .= object ["pool" .= poolId, "epoch" .= epochNo]

    -- Special certificates
    GenesisKeyDelegationCertificate genesisKeyHash delegateKeyHash vrfKeyHash ->
      "genesis key delegation" .=
        object
          [ "genesis key hash"  .= serialiseToRawBytesHexText genesisKeyHash
          , "delegate key hash" .= serialiseToRawBytesHexText delegateKeyHash
          , "VRF key hash"      .= serialiseToRawBytesHexText vrfKeyHash
          ]
    MIRCertificate pot target ->
      "MIR" .= object ["pot" .= friendlyMirPot pot, friendlyMirTarget target]

friendlyMirTarget :: MIRTarget -> (Text, Aeson.Value)
friendlyMirTarget = \case
  StakeAddressesMIR addresses ->
    "target stake addresses" .=
      [ object
          [ friendlyStakeCredential "" credential
          , "amount" .= friendlyLovelace lovelace
          ]
      | (credential, lovelace) <- addresses
      ]
  SendToReservesMIR amount -> "send to reserves" .= friendlyLovelace amount
  SendToTreasuryMIR amount -> "send to treasury" .= friendlyLovelace amount

friendlyStakeCredential :: Text -> StakeCredential -> (Text, Aeson.Value)
friendlyStakeCredential subkey = \case
  StakeCredentialByKey keyHash ->
    unwords
      ("stake" : [subkey | subkey /= ""] ++ ["credential key hash"])
    .= serialiseToRawBytesHexText keyHash
  StakeCredentialByScript scriptHash ->
    "stake credential script hash" .= serialiseToRawBytesHexText scriptHash

friendlyPaymentCredential :: PaymentCredential -> (Text, Aeson.Value)
friendlyPaymentCredential = \case
  PaymentCredentialByKey keyHash ->
    "payment credential key hash" .= serialiseToRawBytesHexText keyHash
  PaymentCredentialByScript scriptHash ->
    "payment credential script hash" .= serialiseToRawBytesHexText scriptHash

friendlyMirPot :: MIRPot -> Aeson.Value
friendlyMirPot = \case
  ReservesMIR -> "reserves"
  TreasuryMIR -> "treasury"

friendlyStakePoolParameters :: StakePoolParameters -> Aeson.Value
friendlyStakePoolParameters
  StakePoolParameters
    { stakePoolId
    , stakePoolVRF
    , stakePoolCost
    , stakePoolMargin
    , stakePoolRewardAccount
    , stakePoolPledge
    , stakePoolOwners
    , stakePoolRelays
    , stakePoolMetadata
    } =
  object
    [ "pool"            .= stakePoolId
    , "VRF key hash"    .= serialiseToRawBytesHexText stakePoolVRF
    , "cost"            .= friendlyLovelace stakePoolCost
    , "margin"          .= friendlyRational stakePoolMargin
    , "reward account"  .= object (friendlyStakeAddress stakePoolRewardAccount)
    , "pledge"          .= friendlyLovelace stakePoolPledge
    , "owners (stake key hashes)"
                        .= map serialiseToRawBytesHexText stakePoolOwners
    , "relays"          .= map textShow stakePoolRelays
    , "metadata"        .= fmap textShow stakePoolMetadata
    ]

friendlyRational :: Rational -> Aeson.Value
friendlyRational r =
  String $
    case d of
      1 -> textShow n
      _ -> textShow n <> "/" <> textShow d
  where
    n = numerator r
    d = denominator r

friendlyFee :: TxFee era -> Aeson.Value
friendlyFee = \case
  TxFeeImplicit _     -> "implicit"
  TxFeeExplicit _ fee -> friendlyLovelace fee

friendlyLovelace :: Lovelace -> Aeson.Value
friendlyLovelace (Lovelace value) = String $ textShow value <> " Lovelace"

friendlyMintValue :: TxMintValue ViewTx era -> Aeson.Value
friendlyMintValue = \case
  TxMintNone        -> Null
  TxMintValue _ v _ -> toJSON v

friendlyTxOutValue :: TxOutValue era -> Aeson.Value
friendlyTxOutValue = \case
  TxOutAdaOnly _ lovelace -> friendlyLovelace lovelace
  TxOutValue _ multiasset -> toJSON multiasset

friendlyMetadata :: TxMetadataInEra era -> Aeson.Value
friendlyMetadata = \case
  TxMetadataNone                   -> Null
  TxMetadataInEra _ (TxMetadata m) -> toJSON $ friendlyMetadataValue <$> m

friendlyMetadataValue :: TxMetadataValue -> Aeson.Value
friendlyMetadataValue = \case
  TxMetaNumber int   -> toJSON int
  TxMetaBytes  bytes -> String $ textShow bytes
  TxMetaList   lst   -> array $ map friendlyMetadataValue lst
  TxMetaMap    m     ->
    array
      [array [friendlyMetadataValue k, friendlyMetadataValue v] | (k, v) <- m]
  TxMetaText   text  -> toJSON text

friendlyAuxScripts :: TxAuxScripts era -> Aeson.Value
friendlyAuxScripts = \case
  TxAuxScriptsNone       -> Null
  TxAuxScripts _ scripts -> String $ textShow scripts

friendlyInputs :: [(TxIn, build)] -> Aeson.Value
friendlyInputs = toJSON . map fst
