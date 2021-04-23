{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.TxView (txViewTests) where

import           Cardano.Prelude

import           Hedgehog (Group (..), Property, checkSequential)
import           Hedgehog.Extras.Test.Base (moduleWorkspace, propertyOnce)
import           System.FilePath ((</>))

import           Test.OptParse (execCardanoCLI, noteTempFile)
import           Test.Utilities (diffVsGoldenFile)

{- HLINT ignore "Use camelCase" -}

txViewTests :: IO Bool
txViewTests =
  checkSequential $
    Group "`transaction view` Goldens"
      [ ("golden_view_byron",   golden_view_byron)
      , ("golden_view_shelley", golden_view_shelley)
      , ("golden_view_allegra", golden_view_allegra)
      , ("golden_view_mary",    golden_view_mary)
      ]

golden_view_byron :: Property
golden_view_byron =
  propertyOnce $
  moduleWorkspace "tmp" $ \tempDir -> do
    transactionBodyFile <- noteTempFile tempDir "transaction-body-file"

    -- Create transaction body
    void $
      execCardanoCLI
        [ "transaction", "build-raw"
        , "--byron-era"
        , "--tx-in"
        ,   "F8EC302D19E3C8251C30B1434349BF2E949A1DBF14A4EBC3D512918D2D4D5C56\
            \#88"
        , "--tx-out"
        ,   "5oP9ib6ym3XfwXuy3ksXZzgtBzXSArXAACQVXKqcPhiLnHVYjXJNu2T6Zomh8LAWLV\
            \+68"
        , "--out-file", transactionBodyFile
        ]

    -- View transaction body
    result <-
      execCardanoCLI
        ["transaction", "view", "--tx-body-file", transactionBodyFile]
    diffVsGoldenFile result "test/data/golden/byron/transaction-view.out"

golden_view_shelley :: Property
golden_view_shelley = let
  certDir = "test/data/golden/shelley/certificates"
  certs =
    (certDir </>) <$>
    [ "genesis_key_delegation_certificate"
    , "mir_certificate"
    , "stake_address_deregistration_certificate"
    , "stake_address_registration_certificate"
    , "stake_pool_deregistration_certificate"
    , "stake_pool_registration_certificate"
    ]
  in
  propertyOnce $
  moduleWorkspace "tmp" $ \tempDir -> do
    transactionBodyFile <- noteTempFile tempDir "transaction-body-file"

    -- Create transaction body
    void $
      execCardanoCLI $
        [ "transaction", "build-raw"
        , "--shelley-era"
        , "--tx-in"
        ,   "fe5dd07fb576bff960d6e066eade5b26cdb5afebe29f76ea58d0a098bce5d891\
            \#29"
        , "--tx-out"
        ,   "addr_test1vz7w0r9epak6nmnh3mc8e2ypkjyu8zsc3xf7dpct6k577acxmcfyv+31"
        , "--fee", "32"
        , "--invalid-hereafter", "33"
        , "--withdrawal"
        , "stake_test1up00fz9lyqs5sjks82k22eqz7a9srym9vysjgp3h2ua2v2cm522kg+42"
        , "--out-file", transactionBodyFile
        ]
        ++
        ["--certificate-file=" <> cert | cert <- certs]

    -- View transaction body
    result <-
      execCardanoCLI
        ["transaction", "view", "--tx-body-file", transactionBodyFile]
    diffVsGoldenFile result "test/data/golden/shelley/transaction-view.out"

golden_view_allegra :: Property
golden_view_allegra =
  propertyOnce $
  moduleWorkspace "tmp" $ \tempDir -> do
    transactionBodyFile <- noteTempFile tempDir "transaction-body-file"

    -- Create transaction body
    void $
      execCardanoCLI
        [ "transaction", "build-raw"
        , "--allegra-era"
        , "--tx-in"
        ,   "fe5dd07fb576bff960d6e066eade5b26cdb5afebe29f76ea58d0a098bce5d891\
            \#94"
        , "--tx-out"
        ,   "addr_test1qrefnr4k09pvge6dq83v6s67ruter8sftmky8qrmkqqsxy7q5psgn8tgqmupq4r79jmxlyk4eqt6z6hj5g8jd8393msqaw47f4\
            \+99"
        , "--fee", "100"
        , "--invalid-hereafter", "101"
        , "--out-file", transactionBodyFile
        ]

    -- View transaction body
    result <-
      execCardanoCLI
        ["transaction", "view", "--tx-body-file", transactionBodyFile]
    diffVsGoldenFile result "test/data/golden/allegra/transaction-view.out"

golden_view_mary :: Property
golden_view_mary =
  propertyOnce $
  moduleWorkspace "tmp" $ \tempDir -> do
    transactionBodyFile <- noteTempFile tempDir "transaction-body-file"

    -- Create transaction body
    void $
      execCardanoCLI
        [ "transaction", "build-raw"
        , "--mary-era"
        , "--tx-in"
        ,   "fe5dd07fb576bff960d6e066eade5b26cdb5afebe29f76ea58d0a098bce5d891\
            \#135"
        , "--tx-out"
        ,   "addr_test1qrefnr4k09pvge6dq83v6s67ruter8sftmky8qrmkqqsxy7q5psgn8tgqmupq4r79jmxlyk4eqt6z6hj5g8jd8393msqaw47f4\
            \+138"
        , "--fee", "139"
        , "--invalid-before", "140"
        , "--mint"
        ,   "42 69596718df8203759c0f9e86f3f79d1dd45bc9d34109a4fccc824e02\
            \ + \
            \43 A5D48A2659925B8574DA58B66065A61283634E43C7FB4E76AFD0C35A.snow\
            \ + \
            \44 69596718df8203759c0f9e86f3f79d1dd45bc9d34109a4fccc824e02.sky"
        , "--minting-script-file"
        ,   "test/data/golden/shelley/multisig/scripts/any"
        , "--out-file", transactionBodyFile
        ]

    -- View transaction body
    result <-
      execCardanoCLI
        ["transaction", "view", "--tx-body-file", transactionBodyFile]
    diffVsGoldenFile result "test/data/golden/mary/transaction-view.out"
