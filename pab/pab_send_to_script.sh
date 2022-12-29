#!/bin/bash

WALLET_ID=$(cat $PLUTUS_DEVS_SCRIPTS_FILES/wallets/$walletName.id)
echo "WALLET ID:" $WALLET_ID

#Para poder ejecutar el cabal exec necesito estar en la carpeta $PLUTUS_DEVS_HASKELL donde hice el cabal build
CWD=$(pwd)
cd $PLUTUS_DEVS_HASKELL

pkh=$(cabal exec utils-payment-key-hash -- $walletAddr)
skh=$(cabal exec utils-stake-key-hash -- $walletAddr)

cd $CWD

echo "payment key hash: $pkh"
echo "stake key hash: $skh"

printf "\nCantidad ADA: "
read cantidad

printf "\nTxID: "
read txid

printf "\nTxIndex: "
read txindex

#Para poder ejecutar el cabal exec necesito estar en la carpeta $PLUTUS_DEVS_HASKELL donde hice el cabal build
CWD=$(pwd)
cd $PLUTUS_DEVS_HASKELL
tokenName=$(cabal exec utils-consByteString $txid $txindex)
cd $CWD

echo $tokenName

exit


# echo 'POST' \
#   'http://localhost:9080/api/contract/activate' \
#   -H 'accept: application/json;charset=utf-8' \
#   -H 'Content-Type: application/json;charset=utf-8' \
#   -d '{
#     "caWallet": {"getWalletId": "'"$WALLET_ID"'"},
#     "caID": {
#         "contents": {
#             "spDeadline": 1657143764000,
#             "spName": 444,
#             "spAdaQty": '$cantidad'
#         },
#         "tag": "Start"
#     }
# }'

curl -X 'POST' \
  'http://localhost:9080/api/contract/activate' \
  -H 'accept: application/json;charset=utf-8' \
  -H 'Content-Type: application/json;charset=utf-8' \
  -d '{
        "caWallet": {
            "getWalletId": "'"$WALLET_ID"'"
        },
        "caID": {
            "contents": {
            "pmcpFund": '$cantidad',
            "pmcpPoolNFTTokenName": {
                "unTokenName": "\u00000x32af9a7b92e458253056b30374deccbac2acfc23c3d58c196f414a459714dd900f"
            },
            "pmcpPoolNFTTxOutRef": {
                "txOutRefId": {
                "getTxId": "f28b9745deff67e2ffbd7a197c1b32160fe51588f3b1fbd8615054f4d494f353"
                },
                "txOutRefIdx": 0
            },
            "pmcpPoolParam": {
                "ppCurSymbolForMintingNFTPolicy": {
                "unCurrencySymbol": "502f5c883fb9973ab959daad5c4744669609d0cacfaf151e96366300"
                },
                "ppDeadline": 1596059594999,
                "ppInterest": 10,
                "ppMasters": [
                {
                    "unPaymentPubKeyHash": {
                    "getPubKeyHash": "a2c20c77887ace1cd986193e4e75babd8993cfd56995cd5cfce609c2"
                    }
                },
                {
                    "unPaymentPubKeyHash": {
                    "getPubKeyHash": "80a4f45b56b88d1139da23bc4c3c75ec6d32943c087f250b86193ca7"
                    }
                }
                ],
                "ppMaximunInvest": 50000000,
                "ppMinimunClaim": 1000000,
                "ppMinumunCompoundInvest": 3000000,
                "ppMinumunInvest": 5000000,
                "ppPoolNFT": {
                "unAssetClass": [
                    {
                    "unCurrencySymbol": "502f5c883fb9973ab959daad5c4744669609d0cacfaf151e96366300"
                    },
                    {
                    "unTokenName": "\u00000x32af9a7b92e458253056b30374deccbac2acfc23c3d58c196f414a459714dd900f"
                    }
                ]
                },
                "ppPoolNFTTxOutRef": {
                "txOutRefId": {
                    "getTxId": "f28b9745deff67e2ffbd7a197c1b32160fe51588f3b1fbd8615054f4d494f353"
                },
                "txOutRefIdx": 0
                },
                "ppValidTimeRange": 10000
            }
            },
            "tag": "MasterPreparePool"
        }
        }'


# "tpAddress": {
#                 "addressCredential": {
#                     "contents": {"getPubKeyHash": "'"$pkh"'"},
#                     "tag": "PubKeyCredential"
#                 },
#                 "addressStakingCredential": {
#                     "contents": {
#                         "contents": {"getPubKeyHash": "'"$skh"'"},
#                         "tag": "PubKeyCredential"
#                     },
#                     "tag": "StakingHash"
#                 }
#             },