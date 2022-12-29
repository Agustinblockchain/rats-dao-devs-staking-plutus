#!/bin/bash

echo "Creando PAB Config File desde TEMPLATE: "

echo " "
echo "--template config: "$CARDANO_PAB_SERVER_TEMPLATE_CONFIG

# cat $CARDANO_PAB_SERVER_TEMPLATE_CONFIG
# echo " "

# $CARDANO_PAB_SERVER_CONFIG=$(echo "$CARDANO_PAB_SERVER_CONFIG" | sed 's,VALIDATOR_SCRIPT_NAME,'"$scriptName"',')
# $CARDANO_PAB_DATABASE=$(echo "$CARDANO_PAB_DATABASE" | sed 's,VALIDATOR_SCRIPT_NAME,'"$scriptName"',')

CARDANO_PAB_SERVER_CONFIG="${CARDANO_PAB_SERVER_CONFIG/VALIDATOR_SCRIPT_NAME/"$scriptName"}"
CARDANO_PAB_DATABASE="${CARDANO_PAB_DATABASE/VALIDATOR_SCRIPT_NAME/"$scriptName"}"

# echo " "
# echo "CARDANO_PAB_SERVER_CONFIG $CARDANO_PAB_SERVER_CONFIG"
# echo " "
# echo "CARDANO_PAB_DATABASE $CARDANO_PAB_DATABASE"

if [[ -f "$CARDANO_PAB_SERVER_CONFIG"  ]]; then
    pointBlockId=$(cat $CARDANO_PAB_SERVER_CONFIG | yq -r '.developmentOptions.pabResumeFrom.pointBlockId')
    pointSlot=$(cat $CARDANO_PAB_SERVER_CONFIG | yq -r '.developmentOptions.pabResumeFrom.pointSlot.getSlot')
fi

if [[ -z "$pointBlockId" || -z "$pointSlot" ]]; then 
    echo "Consultando en NODO ultimo BlockId y Slot ...";

    pointBlockId=$($CARDANO_NODE/cardano-cli query tip --$NETWORK_WITH_MAGIC | jq -r '.hash') 
    pointSlot=$($CARDANO_NODE/cardano-cli query tip --$NETWORK_WITH_MAGIC | jq -r '.slot') 

    echo "Usando BlockId: $pointBlockId y Slot: $pointSlot";
else 
    echo "Encontrados en ConfigFile BlockId: $pointBlockId y Slot: $pointSlot, deseas usarlos (y/n)?";

    read -n 1 -s opcion
    if ! [[ $opcion = "y" ]]; then 

        echo "Consultando en NODO ultimo BlockId y Slot ...";

        pointBlockId=$($CARDANO_NODE/cardano-cli query tip --$NETWORK_WITH_MAGIC | jq -r '.hash') 
        pointSlot=$($CARDANO_NODE/cardano-cli query tip --$NETWORK_WITH_MAGIC | jq -r '.slot') 

        echo "Usando BlockId: $pointBlockId y Slot: $pointSlot";

    fi
fi

cp $CARDANO_PAB_SERVER_TEMPLATE_CONFIG $CARDANO_PAB_SERVER_CONFIG

sed -i 's,$CARDANO_NODE_SOCKET_PATH,'"$CARDANO_NODE_SOCKET_PATH"',' $CARDANO_PAB_SERVER_CONFIG
sed -i 's,$CARDANO_PAB_DATABASE,'"$CARDANO_PAB_DATABASE"',' $CARDANO_PAB_SERVER_CONFIG 

sed -i 's,$NETWORKMAGIC_NRO,'$NETWORKMAGIC_NRO',' $CARDANO_PAB_SERVER_CONFIG

sed -i 's,$pointBlockId,'$pointBlockId',' $CARDANO_PAB_SERVER_CONFIG
sed -i 's,$pointSlot,'$pointSlot',' $CARDANO_PAB_SERVER_CONFIG

sed -i 's,$CARDANO_WALLET_PORT,'"$CARDANO_WALLET_PORT"',' $CARDANO_PAB_SERVER_CONFIG
sed -i 's,$CARDANO_NODE_PORT,'"$CARDANO_NODE_PORT"',' $CARDANO_PAB_SERVER_CONFIG
sed -i 's,$CARDANO_CHAIN_INDEX_PORT,'"$CARDANO_CHAIN_INDEX_PORT"',' $CARDANO_PAB_SERVER_CONFIG
sed -i 's,$CARDANO_PAB_PORT,'"$CARDANO_PAB_PORT"',' $CARDANO_PAB_SERVER_CONFIG


# echo "--config: "$CARDANO_PAB_SERVER_CONFIG
# cat $CARDANO_PAB_SERVER_CONFIG

echo " "
echo "Iniciando Pab DB con: "
echo " "
echo "--Config: "$CARDANO_PAB_SERVER_CONFIG
echo " "
echo "--Database: "$CARDANO_PAB_DATABASE


#Para poder ejecutar el cabal exec necesito estar en la carpeta $PLUTUS_DEVS_HASKELL donde hice el cabal build
CWD=$(pwd)
cd $PLUTUS_DEVS_HASKELL

printf "%s\n" "$scriptNumero"  | cabal exec -- pab-api-server --config $CARDANO_PAB_SERVER_CONFIG  migrate

cd $CWD

printf "\nDatabase created and migrated\n"



