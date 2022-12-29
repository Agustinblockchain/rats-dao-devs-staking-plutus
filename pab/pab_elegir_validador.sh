#!/bin/bash

scriptNumero="1"
scriptName="StakePlusV2"

scriptNumeroOpcionExportCBOR=3
scriptNumeroOpcionExportHash=4


until [[ -f "$PLUTUS_DEVS_SCRIPTS_FILES/validators/V2/${scriptName}.plutus" && -f "$PLUTUS_DEVS_SCRIPTS_FILES/validators/V2/${scriptName}.hash"   && -f "$PLUTUS_DEVS_SCRIPTS_FILES/validators/V2/${scriptName}.addr" ]]
do

    if ! [[ -f "$PLUTUS_DEVS_SCRIPTS_FILES/validators/V2/${scriptName}.plutus" && -f "$PLUTUS_DEVS_SCRIPTS_FILES/validators/V2/${scriptName}.hash" ]]
    then
        printf "\nValidator script file ${scriptName} no existe\n"
    else
        $CARDANO_NODE/cardano-cli address build  \
        --payment-script-file $PLUTUS_DEVS_SCRIPTS_FILES/validators/V2/${scriptName}.plutus --out-file $PLUTUS_DEVS_SCRIPTS_FILES/validators/V2/${scriptName}.addr --$NETWORK_WITH_MAGIC
    fi

    printf "\nDesea crear files .plutus, .hash del validator en haskell (y/n)\nImportante: Necesita tener NODO configurado e iniciado\n"
    read -n 1 -s opcion
    if [[ $opcion = "y" ]]; then
     
        #Para poder ejecutar el cabal exec necesito estar en la carpeta $PLUTUS_DEVS_HASKELL donde hice el cabal build
        CWD=$(pwd)
        cd $PLUTUS_DEVS_HASKELL

        printf "%s\n%s\n%s\n" "$scriptNumeroOpcionExportCBOR" "$PLUTUS_DEVS_SCRIPTS_FILES/validators/V2" "$scriptName" | cabal exec deploy-auto 
        printf "%s\n%s\n%s\n" "$scriptNumeroOpcionExportHash" "$PLUTUS_DEVS_SCRIPTS_FILES/validators/V2" "$scriptName" | cabal exec deploy-auto 
        
        cd $CWD

        $CARDANO_NODE/cardano-cli address build  \
        --payment-script-file $PLUTUS_DEVS_SCRIPTS_FILES/validators/V2/${scriptName}.plutus --out-file $PLUTUS_DEVS_SCRIPTS_FILES/validators/V2/${scriptName}.addr --$NETWORK_WITH_MAGIC

    fi

done

scriptAddr=$(cat $PLUTUS_DEVS_SCRIPTS_FILES/validators/V2/${scriptName}.addr)

echo "Script Address:" $scriptAddr