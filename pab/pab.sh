#!/bin/bash

source "$PLUTUS_DEVS_SCRIPTS/tools/get-script-dir.sh"    
getScriptDir ${BASH_SOURCE[0]}

PLUTUS_DEVS_HASKELL="$DIR/../"

echo "DIR: $DIR" 

echo "PLUTUS_DEVS_HASKEL: $PLUTUS_DEVS_HASKELL" 


minimoADA="1800000"

opcionMenuPab=""

while ! [[ $opcionMenuPab = "0" ]]; do

    printf "\nOPERACIONES EN PAB CON WALLET Y SCRIPT\n"
 
    echo "1: Elegir o Crear Wallet (${walletName})"  
    echo "2: Elegir Validador (${scriptName} - Id: $scriptNumero)"  

    echo "--"

    echo "3: Iniciar Pab Database"
    echo "4: Iniciar Pab API Server"

    echo "--"

    echo "5: Deploy"

    echo "--"

    echo "6: Enviar ADA o Tokens a Script"
    echo "7: Redeem ADA o Tokens de Script"

    echo "--"

    echo "0: Regresar al Menu Principal"

    echo "--"

    echo "Opcion: "

    #read -n 1 -s opcionMenuPab
    read  opcionMenuPab

    if [[ $opcionMenuPab = "1" ]]; then 
        source "$PLUTUS_DEVS_SCRIPTS/main/main_elegir_crear_wallet.sh"    
    fi

    if [[ $opcionMenuPab = "2" ]]; then 
        source "$DIR/pab_elegir_validador.sh"
    fi

    if [[ $opcionMenuPab = "3" ]]; then 
        if [[  $scriptName = "" || $scriptNumero = ""  ]]; then
            printf "\nDebe elegir validador\n"
            echo; read -rsn1 -p "Press any key to continue . . ."; echo
        else
            source "$DIR/pab_init_database.sh"
            echo; read -rsn1 -p "Press any key to continue . . ."; echo
        fi
    fi

    if [[ $opcionMenuPab = "4" ]]; then 
        if [[ $walletName = "" || $scriptName = "" || $scriptNumero = ""  ]]; then
             printf "\nDebe elegir wallet y validador primero\n"
             echo; read -rsn1 -p "Press any key to continue . . ."; echo
        else
            if ! [[ -f "$PLUTUS_DEVS_SCRIPTS_FILES/wallets/${walletName}.json" && -f "$PLUTUS_DEVS_SCRIPTS_FILES/wallets/${walletName}.id"  ]]
            then
                printf "\nWallet ${walletName} JSON o id files no existen. No se pueden inicicar los servicios de PAB con esta wallet.\n"
                echo; read -rsn1 -p "Press any key to continue . . ."; echo
            else
                source "$DIR/pab_init_server.sh"
            fi
        fi
    fi

    if [[ $opcionMenuPab = "5" ]]; then 
        if [[ $walletName = "" ||  $scriptName = "" || $scriptNumero = "" ]]; then
             printf "\nDebe elegir wallet y validador primero\n"
             echo; read -rsn1 -p "Press any key to continue . . ."; echo
        else
            source "$DIR/pab_deploy.sh"
        fi
    fi

    if [[ $opcionMenuPab = "6" ]]; then 
        if [[ $walletName = "" ||  $scriptName = "" || $scriptNumero = "" ]]; then
             printf "\nDebe elegir wallet y validador primero\n"
             echo; read -rsn1 -p "Press any key to continue . . ."; echo
        else
            source "$DIR/pab_send_to_script.sh"
        fi
    fi

    if [[ $opcionMenuPab = "7" ]]; then 
        if [[ $walletName = "" ||  $scriptName = "" ]]; then
             printf "\nDebe elegir wallet y validador primero\n"
             echo; read -rsn1 -p "Press any key to continue . . ."; echo
        else
            source "$DIR/pab-redeem_from_script.sh"
        fi 
    fi  

done