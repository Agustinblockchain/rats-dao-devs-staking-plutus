#!/bin/bash


#Para poder ejecutar el cabal exec necesito estar en la carpeta $PLUTUS_DEVS_HASKELL donde hice el cabal build
CWD=$(pwd)
cd $PLUTUS_DEVS_HASKELL

cabal exec -- deploy

cd $CWD



