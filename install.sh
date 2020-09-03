#!/bin/bash

function installGHC() {

        printf "You don't seem to have GHC installed!\n"
        printf "Please select one of the following:\n"
        printf "(1) Mac and *nix users\n"
        printf "(2) Windows users\n"

        read -p "Option: " option

        if [ $option == "1" ]; then
                unixOption
        elif [ $option == "2" ]; then
                windowsOption
        else
                printf "Invalid Choice! Please try again\n"
                installGHC
        fi
}
        
function unixOption() {
        (curl -sSL https://get.haskellstack.org/ | sh) || (wget -qO- https://get.haskellstack.org/ | sh)
        stack upgrade
        stack install Text.Parsec
        ./install.sh
}

function windowsOption() {
        printf "Go to https://www.haskell.org/platform/#windows and follow the install instruction\n"
        printf "Run the script again or just compile Main.hs\n"
        exit
}

hasGHC=$(ls -a ~ | grep ghc)

if [ $hasGHC == ".ghc" ]; then 
        ghc Main.hs -o Olivia && printf "Compiled Olivia\n" 
        rm -rf *.o && rm -rf *.hi && printf "Removed all .o and .hi files\n"  
        rm -rf Main && printf "Removed Main\n"   
        mv Olivia /usr/local/bin
        printf "Start REPL by typing OLivia into the command line!\n"
else
        installGHC      
fi
