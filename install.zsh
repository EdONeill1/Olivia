#!/bin/zsh

echo " ********** INSTALL SCRIPT ********** "

# Detecting OS, echo $OSTYPE
# Downloading Haskell for mac users
# Downloading Haskell for windows users

([ $(ls -a ~ | grep -q .fhc) ] && echo "Downloading Parsec library from Hoogle..." 
                               && stack install parsec) || 
	
   echo "Download Haskell Compiler..." ; 


