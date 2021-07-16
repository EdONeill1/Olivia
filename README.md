# Olivia
### Olivia is an extension to Dijkstras Guarded Command Language. It's purpose is to highlight that correctness is something that can be achieved quite elegantly given a language whise syntax is minimal and actually faciliates correctness.

## Installation
First clone the repoistory.

To install Haskell and Cabal, copy-paste the commands below according to your OS

### Windows
Tested on a friends Windows machine. Their ghc version was 9.0.1 and their Cabal version was 3.4.0.0

Installation for Windows requires Chocolately. The following commands must be run on Powershell.
```
Set-ExecutionPolicy Bypass -Scope Process -Force; [System.Net.ServicePointManager]::SecurityProtocol = [System.Net.ServicePointManager]::SecurityProtocol -bor 3072; iex ((New-Object System.Net.WebClient).DownloadString('https://chocolatey.org/install.ps1'))
choco install haskell-dev
refreshenv
```
To run a program with Windows, one must write : `cabal run Olivia Docs/foo.lsy` to run a program from the Docs folder or `cabal run Olivia Programs/foo.lsy` from the Programs folder.

### Mac OS
This was tested on my own machine and a friends. My friends ghc version and Cabal version was 8.10.4 and 3.4.0.0 respestively. My ghc version is 8.8.3 and my Cabal version is 3.2.0.0.
```
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
source ~/.zshrc  otherwise source ~/.bashrc
echo "Upgrading Cabal" ;
cabal install Cabal cabal-install
```

### Ubuntu
Untested
```
sudo apt-get install haskell-platform
```

### Debian
Untested
```
sudo yum install haskell-platform
```

### Fedora
Untested
```
sudo dnf install haskell-platform
```

Confirm that both the Haskell compiler and cabal are installed with :
```
ghc --version
cabal --version
```

If there's an error in installation visit : `https://www.haskell.org/downloads/#minimal`

Test the following command to make sure everything worked on MacOS : ``` ./Olivia test_program ```
Test the following command to make sure everything worked on Windows : ``` cabal run Olivia test_program ```

If one wishes to build the project (however please note the Olivia program which executes programs is included already):
```
cabal configure
cabal build
mv ../Olivia/dist-newstyle/build/x86_64-osx/ghc-8.8.3/Olivia-0.1.0.0/x/Olivia/build/Olivia/Olivia ../Olivia
```

