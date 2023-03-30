#!/bin/bash

# install Nix
sh <(curl -L https://nixos.org/nix/install) --daemon

# install mosh with true color support
sudo mv /usr/local/bin/protoc /usr/local/bin/protoc-new	
sudo apt-get install debhelper autotools-dev protobuf-compiler libprotobuf-dev dh-autoreconf pkg-config libutempter-dev zlib1g-dev libncurses5-dev libssl-dev bash-completion locales -y
cd /tmp
git clone https://github.com/mobile-shell/mosh
cd mosh
./autogen.sh
./configure
make
sudo make install
sudo mv /usr/local/bin/protoc-new /usr/local/bin/protoc

echo "alias 'd'='cd ~/.config/dev && nix develop'" >> ~/.bashrc 
echo "alias 'd2'='cd ~/.config/dev2 && nix develop'" >> ~/.bashrc 

