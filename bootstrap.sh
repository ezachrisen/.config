#!/bin/bash

# install Nix
sh <(curl -L https://nixos.org/nix/install) --daemon

# install mosh with true color support
sudo add-apt-repository ppa:keithw/mosh-dev
sudo apt update
sudo apt install -y mosh

