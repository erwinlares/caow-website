#!/bin/bash
set -e
wget -qO /tmp/quarto.tar.gz "https://github.com/quarto-dev/quarto-cli/releases/download/v${QUARTO_VERSION}/quarto-${QUARTO_VERSION}-linux-amd64.tar.gz"
tar -xzf /tmp/quarto.tar.gz -C $HOME
mkdir -p $HOME/.local/bin
ln -sf "$HOME/quarto-${QUARTO_VERSION}/bin/quarto" $HOME/.local/bin/quarto
export PATH=$HOME/.local/bin:$PATH
quarto render