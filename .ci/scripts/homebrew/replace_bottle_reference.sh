#!/usr/bin/env bash

sed -i "s/# bottle ventura/sha256 cellar: :any, ventura: \"${BOTTLE_INFO_I7}\"/" ./HomebrewFormula/ligo.rb
sed -i "s/# bottle arm64_ventura/sha256 cellar: :any, arm64_ventura: \"${BOTTLE_INFO_M1}\"/" ./HomebrewFormula/ligo.rb
