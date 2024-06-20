#!/usr/bin/env bash

sed -i "s/# bottle sonoma/${BOTTLE_INFO_I7}/" HomebrewFormula/ligo.rb
sed -i "s/# bottle arm64_sonoma/${BOTTLE_INFO_M1}/" HomebrewFormula/ligo.rb
