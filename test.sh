#!/bin/bash

RED='\033[0;31m';
GREEN='\033[0;32m';
NC='\033[0m';

for file in $1/*.lat; do
    res="$(./latc $file 2>&1 | head -n 1)"
    if [[ $res =~ "OK" ]]; then
        printf "${GREEN}PASS${NC}\n";
    else
        printf "${RED}FAIL${NC}\n";
    fi
done