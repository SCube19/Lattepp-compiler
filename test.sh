#!/bin/bash

RED='\033[0;31m';
GREEN='\033[0;32m';
NC='\033[0m';

for file in $1/*.lat; do
    res="$(./latc $file 2>&1)"
    if [[ $res =~ "OK" ]]; then
        printf "${GREEN}PASS${NC}\n";
    elif [[ $res =~ "ERROR" ]]; then
        printf "${RED}FAIL${NC} ${res}\n";
    else
        printf "UNDEF\n"
    fi
done