#!/usr/bin/env bash

set -e

day=$1

if [ -z "$day" ]; then
  echo "Usage: $0 <day>"
  exit 1
fi

dayStr="Day$(printf "%02d" $day)"

mkdir -p "data/$dayStr"
# curl -s "https://adventofcode.com/2021/day/$day/input" > "data/$dayStr/input.txt"

cat >> package.yaml << EOF

  $dayStr:
    main:                Main.hs
    source-dirs:         src/$dayStr
    dependencies:
      - AdventOfCode2021
EOF

cp -r "template/src" "src/$dayStr"