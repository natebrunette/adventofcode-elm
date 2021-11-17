#!/bin/bash

fail() {
  echo "$1"
  exit 1
}

read_env() {
  export $(cat .env | xargs)
}

set_day() {
  if [ "$1" == "" ]; then
    fail "Must include day parameter"
  fi

  AOC_DAY=$1
  AOC_DAY_FORMATTED=$(printf "%02d" "$AOC_DAY")
}

build_input_dir() {
  mkdir -p "input/$AOC_YEAR"
}

make_source_file() {
  mkdir -p "src/AOC/Year$AOC_YEAR"
  template="templates/$AOC_LANG/source.txt"
  dest="src/AOC/Year$AOC_YEAR/Day$AOC_DAY_FORMATTED.elm"
  if [ ! -f "$dest" ]; then
    awk -v YEAR="$AOC_YEAR" -v DAY="$AOC_DAY_FORMATTED" '{sub(/###YEAR/, YEAR); sub(/###DAY/, DAY); print;}' "$template" > "$dest"
  else
    echo "Not copying '$dest' because file already exists"
  fi
}

make_test_file() {
  mkdir -p "tests/Test/AOC/Year$AOC_YEAR"
  input=$(cat "input/$AOC_YEAR/$AOC_DAY_FORMATTED.txt")
  template="templates/$AOC_LANG/test.txt"
  dest="tests/Test/AOC/Year$AOC_YEAR/Day${AOC_DAY_FORMATTED}Test.elm"
  if [ ! -f "$dest" ]; then
    awk -v YEAR="$AOC_YEAR" -v DAY="$AOC_DAY_FORMATTED" -v INPUT="${input//$'\n'/\\n}" '{sub(/###YEAR/, YEAR); sub(/###DAY/, DAY); sub(/###INPUT/, INPUT); print;}' "$template" > "$dest"
  else
    echo "Not copying '$dest' because file already exists"
  fi
}

build_input_url() {
  echo "https://adventofcode.com/$AOC_YEAR/day/$AOC_DAY/input"
}

fetch_input() {
  file="input/$AOC_YEAR/$AOC_DAY_FORMATTED.txt"
  if [ -f "$file" ]; then
    echo "Day $AOC_DAY input file already exists"
  else
    build_input_dir
    url=$(build_input_url)
    curl "$url" --cookie "$AOC_COOKIE" >"$file"
  fi
}

read_env
set_day "$1"
fetch_input
make_source_file
make_test_file
