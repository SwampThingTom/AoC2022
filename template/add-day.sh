#!/usr/bin/env bash

# Sets up the directory for a new day of Advent of Code 2022
#
# Usage:
# add-day <day-num> <language>

day=$1
language=$2

target_dir="$day-TBD"

day_num=`expr $day + 0`
mkdir $target_dir
cp template/README.md $target_dir
sed -i '' "s/<day>/$day_num/g" $target_dir/README.md
sed -i '' "s/<language>/$language/g" $target_dir/README.md
touch $target_dir/input.txt
