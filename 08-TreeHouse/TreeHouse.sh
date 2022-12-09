#!/opt/homebrew/bin/bash

# Treetop Tree House
# https://adventofcode.com/2022/day/8

declare -A visible_trees=()

add_tree() {
    visible_trees["$1,$2"]=1
}

# Read the input.
map_height=0
while IFS= read -r line; do
    tree_map[$map_height]=$line
    map_height=$((map_height+1))
done < input.txt
map_width=${#tree_map[0]}

#
# Part 1: Find visible trees.
#

# Add edges to trees seen.
for (( x=0; x<$map_width; x++ )); do
    add_tree $x 0
    add_tree $x $((map_height-1))
done
for (( y=0; y<$map_height; y++ )); do
    add_tree 0 $y
    add_tree $((map_width-1)) $y
done

# Find trees visible from west.
for (( y=1; y<$((map_height-1)); y++ )); do
    tallest=${tree_map[$y]:0:1}
    for (( x=1; x<$((map_width-1)); x++ )); do
        height=${tree_map[$y]:$x:1}
        if [ $height -gt $tallest ]; then
            add_tree $x $y
            tallest=$height
        fi
    done
done

# Find trees visible from east.
for (( y=1; y<$((map_height-1)); y++ )); do
    tallest=${tree_map[$y]:$((map_width-1)):1}
    for (( x=$((map_width-1)); x>0; x-- )); do
        height=${tree_map[$y]:$x:1}
        if [ $height -gt $tallest ]; then
            add_tree $x $y
            tallest=$height
        fi
    done
done

# Find trees visible from north.
for (( x=1; x<$((map_width-1)); x++ )); do
    tallest=${tree_map[0]:$x:1}
    for (( y=1; y<$((map_height-1)); y++ )); do
        height=${tree_map[$y]:$x:1}
        if [ $height -gt $tallest ]; then
            add_tree $x $y
            tallest=$height
        fi
    done
done

# Find trees visible from south.
for (( x=1; x<$((map_width-1)); x++ )); do
    tallest=${tree_map[$((map_height-1))]:$x:1}
    for (( y=$((map_height-1)); y>0; y-- )); do
        height=${tree_map[$y]:$x:1}
        if [ $height -gt $tallest ]; then
            add_tree $x $y
            tallest=$height
        fi
    done
done

echo "Part 1: ${#visible_trees[@]}"
