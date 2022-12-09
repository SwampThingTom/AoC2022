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

#
# Part 2: Find most scenic tree.
#

calc_scenic() {
    this_height=${tree_map[$2]:$1:1}

    # Look north.
    trees_seen_n=0
    for (( y=$(($2-1)); y>=0; y-- )); do
        trees_seen_n=$((trees_seen_n+1))
        other_height=${tree_map[$y]:$1:1}
        if [ $other_height -ge $this_height ]; then
            break
        fi
    done

    # Look south.
    trees_seen_s=0
    for (( y=$(($2+1)); y<=$((map_width-1)); y++ )); do
        trees_seen_s=$((trees_seen_s+1))
        other_height=${tree_map[$y]:$1:1}
        if [ $other_height -ge $this_height ]; then
            break
        fi
    done

    # Look east.
    trees_seen_e=0
    for (( x=$(($1+1)); x<=$((map_width-1)); x++ )); do
        trees_seen_e=$((trees_seen_e+1))
        other_height=${tree_map[$2]:$x:1}
        if [ $other_height -ge $this_height ]; then
            break
        fi
    done

    # Look west.
    trees_seen_w=0
    for (( x=$(($1-1)); x>=0; x-- )); do
        trees_seen_w=$((trees_seen_w+1))
        other_height=${tree_map[$2]:$x:1}
        if [ $other_height -ge $this_height ]; then
            break
        fi
    done

    echo $((trees_seen_n * trees_seen_e * trees_seen_s * trees_seen_w))
}

# Find the highest scenic value for all trees.
# Trees along the edges always have a scenic value of 0 so ignore them.
most_scenic=0
for (( y=1; y<$((map_height-1)); y++ )); do
    for (( x=1; x<$((map_width-1)); x++ )); do
        scenic=$(calc_scenic $x $y)
        if [ $scenic -gt $most_scenic ]; then
            most_scenic=$scenic
        fi
    done
done

echo "Part 2: $most_scenic"
