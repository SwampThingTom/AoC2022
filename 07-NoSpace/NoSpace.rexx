#!/usr/bin/env regina

/* No Space Left On Device
   https://adventofcode.com/2022/day/7
*/

/* Initialize root directory with a size of 0. */
directories.0 = 1
directories.1 = ""
sizes.0 = 1
sizes.1 = 0
cwd = ""

call parse_input

say "Part 1:" sum_of_small_directories()
say "Part 2:" size_of_directory_to_delete()
exit

parse_input:
    do while lines(input.txt) > 0  
        line = linein(input.txt)
        if word(line, 1) = "$" then
            if word(line, 2) = "cd" then call change_directory word(line, 3)
            else nop /* ignore ls */
        else if word(line, 1) = "dir" then call add_directory word(line, 2)
        else call add_file_size word(line, 1) 
    end
    return

sum_of_small_directories:
    sum = 0
    do i = 1 to directories.0
        if sizes.i <= 100000 then sum = sum + sizes.i
    end
    return sum

size_of_directory_to_delete:
    unused_space = 70000000 - sizes.1
    need_to_delete = 30000000 - unused_space
    do i = 1 to directories.0
        if sizes.i >= need_to_delete & sizes.i < smallest then smallest = sizes.i
    end
    return smallest

add_directory:
    parse arg dir
    next = directories.0 + 1
    directories.next = cwd"/"dir
    directories.0 = next
    sizes.next = 0
    sizes.0 = next
    return

add_file_size:
    parse arg size
    call add_file_size_to_dir size, cwd
    return

add_file_size_to_dir:
    parse arg size, dir
    index = find_directory(dir)
    sizes.index = sizes.index + size
    if dir <> "" then
        call add_file_size_to_dir size, parent_directory(dir)
    return

find_directory:
    parse arg dir
    do i = 1 to directories.0
        if directories.i = dir then return i
    end
    return -1

change_directory:
    parse arg dir
    if dir = ".." then cwd = parent_directory(cwd)   
    else if dir = "/" then cwd = ""
    else cwd = cwd"/"dir
    return

parent_directory:
    parse arg dir
    last_path_pos = lastpos("/", dir)
    parent = delstr(cwd, last_path_pos)
    return parent

canonical_directory:
    parse arg dir
    if dir = "" then return "/"
    return dir
