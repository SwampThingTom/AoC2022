// Camp Cleanup
// https://adventofcode.com/2022/day/4
//
// Part 1: Count the number of assignments where one section fully contains the other.
// Part 2: Count the number of assignments where one section overlaps the other.

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define DBG_PRINT(s1, s2) printf("%d-%d,%d-%d\n", s1.start, s1.end, s2.start, s2.end)

typedef struct _SectionAssignment {
    int start;
    int end;
} SectionAssignment;

typedef struct _AssignmentPair {
    SectionAssignment elf1;
    SectionAssignment elf2;
} AssignmentPair;

typedef struct _AssignmentList {
    AssignmentPair *assignments;
    int size;
} AssignmentList;

typedef bool (*AssignmentPredicate)(AssignmentPair);

AssignmentList getAssignments() {
    int capacity = 100;
    AssignmentList list = { 
        .assignments = malloc(capacity * sizeof(AssignmentPair)),
        .size = 0
    };

    FILE *file = fopen("input.txt", "r");
    if (file == NULL) {
        fprintf(stderr, "ERROR: Unable to open 'input.txt'.");
        exit(EXIT_FAILURE);
    }

    char *input = NULL;
    size_t len = 0;
    while (getline(&input, &len, file) > 0) {
        if (list.size >= capacity) {
            capacity *= 2;
            list.assignments = realloc(list.assignments, capacity * sizeof(AssignmentPair));
        }

        int result = sscanf(input, "%d-%d,%d-%d", 
            &(list.assignments[list.size].elf1.start), &(list.assignments[list.size].elf1.end),
            &(list.assignments[list.size].elf2.start), &(list.assignments[list.size].elf2.end));
        
        if (result < 0) {
            fprintf(stderr, "ERROR: Unable to parse line '%s'.", input);
            exit(EXIT_FAILURE);
        }
        
        list.size++;
    }

    free(input);
    fclose(file);

    return list;
}

bool containsSection(SectionAssignment section1, SectionAssignment section2) {
    return (section1.start <= section2.start) && (section1.end >= section2.end);
}

bool contains(AssignmentPair assignment) {
    return (containsSection(assignment.elf1, assignment.elf2) || 
            containsSection(assignment.elf2, assignment.elf1));
}

bool overlapsSection(SectionAssignment section1, SectionAssignment section2) {
    return ((section1.start <= section2.start) && (section1.end >= section2.start) ||
            (section1.start <= section2.end) && (section1.end >= section2.end));
}

bool overlaps(AssignmentPair assignment) {
    return (overlapsSection(assignment.elf1, assignment.elf2) || 
            overlapsSection(assignment.elf2, assignment.elf1));
}

int countMatches(AssignmentList list, AssignmentPredicate matches) {
    int count = 0;
    for (int i = 0; i < list.size; i++) {
        if (matches(list.assignments[i])) {
            count++;
        }
    }
    return count;
}

int main()
{
    AssignmentList list = getAssignments();

    int result = countMatches(list, contains);
    printf("Part 1: %d\n", result);

    result = countMatches(list, overlaps);
    printf("Part 2: %d\n", result);

    return EXIT_SUCCESS;
}
