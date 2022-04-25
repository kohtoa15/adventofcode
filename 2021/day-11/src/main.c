#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#include "fsio.h"

// Set to 1 to get debug output
#define DEBUG 0
#define debug_trace(fmt, ...) \
  do { if (DEBUG) fprintf(stderr, "%s:%d:%s(): " fmt, __FILE__, __LINE__, __func__, __VA_ARGS__); } while(0);
#define debug_print(fmt, ...) \
  do { if (DEBUG) fprintf(stderr, fmt, __VA_ARGS__); } while(0);

int* read_ten_nums(char* buf, int start, int end) {
  // check if first char is newline
  if (buf[start] == 0xa) {
    // increase to account for unused char
    start++;
  }
  // check if last char is newline
  if (buf[end] == 0xa || buf[end] == 0x0) {
    // decrease to account for unused char
    end--;
  }
  int num_chars = end - start + 1;
  if (num_chars != 10) {
    // invalid input
    printf("ERROR: Expected 10 chars to convert as input, got %d\n", num_chars);
    return NULL;
  }
  int* nums = (int*) calloc(num_chars, sizeof(int));
  for (int i = 0; i < num_chars; i++) {
    int num = buf[start + i] - '0';
    if (num < 0 || num > 9) {
      printf("ERROR: invalid input number %d\n", num);
      // invalid number
      free(nums);
      return NULL;
    }
    nums[i] = num;
  }
  return nums;
}

int octopus_round(int** octopus) {
  int flashes = 0;
  // create delta mask for octopus
  // this stores the energy increase for each round
  int mask[10][10];
  // flatrate increase of 1
  for (int y = 0; y < 10; y++) { for (int x = 0; x < 10; x++) { mask[y][x] = 1; } }
  // check all octopus for as long as new flashes occur
  bool flashing = false;
  do {
    // reset flash
    flashing = false;
    for (int y = 0; y < 10; y++) {
      for (int x = 0; x < 10; x++) {
        // check if already flashed this round
        if (mask[y][x] == -1) { continue; }
        // check if mask increase + current num makes a flash
        if (octopus[y][x] + mask[y][x] >= 10) {
          // *FLASH*
          flashes++;
          flashing = true;
          mask[y][x] = -1;
          // increase surrounding energy levels
          for (int dy = -1; dy <= 1; dy++) {
            for (int dx = -1; dx <= 1; dx++) {
              // skip own position
              if (dy == 0 && dx == 0) { continue; }
              int mx = x + dx;
              int my = y + dy;
              // check if we crossed the edge
              if (mx < 0 || mx > 9 || my < 0 || my > 9) { continue; }
              // increase energy level if not already flashed
              if (mask[my][mx] != -1) {
                mask[my][mx]++;
              }
            }
          }
        }
      }
    }
  } while (flashing);
  // add delta mask to octopus energy levels
  // for flashed octopus, reset to 0
  for (int y = 0; y < 10; y++) {
    for (int x = 0; x < 10; x++) {
      if (mask[y][x] == -1) {
        // reset flashed
        octopus[y][x] = 0;
      } else {
        // increase energy level
        octopus[y][x] += mask[y][x];
      }
    }
  }
  // report back num of flashes
  return flashes;
}

int main(int argc, char** argv) {
  if (argc != 3) {
    perror("invalid args. Requires input: filename rounds");
    return -1;
  }
  // get filename parameter
  char* filename = (char*) malloc(sizeof(char) * (strlen(argv[1]) + 1));
  strcpy(filename, argv[1]);
  // get rounds parameter
  int rounds = atoi(argv[2]);
  // read file content
  char* s = read_file_to_string(filename);
  unsigned long len = strlen(s);
  debug_print("inputfile %s has length of %lu.\n", filename, len);
  // read 10x10 dumbo octopus
  int** dumbos = (int**) calloc(10, sizeof(int*));
  int offset = 0;
  for (int i = 0; i < 10; i++) {
    int next_offset = next_line(s, len, offset);
    // read nums in the span offset->next_offset
    int* row = read_ten_nums(s, offset, next_offset);
    if (row == NULL) {
      printf("invalid input, aborting program.\n");
      // free mem before abort
      for (int x = 0; x < i; x++) {
        free(dumbos[x]);
      }
      free(dumbos);
      return -1;
    }
    dumbos[i] = row;
    // advance offset cursor
    offset = next_offset;
  }
  // debug: print octopus nums
  if (DEBUG) {
    printf("THE OCTOPUS:\n");
    for (int y = 0; y < 10; y++) {
      for (int x = 0; x < 10; x++) {
        printf("%d", dumbos[y][x]);
      }
      printf("\n");
    }
  }

  int first_step_all_flash = -1;
  int total_flashes = 0;
  for (int i = 0; i < rounds; i++) {
    int flashes = octopus_round(dumbos);
    if (DEBUG) {
      printf("round %d:\n", i + 1);
      for (int y = 0; y < 10; y++) {
        for (int x = 0; x < 10; x++) {
          printf("%d", dumbos[y][x]);
        }
        printf("\n");
      }
    }
    total_flashes += flashes;
    // check if all flashes
    if (first_step_all_flash < 0 && flashes == 100) {
      first_step_all_flash = i + 1;
    }
  }
  printf("\n1. After %d rounds, %d octopus flashes could be observed.\n", rounds, total_flashes);

  // if we haven't found the first synchronized flash yet, we will continue to look for it
  for (int i = rounds; first_step_all_flash < 0; i++) {
    int flashes = octopus_round(dumbos);
    if (DEBUG) {
      printf("round %d:\n", i + 1);
      for (int y = 0; y < 10; y++) {
        for (int x = 0; x < 10; x++) {
          printf("%d", dumbos[y][x]);
        }
        printf("\n");
      }
    }
    // check if all flashes
    if (first_step_all_flash < 0 && flashes == 100) {
      first_step_all_flash = i + 1;
    }
  }
  printf("\n2. The first step during which all octopuses flashed was %d\n", first_step_all_flash);

  return 0;
}
