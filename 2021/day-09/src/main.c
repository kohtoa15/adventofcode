#include <stdio.h>
#include <stdlib.h>
#include <limits.h>
#include <string.h>
#include <stdbool.h>

typedef unsigned char u8;
typedef unsigned short u16;
typedef unsigned int u32;

// Set to 1 to get debug output
#define DEBUG 0
#define debug_trace(fmt, ...) \
  do { if (DEBUG) fprintf(stderr, "%s:%d:%s(): " fmt, __FILE__, __LINE__, __func__, __VA_ARGS__); } while(0);
#define debug_print(fmt, ...) \
  do { if (DEBUG) fprintf(stderr, fmt, __VA_ARGS__); } while(0);

/*
 * MAP TYPE DEFINTION AND FUNCTIONS
 */
struct Map {
  u8 rows;
  u8 cols;
  u8* data;
};

struct Map create_map(u8 cols, u8 rows) {
  struct Map m;
  m.cols = cols;
  m.rows = rows;
  m.data = (u8*) malloc(sizeof(u8) * cols * rows);
  return m;
}

int map_calculate_index(int cols, int x, int y) {
  return y * cols + x;
}

u8 map_get_value(struct Map m, u8 x, u8 y) {
  if (x < m.cols && y < m.rows) {
    int i = map_calculate_index((int) m.cols, (int) x, (int) y);
    return m.data[i];
  } else {
    return UCHAR_MAX - 1;
  }
}

u8 map_insert_value(struct Map m, u8 x, u8 y, u8 value) {
  if (x < m.cols && y < m.rows) {
    int i = map_calculate_index((int) m.cols, (int) x, (int) y);
    m.data[i] = value;
    return 1;
  } else {
    return 0;
  }
}

u8 map_insert_row(struct Map m, u8 y, u8* values) {
  for (u8 x = 0; x < m.cols; x++) {
    if (map_insert_value(m, x, y, values[x]) == 0) {
      return 0;
    }
  }
  return 1;
}


/*
 * LOW POINT FUNCTIONS
 */
u8 is_low_point(struct Map m, u8 x, u8 y) {
  u8 local_height = map_get_value(m, x, y);
  // Check point to the LEFT
  if (x > 0 && map_get_value(m, x - 1, y) <= local_height) {
    // Not a low point
    return 0;
  }
  // Check point to the RIGHT
  if (x < (m.cols - 1) && map_get_value(m, x + 1, y) <= local_height) {
    // Not a low point
    return 0;
  }
  // Check point ABOVE
  if (y > 0 && map_get_value(m, x, y - 1) <= local_height) {
    // Not a low point
    return 0;
  }
  // Check point BELOW
  if (y < (m.rows - 1) && map_get_value(m, x, y + 1) <= local_height) {
    // Not a low point
    return 0;
  }
  // Is a low point if all checks have passed
  return 1;
}

struct List {
  unsigned int length;
  u16* data;
};

u16 pack_xy(u8 x, u8 y) {
  // pack both dimensions into single double-width value
  return (u16) x + ((u16) y<<8);
}

u8 unpack_x(u16 pt) {
  return (u8) pt;
}

u8 unpack_y(u16 pt){
  return (u8) (pt>>8);
}

struct List get_low_points(struct Map m) {
  u32 length = 0;
  u32 alloc = 1;
  const u32 bufchunk = 16;
  // init with 1 value size as buffer
  u16* points = (u16*) malloc(sizeof(u16) * alloc);
  for (u8 y = 0; y < m.rows; y++) {
    for (u8 x = 0; x < m.cols; x++) {
      if (is_low_point(m, x, y) == 1) {
        debug_print("%d, %d is a low point.\n", x, y);
        points[length] = pack_xy(x, y);
        // Update length and allocated length vals
        length++;
        if (length == alloc) {  
          // reached limit, resize required
          alloc += bufchunk;
          points = (u16*) realloc(points, sizeof(u16) * alloc);
        }
      }
    }
  }
  struct List ret;
  ret.length = length;
  ret.data = points;
  return ret;
}

size_t read_line(char* s, size_t offset, u8 cols, u8* values) {
  u8 flag_err = 0;

  if (s == NULL) {
    printf("ERROR: Could not read from stream!\n");
    return 0;
  }

  // Parse next N chars as digits (N is the num of specified columns)
  for (u8 i = 0; i < cols; i++) {
    char c = s[offset + i];
    // Check if char is digit
    if (c >= 48 && c <= 57) {
      // Parse to int and save
      values[i] = (u8) (c - 48);
    } else {
      printf("ERROR: invalid character: 0x%x is not a digit (char %d in line)\n", c, i);
      return 0;
    }
  }
  // Check whether next char is newline
  if (s[offset + cols] != 0x0a) {
    printf("Invalid value: 0x%x (should be newline)\n", s[offset + cols]);
  }

  return offset + cols + 1;
}

char* read_file_to_string(char* filepath) {
  FILE* fp = fopen(filepath, "r");

  if (fp == NULL) {
    printf("Could not open file %s\n", filepath);
    return NULL;
  }

  fseek(fp, 0, SEEK_END);
  long int fsize = ftell(fp);
  fseek(fp, 0, SEEK_SET);

  char* str = (char*) malloc(fsize);
  size_t n = fread(str, 1, fsize, fp);
  fclose(fp);
  return str;
}

struct Map read_map_from_file(char* filepath, u8 rows, u8 cols) {
  u8 flag_err = 0;
  char* s = read_file_to_string(filepath);
  if (s == NULL) {
    rows = 0;
    flag_err = 1;
  }

  struct Map m = create_map(cols, rows);
  size_t offset = 0;
  for (u8 y = 0; y < rows; y++) {
    u8* vals = (u8*) malloc(sizeof(u8) * cols);
    offset = read_line(s, offset, cols, vals);
    debug_print("read line, offset at %lu\n", offset);
    if (vals == NULL) {
      // Invalid input, delete map data and invalidate return value
      flag_err = 1;
      break;
    }
    if (map_insert_row(m, y, vals) == 0) {
      printf("ERROR: Invalid row values! (y: %d)\n", y);
      flag_err = 1;
      break;
    }
    free(vals);
  }

  if (flag_err == 1) {
    free(m.data);
    m.cols = 0;
    m.rows = 0;
  }
  return m;
}

u8 flowing_to(struct Map m, u8 x, u8 y, int dx, int dy, u8 lp_id) {
  u8 height_from = map_get_value(m, x, y);
  u8 mx = (u8) (dx + ((int) x));
  u8 my = (u8) (dy + ((int) y));
  u8 height_to = map_get_value(m, mx, my);
  if (height_to == 9)
    return 0;
  if (height_from == height_to) {
    return lp_id;
  }
  // from the left
  if (dx == 1)
    return height_from < height_to ? lp_id : 0xff;
  // from the right
  if (dx == -1)
    return height_from < height_to ? lp_id : 0xff;
  // from above
  if (dy == 1)
    return height_from < height_to ? lp_id : 0xff;
  // from below
  if (dy == -1)
    return height_from < height_to ? lp_id : 0xff;
  return 0xff;
}

bool insert_flow(struct Map m, u8 x, u8 y, int dx, int dy, struct Map flows, u8 lp) {
  u8 val = flowing_to(m, x, y, dx, dy, lp);
  bool ret = val == lp;
  if (ret) {
    // enter into flowmap
    map_insert_value(flows, x + dx, y + dy, lp);
  }
  // return whether we should process from this point
  return ret;
}

void get_flow_from_low_point(struct Map m, struct Map flow, u8 x, u8 y) {
  int pt_alloc = m.cols * m.rows * 10;
  u16* points = (u16*) malloc(sizeof(u16) * pt_alloc);
  points[0] = pack_xy(x, y);

  u8 lp = map_get_value(flow, x, y);
  // Keep track of next points
  int end = 1;
  // go through current points in list and add outlining ones
  for (int i = 0; i < end && end < pt_alloc; i++) {
    u16 val = points[i];
    u8 x = unpack_x(val);
    u8 y = unpack_y(val);

    if (insert_flow(m, x, y, 0, 1, flow, lp)) { points[end++] = pack_xy(x, y + 1); }
    if (insert_flow(m, x, y, 0, -1, flow, lp)) { points[end++] = pack_xy(x, y - 1); }
    if (insert_flow(m, x, y, 1, 0, flow, lp)) { points[end++] = pack_xy(x + 1, y); }
    if (insert_flow(m, x, y, -1, 0, flow, lp)) { points[end++] = pack_xy(x - 1, y); }

    if (end >= pt_alloc + 4) {
      printf("Reached max points len - reuse: %d->%d [%d->%d]\n", i, end, 0, end - i);
      // buffer reuse => transfer i..end bytes to 0..(end - i)
      int new_end = end - i;
      for (int t = 0; t < new_end; t++) {
        points[t] = points[i + t]; 
      }
      i = 0;
      end = new_end;
    }
  }

  free(points);
}

struct Map get_flow(struct Map m, struct List lps) {
  struct Map flow = create_map(m.cols, m.rows);
  // Set initial low points
  for (int i = 0; i < lps.length; i++) {
    u16 val = lps.data[i];
    u8 x = unpack_x(val);
    u8 y = unpack_y(val);

    map_insert_value(flow, x, y, i + 1);
  }
  for (int i = 0; i < lps.length; i++) {
    u16 val = lps.data[i];
    u8 x = unpack_x(val);
    u8 y = unpack_y(val);
    get_flow_from_low_point(m, flow, x, y);
  }
  return flow;
}

int main(int argc, char** argv) {
  char* inputfile;
  u8 rows;
  u8 cols;
  if (argc == 4) {
    inputfile = (char*) malloc(sizeof(char) * (strlen(argv[1] + 1)));
    strcpy(inputfile, argv[1]);
    rows = (u8) atoi(argv[2]);
    cols = (u8) atoi(argv[3]);
  } else {
    perror("Invalid input! Required: inputfile rows cols\n");
    exit(-1);
  }
  struct Map m = read_map_from_file(inputfile, rows, cols);
  if (m.cols == 0 || m.rows == 0) {
    return -1;
  }

  // Print Map
  printf("THE MAP:\n");
  for (u8 y = 0; y < m.rows; y++) {
    for (u8 x = 0; x < m.cols; x++) {
      u8 h = map_get_value(m, x, y);
      printf("%d", h);
    }
    printf("\n");
  }

  struct List points = get_low_points(m);
  debug_print("found %d low points\n", points.length);
  // Get heights for low points
  int total_risk = 0;
  for (u8 i = 0; i < points.length; i++) {
    u16 pt = points.data[i];
    // unpack dimension values x and y
    u8 x = unpack_x(pt);
    u8 y = unpack_y(pt);
    u8 height = map_get_value(m, x, y);
    debug_print("Height at %d, %d: '%d'\n", x, y, height);
    total_risk += (int) height + 1;
  }
  printf("\nTotal risk is: %d\n", total_risk);

  struct Map flow = get_flow(m, points);

  if (DEBUG) {
    for (int y = 0; y < flow.rows; y++) {
      for (int x = 0; x < flow.cols; x++) {
        u8 basin = map_get_value(flow, x, y);
        char fc = basin == 0 ? 0x20 : (char) (basin % 26) + 64;
        printf("%c", fc);
      }
      printf("\n");
    }
  }

  int* sizes = (int*) malloc(sizeof(int) * points.length);
  for (int i = 0; i < points.length; i++) { sizes[i] = 0; }

  for (int y = 0; y < m.rows; y++) {
    for (int x = 0; x < m.cols; x++) {
      u8 basin = map_get_value(flow, x, y);
      // count size for each low point
      if (basin > points.length) { printf("Error: invalid basin %d\n", basin); continue; }
      sizes[basin - 1]++;
    }
  }
  
  int max_basins[3] = {0, 0, 0};
  for (int i = 0; i < points.length; i++) {
    // use max_basins as FIFO queue of largest sizes
    for (int j = 0; j < 3; j++) {
      if (sizes[i] > max_basins[j]) {
        for (int m = 2; m > j ; m--) {
          max_basins[m] = max_basins[m - 1];
        }
        max_basins[j] = sizes[i];
        break;
      }
    }
  }

  int total_sum = 1;
  for (int m = 0; m < 3; m++) {
    debug_print("largest basins: %d\n", max_basins[m]);
    total_sum = total_sum * max_basins[m];
  }
  printf("\ntotal product of largest basins: %d\n", total_sum);

  // Free memory
  free(m.data);
  free(points.data);
  return 0;
}
