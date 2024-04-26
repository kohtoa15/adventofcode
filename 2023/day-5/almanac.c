#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define ERR_INVALID_INPUT 42
#define ERR_MEM 69

// #define DEBUG
#ifdef DEBUG
#define DEBUG_PRINT(x) printf x
#else
#define DEBUG_PRINT(x) do {} while (0)
#endif

#define BUFLEN 64
int ReadMagicWord(char* magic, FILE* f) {
	int len = strlen(magic) + 1;
	if (len > BUFLEN)
		return -1;

	char buf[BUFLEN];
	if (fgets((char*)&buf, len, f) == NULL)
		return 1;

	DEBUG_PRINT(("fgets='%s'\n", buf));

	if (strncmp(buf, magic, len) != 0)
		return 2;

	return 0;
	};

struct num_vec_t {
	size_t cap;
	size_t len;
	size_t* list;
};

int ReadRangeMapArray(struct num_vec_t* list, FILE* f) {
	if (list->list == NULL) {
		list->cap = 1024;
		list->len = 0;
		void* ptr = malloc(list->cap * sizeof(size_t));
		if (ptr == NULL)
			return ERR_MEM;
		list->list = ptr;
	}

	while (1) {
		size_t open_cap = list->cap - list->len;
		if (open_cap <= 3) {
			list->cap += 1024;
			void* ptr = realloc(list->list, list->cap * sizeof(size_t));
			if (ptr == NULL)
				return ERR_MEM;
			list->list = ptr;
		}

		if (fscanf(f, "%lu %lu %lu\n",
			list->list + list->len,
			list->list + list->len + 1,
			list->list + list->len + 2
		) != 3) {
			break;
			}
		list->len += 3;
		}
	return 0;
	}

struct range_mapping_t {
	size_t dest_range_start;
	size_t src_range_start;
	size_t range_length;
	};

size_t TranslateMapping(size_t src, size_t* mappings, size_t start, size_t end) {
	for (size_t i = start; i < end; i+=3) {
		struct range_mapping_t* mapping = (struct range_mapping_t*)(mappings + i);
		size_t diff = src - mapping->src_range_start;
		if (diff < mapping->range_length) {
			return mapping->dest_range_start + diff;
			}
		}
	// no explicit mapping => same dest number
	return src;
	}

size_t SeedToLocation(size_t seed, size_t* list, size_t* end_markers) {
	size_t val = seed;
	for (size_t i = 0; i < 7; i++) {
		size_t start = i == 0 ? 0 : end_markers[i-1];
		val = TranslateMapping(val, list, start, end_markers[i]);
		}
	return val;
	}

struct tuple_t {
	size_t value;
	size_t offset;
	};

struct tuple_t TranslateMappingFastRange(size_t src, size_t range, size_t* mappings, size_t start, size_t end) {
	struct tuple_t tuple;
	size_t next_translation = -1;
	for (size_t i = start; i < end; i+=3) {
		struct range_mapping_t* mapping = (struct range_mapping_t*)(mappings + i);
		long diff = (long)src - (long)mapping->src_range_start;
		if (diff >= 0 && diff < (long)mapping->range_length) {
			long used_range = (long)mapping->range_length - diff;
			tuple.value = mapping->dest_range_start + diff;
			tuple.offset = used_range >= range ? 0 : used_range;
			return tuple;
			}
		if (diff < 0) {
			size_t away = (size_t)-diff;
			if (away < next_translation)
				next_translation = away;
			}
		}

	tuple.value = src;
	tuple.offset = next_translation >= range ? 0 : next_translation;
	return tuple;
	}

size_t SeedToLocationFastRange(size_t seed, size_t range, size_t* list, size_t* end_markers) {
	size_t inputs[1024];
	size_t num_in = 2;
	inputs[0] = seed;
	inputs[1] = range;

	for (size_t i = 0; i < 7; i++) {
		size_t start = i == 0 ? 0 : end_markers[i-1];

		for (size_t k = 0; k < num_in; k+=2) {
			size_t src = inputs[k];
			size_t range = inputs[k+1];

			printf("%lu..%lu, ", src, src + range - 1);
			struct tuple_t res = TranslateMappingFastRange(src, range, list, start, end_markers[i]);
			inputs[k] = res.value;
			inputs[k+1] = res.offset > 0 ? res.offset : range;
			if (res.offset > 0) {
				DEBUG_PRINT(("split range at offset %lu\n", res.offset));
				inputs[num_in++] = src + res.offset;
				inputs[num_in++] = range - res.offset;
				}
			}
		printf("\n");
		}

	size_t val = -1;
	for (size_t i = 0; i < num_in; i+=2) {
		if (inputs[i] < val)
			val = inputs[i];
		}
	return val;
	}

int ReadRanges(struct num_vec_t* mappings, size_t* end_markers, char** magic_words, size_t num_ranges, FILE* f) {
	for (size_t i = 0; i < num_ranges; i++) {
		if (ReadMagicWord(magic_words[i], f) != 0)
			return ERR_INVALID_INPUT;
		int ret = ReadRangeMapArray(mappings, f);
		if (ret != 0)
			return ret;
		end_markers[i] = mappings->len;
		}
	return 0;
}

char* MAGIC_WORDS[] = {
	"seed-to-soil map:\n",
	"soil-to-fertilizer map:\n",
	"fertilizer-to-water map:\n",
	"water-to-light map:\n",
	"light-to-temperature map:\n",
	"temperature-to-humidity map:\n",
	"humidity-to-location map:\n"
};

int main(void) {
	FILE* f = fopen("input.txt", "r");
	if (f == NULL) {
		fprintf(stderr, "cannot open file input.txt");
		return 1;
		}

	if (ReadMagicWord("seeds: ", f) != 0)
		return ERR_INVALID_INPUT;

	size_t seeds[64];
	size_t seed_index = 0;
	while (fscanf(f, "%lu ", &seeds[seed_index]) == 1 && seed_index < 64) {
		seed_index++;
		}

	struct num_vec_t mappings;
	mappings.list = NULL;

	size_t end_markers[7];
	int ret = ReadRanges(&mappings, end_markers, (char**)MAGIC_WORDS, 7, f);
	if (ret != 0) {
		if (mappings.list != NULL)
			free(mappings.list);
		return ret;
		}
	fclose(f);

	printf("Hello, I'm the almanac!\n");
	size_t low_locn = -1;
	for (size_t i = 0; i < seed_index; i++) {
		size_t seed = seeds[i];
		DEBUG_PRINT(("seed %lu: %lu\n", i, seed));
		size_t locn = SeedToLocation(seed, mappings.list, end_markers);
		printf("seed %lu => location %lu\n", seed, locn);
		if (locn < low_locn)
			low_locn = locn;
		}
	printf("Lowest Location Number = %lu\n", low_locn);

 	size_t low_locn_ranged = -1;
 	for (size_t i = 0; i < seed_index; i+=2) {
 		size_t seed = seeds[i];
 		size_t seed_range = seeds[i+1];
		printf("seed %lu, range %lu\n", seed, seed_range);
		size_t locn = SeedToLocationFastRange(seed, seed_range, mappings.list, end_markers);
		printf("seed %lu => location %lu\n", seed, locn);
 		if (locn < low_locn_ranged)
			low_locn_ranged = locn;
 		}
	printf("Lowest Location Number (from ranges) = %lu\n", low_locn_ranged);
	
	free(mappings.list);
	return 0;
	}
