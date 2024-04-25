#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define ERR_INVALID_INPUT 42
#define ERR_MEM 69

#define DEBUG
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

	// DEBUG
	for (size_t i = 0; i < seed_index; i++) {
		DEBUG_PRINT(("seed %lu: %lu\n", i, seeds[i]));
		}

	struct num_vec_t mappings;
	mappings.list = NULL;
	int ret;

	if (ReadMagicWord("seed-to-soil map:\n", f) != 0)
		return ERR_INVALID_INPUT;
	if ((ret = ReadRangeMapArray(&mappings, f)) != 0) {
		return ret;
		}
	size_t seed_to_soil_end = mappings.len;
	DEBUG_PRINT(("end of seed-to-soil: %lu\n", seed_to_soil_end));

	if (ReadMagicWord("soil-to-fertilizer map:\n", f) != 0)
		return ERR_INVALID_INPUT;
	if ((ret = ReadRangeMapArray(&mappings, f)) != 0) {
		return ret;
		}
	size_t soil_to_fert_end = mappings.len;
	DEBUG_PRINT(("end of soil-to-fertilizer: %lu\n", soil_to_fert_end));

	if (ReadMagicWord("fertilizer-to-water map:\n", f) != 0)
		return ERR_INVALID_INPUT;
	if ((ret = ReadRangeMapArray(&mappings, f)) != 0) {
		return ret;
		}
	size_t fertilizer_to_water_end = mappings.len;
	DEBUG_PRINT(("end of fertilizer-to-water: %lu\n", fertilizer_to_water_end));

	if (ReadMagicWord("water-to-light map:\n", f) != 0)
		return ERR_INVALID_INPUT;
	if ((ret = ReadRangeMapArray(&mappings, f)) != 0) {
		return ret;
		}
	size_t water_to_light_end = mappings.len;
	DEBUG_PRINT(("end of water-to-light: %lu\n", water_to_light_end));

	if (ReadMagicWord("light-to-temperature map:\n", f) != 0)
		return ERR_INVALID_INPUT;
	if ((ret = ReadRangeMapArray(&mappings, f)) != 0) {
		return ret;
		}
	size_t light_to_temperature_end = mappings.len;
	DEBUG_PRINT(("end of light-to-temperature: %lu\n", light_to_temperature_end));

	if (ReadMagicWord("temperature-to-humidity map:\n", f) != 0)
		return ERR_INVALID_INPUT;
	if ((ret = ReadRangeMapArray(&mappings, f)) != 0) {
		return ret;
		}
	size_t temperature_to_humidity_end = mappings.len;
	DEBUG_PRINT(("end of temperature-to-humidity: %lu\n", temperature_to_humidity_end));

	if (ReadMagicWord("humidity-to-location map:\n", f) != 0)
		return ERR_INVALID_INPUT;
	if ((ret = ReadRangeMapArray(&mappings, f)) != 0) {
		return ret;
		}
	size_t humidity_to_location_end = mappings.len;
	DEBUG_PRINT(("end of humidity-to-location: %lu\n", humidity_to_location_end));
	fclose(f);

	printf("Hello, I'm the almanac!\n");
	size_t low_locn = -1;
	for (size_t i = 0; i < seed_index; i++) {
		size_t seed = seeds[i];
		DEBUG_PRINT(("seed %lu: %lu\n", i, seed));
		// seed to soil
		size_t soil = TranslateMapping(seed, mappings.list, 0, seed_to_soil_end);
		DEBUG_PRINT(("\tseed %lu => soil %lu\n", seed, soil));
		// soil to fertilizer
		size_t fert = TranslateMapping(soil, mappings.list, seed_to_soil_end, soil_to_fert_end);
		DEBUG_PRINT(("\tsoil %lu => fertilizer %lu\n", soil, fert));
		// fertilizer to water
		size_t water = TranslateMapping(fert, mappings.list, soil_to_fert_end, fertilizer_to_water_end);
		DEBUG_PRINT(("\tfertilizer %lu => water %lu\n", fert, water));
		// water to light
		size_t light = TranslateMapping(water, mappings.list, fertilizer_to_water_end, water_to_light_end);
		DEBUG_PRINT(("\twater %lu => light %lu\n", water, light));
		// light to temperature
		size_t temp = TranslateMapping(light, mappings.list, water_to_light_end, light_to_temperature_end);
		DEBUG_PRINT(("\tlight %lu => temperature %lu\n", light, temp));
		// temperature to humidity
		size_t humi = TranslateMapping(temp, mappings.list, light_to_temperature_end, temperature_to_humidity_end);
		DEBUG_PRINT(("\ttemperature %lu => humidity %lu\n", temp, humi));
		// humidity to location
		size_t locn = TranslateMapping(humi, mappings.list, temperature_to_humidity_end, humidity_to_location_end);
		DEBUG_PRINT(("\thumidity %lu => location %lu\n", humi, locn));
		printf("seed %lu => location %lu\n", seed, locn);
		if (locn < low_locn)
			low_locn = locn;
		}
	printf("Lowest Location Number = %lu\n", low_locn);

	return 0;
	}
