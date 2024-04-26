#!/usr/bin/env bash
set -eu
set -o pipefail

rm -f /tmp/cards

JOKER_VALUE="41"
JOKER_MODE=""
if [[ "$1" == "--joker" ]]; then
	JOKER_VALUE="00"
	JOKER_MODE=1
	shift
fi

INPUT_FILE=$1
while read -r line; do
	rm -rf /tmp/symbols
	mkdir /tmp/symbols

	for symbol in $(echo "$line" | awk '{print $1}' | grep -o . | sort); do
		echo "$symbol" >> "/tmp/symbols/$symbol"
	done
	touch /tmp/symbols/dummy
	touch /tmp/symbols/J

	if [[ -z "$JOKER_MODE" ]]; then 
		TYPE=$(wc -l /tmp/symbols/* | grep -v total | awk '{print $1}' | sort -r | head -n 2 | tr -d '\n')
	else
		WITHOUT_JOKER=$(wc -l /tmp/symbols/* | grep -v -E "(total|J)" | awk '{print $1}' | sort -r | head -n 2 | tr -d '\n')
		JOKER_ADD="$(wc -l /tmp/symbols/J | awk '{print $1}')0"
		TYPE=$((WITHOUT_JOKER + JOKER_ADD))
	fi
	case $TYPE in
		50) value="6";;
		41) value="5";;
		32) value="4";;
		31) value="3";;
		22) value="2";;
		21) value="1";;
		11) value="0";;
	esac

	for char in $(echo "$line" | awk '{print $1}' | grep -o .); do
		case $char in
			A) value="${value}44";;
			K) value="${value}43";;
			Q) value="${value}42";;
			J) value="${value}${JOKER_VALUE}";;
			T) value="${value}40";;
			*) value="${value}$(echo "$char" | xxd -l 1 | awk '{print $2}')";;
		esac
	done

	echo "$value $line" >> /tmp/cards
done < "$INPUT_FILE"

count=1
total=0
for bid in $(sort /tmp/cards | awk '{print $3}'); do
	win=$((bid * count))
	total=$((total + win))
	count=$((count + 1))
done

echo "total winnings: $total"
