#!/bin/bash
for i in $(seq 2000 2017)
do
	echo $i
	grep -E "$i-\\d\\d-\\d\\d" Krall_speciation.csv > Krall_speciation-$i.csv
done
