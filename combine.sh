#!/bin/bash
F=PL-mortality.csv
echo 'year;sex;week;date;age;geo;value > $F
cat P_zgony__*csv | grep -v 'year;sex;week' >> $F
##
## Tylko rok 2015 i lepiej
## z drobnym przekodowaniem
perl  extract.pl > PL-mortality-2015.csv
