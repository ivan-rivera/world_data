#!/bin/bash

# download WDI CSV data
doc_path="/Users/Ivan/Documents/Projects/my_shiny_apps/wdi/data/"
dl_path="http://databank.worldbank.org/data/download/WDI_csv.zip"
wget -P $doc_path/ $dl_path

# unzip
DATE=`date +%Y_%m_%d`
unzip $doc_path/WDI_csv.zip -d $doc_path/wdi_$DATE

# clean up
if [ -f $doc_path/WDI_csv.zip ];
	then rm $doc_path/WDI_csv.zip
fi

# the below part is done in R
#
# save latest file into a placeholder
# if [ -f $doc_path/wdi_data.csv ];
#   	then rm $doc_path/wdi_data.csv
# fi
# 
# pull from WDI_data.csv
# cp $doc_path/wdi_$DATE/WDI_Data.csv $doc_path/wdi_data.csv