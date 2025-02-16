### make climate data into yearly data ###
#!/bin/sh
directory="./../dat/input/climate"
out_dir="./../dat/input/climate"

mkdir -p "$out_dir"

var="(tasmax|tasmin|tas|pr|rsds|huss|sfcwind|ps)_" 

files=$(ls "$directory" | grep -E "$var" | grep -E "daily" | grep -v "prsn")
 #filter out the prsn because it is mistakenly read in pr #echo "$files"
#files = "cnrm-esm2-1_r1i1p1f2_w5e5_ssp585_rsds_global_daily_2015_2100_05g.nc"
for file in $files; do
    echo "Processing file: $file"
    file_var=$(echo "$file" | grep -oP "${var}")
    
    echo "splityear" 
    cdo -s -O splityear "${directory}/${file}" "${out_dir}/${file_var}_ISIMIP_"
done
