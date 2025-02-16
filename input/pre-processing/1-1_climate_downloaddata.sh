###download climate data####
#!/bin/bash

out_dir="./../dat/input/climate/"
mkdir -p "$out_dir"

years="1981_1990 1991_2000 2001_2010 2011_2019 1961_1970 1971_1980 1901_1910 1911_1920 1921_1930 1931_1940 1941_1950 1951_1960"
vars="sfcwind ps rsds huss tasmax tasmin tas pr" 

# inline list of URLs
for year in $years; do
      for var in $vars; do
      printf "%s\n" \
            "https://files.isimip.org/ISIMIP3a/InputData/climate/atmosphere/counterclim/global/daily/historical/GSWP3-W5E5/gswp3-w5e5_counterclim_${var}_global_daily_${year}.nc" | \
      

#download data use wget
 wget -P "$out_dir" --no-check-certificate -i -
      done
done