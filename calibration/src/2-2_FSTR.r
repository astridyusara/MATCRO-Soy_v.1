################################################################
# calculate  Partition to shielded reserve (starch) in stem FTSR#
################################################################

rm(list = ls())
library(stringr)
library(dplyr)

workdir <- "./.."
outdir <- paste0(workdir, "/prc/")
dir.create(outdir, recursive = TRUE)

CF.STEM <- 0.494 # CARBON FRACTION #See Penning de Vries et al., 1989
CF.STCH <- 0.444 # Carbon fraction in starch
SEEDFILL <- read.table(paste0(workdir, "/prc/seedfillGDD.txt"), header = FALSE)
SEEDFILL <- SEEDFILL[1, 1] # 0.65
FLW <- read.table(paste0(workdir, "/prc/headGDD.txt"), header = FALSE)
FLW <- FLW[1, 1] # 0.46 flowering time
HEAD <- (SEEDFILL + FLW) / 2
FSTRs <<- numeric()

# Read Biomass and Phenology data
biomass <- read.table(paste0(outdir, "Biomass_leaffill_DVS.csv"), header = TRUE, sep = ",")
pheno <- read.table("./prc/Phenology.csv", header = TRUE, sep = ",")
biomass <- biomass %>%
  arrange(EXP, DVS, DOY, STM)

cols <- c("EXP", "Year", "DOY", "DVS", "STM", "LEF", "LAI", "Yield", "POD", "CO2ave", "MTR")
# Calculate FSTR values
EXPs <- unique(biomass$EXP)
for (exp in EXPs) {
  print(exp)
  BIODVS <- biomass[as.character(biomass$EXP) == as.character(exp), ]
  dt <- BIODVS[cols]
  dt <- dt[!is.na(dt$STM), ]
  if (length(dt$STM) == 0) {
    # Skip to the next iteration if length is zero
    cat("No data for STM in:", exp, "\n")
    next
  }

  maxSTEM <<- 0
  minSTEM <<- 10000000000.0
  totSTEM <<- numeric()
  for (j in 1:nrow(dt)) {
    if (!is.na(dt$STM[j])) {
      if (maxSTEM < dt$STM[j]) {
        totSTEM <<- c(totSTEM, dt$STM[j])
        maxSTEM <<- dt$STM[j]
      } else {
        totSTEM <<- c(totSTEM, maxSTEM)
      }

      if (maxSTEM > dt$STM[j] & minSTEM > dt$STM[j]) {
        minSTEM <<- dt$STM[j]
      }
    }
  }

  FSTR <- (maxSTEM - minSTEM) / (maxSTEM - minSTEM + minSTEM * CF.STEM / CF.STCH)

  if (maxSTEM < minSTEM) {
    FSTR <- 0
  }
  if (maxSTEM == 0 && minSTEM == 0) {
    FSTR <- 0
  }
  print(c(FSTR, maxSTEM, minSTEM))
  ## ##########################################
  FSTRs <<- c(FSTRs, FSTR)
}

print(paste(mean(FSTRs), sd(FSTRs) / mean(FSTRs)))

mean_without_zero <- mean(FSTRs[FSTRs != 0 & FSTRs != 1])
cat("mean without zero:", mean_without_zero)

# write.table(mean(FSTRs),paste0(outdir,"/FSTR.txt"),append=F,quote=F,row.names=F,col.names=F)
write.table(mean(mean_without_zero), paste0(outdir, "/FSTR.txt"), append = F, quote = F, row.names = F, col.names = F)
