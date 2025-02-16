# this code making partition calculation from the biomass#

# the root partitioning all is NA, because not enough data to see the partitioning during the developmental stage, US1 which has ROOT data#
rm(list = ls())
library(stringr)

workdir <- "./../prc"
outdir <- workdir
dir.create(outdir, recursive = TRUE)

# Read Biomass and Phenology data
biomass <- read.table(paste0(outdir, "/Biomass_leaffill_DVS.csv"), header = TRUE, sep = ",")
pheno <- read.table(paste0(workdir, "/Phenology.csv"), header = TRUE, sep = ",")
FSTR.DF <- read.table(paste0(outdir, "/FSTR.txt"), header = FALSE)
FSTR <- FSTR.DF[1, 1]

CF.LEAF <- 0.459 ## CARBON FRACTION ##
CF.STEM <- 0.494
CF.ROOT <- 0.467
CF.PNCL <- 0.527
CF.STCH <- 0.444 # Carbon fraction in starch

C2CHO <- 30 / 12

GL.LEAF <- 1.687 ## Required Glucose
GL.STEM <- 1.603
GL.ROOT <- 1.534
GL.PNCL <- 2.161

EXPs <- unique(biomass$EXP)
cols <- c("EXP", "SITE", "VRT", "Year", "DOY", "AGB", "LAI", "Yield", "STM", "LEF", "POD", "ROT", "DVS", "SOW", "EMR", "FLW", "SEEDFILL", "MTR", "IRR", "CO2ave")

list_PAT <- list()
list_dt <- list()
for (exp in EXPs) {
  # exp = "US1_04_PIO_149_0_0_62.4_AMBIENT" #has ROT
  print(exp)
  dt <- biomass[as.character(biomass$EXP) == as.character(exp), ]
  dt <- dt[cols]
  dt$PAT.LEAF0 <- dt$LEF / dt$AGB
  dt <- dt[!is.na(dt$STM), ]
  dt <- dt[order(dt$DVS, decreasing = FALSE), ] # make sure DVS in order
  dt$ROT[dt$ROT == 0] <- NA

  DVS <- data.frame("DOY" = dt$DOY, "DVS" = dt$DVS)

  # find how long the period
  DIF.DOY <- numeric()
  for (j in 1:(length(DVS$DOY) - 1)) {
    DIF.DOY <- c(DIF.DOY, DVS$DOY[j + 1] - DVS$DOY[j])
  }

  ## #################
  ## CALC total LEAF #
  ## #################

  # to remove the dead leaves data (decreasing value) and get the latest grown leaf data
  maxLEAF <- 0.0
  totLEAF <- numeric()
  if (length(dt$LEF) > 0) {
    for (j in 1:nrow(dt)) {
      if (!is.na(dt$LEF[j])) {
        if (maxLEAF < dt$LEF[j]) {
          totLEAF <- c(totLEAF, dt$LEF[j])
          maxLEAF <- dt$LEF[j]
        } else {
          totLEAF <- c(totLEAF, maxLEAF)
        }
      } else {
        totLEAF <- c(totLEAF, NA)
      }
    }
  }
  ## modify data for leaf to consider decreased leaves/ leave senescence
  maxLEAF <- 0.0
  totLEAF.2 <- numeric()
  for (j in 1:nrow(dt)) {
    k <- nrow(dt) - (j - 1)

    if (k > 0 && k <= nrow(dt) && !is.na(dt$LEF[k])) {
      if (maxLEAF < dt$LEF[k]) {
        totLEAF.2 <- c(totLEAF.2, dt$LEF[k])
        maxLEAF <- dt$LEF[k]
      } else {
        totLEAF.2 <- c(totLEAF.2, maxLEAF)
      }
    } else {
      totLEAF.2 <- c(totLEAF.2, NA)
    }
  }
  totLEAF.2 <- rev(totLEAF.2)

  # modify data to not consider stem senescence
  # taking the last row value as the last stem value, so there is no 0 value in observation data
  maxSTEM <<- 0
  minSTEM <<- 10000000000.0
  totSTEM <<- numeric()
  if (length(dt$STM) > 0) {
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
  }
  ## #################
  ## CALC PARTIONING #
  ## #################

  ## SHOOT # (Glucose)
  STEM <- totSTEM * CF.STEM * C2CHO / ((1.0 - FSTR) + FSTR * CF.STEM / CF.STCH) # including WIR
  LEAF <- totLEAF * CF.LEAF * C2CHO / (1.0 + 0.1) # WAR = WLF * 0.1
  PNCL <- dt$POD * CF.PNCL * C2CHO
  ROOT <- dt$ROT * CF.ROOT * C2CHO
  SHOT <- STEM + LEAF + PNCL
  TOT <- SHOT + ROOT
  LEAF.2 <- totLEAF.2

  DIF.TOT <- numeric()
  # DIF.ROOT <- numeric()
  DIF.STEM <- numeric()
  DIF.LEAF <- numeric()
  DIF.PNCL <- numeric()
  DIF.SHOT <- numeric()
  RAT.D.LEAF <- numeric()

  for (j in 1:(length(STEM) - 1)) {
    DIF.TOT <- c(DIF.TOT, TOT[j + 1] - TOT[j])
    # DIF.ROOT <- c(DIF.ROOT,ROOT[j+1] - ROOT[j])
    DIF.STEM <- c(DIF.STEM, STEM[j + 1] - STEM[j])
    DIF.LEAF <- c(DIF.LEAF, LEAF[j + 1] - LEAF[j])
    DIF.PNCL <- c(DIF.PNCL, PNCL[j + 1] - PNCL[j])
    DIF.SHOT <- DIF.STEM + DIF.LEAF + DIF.PNCL
    RAT.D.LEAF <- c(RAT.D.LEAF, (LEAF.2[j] - LEAF.2[j + 1]) / DIF.DOY[j] / LEAF.2[j] / 86400.0)
    ##      RAT.D.LEAF <- c(RAT.D.LEAF,(log(LEAF.2[j])-log(LEAF.2[j+1]))/DIF.DOY[j])
  }

  PAT.STEM <- DIF.STEM / DIF.SHOT
  PAT.LEAF <- DIF.LEAF / DIF.SHOT
  PAT.PNCL <- DIF.PNCL / DIF.SHOT
  # PAT.ROOT <- DIF.ROOT / DIF.TOT


  ## ################################
  ## CALC INTERMEDIATE POINT of DVS #
  ## ################################
  INT.DVS <- numeric()
  for (j in 1:(nrow(dt) - 1)) {
    INT.DVS <- c(INT.DVS, (dt$DVS[j + 1] + dt$DVS[j]) * 0.5)
  }

  # INT.DVS.2 <- numeric()
  # #print(DVS)
  # for(j in 1:(nrow(DF.ROOT.2)-1)){
  #   INT.DVS.2 <- c(INT.DVS.2,(DF.ROOT.2$DVS[j+1] + DF.ROOT.2$DVS[j])*0.5)
  # }

  PAT_df <- data.frame(
    "EXP" = rep(exp, length(INT.DVS)), # Repeat 'exp' to match the length of other vectors
    "SITE" = rep(unique(dt$SITE), length(INT.DVS)),
    "IRR" = rep(unique(dt$IRR), length(INT.DVS)),
    "VRT" = rep(unique(dt$VRT), length(INT.DVS)),
    "Year" = rep(dt$Year[1], length(INT.DVS)),
    "INT.DVS" = INT.DVS,
    "PAT.STEM" = PAT.STEM,
    "PAT.LEAF" = PAT.LEAF,
    "PAT.PNCL" = PAT.PNCL,
    #  "PAT.ROOT" = PAT.ROOT,
    "RAT.D.LEAF" = RAT.D.LEAF,
    "DIF.STEM" = DIF.STEM,
    "DIF.LEAF" = DIF.LEAF,
    "DIF.PNCL" = DIF.PNCL,
    #  "DIF.ROOT" = DIF.ROOT,
    "DIF.SHOT" = DIF.SHOT
  )

  #  plot(dt$DVS,dt$LEF, type = "l")
  list_PAT[[exp]] <- PAT_df
  list_dt[[exp]] <- dt
}
PAT_DF <- do.call(rbind, list_PAT)
dt_DF <- do.call(rbind, list_dt)
write.table(PAT_DF,
  file = paste0(outdir, "Partition.csv"), quote = FALSE, sep = ",", col.names = TRUE,
  row.names = FALSE
)
