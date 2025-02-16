# this code for FITTING parameter of biomass partitioning#
rm(list = ls())
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(tidyverse) # for facet_grid
library(patchwork) # for guided common legend

LEVY0 <- 0.38
LEVX1 <- 0.25
LEVY1 <- 0.6
LEVX2 <- 0.659
LEVY2 <- 0
LEVX3 <- 1.2
LEVY3 <- 0
PNCLX1 <- 0.48 # 0.53
PNCLY1 <- 0
PNCLX2 <- 0.72 # 0.72
PNCLY2 <- 1
PNCLX3 <- 1 # 1
PNCLY3 <- 1
DLVX1 <- 0.659
DLVY1 <- 0
DLVX2 <- 1.0
DLVY2 <- 0.0000010
DLVX3 <- 2.0
DLVY3 <- 0.0000010
LLFst <- 4.0
kLLF <- 1.0
RTX <- 0.0
RTY <- 0.5
RTX2 <- 1.0
SLWYA <- 250
SLWYB <- 600
SLWX <- 2.5
SLWY0 <- 180
SLWX1 <- 0.25
SLWY1 <- 450
SLWX2 <- 0.4 # flowering #0.53 #middle of flowering and seedfilling
SLWY2 <- 370
SLWX3 <- 0.659 # SEEDFILL
SLWY3 <- 500

workdir <- "./../prc"
SEEDFILLdvs <- read.table(paste0(workdir, "/seedfillGDD.txt"), header = FALSE)
SEEDFILLdvs <- SEEDFILLdvs[1, 1] # 0.65
FLW <- read.table(paste0(workdir, "/flowerGDD.txt"), header = FALSE)
FLW <- FLW[1, 1] # 0.46 flowering time
FLWdvs <- FLW
indir <- workdir # Load data
partitioned <- read.table(paste0(indir, "/Partition.csv"), header = TRUE, sep = ",")
biomass <- read.table(paste0(indir, "/Biomass_leaffill_DVS.csv"), header = TRUE, sep = ",")
outdir <- paste0(workdir, "/3_FITTING/")
dir.create(outdir, recursive = TRUE)

# ##REMOVE DATA FOR POINT SCALE VALIDATION##
# FOR FITTING
# BRAZIL : BR3, BR2
# Japan : JP2
# USA : 2002-2006
# China : except JIU, NAN, TEX

# partitioned <- partitioned[!grepl("BR1", partitioned$SITE), ]
# biomass <- biomass[!grepl("BR1", biomass$SITE), ]
# # partitioned <- partitioned[!grepl("JP1", partitioned$SITE), ]
# # biomass <- biomass[!grepl("JP1", biomass$SITE), ]
# partitioned <- partitioned[!grepl("2007", partitioned$Year), ]
# biomass <- biomass[!grepl("2007", biomass$Year), ]
# partitioned <- partitioned[!grepl("Jiu", partitioned$VRT), ]
# biomass <- biomass[!grepl("Jiu", biomass$VRT), ]
# partitioned <- partitioned[!grepl("Nan", partitioned$VRT), ]
# biomass <- biomass[!grepl("Nan", biomass$VRT), ]
# partitioned <- partitioned[!grepl("Tex", partitioned$VRT), ]
# biomass <- biomass[!grepl("Tex", biomass$VRT), ]

# FOR POINT-SCALE VALIDATION
# BRAZIL : BR1
# Japan : JP1
# USA : 2007
# China : 2014-2016 JIU, NAN, TEX

# Additional colors from Brewer palettes
additional_colors <- c(
  brewer.pal(9, "Reds"),
  brewer.pal(9, "Greens"),
  brewer.pal(9, "Purples"),
  brewer.pal(9, "Blues"),
  brewer.pal(9, "Oranges"),
  brewer.pal(9, "YlOrBr"),
  brewer.pal(9, "YlOrRd"),
  brewer.pal(9, "PuRd"),
  brewer.pal(9, "RdPu"),
  brewer.pal(9, "BuPu"),
  brewer.pal(9, "GnBu"),
  brewer.pal(9, "PuBuGn"),
  brewer.pal(9, "YlGnBu")
)

# Function to create a scatter plot
create_scatter_plot <- function(data, x, y, color, shape, title, ylab,
                                x_min, x_max, y_min, y_max,
                                X0, X1, X2, X3, X4, Y0, Y1, Y2, Y3, Y4) {
  ggplot(data = data, aes(x = !!sym(x), y = !!sym(y), color = !!sym(color), shape = !!sym(shape))) +
    geom_point(size = 4, color = "black") +
    labs(title = title, x = "Developmental Stage (DVS)", y = ylab) +
    scale_shape_manual(values = c(0:10)) + # c(0,1,2,5)#only for 6 data
    #    scale_shape_manual(values = c("Irrigated" = 19, "Rainfed" = 1)) + #IRR=1, RF=0 #If I want to put the shape depend on rainfed and irrigated
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      legend.position = "right",
      legend.text = element_text(size = 18),
      axis.text = element_text(size = 12),
      axis.title = element_text(size = 14),
      panel.background = element_rect(fill = "white", color = "black"), # to give color in the panel background (where the data been plotted)
      plot.background = element_rect(fill = "white", color = NA), # to give color in the background of the plot
      panel.grid = element_blank()
    ) +
    xlim(x_min, x_max) +
    ylim(y_min, y_max) +
    geom_segment(aes(x = X0, xend = X1, y = Y0, yend = Y1), color = "#E0115F", linewidth = 0.75) +
    geom_segment(aes(x = X1, xend = X2, y = Y1, yend = Y2), color = "#E0115F", linewidth = 0.75) +
    geom_segment(aes(x = X2, xend = X3, y = Y2, yend = Y3), color = "#E0115F", linewidth = 0.75) +
    geom_segment(aes(x = X3, xend = X4, y = Y3, yend = Y4), color = "#E0115F", linewidth = 0.75) +
    geom_vline(xintercept = FLWdvs, linetype = "dashed", color = "black", linewidth = 0.25) +
    geom_vline(xintercept = SEEDFILLdvs, linetype = "dashed", color = "black", linewidth = 0.25) +
    geom_text(aes(x = FLWdvs - 0.025, y = 0.4 * y_max, label = "Flowering"), color = "grey", size = 4, hjust = 0) +
    geom_text(aes(x = SEEDFILLdvs - 0.045, y = 0.5 * y_max, label = "Seed Filling"), color = "grey", size = 4, hjust = 0) +
    geom_text(aes(x = x_max - 0.05, y = 0.5 * y_max, label = "Harvest", color = "black"), color = "grey", size = 4, hjust = 0) +
    guides(shape = guide_legend(override.aes = list(size = 6))) # Increase legend symbol size
}

######################
# function to extract and prepare the data for plotting for Partition data
transform_data <- function(input_df, col, find, replace) {
  df <- input_df
  df$Treatment <- df$IRR # Create a new column 'Treatment' based on 'IRR' for shape which is tidy for legend
  df$Variety <- df$VRT

  # Replace the name of specified list in column "col" named "find" with "replace"
  df <- df %>%
    mutate(across({{ col }}, ~ str_replace_all(., setNames(replace, find))))

  # set the arrangement levels for the column based on the list "replace" by the listed order
  df <- df %>%
    mutate({{ col }} := factor({{ col }}, levels = replace)) %>%
    rename(Site = SITE)

  df$Year <- as.factor(df$Year)
  df$Treatment <- as.factor(df$Treatment)
  df$Variety <- as.factor(df$Variety)

  return(df)
}

# data for partitioning#
df <- transform_data(
  partitioned, SITE, c("BR1", "BR2", "BR3", "JP1", "JP2", "US1", "USA_AgMIP", "CH1"),
  c(
    "Piracicaba (Brazil)", "Frederico Westphalen (Brazil)", "Londrina (Brazil)", "Morioka (Japan)",
    "Tsukubamirai (Japan)", "Champaign (US)", "Ya'an (China)"
  )
)
df$Treatment <- ifelse(df$Treatment == 0, "Rainfed", "Irrigated")

#############
# transpose the database from wide to long format
df_long <- df %>%
  pivot_longer(cols = PAT.STEM:PAT.PNCL, names_to = "Partitioning", values_to = "Glucose Ratio")
# rename the data
df_long <- df_long %>%
  mutate(across(Partitioning, ~ str_replace_all(., c("PAT.STEM" = "Stem", "PAT.LEAF" = "Leaves", "PAT.PNCL" = "Pod"))))

# data for Specific Leaf Weight#
bio <- transform_data(
  biomass, SITE, c("BR1", "BR2", "BR3", "JP1", "JP2", "US1", "CH1"),
  c(
    "Piracicaba (Brazil)", "Frederico Westphalen (Brazil)", "Londrina (Brazil)", "Morioka (Japan)",
    "Tsukubamirai (Japan)", "Champaign (US)", "Ya'an (China)"
  )
)
bio$Treatment <- ifelse(bio$Treatment == 0, "Rainfed", "Irrigated")

#####################
# LEAF PARTITIONING #
#####################
PAT.plot_LEAF <- create_scatter_plot(
  data = df,
  x = "INT.DVS", y = "PAT.LEAF", color = "Site", shape = "Site", # "Treatment",
  title = "", ylab = "Leaves/Shoot",
  x_min = 0, x_max = 1.02, y_min = 0, y_max = 1,
  X0 = 0, # Masutomi san comment
  Y0 = LEVY0,
  X1 = LEVX1,
  Y1 = LEVY1,
  X2 = LEVX2, # SEEDFILL
  Y2 = LEVY2,
  X3 = LEVX3,
  Y3 = LEVY3,
  X4 = 1.2,
  Y4 = 1
)
ggsave(paste0(outdir, "3_LEF_ALL.jpeg"), PAT.plot_LEAF, width = 6, height = 6)

#####################
# POD PARTITIONING #
#####################
PAT.plot_POD <- create_scatter_plot(
  data = df,
  x = "INT.DVS", y = "PAT.PNCL", color = "Site", shape = "Site",
  title = "", ylab = "Pod/Shoot",
  x_min = 0, x_max = 1.02, y_min = 0.0, y_max = 1.0,
  X0 = 0,
  Y0 = 0,
  X1 = PNCLX1,
  Y1 = PNCLY1,
  X2 = PNCLX2,
  Y2 = PNCLY2,
  X3 = PNCLX3,
  Y3 = PNCLY3,
  X4 = 1,
  Y4 = 1
)
ggsave(paste0(outdir, "3_POD_ALL.jpeg"), PAT.plot_POD, width = 6, height = 6)


############################
# DEAD LEAVES PARTITIONING #
############################
PAT.plot_DLEF <- create_scatter_plot(
  data = df,
  x = "INT.DVS", y = "RAT.D.LEAF", color = "Site", shape = "Site",
  title = " ", ylab = expression(paste("Dead leaf ratio (", s^{
    -1
  }, ")")),
  x_min = 0, x_max = 1.02, y_min = 0, y_max = 0.0000011,
  X0 = 0, # current
  Y0 = 0,
  X1 = DLVX1, # 0.725
  Y1 = DLVY1,
  X2 = DLVX2,
  Y2 = DLVY2,
  X3 = DLVX3,
  Y3 = DLVY3,
  X4 = 2,
  Y4 = 0.0000011
)
PAT.plot_DLEF <- PAT.plot_DLEF + theme(legend.position = "right")
ggsave(paste0(outdir, "3_DLEF_ALL.jpeg"), PAT.plot_DLEF, width = 10, height = 5)

#####################
#### SLW FITTING ####
#####################

#############
# bio_CN = bio[grepl("Sichuan", bio$Site), ]
bio <- bio[!grepl("Champaign \\(US\\)", bio$Site), ]
PAT.plot_SLW <- create_scatter_plot(
  data = bio,
  x = "DVS", y = "SLW", color = "Site", shape = "Site",
  title = " ", ylab = expression(paste("Specific Leaf Weight (kg", ha^{
    -1
  }, ")")),
  x_min = 0, x_max = 1.02, y_min = 180, y_max = 600,
  X0 = 0,
  Y0 = -900, # SLWY0,
  X1 = SLWX1, # EMERGENCE
  Y1 = -900, # SLWY1,
  X2 = SLWX2, # FLOWERING
  Y2 = -900, # SLWY2,
  X3 = SLWX3, # SEEDFILL
  Y3 = -900, # SLWY3,
  X4 = 1,
  Y4 = -900
)
SLW <- PAT.plot_SLW + geom_function(fun = function(x) SLWYB + (SLWYA - SLWYB) * exp(-SLWX * x), color = "red", linetype = "solid") &
  theme(
    plot.tag = element_text(size = 10),
    legend.text = element_text(size = 16)
  )
# SLW = SLWYB + (SLWYA - SLWYB) * EXP(-SLWX * DVS) ! org
ggsave(paste0(outdir, "3_SLW_ALL.jpeg"), SLW, width = 10, height = 6)

p1 <- PAT.plot_LEAF
p2 <- PAT.plot_POD
p3 <- PAT.plot_DLEF
p4 <- SLW
ncol <- 2
nrow <- 1

combined <- p1 + p2 & theme(legend.position = "bottom")
combined <- combined + plot_layout(guides = "collect", ncol = ncol) +
  plot_annotation(tag_levels = "a") &
  theme(
    plot.tag = element_text(size = 16), # Adjust size for plot tags
    legend.text = element_text(size = 12), # Adjust size for legend text
    plot.tag.position = c(0.18, 0.9) # "topleft"
  )
combined <- combined +
  plot_annotation(tag_prefix = "(", tag_suffix = ")") # Add parentheses around the tags

ggsave(paste0(outdir, "parameterization.png"), combined, width = 6 * ncol, height = 6 * nrow)


##### All partitioning in 1 frame ######
ggplot(df_long, aes(x = `Glucose Ratio`, fill = Partitioning)) +
  geom_density(alpha = 0.3) +
  labs(
    title = "Density Plot of Glucose Ratio vs INT.DVS",
    x = "INT.DVS",
    y = "Glucose Ratio",
    fill = "Partitioning"
  ) +
  theme_minimal()
DVS <- seq(0, 1, by = 0.01)

# Define each variable
####################################################
##### check the parameter setting WITH FORTRAN VERSION


PRTPNC <- function(DVS, PNCLX1, PNCLY1, PNCLX2, PNCLY2, PNCLX3, PNCLY3) {
  PRTPNC <- numeric(length(DVS)) # Create an empty vector to store results
  for (i in 1:length(DVS)) {
    if (DVS[i] < PNCLX1) {
      PRTPNC[i] <- PNCLY1
    } else if (DVS[i] < PNCLX2) {
      PRTPNC[i] <- ((DVS[i] - PNCLX1) * (PNCLY2 - PNCLY1) / (PNCLX2 - PNCLX1)) + PNCLY1
    } else if (DVS[i] < PNCLX3) {
      PRTPNC[i] <- ((DVS[i] - PNCLX2) * (PNCLY3 - PNCLY2) / (PNCLX3 - PNCLX2)) + PNCLY2
    } else {
      PRTPNC[i] <- 0
    }
  }
  return(PRTPNC) # Return the vector after the loop
}

PRTPNC_df <- PRTPNC(
  DVS = DVS,
  PNCLX1 = PNCLX1,
  PNCLY1 = PNCLY1,
  PNCLX2 = PNCLX2,
  PNCLY2 = PNCLY2,
  PNCLX3 = PNCLX3,
  PNCLY3 = PNCLY3
)

PRTLEF <- function(DVS, LEFX1, LEFY0, LEFY1, LEFX2, LEFY2, LAI, LLFst, kLLF) {
  PRTLEF <- numeric(length(DVS))
  for (i in 1:length(DVS)) {
    if (DVS[i] < LEFX1) {
      PRTLEF[i] <- (LEFY1 - LEFY0) / LEFX1 * DVS[i] + LEFY0
    } else if (DVS[i] < LEFX2) {
      LEFXX2 <- (LEFX2 - DVS[i]) * kLLF + DVS[i]
      LEFXX1 <- DVS[i]
      LEFYY1 <- (LEFY2 - LEFY1) / (LEFX2 - LEFX1) * DVS[i] + LEFY2 - (LEFY2 - LEFY1) / (LEFX2 - LEFX1) * LEFX2
      PRTLEF[i] <- (LEFY2 - LEFYY1) / (LEFXX2 - LEFXX1) * DVS[i] + LEFY2 - (LEFY2 - LEFYY1) / (LEFXX2 - LEFXX1) * LEFXX2
    } else {
      PRTLEF[i] <- 0
    }
    PRTLEF[i] <- max(0, PRTLEF[i])
  }
  return(PRTLEF)
}
PRTLEF_df <- PRTLEF(
  DVS = DVS,
  LEFY0 = LEVY0,
  LEFX1 = LEVX1,
  LEFY1 = LEVY1,
  LEFX2 = LEVX2,
  LEFY2 = LEVY2,
  LLFst = 4,
  kLLF = 1
)

# SLWexp
SLW <- function(DVS, SLWYA, SLWYB, SLWX) {
  SLW_result <- numeric(length(DVS))
  for (i in 1:length(DVS)) {
    SLW_result[i] <- SLWYB + (SLWYA - SLWYB) * exp(-SLWX * DVS[i])
  }
  return(SLW_result)
}

SLWexp_df <- SLW(
  DVS = DVS,
  SLWYA = SLWYA,
  SLWYB = SLWYB,
  SLWX = SLWX
)

SLW <- function(DVS) {
  SLW_result <- numeric(length(DVS))
  for (i in 1:length(DVS)) {
    if (DVS[i] < SLWX1) {
      SLW_result[i] <- SLWY0 + (SLWY1 - SLWY0) / (SLWX1 - 0) * DVS[i]
    } else if (DVS[i] < SLWX2) {
      SLW_result[i] <- (SLWY2 - SLWY1) / (SLWX2 - SLWX1) * (DVS[i] - SLWX1) + SLWY1
    } else if (DVS[i] < SLWX3) {
      SLW_result[i] <- (SLWY3 - SLWY2) / (SLWX3 - SLWX2) * (DVS[i] - SLWX2) + SLWY2
    } else {
      SLW_result[i] <- SLWY3
    }
  }
  return(SLW_result)
}

# y = (y2-y1)/(x2-x1)*(x-x1) + y1
SLW_df <- SLW(DVS = DVS)

LLF <- function(DVS, DLFX1, DLFY1, DLFX2, DLFY2, DLFX3, DLFY3) {
  LLF <- numeric(length(DVS))
  for (i in 1:length(DVS)) {
    if (DVS[i] < DLVX1) {
      LLF[i] <- DLVY1
    } else if (DVS[i] < DLVX2) {
      LLF[i] <- (DLVY2 - DLVY1) / (DLVX2 - DLVX1) * DVS[i] + DLVY2 - (DLVY2 - DLVY1) / (DLVX2 - DLVX1) * DLVX2
    } else if (DVS[i] < DLVX3) {
      LLF[i] <- (DLVY3 - DLVY2) / (DLVX3 - DLVX2) * DVS[i] + DLVY3 - (DLVY3 - DLVY2) / (DLVX3 - DLVX2) * DLVX3
    } else {
      LLF[i] <- DLVY3
    }
  }
  return(LLF)
}
LLF_df <- LLF(
  DVS = DVS,
  DLFX1 = DLFX1,
  DLFY1 = DLFY1,
  DLFX2 = DLFX2,
  DLFY2 = DLFY2,
  DLFX3 = DLFX3,
  DLFY3 = DLFY3
)

PRTSHT <- function(DVS, RTX, RTY, RTX2) {
  PRTSHT <- numeric(length(DVS))
  for (i in 1:length(DVS)) {
    if (DVS[i] < RTX) {
      PRTSHT[i] <- 1 - RTY
    } else if (DVS[i] < RTX2) {
      PRTSHT[i] <- 1 - RTY * (RTX2 - DVS[i]) / (RTX2 - RTX)
    } else {
      PRTSHT[i] <- 1
    }
  }
  return(PRTSHT)
}

PRTSHT_df <- PRTSHT(
  DVS = DVS,
  RTX = RTX,
  RTY = RTY,
  RTX2 = RTX2
)

## Making SLN graph
calculate_SLN <- function(NFERT, DVS) {
  SLN <- numeric(length(DVS))
  Y3 <- (2.25 - 1.8) / (300) * NFERT + 1.8 # YM 20230804
  for (i in 1:length(DVS)) {
    if (DVS[i] < 0.3 / 2) {
      SLN[i] <- (2.25 - 0.75) / (0.3 / 2 - 0) * (DVS[i] - 0) + 0.75 # YM 20230718
    } else if (DVS[i] < 0.8 / 2) {
      SLN[i] <- (1.7 - 2.25) / (0.8 / 2 - 0.3 / 2) * (DVS[i] - 0.8 / 2) + 1.7
    } else if ((DVS < 1.318 / 2)) {
      SLN[i] <- (Y3 - 1.7) / (1.318 / 2 - 0.8 / 2) * (DVS[i] - 1.318 / 2) + Y3
    } else {
      SLN[i] <- (0.75 - Y3) / (1 - 1.318 / 2) * (DVS[i] - 1) + 0.75 # YM 20230718
    }
  }
  return(SLN)
  # return(data.frame(DVS = DVS, SLN = SLN))
}
SLN_0 <- calculate_SLN(0, DVS)
SLN_300 <- calculate_SLN(300, DVS)

merged_PRT <- data.frame(
  DVS = DVS, PRT.PNC = PRTPNC_df, PRT.LEF = PRTLEF_df, PRT.SHOT = PRTSHT_df,
  LLF = LLF_df, SLW = SLW_df, SLWexp = SLWexp_df,
  SLNfert0 = SLN_0, SLNfert300 = SLN_300
)

# Plotting
plot <- ggplot(merged_PRT, aes(x = DVS)) +
  geom_line(aes(y = SLNfert0, color = "SLNfert0"), linewidth = 2) +
  geom_line(aes(y = SLNfert300, color = "SLNfert300"), linewidth = 2) +
  labs(
    title = "Plot of Merged Data",
    x = "DVS",
    y = "Values"
  ) +
  scale_color_manual(values = c(
    "PRT.PNC" = "blue",
    "PRT.LEF" = "red",
    "PRT.SHOT" = "green",
    "LLF" = "purple",
    "SLW" = "orange",
    "SLWexp" = "pink",
    "SLNfert0" = "blue",
    "SLNfert300" = "red"
  )) +
  theme_minimal() +
  ylim(0, 3) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    legend.position = "bottom",
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    panel.background = element_rect(fill = "white", color = "black"), # to give color in the panel background (where the data been plotted)
    plot.background = element_rect(fill = "white", color = NA), # to give color in the background of the plot
    panel.grid = element_blank()
  )

ggsave(paste0(outdir, "SLN.jpeg"), plot, width = 6, height = 6)
