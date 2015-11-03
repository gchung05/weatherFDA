# Author: Gary Chung
# Project: Weather Project Using Open FDA Data
# Description: Get historical weather data from NCDC NOAA

# BE WARNED! This script downloads a large amount of data and consumes a lot
# of computational power. Run overnight.

# =========================================

# -----------------
# Required Packages
# -----------------

require(XML)
require(plyr)
require(dplyr)

# ----------------
# Required Sources
# ----------------

# N/A

# ------------------
# Defined Functions
# ------------------

# Read remaining quarters of FDA data
Read.FDA <- function(dset, i){
  plyr::rbind.fill(eval(parse(text=dset)),
                   read.delim(paste0("Data/ascii/", dset, qtrs[i], ".txt"), 
                              sep="$", header=T, stringsAsFactors=F))
}

# ------
# Script
# ------

test <- function(invar){
  eval(parse(text=invar))
}
test("qtrs")

# First download data from AERS database on fda.gov
# Download source: http://www.fda.gov/Drugs/GuidanceComplianceRegulatoryInformation/Surveillance/AdverseDrugEffects/ucm082193.htm
# Download in ASCII format, extract and save all .txt files to Data/ascii
# Then run the following:

# Set the starting and ending quarters of data
start <- "12Q4"
end <- "15Q2"

# Sequence of possible quarters
qtrs <- apply(expand.grid(c(10:20), paste0("Q", c(1:4))), 1, paste, collapse="")
# Filter 
qtrs <- qtrs[qtrs <= end & qtrs >= start]
qtrs <- qtrs[order(qtrs)]

# Set different table names
tbls <- c("DEMO", "DRUG", "INDI", "OUTC", "REAC", "RPSR", "THER")

# Set sequence of year-months
year.mons <- format(seq(as.Date("2007-05-01"), as.Date("2015-08-01"), "month"), 
                    "%Y%m")

# Read in FDA data
demo <- data.frame()
drug <- data.frame()
indi <- data.frame()
outc <- data.frame()
reac <- data.frame()
rpsr <- data.frame()
ther <- data.frame()
for(i in 1:length(qtrs)){
  print(qtrs[i])
  demo <- Read.FDA("demo", i)
  cat("demo ")
  drug <- Read.FDA("drug", i)
  cat("drug ")
  indi <- Read.FDA("indi", i)
  cat("indi ")
  outc <- Read.FDA("outc", i)
  cat("reac ")
  reac <- Read.FDA("reac", i)
  cat("rpsr ")
  rpsr <- Read.FDA("rpsr", i)
  cat("ther \n")
  ther <- Read.FDA("ther", i)
}

# Save
save(demo, drug, indi, outc, reac, rpsr, ther, file="Data/FDA-AERS.Rdata")

# Create a US-only dataset
u.demo <- filter(demo, occr_country == "US")
u.drug <- filter(drug, primaryid %in% unique(u.demo$primaryid))
u.indi <- filter(indi, primaryid %in% unique(u.demo$primaryid))
u.outc <- filter(outc, primaryid %in% unique(u.demo$primaryid))
u.reac <- filter(reac, primaryid %in% unique(u.demo$primaryid))
u.rpsr <- filter(rpsr, primaryid %in% unique(u.demo$primaryid))
u.ther <- filter(ther, primaryid %in% unique(u.demo$primaryid))

# Save
save(u.demo, u.drug, u.indi, u.outc, u.reac, u.rpsr, u.ther, file="Data/FDA-US-AERS.Rdata")

# -------------------------------

# Download weather data from the National Climatic Data Center
# Web reference: www.ncdc.noaa.gov
for(i in 1:length(year.mons)){
  # Get the current URL
  url.now <- c(paste0("http://www.ncdc.noaa.gov/orders/qclcd/QCLCD", year.mons[i],
                      ".zip"))
  download.file(url.now, paste0("Downloads/", year.mons[i]))
}

# Unzip all downloaded files
# Read monthly & daily data into R then delete working files
for(i in 1:length(year.mons)){
  unzip(paste0("Downloads/", year.mons[i]), exdir="./Downloads")
    unzip(zipfile=paste0("Downloads/", "temp"), 
          exdir="./Downloads")

  # Save the current monthly and daily data
  monthly.now <- read.csv(paste0("Downloads/", year.mons[i], "monthly.txt"))
  daily.now <- read.csv(paste0("Downloads/", year.mons[i], "daily.txt"))
  
  # Append to running dataset
  monthly <- rbind.data.frame(monthly, monthly.now)
  daily <- rbind.data.frame(daily, daily.now)
  
  # Delete all downloaded files
  file.remove(paste0("Downloads/", year.mons[i], "station.txt"))
  file.remove(paste0("Downloads/", year.mons[i], "daily.txt"))
  file.remove(paste0("Downloads/", year.mons[i], "hourly.txt"))
  file.remove(paste0("Downloads/", year.mons[i], "monthly.txt"))
  file.remove(paste0("Downloads/", year.mons[i], "precip.txt"))
  file.remove(paste0("Downloads/", year.mons[i], "remarks.txt"))
  file.remove(paste0("Downloads/", year.mons[i]))
}

# Save
save(daily, monthly, file="Data/allWeather.Rdata")
