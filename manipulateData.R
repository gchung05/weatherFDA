# Author: Gary Chung
# Project: FDA AERS Machine Learning
# Description: Manipulate data

# =========================================

# -----------------
# Required Packages
# -----------------

require(dplyr)

# ----------------
# Required Sources
# ----------------

load(file="Data/Data/FDA-US-AERS.Rdata")

# ------------------
# Defined Functions
# ------------------

# N/A

# ------
# Script
# ------

# Filter down to reactions for which 1000 or more were received
# Use only the initial report
counts <- table(as.factor(u.reac$pt))
pt.list <- names(counts[counts > 1000])
reac.list <- filter(u.reac, pt %in% pt.list & primaryid %% 10 == 1)
reac.list <- reac.list[!duplicated(reac.list), ]
demo.list <- filter(u.demo, primaryid %% 10 == 1)
demo.list <- demo.list[!duplicated(demo.list), ]
id.list <- unique(demo.list$primaryid)
indi.list <- filter(u.indi, primaryid %in% id.list)
indi.list <- indi.list[!duplicated(indi.list), ]
drug.list <- filter(u.drug, primaryid %in% id.list)
drug.list <- drug.list[!duplicated(drug.list), ]

# Create reactions by demographics dataset
aSet <- merge(reac.list, demo.list, all.x=T, by="primaryid")

save(aSet, reac.list, demo.list, indi.list, drug.list, counts, pt.list, id.list,
     file="Data/analysisData.Rdata")

rm(u.demo, u.drug, u.indi, u.outc, u.reac, u.rpsr, u.ther)