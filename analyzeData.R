# Author: Gary Chung
# Project: FDA AERS Machine Learning
# Description: Analyze data

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

load(file="Data/analysisData.Rdata")

# ------------------
# Defined Functions
# ------------------

# N/A

# ------
# Script
# ------

# Clean up event date. Remove year-only, keep year-mon and full date.
bSet <- filter(aSet, event_dt >= 200000)
bSet$event_dt <- with(bSet, ifelse(event_dt < 300000, event_dt*100 + 1, 
                                     event_dt))
bSet <- filter(bSet, event_dt >= 20100401)
bSet$Ym <- as.Date(paste0(substr(as.character(bSet$event_dt), 1, 6), "01"), 
                   "%Y%m%d")


# EXPLORATORY ANALYSIS. RUN AND REVIEW.
# -------------------------------------
# 
#   # Run STL on all MedDRA PTs with greater than a set number of events
#   that <- data.frame()
#   for(i in 1:length(pt.list)){
#     this <- filter(bSet, pt == pt.list[i])
#     if(nrow(this) >= 250) {
#       this.mon <- this %>% group_by(Ym) %>% dplyr::summarise(ct=n())
#       this.ts <- ts(this.mon$ct, start=c(2011, 4), end=c(2015, 6), frequency=12)
#       this.fit <- stl(this.ts, t.window=15, s.window="periodic", robust=TRUE)
#       this.sea <- IQR(as.vector(this.fit$time.series[,"seasonal"])) / 
#         IQR(this.mon$ct)
#       that <- rbind.data.frame(that, cbind.data.frame(pt=pt.list[i], sea=this.sea))    
#     }
#   }
#   rm(this, this.mon, this.ts, this.fit, this.sea, i)
#   that <- arrange(that, desc(sea))
#   
#   # Plot selected PTs and visually review
#   i <- 2
#   this <- filter(bSet, pt == that$pt[i])
#   as.character(that$pt[i])
#   nrow(this)
#   this.mon <- this %>% group_by(Ym) %>% dplyr::summarise(ct=n())
#   this.ts <- ts(this.mon$ct, start=c(2011, 4), end=c(2015, 6), frequency=12)
#   this.fit <- stl(this.ts, t.window=15, s.window="periodic", robust=TRUE)
#   plot(this.fit)
#   rm(i, this, this.mon, this.ts, this.fit)
#   
#   # Found signals at 200, t.window 15 s.window periodic
#   f1 <- that$pt[c(1, 2, 17, 21)]
#   # Found signals at 500, t.window 15 s.window periodic
#   f2 <- that$pt[c(1, 2, 9, 11, 12, 17)]
#   # Final list of MedDRA PTs with potentially interesting signals
#   pts <- c(as.character(f1), as.character(f2))
#   rm(f1, f2, that)
# 
#   save(pts, file="Output/pts.Rdata")
# 
# END EXPLORATORY ANALYSIS
# ------------------------

load(file="Output/pts.Rdata")

# Find reasons for this repeated behavior
# Requires pts from exploratory analysis above

# Create a cleaner analysis dataset
bSet$age <- as.numeric(bSet$age)
bSet$age <- with(bSet, ifelse(age_cod == "WK", age / (365/7), age))
bSet$age <- with(bSet, ifelse(age_cod == "MON", age / 12, age))
bSet$age <- with(bSet, ifelse(age_cod == "HR", 1e-2, age))
bSet$age <- with(bSet, ifelse(age_cod == "DY", age / 365.25, age))
bSet$age <- with(bSet, ifelse(age_cod == "DEC", age * 10, age))
bSet$wt <- as.numeric(bSet$wt)
bSet$wt <- with(bSet, ifelse(wt_cod == "LBS", wt * 0.453592, wt))
bSet$sex <- with(bSet, ifelse(is.na(sex), gndr_cod, sex))
cSet <- select(bSet, -caseid.x, -caseid.y, -drug_rec_act, -caseversion,
               i_f_code, -mfr_dt, -init_fda_dt, -fda_dt, -auth_num, -mfr_num,
               -age_cod, -age_grp, -wt_cod, -rept_dt, -to_mfr, -reporter_country,
               -occr_country, -gndr_cod, -event_dt)

# Add in drugs and indications
hold <- drug.list
hold$nda <- with(hold, ifelse(is.na(nda_num), 0, 1))
cSet.di <- merge(cSet, 
                 select(hold, -caseid, -prod_ai, -val_vbm, -dose_vbm, 
                        -cum_dose_chr, -cum_dose_unit, -dechal, -rechal, 
                        -lot_num, -exp_dt, -nda_num), 
                 all.x=T, by="primaryid")
cSet.di <- merge(cSet.di, select(indi.list, -caseid), all.x=T, 
                 by.x=c("primaryid", "drug_seq"), 
                 by.y=c("primaryid", "indi_drug_seq"))
rm(hold)


# Plot overall signal, seasonal signal, and subgroups for easy vis
require(dygraphs)
require(xts)
require(reshape2)
require(googleVis)

dates <- data.frame(Ym=seq(as.Date("2011/4/1"), as.Date("2015/6/1"), "months"))

i <- 10 # Run once

ten.a <- list()
ten.y <- list()
ten.z <- list()
ten.s <- list()

for(i in 1:10){ # Run for all 10
  # Obtain overall signal by PT, seasonal and trend
  this <- filter(cSet, pt == pts[i])
  this.mon <- this %>% group_by(Ym) %>% dplyr::summarise(ct=n())
  this.ts <- ts(this.mon$ct, start=c(2011, 4), end=c(2015, 6), frequency=12)
  this.fit <- stl(this.ts, t.window=15, s.window="periodic", robust=TRUE)
  # this.mon$seasonal <- this.fit$time.series[, 1]
  y <- merge(dates, this.mon, all.x=T)
  y <- cbind.data.frame(y, seasonal=this.fit$time.series[, 1],
                        trend=this.fit$time.series[, 2])
  y$ct[is.na(y$ct)] <- 0
  rm(this, this.mon, this.ts, this.fit)
  
  # Obtain signal by indication
  this <- filter(cSet.di, pt == pts[i])
  # Count top 5 indications
  counts <- this %>% filter(indi_pt != "Product used for unknown indication" &
                              !is.na(indi_pt)) %>% group_by(indi_pt) %>% dplyr::summarise(ct=n()) %>%
    arrange(desc(ct)) %>% slice(1:5)
  this.mon <- this %>% filter(indi_pt %in% counts$indi_pt) %>% 
    group_by(indi_pt, Ym) %>% dplyr::summarise(ct=n())
  z <- merge(merge(dates, counts$indi_pt, all.x=T),
             this.mon, all.x=T, by.x=c("Ym", "y"), by.y=c("Ym", "indi_pt"))
  z$ct[is.na(z$ct)] <- 0
  z <- dcast(melt(z, c("Ym", "y"), c("ct")), Ym ~ y)
  rm(this, counts, this.mon)
  
  # Sinus Congestion
  # temp <- filter(this, indi_pt == "Crohn's disease")
  
  # Obtain signal by drug name
  this <- filter(cSet.di, pt == pts[i])
  # Do approximate string matching on drug names
  # First get a list of all drug names >= 5 characters long
  # Then replace similar longer names with their shorter counterparts
  this.drug <- unique(this$drugname)[order(nchar(unique(this$drugname)), 
                                           unique(this$drugname))]
  this.drug <- this.drug[nchar(this.drug) >= 5]
  for(j in 1:length(this.drug)){
    hold <- this.drug[j]
    this$drugname[agrepl(this.drug[j], this$drugname, ignore.case=T)] <- 
      this.drug[j]
  }
  rm(hold, j)
  # Now count top 5 drug names
  counts <- this %>% group_by(drugname) %>% dplyr::summarise(ct=n()) %>%
    arrange(desc(ct)) %>% slice(1:5)
  this.mon <- this %>% filter(drugname %in% counts$drugname) %>% 
    group_by(drugname, Ym) %>% dplyr::summarise(ct=n())
  a <- merge(merge(dates, counts$drugname, all.x=T),
             this.mon, all.x=T, by.x=c("Ym", "y"), by.y=c("Ym", "drugname"))
  a$ct[is.na(a$ct)] <- 0
  a <- dcast(melt(a, c("Ym", "y"), c("ct")), Ym ~ y)
  # Now create Sankey dataset for top 5 drugs
  this <- filter(this, drugname %in% counts$drugname)
  this.ct <- this %>% group_by(drugname, indi_pt) %>% dplyr::summarise(value=n())
  s <- filter(this.ct, !is.na(indi_pt))
  sankey <- gvisSankey(filter(this.ct, !is.na(indi_pt)), 
                       from="drugname", to="indi_pt", weight="value",
                       options=list(width=700, height=600,
                                    sankey="{link: {color: { fill: '#d799ae' } },
                              node: { color: { fill: '#a61d4c' },
                              label: { color: '#871b47', fontSize: 16 } }}"))
  rm(this, counts, this.mon, this.ct)
  
  ten.a[[i]] <- a
  ten.y[[i]] <- y
  ten.z[[i]] <- z
  ten.s[[i]] <- s
}

save(dates, cSet, cSet.di, pts, pt.list, ten.a, ten.s, ten.y, ten.z,
     file="Output/reportData.Rdata")

# Graph
y.xts <- xts(select(y, -Ym), y$Ym)
z.xts <- xts(select(z, -Ym), z$Ym)
a.xts <- xts(select(a, -Ym), a$Ym)
dygraph(y.xts, main = pts[i], group="jamby") %>%
  dyOptions(colors = RColorBrewer::brewer.pal(3, "Set1")) %>%
  dyRoller(rollPeriod = 3)
dygraph(z.xts, main = pts[i], group="jamby") %>%
  dyOptions(colors = RColorBrewer::brewer.pal(5, "Set2")) %>%
  dyRoller(rollPeriod = 3) %>%
  dyHighlight(highlightCircleSize = 5, 
              highlightSeriesBackgroundAlpha = 0.2,
              highlightSeriesOpts = list(strokeWidth = 3))
dygraph(a.xts, main = pts[i], group="jamby") %>%
  dyOptions(colors = RColorBrewer::brewer.pal(5, "Set2")) %>%
  dyRoller(rollPeriod = 3) %>%
  dyHighlight(highlightCircleSize = 5, 
              highlightSeriesBackgroundAlpha = 0.2,
              highlightSeriesOpts = list(strokeWidth = 3))
plot(sankey)


# OPEN FDA
##########
require(jsonlite)

data1 <- fromJSON(paste0("https://api.fda.gov/drug/event.json?",
                         "api_key=szuDIP4PDwueFy4Ol8Zfxg4Q5O4hNNHkZs0fDY4o&", 
                         "search=receivedate:[20040101+TO+20150101]"))
temp<-data1$results
data1$meta


#########################
# RETIRED CODE

i <- 1
this <- filter(cSet.di, pt == pts[i] & indi_pt == "Psoriasis")
this.mon <- this %>% group_by(Ym) %>% dplyr::summarise(ct=n())
this.ts <- ts(this.mon$ct, start=c(2011, 4), end=c(2014, 9), frequency=12)
this.fit <- stl(this.ts, t.window=15, s.window="periodic", robust=TRUE)
plot(this.fit, main=paste(nrow(this), pts[i]))
rm(i, this, this.mon, this.ts, this.fit)


# Review the list of PTs
i <- 10
this <- filter(cSet, pt == pts[i])
this.mon <- this %>% group_by(Ym) %>% dplyr::summarise(ct=n())
this.ts <- ts(this.mon$ct, start=c(2011, 4), end=c(2014, 9), frequency=12)
this.fit <- stl(this.ts, t.window=15, s.window="periodic", robust=TRUE)
plot(this.fit, main=paste(nrow(this), pts[i]))
rm(i, this, this.mon, this.ts, this.fit)

# Review the list of PTs controlled by a certain drug/indication
i <- 1
this <- filter(cSet.di, pt == pts[i] & indi_pt == "Psoriasis")
this.mon <- this %>% group_by(Ym) %>% dplyr::summarise(ct=n())
this.ts <- ts(this.mon$ct, start=c(2011, 4), end=c(2014, 9), frequency=12)
this.fit <- stl(this.ts, t.window=15, s.window="periodic", robust=TRUE)
plot(this.fit, main=paste(nrow(this), pts[i]))
rm(i, this, this.mon, this.ts, this.fit)

# Review list of PTs with drugs/indications
i <- 1
this <- filter(cSet.di, pt == pts[i])
length(unique(this$drugname))
this.drug <- unique(this$drugname)[order(nchar(unique(this$drugname)), 
                                         unique(this$drugname))]
this.drug <- this.drug[nchar(this.drug) >= 5]

# Do approximate string matching on drug names
for(i in 1:length(this.drug)){
  hold <- this.drug[i]
  this$drugname[agrepl(this.drug[i], this$drugname, ignore.case=T)] <- 
    this.drug[i]
}
rm(hold, i)


dates <- seq(as.Date("2011/4/1"), as.Date("2014/9/1"), "months")
y <- cbind.data.frame(Ym=dates, seasonal=this.fit$time.series[, 1])

# Create dataset that can be analyzed for factors contributing to seasonality
predSet <- merge(this, y, by="Ym") %>% select(-primaryid, -drug_seq, -pt, -Ym,
                                              -dose_amt, -dose_unit, -dose_form,
                                              -dose_freq)
predSet <- mutate_each(predSet, funs(factor), c(i_f_code, rept_cod, mfr_sndr, 
                                                lit_ref, sex, e_sub, occp_cod, 
                                                role_cod, drugname, route, nda, 
                                                indi_pt))

temp <- (aov(seasonal ~ drugname, predSet))
summary(temp)

require(caret)

require(dplyr)
require(dygraphs)

counts <- this %>% filter(indi_pt != "Product used for unknown indication" &
                            !is.na(indi_pt)) %>% group_by(indi_pt) %>% dplyr::summarise(ct=n()) %>%
  arrange(desc(ct)) %>% slice(1:20)

counts %>%
  mjs_plot(x=ct, y=indi_pt, width=500, height=400) %>%
  mjs_bar() %>%
  mjs_axis_x(xax_format = 'plain')

counts <- this %>% group_by(drugname) %>% dplyr::summarise(ct=n()) %>%
  arrange(desc(ct)) %>% slice(1:20)

counts %>%
  mjs_plot(x=ct, y=drugname, width=500, height=400) %>%
  mjs_bar() %>%
  mjs_axis_x(xax_format = 'plain')

