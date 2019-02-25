### GLMM models for Energy budget paper, DEE ~ resources, temperature
## Feb 24, 2019
## code: Anusha Shankar, nushiamme<at>gmail<dot>com

## Reading in packages
require(ggplot2)
require(reshape)
require(plyr)
require(dplyr)
require(ggthemes) ## Trying out Tufteboxplot
library(stringr)

## Set working directory
setwd("C:\\Users\\nushi\\Dropbox\\Anusha Committee\\BBLH_EnergyBudget\\Submission_FuncEcol\\Data/")
## wd at GFU
#setwd("/Users/anshankar/Dropbox/Anusha Committee/BBLH_EnergyBudget/Tables")

## Read in resource files
floral <- read.csv("FloralCensusData2013.csv") #ver3 
floralsumm <- floral[floral$Site %in% c("Harshaw", "Sonoita"),]

##Temperature files
## Read in file with temperature from each sensor per hour per site (hence temp "details")
temp_details <- read.csv("BBLH_temperatures_compiled.csv")
## Read in premelted dataframes with temperatures and calculated thermoregulatory costs
m.ta_det <- read.csv("Melted_Ta_thermo.csv")

## DEE and EB model files
energymodels <- read.csv("EnergyBudget_model_values.csv") # Fir figures 2b and 3
dlw_bblh <- read.csv("DLW_summary.csv") ## For Figure 2 
dlw_bblh$DayMonthYear <- paste0(str_pad(dlw_bblh$Day, 2, pad = "0"), "0", dlw_bblh$Month, dlw_bblh$Year)

#### New columns/dataframes ####
colnames(floralsumm)[colnames(floralsumm)=="Pre_post"] <- "Season"
floralsumm$Season<- revalue(floralsumm$Season, c("Pre"="Dry", "Post"="Early-wet"))
flo <- group_by(floralsumm, Site, Transect, Season)
dflo <- summarize(flo, Flowers = sum(TotalFlowers, na.rm = T))
#dhumm <- summarize(flo, Hummcount = sum(HummSp, na.rm=T))
#dfru <- summarize(flo, Fruits = sum(Fruits, na.rm=T))
#dbud <- summarize(flo, Buds = sum(Buds, na.rm=T))
dflo$Site_Season <- paste(dflo$Site, dflo$Season, sep="_")
dflo$Site_Season <- factor(dflo$Season, levels = c("Harshaw_Dry", "Harshaw_Early-wet", 
                                                   "Sonoita_Dry", "Sonoita_Early-wet"))

#### New columns/dataframes, Jan 12 ####
colnames(scrop)[colnames(scrop)=="Pre_post"] <- "Season"
scrop$Season<- revalue(scrop$Season, c("Pre"="Dry", "Post"="Early-wet"))
dcrop <- group_by(scrop, Site, Year, Season, Transect)
dcrop_summ <- summarize(dcrop, Calories = sum(Calories, na.rm = T))

## New dataframe for GLMM Feb 24, 2019
flo.season_site_sum <- aggregate(floralsumm$TotalFlowers, 
                                 by=list(floralsumm$Season, floralsumm$Site), FUN="sum", na.rm=T)
names(flo.season_site_sum) <- c("Season", "Site", "Sum_flowers")
levels(flo.season_site_sum$Site) <- droplevels(flo.season_site_sum$Site)

## Temperature aggregates for glmm model, Feb 24, 2019
temp.agg <- aggregate(m.ta_det$Ta, 
                     by=list(floralsumm$PlantSpecies, floralsumm$Month), FUN="mean", na.rm=T)
names(flo.agg) <- c("Species", "Month", "Avg_flowers")

## Pasting day, month, year together for temp file
## Adding zeros before days and months
m.ta_det$DayMonthYear <- paste0(str_pad(m.ta_det$Day, 2, pad = "0"), "0", m.ta_det$Month, m.ta_det$Year)
temp.summ <- ddply(m.ta_det, .(DayMonthYear, Site), summarise,
      minTemp = min(Ta),
      meanTemp = mean (Ta),
      maxTemp = max(Ta))

dlw_merged_models <- merge(dlw_bblh, temp.summ, by=c("Site", "DayMonthYear"))

dlw_merged_models <- merge(dlw_merged_models, flo.season_site_sum[,"Sum_flowers"], by=c("Site", "Season"))


#### Models ####
DEE_resource_temps <- MCMCglmm(DEE~Flowers+Ta_min+Season,
                               data=)
