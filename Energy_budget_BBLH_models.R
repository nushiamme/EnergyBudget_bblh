### GLMM models for Energy budget paper, DEE ~ resources, temperature
## Feb 24, 2019
## code: Anusha Shankar, nushiamme<at>gmail<dot>com

## Reading in packages
require(ggplot2)
require(reshape)
require(plyr)
require(dplyr)
require(ggthemes) ## Trying out Tufteboxplot
require(stringr)
require(MCMCglmm)
require(lme4)
require(lmerTest)
require(multcomp) ## for glht function for plotting lme4 results

## Set working directory
setwd("C:\\Users\\nushi\\Dropbox\\Anusha Committee\\BBLH_EnergyBudget\\Submission_FuncEcol\\Data/")

## Read in resource files
floral <- read.csv("FloralCensusData2013.csv") #ver3 
floralsumm <- floral[floral$Site %in% c("Harshaw", "Sonoita"),]

##Temperature files
## Read in premelted dataframes with temperatures and calculated thermoregulatory costs
m.ta_det <- read.csv("Melted_Ta_thermo.csv")

#### General functions ####
my_theme <- theme_classic(base_size = 30) + 
  theme(panel.border = element_rect(colour = "black", fill=NA))

## DEE and EB model files
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

## New dataframe for GLMM Feb 24, 2019
flo.season_site_sum <- aggregate(floralsumm$TotalFlowers, 
                                 by=list(floralsumm$Season, floralsumm$Site), FUN="sum", na.rm=T)
names(flo.season_site_sum) <- c("Season", "Site", "Sum_flowers")
flo.season_site_sum$Site <- droplevels(flo.season_site_sum$Site)
flo.season_site_sum$Season<- revalue(flo.season_site_sum$Season, c("Pre"="Dry", "Post"="Early-wet"))
flo.season_site_sum$Site<- revalue(flo.season_site_sum$Site, c("Harshaw"="HC", "Sonoita"="SC"))

## Pasting day, month, year together for temp file
## Adding zeros before days and months
m.ta_det$DayMonthYear <- paste0(str_pad(m.ta_det$Day, 2, pad = "0"), "0", m.ta_det$Month, m.ta_det$Year)
temp.summ <- ddply(m.ta_det, .(DayMonthYear, Site), summarise,
      minTemp = min(Ta),
      meanTemp = mean (Ta),
      maxTemp = max(Ta))


dlw_merged_models <- merge(dlw_bblh, temp.summ, by=c("Site", "DayMonthYear"))

dlw_merged_models <- merge(dlw_merged_models, flo.season_site_sum, by=c("Site", "Season"))

#### Models ####
DEE_full <- lmer(kJ_day~log(Sum_flowers)+ meanTemp + maxTemp +Initial_mass_g +(1|Site_proxy), REML=F,
                               data= dlw_merged_models)
summary(DEE_full)
coef(DEE_full)
plot(DEE_full)
plot(ranef(DEE_full))
plot(residuals(DEE_full))

## Taking out season
#DEE_no_ranef <- lm(kJ_day~log(Sum_flowers)+ meanTemp + maxTemp +Initial_mass_g,
#                 data= dlw_merged_models)
#summary(DEE_no_ranef)


## Taking out temps
DEE_resource_mass <- lmer(kJ_day~log(Sum_flowers) + Initial_mass_g +(1|Site_proxy),  REML=F, data= dlw_merged_models)
summary(DEE_resource_mass)

DEE_resource_temps <- lmer(kJ_day~log(Sum_flowers) + meanTemp + maxTemp +(1|Site_proxy),  REML=F, data= dlw_merged_models)
summary(DEE_resource_temps)

DEE_resource <- lmer(kJ_day~log(Sum_flowers) +(1|Site_proxy), REML=F, data= dlw_merged_models)
summary(DEE_resource)

DEE_temps <- lmer(kJ_day~ meanTemp + maxTemp +(1|Site_proxy), REML=F, data= dlw_merged_models)
summary(DEE_temps)

## Best model ### March 19, 2019
DEE_resource_noranef <- lm(kJ_day~log(Sum_flowers), data= dlw_merged_models)
summary(DEE_resource_noranef)
coef(DEE_resource_noranef)
plot(DEE_resource_noranef)


anova(DEE_full, DEE_resource_mass, DEE_resource_temps, DEE_resource, DEE_resource_noranef, DEE_temps)

tmp <- as.data.frame(confint(glht(DEE_resource_mass, mcp(Site_proxy = "Tukey")))$confint)
tmp$Comparison <- rownames(tmp)
ggplot(tmp, aes(x = Comparison, y = Estimate, ymin = lwr, ymax = upr)) +
  geom_errorbar() + geom_point()
