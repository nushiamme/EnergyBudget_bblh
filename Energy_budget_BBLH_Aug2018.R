## Code for paper titled:
#"Assessing energy budget flexibility in a small endotherm"
## Paper authors: A Shankar, CH Graham, JR Canepa, SM Wethington, DR Powers
## Code by: Anusha Shankar, github/nushiamme; 
# contact: nushiamme<at>gmail<dot>com for questions about code

### Contents ####
## Setup, 
## Read files in
## General functions
## Format data - aggregating and melting
## Figure 2: Energy budget models compared against doubly labelled water (DLW) measurements of daily energy expenditure. 
  #Figure 2a has DLW measurements, with individuals recaptured multiple times as colored points. 
  #Figure 2b has DLW values compared directly with model values
## Figure 3: Stacked bar graph showing the different modeled components of the daily energy budget 
## Figure S1: Validation of the modified DLW method
## Figure S3: Scholander-Irving curve for Costa's hummingbirds


#### Setup ####
library(ggplot2)
library(dplyr) 
library(reshape2) 
library(gridExtra)

setwd("C:\\Users\\nushi\\Dropbox\\Anusha Committee\\BBLH_EnergyBudget\\Submission_FuncEcol\\Data")

#### Read in files #### 
#Includes min and max 24h cost by varying activity; per activity, thermo, NEE and BMR scenario; and adjusting hovering for thermoregulatory substitution
energymodels <- read.csv("EnergyBudget_model_values.csv") # For figures 2b and 3
dlw_bblh <- read.csv("DLW_summary.csv") ## For Figure 2 
costas <- read.csv("Costas1986_VO2_DRPowers.csv") ## For Supp fig S3

# Files for Figure S1: DLW validation plots for supplement
valida_A <- read.csv("Validation_Enrichment_dose_A.csv")
valida_B <- read.csv("Validation_enrichment_eqb_B.csv")
valida_C <- read.csv("Validation_CO2produc_dose_C.csv")

#### General functions ####
my_theme <- theme_classic(base_size = 32) + 
  theme(panel.border = element_rect(colour = "black", fill=NA))

my_theme2 <- theme_classic(base_size = 25) + 
  theme(panel.border = element_rect(colour = "black", fill=NA))

Temp.lab <- expression(atop(paste("Temperature (", degree,"C)")))

give.n <- function(x){
  return(c(y = mean(x), label = length(x)))
}

#### Aggregating energy budget models so they can be summarised in a useful way ####

## Minimum per-minute energy costs for activity
m_energymodels_min <- as.data.frame(as.list(aggregate(energymodels$kJ_min_day_HovThermo_adj,
                                                      by=list(energymodels$Activity_budget_type,
                                                              energymodels$BMR_assump,
                                                              energymodels$Thermoreg_scenario,
                                                              energymodels$Site_proxy),
                                                      FUN = function(x) mn = min(x))))
names(m_energymodels_min) <- c("Activity_budget_type", "BMR_category", "Thermoreg_scenario", "Site_proxy",
                               "Min_kJ_day")
head(m_energymodels_min)

## Mean per-minute energy costs for activity
m_energymodels_mean <- as.data.frame(as.list(aggregate(energymodels$kJ_adjBMR_day_HovThermo_adj,
                                                      by=list(energymodels$Activity_budget_type,
                                                              energymodels$BMR_assump,
                                                              energymodels$Thermoreg_scenario,
                                                              energymodels$Site_proxy),
                                                      FUN = function(x) mi = mean(x))))
names(m_energymodels_mean) <- c("Activity_budget_type", "BMR_category", "Thermoreg_scenario", "Site_proxy",
                               "kJ_day")

## Maximum per-minute energy costs for activity
m_energymodels_max <- as.data.frame(as.list(aggregate(energymodels$kJ_max_day_HovThermo_adj,
                                                      by=list(energymodels$Activity_budget_type,
                                                              energymodels$BMR_assump,
                                                              energymodels$Thermoreg_scenario,
                                                              energymodels$Site_proxy),
                                                      FUN = function(x) mx = max(x))))
names(m_energymodels_max) <- c("Activity_budget_type", "BMR_category", "Thermoreg_scenario", "Site_proxy",
                               "Max_kJ_day")

## Merging the min, mean, and max aggregated data frames
m_energymodels <- merge(m_energymodels_min,m_energymodels_mean,
      by=c("Activity_budget_type", "BMR_category", "Thermoreg_scenario", "Site_proxy"))

m_energymodels <- merge(m_energymodels,m_energymodels_max,
                            by=c("Activity_budget_type", "BMR_category", "Thermoreg_scenario", "Site_proxy"))


## Ordering the activity budget types in a useful way- least to most active
m_energymodels$Activity_budget_type <- factor(m_energymodels$Activity_budget_type,
                                                  levels= c("5_20_75", "15_15_70", "25_30_45", "40_40_20"))
m_energymodels$Activity_bmr_thermo <- paste(m_energymodels$Activity_budget_type, 
                                                m_energymodels$BMR_category, m_energymodels$Thermoreg_scenario, sep= "_")

mm_energymodels <- melt(m_energymodels, 
                            id.vars = c("Activity_budget_type", "BMR_category", "Thermoreg_scenario", "Site_proxy"),
                            measure.vars = c("Min_kJ_day", "kJ_day", "Max_kJ_day"))
names(mm_energymodels)[names(mm_energymodels) == 'variable'] <- 'min_mean_max_kJ'
names(mm_energymodels)[names(mm_energymodels) == 'value'] <- 'kJ_day'

## Keeping BMR and thermo at average and only allowing unit activity costs to vary, per activity budget scenario
mm_energymodels_var_act<- mm_energymodels[mm_energymodels$BMR_category=="BMR_mean" & 
                                                mm_energymodels$Thermoreg_scenario=="Random",]
m_energymodels_act <- as.data.frame(as.list(aggregate(mm_energymodels_var_act$kJ_day,
                                                      by=list(mm_energymodels_var_act$Activity_budget_type,
                                                              mm_energymodels_var_act$Site_proxy),
                                                      FUN = function(x) c(mi = min(x), mn = mean(x), mx = max(x)))))

names(m_energymodels_act) <- c("Activity_budget_type", "Site_proxy",
                               "Min_kJ_day", "kJ_day", "Max_kJ_day")
head(m_energymodels_act)


## Allowing everything to vary, aggregate by thermoregulatory scenario and activity scenario
m_energymodels_therm <- as.data.frame(as.list(aggregate(mm_energymodels$kJ_day,
                                                        by=list(mm_energymodels$Activity_budget_type,
                                                                mm_energymodels$Site_proxy,
                                                                mm_energymodels$Thermoreg_scenario),
                                                        FUN = function(x) c(mi = min(x), mn = mean(x), mx = max(x)))))
names(m_energymodels_therm) <- c("Activity_budget_type", "Site_proxy", "Thermoreg_scenario",
                                 "Min_kJ_day", "kJ_day", "Max_kJ_day")
m_energymodels_therm$Activity_budget_type <- factor(m_energymodels_therm$Activity_budget_type,
                                                levels= c("5_20_75", "15_15_70", "25_30_45", "40_40_20"))

head(m_energymodels_therm)


## Allowing everything to vary, aggregate by thermoregulatory scenario and activity scenario
m_energymodels_mean$Activity_budget_type <- factor(m_energymodels_mean$Activity_budget_type,
                                                  levels= c("5_20_75", "15_15_70", "25_30_45", "40_40_20"))
m_energymodels_avg_act <- as.data.frame(as.list(aggregate(m_energymodels_mean$kJ_day,
                                                        by=list(m_energymodels_mean$Activity_budget_type,
                                                                m_energymodels_mean$Site_proxy,
                                                                m_energymodels_mean$Thermoreg_scenario),
                                                        FUN = function(x) c(mi = min(x), mn = mean(x), mx = max(x)))))
names(m_energymodels_avg_act) <- c("Activity_budget_type", "Site_proxy", "Thermoreg_scenario",
                                 "Min_kJ_day", "kJ_day", "Max_kJ_day")
head(m_energymodels_avg_act)

### Stacked bar for energy budget, with activity variability
energymodels_stack <- energymodels[energymodels$BMR_assump != "BMR_min" & energymodels$BMR_assump != "BMR_max" &
                                 energymodels$Thermoreg_scenario != "Rand_cost_min" & energymodels$Thermoreg_scenario != "Rand_cost_max",]
energymodels_stack$Site_date <- paste(energymodels_stack$Site, energymodels_stack$Day, "0", energymodels_stack$Month, sep="")
energymodels_stack$Thermoreg_scenario <- paste("T", energymodels_stack$Thermoreg_scenario, sep="")
energymodels_stack$Thermo_NEE <- paste(energymodels_stack$Thermoreg_scenario, energymodels_stack$NEE_low_high, sep="_")
energymodels_stack$Activity_budget_type <- factor(energymodels_stack$Activity_budget_type,
                                             levels= c("5_20_75", "15_15_70", "25_30_45", "40_40_20"))
m_energymodels_stack <- melt(energymodels_stack, 
                             id.vars=c("kJ_adjBMR_day_HovThermo_adj", "Activity_budget_type", 
                                       "Site_date", "Thermoreg_scenario"), 
                             measure.vars = c("NEE_addon_noTorpor", "NEE_high_torpor", "Act_kJ_day_minusThermo", "Thermo_adj_kJ_day"))

m_energymodels_stack_min <- melt(energymodels_stack, 
                                 id.vars=c("kJ_min_day_HovThermo_adj", "Activity_budget_type", 
                                           "Site_date", "Thermoreg_scenario"), 
                                 measure.vars = c("NEE_addon_noTorpor", "NEE_high_torpor", "ACT_min_kJ_daytime_minusThermo", "Thermo_adj_kJ_day"))

m_energymodels_stack_max <- melt(energymodels_stack, 
                                 id.vars=c("kJ_max_day_HovThermo_adj", "Activity_budget_type", 
                                           "Site_date", "Thermoreg_scenario"), 
                                 measure.vars = c("NEE_addon_noTorpor", "NEE_high_torpor", "ACT_max_kJ_daytime_minusThermo", "Thermo_adj_kJ_day"))

m_energymodels_stack$Activity_budget_type <- factor(m_energymodels_stack$Activity_budget_type,
                                                    levels= c("5_20_75", "15_15_70", "25_30_45", "40_40_20"))
m_energymodels_stack$Thermoreg_scenario <- factor(m_energymodels_stack$Thermoreg_scenario,
                                                  levels= c("TMinimum", "TRandom", "TMaximum"))

m_energymodels_stack_min$Activity_budget_type <- factor(m_energymodels_stack$Activity_budget_type,
                                                     levels= c("5_20_75", "15_15_70", "25_30_45", "40_40_20"))
m_energymodels_stack_min$Thermoreg_scenario <- factor(m_energymodels_stack$Thermoreg_scenario,
                                                   levels= c("TMinimum", "TRandom", "TMaximum"))

m_energymodels_stack_max$Activity_budget_type <- factor(m_energymodels_stack$Activity_budget_type,
                                                     levels= c("5_20_75", "15_15_70", "25_30_45", "40_40_20"))
m_energymodels_stack_max$Thermoreg_scenario <- factor(m_energymodels_stack$Thermoreg_scenario,
                                                   levels= c("TMinimum", "TRandom", "TMaximum"))

activitymodels_24h <- energymodels[energymodels$BMR_assump != "BMR_min" & energymodels$BMR_assump != "BMR_max" &
                                       energymodels$Thermoreg_scenario == "Random",]
activitymodels_24h$Site_date <- paste(activitymodels_24h$Site, activitymodels_24h$Day, "0", activitymodels_24h$Month, sep="")
activitymodels_24h$Activity_budget_type <- factor(activitymodels_24h$Activity_budget_type,
                                                levels= c("5_20_75", "15_15_70", "25_30_45", "40_40_20"))
m_activity_stack <- melt(activitymodels_24h, 
                             id.vars=c("Activity_budget_type", 
                                       "Site_date", "Thermoreg_scenario"), 
                             measure.vars = c("kJ_min_day", "kJ_adjBMR_day", "kJ_max_day"))
m_activity_stack$Activity_budget_type <- factor(m_activity_stack$Activity_budget_type,
                                                  levels= c("5_20_75", "15_15_70", "25_30_45", "40_40_20"))

## Getting percentages for the contribution of each component to the activity budget
m_energymodels_stack$proportion <- (m_energymodels_stack$value/m_energymodels_stack$kJ_adjBMR_day)*100

#Summarize the percentages ## MAY 2018
percent_full_model <- as.data.frame(as.list(aggregate(m_energymodels_stack$proportion,
                                by=list(m_energymodels_stack$variable,
                                        m_energymodels_stack$Thermoreg_scenario),
                                FUN = function(x) c(mi = min(x), mn = mean(x), mx = max(x)))))
names(percent_full_model) <- c("Model_component", "Thermoreg_scenario",  
                           "Min_percentkJ_24h", "Mean_percentkJ_24h", "Max_percentkJ_24h")

#Summarize the percentages for SC 0207 ## MAY 2018
kJ_split_full_model <- as.data.frame(as.list(aggregate(m_energymodels_stack$value,
                                                      by=list(m_energymodels_stack$variable,
                                                              m_energymodels_stack$Thermoreg_scenario),
                                                      FUN = function(x) c(mi = min(x), mn = mean(x), mx = max(x)))))
names(kJ_split_full_model) <- c("Model_component", "Thermoreg_scenario",  
                               "Min_kJ_24h", "Mean_kJ_24h", "Max_kJ_24h")
write.csv(kJ_split_full_model,file="kJ_splitEB_May.csv")

#### plots ####
## Figure 2
## March 25, 2019. Response to reviewer comments on Figure 2
## Plotting DLW values, individual points, and model values all on the same plot, and 
## splitting by site rather than by DLW/model
#### Figure 2a Harshaw ####
Harshaw_dlw <- dlw_bblh[dlw_bblh$Site=="HC",]
Harshaw_dlw<- droplevels(Harshaw_dlw)
dlw_models_hc <- ggplot(Harshaw_dlw, aes(Site_proxy, kJ_day)) + my_theme2 +
  geom_boxplot(alpha=0.5, fill="light grey") +
  geom_point(data=Harshaw_dlw[is.na(Harshaw_dlw$Band_no_recaps),], size=4, alpha=0.9) +
  geom_line(data=Harshaw_dlw[!is.na(Harshaw_dlw$Band_no_recaps),], aes(group=Band_no_recaps), size=1, linetype = 2) +
  geom_point(data=Harshaw_dlw[!is.na(Harshaw_dlw$Band_no_recaps),], aes(fill=Band_no_recaps), pch=21, size=5, alpha=0.9) + 
  scale_fill_manual(values = c("red", "green", "purple")) +
  scale_x_discrete(breaks=c('A','B'),
                   labels=c("Dry", "Early-wet")) +
  stat_summary(fun.data = give.n, geom = "text", hjust=1.5, vjust=-4.5, size=9) +
  theme(legend.position = 'none', axis.ticks.x = element_blank(),
        axis.ticks.y = element_line(size=2),
        axis.text = element_text(color = 'black', hjust=0.5),
        axis.title = element_text(face='bold'),
        axis.title.y = element_text(hjust=0.5),
        plot.title = element_text(hjust = 0.5)) + ylim(9,41) +
  geom_linerange(data=m_energymodels_therm[m_energymodels_therm$Site_proxy %in% c("A", "B"),], #with everything varying
                 aes(Site_proxy, ymin = Min_kJ_day, ymax = Max_kJ_day, 
                     color = Activity_budget_type),
                 position=position_dodge(width = 0.8),
                 size = 2, alpha = 0.4) + 
  geom_linerange(data=m_energymodels_act[m_energymodels_act$Site_proxy %in% c("A", "B"),], #with only activity costs varying
                 aes(x=Site_proxy, ymin = Min_kJ_day, ymax = Max_kJ_day, 
                     color = Activity_budget_type), 
                 position=position_dodge(width=0.8), 
                 size = 5, alpha = 0.4) + 
  geom_linerange(data=m_energymodels_avg_act[m_energymodels_avg_act$Site_proxy %in% c("A", "B"),], #with average activity costs
                 aes(Site_proxy, ymin = Min_kJ_day, ymax = Max_kJ_day, 
                     color = Activity_budget_type), 
                 position=position_dodge(width=0.8), 
                 size = 5) +
  scale_color_manual(values = c('darkgreen', 'orangered2', 'slateblue4', 'violetred3'), 
                     guide = guide_legend(title = "Activity budget \n percent time \n hover_fly_perch")) +
  ggtitle("Harshaw") + xlab("Season") + ylab("Daily \n energy expenditure (kJ)\n")

#### Figure 2a Sonoita ####
Sonoita_dlw <- dlw_bblh[dlw_bblh$Site=="SC",]
Sonoita_dlw<- droplevels(Sonoita_dlw)
dlw_models_sc <- ggplot(Sonoita_dlw, aes(Site_proxy, kJ_day)) + my_theme2 +
  geom_boxplot(alpha=0.5, fill="light grey") +
  geom_point(data=Sonoita_dlw[is.na(Sonoita_dlw$Band_no_recaps),], size=4, alpha=0.9) +
  scale_x_discrete(breaks=c('C','D'),
                   labels=c("Dry", "Early-wet")) +
  stat_summary(fun.data = give.n, geom = "text", hjust=1.5, vjust=-3.5, size=9) +
  geom_linerange(data=m_energymodels_therm[m_energymodels_therm$Site_proxy %in% c("C", "D"),], #with everything varying
                 aes(Site_proxy, ymin = Min_kJ_day, ymax = Max_kJ_day, 
                     color = Activity_budget_type),
                 position=position_dodge(width = 0.8),
                 size = 2, alpha = 0.4) + 
  geom_linerange(data=m_energymodels_act[m_energymodels_act$Site_proxy %in% c("C", "D"),], #with only activity costs varying
                 aes(x=Site_proxy, ymin = Min_kJ_day, ymax = Max_kJ_day, 
                     color = Activity_budget_type), 
                 position=position_dodge(width=0.8), 
                 size = 5, alpha = 0.4) + 
  geom_linerange(data=m_energymodels_avg_act[m_energymodels_avg_act$Site_proxy %in% c("C", "D"),], #with average activity costs
                 aes(Site_proxy, ymin = Min_kJ_day, ymax = Max_kJ_day, 
                     color = Activity_budget_type), 
                 position=position_dodge(width=0.8), 
                 size = 5) +
  scale_color_manual(values = c('darkgreen', 'orangered2', 'slateblue4', 'violetred3'), 
                     guide = guide_legend(title = "Activity budget \n percent time \n hover_fly_perch")) +
  theme(legend.key.size = unit(4, 'lines'), 
        legend.key.height = unit(4, 'lines'),
        legend.margin = margin(t=0.5, unit='cm'),
        legend.title.align = 0.5,
        legend.text.align = 0.5,
        #legend.text=element_text(size=32),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_line(size=2),
        axis.text = element_text(color = 'black', hjust=0.5),
        axis.title = element_text(face='bold'),
        axis.title.y = element_text(hjust=0.5),
        plot.title = element_text(hjust = 0.5)) +
  ggtitle("Sonoita") + xlab("Season") + ylab("")#ylab("Daily \n energy expenditure (kJ)")

#### Figure 2, arranging HC and SC's DLW + models plots together ####
grid.arrange(dlw_models_hc, dlw_models_sc, nrow=1, widths = c(1,1.2))


#### Figure 3 ####
## Stacked bar plots for breaking down energy budget, just one site+date at a time
pl_vSC0207 <- ggplot(m_energymodels_stack[m_energymodels_stack$Site_date=="SC207",], aes(Thermoreg_scenario, y=value, fill=variable)) + 
  facet_grid(~Activity_budget_type, scales='free_x') +
  geom_bar(stat="identity") +
  scale_fill_manual(labels = c("Nighttime no torpor", "Nighttime with torpor", "Daytime activity", "Thermoregulation + BMR"),
                    values = c("#B47ED5", "lavender", "red", "dark blue")) +
  #geom_text(aes(x=Thermoreg_scenario, y =kJ_adjBMR_day_HovThermo_adj, label=kJ_adjBMR_day_HovThermo_adj), size=5) +
  xlab("Thermoregulatory scenarios") +
  ylab("kiloJoules per day\n") +
  ggtitle("Sonoita Creek dry season") +
  my_theme + theme(axis.text.x = element_text(angle=30, size=15, hjust=0.5, vjust=0.5),
                   legend.key.height=unit(3, 'lines'), plot.title = element_text(hjust=0.5)) + 
  guides(fill = guide_legend(title="Energy budget \n component"))


### To make just the DLW data, but in a black-background plot. Not used in the paper
ggplot(dlw_bblh, aes(Site_proxy, kJ_day)) + theme_jetblack(base_size = 20) +
  geom_boxplot(alpha=0.5, fill="grey30", col="grey80") +
  geom_point(data=dlw_bblh[is.na(dlw_bblh$Band_no_recaps),], col="white", size=3, alpha=0.9) +
  geom_line(data=dlw_bblh[!is.na(dlw_bblh$Band_no_recaps),], aes(group=Band_no_recaps, col=Band_no_recaps), size=1, linetype = 2) +
  geom_point(data=dlw_bblh[!is.na(dlw_bblh$Band_no_recaps),], aes(fill=Band_no_recaps),  pch=21, size=5, alpha=0.9) + 
  scale_fill_manual(values = c("red", "green", "purple")) +
  scale_color_manual(values = c("red", "green", "purple")) +
  scale_x_discrete(breaks=c('A','B', 'C', 'D'),
                   labels=c("Cooler, Wet \n Pre", "Cooler, Wet \n Post", "Hotter, Dry \n Pre", "Hotter, Dry \n Post")) +
  stat_summary(fun.data = give.n, geom = "text", hjust=-0.5, vjust=-2, size=5, col="white") +
  theme(panel.border = element_rect(colour = "grey80", fill=NA),
        legend.position = 'none', axis.ticks.x = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks.y = element_line(size=2),
        axis.text = element_text(color = 'black', hjust=0.5),
        axis.title = element_text(face='bold'),
        axis.title.y = element_text(hjust=0.5),
        plot.title = element_text(hjust = 0.5)) + ylim(9,41) +
  xlab("Season") + ylab("Daily \n energy expenditure (kJ)\n")

#### Supplementary plots ####
#### Figure S1: DLW Validation plots ####
# Figure S1a: Enrichment vs. DLW dose (g)
ggplot(valida_A, aes(DLW_dose_g, O_18_Enrichment_ppm, col=Treatment)) + geom_point(size=3, alpha=0.9) + my_theme +
  scale_color_manual(values = c("black", "grey70")) + xlab("DLW dose (g)") + ylab(bquote(~O^18~ 'Enrichment (ppm)')) +
  geom_smooth(method='lm') + theme(legend.key.height=unit(3, 'lines')) + ylim(0,6000)

# Figure S1b: Initial enrichment vs equilibration time
ggplot(valida_B, aes(Eqb_time_min, Initial_enrichment_ppm_per_mg)) + geom_point(size=3, alpha=0.9) + my_theme +
  xlab("Equilibration time (min)") + ylab("Initial enrichment (ppm/mg)") +
  geom_smooth(method='lm') + ylim(0,30)

# Figure S1c: CO2 production vs. DLW dose (g)
ggplot(valida_C, aes(DLW_dose_g, CO2_production_mL_h)) + geom_point(size=3, alpha=0.9) + my_theme +
  xlab("DLW dose (g)") + ylab(bquote(~CO[2]~ 'production (mL/hr)')) +
  geom_smooth(method='lm') + theme(legend.key.height=unit(3, 'lines')) + ylim(0,80)

#### Figure S3: Costa's (Calyptae costae) Scholander-Irving curve for metabolic rate vs. temperature ####
ggplot(costas, aes(Temperature, VO2_ml.g.h)) + geom_point(size=2) + my_theme +
  geom_smooth(method = "loess", col='grey') +
  ylab("Oxygen consumption (ml/min)") + xlab(Temp.lab) + ylim(0.1,0.45)
