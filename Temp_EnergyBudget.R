## Analyzing and plotting ambient and chamber temperature data for BBLH energy budget paper
## To make a thermoregulatory model for HC and SC
## Script by Anusha Shankar, contact nushiamme<at>gmail<dot>com for questions about code/datasets

### Contents ####
## Read packages in
## Read files in
## General functions
## Format data - New columns, etc.
## Figure 1d: Temperature per site, faceted by Day/Night 
## Running thermoregulatory models
  # Random model
  # Minimum thermoregulatory cost model
  # Max thermoregulatory cost model

## Read in packages
library(reshape)
library(ggplot2)
library(dplyr)
library(data.table)
library(grid)
library(gridExtra)
library(scales) # ! important for temperature density

#### Reading in files and reshaping ####
## Set wd
setwd("C:\\Users\\nushi\\Dropbox\\Anusha Committee\\BBLH_EnergyBudget\\Submission_FuncEcol\\Data")

## Read in file with temperature from each sensor per hour per site (hence temp "details")
temp_details <- read.csv("BBLH_temperatures_compiled.csv")

## Read in premelted dataframes with temperatures and calculated thermoregulatory costs
m.te_det <- read.csv("Melted_Te_thermo.csv")
m.ta_det <- read.csv("Melted_Ta_thermo.csv")

## Can change this depending on Thermoregulatory model- Multiply $thermo_mlO2 (in O2 ml/min) by 15 to get 
#thermoregulatory costs per 15min, assuming the bird is exposed to each temperature sampled for 15 minutes
m.te_det$Hour <- as.factor(m.te_det$Hour)
m.ta_det$Hour <- as.factor(m.ta_det$Hour)
m.te_det$thermo_mlO2_15min <- m.te_det$thermo_mlO2*15
m.ta_det$thermo_mlO2_15min <- m.ta_det$thermo_mlO2*15

#### General functions ####
my_theme <- theme_classic(base_size = 30) + 
  theme(panel.border = element_rect(colour = "black", fill=NA))

# Easy axis labels
Te.lab <- expression(atop(paste("Operative Temperature ( ", degree,"C)")))
Ta.lab <- expression(atop(paste("Ambient Temperature ( ", degree,"C)")))

## Create a day-night variable to make the temperature changes more visible
temp_details$Day_night <- 0
temp_details$Day_night[600<temp_details$Hour& temp_details$Hour<1900] <- "Day"
temp_details$Day_night[temp_details$Hour<700 | 1800<temp_details$Hour] <- "Night"
temp_details$Day_night <- factor(temp_details$Day_night, levels=c('Night', "Day"))


#### Figure 1d: Temperature per site, faceted by Day/Night ####
temp_details$Site <- factor(temp_details$Site, levels=c("SC", "HC"))
ggplot(temp_details, aes(Ta_mean)) + 
  geom_density(aes(fill=Site),alpha=0.7) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), name = "Percentage of values") +
  xlab(Ta.lab) + 
  geom_line(aes(col=Site), stat="density", size = 1) +
  geom_rug(aes(x = Ta_mean, y = 0, col=Site), alpha=0.3, position = position_jitter(height = 0)) +
  my_theme + facet_grid(~Day_night) + #coord_flip() + 
  theme(legend.key.height = unit(3, 'lines'), axis.text.x=element_text(angle=90, vjust=0.5, size=20)) + 
  scale_fill_manual(values = c('red', 'grey'), guide=F) +
  scale_color_manual(values = c('red', 'grey'), guide=F)

# Building a model for thermoregulatory costs ####

#### First, subset each site and the dates needed (from DLW data), make it a separate dataframe, 
# then save it as a list where each hour from the day is a separate object ####

te_hc_1306 <- m.te_det[m.te_det$Site=="HC" & m.te_det$DayMonth=="13,6",]
telist_hc_1306 <- split(te_hc_1306, te_hc_1306$Hour)

te_hc_2706 <- m.te_det[m.te_det$Site=="HC" & m.te_det$DayMonth=="27,6",]
telist_hc_2706 <- split(te_hc_2706, te_hc_2706$Hour)

te_hc_1107 <- m.te_det[m.te_det$Site=="HC" & m.te_det$DayMonth=="11,7",]
telist_hc_1107 <- split(te_hc_1107, te_hc_1107$Hour)

ta_hc_1306 <- m.ta_det[m.ta_det$Site=="HC" & m.ta_det$DayMonth=="13,6",]
talist_hc_1306 <- split(ta_hc_1306, ta_hc_1306$Hour)

ta_hc_2706 <- m.ta_det[m.ta_det$Site=="HC" & m.ta_det$DayMonth=="27,6",]
talist_hc_2706 <- split(ta_hc_2706, ta_hc_2706$Hour)

## Don't have Ta data for July 11, so using July 9 data - also dry season
ta_hc_0907 <- m.ta_det[m.ta_det$Site=="HC" & m.ta_det$DayMonth=="9,7",]
talist_hc_0907 <- split(ta_hc_0907, ta_hc_0907$Hour)

te_sc_0207 <- m.te_det[m.te_det$Site=="SC" & m.te_det$DayMonth=="2,7",]
telist_sc_0207 <- split(te_sc_0207, te_sc_0207$Hour)

te_sc_1607 <- m.te_det[m.te_det$Site=="SC" & m.te_det$DayMonth=="16,7",]
telist_sc_1607 <- split(te_sc_1607, te_sc_1607$Hour)

ta_sc_0207 <- m.ta_det[m.ta_det$Site=="SC" & m.ta_det$DayMonth=="2,7",]
talist_sc_0207 <- split(ta_sc_0207, ta_sc_0207$Hour)

##Don't have 16 July data, so using July 9, which is the last day with >1 sensor
ta_sc_0907 <- m.ta_det[m.ta_det$Site=="SC" & m.ta_det$DayMonth=="9,7",]
talist_sc_0907 <- split(ta_sc_0907, ta_sc_0907$Hour)

#### Randomly sampling temperatures to get random theroregulatory costs ####
## Using function to pull random values from 4 sensors at a site, with replacement, to represent 
# temperatures in 15 min intervals
rand_therm <- function (list_day) {
  for (i in 1:100){
    iter <- lapply(list_day, function(x) {
      if(as.numeric(as.character(x$Hour[1]))<=1900 & 
           as.numeric(as.character(x$Hour[1])) >=500) {
      temp_rows <- sample_n(x, 4, replace = F) # currently without replacement
      sum(temp_rows$thermo_mlO2_15min)
      }
    })
   test <- do.call(sum, iter)
   # make ddmm variable to call date and month for file name
   ddmm_site <- paste(list_day[[1]][1,3], list_day[[1]][1,4], list_day[[1]][1,2], sep="_") 
   saveRDS(iter, paste("Thermo_iterations//iter//" , ddmm_site, "_iter", i, ".RDS", sep = ""))
   saveRDS(test, paste("Thermo_iterations//test//", ddmm_site, "_test", i, ".RDS", sep = ""))
  }
}

## Applying the function to just ambient temperatures at the sites, on the days for which we have DLW DEE data
rand_therm(talist_hc_1306)
rand_therm(talist_hc_2706)
rand_therm(talist_hc_0907)
rand_therm(talist_sc_0207)
rand_therm(talist_sc_0907)


## Bind data from all iterations of one day together
compile_iters <- function(x) {
  compiled_daily_thermo <- list.files(path = 'Thermo_iterations\\test\\', pattern = x)
  setwd("C:\\Users\\nushi\\Dropbox\\Anusha Committee\\BBLH_EnergyBudget\\Submission_FuncEcol\\Data\\Thermo_iterations\\test")
  dat_list <- lapply(compiled_daily_thermo, function (x) data.table(readRDS(x)))
  daily_thermo_results <- rbindlist(dat_list)
  setwd("C:\\Users\\nushi\\Dropbox\\Anusha Committee\\BBLH_EnergyBudget\\Submission_FuncEcol\\Data")
  plot_iter <- ggplot(daily_thermo_results, aes(V1)) + geom_histogram(bins = 20) + my_theme +
    xlab("Daily thermoregulatory costs (mL O2 consumed)") + ggtitle(x)
  plot_iter
  Rand_res <- data.frame(matrix(NA, nrow = 3, ncol = 3))
  names(Rand_res) <- c("RandTemp_median_thermo_day", "RandTemp_min_thermo_day", "RandTemp_max_thermo_day")
  paste("Median daytime thermo cost in mL O2 = ", round(median(daily_thermo_results$V1),2),
        "; Min daytime thermo cost in mL O2 = ", round(min(daily_thermo_results$V1),2), 
        "; Max daytime thermo cost in mL O2 = ", round(max(daily_thermo_results$V1),2))
}

compile_iters('1306_test.*.RDS$')
compile_iters('2706_test.*.RDS$')
compile_iters('9070HC_test.*.RDS$')
compile_iters('207_test.*.RDS$')
compile_iters('9070SC_test.*.RDS$')

#### Minimum thermoregulatory costs ####
## Simmilar function as above, but using lowest 4 thermoreg costs from that hour and day, rather than 
## randomly sampling 4 temperatures across the landscape
minTemp_therm <- function (list_day) {
  iter <- lapply(list_day, function(x) {
    if(as.numeric(as.character(x$Hour[1]))<=1900 & 
       as.numeric(as.character(x$Hour[1])) >=500) {
      min_rows <- data.frame(matrix(NA, nrow = 4, ncol = 1))
      min_rows$thermo_mlO2_15min[1] <- sort(x$thermo_mlO2_15min)[1]
      min_rows$thermo_mlO2_15min[2] <- sort(x$thermo_mlO2_15min)[2]
      min_rows$thermo_mlO2_15min[3] <- sort(x$thermo_mlO2_15min)[3]
      min_rows$thermo_mlO2_15min[4] <- sort(x$thermo_mlO2_15min)[4]
      sum(min_rows$thermo_mlO2_15min)
    }
  })
  test <- do.call(sum, iter)
  # make ddmm variable to call date and month for file name
  ddmm_site <- paste(list_day[[1]][1,3], list_day[[1]][1,4], list_day[[1]][1,2], sep="_")
  saveRDS(iter, paste("Thermo_iterations//iter_min2//" , ddmm_site, "_itermin2", ".RDS", sep = ""))
  saveRDS(test, paste("Thermo_iterations//test_min2//", ddmm_site, "_testmin2", ".RDS", sep = ""))
}

minTemp_therm(talist_hc_1306)
minTemp_therm(talist_hc_2706)
minTemp_therm(talist_hc_0907)
minTemp_therm(talist_sc_0207)
minTemp_therm(talist_sc_0907)

#### Maximum thermoregulatory costs ####
## Function to calculate thermoregulatory costs if the bird spent its time, every hour, with the 
# highest thermoregulatory costs per hour
maxTemp_therm <- function (list_day) {
  iter <- lapply(list_day, function(x) {
    if(as.numeric(as.character(x$Hour[1]))<=1900 & 
       as.numeric(as.character(x$Hour[1])) >=500) {
      n <- length(x$thermo_mlO2_15min)
      max_rows <- data.frame(matrix(NA, nrow = 4, ncol = 1))
      max_rows$thermo_mlO2_15min[1] <- sort(x$thermo_mlO2_15min,partial=n)[n]
      max_rows$thermo_mlO2_15min[2] <- sort(x$thermo_mlO2_15min,partial=n-1)[n-1]
      max_rows$thermo_mlO2_15min[3] <- sort(x$thermo_mlO2_15min,partial=n-2)[n-2]
      max_rows$thermo_mlO2_15min[4] <- sort(x$thermo_mlO2_15min,partial=n-2)[n-3]
      sum(max_rows$thermo_mlO2_15min)
    }
  })
  test <- do.call(sum, iter)
  # make ddmm variable to call date and month for file name
  # can use zero to separate because months in study 
  ddmm_site <- paste(list_day[[1]][1,3], list_day[[1]][1,4], list_day[[1]][1,2], sep="_") 
  # were single digit (i.e. <10)
  saveRDS(iter, paste("Thermo_iterations//iter_max2//" , ddmm_site, "_itermax2", ".RDS", sep = ""))
  saveRDS(test, paste("Thermo_iterations//test_max2//", ddmm_site, "_testmax2", ".RDS", sep = ""))
}

maxTemp_therm(talist_hc_1306)
maxTemp_therm(talist_hc_2706)
maxTemp_therm(talist_hc_0907)
maxTemp_therm(talist_sc_0207)
maxTemp_therm(talist_sc_0907)


