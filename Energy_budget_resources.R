## Floral census data 2013 HC/SC analyses
## Anusha Shankar
## Data collection headed by Susan M Wethington, with the help of a number of interns at HMN
## For BBLH energy budget paper

## Reading in packages
require(ggplot2)
require(reshape)
#require(plyr)
require(dplyr)

## Set working directory
setwd("C:\\Users\\nushi\\Dropbox\\Anusha Committee\\BBLH_EnergyBudget\\Submission_FuncEcol\\Data")

## Read in file
floral <- read.csv("FloralCensusData2013.csv")
floralsumm <- floral[floral$Site %in% c("Harshaw", "Sonoita"),]

#### New columns/dataframes ####
colnames(floralsumm)[colnames(floralsumm)=="Pre_post"] <- "Season"
floralsumm$Season<- plyr::revalue(floralsumm$Season, c("Pre"="Dry", "Post"="Early-wet"))
flo <- group_by(floralsumm, Site, Transect, Season)
## If this only creates a single-value dataframe, it's because because R is using plyr::summarise instead of 
## dplyr's summarise. Solution: Don't require(plyr)
dflo <- summarize(flo, Flowers = sum(TotalFlowers, na.rm = T))
dflo$Site_Season <- paste(dflo$Site, dflo$Season, sep="_")
dflo$Site_Season <- factor(dflo$Season, levels = c("Harshaw_Dry", "Harshaw_Early-wet", 
                                                       "Sonoita_Dry", "Sonoita_Early-wet"))

#### General functions ####
## Saving standard theme  
my_theme <- theme_classic(base_size = 30) + 
  theme(axis.title.y = element_text(color = "black", vjust = 2),
        panel.border = element_rect(colour = "grey60", fill=NA),
        axis.line = element_line(colour = "grey60"),
        strip.background = element_rect(colour="grey60", size = 2))

## Theme with slightly smaller font
my_theme2 <- my_theme + theme_classic(base_size = 15)

## Give sample size
give.n <- function(x){
  return(c(y = mean(x), label = length(x)))
}

#### Plots ####
dflo$Pre_post <- factor(dflo$Season, levels = c("Dry", "Early-wet"))

## Figure 1e: plot of resources at Hawshaw vs Sonoita, Dry- vs early-wet
ggplot(dflo[dflo$Flowers>0,], aes(Season, log(Flowers))) + #facet_grid(~Site, scales="free_x") +
  geom_boxplot(aes(fill=Site), position="dodge", show.legend = F, alpha=0.7) + 
  geom_point(aes(x=Season, size=log(Flowers)), alpha=0.8, show.legend = F) + facet_grid(~Site) +
  scale_fill_manual(values=c("grey", "red")) +
  #geom_text(aes(label=Flowers), hjust=-0.3, size=6, position=position_jitter(width=0.3)) +
  my_theme + theme(legend.key.height = unit(3,"line")) +
  xlab("Season")

ggplot(dflo[dflo$Flowers>0,], aes(Season, log(Flowers))) + #facet_grid(~Site, scales="free_x") +
  geom_boxplot(aes(fill=Site), position="dodge", show.legend = F, alpha=0.7) + 
  geom_point(aes(x=Season, size=log(Flowers)), alpha=0.8, show.legend = F) + facet_grid(~Site) +
  scale_fill_manual(values=c("grey", "red")) +
  #geom_text(aes(label=Flowers), hjust=-0.3, size=6, position=position_jitter(width=0.3)) +
  xlab("Season") + theme(axis.text.x = element_text(size=12),
      axis.text.y = element_text(size=12,color="black"),
      axis.title.y=element_text(size=20),
      plot.background = element_blank(),
      panel.border=element_blank(),
      panel.grid.major= element_line(colour=NA), 
      panel.grid.minor=element_line(colour=NA),
      title=element_text(size=20),
      panel.background = element_rect(fill = "white"),
      axis.line.x=element_line(colour="black"),
      axis.line.y=element_line(colour="black"),
      strip.background=element_rect(fill="white", color="black"),
      strip.text=element_text(size=15))
