#### Load packages ####
library(dplyr)
library(ggplot2)
library(cowplot)
library(grid)
library(gridExtra)

#### Import data and prep for analysis ####
data <- read.csv(here::here("6 Data/FishData", "FishData.csv"))

Hisites <- c("Josef", "Burial", "Porlier", "Grainger")
Losites <- c("Coffin", "Snake", "Dragon", "McKenzie", "Anniversary", "Georgina")
fishdata <- data %>% 
  #Generate Weight values for each fish
  mutate(Weight = case_when(abSource == "DFO Haggarty and King 2004 Technical Report 2533" ~ a * (Length ^ b),
                            abSource == "California Fish and Game Fish Bulletin 177" ~ a * ((Length * 10) ^ b),
                            abSource == "NOAA 1978 Washington, Gowan, & Ito" ~ a * ((Length * 10) ^ b),
                            abSource == "FishBase" ~ 10 ^ (log10(a) + (b * log10(Length))))) %>% 
  #Create an identifier
  mutate(TransectID = paste(Site, TransectDepth, Replicate, sep = "_"),
         SiteID = paste(Site, TransectDepth, sep = "_"),
         Hi_Lo = case_when(Site %in% Hisites ~ "High",
                           Site %in% Losites ~ "Low"),
         Hi_Lo = factor(Hi_Lo, levels = c("High", "Low"), ordered = TRUE),
         Latin = factor(Latin),
         CurrDepth = paste(Hi_Lo, TransectDepth, sep = ""),
         CurrDepth = factor(CurrDepth, levels = c("High3", "High15", "Low3", "Low15"),
                            labels = c("High 3m", "High 15m", "Low 3m", "Low 15m"), ordered = TRUE)) %>% 
  #Remove replicate 4 from Georgina - influenced by nesting ling cod
  filter(TransectID != "Georgina_15_4",
         TransectID != "Georgina_3_4",
         Family != "Embiotocidae")

# Create a data frame with site richness, abundance, and biomass
fishtransectsummary <- fishdata %>% 
  group_by(Site, Hi_Lo, TransectDepth, CurrDepth, Replicate, SiteID, TransectID) %>% 
  summarize(Richness = length(unique(Latin)),
            Abundance = length(Length),
            Biomass = round(sum(Weight), 2)) %>% 
  mutate(Biomass_kg = Biomass/1000)
#write.csv(fishtransectsummary, here::here("6 Data/FishData", "FishTransectSummary.csv"), row.names = FALSE)

#ONLY use if I am doing my Mann-Whitney U tests at the site level (aka: avg over replicates at each site)
fishsitesummary <- fishtransectsummary %>% 
  group_by(Site, Hi_Lo, TransectDepth, CurrDepth, SiteID) %>% 
  summarize(Richness = mean(Richness),
            Abundance = mean(Abundance),
            Biomass = mean(Biomass))
#
#### Data Exploration ####
fishtransectsummary$row <- seq_along(fishtransectsummary[[1]])
         
(RichDot <- ggplot(fishtransectsummary) + 
    theme_cowplot() +
    geom_point(aes(x = Richness, y = row), shape = 1) +
    labs(y = "Row", x = "Richness"))
(RichHist <- ggplot(fishtransectsummary, aes(x = Richness)) +
    theme_cowplot() +
    scale_x_continuous(breaks = seq(0, 7, by = 1)) +
    geom_bar(aes(y = ..prop.., group = 1), width = 0.1) +
    labs(y = "Proportion of Surveys", x = "Richness"))

(AbundDot <- ggplot(fishtransectsummary) + 
  theme_cowplot() +
  geom_point(aes(x = Abundance, y = row), shape = 1) +
  labs(y = "Row", x = "Abundance"))
(AbundHist <- ggplot(fishtransectsummary, aes(x = Abundance)) +
  theme_cowplot() +
  geom_bar(aes(y = ..prop.., group = 1), width = 1) +
  labs(y = "Proportion of Surveys", x = "Abundance"))

(BioDot <- ggplot(fishtransectsummary) + 
  theme_cowplot() +
  geom_point(aes(x = Biomass, y = row), shape = 1) +
  labs(y = "Row", x = "Biomass"))
(BioHist <- ggplot(fishtransectsummary, aes(x = Biomass)) +
  theme_bw() +
  theme(panel.grid = element_line(linetype = "blank")) +
  geom_bar(aes(y = ..prop.., group = 1), width = 10) +
  labs(y = "Proportion of Surveys", x = "Biomass"))

# CurrDepth richness, abundance, and biomass
(SiteRich <- ggplot(fishtransectsummary, aes(colour = CurrDepth)) +
  theme_cowplot() +
  facet_wrap(~ CurrDepth) + 
  geom_boxplot(aes(Site, Richness)) + 
  geom_point(aes(Site, Richness)) +
  labs(title = "Fish Species Richness By Site and Depth", y = "Speices Richness", x = "Site") +
  theme(axis.text.x = element_text(angle = 30, vjust = 0.95, hjust = 0.95)))

(SiteAbund <- ggplot(fishtransectsummary, aes(colour = CurrDepth)) +
  theme_cowplot() +
  facet_wrap(~ CurrDepth) + 
  geom_boxplot(aes(Site, Abundance)) + 
  geom_point(aes(Site, Abundance)) +
  labs(title = "Fish Species Abundance By Site and Depth", y = "Number of Fish", x = "Site") +
  theme(axis.text.x = element_text(angle = 30, vjust = 0.95, hjust = 0.95)))

(SiteBio <- ggplot(fishtransectsummary, aes(colour = CurrDepth)) +
  theme_cowplot() +
  facet_wrap(~ CurrDepth, ncol = 2) + 
  geom_boxplot(aes(Site, Biomass)) + 
  geom_point(aes(Site, Biomass)) +
  labs(title = "Fish Species Biomass By Site and Depth", y = "Biomass (g)", x = "Site") +
  theme(axis.text.x = element_text(angle = 30, vjust = 0.95, hjust = 0.95)))

# Display all three plots in the same figure
AllFishPlots <- plot_grid(SiteRich, SiteAbund, SiteBio, ncol = 1, align = "v")

#### Table 1 Species Tables ####
#Check out some basic stats from the data
hilostats <- fishdata %>% 
  group_by(Hi_Lo) %>% 
  summarize(rich = length(unique(Latin)), 
            count = length(Length))

#Create a species table
species_table <- as.data.frame.matrix(table(fishdata$Latin, fishdata$CurrDepth))

sum(species_table$Lo > 0)
sum(species_table$Hi > 0)
sum(species_table$Hi & species_table$Lo)

#### Curr Depth Stats - Currently stats are done over site (replicates averaged). Change to fishtransectsummary if I CAN'T avg over site.  ####
Rich <- fishsitesummary %>%
  group_by(CurrDepth) %>% 
  summarize(Mean = mean(Richness),
            SD = sd(Richness))
kruskal.test(Richness ~ CurrDepth, data = fishsitesummary) # U = 5.9393, df = 3, p = 0.1146

Abund <- fishsitesummary %>%
  group_by(CurrDepth) %>% 
  summarize(Mean = round(mean(Abundance), digits = 2),
            SD = sd(Abundance))
kruskal.test(Abundance ~ CurrDepth, data = fishsitesummary) #U = 11.445, df = 3, p = 0.009548
pairwise.wilcox.test(fishsitesummary$Abundance, fishsitesummary$CurrDepth, p.adjust.method = "none")
#3m hi and lo are not sig diff; 15m hi and lo are not sig diff (clusters by depth) **Sig more fish at depth

Bio <- fishsitesummary %>%
  group_by(CurrDepth) %>% 
  summarize(Mean = mean(Biomass),
            SD = round(sd(Biomass), digits = 2))
kruskal.test(Biomass ~ CurrDepth, data = fishsitesummary) #U = 4.0119, df = 3, p = 0.2602

#### Fig 6 Curr Depth Plots - shows each transect as a data point ####

## Function to calculate the mean and the standard deviation for each group
# data : a data frame
# varname : the name of a column containing the variable to be summariezed
# groupnames : vector of column names to be used as grouping variables
# FROM http://www.sthda.com/english/wiki/ggplot2-error-bars-quick-start-guide-r-software-and-data-visualization
library(Hmisc)
data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}

(CurrDepthRich <- ggplot(fishtransectsummary, aes(x = CurrDepth, y = Richness, fill = CurrDepth)) +
   theme_cowplot() +
   theme(legend.position = "none") +
   labs(y = "Species Richness", x = "") +
   geom_jitter(width = 0.1) + 
   stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), 
                 geom = "errorbar", color = "red", width = 0.2) +
   stat_summary(fun = mean, geom = "point", color = "red") +
   scale_fill_manual(values = c("red", "red4", "blue", "blue4"), name = "Current/Depth Category") +
   scale_y_continuous(breaks = seq(0, 7, by = 1), 
                     limits = c(0, 7)))
(CurrDepthAbund <- ggplot(fishtransectsummary, aes(x = CurrDepth, y = Abundance, fill = CurrDepth)) +
    theme_cowplot() +
    theme(legend.position = "none") +
    labs(y = "Number of Fish", x = "") +
    geom_jitter(width = 0.1) + 
    stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), 
                 geom = "errorbar", color = "red", width = 0.2) +
    stat_summary(fun = mean, geom = "point", color = "red") +
    scale_fill_manual(values = c("red", "red4", "blue", "blue4"), name = "Current/Depth Category") +
    scale_y_continuous(breaks = seq(0, 80, by = 20),
                       limits = c(0, 80)))
(CurrDepthBio <- ggplot(fishtransectsummary, aes(x = CurrDepth, y = Biomass_kg, fill = CurrDepth)) +
    theme_cowplot() +
    theme(legend.position = "none") +
    labs(y = "Mass of Fish (kg)", x = "") +
    geom_jitter(width = 0.1) + 
    stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), 
                 geom = "errorbar", color = "red", width = 0.2) +
    stat_summary(fun = mean, geom = "point", color = "red") +
    scale_fill_manual(values = c("red", "red4", "blue", "blue4"), name = "Current/Depth Category") +
    scale_y_continuous(breaks = seq(0, 20, by = 5),
                       limits = c(-2, 20)))

currdepth <- grid.arrange(arrangeGrob(CurrDepthRich, CurrDepthAbund, CurrDepthBio, ncol = 3, nrow = 1),
                          bottom = textGrob("Current/Depth Category", vjust = -1, gp = gpar(fontsize = 17)),
                          top = textGrob("C.", hjust = 25, gp = gpar(fontsize = 19)))
#
#### Create stacked bar graphs of abundance and biomass by family by hi vs lo ####
fish_stackedbar <- fishdata %>% 
  group_by(CurrDepth, Family) %>% 
  summarise(FamilyAbund = length(Length),
            FamilyBio = sum(Weight)/1000) %>%  #change weight from g to kg
  mutate(Family = replace(Family, Family == "Hemitripteridae", "Cottidae"),
         Family = replace(Family, Family == "Rhamphocottidae", "Cottidae"),
         Family = replace(Family, Family == "Cottidae", "Cottoidea"))

(Abund_Family <- ggplot(data = fish_stackedbar, aes(x = CurrDepth, y = FamilyAbund, fill = Family)) +
    theme_cowplot() +
    theme(legend.position = "none") +
    scale_fill_brewer(palette = "Spectral") +
    geom_bar(position = "stack", stat = "identity") +
    labs(y = "Fish Abundance", x = ""))
(Bio_Family <- ggplot(data = fish_stackedbar, aes(x = CurrDepth, y = FamilyBio, fill = Family)) +
    theme_cowplot() +
    scale_fill_brewer(palette = "Spectral") +
    geom_bar(position = "stack", stat = "identity") +
    labs(y = "Fish Biomass(kg)", x = ""))

get_legend <- function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

mylegend <- get_legend(Bio_Family)
Bio_Family <- Bio_Family + theme(legend.position = "none")

(FamilyAbundBio <- grid.arrange(Abund_Family, Bio_Family, mylegend, ncol = 3, widths = c(1, 1, 0.5)))
#