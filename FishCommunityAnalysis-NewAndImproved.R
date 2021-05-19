#### Load libraries ####
library(dplyr)
library(vegan)
library(reshape2)
library(ggplot2)
library(grid)
library(gridExtra)
library(cowplot)

#### Import data & prep for analysis ####
data <- read.csv(here::here("6 Data/FishData", "FishData.csv")) %>% 
  mutate(Site = factor(Site),
         TransectDepth = factor(TransectDepth),
         Replicate = factor(Replicate),
         Latin = factor(Latin),
         CurrDepth = paste(Hi_Lo, TransectDepth, sep = ""),
         CurrDepth = factor(CurrDepth, levels = c("Hi3", "Hi15", "Lo3", "Lo15"),
                                   labels = c("High 3m", "High 15m", "Low 3m", "Low 15m"), ordered = TRUE),
         TransectID = paste(Site, TransectDepth, Replicate, sep = "_"),
         SiteDepth = paste(Site,  TransectDepth, sep = "_"))

#### Create a hierarchical dataframe of each combination of Site, TransectDepth, Replicate, and Latin (this accounts for 0 sightings) ####
Hisites <- c("Josef", "Burial", "Porlier", "Grainger")
Losites <- c("Coffin", "Snake", "Dragon", "McKenzie", "Anniversary", "Georgina")
Embiotocidae <- c("Brachyistius frenatus", "Cymatogaster aggregata", "Embiotoca lateralis", "Rhacochilus vacca")
fishsum <- with(data, expand.grid(Site = levels(Site),
                                  TransectDepth = levels(TransectDepth),
                                  Replicate = levels(Replicate),
                                  Latin = levels(Latin))) %>% 
  # Remove fourth replicate transects not conducted, replicates that weren't done are given a value of 1
  mutate(exclude = case_when(Site == "Josef" & Replicate == 4 ~ 1,
                             Site == "Coffin" & Replicate == 4 ~ 1,
                             Site == "Porlier" & Replicate == 4 ~ 1,
                             Site == "Grainger" & Replicate == 4 ~ 1,
                             Site == "Anniversary" & Replicate == 4 & TransectDepth == 3 ~ 1,
                             Site == "Georgina" & Replicate == 4 ~ 1,
                             Latin %in% Embiotocidae ~ 1)) %>%
  # Keep only the sites that were done/have data
  filter(is.na(exclude)) %>% 
  select(-one_of(c("exclude"))) %>%  # Remove exclude column
  # Create observation identifiers
  mutate(Hi_Lo = case_when(Site %in% Hisites ~ "High",
                           Site %in% Losites ~ "Low"),
         TransectID = paste(Site, TransectDepth, Replicate, sep = "_"),
         TransectIDLatin = paste(TransectID, Latin, sep = "_"),
         TransectDepth = TransectDepth %>% as.character() %>% factor(levels = c("3", "15"), ordered = TRUE),
         Hi_Lo = factor(Hi_Lo, levels = c("Low", "High"), ordered = TRUE),
         CurrDepth = paste(Hi_Lo, TransectDepth, sep = ""),
         CurrDepth = factor(CurrDepth, levels = c("High3", "High15", "Low3", "Low15"), ordered = TRUE)) %>% 
  filter(TransectID != "Georgina_3_4",
         TransectID != "Georgina_15_4")

# Create a data frame with each TransectID abundance
transectabund <- data %>% 
  filter(Family != "Embiotocidae") %>% 
  mutate(TransectIDLatin = paste(Site, TransectDepth, Replicate, Latin, sep = "_")) %>% 
  group_by(TransectIDLatin) %>% 
  summarize(Abundance = length(Length))

# Combine fishsum with transectabund
fishsum <- dplyr::left_join(fishsum, transectabund, by = c("TransectIDLatin"))
#
#### Create nMDS plot ####
#Create wide data frame
sp_matrix1 <- dcast(fishsum, TransectID ~ Latin, value.var = "Abundance", fill = 0)
sp_matrix <- sp_matrix1[, 2:27]

#Extract CurrDepth values for each TransectID to create nMDS ellipses
sp_matrix_TransectID <- sp_matrix1 %>% 
  select(TransectID)
CurrDepthCat <- fishsum %>% 
  select(TransectID, CurrDepth) %>%  
  distinct() 
CurrDepthCat <- sp_matrix_TransectID %>% 
  left_join(CurrDepthCat, by = "TransectID") %>% 
  select(CurrDepth)
CurrDepth_Vector <- CurrDepthCat$CurrDepth

#Determine the best distribution for these data
rankindex(CurrDepth_Vector, sp_matrix)
#gower distribution is the best distribution as it is suitable for data with factors

sp_matrix_NMDS <- metaMDS(sp_matrix, k = 2, dist = "gow", try = 20, trymax = 100, noshare = 0.2)
#16 of 25 species (64%) were observed 7 times or less over 69 observations (10%) 64% of the species were observed only 10% of the time.
#Super 0 inflated data! 84% of the data are zeros 1506 values of 0, 288 values of not 0 
#The function performs a Wisconsin double standardization and a sqrt transformation since the abundances have so many zeros
#In a Wisconsin double standardization, each element is divided by its column maximum and then divided by the row total
#Square root transformation is used to deal with the zero inflated data
#noshare is used when there are many sites that do not share species.
#The stepacross function is used when 20% or more of the site pairs have no shared species. (10% wasn't great enough and convergence was not possible)
#Stress = 0.2430836 (not great)

stressplot(sp_matrix_NMDS)

#Create nMDS plot
ordiplot(sp_matrix_NMDS, type = "n", xlim = c(-0.3, 0.3), ylim = c(-0.15, 0.2))
text(x = 0.1925, y = 0.22, paste("ANOSIM R Statistic = ", round(anosim$statistic, digits = 3)), font = 2, cex = 0.8)
text(x = 0.235, y = 0.2, paste("P-value = ", round(anosim$signif, digits = 3)), font = 2, cex = 0.8)
text(x = 0.24, y = 0.18, paste("Stress = ",round(sp_matrix_NMDS$stress, digits = 3)), font = 2, cex = 0.8)
legend(x = -0.31, y = 0.24, title = "Current-Depth Category", c("High 3 meter", "High 15 meter", "Low 3 meter", "Low 15 meter"), 
       fill = c("red", "red4", "blue", "blue4"), cex = 0.75, box.lty = 0, y.intersp = 0.75, x.intersp = 0.5)
orditorp(sp_matrix_NMDS, display = "species", cex = 0.8, air = 0.01, col = "black")
ordiellipse(sp_matrix_NMDS, groups = CurrDepth_Vector, draw = "polygon", kind = "se", conf = 0.95,
            show.groups = "High3", col = "red", label = FALSE)
ordiellipse(sp_matrix_NMDS, groups = CurrDepth_Vector, draw = "polygon", kind = "se", conf = 0.95,
            show.groups = "High15", col = "red4", label = FALSE)
ordiellipse(sp_matrix_NMDS, groups = CurrDepth_Vector, draw = "polygon", kind = "se", conf = 0.95,
            show.groups = "Low3", col = "blue", label = FALSE)
ordiellipse(sp_matrix_NMDS, groups = CurrDepth_Vector, draw = "polygon", kind = "se", conf = 0.95,
            show.groups = "Low15", col = "blue4", label = FALSE)
#650*542

#### ANOSIM & SIMPER ####
anosim <- anosim(sp_matrix, group = CurrDepth_Vector, permutations = 999, distance = "gow") 
summary(anosim) #anosim stat R = 0.1691, significance = 0.001, low R means there is not much difference between lo and hi,
#there is statistically significant difference between currdepth categories
plot(anosim)

simper <- simper(sp_matrix, group = CurrDepth_Vector, permutations = 999)
summary(simper, ordered = TRUE, digits = 3)
#Scalyhead sculpins, Blackeye gobies, and longfin sculpins account for 75% or more of the dissimilarities b/w currdepth categories in 5 of the 6 currdepth pairs
#The Low3 High3 pairing has scalyhead and goby accounting for 62.5% with copper rockfish coming in third for a combined dissimilarity of 71.9%. zonope came in fifth.

#### Fish with 2 or more observations at each CurrDepth ####
data <- data %>% 
  mutate(CurrDepth = paste(Hi_Lo, TransectDepth, sep = "_"))
table(data$Latin, data$CurrDepth) #Artedius harringtoni, Hexagrammos decagrammus, Jordania zonope, Rhinogobiops nicholsii, Sebastes caurinus
table(data$Latin, data$Hi_Lo) #c("Artedius harringtoni", "Hexagrammos decagrammus", "Jordania zonope", "Oxylebius pictus", "Rhinogobiops nicholsii", "Sebastes caurinus")

#Species lists based on 2 or more obs at each currdepth
specieslist <- c("Artedius harringtoni", "Hexagrammos decagrammus", "Jordania zonope", "Rhinogobiops nicholsii", "Sebastes caurinus")
commonspecieslist <- c("Blackeye Goby", "Copper Rockfish", "Kelp Greenling", "Longfin Sculpin", "Scalyhead Sculpin")
#
#### CurrDepth Species Abundances Figure ####
#average counts over replicates
currdepthabund <- data %>% 
  filter(Latin %in% specieslist) %>% 
  mutate(SiteDepth = paste(Site, TransectDepth, sep = "")) %>% 
  group_by(CurrDepth, SiteDepth, Latin, TransectID) %>% 
  summarize(TransectAbund = length(Length))

## Function to calculate the mean and the standard deviation for each group
# data : a data frame
# varname : the name of a column containing the variable to be summariezed
# groupnames : vector of column names to be used as grouping variables
# FROM http://www.sthda.com/english/wiki/ggplot2-error-bars-quick-start-guide-r-software-and-data-visualization
library(Hmisc) #this messes up dplyr in a big way
data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm = TRUE),
      sd = sd(x[[col]], na.rm = TRUE))
  }
  data_sum <- ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}
detach("package:Hmisc", unload = TRUE)
library(dplyr)

(AllAbund <- ggplot(currdepthabund, aes(x = CurrDepth, y = TransectAbund, fill = CurrDepth)) +
    theme_cowplot() +
    theme(legend.position = "none",
          strip.text.x = element_text(face = "italic")) +
    labs(y = "Abundance", x = "Current Depth Category") +
    facet_wrap(~ Latin, ncol = 3, scales = "free") +
    geom_jitter(width = 0.1) + 
    stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), 
                 geom = "errorbar", color = "red", width = 0.2) +
    stat_summary(fun = mean, geom = "point", color = "red") + 
    scale_fill_manual(values = c("red", "red4", "blue", "blue4"), name = "Current/Depth Category"))
#
#### CurrDepth Species Abundances Stats ####
#These stats are DIFFERENT than those in the MS
Ah <- currdepthabund %>% 
  filter(Latin == "Artedius harringtoni")
#These stats are calculated using all replicates.
Ahstats <- Ah %>%
  group_by(CurrDepth) %>% 
  summarize(MeanAbund = round(mean(TransectAbund), digits = 2),
            SDAbund = sd(TransectAbund))
#Stats avg over replicates first
Ahstats_bySiteDepth <- Ah %>% 
  group_by(CurrDepth, SiteDepth) %>% 
  summarize(MeanAbund = round(mean(TransectAbund), digits = 2)) %>% 
  group_by(CurrDepth) %>% 
  summarize(CurrDepthMeanAbund = round(mean(MeanAbund), digits = 2),
            CurrDepthSDAbund = sd(MeanAbund))

kruskal.test(TransectAbund ~ CurrDepth, data = Ah) #p > 0.001, test stat = 23.324
pairwise.wilcox.test(Ah$TransectAbund, Ah$CurrDepth) #high15 sig diff from other 3

##STATS BELOW HAVE NOT BEEN UPDATED
Hd <- fishsum %>% 
  filter(Latin == "Hexagrammos decagrammus")
Hdstats <- Hd %>%
  group_by(CurrDepth) %>% 
  summarize(MeanAbund = mean(Count),
            SDAbund = sd(Count))
kruskal.test(Count ~ CurrDepth, data = Hd) #p = 0.229, test stat = 3.6731

Jz <- fishsum %>% 
  filter(Latin == "Jordania zonope")
Jzstats <- Jz %>%
  group_by(CurrDepth) %>% 
  summarize(MeanAbund = mean(Count),
            SDAbund = sd(Count))
kruskal.test(Count ~ CurrDepth, data = Jz) #p < 0.001, test stat = 16.662
pairwise.wilcox.test(Jz$Count, Jz$CurrDepth,
                     p.adjust.method = "none") #high15 and High3, High 3 and Low 15/Low3, Low 15 and Low 3

Rn <- fishsum %>% 
  filter(Latin == "Rhinogobiops nicholsii")
Rnstats <- Rn %>%
  group_by(CurrDepth) %>% 
  summarize(MeanAbund = mean(Count),
            SDAbund = sd(Count))
kruskal.test(Count ~ CurrDepth, data = Rn) #p < 0.001, test stat = 25.634
pairwise.wilcox.test(Rn$Count, Rn$CurrDepth,
                     p.adjust.method = "none") #high15 and Low15/Low3, High 3 and Low15/Low3, Low 3 and Low 15

Sc <- fishsum %>% 
  filter(Latin == "Sebastes caurinus")
Scstats <- Sc %>%
  group_by(CurrDepth) %>% 
  summarize(MeanAbund = mean(Count),
            SDAbund = sd(Count))
kruskal.test(Count ~ CurrDepth, data = Sc) #p = 0.4717, test stat = 2.5201
#
#### CurrDepth Species Lengths Figures ####
plotfishdata <- data %>% 
  filter(Latin %in% specieslist)

(AllLength <- ggplot(plotfishdata, aes(x = CurrDepth, y = Length, fill = CurrDepth)) +
    theme_cowplot() +
    theme(legend.position = "none",
          strip.text.x = element_text(face = "italic")) +
    labs(y = "Length (cm)", x = "") +
    facet_wrap(~Latin, ncol = 3, scales = "free") +
    geom_jitter(width = 0.1) + 
    stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), 
                 geom = "errorbar", color = "red", width = 0.2) +
    stat_summary(fun = mean, geom = "point", color = "red"))


#800*700
#
#### CurrDepth Species Lengths Stats ####
Ah <- data %>% 
  filter(Latin == "Artedius harringtoni")
#Stats NOT averaged over replicates - Stats here are DIFFERENT from those in the MS
Ahstats <- Ah %>%
  group_by(CurrDepth) %>% 
  summarize(MeanAbund = mean(Length),
            SDAbund = sd(Length))
#Stats averaged over SiteDepth (aka: replicates) first
Ahstats_bySiteDepth <- Ah %>% 
  group_by(CurrDepth, SiteDepth) %>% 
  summarize(MeanLength = mean(Length)) %>% 
  group_by(CurrDepth) %>% 
  summarize(CurrDepthMeanLength = round(mean(MeanLength), digits = 2),
            CurrDepthSDLength = sd(MeanLength))

kruskal.test(Length ~ CurrDepth, data = Ah) #p = 0.4196, test stat = 2.8237
#pairwise.wilcox.test(Ah$Length, Ah$CurrDepth)

##STATS BELOW HAVE NOT BEEN UPDATED
sp <- fishdata %>% 
  filter(Latin == "Hexagrammos decagrammus")
stats <- sp %>%
  group_by(CurrDepth) %>% 
  summarize(MeanAbund = mean(Length),
            SDAbund = sd(Length))
kruskal.test(Length ~ CurrDepth, data = sp) #p = 0.0045, test stat = 13.047
pairwise.wilcox.test(sp$Length, sp$CurrDepth,
                     p.adjust.method = "none") #high15 and other 3, high 3 and low 15

sp <- fishdata %>% 
  filter(Latin == "Jordania zonope")
stats <- sp %>%
  group_by(CurrDepth) %>% 
  summarize(MeanAbund = mean(Length),
            SDAbund = sd(Length))
kruskal.test(Length ~ CurrDepth, data = sp) #p = 0.09975, test stat = 6.257

sp <- fishdata %>% 
  filter(Latin == "Rhinogobiops nicholsii")
stats <- sp %>%
  group_by(CurrDepth) %>% 
  summarize(MeanAbund = mean(Length),
            SDAbund = sd(Length))
kruskal.test(Length ~ CurrDepth, data = sp) #p = 0.002, test stat = 15.322
pairwise.wilcox.test(sp$Length, sp$CurrDepth,
                     p.adjust.method = "none") # high 15 & high 3, low 15; low 15 and low 3

sp <- fishdata %>% 
  filter(Latin == "Sebastes caurinus")
stats <- sp %>%
  group_by(CurrDepth) %>% 
  summarize(MeanAbund = mean(Length),
            SDAbund = sd(Length))
kruskal.test(Length ~ CurrDepth, data = sp) #p < 0.001, test stat = 26.322
pairwise.wilcox.test(sp$Length, sp$CurrDepth,
                     p.adjust.method = "none") #high15 & low3; high 3 & low15, low3; low15 & low3