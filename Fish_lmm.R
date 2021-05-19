## Load packages ---------------------------------------------------------
library(ggplot2)
library(grid)
library(gridExtra)
library(tidyverse)
library(lme4)
library(nlme)
library(bbmle)
library(AICcmodavg)
library(here)
library(sjPlot)

## Set WD and read in data ------------------------------------------------------

setwd(here::here("Jill Campbell analysis/FishData"))
data<- read_csv("FishData_Jan11.csv")
current <-read_csv("CurrentByDay_Jan11.csv")

## FISH DATA prep ---------------------------------------------------------

#summarize by site and replicate (this code is from J. Campbell FishAnalysis Jan 11)

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
fish <- fishdata %>% 
  group_by(Site, Hi_Lo, TransectDepth, CurrDepth, Replicate, SiteID, TransectID) %>% 
  summarize(Richness = length(unique(Latin)),
            Abundance = length(Length),
            Biomass = round(sum(Weight), 2)) %>% 
  mutate(Biomass_kg = Biomass/1000)

## CURRENT DATA prep ------------------------------------------------------------------------------------------
#we are using median max current speed for the prelimnary modelling so that it matches her other analyses

current <- current %>% group_by(logger) %>%
  summarize(CurrentMean = mean(SpeedMean), CurrentMin = mean(SpeedMin), CurrentMax = mean(SpeedMax), MedianMax = median(SpeedMax), meanCurrentSD = sd(SpeedMean), maxCurrentSD = sd(SpeedMax)) #note I think in her final summary Jill used the SD from ALL of the data when you need to calculate error around the the current sample size

current <- current %>% rename(Site = logger) %>% arrange(desc(CurrentMean))

# compare mean vs max in quick plot, need to decide what to use

ggplot(current) + geom_point(aes(CurrentMean, CurrentMax, colour = Site), size = 3) +
  theme_bw()

# rename sites in abiotic to match fish
#abiotic <- abiotic %>% mutate(Site = replace(Site, (1:7), c("Burial", "Coffin","Dragon","Josef","McKenzie","Porlier","Snake")))

# merge abiotic with fish

data <- full_join(current, fish, by = "Site")

## LMM - BIOMASS MODELS (NOT FOR THESIS, STILL NEED FIXING) ---------------------------------------------------------
# using median max current speed, fitting a random effect for site

Null <- lme(Biomass ~ 1, 
            random = ~ 1 | Site, data = data)
Lm1 <-  lme(Biomass ~ TransectDepth, 
            random = ~ 1 | Site, data = data)
Lm2 <-  lme(Biomass ~ MedianMax,
            random = ~ 1 | Site, data = data)
Lm3  <- lme(Biomass ~ TransectDepth + MedianMax, 
            random = ~ 1 | Site, data = data)

names <- c("Null", "Depth", "Current", "Depth + Current")

AICctab(Null, Lm1, Lm2, Lm3, mnames=names, base=TRUE, weights=TRUE, logLik=TRUE)

## let's look at Lm3 in more detail and look at the diagnostics
# model summary
Lm3

# this doesn't look great, there is a very obvious cone shape 
plot(Lm3)

#another way to plot this with ggplot
ggplot(data.frame(biomass=predict(Lm3,type="link"),pearson=residuals(Lm3,type="pearson")),
       aes(x=biomass,y=pearson)) +
  geom_point() +
  theme_bw()

#qqnorm plot not great, not terrible, could try and add a variance structure or log transform biomass

qqnorm(residuals(Lm3))
## Biomass - Variance structure models --------------------------------------------------------------------------
#let's fit an exponential variance structure, these wouldn't converge without increasing the number of iterations so had to go in and set this manually (I think the default is 50?)

Null_var <- update(Null, weights = varExp(form = ~fitted(.)),
                   control= lmeControl(maxIter = 5000, opt = "optim", optimMethod = "BFGS"))

Lm1_var <- update(Lm1, weights = varExp(form = ~fitted(.)),
                  control= lmeControl(maxIter = 5000, opt = "optim", optimMethod = "BFGS"))

Lm2_var <- update(Lm2, weights = varExp(form = ~fitted(.)),
                  control= lmeControl(maxIter = 5000, opt = "optim", optimMethod = "BFGS"))

Lm3_var <- update(Lm3, weights = varExp(form = ~fitted(.)),
                  control= lmeControl(maxIter = 5000, opt = "optim", optimMethod = "BFGS"))

AICctab(Null_var, Lm1_var, Lm2_var, Lm3_var, mnames=names, base=TRUE, weights=TRUE, logLik=TRUE)

# reassess plots for Lm3_var

# model summary
Lm3_var

# this doesn't look great, there is a very obvious cone shape 
plot(Lm3_var)

#another way to plot this with ggplot
ggplot(data.frame(biomass=predict(Lm3_var,type="link"),pearson=residuals(Lm3_var,type="pearson")),
       aes(x=biomass,y=pearson)) +
  geom_point() +
  theme_bw()

#qqnorm plot not great, not terrible

qqnorm(residuals(Lm3_var))


sjPlot::plot_model(Lm3)

## Biomass - Log-transform biomass models -----------------------------------------------

Null.log <- lme(log10(Biomass) ~ 1, 
            random = ~ 1 | Site, data = data)
Lm1.log <-  lme(log10(Biomass) ~ TransectDepth, 
            random = ~ 1 | Site, data = data)
Lm2.log <-  lme(log10(Biomass) ~ MedianMax,
            random = ~ 1 | Site, data = data)
Lm3.log  <- lme(log10(Biomass) ~ TransectDepth + MedianMax, 
            random = ~ 1 | Site, data = data)

names <- c("Null", "Depth", "Current", "Depth + Current")

AICctab(Null.log, Lm1.log, Lm2.log, Lm3.log, mnames=names, base=TRUE, weights=TRUE, logLik=TRUE)

## LMM - ABUNDANCE MODELS (FOR THESIS) ---------------------------------

# hmm these don't actually look that bad haha, Lm1 is top which is not surprising as there is a ton of variation with depth in abundance

Null.ab <- lme(Abundance ~ 1, 
            random = ~ 1 | Site, data = data)
Lm1.ab <-  lme(Abundance ~ TransectDepth, 
            random = ~ 1 | Site, data = data)
Lm2.ab <-  lme(Abundance ~ MedianMax,
            random = ~ 1 | Site, data = data)
Lm3.ab  <- lme(Abundance ~ TransectDepth + MedianMax, 
            random = ~ 1 | Site, data = data)

AICctab(Null.ab, Lm1.ab, Lm2.ab, Lm3.ab, mnames=names, base=TRUE, weights=TRUE, logLik=TRUE)

# diagnostics for Lm1.ab

Lm1.ab

plot(Lm1.ab) # decent

ggplot(data.frame(biomass=predict(Lm1.ab,type="link"),pearson=residuals(Lm1.ab,type="pearson")),
       aes(x=biomass,y=pearson)) +
  geom_point() +
  theme_bw()

qqnorm(resid(Lm1.ab))#decent

sjPlot::plot_model(Lm1.ab)

#quick and dirty plot of predicted mean fish abundance with CIs -----------------------------------------------

depth.means <- c(9.196, 10.832) #from Lm1.ab
depth.sd <- c(3.327, 0.289) #from Lm1.ab
depth <- data.frame(depth.means, depth.sd)
depth$lowerCI <- depth$depth.means - 1.96*depth$depth.sd
depth$upperCI <- depth$depth.means + 1.96*depth$depth.sd
depth$treatment <- c("3 m", " 15 m")

plot(NA, xlim = c(0,2.5), ylim = c(0,16), xlab = "", xaxt = "n", ylab="")
# We'll add a x-axis labelling our variables:
axis(1, 1:2, depth$treatment, las = 1)

# Then we'll draw our slopes as points (`pch` tells us what type of point):
points(depth$depth.means, pch = 21, cex = 1.5, col = "black", bg = "black")
#mtext( c("(32)","(36)","(58)","(29)", "(56)", "(57)"), side = 4, at=1:6, line = -1.2, cex = 0.6, las = 2)

# Then we'll add thick line segments for each 95 CI:
segments(seq(1:2), depth$lowerCI, seq(1:2), depth$upperCI, col = "black", lwd = 2)
mtext("Fish Abundance", 2, line=2.3)

