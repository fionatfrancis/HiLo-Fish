#### Load Libraries ####
library(lubridate)
library(dplyr)
library(tidyr)
library(ggplot2)
library(cowplot)
library(grid)
library(gridExtra)
library(coin)

Hisites <- c("Josef", "Burial", "Porlier", "Grainger")
Losites <- c("Coffin", "Snake", "Dragon", "McKenzie", "Anniversary", "Georgina")

#### Compile Current Data from TCM files ####
setwd(here::here("6 Data/AbioticData/TCM")) #for current and temp loggers

# Read current data files
current.list <- list.files(pattern = "_CR.txt$")
current <- NULL
for (i in current.list) {
  tmp <- read.table(i, header = T, sep = ",")
  tmp$logger <- strsplit(i, split = "_")[[1]][1]
  current <- rbind(current, tmp)
}

# Separate ISO.8601.Time column into Date and Time columns
current <- tidyr::separate(current, ISO.8601.Time, into = c("Date", "Time"), sep = "T")

current <- current %>% 
  mutate(Date = as_date(Date)) %>% 
  filter(Date > "2019-12-15",
         Date < "2020-01-26")

# Summarize current data by day and logger
Current_byday <- group_by(current, logger, Date) %>%
  summarize(SpeedMin = min(Speed..cm.s.),
            SpeedMean = mean(Speed..cm.s.),
            SpeedMax = max(Speed..cm.s.),
            SpeedSD = sd(Speed..cm.s.)) %>% 
  mutate(Hi_Lo = case_when(logger %in% Hisites ~ "High",
                           logger %in% Losites ~ "Low"))

write.csv(Current_byday, here::here("6 Data/AbioticData", "CurrentByDay.csv"), row.names = FALSE)
#
#### Compile Temperature Data from TCM files ####
setwd(here::here("6 Data/AbioticData/TCM")) #for current and temp loggers

# Read temperature data files
temp.list <- list.files(pattern = "_T.txt$")
temperature <- NULL
for (i in temp.list) {
  tmp <- read.table(i, header = T, sep = ",")
  tmp$logger <- strsplit(i, split = "_")[[1]][1]
  temperature <- rbind(temperature, tmp)
}

# Separate ISO.8601.Time column into Date and Time columns
temperature <- tidyr::separate(temperature, ISO.8601.Time, into = c("Date", "Time"), sep = "T") %>% 
  mutate(Date = as_date(Date)) %>% 
  filter(Date > "2019-12-15",
         Date < "2020-01-26")

# Summarize temperature data by day and logger
Temperature_byday <- group_by(temperature, logger, Date) %>%
  summarize(TempMin = min(Temperature..C.),
            TempMean = mean(Temperature..C.), 
            TempMedian = median(Temperature..C.),
            TempMax = max(Temperature..C.),
            TempSD = sd(Temperature..C.))

write.csv(Temperature_byday, here::here("6 Data/AbioticData", "TemperatureByDay.csv"), row.names = FALSE)
#
#### Compile Salinity Data from SO files ####
setwd(here::here("6 Data/AbioticData/StarOddi"))

# Read salinity data files
sal.list <- list.files(pattern = "_SO.csv$")
salinity <- NULL
for (i in sal.list) {
  tmp <- read.csv(i, header = T, sep = ",")
  tmp$logger <- strsplit(i, split = "_")[[1]][1]
  salinity <- rbind(salinity, tmp)}

# Separate ISO.8601.Time column into Date and Time columns
salinity <- tidyr::separate(salinity, Date...Time, into = c("Date", "Time"), sep = " ") %>% 
  mutate(Date = as_date(Date)) %>% 
  filter(Date > "2019-12-15",
         Date < "2020-01-26")

# Summarize temperature data by day and logger
Salinity_byday <- group_by(salinity, logger, Date) %>%
  summarize(SalinityMin = min(Salinity.psu.),
            SalinityMean = mean(Salinity.psu.), 
            SalinityMedian = median(Salinity.psu.),
            SalinityMax = max(Salinity.psu.),
            SalinitySD = sd(Salinity.psu.))

write.csv(Salinity_byday, here::here("6 Data/AbioticData", "SalinityByDay.csv"), row.names = FALSE)
#
#### Join current, temp, salinity data into one data frame ####
site_levels_Hulq <- c("Skthak\n(Porlier Pass)", "Xwéxi7es\n(Grainger Point)", "Burial Islet", "Xwkáthelhp\n(Josef Point)", "Skthóko7lh\n(Georgina Point)", "Anniversary Island", "Xw7élhki7em\n(Snake Island)", "Dragon Point", "Kw'ayówes\n(Coffin Point)", "McKenzie Bight")

Abiotic_byday <- full_join(Current_byday, Temperature_byday, by = c("logger", "Date")) %>% 
  full_join(Salinity_byday, by = c("logger", "Date")) %>% 
  mutate(Site = case_when(logger == "Snake" ~ "Xw7élhki7em\n(Snake Island)",
                          logger == "Dragon" ~ "Dragon Point",
                          logger == "Josef" ~ "Xwkáthelhp\n(Josef Point)",
                          logger == "Coffin" ~ "Kw'ayówes\n(Coffin Point)",
                          logger == "Porlier" ~ "Skthak\n(Porlier Pass)",
                          logger == "Burial" ~ "Burial Islet",
                          logger == "Georgina" ~ "Skthóko7lh\n(Georgina Point)",
                          logger == "Grainger" ~ "Xwéxi7es\n(Grainger Point)",
                          logger == "Anniversary" ~ "Anniversary Island",
                          logger == "McKenzie" ~ "McKenzie Bight"),
         Hi_Lo = case_when(logger %in% Hisites ~ "High",
                           logger %in% Losites ~ "Low"),
         Hi_Lo = factor(Hi_Lo, levels = c("High", "Low"), ordered = TRUE),
         Site = factor(Site, levels = site_levels_Hulq))

write.csv(Abiotic_byday, here::here("6 Data/AbioticData", "AbioticByDay.csv"), row.names = FALSE)

Abiotic_byday <- read.csv(here::here("6 Data/AbioticData", "AbioticByDay.csv"), header = TRUE) %>% 
  mutate(Site = factor(Site, levels = site_levels_Hulq),
         Hi_Lo = factor(Hi_Lo))
#
#### Current Data Exploration ####
# Hi vs Lo Daily Max Speeds - box plot
ggplot(Current_byday, aes(x = Hi_Lo, y = SpeedMax)) +
  geom_boxplot() +
  labs(y = "Speed (cm/sec)", x = "Current Category", title = "Daily max current speeds by current category") +
  scale_x_discrete(labels = c("Low\n(< 50 cm/sec mean daily max)", "High\n(> 50 cm/sec mean daily max)")) +
  theme(axis.text.x = element_text(colour = "black")) +
  theme_classic()

# Hi Daily Max Speed - histogram
HiHist <- Abiotic_byday %>% 
  filter(Hi_Lo == "High")

ggplot(HiHist) + 
  theme_cowplot() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.9, hjust = 0.9)) +
  geom_histogram(aes(x = SpeedMax), binwidth = 0.25, col = "black", fill = "red") +
  facet_wrap(~ logger) +
  scale_x_continuous(breaks = seq(110, 120, by = 1), limits = c(110, 120)) +
  scale_y_continuous(breaks = seq(0, 10, by = 2)) +
  labs(y = "Number of Days", x = "Daily Max Speed (cm/sec)")

#Fake left-skewed histogram
fakedata <- c(120,120,120,120,120,120,120,120,120,120,120,120,120,120,120,120,120,120,119,119,119,119,119,119,118,118,118,118,118,117,117,117)
hist(fakedata, 
     breaks = seq(115, 120, by = 1),
     ylim = c(0, 20),
     col = "red",
     xlab = "Daily Max Speed (cm/sec)",
     ylab = "Number of Days",
     main = "")

ggplot(fakedata) + 
  theme_cowplot() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.9, hjust = 0.9)) +
  geom_histogram(aes(x = SpeedMax), binwidth = 1, col = "black", fill = "red") +
  scale_x_continuous(breaks = seq(110, 120, by = 1), limits = c(110, 120)) +
  scale_y_continuous(breaks = seq(0, 10, by = 2)) +
  labs(y = "Number of Days", x = "Daily Max Speed (cm/sec)")

# datetime v. current speed
ggplot(current, aes(x = Date, y = Speed..cm.s., colour = logger)) +
  geom_path() +
  facet_wrap(~ logger, ncol = 1) +
  theme_bw()

# current speed box plot- using all data
ggplot(current, aes(x = logger, y = Speed..cm.s.)) +
  geom_boxplot() +
  theme_bw()

#Hi vs Lo min mean max
hilostats <- Current_byday %>% 
    group_by(Hi_Lo) %>% 
  summarise(min = min(SpeedMax),
            mean = mean(SpeedMax),
            max = max(SpeedMax))

#Percentage of time near max speed
timehigh <- current %>% 
  group_by(logger) %>% 
  summarize(minutes = sum(Speed..cm.s. > 115),
            percentage = minutes/(60*24*41)*100)

Abiotic_byday %>% 
  group_by(logger) %>% 
  summarize(days = sum(SpeedMax > 118),
            percentage = days/41*100)

#### Current Inflection Point and Figures ####
Current_byday_sort <- Abiotic_byday %>% 
  arrange(desc(SpeedMax))
Current_byday_sort$row <- seq_along(Current_byday_sort[[1]])

library(inflection)
cc <- check_curve(Current_byday_sort$SpeedMax, Current_byday_sort$row)
ipbese <- bese(Current_byday_sort$SpeedMax, Current_byday_sort$row ,cc$index)
ipbese$iplast
#max inflection point 72.945
#mean inflection point 3.801 - only McKenzie would be a Lo current site if SpeedMean is used

# All raw data with inflection point line - visualize what the above function calculated

ggplot(Current_byday_sort, aes(y = SpeedMax, x = row)) +
  theme_cowplot() +
  geom_point() +
  labs(y = "Daily Max Current Speed (cm/sec)", x = "Row Number") +
  geom_hline(yintercept = 72.945, lty = "dashed", col = "red") +
  geom_text(label = "72.94 cm/sec", x = 390, y = 80, col = "red") +
  scale_y_continuous(limits = c(0, 120), breaks = seq(0, 120, by = 20))

# Site SpeedMax stats and box plot with inflection line
Current_byday %>% 
  group_by(logger) %>% 
  summarize(MaxMean = mean(SpeedMax),
            MaxSD = sd(SpeedMax),
            MeanMean = mean(SpeedMean),
            MeanSD = sd(SpeedMean))

#Site SpeedMax figure
site_levels <- rev(c("McKenzie", "Coffin", "Dragon", "Snake", "Anniversary", "Georgina", "Josef", "Burial", "Grainger", "Porlier"))
Current_byday$logger <- factor(Current_byday$logger, levels = site_levels)
site_levels_Hulq <- c("Skthak\n(Porlier Pass)", "Xwéxi7es\n(Grainger Point)", "Burial Islet", "Xwkáthelhp\n(Josef Point)", "Skthóko7lh\n(Georgina Point)", "Anniversary Island", "Xw7élhki7em\n(Snake Island)", "Dragon Point", "Kw'ayówes\n(Coffin Point)", "McKenzie Bight")
Current_byday$Site <- factor(Current_byday$Site, levels = site_levels_Hulq)

ggplot(Current_byday, aes(x = Site, y = SpeedMax, fill = Hi_Lo)) +
  theme_cowplot() +
  theme(axis.text.x = element_text(angle = 30, vjust = 0.95, hjust = 0.9),
        legend.position = "bottom") +
  geom_boxplot() +
  scale_y_continuous(breaks = seq(0, 120, by = 20)) +
  scale_fill_manual(values = c("red", "blue")) +
  labs(y = "Daily Max Current Speed (cm/sec)", x = "Site", fill = "Site Current Speed Category") +
  geom_hline(yintercept = 72.9, lty = "dashed", col = "red")

# current speed box plot with daily Means
SiteMean <- Current_byday %>% 
  group_by(logger) %>% 
  summarise(MeanCurrent = mean(SpeedMean))
  
mean_site_rank <- c("Porlier", "Burial", "Grainger", "Josef", "Georgina", "Snake", "Dragon", "Coffin", "Anniversary", "McKenzie")
Current_byday$logger <- factor(Current_byday$logger, levels = mean_site_rank)

ggplot(Current_byday, aes(x = logger, y = SpeedMean, fill = Hi_Lo)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 30, vjust = 0.95, hjust = 0.9),
        panel.grid = element_line(linetype = "blank"),
        axis.text = element_text(colour = "black", size = 15),
        axis.title = element_text(size = 17),
        legend.position = "bottom") +
  geom_boxplot() +
  scale_y_continuous(breaks = seq(0, 120, by = 20)) +
  scale_fill_manual(values = c("red", "blue")) +
  labs(y = "Daily Mean Current Speed (cm/sec)", x = "Site", fill = "Current Category") +#, title = "Daily max current speeds by site") +
  geom_hline(yintercept = 13, lty = "dashed", col = "red")

#### Current Stats ####
#Current_byday <- read.csv(here::here("6 Data/AbioticData/CurrentByDay.csv"), header = TRUE)
currstats <- Current_byday %>% 
  group_by(Hi_Lo) %>% 
  summarize(SpeedMedian = median(SpeedMax),
            SpeedSD = sd(SpeedMax))
wilcox.test(data = Current_byday, SpeedMax ~ Hi_Lo)

# At High Sites, how many minutes each day is the current > 73 cm/sec?
timespenthigh <- current %>% 
  group_by(logger, Date) %>% 
  summarize(TimePerDay = sum(Speed..cm.s. > 73)) %>% 
  mutate(TimeInHours = TimePerDay/60)

totaltime <- current %>% 
  group_by(logger) %>% 
  summarize(min = min(Speed..cm.s.),
            max = max(Speed..cm.s.),
            totalminutes = sum(Speed..cm.s. > 119),
            percentageoftime = totalminutes/(60*24*41)*100)

timestats <- timespenthigh %>% 
  group_by(logger) %>% 
  summarize(HoursMin = min(TimeInHours),
            HoursMean = mean(TimeInHours),
            HoursMax = max(TimeInHours))

site_levels <- c(rev("McKenzie", "Coffin", "Dragon", "Snake", "Anniversary", "Georgina", "Josef", "Grainger", "Burial", "Porlier"))
timespenthigh$logger <- factor(timespenthigh$logger, levels = rev(site_levels))
timespenthigh <- timespenthigh %>% 
  mutate(Hi_Lo = case_when(logger %in% Hisites ~ "High",
                           logger %in% Losites ~ "Low")) 
ggplot(timespenthigh) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  geom_boxplot(aes(x = logger, y = TimeInHours, fill = Hi_Lo)) +
  labs(y = "Hours Per Day When\nCurrent is > 73 cm/sec", x = "Site") +
  scale_y_continuous(breaks = seq(0, 16, by = 2)) +
  scale_fill_manual(values = c("red", "blue"))

#### Temp Stats ####
Hi <- Abiotic_byday %>% 
  filter(Hi_Lo == "High")
Lo <- Abiotic_byday %>% 
  filter(Hi_Lo == "Low")

#TempMean - non-parametric stats
ggplot(Abiotic_byday, aes(x = TempMean, colour = Hi_Lo)) +
  geom_density()
qqnorm(Hi$TempMean)
qqline(Hi$TempMean)
qqnorm(Lo$TempMean)
qqline(Lo$TempMean)
shapiro.test(Hi$TempMean) #sig diff from normal
shapiro.test(Lo$TempMean) #sig diff from normal
var.test(TempMean ~ Hi_Lo, Abiotic_byday, alternative = "two.sided") #sig diff in variances


#t.test(TempMean ~ Hi_Lo, data = Abiotic_byday)
wilcox_test(TempMean ~ Hi_Lo, data = Abiotic_byday) #Z = 4.0043, p-value = 6.221e-05, Hi median 8.84, Lo median 8.96

Abiotic_byday %>% 
  group_by(Hi_Lo) %>% 
  summarise(n = length(TempMean),
            TempMedian = median(TempMean),
            TempMean = mean(TempMean),
            SD = sd(TempMedian))


Temperature_bydayNoM <- Temperature_byday %>% 
  filter(logger != "McKenzie")

wilcox.test(TempMedian ~ Hi_Lo, data = Temperature_bydayNoM)

tempstats <- Abiotic_byday %>% 
  group_by(Hi_Lo) %>% 
  summarize(Median = median(TempMean),
            SD = sd(TempMean))

#### Temp Figures ####
Temperature_byday$logger <- factor(Temperature_byday$logger, levels = rev(site_levels))
Abiotic_byday <- Abiotic_byday %>% 
  mutate(Hi_Lo = factor(Hi_Lo),
         Site = factor(Site, levels = site_levels_Hulq))

(TempSite <- ggplot(data = Abiotic_byday, aes(x = Site, y = TempMean, fill = Hi_Lo)) +
    theme_cowplot() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
          legend.position = "none") +
    geom_jitter(width = 0.1, col = "black") + 
    #stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), 
    #             geom = "errorbar", width = 0.4) +
    #stat_summary(fun = mean, geom = "point") +
    stat_summary(fun = median, geom = "point", size = 4, shape = 23) +
    labs(x = "Site", y = "Mean Daily Temperature (°C)") +
    #scale_colour_manual(values = c("red", "blue")) +
    scale_y_continuous(limits = c(7, 10.5), breaks = seq(7, 10.5, by = 0.5)) +
    scale_fill_manual(values = c("red", "blue")))
#900*550
#### Fraser River Stats ####
FRtemp <- Abiotic_byday %>% 
  filter(logger %in% c("Porlier", "Josef", "Georgina", "Grainger", "Anniversary"))
t.test(TempMean ~ Hi_Lo, data = FRtemp) #Z = 4.3619, p-value = 1.289e-05
t.test(SalinityMean ~ Hi_Lo, data = FRtemp)

FRtemp %>% 
  group_by(Hi_Lo) %>% 
  na.omit() %>% 
  summarize(n = length(TempMean),
            Mean = mean(TempMean),
            SD = sd(TempMean),
            nSal = length(SalinityMean),
            MeanSal = mean(SalinityMean),
            SDSal = sd(SalinityMean))

#### Salinity Stats ####
ggplot(Abiotic_byday, aes(x = SalinityMean, colour = Hi_Lo)) +
  geom_density()
qqnorm(Hi$SalinityMean)
qqline(Hi$SalinityMean)
qqnorm(Lo$SalinityMean)
qqline(Lo$SalinityMean)
shapiro.test(Hi$SalinityMean) #sig diff from normal
shapiro.test(Lo$SalinityMean) #sig diff from normal
var.test(SalinityMean ~ Hi_Lo, Abiotic_byday, alternative = "two.sided") #sig diff in variances

t.test(SalinityMean ~ Hi_Lo, data = Abiotic_byday)

Abiotic_byday %>% 
  group_by(Hi_Lo) %>% 
  na.omit() %>% 
  summarize(n = length(SalinityMean),
            median = median(SalinityMean),
            mean = mean(SalinityMean),
            sd = sd(SalinityMean))
Hi <- Hi %>% 
  na.omit()
median(Hi$SalinityMean)
Lo <- Lo %>% 
  na.omit()
median(Lo$SalinityMean)
wilcox_test(SalinityMean ~ Hi_Lo, data = Abiotic_byday) #Z = 8.5025, p-value < 2.2e-16

Salinity_bydayNoD <- Salinity_byday %>% 
  filter(logger != "Dragon")

wilcox.test(SalinityMedian ~ Hi_Lo, data = Salinity_bydayNoD)

Salinitystats <- Abiotic_byday %>% 
  na.omit() %>% 
  group_by(Hi_Lo) %>% 
  summarize(Median = median(SalinityMean),
            SD = sd(SalinityMean))

#### Salinity Figures ####
(SalinitySite <- ggplot(data = Abiotic_byday, aes(x = Site, y = SalinityMean, fill = Hi_Lo)) +
    theme_cowplot() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
          legend.position = "bottom") +
   geom_jitter(width = 0.1, col = "black") + 
   #stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), 
   #             geom = "errorbar", width = 0.4) +
   #stat_summary(fun = mean, geom = "point") +
   stat_summary(fun = median, geom = "point", shape = 23, size = 4) +
   labs(x = "Site", y = "Mean Daily Salinity (psu)", colour = "Site Current Speed Category") +
   #scale_colour_manual(values = c("red", "blue")) +
   scale_y_continuous(limits = c(21.5, 30), breaks = seq(22, 30, by = 2)) +
   scale_fill_manual(values = c("red", "blue")))
#900*550

#### Smash Temp and Sal figs together ####
get_legend <- function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

mylegend <- get_legend(SalinitySite)
SalinitySite <- SalinitySite + theme(legend.position = "none")

(TempSalinitySite <- grid.arrange(TempSite, SalinitySite, mylegend, 
                                  ncol = 1, nrow = 3, heights = c(1, 1, 0.1)))

#900*1100
#### Temperature Figures - old ####

# datetime v. temp 
ggplot(temperature, aes(x = Date, y = Temperature..C., colour = logger)) +
  geom_path() +
  facet_wrap(~ logger, ncol = 1) +
  theme_bw()

# temp box plot
ggplot(temperature, aes(x = logger, y = Temperature..C.)) +
  geom_boxplot() +
  theme_bw()

#### Temperature Stats - old ####

Res.anova2 <- aov(TempMedian ~ logger, data = Temperature_byday)
summary(Res.anova2)
plot(Res.anova2,1) # check homogeneity of variance assumption
TukeyHSD(Res.anova2, which = "logger")  # which sites are different

temp_byday_noM <- Temperature_byday %>% 
  filter(logger != "McKenzie")

Res.anovaNoM <- aov(TempMedian ~ logger, data = temp_byday_noM)
summary(Res.anovaNoM)
plot(Res.anovaNoM,1) # check homogeneity of variance assumption
TukeyHSD(Res.anovaNoM, which = "logger")  # which sites are different (Burial and Josef, J and A)


# current category temp box plot with daily means
ggplot(Temperature_byday2, aes(x = Hi_Lo, y = TempMean)) +
  geom_boxplot() +
  labs(y = "Temp (°C)", x = "Current Category", title = "Daily mean temperature by current category") +
  scale_x_discrete(labels = c("Low\n(< 50 cm/sec mean daily max)", "High\n(> 50 cm/sec mean daily max)")) +
  theme(axis.text.x = element_text(colour = "black")) +
  theme_classic()

ggplot(Temperature_byday2) + 
  geom_histogram(aes(x = TempMean)) +
  facet_wrap(~ Hi_Lo)

wilcox.test(TempMean ~ Hi_Lo, data = Temperature_byday)

Temperature_byday2$logger <- as.character(Temperature_byday2$logger)

#### Salinity Figures - old ####

# datetime v. salinty 
ggplot(salinity, aes(x = Date, y = Salinity.psu., colour = logger)) +
  geom_path() +
  facet_wrap(~ logger, ncol = 1) +
  theme_bw()

# salinity box plot
ggplot(salinity, aes(x = logger, y = Salinity.psu.)) +
  geom_boxplot() +
  theme_bw()

#### Salinity Stats - old ####
Res.anova3 <- aov(SalinityMedian ~ logger, data = Salinity_byday)
summary(Res.anova3)
plot(Res.anova3,1) # check homogeneity of variance assumption
TukeyHSD(Res.anova3, which = "logger")  # which sites are different

ggplot(Salinity_byday2, aes(x = Hi_Lo, y = SalinityMean)) +
  geom_boxplot() +
  labs(y = "Salinity (psu)", x = "Current Category", title = "Daily mean salinity by current category") +
  scale_x_discrete(labels = c("Low\n(< 50 cm/sec mean daily max)", "High\n(> 50 cm/sec mean daily max)")) +
  theme(axis.text.x = element_text(colour = "black")) +
  theme_classic()

ggplot(Salinity_byday2) + 
  geom_histogram(aes(x = SalinityMean)) +
  facet_wrap(~ Hi_Lo)

SalinityCategory <- wilcox.test(SalinityMedian ~ Hi_Lo, data = Salinity_byday)

FRsal <- Abiotic_byday %>% 
  filter(logger %in% c("Porlier", "Georgina", "Grainger", "Anniversary"))
wilcox_test(SalinityMean ~ Hi_Lo, data = FRsal) #Z = 9.1132, p-value < 2.2e-16

FRSalinitystats <- FRsal %>% 
  group_by(Hi_Lo) %>% 
  summarize(Mean = median(SalinityMean),
            SD = sd(SalinityMean))


#### Old line plots of each site, min mean max ####

ggplot(Current_byday, aes(x = Date, y = SpeedMean)) +
  geom_line() +
  geom_line(aes(x = Date, y = SpeedMin), col = "blue") +
  geom_line(aes(x = Date, y = SpeedMax), col = "red") +
  labs(title = "Daily min, mean, and max current speed by site", y = "Speed (cm/sec)") +
  facet_wrap(~ logger, ncol = 4) + 
  theme_bw()

ggplot(Temperature_byday, aes(x = Date, y = TempMean)) +
  geom_line() +
  geom_line(aes(x = Date, y = TempMin), col = "blue") +
  geom_line(aes(x = Date, y = TempMax), col = "red") +
  labs(title = "Daily min, mean, and max temperature by site", y = expression(paste("Temperature (",degree,"C)"))) +
  facet_wrap(~ logger, ncol = 4) + 
  theme_bw()

ggplot(Salinity_byday, aes(x = Date, y = SalinityMean)) +
  geom_line() +
  geom_line(aes(x = Date, y = SalinityMin), col = "blue") +
  geom_line(aes(x = Date, y = SalinityMax), col = "red") +
  labs(title = "Daily min, mean, and max salinity by site", y = "Salinity (psu)") +
  facet_wrap(~ logger, ncol = 4) + 
  theme_bw()
