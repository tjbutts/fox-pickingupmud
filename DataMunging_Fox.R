# Fox River Data Visualization #

library(tidyverse)
library(magrittr)
library(lubridate)
library(forcats)

fox <- read.csv('foxriverdat_clean.csv')
as_tibble(fox) 

fox_clean <- fox %>% 
  mutate(date = mdy(date)) %>% 
  mutate(site = as_factor(site), year = as_factor(year), treatment = as_factor(treatment), period = as_factor(period)) %>%
  as_tibble() # change character date row to date 
fox_clean

# ========= FOX COLUMN HEADER METADATA ========= #
# sampleid = unique sample identifier containing river, year, and site information
##F = Fox River; YY = Year; 1-3 = Sampling Period; 001-006 = Site; 1-3 = replicate (e.g., F1310013 = Fox River, 2013, Period 1, Site 1, 3rd replicate)
# date = calender date 
# site = site 1-6 along the Fox River 
# year = calendar year (2013 - 2016) 
# period = early (1), middle(2), or late(3) summer 
# treatment = dred (site near active dredging), prevd (site previously dredged), and down (downstream of active dredging)
# rep = sample replicate 
# chl = chl-a (ug/L) derived from fluorometry 
# tss = total suspended solids (mg/L)
# tp = total phosphorus (ug/L) - molybdenum blue method + digestion
# tn = total nitrogen (mg/L) - lab method (look up)
# srp = soluble reactive phosphorus - molybdenum blue method
# surf_do = dissolved oxygen concentration (mg/L) taken at 0.25 m 
# secchi = secchi depth (m)
# pH = pH taken at 0.25 m 
# discharge = discharge (ft3/s) near site 6 on the Lower Fox River
# ug_total = total zooplankton biomass (ug/L) - Kissman Lab SOP; vertical tow
# miv_abund = macroinvertebrate abundance (D-Net shuffle method) 
# miv_hbi = hilsenhoff biotic index using macroinvertebrate family information 
# zp_eH = Effective Shannon-Weiner Diversity of zooplankton describing the effective number of species controlling for evenness and richness 
# ug_Bos = biomass of Bosmina (ug/L)
# ug_Chyd = biomass of Chydorus (ug/L)
# ug_Cerio = biomass of Ceriodaphnia (ug/L)
# ug_Daph = biomass of Daphnia (ug/L)
# ug_Diaph = biomass of Diaphanosoma (ug/L)
# ug_Cyclo = biomass of Cyclopoida (ug/L)
# ug_Cala = biomass of Calanoida (ug/L)
# ug_Naup = biomass of Nauplii (ug/L)


# ================= FOX UNIVARIATE DATA ANALYSIS ================== # 
# Does Sediment Dredging have detrimental affects on Fox River Zooplankton or Macroinvertebrates ? # 

# Select univariate dataset
fox_uni <- fox_clean %>% select(sampleid, date, site, year, period, 
                                treatment, chl, tss, tp, tn, srp, surf_do, 
                                secchi, pH, ug_total, miv_abund, miv_hbi, zp_eH, discharge) %>% as_tibble()
fox_uni

# Select zoop multivariate dataset 
fox_multi <- fox_clean %>% select(-c(chl, tss, tp, tn, srp, surf_do, 
                                     secchi, pH, ug_total, miv_abund, miv_hbi, zp_eH, discharge)) %>% as_tibble()
fox_multi 

# Select environmental dataset 
fox_env <- fox_clean %>% select(sampleid, date, site, year, period, treatment, chl, tss, tp, tn, srp, surf_do, secchi, pH, discharge) %>% as_tibble()
fox_env

# Zooplankton Biomass #=====================================

library(lme4)
library(car)
library(multcomp)

hist(fox_uni$ug_total) # very right-skewed
hist(log(fox_uni$ug_total+1)) #better

totb.model <- lm(log(ug_total+1) ~ site + year + period + treatment, data = fox_uni)

totb.model
Anova(totb.model) #o

confint(totb.model)
summary(totb.model)
plot(totb.model)
qqnorm(resid(totb.model))
qqline(resid(totb.model))

fox_uni$treatment <- factor(fox_uni$treatment, levels = c('prevd', 'dred', 'down'))
boxplot(fox_uni$ug_total~fox_uni$treatment) # 5/28/2015 - Large Daphnia bloom early in season contributing to prevd outliers 
boxplot(fox_uni$ug_total~fox_uni$year)
boxplot(fox_uni$ug_total~fox_uni$treatment + fox_uni$year)

# moving window below the outlier Daphnia bloom in early 2015 
boxplot(fox_uni$ug_total~fox_uni$treatment, ylim = c(0,200)) 
boxplot(fox_uni$ug_total~fox_uni$year, ylim = c(0,200))
boxplot(fox_uni$ug_total~fox_uni$treatment + fox_uni$year, ylim = c(0,200))

## Detach Package Script ## 

detach_package <- function(pkg, character.only = FALSE)
{
  if(!character.only)
  {
    pkg <- deparse(substitute(pkg))
  }
  search_item <- paste("package", pkg, sep = ":")
  while(search_item %in% search())
  {
    detach(search_item, unload = TRUE, character.only = TRUE)
  }
}


# remove masking packages 
detach_package('lme4')
detach_package('car')
detach_package('multcomp')

# detach packages 
detach_package(lme4)
detach_package(car)
detach_package(multcomp)

# Multiple linear model code #===================================
# Fox Bio 
hist(fox_clean$ug_total)
hist(log(fox_clean$ug_total+1)) # better 

hist(fox_clean$miv_abund)
hist(log(fox_clean$miv_abund+1)) # better

fox_clean$log_ug_total <- log(fox_clean$ug_total+1)
fox_clean$log_mivabund <- log(fox_clean$miv_abund+1)

foxbio.model <- lm(cbind(log_ug_total, log_mivabund) ~ site + year + period + treatment, data = fox_clean)
summary(foxbio.model)
head(resid(foxbio.model))
head(fitted(foxbio.model))
coef(foxbio.model)
sigma(foxbio.model)

vcov(foxbio.model)

library(car)
Anova(foxbio.model)
foxbio.model2 <- update(foxbio.model, . ~ . -treatment)
anova(foxbio.model, foxbio.model2) # model without treatment and site fits just as well as the one with them

# Total Zooplankton Biomass - Interesting 
library(multcomp)
zpbio.model <- lm(log_ug_total ~ site + year + period, data = fox_clean)
summary(zpbio.model)
plot(zpbio.model)
qqnorm(resid(zpbio.model))
qqline(resid(zpbio.model))

totalzpbio_year <- summary(glht(zpbio.model, linfct=mcp(year='Tukey')))
summary(totalzpbio_year, tes=adjusted('bonferroni')) # significant
totalzpbio_site <- summary(glht(zpbio.model, linfct=mcp(site='Tukey')))
summary(totalzpbio_site, tes=adjusted('bonferroni')) # no significance 
totalzpbio_period <- summary(glht(zpbio.model, linfct=mcp(period='Tukey')))
summary(totalzpbio_period, tes=adjusted('bonferroni')) # no significance 

# Boxplot - Treatment & Year; no significant site, season effect 
fox_clean$treatment <- factor(fox_clean$treatment, levels = c('prevd', 'dred', 'down'))
boxplot(fox_clean$ug_total~fox_clean$treatment, ylab = 'Total Biomass', xlab = 'Classification')
boxplot(ug_total~treatment*year, data = fox_clean, 
        ylab = ' zooplankton biomass')

