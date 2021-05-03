# Fox River Data Exploration # 
# Purpose of this script is to explore variability of zooplankton biomass by replicate, site, and year from 2013-2016 
# Script by Tyler J Butts: Last updated 4-23-2021
rm(list=ls())

#set wd to wherever "foxriverdat_clean.csv" is # 
setwd("C:/Users/Owner/Box/Butts_Scripts/Fox River Project/fox-pickingupmud") 

# load libraries # 
library(tidyverse)

# load data # 
fox_dat = read_csv('foxriverdat_clean.csv') # Compiled data collected from the Fox River 2013 - 2016 

# Select data specific to zooplankton biomass 
fox_zp = fox_dat %>%
  select(!c(chl, tss, tp, tn,srp,surf_do,secchi,pH,discharge, miv_abund, miv_hbi, zp_eH))
fox_zp

# assess variability between reps separated by sampling day and site =======================
# NOTE: Unsure of the best way to assess variability between reps here?  
fox_repanalysis = fox_zp %>%
  select(sampleid, date, site, year, period, rep, ug_total) %>%
  group_by(sampleid, date, site, year, period) %>%
  summarise(range_rep = range(ug_total), 
            sd_rep = sd(ug_total), # didn't work for some reason 
            sd_var = sd_rep^2) %>% # also didn't work
  ungroup() %>%
  as_tibble()
fox_repanalysis = as.data.frame(fox_repanalysis)
fox_repanalysis$obsv = 1:nrow(fox_repanalysis) # Make a column of ascending numbers
plot(range_rep~obsv, data = fox_repanalysis) # A few data points have large ranges between replicates, might be useful to look at log
plot(log(range_rep+1)~obsv, data = fox_repanalysis, yaxt='n')
axis(side=2,
     at=c(log(1),
          log(2),log(3),log(4),log(5),log(6),log(7),log(8),log(9),log(10),
          log(20),log(30),log(40),log(50),log(60),log(70),log(80),log(90),log(100),
          log(200), log(300), log(400), log(500), log(600), log(700), log(800), log(900), log(1000)), #Where the tick marks should be drawn
     labels = c('1', '', '','','','','','','','10','','','','','','','','','100', '', '', '','','','','','','1000'), las=2, cex.axis=1)

# Will average across rep for further analyses - range of reps is mostly within 10, but there's some extreme outliers that get up to the 100 ug range.
# While visualizing with boxplot will keep reps 

# Variation in zooplankton biomass per 'dredging history' ================================================
# sec1 = Site 1 & 2 = Actively Dredged in 2013 then Previously Dredged following three years 
# sec2 = Site 3 & 4 = Actively Dredged in 2013-2014 then Previously Dredged following two years 
# sec3 = Site 5 = Downstream of Dredging 2013-2014 then Actively Dredged following two years 
# sec4 = Site 6 = Downstream 2013-2015 then Actively Dredged final year 

# Need to separate full dataset into these different sections then visualize via boxplots 
# Legend for periods: 1=early summer, 2=mid-summer, 3=late summer 
windows(height=5, width=5)
par(mai=c(0.9,1,0.6,1))
plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
legend("center", legend=c('early summer', 'mid-summer' ,'late summer'), 
       pch = 15, pt.cex=3, cex=1.5, bty='n', col = cols)

# Site 1 & 2 = Dredged in 2013 then previously dredged 2014-2016
fox_sec1_zp = fox_zp %>%
  filter(site == 1 | site == 2) %>%  # select the sites that fit into sec1 
  select(!c(sampleid, treatment))
fox_sec1_zp
fox_sec1_long = fox_sec1_zp %>%
  pivot_longer(cols = !c(date, site, year, period,rep), names_to = 'group', values_to = 'biomass_ugL') %>% # make data long 
  arrange(year)
fox_sec1_long

# Just look at total biomass for now 
fox_sec1_tot = fox_sec1_long %>%
  filter(group == 'ug_total')

# Visualize data - collapses site differences, just looks at dredging and temporal aspects
windows(height= 5, width = 8)
par(mai=c(0.9,1,0.6,1))
cols <- c('#33A6B2', '#9AC836', '#91C46C')
boxplot(log(biomass_ugL+1)~period + year, data = fox_sec1_tot,
        col = cols, yaxt = 'n', xaxs=F, at = c(1:3, 5:7, 9:11, 13:15),
        names = c('','2013','','','2014','','','2015','','','2016',''), 
        ylab = '', xlab='', ylim=c(log(1),log(1000)))
mtext('ln(zooplankton biomass)',side =2, line=3, cex=1.5)
mtext('Year', side=1, line=3, cex=1.5)
mtext('Dredged 2013 -> Previously Dredged 2014-2016', side=3, line=1.5, cex=1.5)
axis(side=2,
     at=c(log(1),
          log(2),log(3),log(4),log(5),log(6),log(7),log(8),log(9),log(10),
          log(20),log(30),log(40),log(50),log(60),log(70),log(80),log(90),log(100),
          log(200), log(300), log(400), log(500), log(600), log(700), log(800), log(900), log(1000)), #Where the tick marks should be drawn
     labels = c('1', '', '','','','','','','','10','','','','','','','','','100', '', '', '','','','','','','1000'), las=2, cex.axis=1)
abline(v = 4, lty = 2, lwd=2, col='gray60') # Denotes change in dredging status 

# Site 3 & 4 - Dredged 2013-2014 and previosuly dredged 2015-2016
fox_sec2_zp = fox_zp %>%
  filter(site == 3 | site == 4) %>%  # select the sites that fit into sec1 
  select(!c(sampleid, treatment))
fox_sec2_zp
fox_sec2_long = fox_sec2_zp %>%
  pivot_longer(cols = !c(date, site, year, period,rep), names_to = 'group', values_to = 'biomass_ugL') %>% # make data long 
  arrange(year)
fox_sec2_long

# Just look at total biomass for now 
fox_sec2_tot = fox_sec2_long %>%
  filter(group == 'ug_total')

# Visualize data - collapses site differences, just looks at dredging and temporal aspects
windows(height= 5, width = 8)
par(mai=c(0.9,1,0.6,1))
cols <- c('#33A6B2', '#9AC836', '#91C46C')
boxplot(log(biomass_ugL+1)~period + year, data = fox_sec2_tot,
        col = cols, yaxt = 'n', xaxs=F, at = c(1:3, 5:7, 9:11, 13:15),
        names = c('','2013','','','2014','','','2015','','','2016',''), 
        ylab = '', xlab='', ylim=c(log(1),log(1000)))
mtext('ln(zooplankton biomass)',side =2, line=3, cex=1.5)
mtext('Year', side=1, line=3, cex=1.5)
mtext('Dredged 2013-2014 -> Previously Dredged 2015-2016', side=3, line=1.5, cex=1.5)
axis(side=2,
     at=c(log(1),
          log(2),log(3),log(4),log(5),log(6),log(7),log(8),log(9),log(10),
          log(20),log(30),log(40),log(50),log(60),log(70),log(80),log(90),log(100),
          log(200), log(300), log(400), log(500), log(600), log(700), log(800), log(900), log(1000)), #Where the tick marks should be drawn
     labels = c('1', '', '','','','','','','','10','','','','','','','','','100', '', '', '','','','','','','1000'), las=2, cex.axis=1)
abline(v = 8, lty = 2, lwd=2, col='gray60') # Denotes change in dredging status 

# Site 5 - Downstream of Dredging 2013-2014 then Actively Dredged following two years 
fox_sec3_zp = fox_zp %>%
  filter(site == 5) %>%  # select the sites that fit into sec1 
  select(!c(sampleid, treatment))
fox_sec3_zp
fox_sec3_long = fox_sec3_zp %>%
  pivot_longer(cols = !c(date, site, year, period,rep), names_to = 'group', values_to = 'biomass_ugL') %>% # make data long 
  arrange(year)
fox_sec3_long

# Just look at total biomass for now 
fox_sec3_tot = fox_sec3_long %>%
  filter(group == 'ug_total')

# Visualize data - collapses site differences, just looks at dredging and temporal aspects
windows(height= 5, width = 8)
par(mai=c(0.9,1,0.6,1))
cols <- c('#33A6B2', '#9AC836', '#91C46C')
boxplot(log(biomass_ugL+1)~period + year, data = fox_sec3_tot,
        col = cols, yaxt = 'n', xaxs=F, at = c(1:3, 5:7, 9:11, 13:15),
        names = c('','2013','','','2014','','','2015','','','2016',''), 
        ylab = '', xlab='', ylim=c(log(1),log(1000)))
mtext('ln(zooplankton biomass)',side =2, line=3, cex=1.5)
mtext('Year', side=1, line=3, cex=1.5)
mtext('Downstream 2013-2014 -> Dredged 2015-2016', side=3, line=1.5, cex=1.5)
axis(side=2,
     at=c(log(1),
          log(2),log(3),log(4),log(5),log(6),log(7),log(8),log(9),log(10),
          log(20),log(30),log(40),log(50),log(60),log(70),log(80),log(90),log(100),
          log(200), log(300), log(400), log(500), log(600), log(700), log(800), log(900), log(1000)), #Where the tick marks should be drawn
     labels = c('1', '', '','','','','','','','10','','','','','','','','','100', '', '', '','','','','','','1000'), las=2, cex.axis=1)
abline(v = 8, lty = 2, lwd=2, col='gray60') # Denotes shift in dredging status

# Site 6 - Downstream 2013-2015 then Actively Dredged final year 
fox_sec4_zp = fox_zp %>%
  filter(site == 6) %>%  # select the sites that fit into sec1 
  select(!c(sampleid, treatment))
fox_sec4_zp
fox_sec4_long = fox_sec4_zp %>%
  pivot_longer(cols = !c(date, site, year, period,rep), names_to = 'group', values_to = 'biomass_ugL') %>% # make data long 
  arrange(year)
fox_sec4_long

# Just look at total biomass for now 
fox_sec4_tot = fox_sec4_long %>%
  filter(group == 'ug_total')

# Visualize data - collapses site differences, just looks at dredging and temporal aspects
windows(height= 5, width = 8)
par(mai=c(0.9,1,0.6,1))
cols <- c('#33A6B2', '#9AC836', '#91C46C')
boxplot(log(biomass_ugL+1)~period + year, data = fox_sec4_tot,
        col = cols, yaxt = 'n', xaxs=F, at = c(1:3, 5:7, 9:11, 13:15),
        names = c('','2013','','','2014','','','2015','','','2016',''), 
        ylab = '', xlab='', ylim=c(log(1),log(1000)))
mtext('ln(zooplankton biomass)',side =2, line=3, cex=1.5)
mtext('Year', side=1, line=3, cex=1.5)
mtext('Downstream 2013-2015 -> Dredged 2016', side=3, line=1.5, cex=1.5)
axis(side=2,
     at=c(log(1),
          log(2),log(3),log(4),log(5),log(6),log(7),log(8),log(9),log(10),
          log(20),log(30),log(40),log(50),log(60),log(70),log(80),log(90),log(100),
          log(200), log(300), log(400), log(500), log(600), log(700), log(800), log(900), log(1000)), #Where the tick marks should be drawn
     labels = c('1', '', '','','','','','','','10','','','','','','','','','100', '', '', '','','','','','','1000'), las=2, cex.axis=1)
abline(v = 12, lty = 2, lwd=2, col='gray60') # Denotes shift in Dredging status






