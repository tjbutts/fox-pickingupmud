#DataAnalysis_Fox.R Darlings# 

# Averaging fox data by rep ========================
# Site 1 & 2 
fox_sec1_zp = fox_zp %>%
  filter(site == 1 | site == 2) %>% # select the sites that fit into sec1 
  group_by(date, site, year, period) %>%
  summarise(ug_total = mean(ug_total), # Average by rep
            ug_Bos = mean(ug_Bos), 
            ug_Chyd = mean(ug_Chyd), 
            ug_Cerio = mean(ug_Cerio),
            ug_Daphnia = mean(ug_Daphnia),
            ug_Diaph = mean(ug_Diaph), 
            ug_Cyclo = mean(ug_Cyclo), 
            ug_Cala = mean(ug_Cala), 
            ug_Naup = mean(ug_Naup)) %>% 
  ungroup() %>%
  as_tibble()
fox_sec1_zp # Need to make data long before visualizing 

fox_sec1_long = fox_sec1_zp %>%
  pivot_longer(cols = !c(date, site, year, period), names_to = 'group', values_to = 'biomass_ugL') # make data long 
fox_sec1_long
