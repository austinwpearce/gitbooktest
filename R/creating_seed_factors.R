
# reading files and summing up sources to create seed factors

library(tidyverse)
library(here)

my_files <- list.files(path = here("DREAM5/output/split"),
                          pattern = "\\_by_categories.csv$",
                          full.names = TRUE)

data_csv <- my_files %>% 
  set_names() %>% 
  map_dfr(.f = read_delim,
          delim = ",",
          .id = "file_name")

# energy use

seed_eu <- data_csv %>% 
  filter(!str_detect(file_name, "Cotton|Alfalfa|Sugar")) %>% 
  filter(metric == "Energy Use") %>% 
  filter(basis == "kg crop") %>% 
  filter(str_detect(field_name, "seed_factor")) %>% 
  #filter(source_category != "Energy use associated with production of seed") %>% 
  group_by(file_name) %>% 
  summarize(my_mj_sum = sum(MJ)) %>% 
  mutate(file_name = str_replace_all(file_name, 
                                     "C:/Users/EricCoronel/Documents/GitHub/workspace-to-iterate/DREAM5/output/split/", ""))

write.table(seed_eu, file = "clipboard", sep = ",")

# alfalfa
alfalfa_eu_mj_hectare <- data_csv %>% 
  filter(str_detect(field_name, "seed_factor")) %>% 
  filter(str_detect(file_name, "Alfalfa")) %>% 
  filter(metric == "Energy Use") %>% 
  filter(basis == "hectare") %>%
  #filter(source_category != "Energy use associated with production of seed") %>% 
  summarize(my_mj = sum(MJ))
  
alfalfa_seed_assumption <- 2242 # kg/ha

alfalfa_eu_mj_hectare %>% pull(my_mj) / alfalfa_seed_assumption

# sugar beets

sugarbeets_eu_mj_hectare <- data_csv %>% 
  filter(str_detect(field_name, "seed_factor")) %>% 
  filter(str_detect(file_name, "Sugar")) %>% 
  filter(metric == "Energy Use") %>% 
  filter(basis == "hectare") %>%
  #filter(source_category != "Energy use associated with production of seed") %>% 
  summarize(my_mj = sum(MJ))

sugarbeet_seed_assumption <- 3000 *1.121 # 3000 lb/acre to kg/ha

sugarbeets_eu_mj_hectare %>% pull(my_mj) / sugarbeet_seed_assumption

# cotton

cotton_eu_mj_hectare <- data_csv %>% 
  filter(str_detect(field_name, "seed_factor")) %>% 
  filter(str_detect(file_name, "Cotton")) %>% 
  filter(metric == "Energy Use") %>% 
  filter(basis == "hectare") %>%
  #filter(source_category != "Energy use associated with production of seed") %>% 
  summarize(my_mj = sum(MJ))

cotton_seed_assumption <- 500 # kg/ha of cottonseed

(cotton_eu_mj_hectare %>% pull(my_mj)*0.13) / cotton_seed_assumption # 13% allocation

# notes
# cotton value is multiplied by 0.13, seed allocation of 13%. I need to calculate it per acre and then do the 0.13 allocation, same as alfalfa and sugar beets.
# Alfalfa seed yields generally average from 150-200 pounds per acre. Need to use data per hectare and divide it the seed yield.
# 42978.34801 mj/ha divided by 2000 lb/acre =  2242 kg/ha =  19.16965
#  Seed yields for sugarbeet seed crops range from 2000-3000 lb/acre. Do the same as alfalfa.
# https://nishat2013.wordpress.com/wp-content/uploads/2013/11/sugarbeet-seed-production.pdf

# ghg emissions - raw gas

seed_ghg <- data_csv %>% 
  mutate(file_name = str_replace_all(file_name, 
  "C:/Users/EricCoronel/Documents/GitHub/workspace-to-iterate/DREAM5/output/split/", "")) %>% 
  filter(metric == "GHG Emissions") %>% 
  filter(basis == "kg crop") %>% 
  filter(units == "kg") %>% 
  filter(str_detect(field_name, "seed_factor")) %>% 
  #filter(source_category != "GHG emissions associated with production of seed") %>%
  filter(source_category != "Soil carbon stock changes") %>% 
  group_by(file_name) %>% 
  summarise(across(CO2_fossil:SF6, ~ sum(.x))) %>% 
  ungroup() %>% 
  mutate(CO2_fossil = CO2_fossil + CO2_biogenic) %>% 
  select(-CO2_biogenic) %>% 
  filter(!str_detect(file_name, "Cotton|Alfalfa|Sugar"))

write.table(seed_ghg, file = "clipboard", sep = ",")

# alfalfa

alfalfa_seed_assumption <- 2242 # kg/ha

alfalfa_ghg_hectare <- data_csv %>% 
  filter(str_detect(field_name, "seed_factor")) %>% 
  filter(str_detect(file_name, "Alfalfa")) %>% 
  filter(metric == "GHG Emissions") %>% 
  filter(basis == "hectare") %>%
  filter(units == "kg") %>% 
  #filter(source_category != "Energy use associated with production of seed") %>% 
  filter(source_category != "Soil carbon stock changes") %>% 
  summarise(across(CO2_fossil:SF6, ~ sum(.x))) %>% 
  mutate(CO2_fossil = CO2_fossil + CO2_biogenic) %>% 
  select(-CO2_biogenic) %>% 
  summarise(across(CO2_fossil:SF6, ~ (.x / alfalfa_seed_assumption)))

# sugar beets

sugarbeet_seed_assumption <- 3000 *1.121 # 3000 lb/acre to kg/ha

sugarbeet_ghg_hectare <- data_csv %>% 
  filter(str_detect(field_name, "seed_factor")) %>% 
  filter(str_detect(file_name, "Sugar")) %>% 
  filter(metric == "GHG Emissions") %>% 
  filter(basis == "hectare") %>%
  filter(units == "kg") %>% 
  #filter(source_category != "Energy use associated with production of seed") %>% 
  filter(source_category != "Soil carbon stock changes") %>% 
  summarise(across(CO2_fossil:SF6, ~ sum(.x))) %>% 
  mutate(CO2_fossil = CO2_fossil + CO2_biogenic) %>% 
  select(-CO2_biogenic) %>% 
  summarise(across(CO2_fossil:SF6, ~ (.x / sugarbeet_seed_assumption)))

# cotton

cotton_seed_assumption <- 500 # kg/ha of cottonseed

cotton_ghg_hectare <- data_csv %>% 
  filter(str_detect(field_name, "seed_factor")) %>% 
  filter(str_detect(file_name, "Cotton")) %>% 
  filter(metric == "GHG Emissions") %>% 
  filter(basis == "hectare") %>%
  filter(units == "kg") %>% 
  #filter(source_category != "Energy use associated with production of seed") %>% 
  filter(source_category != "Soil carbon stock changes") %>% 
  summarise(across(CO2_fossil:SF6, ~ sum(.x))) %>% 
  mutate(CO2_fossil = CO2_fossil + CO2_biogenic) %>% 
  select(-CO2_biogenic) %>% 
  summarise(across(CO2_fossil:SF6, ~ ((.x * 0.13) / cotton_seed_assumption)))
