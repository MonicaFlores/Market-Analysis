pkgs <- c("haven", "tidyverse", "stringr", "glue", "dplyr", "foreign", "WriteXLS", "lubridate")
install.packages(pkgs)


library(tidyverse)
library(stringr)
library(glue)
library(dplyr)
library(haven)
library(foreign)
library(WriteXLS)
library (lubridate)

data_dir <- "/Users/MoniFlores/Desktop/Supply/" 

#deptos_units_floors_per_building.csv
#deptos_02_16_units_m2.csv (overall units and surface per month)

#Import Dataset units and floors per building
un_fl_bldg<- glue("{data_dir}/deptos_units_floors_per_building.csv") %>% 
  read_delim(delim=";")

yearly_units <- un_fl_bldg %>% group_by(Year) %>%
  summarise(
    units_bldg_av = mean(cantidad_unidad),
    units_bldg_med = median(cantidad_unidad),
    floors_bldg_av = mean(num_pisos),
    floors_bldg_med = median(num_pisos)
  )

yearly_units %>% WriteXLS(glue("{data_dir}/clean/yearly_units.xls"))

#Import Dataset overall units and surface per month
un_m2<- glue("{data_dir}/deptos_02_16_units_m2.csv") %>% 
  read_delim(delim=";")

yearly_m2_unit <- un_m2 %>% group_by(YEAR) %>%
  summarise(
    deptos_year = sum(n_deptos),
    m2_year = sum(m2_deptos)
  ) %>% ungroup %>% 
  mutate (av_m2_unit=m2_year/deptos_year)

yearly_m2_unit %>% WriteXLS(glue("{data_dir}/clean/yearly_m2_unit.xls"))
