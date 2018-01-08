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
#deptos_02_16_units_m2.csv

#Import Data Sets
raw<- glue("{data_dir}/Permisos_edificacion.csv") %>% 
  read_delim(delim=";")



yearly<- raw %>% select(-MONTH) %>% group_by(YEAR) %>% 
  summarise(
    total=sum(TOTAL),
    deptos=sum(DEPARTAMENTOS),
    casas=sum(CASAS)
  ) %>% ungroup

yearly %>% WriteXLS(glue("{data_dir}/permisos_anual.xls"))

total_02_16 <- yearly %>% 
  summarise(
    total_0216=sum(total),
    deptos_0216=sum(deptos),
    casas_0216=sum(casas)
  )

