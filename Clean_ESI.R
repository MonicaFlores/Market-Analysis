pkgs <- c("haven", "tidyverse", "stringr", "glue", "dplyr", "foreign", "WriteXLS")
install.packages(pkgs)


library(tidyverse)
library(stringr)
library(glue)
library(dplyr)
library(haven)
library(foreign)
library(WriteXLS)

data_dir <- "/Users/MoniFlores/Desktop/ESI" 
raw1_10<- glue("{data_dir}/raw/Base_Hogares_NESI_2010_Usuarios_Externos_Con_Becas.sav") %>% 
  read_spss() %>% select(ID_IDENTIFICACION, FACT_HOG, ANO_ENCUESTA, MESH, REGION_H, R_P_C, TOTPERH, GRUPO_EDAD,
                      CONYUGALJH, CINEJH, ING_TOTAL)
raw2_10<- glue("{data_dir}/raw/Base_Hogares_NESI_2010_Usuarios_Externos_Sin_Becas.sav") %>% 
  read_spss() %>% select(ID_IDENTIFICACION, FACT_HOG, ANO_ENCUESTA, MESH, REGION_H, R_P_C, TOTPERH, GRUPO_EDAD,
                    CONYUGALJH, CINEJH, ING_TOTAL)
raw1_16<- glue("{data_dir}/raw/ESI_2016_HOGARES_CON_BECAS_USUARIOSEXTERNOS.sav") %>% 
  read_spss() %>% select(ID_IDENTIFICACION, FACT_HOG, ANO_ENCUESTA, MESH, REGION_H, R_P_C, TOTPERH, GRUPO_EDAD,
                         CONYUGALJH, CINEJH, ING_TOTAL)
raw2_16<- glue("{data_dir}/raw/ESI_2016_HOGARES_SIN_BECAS_USUARIOSEXTERNOS.sav") %>% 
  read_spss() %>% select(ID_IDENTIFICACION, FACT_HOG, ANO_ENCUESTA, MESH, REGION_H, R_P_C, TOTPERH, GRUPO_EDAD,
                        CONYUGALJH, CINEJH, ING_TOTAL)

raw10<- raw1_10 %>% union(raw2_10)
raw16<- raw1_16 %>% union(raw2_16)
nunoa<- raw10 %>% union(raw16) %>% filter(R_P_C==13120)
rm<- raw10 %>% union(raw16) %>% filter(REGION_H==13)

income<- nunoa %>%  group_by(ANO_ENCUESTA) %>% 
  summarise(
  mean_incw=weighted.mean(ING_TOTAL, FACT_HOG),
  mean_inc=mean(ING_TOTAL),
  median_inc=median(ING_TOTAL)
  ) %>% ungroup() %>% 
  mutate(
    median_inc_adj = case_when(ANO_ENCUESTA=="2010" ~ median_inc*114.11/102.35,
                              ANO_ENCUESTA=="2016" ~ median_inc),
    mean_inc_adj = case_when(ANO_ENCUESTA=="2010" ~ mean_inc*114.11/102.35,
                            ANO_ENCUESTA=="2016" ~ mean_inc),
    wmean_inc_adj = case_when(ANO_ENCUESTA=="2010" ~ mean_incw*114.11/102.35,
                             ANO_ENCUESTA=="2016" ~ mean_incw),
    inc_usd_adj_year = wmean_inc_adj/631.16*12
  )

income_rm<- rm %>%  group_by(ANO_ENCUESTA) %>% 
  summarise(
    mean_incw=weighted.mean(ING_TOTAL, FACT_HOG),
    mean_inc=mean(ING_TOTAL),
    median_inc=median(ING_TOTAL)
  ) %>% ungroup() %>% 
  mutate(
    median_inc_adj = case_when(ANO_ENCUESTA=="2010" ~ median_inc*114.11/102.35,
                               ANO_ENCUESTA=="2016" ~ median_inc),
    mean_inc_adj = case_when(ANO_ENCUESTA=="2010" ~ mean_inc*114.11/102.35,
                             ANO_ENCUESTA=="2016" ~ mean_inc),
    wmean_inc_adj = case_when(ANO_ENCUESTA=="2010" ~ mean_incw*114.11/102.35,
                              ANO_ENCUESTA=="2016" ~ mean_incw),
    inc_usd_adj_year = wmean_inc_adj/631.16*12
  )

educ_nunoa<-  nunoa %>%  group_by(ANO_ENCUESTA, CINEJH) %>% 
  summarise(
    freq=n(),
    weight=sum(FACT_HOG)
  ) %>% ungroup %>% 
  mutate(
    w_freq=freq*weight,
    high_ed= case_when(CINEJH>=6 ~ 1,
                       TRUE ~ 0)
  )

educ_sum_n<- educ_nunoa %>% group_by(ANO_ENCUESTA, high_ed) %>% 
  summarise(
    freq_ed=sum(w_freq),
    wtot=sum(weight)
  ) %>% 
  mutate(
    wfreq_ed=freq_ed/wtot
  )

educ_sum_n%>% 
  WriteXLS (glue("{data_dir}/educ_sh_6.xls"))

hh_size_nunoa<- nunoa %>% group_by(ANO_ENCUESTA) %>% 
  summarise(
    wmean_hhsize=weighted.mean(TOTPERH, FACT_HOG),
    mean_hhsize=mean(TOTPERH),
    median_hhsize=median(TOTPERH)
  )

hh_size_rm<- rm %>% group_by(ANO_ENCUESTA) %>% 
  summarise(
    wmean_hhsize=weighted.mean(TOTPERH, FACT_HOG),
    mean_hhsize=mean(TOTPERH),
    median_hhsize=median(TOTPERH)
  )
            
hh_comp_nunoa<-nunoa %>% group_by(ANO_ENCUESTA, CONYUGALJH) %>% 
  summarise(
    freq=n(),
    weight=sum(FACT_HOG)
  ) %>% ungroup %>% 
  mutate(
  wfreq_ec=freq*weight)