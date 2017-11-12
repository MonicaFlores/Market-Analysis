pkgs <- c("haven", "tidyverse", "stringr", "glue", "dplyr", "foreign", "WriteXLS")
install.packages(pkgs)


library(tidyverse)
library(stringr)
library(glue)
library(dplyr)
library(haven)
library(foreign)
library(WriteXLS)

data_dir <- "/Users/MoniFlores/Desktop/casen" 

#Import Data Sets
raw_15<- glue("{data_dir}/2015/Casen 2015.sav") %>% 
  read_spss() %>% 
  select(folio, o, region, comuna, tot_per, edad, ecivil, expc, expr, ypch,
         ytoth, educ) %>% 
  mutate(year="2015",
         #Change ecivil for matching 2013, we make union civil equals conviviente
         ecivil=case_when(
           ecivil==1 ~ 1,
           ecivil==2 ~ 2,
           ecivil==3 ~ 2,
           ecivil==4 ~ 3,
           ecivil==5 ~ 4,
           ecivil==6 ~ 5,
           ecivil==7 ~ 6,
           ecivil==8 ~ 7,
           ecivil==9 ~ NA_real_
         ) )%>% 
  filter(region==13)

raw_13<- glue("{data_dir}/2013/CASEN_2013_MN_B_Principal.sav") %>% 
  read_spss() %>%  
  select(folio, o, region, comuna, numper, edad, ecivil, expc, expr, ypchtot,
         ymoneCorh, educ) %>% 
  mutate(year="2013",
         tot_per=numper,
         ytoth=ymoneCorh,
         ypch=ypchtot) %>% 
  filter(region==13) %>% 
  select(folio, o, region, comuna, tot_per, edad, ecivil, expc, expr, ypch,
         ytoth, educ, year)

rm<- raw_15 %>% bind_rows(raw_13)
hhrm<- rm %>% filter(o==1)
nunoa<- rm %>% filter(comuna==13120)
#keep one obs per household
hhnunoa<-nunoa %>% filter(o==1)

#Calculate statistics by year
#hhinc nunoa
hhincome_n_tot<- hhnunoa %>%
  group_by(year) %>% 
  summarise(
  mean_incw=weighted.mean(ytoth, expc),
  mean_inc=mean(ytoth),
  median_inc=median(ytoth),
  tothhw=sum(expc)
  ) %>% ungroup() %>% 
  mutate(
    #adjust values to 2015 pesos
    median_inc_adj = case_when(year=="2013" ~ median_inc*110.87/111.88,
                              year=="2015" ~ median_inc),
    mean_inc_adj = case_when(year=="2013" ~ mean_inc*110.87/111.88,
                            year=="2015" ~ mean_inc),
    wmean_inc_adj = case_when(year=="2013" ~ mean_incw*110.87/111.88,
                             year=="2015" ~ mean_incw),
    inc_usd_adj_year = wmean_inc_adj/631.16*12.
  )

#hhinc nunoa edad 25-35
hhincome_n_25_35<- hhnunoa %>% filter(edad>=25,
                                      edad<=35) %>%
  group_by(year) %>% 
  summarise(
    mean_incw=weighted.mean(ytoth, expc),
    mean_inc=mean(ytoth),
    median_inc=median(ytoth),
    tothhw=sum(expc)
  ) %>% ungroup() %>% 
  mutate(
    #adjust values to 2015 pesos
    median_inc_adj = case_when(year=="2013" ~ median_inc*110.87/111.88,
                               year=="2015" ~ median_inc),
    mean_inc_adj = case_when(year=="2013" ~ mean_inc*110.87/111.88,
                             year=="2015" ~ mean_inc),
    wmean_inc_adj = case_when(year=="2013" ~ mean_incw*110.87/111.88,
                              year=="2015" ~ mean_incw),
    inc_usd_adj_year = wmean_inc_adj/631.16*12
  )

#hhinc rm
hhincome_rm<- hhrm %>%
  group_by(year) %>% 
  summarise(
    mean_incw=weighted.mean(ytoth, expr),
    mean_inc=mean(ytoth),
    median_inc=median(ytoth),
    tothhw=sum(expr)
  ) %>% ungroup() %>% 
  mutate(
    #adjust values to 2015 pesos
    median_inc_adj = case_when(year=="2013" ~ median_inc*110.87/111.88,
                               year=="2015" ~ median_inc),
    mean_inc_adj = case_when(year=="2013" ~ mean_inc*110.87/111.88,
                             year=="2015" ~ mean_inc),
    wmean_inc_adj = case_when(year=="2013" ~ mean_incw*110.87/111.88,
                              year=="2015" ~ mean_incw),
    inc_usd_adj_year = wmean_inc_adj/631.16*12.
  )

#Educacion
educ_nunoa<-  hhnunoa %>%  group_by(year, educ) %>% 
  summarise(
    freq=n(),
    tothh=sum(expc)
  ) %>% ungroup %>% 
  mutate(
    high_ed= case_when(#educ==8 ~ 1, 
                       #educ==9 ~ 1, 
                       educ==10 ~ 1, 
                       educ==11 ~ 1, 
                       educ==12 ~ 1, 
                       TRUE ~ 0))
educ_sum_n<- educ_nunoa %>% group_by(year, high_ed) %>% 
  summarise(
    freq_ed=sum(freq),
    wtot=sum(tothh)
  ) 
#Educacion 25-35
educ_nunoa_25_35<-  hhnunoa %>%  filter(edad>=25,
                                  edad<=35) %>% 
  group_by(year, educ) %>% 
  summarise(
    freq=n(),
    tothh=sum(expc)
  ) %>% ungroup %>% 
  mutate(
    high_ed= case_when(#educ==8 ~ 1, 
      #educ==9 ~ 1, 
      educ==10 ~ 1, 
      educ==11 ~ 1, 
      educ==12 ~ 1, 
      TRUE ~ 0))
educ_sum_n_25_35<- educ_nunoa_25_35 %>% group_by(year, high_ed) %>% 
  summarise(
    freq_ed=sum(freq),
    wtot=sum(tothh)
  ) 

#Educacion RM
educ_rm<-  hhrm %>%  group_by(year, educ) %>% 
  summarise(
    freq=n(),
    tothh=sum(expr)
  ) %>% ungroup %>% 
  mutate(
    high_ed= case_when(#educ==8 ~ 1, 
      #educ==9 ~ 1, 
      educ==10 ~ 1, 
      educ==11 ~ 1, 
      educ==12 ~ 1, 
      TRUE ~ 0))
educ_sum_rm<- educ_rm %>% group_by(year, high_ed) %>% 
  summarise(
    freq_ed=sum(freq),
    wtot=sum(tothh)
  ) 

#HH size nunoa
hh_size_nunoa<- hhnunoa %>% group_by(year) %>% 
  summarise(
    wmean_hhsize=weighted.mean(tot_per, expc),
    mean_hhsize=mean(tot_per),
    median_hhsize=median(tot_per)
  )
#HH size nunoa 25-35
hh_size_nunoa_25_35<- hhnunoa %>% filter(edad>=25,
                                         edad<=35) %>% 
  group_by(year) %>% 
  summarise(
    wmean_hhsize=weighted.mean(tot_per, expc),
    mean_hhsize=mean(tot_per),
    median_hhsize=median(tot_per)
  )

#HHsize rm
hh_size_rm<- hhrm %>% group_by(year) %>% 
  summarise(
    wmean_hhsize=weighted.mean(tot_per, expr),
    mean_hhsize=mean(tot_per),
    median_hhsize=median(tot_per)
  )

#HHcomposition            
hh_comp_nunoa<-nunoa %>% group_by(year, ecivil) %>% 
  summarise(
    freq=n(),
    weight=sum(expc)
  ) 