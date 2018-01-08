#pkgs <- c("haven", "tidyverse", "stringr", "glue", "dplyr", "foreign", "WriteXLS")
#install.packages(pkgs)


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
#Providencia, Las Condes, Santiago Centro
cono_or<- rm %>% filter(comuna==13123 | comuna==13114 |comuna==13101)
#keep one obs per household
hhcono_or<-cono_or %>% filter(o==1)

#Calculate statistics by year
#hhinc cono_or
hhincome_n_tot<- hhcono_or %>%
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

#Income categories
hhcono_or_inc <- hhcono_or %>% mutate(
  hhinc_adj = case_when(year=="2013" ~ ytoth*110.87/111.88,
                        year=="2015" ~ ytoth),
  hhinc_usd_year= hhinc_adj/631.16*12,
  inc_category= case_when(hhinc_usd_year<= 25000  ~ "1) up to 25,000",
                          hhinc_usd_year > 25000 & hhinc_usd_year <= 50000 ~ "2) 25,000 - 50,000",
                          hhinc_usd_year > 50000 & hhinc_usd_year <= 75000 ~ "3) 50,000 - 75,000",
                          hhinc_usd_year > 75000 & hhinc_usd_year <= 100000 ~ "4) 75,000 - 100,000",
                          hhinc_usd_year > 100000 & hhinc_usd_year <= 135000 ~ "5) 100,000 - 135,000",
                          hhinc_usd_year>= 135000  ~ "6) 135,000+")
)

inc_count_cono_or <-  hhcono_or_inc %>%  group_by(inc_category) %>% 
  summarise (
    num_hhw=sum(expc),
    num_hh= sum(o)
  )

inc_25_35_count_cono_or <- hhcono_or_inc %>% filter (edad>=25,
                                                 edad<=35) %>% 
  group_by(inc_category) %>% 
  summarise (
    num_hhw=sum(expc),
    num_hh= sum(o)
  )
  
#Export income category table
exp_path_inc<- glue("{data_dir}/clean/cono_or_income.xls") 
inc_count_cono_or %>%  WriteXLS(exp_path_inc)

#Export income category table
exp_path_2535<- glue("{data_dir}/clean/cono_or_income_25_35.xls") 
inc_25_35_count_cono_or %>%  WriteXLS(exp_path_2535)

#hhinc cono_or edad 25-35
hhincome_n_25_35<- hhcono_or %>% filter(edad>=25,
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
educ_cono_or<-  hhcono_or %>%  group_by(year, educ) %>% 
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
educ_sum_n<- educ_cono_or %>% group_by(year, high_ed) %>% 
  summarise(
    freq_ed=sum(freq),
    wtot=sum(tothh)
  ) 
#Educacion 25-35
educ_cono_or_25_35<-  hhcono_or %>%  filter(edad>=25,
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
educ_sum_n_25_35<- educ_cono_or_25_35 %>% group_by(year, high_ed) %>% 
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

#HH size cono_or
hh_size_cono_or<- hhcono_or %>% group_by(year) %>% 
  summarise(
    wmean_hhsize=weighted.mean(tot_per, expc),
    mean_hhsize=mean(tot_per),
    median_hhsize=median(tot_per)
  )
#HH size cono_or 25-35
hh_size_cono_or_25_35<- hhcono_or %>% filter(edad>=25,
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
hh_comp_cono_or<-cono_or %>% group_by(year, ecivil) %>% 
  summarise(
    freq=n(),
    weight=sum(expc)
  ) 