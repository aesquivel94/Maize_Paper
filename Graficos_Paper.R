rm(list = ls()); gc(reset = TRUE)
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Made by:     Alejandra Esquivel Arias. 
# Created in:  Date: 7-2019.
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# graphs Leo's paper.

library(tidyverse)
library(lubridate)
library(glue)
library(cowsay)



Historicos <- read_csv("Datos_Julio/Historicos_1980-2013.csv") %>% dplyr::select(-X1)

rayos <- Historicos %>%
  dplyr::select(RUNNO,TNAM.....................,  pd, year, HWAM,  variety, zone) %>% 
  mutate(zone = replace(zone, zone == 'LA UNION', 'LA_UNION')) %>%
  mutate(pd = as.Date(pd,"%m/%d/%Y")) %>%
  separate(TNAM....................., c('month', 'order_date'), sep = '/') %>%
  mutate(month = str_remove(month, 'P') %>% as.numeric(), 
         order_date = as.numeric(order_date)) %>% 
  mutate(ciclo = case_when(
    zone == 'CERETE' & month < 9 ~  'A',
    zone == 'CERETE' & month >= 9 ~  'B',
    zone == 'LA_UNION' & month < 10 ~ 'A',
    zone == 'LA_UNION' & month >= 10 ~ 'B',
    zone == 'ESPINAL' & month < 10 ~ 'A',
    zone == 'ESPINAL' & month >= 10 ~ 'B')) %>% 
  nest(-zone, -ciclo,  -variety, -year) %>% 
  mutate(data_mod = purrr::map(.x = data, .f = function(x){ x %>% dplyr::select(-RUNNO, -pd) %>% mutate(date = glue::glue('{month}-{order_date}'), id = 1:12)})) %>%
  dplyr::select(-data) %>% 
  filter(row_number() == 1) %>% 
  unnest() %>% 
  dplyr::select(year, variety, zone, ciclo, month, id, date, HWAM)
  


rayos



  
