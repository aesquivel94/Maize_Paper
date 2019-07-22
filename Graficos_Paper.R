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
  # filter(row_number() == 1) %>% 
  unnest() %>% 
  dplyr::select(year, variety, zone, ciclo, month, id, date, HWAM)
  


cv_Data <- rayos %>% # nest(-zone, -ciclo, -variety, -year)
  group_by(zone, ciclo, variety, id) %>% 
  summarise(HWAM_sd = sd(HWAM), M_HWAM = mean(HWAM)) %>% 
  mutate(HWAM_cv = round(HWAM_sd/M_HWAM, 3)* 100 ) %>% 
  ungroup() %>%
  group_by(zone, ciclo, id) %>% 
  summarise(cv_mean = mean(HWAM_cv)) 



ggplot(cv_Data, aes(id, y = cv_mean, colour = ciclo)) + 
  geom_line() + 
  geom_point() +
  scale_x_continuous(breaks = seq(from = 1, to = 12, by = 1)) +
  scale_colour_manual(values = c("#000080", "#008080")) +
  facet_grid(. ~ zone,  labeller = labeller(zone = as_labeller(c("CERETE" = 'Cereté', "ESPINAL" = 'Espinal',  "LA_UNION" = 'La Unión')))) +
  ylim(0, 100) +
  theme_bw() + 
  labs(x = 'Sowing date', y = 'Coefficient of variation (%)', colour = 'Planting\n cycle')


