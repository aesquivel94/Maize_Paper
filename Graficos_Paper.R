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


# Datos historicos...
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
  scale_colour_manual(values = c("gray30", "#008080")) +
  facet_grid(. ~ zone,  labeller = labeller(zone = as_labeller(c("CERETE" = 'Cereté (Córdoba)', "ESPINAL" = 'El Espinal (Tolima)',  "LA_UNION" = 'La Unión (Valle del Cauca)')))) +
  ylim(0, 100) +
  theme_bw() + 
  labs(x = 'Planting date', y = 'Coefficient of variation (%)', colour = 'cycle')

ggsave("graphs/His_cv.pdf", width = 8, height = 4)
ggsave("graphs/His_cv.png", width = 8, height = 4)

# 

limites <- rayos %>% # nest(-zone, -ciclo, -variety, -year)
  group_by(zone, ciclo, variety, id) %>% 
  summarise(HWAM_sd = sd(HWAM), M_HWAM = mean(HWAM)) %>% 
  mutate(HWAM_cv = round(HWAM_sd/M_HWAM, 3)* 100 ) %>% 
  ungroup() %>%
  group_by(zone, ciclo, id) %>% 
  summarise(cv_mean = mean(HWAM_cv), cv_min = min((HWAM_cv)), cv_max = max(HWAM_cv)) 



ggplot(limites, aes(id, y = cv_mean, colour = ciclo)) + 
  geom_line()  + 
  geom_ribbon(aes(ymin=cv_min, ymax=cv_max , fill=ciclo), alpha=0.2) + 
  geom_point() +
  scale_x_continuous(breaks = seq(from = 1, to = 12, by = 1)) +
  scale_fill_manual( 'cycle' , values = c("gray30", "#008080")) +
  scale_colour_manual('cycle' , values = c("gray30", "#008080")) +
  facet_grid(. ~ zone,  labeller = labeller(zone = as_labeller(c("CERETE" = 'Cereté (Córdoba)', "ESPINAL" = 'El Espinal (Tolima)',  "LA_UNION" = 'La Unión (Valle del Cauca)')))) +
  ylim(0, 100) +
  theme_bw() + 
  labs(x = 'Planting date', y = 'Coefficient of variation (%)')

ggsave("graphs/His_Ic.pdf", width = 8, height = 4)
ggsave("graphs/His_Ic.png", width = 8, height = 4)



########################################################################################

Groc <- read_csv("Datos_Julio/Groc.csv") %>% 
  dplyr::select(-X1, -freq) %>%
  separate(TNAM....................., c('month', 'order_date'), sep = '/') %>%
  mutate(zone = replace(zone, zone == 'LA UNION', 'LA_UNION'), 
         month = str_remove(month, 'P') %>% as.numeric(), 
         order_date = as.numeric(order_date)) 


# Solo para verificacion...
Groc %>% drop_na() %>% nest(-zone, -ciclo,  -variety, -year) %>% 
  mutate(data_mod = purrr::map(.x = data, .f = nrow) ) %>% unnest(data_mod) %>% 
  dplyr::select(data_mod) %>% unique() # **** Por ahora todos los datos parecen completos. 


# ####### - ###### - ####### - ########

p <- Groc %>%
  # drop_na() %>% 
  nest(-zone, -ciclo,  -variety, -year) %>% 
  mutate(data_mod = purrr::map(.x = data, .f =  function(x){ x %>% mutate(date = glue::glue('{month}-{order_date}'), id = 1:nrow(.))})) %>% 
  dplyr::select(-data) %>% 
  unnest() 


count_p <- p %>% 
  group_by(zone, variety, ciclo, id) %>% 
  summarise(count = n())

# Hit score... matenme 

mean_p <- p %>% 
  dplyr::select(-order_date, -pd, -month, -date) %>% 
  rename('Obs' = 'HWAM', 'Est' = 'HWAM.E') %>% 
  gather(type, yield, -year, -variety, -zone, -ciclo, -cat, -id) %>% 
  group_by(zone, variety, ciclo, id, type) %>% 
  summarise( y_sd= sd(yield, na.rm = TRUE), median_Y = median(yield), yield = mean(yield)) %>% 
  ungroup()
  
# mean_p %>% dplyr::select(variety) %>% unique

C <- as_labeller(c('1' = 'Cycle 1', '2' = 'Cycle 2'))
Z <- as_labeller(c("CERETE" = 'Cereté (Córdoba)', "ESPINAL" = 'El Espinal (Tolima)',  "UNION" = 'La Unión (Valle del Cauca)'))

# Este graph es por variedad, asi que aun no esta listo para el guardado...
# ggplot(mean_p %>% filter(variety == 'FNC3056'), aes(x = id, y = yield, colour = type)) + 
#   geom_line() +
#   geom_point() + 
#   geom_ribbon(aes(ymin=yield - y_sd, ymax= yield + y_sd , fill=type), alpha=0.2)  + 
#   scale_fill_manual( NULL , values = c("gray30", "#008080"), labels = c('Sim', 'Obs')) +
#   scale_colour_manual(NULL , values = c("gray30", "#008080"), labels = c('Sim', 'Obs')) +
#   ylim(c(0, 10000)) +
#   scale_x_continuous(breaks = seq(from = 1, to = 12, by = 1)) +
#   facet_grid(ciclo~zone,  labeller = labeller(ciclo = C,  zone = Z)) + 
#   theme_bw() + 
#   labs(x = 'Planting date', y = 'Yield (Kg/ha)')




# =-=-= Promedio de promedios
mean_p %>% 
  group_by(zone, ciclo, id, type) %>% 
  summarise( y_sd= sd(yield, na.rm = TRUE), median_Y = median(yield), yield = mean(yield)) %>% 
  ggplot(aes(x = id, y = yield, colour = type)) + 
  geom_line() +
  geom_point() + 
  geom_ribbon(aes(ymin=yield - y_sd, ymax= yield + y_sd , fill=type), alpha=0.2) + 
  scale_fill_manual( NULL , values = c("gray30", "#008080"), labels = c('Sim', 'Obs')) +
  scale_colour_manual(NULL , values = c("gray30", "#008080"), labels = c('Sim', 'Obs')) +
  ylim(c(0, 10000)) +
  scale_x_continuous(breaks = seq(from = 1, to = 12, by = 1)) +
  facet_grid(ciclo~zone,  labeller = labeller(ciclo = C,  zone = Z)) + 
  theme_bw() + 
  labs(x = 'Planting date', y = 'Yield (Kg/ha)')

ggsave("graphs/G_mean_Ic.pdf", width = 10, height = 5)
ggsave("graphs/G_mean_Ic.png", width = 10, height = 5)

 
# library(verification)

# ####### - ###### - ####### - ########


# Mejor fecha de siembra...
Datos_p <- p %>% 
  dplyr::select(-order_date, -pd, -month, -date, -cat) %>% 
  rename('Obs' = 'HWAM', 'Est' = 'HWAM.E') %>% 
  gather(type, yield, -year, -variety, -zone, -ciclo, -id) %>% 
  nest(-year, -zone, -variety,-ciclo, -type) %>%
  mutate(max_data = purrr::map(.x = data, .f = function(.x){.x %>% arrange(yield) %>% slice(n())})) %>% 
  dplyr::select(-data) %>% 
  unnest()
 
# =-=-=-=-=-= Listo... entonces de aquí en adelante 
test <- Datos_p %>%
  nest(-year, -variety, -zone, -ciclo) %>% 
  mutate(Dif = purrr::map(.x = data, .f = function(.x){.x[1,-1] - .x[2,-1]})) %>% 
  dplyr::select(-data) %>% 
  unnest()


test1 <- test %>%
  # dplyr::group_by(variety, zone, ciclo) %>% 
  mutate(id = case_when( id > 3 ~ 4, id < -3 ~ -4, TRUE ~ as.numeric(id))) %>%
  mutate(id = abs(id)) %>% 
  count(zone, variety, zone, ciclo, id) # %>% write_csv(., path = 'Datos_Julio/test1.csv')
  

# =-=-=
test1 %>% 
  ggplot(aes(x = id, y = n, fill = as.character(ciclo)))+
  geom_bar(stat = 'identity', position = 'dodge', colour = 'black', alpha = 0.7) + 
  facet_grid(zone ~ variety,  labeller = labeller(zone = Z)) + 
  scale_x_continuous(breaks=0:4, labels=c('0', '5', "10", '15', '20+'))+
  scale_fill_manual(values = c("gray30", "#008080")) +
  scale_y_continuous(breaks = seq(from = 0, to = 9, by = 1)) + 
  labs(x = 'Diference between planting dates obs - sim (days)', y = 'years', fill = 'Cycle') +
  theme_bw() 

ggsave("graphs/Diference_VZ.pdf", width = 10, height = 5)
ggsave("graphs/Diference_VZ.png", width = 10, height = 5)


# =-=-=
test1 %>% 
  ggplot(aes(x = id, y = n, fill = variety))+
  geom_bar(stat = 'identity', position = 'dodge', colour = 'black', alpha = .7) + 
  facet_grid(ciclo~zone,  labeller = labeller(ciclo = C,  zone = Z)) + 
  scale_fill_viridis_d() + 
  scale_x_continuous(breaks=0:4, labels=c('0', '5', "10", '15', '20+'))+
  # scale_fill_manual( NULL , values = c("gray30", "#008080"), labels = c('Sim', 'Obs')) +
  scale_y_continuous(breaks = seq(from = 0, to = 9, by = 1)) + 
  labs(x = 'Diference between planting dates obs - sim (days)', y = 'years', fill = 'Variety') +
  theme_bw() 

ggsave("graphs/Diference_ZC.pdf", width = 10, height = 5)
ggsave("graphs/Diference_ZC.png", width = 10, height = 5)

#############################################################################################
###  Acumulando 

a <- Datos_p %>%
  nest(-year, -variety, -zone, -ciclo) %>% 
  mutate(Dif = purrr::map(.x = data, .f = function(.x){.x[1,-1] - .x[2,-1]})) %>% 
  dplyr::select(-data) %>% 
  unnest() %>% 
  mutate(id = abs(id)) %>%
  # dplyr::group_by(variety, zone, ciclo) %>% 
  mutate(id = case_when( id > 3 ~ 4, id < -3 ~ -4, TRUE ~ as.numeric(id))) %>%
  mutate(id = abs(id)) %>% 
  count(zone, variety, zone, ciclo, id) %>% 
  nest(-zone, -variety, -zone, -ciclo) %>% 
  mutate(acum = purrr::map(.x = data, .f = function(.x){.x %>% mutate(n = cumsum(n)) })) %>% 
  dplyr::select(-data) %>% 
  unnest()


# =-=-=-=-=-=-=-=-=-=-=-=-=-= Grafico de prueba... solo tratando de aprenderlo. 
# test1 %>% 
#   ggplot(aes(x = id, y = n, , label=n,  colour = variety)) + 
#   geom_point(stat='identity', size=6)  +
#   geom_segment(aes(y = 0,  x = id,  yend = n,  xend =  id)) +
#   geom_text(color="white", size=2) + 
#   scale_x_continuous(breaks=0:4, labels=c('0', '5', "10", '15', '20+'))+
#   scale_y_continuous(breaks = seq(from = 0, to = 9, by = 1))   +
#   # coord_flip() +
#   facet_grid(ciclo ~ zone,  labeller = labeller(ciclo = C,  zone = Z)) +
#   theme_bw()

# ggsave("graphs/mtcars.pdf", width = 10, height = 5)
# ggsave("graphs/mtcars.png", width = 10, height = 5)



library(pROC)
# f <- p %>% nest(-zone,  -id) %>% filter(row_number() == 1) %>% dplyr::select(data) %>% unnest()
ROC_m <- function(f){
  ter <- quantile(f$HWAM, c(0.33, 0.66))
  f <- f %>% mutate(cat = case_when( HWAM < ter[1] ~ 'Bajo', 
                                     HWAM > ter[2] ~ 'Alto', TRUE ~ 'Medio'))
  
  a <- pROC::roc(f %>% filter(cat != 'Alto') %>% .$cat, f %>% filter(cat != 'Alto') %>% .$HWAM.E)$auc %>% as.numeric() %>% round(., 2)
  b <- pROC::roc(f %>% filter(cat != 'Bajo') %>% .$cat, f %>% filter(cat != 'Bajo') %>% .$HWAM.E)$auc %>% as.numeric() %>% round(., 2)
  c <- pROC::roc(f %>% filter(cat != 'Medio') %>% .$cat, f %>% filter(cat != 'Medio') %>% .$HWAM.E)$auc %>% as.numeric() %>% round(., 2)
  d <- mean(c(a, b, c)) %>% round(., 2)
  
  cat(glue::glue('No Alto: {a} - No Bajo: {b} - No Medio: {c} - GROC = {d}'))
  all <- tibble(N_Alto = a, N_Bajo = b, N_Medio = c, M_all = d, nrow_data = nrow(f))
return(all)}

ROC_t <- p %>% 
  nest(-zone,  -ciclo) %>% 
  # filter(! row_number() %in% 25:28) %>%
  mutate(ROC = purrr::map(.x = data, .f = ROC_m))


# p %>% 
#   nest(-zone, -id) %>% 
#   filter(row_number() == 36) %>% 
#   dplyr::select(data) %>%
#   unnest() %>% 
#   dplyr::select(cat) %>% 
#   unique()

ROC_t %>% 
  dplyr::select(-data) %>% 
  unnest() %>% 
  dplyr::select(-nrow_data) %>% 
  ggplot(aes(zone, M_all, fill = as.character(ciclo))) + 
  geom_bar(stat = 'identity', position = 'dodge') + 
  scale_fill_manual( 'cycle' , values = c("gray30", "#008080")) +
  ylim(c(0, 1)) + 
  theme_bw() + 
  geom_hline(yintercept = 0.5,  color= "gray", size = 1.2) +
  labs(x = NULL, y = 'GROC', fill = NULL)


p %>% 
  nest(-ciclo,-id) %>%
  mutate(ROC = purrr::map(.x = data, .f = ROC_m)) %>% 
  dplyr::select(-data) %>% 
  unnest() %>% 
  dplyr::select(-nrow_data) %>% 
  ggplot(aes(id, M_all, fill = as.character(ciclo))) + 
  geom_bar(stat = 'identity', position = 'dodge') + 
  scale_fill_manual( 'cycle' , values = c("gray30", "#008080")) +
  scale_x_continuous(breaks = seq(from = 1, to = 12, by = 1)) + 
  ylim(c(0, 1)) + 
  theme_bw() + 
  geom_hline(yintercept = 0.5,  color= "gray", size = 1.2) + 
  labs(x = 'Planting date', y = 'GROC', fill = NULL)






# =-=-=-=-=-=-=-=-=-=-=-=-=

f <- p %>% nest(-zone,  -id) %>% filter(row_number() == 10) %>% dplyr::select(data) %>% unnest()

j <- f  %>% 
  dplyr::select(-order_date, -month, -year, -pd, -cat, -date, -ciclo) %>% 
  mutate(dif = HWAM - HWAM.E, dif_two = dif^2, dif_abs = abs(dif))

ind <- j %>% 
  summarise(mean_obs = mean(HWAM),
    RMSE = sqrt((1/nrow(.))*sum(dif_two)), 
    d_a = (1/nrow(.))*sum(dif),
    d_ap = d_a/mean_obs*100, 
    pearson = cor(HWAM, HWAM.E), 
    R_2 = pearson^2, 
    spearman = cor(HWAM, HWAM.E, method = 'spearman'), 
    kendall = cor(HWAM, HWAM.E, method = 'kendall'))

ggplot(j, aes(x = HWAM.E, y = HWAM)) + geom_point() + geom_smooth(method = lm, se = FALSE) + theme_bw()


sd_d <- j %>% mutate(sd = (dif - ind$d_a)^2/(nrow(.)-1) ) %>% summarise(sd = sqrt(sum(sd)) ) %>% as.numeric()

ind %>% mutate(sd_d = sd_d) %>% dplyr::select(-mean_obs)


# RMSE
# f %>% yardstick::rmse(truth = HWAM,  estimate = HWAM.E)
# f %>% yardstick::mae(truth = HWAM,  estimate = HWAM.E)
# yardstick::metrics(f, truth = HWAM,  estimate = HWAM.E)



