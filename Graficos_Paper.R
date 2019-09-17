rm(list = ls()); gc(reset = TRUE)
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Made by:     Alejandra Esquivel Arias. 
# Created in:  Date: 8-2019.
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# graphs Leo's paper.

library(tidyverse)
library(lubridate)
library(glue)
library(cowsay)
library(pROC)
library(groupdata2)
library(future)
library(furrr)

# =-=-=-=-=-= Generacion de los graficos para datos Historical. 
cowsay::say(what = "Generation of graphs for historical data.", by = "rabbit", what_color = "#FF4500", by_color = "red")

# =-=-=-=-=-=-=-=-=
# Data reading.
Historical <- read_csv("D:/OneDrive - CGIAR/Desktop/Maize_Paper/Maize_Paper/Datos_Julio/Historicos_1980-2013.csv") %>% dplyr::select(-X1)


# Filter historic data. 
Historic_fil <- Historical %>%
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
  unnest() %>% 
  dplyr::select(year, variety, zone, ciclo, month, id, date, HWAM)
  
# Historical coefficient of variation.
cv_Data <- Historic_fil %>% 
  group_by(zone, ciclo, variety, id) %>% 
  summarise(HWAM_sd = sd(HWAM), M_HWAM = mean(HWAM)) %>% 
  mutate(HWAM_cv = round(HWAM_sd/M_HWAM, 3)* 100 ) %>% 
  ungroup() %>%
  group_by(zone, ciclo, id) %>% 
  summarise(cv_mean = mean(HWAM_cv)) 

# CV historical data graph. 
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


# Construction of the limits.
limits <- Historic_fil %>% 
  group_by(zone, ciclo, variety, id) %>% 
  summarise(HWAM_sd = sd(HWAM), M_HWAM = mean(HWAM)) %>% 
  mutate(HWAM_cv = round(HWAM_sd/M_HWAM, 3)* 100 ) %>% 
  ungroup() %>%
  group_by(zone, ciclo, id) %>% 
  summarise(cv_mean = mean(HWAM_cv), cv_min = min((HWAM_cv)), cv_max = max(HWAM_cv)) 

# Graph with limits. 
ggplot(limits, aes(id, y = cv_mean, colour = ciclo)) + 
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
# =-=-=-=-=-= Retrospective analysis. 
cowsay::say(what = "Retrospective analysis.", by = "smallcat", what_color = "#FF4500", by_color = "red")

# Reading the retrospective database
Retro_data <- read_csv("Datos_Julio/Groc.csv") %>% 
  dplyr::select(-X1, -freq) %>%
  separate(TNAM....................., c('month', 'order_date'), sep = '/') %>%
  mutate(zone = replace(zone, zone == 'LA UNION', 'LA_UNION'), 
         month = str_remove(month, 'P') %>% as.numeric(), 
         order_date = as.numeric(order_date)) 


# Filter retrospective data
Retro_filter <- Retro_data %>%
  nest(-zone, -ciclo,  -variety, -year) %>% 
  mutate(data_mod = purrr::map(.x = data, .f =  function(x){ x %>% mutate(date = glue::glue('{month}-{order_date}'), id = 1:nrow(.))})) %>% 
  dplyr::select(-data) %>% 
  unnest() 

# Concordance count planting dates.
count_pd <- Retro_filter%>% 
  group_by(zone, variety, ciclo, id) %>% 
  summarise(count = n())

# Simulated average performance with retrospective and observed data.
mean_p <- Retro_filter%>% 
  dplyr::select(-order_date, -pd, -month, -date) %>% 
  rename('Obs' = 'HWAM', 'Est' = 'HWAM.E') %>% 
  gather(type, yield, -year, -variety, -zone, -ciclo, -cat, -id) %>% 
  group_by(zone, variety, ciclo, id, type) %>% 
  summarise( y_sd= sd(yield, na.rm = TRUE), median_Y = median(yield), yield = mean(yield)) %>% 
  ungroup()

# =-=-=-= ggplot labellers (only for the graphs).  
C <- as_labeller(c('1' = 'Cycle 1', '2' = 'Cycle 2'))
Z <- as_labeller(c("CERETE" = 'Cereté (Córdoba)', "ESPINAL" = 'El Espinal (Tolima)',  "UNION" = 'La Unión (Valle del Cauca)'))


# =-=-= Comparison of simulated average performance with retrospective and observed data.
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

 
# ####### - ###### - ####### - ########
# Define better planting date.
Datos_p <- Retro_filter%>% 
  dplyr::select(-order_date, -pd, -month, -date, -cat) %>% 
  rename('Obs' = 'HWAM', 'Est' = 'HWAM.E') %>% 
  gather(type, yield, -year, -variety, -zone, -ciclo, -id) %>% 
  nest(-year, -zone, -variety,-ciclo, -type) %>%
  mutate(max_data = purrr::map(.x = data, .f = function(.x){.x %>% arrange(yield) %>% slice(n())})) %>% 
  dplyr::select(-data) %>% 
  unnest()
 
# Difference between planting dates. 
D_B_plantingD <- Datos_p %>%
  nest(-year, -variety, -zone, -ciclo) %>% 
  mutate(Dif = purrr::map(.x = data, .f = function(.x){.x[1,-1] - .x[2,-1]})) %>% 
  dplyr::select(-data) %>% 
  unnest()

# Limits for planting dates.
plantingD <- D_B_plantingD %>%
  mutate(id = case_when( id > 3 ~ 4, id < -3 ~ -4, TRUE ~ as.numeric(id))) %>%
  mutate(id = abs(id)) %>% 
  count(zone, variety, zone, ciclo, id) # %>% write_csv(., path = 'Datos_Julio/plantingD.csv')
  

plantingD %>% 
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

# Concordance of planting dates - [cycle * zone] - colors: variety.
plantingD %>% 
  ggplot(aes(x = id, y = n, fill = variety))+
  geom_bar(stat = 'identity', position = 'dodge', colour = 'black', alpha = .7) + 
  facet_grid(ciclo~zone,  labeller = labeller(ciclo = C,  zone = Z)) + 
  scale_fill_viridis_d() + 
  scale_x_continuous(breaks=0:4, labels=c('0', '5', "10", '15', '20+'))+
  scale_y_continuous(breaks = seq(from = 0, to = 9, by = 1)) + 
  labs(x = 'Diference between planting dates obs - sim (days)', y = 'years', fill = 'Variety') +
  theme_bw() 

ggsave("graphs/Diference_ZC.pdf", width = 10, height = 5)
ggsave("graphs/Diference_ZC.png", width = 10, height = 5)

# =-=-=-=-=-=-=-=
# Accumulation of matches by date.
Acum_Con_by_date <- Datos_p %>%
  nest(-year, -variety, -zone, -ciclo) %>% 
  mutate(Dif = purrr::map(.x = data, .f = function(.x){.x[1,-1] - .x[2,-1]})) %>% 
  dplyr::select(-data) %>% 
  unnest() %>% 
  mutate(id = abs(id)) %>%
  mutate(id = case_when( id > 3 ~ 4, id < -3 ~ -4, TRUE ~ as.numeric(id))) %>%
  mutate(id = abs(id)) %>% 
  count(zone, variety, zone, ciclo, id) %>% 
  nest(-zone, -variety, -zone, -ciclo) %>% 
  mutate(acum = purrr::map(.x = data, .f = function(.x){.x %>% mutate(n = cumsum(n)) })) %>% 
  dplyr::select(-data) %>% 
  unnest()


# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
cowsay::say(what = "Indicators: GROC, RMSE, ...", by = "owl", what_color = "#FF4500", by_color = "red")

# This function computes validation indicators. 
ROC_m <- function(f, name){
  # f <- Retro_filter%>% nest(-zone,  -ciclo) %>% filter(row_number() == 2) %>% dplyr::select(data) %>% unnest()
  # name <- Retro_filter%>% nest(-zone,  -ciclo) %>% filter(row_number() == 2) %>% mutate(name = glue::glue('{zone}_{ciclo}')) %>% .$name
  
  ter <- quantile(f$HWAM, c(0.33, 0.66))
  f <- f %>% mutate(cat = case_when( HWAM < ter[1] ~ 'Bajo', 
                                     HWAM > ter[2] ~ 'Alto', TRUE ~ 'Medio'))
  
  a <- pROC::roc(f %>% filter(cat != 'Alto') %>% .$cat, f %>% filter(cat != 'Alto') %>% .$HWAM.E)$auc %>% as.numeric() %>% round(., 2)
  b <- pROC::roc(f %>% filter(cat != 'Bajo') %>% .$cat, f %>% filter(cat != 'Bajo') %>% .$HWAM.E)$auc %>% as.numeric() %>% round(., 2)
  c <- pROC::roc(f %>% filter(cat != 'Medio') %>% .$cat, f %>% filter(cat != 'Medio') %>% .$HWAM.E)$auc %>% as.numeric() %>% round(., 2)
  d <- mean(c(a, b, c)) %>% round(., 2)
  
  cat(glue::glue('No Alto: {a} - No Bajo: {b} - No Medio: {c} - GROC = {d}'))
  all <- tibble(N_Alto = a, N_Bajo = b, N_Medio = c, M_all = d, nrow_data = nrow(f))
  
  
  j <- f   %>% mutate(dif = HWAM - HWAM.E, dif_two = dif^2, dif_abs = abs(dif))
  
  ind <- j %>% summarise(mean_obs = mean(HWAM),
              RMSE = sqrt((1/nrow(.))*sum(dif_two)), 
              d_a = (1/nrow(.))*sum(dif), d_ap = abs(d_a)/mean_obs*100, 
              pearson = cor(HWAM, HWAM.E), R_2 = pearson^2, 
              spearman = cor(HWAM, HWAM.E, method = 'spearman'), 
              kendall = cor(HWAM, HWAM.E, method = 'kendall'))
  
  sd_d <- j %>% mutate(sd = (dif - ind$d_a)^2/(nrow(.)-1) ) %>% summarise(sd = sqrt(sum(sd)) ) %>% as.numeric()
  all <- ind %>% mutate(sd_d = sd_d) %>% dplyr::select(-mean_obs) %>% bind_cols(., all)
  
  
  min_max <- j %>% dplyr::select(HWAM, HWAM.E) %>% gather(name, value) %>% arrange(value) %>% slice(1,n()) %>% .$value
  
 p <- ggplot(j, aes(x = HWAM.E, y = HWAM)) + geom_point(colour = "#008080") + 
    geom_smooth(method = lm, se = FALSE) + theme_bw() + xlim(c(min_max[1], min_max[2])) +
    ylim(c(min_max[1], min_max[2])) + labs(x = 'Simulated', y ='Observed')
  
 png(glue::glue('graphs/disp/{name}.png'),  width = 720, height = 450, res = 120)
 print(p)
 dev.off()
   
  return(all)}


# Table with indicators. 
ROC_t <- Retro_filter%>% 
  nest(-zone,  -ciclo) %>% # if you want other filters chage this line
  mutate(name = glue::glue('{zone}_{ciclo}')) %>% # and this line
  mutate(ROC = purrr::map2(.x = data, .y = name, .f = ROC_m))

# Graph for GROC indicator. 
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
ggsave("graphs/GROC_ZC.pdf", width = 10, height = 5)
ggsave("graphs/GROC_ZC.png", width = 10, height = 5)


# Other indicator...
ROC_t %>% dplyr::select(-data) %>% unnest() %>% dplyr::select(-nrow_data) %>% 
  ggplot(aes(zone, pearson, fill = as.character(ciclo))) + # If you want another indicator change this part. 
  geom_bar(stat = 'identity', position = 'dodge') + 
  scale_fill_manual( 'cycle' , values = c("gray30", "#008080")) +
  # ylim(c(0, 1)) + # Change for the correct limits
  theme_bw() + 
  geom_hline(yintercept = 0.5,  color= "gray", size = 1.2) +
  labs(x = NULL, y = NULL, fill = NULL) # Change y = 'new variable'.

ggsave("graphs/other_ZC.pdf", width = 10, height = 5)
ggsave("graphs/other_ZC.png", width = 10, height = 5)



# Other graph example...
Retro_filter %>% 
  nest(-ciclo,-id) %>%
  mutate(name = glue::glue('{ciclo}_{id}')) %>% 
  mutate(ROC = purrr::map2(.x = data, .y = name, .f = ROC_m)) %>% 
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

ggsave("graphs/other_IdC.pdf", width = 10, height = 5)
ggsave("graphs/other_IdC.png", width = 10, height = 5)




# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
#     _.       _.       _        _        _        _        _        _.   
#   _( )__   _( )__   _( )__   _( )__   _( )__   _( )__   _( )__   _( )__ 
# _|     _|_|     _|_|     _|_|     _|_|     _|_|     _|_|     _|_|     _|
# (_ G _ (_(_ I _ (_(_   _ (_(_ G _ (_(_ R _ (_(_ A _ (_(_ P _ (_(_ H _ (_ 
#  |_( )__| |_( )__| |_( )__| |_( )__| |_( )__| |_( )__| |_( )__| |_( )__|
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=                                                                           

path <- 'D:/OneDrive - CGIAR/Desktop/Maize_Paper/GI'

GI_retro <- function(data){
  postitions <- which(is.na(data$Index_1)) - 1
  
  data <- data %>%
    filter(row_number() %in% postitions) %>%
    filter(X != 'Training', X != 'CURRENT', X != 'X') %>%
    .[, -(1:4)] %>%
    mutate(year = 2005:2013)
  return(data)}
GI <- read_table2("D:/OneDrive - CGIAR/Desktop/Maize_Paper/GI/Cerete_initial_Jul/GI_M1_Cerete_ASO_month_Jul.txt",  skip = 5)

# list.files(path, recursive = TRUE, pattern = 'GI', full.names = TRUE)[1] %>% read_table2(., skip = 5) %>% GI_retro

all_GI <- list.files(path, recursive = TRUE, pattern = 'GI') %>%
  basename(.)  %>%
  str_remove('.txt') %>%
  tibble() %>%
  rename(file = '.') %>%
  mutate(names = list.files(path, recursive = TRUE, pattern = 'GI', full.names = TRUE)) %>%
  mutate(GI_raw = purrr::map(.x = names, .f = function(.x) read_table2(.x, skip = 5) )) %>%
  dplyr::select(-names) %>%
  mutate(GI_retro = purrr::map(.x = GI_raw, .f = GI_retro)) %>%
  dplyr::select(-GI_raw) %>%
  unnest

character <- function(character){
  character<-  character %>%
    str_split('_') %>%
    unlist %>%
    .[c(2:4, 6)] %>%
    t() %>%
    as_tibble() %>%
    set_names('M','place', 'season', 'IC')
  return(character)}

all_GI <- all_GI %>% mutate(summary_file = purrr::map(.x = file, .f = character)) %>%
  dplyr::select(-file) %>% unnest %>% mutate(Index_1 = as.numeric(Index_1)) %>%
  mutate(ciclo = ifelse(IC %in% c("Jan", "Feb",  "Mar"), 'A', 'B' ))

all_GI %>% write_csv('GI_retro.csv')

all_GI <- all_GI %>% group_by(place, ciclo,  M,season) %>%
  summarise(mean = mean(Index_1), sd = sd(Index_1))

all_GI <- all_GI %>%
  ungroup() %>%
  separate(col = M, into = c('M', 'Month'), sep = 'M') %>%
  dplyr::select(-M)


# ggplot(all_GI, aes(x = Month, y = mean)) +
#   geom_bar(stat = 'identity', position = 'dodge', alpha = 0.7, fill = "gray30", colour = 'black') +
#   geom_errorbar(data = all_GI, aes(ymin = mean-sd, ymax = mean+sd), width=.3 )+
#   labs(y = 'Goodness Index - Mean') +
#   ylim(c(-0.04, 0.5)) +
#   geom_text(aes(label = season), vjust = -2, 
#             position=position_dodge(width=0.9)) +
#   facet_grid(ciclo~place,
#              labeller=labeller(ciclo =  as_labeller(c('A' = 'Season A', 'B' = 'Season B')), place = as_labeller(c('Cerete' = 'Cordoba','Espinal' = 'Tolima', 'LaUnion'  = 'Valle del Cauca'))) )+
#   theme_bw()


max_min <- all_GI %>% arrange(mean) %>% slice(1, n()) %>% pull(mean) 

a_gi <- all_GI %>% 
  mutate(var = glue::glue('{Month}.{season}')) %>% 
  filter(ciclo == 'A') %>%
  ggplot(aes(x = var, y = mean)) +
  geom_bar(stat = 'identity', position = 'dodge', alpha = 0.7, fill = "gray30", colour = 'black') +
  geom_errorbar(aes(ymin = mean-sd, ymax = mean+sd), width=.3 )+
  labs(y = 'Goodness Index - Mean', x = NULL) +
  ylim(c( max_min[1] - 0.03, max_min[2] + 0.02)) +
  facet_grid(ciclo~place, scales = 'free_x',
             labeller=labeller(ciclo =  as_labeller(c('A' = 'Season A', 'B' = 'Season B')), place = as_labeller(c('Cerete' = 'Cordoba','Espinal' = 'Tolima', 'LaUnion'  = 'Valle del Cauca'))) )+
  theme_bw()


b_gi <- all_GI %>% mutate(var = glue::glue('{Month}.{season}')) %>% 
  filter(ciclo == 'B') %>% ggplot(aes(x = var, y = mean)) +
  geom_bar(stat = 'identity', position = 'dodge', alpha = 0.7, fill = "gray30", colour = 'black') +
  geom_errorbar(aes(ymin = mean-sd, ymax = mean+sd), width=.3 )+
  labs(y = 'Goodness Index - Mean', x = NULL) +
  ylim(c( max_min[1] - 0.03, max_min[2] + 0.02)) +
  facet_grid(ciclo~place, scales = 'free_x',
             labeller=labeller(ciclo =  as_labeller(c('A' = 'Season A', 'B' = 'Season B')), place = as_labeller(c('Cerete' = 'Cordoba','Espinal' = 'Tolima', 'LaUnion'  = 'Valle del Cauca'))) )+
  theme_bw()


f <- gridExtra::grid.arrange(a_gi, b_gi)

ggsave(f, filename = 'graphs/GI.png', height = 6.5, width = 10, dpi = 300)
ggsave(f, filename = 'graphs/GI.pdf', height = 6.5, width = 10, dpi = 200)



#####  Para trabajar la proxima semana. 

# Para el grafico de los roc hay 3 propuestas.
# Primero calcular un graph con facet_grid donde en el primer panel se incluya el GROC, 
# en el segundo y tercer panel es el ROC encima de lo normal y debajo. 
# Hacer el mismo graph que ya teniamos, pero... 
# Es el graph de barras, solo que le incluimos los puntos con los ROC individuales encima. 

# Para el graph de las fechas de siembra.
# Cambiar todo a pentadias, es decir agrupar cada 10 dias (penta es 5 pero meh)
# ademas hacer varios tipos de graph dejando y quitando los hibridos. 
# Voy a hacer tres tipos de graph para los tipos de hibridos, primero un graph de barras normal
# El siguiente uno 

# Grafico de barras doble. 
# data(diamonds )
# dio2 <- diamonds %>% count(cut, clarity)
# ggplot(dio2, aes(x=cut, y=n)) +
#   geom_bar(stat="identity", alpha=0.4) +
#   geom_bar(stat="identity", aes(fill=clarity), position="dodge")


cowsay::say(what = "Retrospective analysis.", by = "owl", what_color = "#FF4500", by_color = "red")
# Leyendo los datos de Leo... a ver... que cambios debo realizar...
# en que graphs utilizo las simulaciones

test <- rio::import('D:/OneDrive - CGIAR/Desktop/Maize_Paper/Maize_Paper/Datos_Agosto/Groc2.csv' )

test <- test %>% 
  dplyr::select(-V1)   %>% # Por ahora voy a quitar la variable categoria
  dplyr::select(TNAM....................., HWAM, year, pd, variety, zone, ciclo, cat, RUNNO, HWAM.E)  %>%
  separate(TNAM....................., c('month', 'order_date'), sep = '/') %>%
  mutate(zone = replace(zone, zone == 'LA UNION', 'LA_UNION'), 
         month = str_remove(month, 'P') %>% as.numeric(), 
         order_date = as.numeric(order_date)) 

# =-=-=-=-=-=-=-=-=-=-=-=

# Retro_data %>%
#   nest(-zone, -ciclo,  -variety, -year) %>% 
#   filter(row_number() == 1) %>% 
#   dplyr::select(data) %>% 
#   unnest() %>% 
#   mutate(date = glue::glue('{month}-{order_date}'), id = 1:nrow(.))

test_filter <- test %>%
  nest(-zone, -ciclo,  -variety, -year) %>% 
  mutate(data_mod = purrr::map(.x = data, .f = function(.x){
    .x %>% mutate(date = glue::glue('{month}-{order_date}')) %>% 
      groupdata2::group(., 12, method = 'n_dist', col_name = 'grupo') %>% 
      groupdata2::group(., 99, method = 'n_dist', col_name = 'id') })) %>% 
  dplyr::select(-data) %>% 
  unnest() 


# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
cowsay::say(what = "Indicators: GROC, RMSE, ...", by = "cat", what_color = "#FF4500", by_color = "red")

# This function computes validation indicators. 
ROC_m_rep <- function(f, name){
  #  Probando los indicadores, no estoy segura de como hacerlo con seguridad. 
  # test_filter %>%  nest(-zone,  -ciclo)
  # f <- test_filter %>%  nest(-zone,  -ciclo) %>% filter(row_number() == 2) %>% dplyr::select(data) %>% unnest()
  # name <- test_filter %>% nest(-zone,  -ciclo) %>% filter(row_number() == 2) %>% mutate(name = glue::glue('{zone}_{ciclo}')) %>% .$name
  
  ter <- quantile(f$HWAM, c(0.33, 0.66))
  f <- f %>% mutate(cat = case_when( HWAM < ter[1] ~ 'Bajo', 
                                     HWAM > ter[2] ~ 'Alto', TRUE ~ 'Medio'))
  
  a <- pROC::roc(f %>% filter(cat != 'Alto') %>% .$cat, f %>% filter(cat != 'Alto') %>% .$HWAM.E)$auc %>% as.numeric() %>% round(., 2)
  b <- pROC::roc(f %>% filter(cat != 'Bajo') %>% .$cat, f %>% filter(cat != 'Bajo') %>% .$HWAM.E)$auc %>% as.numeric() %>% round(., 2)
  c <- pROC::roc(f %>% filter(cat != 'Medio') %>% .$cat, f %>% filter(cat != 'Medio') %>% .$HWAM.E)$auc %>% as.numeric() %>% round(., 2)
  d <- mean(c(a, b, c)) %>% round(., 2)
  e <- mean(c(a, b))
  
  # cat(glue::glue('No Alto: {a} - No Bajo: {b} - No Medio: {c} - GROC = {d}'))
  all <- tibble(ROC_BM = a, ROC_AM = b,  ROC_AB = c, M_all = d, Mean_AB = e, nrow_data = nrow(f))
  
  normal_metrics <- f %>% 
    filter(complete.cases(.)) %>%
    summarise(n = n(),
              r_pearson = cor(HWAM, HWAM.E, method = c("pearson")),
              r_spearman = cor(HWAM, HWAM.E, method = c("spearman")), 
              r_kendall = cor(HWAM, HWAM.E, method = c("kendall")),
              RMSE = sqrt(mean((HWAM.E - HWAM)^2, na.rm = T)),
              NRMSE = RMSE/mean(HWAM, na.rm = T),
              MAE = sum(abs(HWAM.E - HWAM)/n),
              MBE = sum((HWAM.E - HWAM))/n,
              d = 1 - ((sum((HWAM.E - HWAM)^2, na.rm = T))/
                         sum((abs(HWAM.E - mean(HWAM, na.rm = T)) +
                                abs(HWAM - mean(HWAM, na.rm = T)))^2, na.rm = T)),
              NSE = 1 - ((sum((HWAM.E - HWAM)^2, na.rm = T))/
                           sum((HWAM - mean(HWAM, na.rm = T))^2, na.rm = T)),
              rsq_2 = summary(lm(HWAM.E ~ HWAM))$r.squared)
  
  
  all <- bind_cols(all, normal_metrics)
  
  
  return(all)}

# Table with indicators. 
ROC_tf <- test_filter%>% 
  nest(-zone,  -ciclo) %>% # if you want other filters chage this line
  mutate(name = glue::glue('{zone}_{ciclo}')) %>% # and this line
  mutate(ROC = purrr::map2(.x = data, .y = name, .f = ROC_m_rep))

af <- ROC_tf %>% 
  dplyr::select(-data) %>% 
  unnest() %>% 
  dplyr::select(-nrow_data) 

ROC_label <- as_labeller(c('M_all' = 'GROC', 'Mean_AB' = 'Mean ROC(above-below)', 'ROC_AM' = 'ROC(above-normal)', 'ROC_BM' = 'ROC(below-normal)'))

j <- af %>% dplyr::select(-n)

af <- af %>% 
  dplyr::select(zone, ciclo, name,  ROC_BM, ROC_AM, ROC_AB, M_all, Mean_AB)

# Ultima idea para los GROC.
af %>% 
  dplyr::select(-ROC_AB) %>% 
  gather(type, value, -zone, -ciclo, -name) %>% 
  ggplot(aes(zone, value, fill = as.character(ciclo))) +
  geom_bar(stat = 'identity', position = 'dodge') +
  facet_grid(~type, labeller = labeller(type = ROC_label) ) + 
  ylim(c(0, 1)) +
  scale_fill_manual( 'cycle' , values = c("gray30", "#008080")) +
  theme_bw() + 
  geom_hline(yintercept = 0.5,  color= "gray", size = 1.2) +
  labs(x = NULL, y = 'AUC', fill = NULL)

ggsave("graphs/GROC_ZC.pdf", width = 10, height = 5)
ggsave("graphs/GROC_ZC.png", width = 10, height = 5)


af1 <- af %>% 
  dplyr::select(-M_all)  %>% 
  gather(type, value, -zone, -ciclo, -name)


# Segunda mejor posibilidad para los ROC.
ggplot() +
  geom_bar( data = af, aes(zone, M_all, fill = as.character(ciclo)) ,stat = 'identity', position = 'dodge', alpha = 0.6)  + 
  geom_point(data = af1,  aes(zone, value, colour = as.character(ciclo), shape = type, group = as.character(ciclo)), size = 3.3,  position = position_dodge(width = 1)) + 
  ylim(c(0, 1)) +
  scale_x_discrete(labels = c('Cereté\n(Córdoba)', 'El Espinal\n(Tolima)', 'La Unión\n(Valle del Cauca)')) +
  scale_fill_manual( 'Season' , values = c("gray30", "#008080"), labels = c('A', 'B')) +
  scale_colour_manual( 'Season' , values = c("gray30", "#008080"), labels = c('A', 'B'), guide = FALSE) + 
  scale_shape(name = NULL, solid = TRUE, 
              labels = c('Mean ROC(above-below)', 'ROC(above-below)', 'ROC(above-normal)', 'ROC(below-normal)'))+
  theme_bw() + 
  geom_hline(yintercept = 0.5,  color= "gray", size = 1.2) + 
  labs(x = NULL, y = 'AUC', fill = 'Season') +
  theme(legend.position = "top", legend.box = "vertical") 

ggsave("graphs/GROC_ZC2.pdf", width = 8, height = 7)
ggsave("graphs/GROC_ZC2.png", width = 8, height = 7)  



# Mejor posibilidad para los ROC. 
ggplot() +
  geom_bar( data = af, aes(zone, M_all, fill = as.character(ciclo)) ,stat = 'identity', position = 'dodge', alpha = 0.6)  + 
  geom_jitter(data = af1,  aes(zone, value, colour = as.character(ciclo), shape = type, group = as.character(ciclo)),  position = position_jitterdodge(), size = 3.3) + 
  ylim(c(0, 1)) +
  scale_fill_manual( 'Cycle' , values = c("gray30", "#008080")) +
  scale_colour_manual( 'Cycle' , values = c("gray30", "#008080")) + 
  scale_shape(name = NULL, solid = TRUE, 
              labels = c('Mean ROC(above-below)', 'ROC(above-below)', 'ROC(above-normal)', 'ROC(below-normal)'))+
  theme_bw() + 
  geom_hline(yintercept = 0.5,  color= "gray", size = 1.2) + 
  labs(x = NULL, y = 'AUC', fill = NULL) +
  theme(legend.position = "top", legend.box = "vertical")

ggsave("graphs/GROC_ZC1.pdf", width = 8, height = 7)
ggsave("graphs/GROC_ZC1.png", width = 8, height = 7)  




j %>% write_csv('Indicadores.csv')



# =-=-=-=-=-=-=-=-=-=-=-=
# =-=-=- Order cultivar
# =-=-=-=-=-=-=-=-=-=-=-=

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
cowsay::say(what = "Indicators: GROC, RMSE, ...", by = "smallcat", what_color = "#FF4500", by_color = "red")

# This function make a cultivar order...
cultivar_order <- function(data){
  # data <- test_filter  %>%  dplyr::select(-cat) %>% nest(-zone, -year, -ciclo, -date) %>%
  # filter(row_number() == 100) %>% dplyr::select(data) %>% unnest()
  
  obs <- data %>% dplyr::select(variety, HWAM) %>% unique() %>% 
    arrange(desc(HWAM)) %>% slice(c(1, n())) %>% mutate(max_min = 1:2)
  
  sim <- data %>% dplyr::select(variety, RUNNO, HWAM.E) %>% nest(-RUNNO) %>% 
    mutate(data_f = purrr::map(data, .f = function(.x){.x %>% arrange(desc(HWAM.E)) %>% slice(c(1, n())) %>% mutate(max_min = 1:2) })) %>% 
    dplyr::select(-data) %>% unnest() %>% group_by(max_min, variety) %>%  
    summarise(n = n()/99 * 100) %>% ungroup()
  
  max <- filter(sim, max_min == 1, variety == obs$variety[1])
  min <- filter(sim, max_min == 2, variety == obs$variety[2])
  
  max_min <- bind_rows(max, min)
  
  max_min <-  if(nrow(max_min) == 1){
    bind_rows(max_min, tibble(max_min = 0, variety = NA_character_, n = NA_real_))    
  }else if(nrow(max_min) == 0){
    tibble(max_min = rep(0, 2), variety = NA_character_, n = NA_real_)
  }else if(nrow(max_min) == 2){bind_rows(max, min)}
  
  return(max_min)}

cores <- parallel::detectCores() - 1
plan(cluster, workers = cores)

tictoc::tic()
cultivar <- test_filter  %>%
  dplyr::select(-cat) %>%
  nest(-zone, -year, -ciclo, -date, -grupo) %>%
  # filter(row_number() < 101) %>% #vamos a dejar todo montado... por ahora.
  mutate(best_worst = furrr::future_map(.x = data, .f = cultivar_order) )
tictoc::toc() # 3.25 minutos.

plan(sequential) # 50.64 sec


# save files. 
cultivar %>% 
  dplyr::select(-data) %>% 
  unnest() %>% 
  mutate(max_min_names = case_when(max_min == 1 ~ 'Max', 
                                   max_min == 2 ~ 'Min', 
                                   TRUE ~ as.character(max_min))) %>% 
  write_csv('D:/OneDrive - CGIAR/Desktop/Maize_Paper/Maize_Paper/cultivar_order.csv')


# Graphs tipo
C <- as_labeller(c('1' = 'Season A', '2' = 'Season B'))
Z <- as_labeller(c("CERETE" = 'Cereté (Córdoba)', "ESPINAL" = 'El Espinal (Tolima)',  "UNION" = 'La Unión (Valle del Cauca)'))

cultivar %>% 
  dplyr::select(-data) %>% 
  unnest() %>% 
  filter(max_min == 1) %>% 
  ggplot(aes(x = grupo, y = n/9, fill = variety)) + 
  geom_bar(stat = 'identity', alpha = 0.7) +
  scale_fill_viridis_d(direction = -1) + 
  facet_grid(ciclo ~ zone, labeller = labeller(ciclo = C,  zone = Z)) +
  theme_bw() +
  ylim(c(0, 100)) +
  labs(x = 'Planting dates', y = 'Percentage', fill = 'Variety')

ggsave(filename = 'graphs/Cultivar_max.png', height = 6.5, width = 10, dpi = 300)
ggsave(filename = 'graphs/Cultivar_max.pdf', height = 6.5, width = 10, dpi = 200)




# --------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------

# data <- test_filter  %>%  dplyr::select(-cat) %>% nest(-zone, -year, -ciclo, -date) %>%
# filter(row_number() == 100) %>% dplyr::select(data) %>% unnest()

max_hwam <- function(data2){
  
  # data2 <-  test_filter  %>%  dplyr::select(-cat) %>% nest(-zone, -year, -ciclo, -order_date ,-date, -id) %>%
  #   dplyr::select(data) %>% filter(row_number() == 1) %>% unnest()
  
  obs <- data2 %>%
    dplyr::select(variety, HWAM) %>% 
    unique() %>% 
    arrange(desc(HWAM)) %>% 
    slice(1) %>% 
    mutate(type = 'obs')
  
  forecast <- data2 %>% 
    dplyr::select( -pd, -HWAM) %>% 
    group_by(variety) %>%
    summarise(HWAM.E = max(HWAM.E)) %>% 
    arrange(desc(HWAM.E)) %>% 
    slice(1)  %>% 
    mutate(type = 'forecast') %>% 
    rename('HWAM' = 'HWAM.E')
  
  all <- bind_rows(obs, forecast)
  return(all)}

data2 <- test_filter  %>%  
  dplyr::select(-cat) %>% 
  # nest(-zone, -year, -ciclo, -order_date ,-date, -id) %>% 
  nest(-zone, -year, -ciclo, -order_date ,-date, -grupo) %>% 
  mutate(test = purrr::map(.x = data, .f = max_hwam)) 



data <- data2 %>% 
  dplyr::select(-data) %>% 
  unnest() %>% 
  nest(-zone, -year, -ciclo, -type) %>% 
  mutate(max = purrr::map(.x = data, .f = function(x){ x %>% arrange(desc(HWAM)) %>% slice(1)})) %>% 
  dplyr::select(-data) %>% 
  unnest() %>% 
  mutate(grupo = as.numeric(grupo)) %>%
  nest(-year, -zone, -ciclo)


subtracting_rows <- function(x){
  
  x <- x %>% 
    dplyr::select(order_date, grupo, HWAM) 
  
  x <- x[1,] - x[2,]
  
  return(x)}


a <- data %>% 
  mutate(dif = purrr::map(.x = data, .f = subtracting_rows)) %>% 
  dplyr::select(-data) %>% 
  unnest() %>% 
  mutate(dif = abs(grupo) * 5) %>% 
  mutate(dec_day = case_when( dif < 10 ~ '[0-10)', dif %in% 10:19 ~ '[10-20)',
                              dif %in% 20:29 ~ '[20-30)', dif %in% 30:39 ~ '[30-40)',
                              dif %in% 40:49 ~ '[40-50)', dif %in% 50:60 ~ '[50-60]')) 



ggplot(a, aes(x = dec_day)) + 
  geom_bar(position = 'dodge') + 
  facet_grid(ciclo ~ zone)




# =-----------------------------------------------------------    
# Graph to sowing date. 
# =-----------------------------------------------------------

max_hwam_RUNNO <- function(data2){
  
  # data2 <-  test_filter  %>%  dplyr::select(-cat) %>% nest(-zone, -year, -ciclo, -date, -grupo) %>%
  # dplyr::select(data) %>% filter(row_number() == 1) %>% unnest()
  
  obs <- data2 %>%
    dplyr::select(variety, HWAM) %>% 
    unique() %>% 
    arrange(desc(HWAM)) %>% 
    slice(1) %>% 
    mutate(type = 'obs', RUNNO = 0) %>% 
    dplyr::select(-variety)
  
  
  forecast <-  data2 %>%
    dplyr::select( -pd, -HWAM) %>% 
    group_by(RUNNO) %>% 
    summarise(HWAM.E = max(HWAM.E)) %>% 
    # arrange(desc(HWAM.E)) %>% 
    # slice(1)  %>% 
    mutate(type = 'forecast') %>% 
    rename('HWAM' = 'HWAM.E')
  
  all <- bind_rows(obs, forecast)
  return(all)}

data2 <- test_filter  %>%  
  dplyr::select(-cat) %>% 
  # nest(-zone, -year, -ciclo, -order_date ,-date, -id) %>%
  nest(-zone, -year, -ciclo, -order_date ,-date, -grupo) %>% 
  mutate(test = purrr::map(.x = data, .f = max_hwam_RUNNO)) 


freq_n <- function(data){
  # test <- data3 %>% filter(row_number() == 1) %>% dplyr::select(data) %>% unnest() %>% mutate(grupo = as.numeric(grupo))
  
  a <- data %>% 
    filter(type == 'obs') %>% 
    arrange(desc(HWAM)) %>% 
    slice(1)
  
  b <- data %>% 
    filter(type == 'forecast') %>%
    nest(-RUNNO) %>% 
    mutate(new_max = purrr::map(.x = data, .f = function(x){x %>% arrange(desc(HWAM)) %>% slice(1)})) %>%
    dplyr::select(-data) %>%
    unnest()
  
  c <- b %>%
    dplyr::select(-type) %>%
    group_by(order_date, date, grupo) %>%
    summarise(freq_n = n()/99 * 100, HWAM = mean(HWAM)) %>% 
    mutate(dif_grupo = abs(grupo - a$grupo)* 5, 
           dif_order_date = order_date - a$order_date) %>% 
    ungroup()
  
  return(c)}


data3 <- data2 %>% 
  dplyr::select(-data) %>% 
  unnest() %>% 
  mutate(grupo = as.numeric(grupo)) %>% 
  nest(-zone, -year, -ciclo) %>% 
  mutate(freq_n = purrr::map(.x = data, .f = freq_n))



test <- data3 %>% 
  dplyr::select(-data) %>% 
  unnest() %>% 
  mutate(decaday = case_when(dif_grupo < 10 ~ '[0, 10)', 
                             10 <= dif_grupo & dif_grupo < 20 ~ '[10, 20)', 
                             20 <= dif_grupo & dif_grupo < 30 ~ '[20, 30)', 
                             30 <= dif_grupo & dif_grupo < 40 ~ '[30, 40)', 
                             40 <= dif_grupo & dif_grupo < 50 ~ '[40, 50)', 
                             50 <= dif_grupo & dif_grupo <= 60 ~ '[50, 60]'))



# Type 1. --- only for visualization. 
ggplot(test, aes(x = decaday, y = freq_n, fill = as.factor(ciclo))) +
  geom_bar(stat = 'identity', position = 'dodge') +
  facet_grid(zone~year) +
  scale_fill_manual( 'Cycle' , values = c("gray30", "#008080")) +
  scale_colour_manual( 'Cycle' , values = c("gray30", "#008080")) +
  labs(x = 'days', y = 'frequency (%)', fill = 'Cycle') +
  ylim(c(0, 100)) +
  theme_bw() +
  theme(legend.position = 'top')


# Type 2. 
test %>% 
  group_by(zone, ciclo, decaday) %>% 
  summarise(mean_freq = mean(freq_n, na.rm = TRUE)) %>%
  ggplot() + 
  geom_bar(aes(x = decaday, y = mean_freq), stat = 'identity' , alpha = 0.6,position = 'dodge') +
  geom_point(data = test,
             aes(decaday, freq_n, colour = as.character(year), shape = as.character(year),
                 group = as.character(year)), size = 3.3) +
  scale_colour_viridis_d() +
  scale_shape_manual(values = c(1:9)) + 
  facet_grid(ciclo ~zone, labeller = labeller(ciclo = C,  zone = Z)) + 
  ylim(c(0, 100)) +
  labs(x = 'Days', y = 'Frequency (%)', shape = 'Year', colour = 'Year') + 
  guides(col = guide_legend(nrow = 1)) + 
  theme_bw() + 
  theme(legend.position = 'top')


ggsave(filename = 'graphs/sowing_date_type2.png', height = 6.5, width = 10, dpi = 300)
ggsave(filename = 'graphs/sowing_date_type2.pdf', height = 6.5, width = 10, dpi = 200)



# Type 3. 
b <- test %>%
  group_by(year, zone, ciclo, decaday) %>% 
  summarise(freq_n = sum(freq_n)) %>% 
  ungroup %>% 
  group_by(zone, ciclo, decaday) %>% 
  summarise(mean_freq = median(freq_n, na.rm = TRUE), min_freq = min(freq_n), max_freq = max(freq_n))


ggplot(data = b) + 
  geom_bar(aes(x = decaday, y = mean_freq, fill = as.factor(ciclo)),  alpha = 0.6,stat = 'identity', position = 'dodge') + 
  geom_errorbar(aes(x = decaday, ymin = min_freq, ymax = max_freq, colour = as.factor(ciclo)), position = "dodge") + 
  scale_fill_manual( 'Cycle' , values = c("gray30", "#008080")) +
  scale_colour_manual( 'Cycle' , values = c("gray30", "#008080")) + 
  facet_wrap(~zone, , labeller = labeller( zone = Z)) + 
  labs(x = 'Days', y = 'Frequency (%)', fill = 'Cycle') + 
  ylim(c(0, 100)) +
  theme_bw()

ggsave(filename = 'graphs/sowing_date_type3.png', height = 3.5, width = 10, dpi = 300)
ggsave(filename = 'graphs/sowing_date_type3.pdf', height = 3.5, width = 10, dpi = 200)

# Type 4
ggplot(data = b) +
  geom_linerange(aes(x = decaday, ymin = min_freq, ymax = max_freq, colour = as.factor(ciclo)), position = position_dodge(width = 0.5)) +
  geom_point(aes(x = decaday, y = mean_freq, colour = as.factor(ciclo)) , position = position_dodge(width = 0.5) ) +
  scale_colour_manual( 'Cycle' , values = c("gray30", "#008080")) + 
  facet_wrap(~zone, , labeller = labeller( zone = Z)) + 
  labs(x = 'Days', y = 'Frequency (%)', fill = 'Cycle') + 
  ylim(c(0, 100)) +
  theme_bw()
ggsave(filename = 'graphs/sowing_date_type4.png', height = 3.5, width = 10, dpi = 300)
ggsave(filename = 'graphs/sowing_date_type4.pdf', height = 3.5, width = 10, dpi = 200)

# Type 5. 
ggplot() +
  geom_errorbar(data = b, aes(x = decaday, ymin = min_freq, ymax = max_freq, colour = as.factor(ciclo)), position = position_dodge(width = 0.5)) +
  geom_point(data = test, aes(decaday, freq_n, colour = as.character(ciclo), shape = as.character(year) ), 
             position = position_dodge(width = 0.5) ) +
  scale_colour_manual( 'Cycle' , values = c("gray30", "#008080")) + 
  facet_wrap(~zone, , labeller = labeller( zone = Z)) + 
  scale_shape_manual(values = c(1:9)) + 
  ylim(c(0, 100)) +
  labs(x = 'Days', y = 'Frequency (%)', fill = 'Cycle', shape = 'Year') + 
  theme_bw()

ggsave(filename = 'graphs/sowing_date_type5.png', height = 4.5, width = 10, dpi = 300)
ggsave(filename = 'graphs/sowing_date_type5.pdf', height = 4.5, width = 10, dpi = 200)



# Type 6....

print.numeric<-function(x, digits = 1) formatC(x, digits = digits, format = "f")

filter_test_b <- test %>% 
  group_by(year, zone, ciclo, decaday) %>% 
  summarise(sum_freq = sum(freq_n) ) %>% 
  mutate(var = ifelse(sum_freq > 50, 'a', 'b')) %>% 
  filter(var == 'b') %>%
  mutate(req = print.numeric(sum_freq) )

filter_test_a <- test %>% 
  group_by(year, zone, ciclo, decaday) %>% 
  summarise(sum_freq = sum(freq_n)) %>% 
  mutate(var = ifelse(sum_freq > 50, 'a', 'b')) %>% 
  filter(var == 'a') %>% 
  # mutate(sum_freq = round(sum_freq, 1) ) %>% 
  mutate(req = print.numeric(sum_freq) )



test %>% 
  group_by(year, zone, ciclo, decaday) %>% 
  summarise(sum_freq = sum(freq_n) ) %>% 
  mutate(var = ifelse(sum_freq > 50, 'a', 'b')) %>% 
  ggplot(aes(x = decaday, y = as.character(year), fill = sum_freq)) +
  geom_tile()  +
  geom_text(data = filter_test_b, aes(label = req), size = 3,show.legend = FALSE) +
  geom_text(data = filter_test_a, aes(label = req), fontface = 'bold',show.legend = FALSE) + # show_guide  = F
  scale_fill_gradient(low = "white", high = "#008080", limits = c(0,100)) +
  labs(x = 'Days', y = 'Year', fill = 'Freq. (%)', colour = NULL) +
  facet_grid(ciclo~zone,  labeller = labeller(ciclo = C,  zone = Z)) +
  theme_bw()  #+ theme(legend.position = 'top')


ggsave(filename = 'graphs/sowing_date_type6.png', height = 5.5, width = 10.5, dpi = 300)
ggsave(filename = 'graphs/sowing_date_type6.pdf', height = 5.5, width = 10.5, dpi = 200)
