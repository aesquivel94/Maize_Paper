library(tidyverse)

Proof <- read_csv("C:/Users/aesquivel/Desktop/Proof.csv")


Proof %>% 
  ggplot(aes(pd, mean_pd, fill = variety)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  theme_bw() +
  facet_wrap(ciclo~zone, scales = 'free')
  




order_pdate <- function(data){
  mini <- data %>%
    mutate(id = rep(1:12, each = 2)) 

  new_HWAM <- mini %>% 
    group_by(id) %>%
    summarise(mean_pd = mean(mean_pd))
  
  new_HWAM <- mini %>%
    filter(id %in% seq(2,12, 2)) %>%
    dplyr::select(-id ,-mean_pd) %>% 
    bind_cols(., new_HWAM)
  
return(new_HWAM)}


Proof <- Proof %>% 
  dplyr::select(-X1) %>%
  nest(-zone, -ciclo, -variety) %>%
  mutate(new_data = purrr::map(.x = data, .f = order_pdate)) %>%
  dplyr::select(-data) %>%
  unnest





Proof %>% 
  ggplot(aes(pd, mean_pd, fill = variety)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  geom_errorbar(aes(ymin=min_pd, ymax=max_pd), width=.2,
                position=position_dodge(.9))  +
  scale_fill_brewer() +
  theme_bw() +
  labs(x = 'Month - day', y = 'Yield (kg/ha)') +
  facet_wrap(ciclo~zone, scales = 'free') + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))





# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Serie de tiempo historica. 
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=


global_data <- list.files("daily_preliminar/", pattern = '.csv', full.names = TRUE) %>% 
  as_tibble() %>% 
  mutate(name = list.files("daily_preliminar/", pattern = '.csv') %>% str_remove('.csv')) %>% 
  mutate(base = purrr::map(.x = value, .f = read_csv)) %>% 
  dplyr::select(-value) %>% 
  unnest


global_data <- global_data %>% 
  group_by(month, year) %>% 
  summarise(precip =  sum(precip), tmin = mean(tmin), tmax = mean(tmax)) %>% 
  ungroup %>% 
  group_by(month) %>% 
  summarise(precip = mean(precip), tmin = mean(tmin), tmax = mean(tmax)) 



# =-=-=-=-=-=-=-=-=


LaUnion <- read_csv("daily_preliminar/Espinal.csv")


LaUnion <- LaUnion %>% 
  group_by(month, year) %>% 
  summarise(precip =  sum(precip), tmin = mean(tmin), tmax = mean(tmax)) %>% 
  ungroup %>% 
  group_by(month) %>% 
  summarise(precip = mean(precip), tmin = mean(tmin), tmax = mean(tmax)) 


test <- LaUnion %>%  
  gather(type, Temperature, -month, -precip) 



round(max(test$precip), 0)/round(max(test$Temperature), 0)


gp1 <- test %>%
  ggplot() + 
  geom_bar(mapping = aes(x = month, y = precip * round(max(test$Temperature), 0) / round(max(test$precip), 0) ), 
           stat = "identity", fill = '#A4A4A4', alpha = 0.8) + 
  geom_point(mapping = aes(x = month, y = Temperature, colour = type )) + 
  geom_line(mapping = aes(x = month, y = Temperature, colour = type, group = factor(type))) +
  scale_y_continuous(name = expression("Temperature ("~degree~"C)"), limits = c(0, round(max(test$Temperature), 0) + 1)) +
  scale_colour_manual( breaks=c('tmax', 'tmin'),
                       labels=c('T. Max', 'T. Min'),
                       values = c("red", "blue")) +
  scale_x_continuous( breaks = 1:12, labels = month.abb)

gp1 <- gp1 %+% 
  scale_y_continuous(name = expression("Temperature ("~degree~"C)"),
                                  sec.axis = sec_axis(~ . * round(max(test$precip), 0) / round(max(test$Temperature), 0) , 
                                                      name = "Precipitation (mm)"), 
                                  limits = c(0, round(max(test$Temperature), 0) + 1)) +
  theme_bw() +
  labs(x = '', colour = '') +
  theme(legend.position = 'none', 
        text = element_text(size=15), 
        axis.text=element_text(colour="black"))

gp1  



ggsave(filename = 'daily_preliminar/laUnion.png', height = 5, 
       width = 8, dpi = 300)







# =-=-=-=-=-=-=-=-=-=--=-=-=-=

# M.factor <- round(max(global_data$tmax), 0) / round(max(global_data$precip), 0)
M.factor <- round(max(test$Temperature), 0) / round(max(test$precip), 0)

# id <-  


test %>%
  ggplot() + 
  geom_bar(mapping = aes(x = month, y = precip * M.factor), 
           stat = "identity", fill = '#A4A4A4', alpha = 0.8) + 
  geom_point(mapping = aes(x = month, y = Temperature, colour = type )) + 
  geom_line(mapping = aes(x = month, y = Temperature, colour = type, group = factor(type))) +
  scale_y_continuous(name = expression("Temperature ("~degree~"C)"), 
                     limits = c(0, round(max(global_data$tmax, 0) + 1)))  +
  scale_colour_manual( breaks=c('tmax', 'tmin'),
                       labels=c('T. Max', 'T. Min'),
                       values = c("red", "blue")) +
  scale_x_continuous( breaks = 1:12, labels = month.abb)



id <- id %+% 
  scale_y_continuous(name = expression("Temperature ("~degree~"C)"),
                     sec.axis = sec_axis(~ . * round(max(test$precip), 0) / round(max(test$Temperature), 0) , 
                                         name = "Precipitation (mm)"), 
                     limits = c(0, round(max(global_data$tmax), 0) + 1)) +
  theme_bw() +
  labs(x = '', colour = '') +
  theme(legend.position = 'none', 
        text = element_text(size=15), 
        axis.text=element_text(colour="black"))


id



ggsave(filename = 'daily_preliminar/laUnion.png', height = 5, 
       width = 8, dpi = 300)



# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=


path <- 'C:/Users/aesquivel/Desktop/Maize_Paper/GI'


list.files(path, recursive = TRUE, pattern = 'GI', full.names = TRUE)[1] 
  

GI_retro <- function(data){
  postitions <- which(is.na(data$X)) - 1
  
  data <- data %>% 
    filter(row_number() %in% postitions) %>% 
    filter(X != 'Training', X != 'CURRENT', X != 'X') %>%
    .[, -(1:4)] %>% 
    mutate(year = 2005:2013) 
return(data)}




GI <- read_table2("GI/Cerete_initial_Jul/GI_M1_Cerete_ASO_month_Jul.txt", 
                    skip = 5)



# list.files(path, recursive = TRUE, pattern = 'GI', full.names = TRUE)[1] %>% 
#   read_table2(., skip = 5) %>% 
#   GI_retro





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






all_GI <- all_GI %>% 
  mutate(summary_file = purrr::map(.x = file, .f = character)) %>% 
  dplyr::select(-file) %>% 
  unnest %>% 
  mutate(Index_1 = as.numeric(Index_1)) %>%
  mutate(ciclo = ifelse(IC %in% c("Jan", "Feb",  "Mar"), 'A', 'B' ))



all_GI %>% 
  write_csv('GI_retro.csv')



all_GI <- all_GI %>%
  group_by(place, ciclo,  M,season) %>% 
  summarise(mean = mean(Index_1), sd = sd(Index_1)) 


all_GI <- all_GI %>% 
  ungroup() %>% 
  separate(col = M, into = c('M', 'Month'), sep = 'M') %>% 
  dplyr::select(-M)



  ggplot(all_GI, aes(x = Month, y = mean)) + 
  geom_bar(stat = 'identity', position = 'dodge',  fill = 'skyblue1', colour = 'black') + 
  geom_errorbar(data = all_GI, aes(ymin = mean-sd, ymax = mean+sd), width=.3 )+
  labs(y = 'Goodness Index - Mean') + 
  facet_grid(ciclo~place, 
             labeller=labeller(place = as_labeller(c('Cerete' = 'Cordoba','Espinal' = 'Tolima', 'LaUnion'  = 'Valle del Cauca'))) )+ 
  theme_bw()



ggsave(filename = 'GI.png', height = 5, 
         width = 10, dpi = 300)
  

ggsave(filename = 'GI.pdf', height = 5, 
       width = 10, dpi = 200)







# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=


