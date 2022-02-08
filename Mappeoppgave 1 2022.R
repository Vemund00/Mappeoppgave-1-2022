# Mappeoppgave 1
# Samarbeid om koder med Ida Marie Hansen og Sofia Andrea Møen.
library(readr)
library(zoo)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(data.table)
library(cowplot)
library(rlang)
theme_set(theme_bw(16))

#Leser inn filen med read_lines og ser på dataen.
LowerTroposphere <- read_lines("http://vortex.nsstc.uah.edu/data/msu/v6.0/tlt/uahncdc_lt_6.0.txt")
View(LowerTroposphere)

#Ser på de øverste resultatene.
head(LowerTroposphere)

#Ser på de siste 12 radene, hvor det befinner seg data som inneholder noe 
#"unødig" informasjon i slutten av fila som ikke skal være med.
tail(LowerTroposphere, n = 12)

#Fjerner de "unødvendige" dataene.
LowerTroposphere <- head(LowerTroposphere, -12)

Lav_tropo <- read_table(LowerTroposphere)

#Plotter
Lav_tropo %>%
  mutate(date = as.Date(paste(Year, Mo, 1, sep="-"))) %>% 
  mutate(moving_average = rollmean(Globe, 13,
                                   align="left",
                                   fill=0)) %>% 
  ggplot(aes(x=date)) + 
  geom_point(aes(y=Globe), col="deepskyblue3") +
  geom_line(aes(y=Globe), col="blue") +
  geom_line(aes(y=moving_average),
            color = "red",
            size = 1)+
  scale_y_continuous(breaks = c(-0.7,-0.6,-0.5,-0.4,-0.3,-0.2,-0.1,0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9)) +
  geom_hline(yintercept = 0) +
  labs(title = "Latest Global Temps",
       x ="Latest Global Average Tropospheric Temperatures", 
       y ="T Departurefrom '91-'20 Avg (deg.C)") +
  theme_bw()+ 
  theme(axis.text.x = element_text(angle = 90))


#Oppgave 2
p1 <-
  Lav_tropo %>% select("NoPol", "Year", "Mo") %>%
  mutate(date = as.Date(paste(Year, Mo, 1, sep="-"))) %>% 
  mutate(moving_average = rollmean(NoPol, 13,
                                   align="left",
                                   fill=0)) %>% 
  ggplot(aes(x=date, y=moving_average)) +
  geom_line(col="dark green") +
  labs(x = " ",
       y = "") +
  theme_bw()

p1


data_url2 <- "http://vortex.nsstc.uah.edu/data/msu/v6.0/tmt/uahncdc_mt_6.0.txt"
Mid_tropo <- read_lines(data_url2)
Mid_tropo
Mid_tropo <- head(Mid_tropo, -12)
Mid_tropo <- df <- read_table(Mid_tropo)

p2 <-
  Mid_tropo %>% select("NoPol", "Year", "Mo") %>%
  mutate(date = as.Date(paste(Year, Mo, 1, sep="-"))) %>% 
  mutate(moving_average = rollmean(NoPol, 13,
                                   align="left",
                                   fill=0)) %>% 
  ggplot(aes(x=date, y=moving_average)) +
  geom_line(col="blue") +
  labs(x = " ",
       y = "") +
  theme_bw()

p2

data_url3 <- "http://vortex.nsstc.uah.edu/data/msu/v6.0/ttp/uahncdc_tp_6.0.txt"
Tropopause <- read_lines(data_url3)
Tropopause
Tropopause <- head(Tropopause, -12)
Tropopause <- df <- read_table(Tropopause)


p3<-
  Tropopause %>% select("NoPol", "Year", "Mo") %>% 
  mutate(date = as.Date(paste(Year, Mo, 1, sep="-"))) %>% 
  mutate(moving_average = rollmean(NoPol, 13,
                                   align="left",
                                   fill=0)) %>% 
  ggplot(aes(x=date, y=moving_average)) +
  geom_line(col="purple") +
  labs(x = " ",
       y = "") +
  theme_bw()

p3

data_url4 <- "http://vortex.nsstc.uah.edu/data/msu/v6.0/tls/uahncdc_ls_6.0.txt"
Lav_stratos <- read_lines(data_url4)
Lav_stratos
Lav_stratos <- head(Lav_stratos, -12)
Lav_stratos <- df <- read_table(Lav_stratos)

p4<-
  Lav_stratos %>% select("NoPol", "Year", "Mo") %>%
  mutate(date = as.Date(paste(Year, Mo, 1, sep="-"))) %>% 
  mutate(moving_average = rollmean(NoPol, 13,
                                   align="left",
                                   fill=0)) %>% 
  ggplot(aes(x=date, y=moving_average)) +
  geom_line( col="orange") +
  labs(x = " ",
       y = "") +
  theme_bw()

p4


rbind(p1, p2, p3, p4)

p1 <-
  Lav_tropo %>% select("NoPol", "Year", "Mo") %>%
  mutate(date = as.Date(paste(Year, Mo, 1, sep="-"))) %>% 
  mutate(moving_average = rollmean(NoPol, 13,
                                   align="left",
                                   fill=0))



data_url2 <- "http://vortex.nsstc.uah.edu/data/msu/v6.0/tmt/uahncdc_mt_6.0.txt"
Mid_tropo <- read_lines(data_url2)
Mid_tropo
Mid_tropo <- head(Mid_tropo, -12)
Mid_tropo <- df <- read_table(Mid_tropo)



p2 <-
  Mid_tropo %>% select("NoPol", "Year", "Mo") %>%
  mutate(date = as.Date(paste(Year, Mo, 1, sep="-"))) %>% 
  mutate(moving_average = rollmean(NoPol, 13,
                                   align="left",
                                   fill=0)) 

data_url3 <- "http://vortex.nsstc.uah.edu/data/msu/v6.0/ttp/uahncdc_tp_6.0.txt"
Tropopause <- read_lines(data_url3)
Tropopause
Tropopause <- head(Tropopause, -12)
Tropopause <- df <- read_table(Tropopause)


p3<-
  Tropopause %>% select("NoPol", "Year", "Mo") %>% 
  mutate(date = as.Date(paste(Year, Mo, 1, sep="-"))) %>% 
  mutate(moving_average = rollmean(NoPol, 13,
                                   align="left",
                                   fill=0))

data_url4 <- "http://vortex.nsstc.uah.edu/data/msu/v6.0/tls/uahncdc_ls_6.0.txt"
Lav_stratos <- read_lines(data_url4)
Lav_stratos
Lav_stratos <- head(Lav_stratos, -12)
Lav_stratos <- df <- read_table(Lav_stratos)

p4 <-
  Lav_stratos %>% select("NoPol", "Year", "Mo") %>%
  mutate(date = as.Date(paste(Year, Mo, 1, sep="-"))) %>% 
  mutate(moving_average = rollmean(NoPol, 13,
                                   align="left",
                                   fill=0))
alleihop <- data.frame(p1, p2, p3, p4) 

alle <- rbind(Lav_tropo, Lav_stratos, Tropopause, Mid_tropo, deparse.level = 4)

alle <- alle %>% mutate(gjennomsnittNopol = mean(NoPol))
alle <- data.frame(alle) %>% 
  select(30)

alleihop %>%
  select(date,moving_average, moving_average.1, moving_average.2, moving_average.3) %>% 
  ggplot(aes(x=date)) + 
  geom_line(aes(y=alle[1:1,]),
            color = "red",
            size = 3)+
  geom_line(aes(y=moving_average),
            color = "black",
            size = 1)+
  geom_line(aes(y=moving_average.1),
            color = "blue",
            size = 1)+
  geom_line(aes(y=moving_average.2),
            color = "green",
            size = 1)+
  geom_line(aes(y=moving_average.3),
            color = "red",
            size = 1)+
  geom_hline(yintercept = 0) +
  labs(title = "Latest Global Temps",
       x ="Latest Global Average NoPol temperatures for all datasets", 
       y ="T Departurefrom '91-'20 Avg (deg.C)") +
  theme_bw()+ 
  theme(axis.text.x = element_text(angle = 90))