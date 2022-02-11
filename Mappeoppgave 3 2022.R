# Mappeoppgave 3
library(tidyverse)
library(rvest)
library(ggplot2)
library(janitor)
library(stringr)

# Oppgave 1

# Leser inn dataen og kaller den car_length.
car_length <- read_html("https://www.motor.no/aktuelt/motors-store-vintertest-av-rekkevidde-pa-elbiler/217132")

#Trekker ut de to tabellene fra nettsiden og kaller de table.
tables <- car_length %>% html_table(fill = TRUE)

#Ser på lengden på tables og finner ut av at det er to tabeller.
length(tables)

#Vi skal bruke den første tabellen og velger den ved bruk av [[1]].
first_table <- tables[[1]]

#Ser på overskriftene på tabellen og ser at de er feil.
colnames(first_table) 

#Endrer overskriftene til ...
names(first_table) <- c("Modell (temp. varierte fra 0° til -10°)", "WLTP-tall", "STOPP", "Avvik")
first_table <- first_table[-1,]
glimpse(first_table)
#view(first_table)

first_table <- first_table %>% clean_names()

first_table <- as.data.frame(first_table)
first_table

first_table <- first_table[-c(19, 26), ] 

# Fjerner "km" fra ¨stopp¨ med å si at "km" = ""  ,(ingenting).
first_table <- first_table %>% 
  mutate(stopp = as.numeric(gsub("km", "", stopp)))

# Fjerner "km" og ¨alt etter¨ med å si at "km.*" = "", (ingenting).
# 
first_table <- first_table %>% 
  mutate(wltp_tall = as.numeric(gsub("km.*", "", wltp_tall)))

# Deretter plotter vi med wltp_tall på x-aksen og stopp på y-aksen.
first_table %>% 
  select(wltp_tall, stopp) %>% 
  ggplot() + geom_point(aes(x = wltp_tall, y = stopp)) + 
  ggtitle("car length") +
  ylab("stopp") + 
  xlab("wltp") +
  scale_y_continuous(limits = c(200, 600)) +
  scale_x_continuous(limits = c(200, 600)) +
  geom_abline(col = "red",
              size = 1)+
  theme_bw()

# Grafen viser 29 el-bil modeller - 2 el-biler som kom over 600 
# wltp og ble derfor fjernet fra grafen. Alle bil modellene var 
# med i den store rekkeviddetesten hvor man fant ut at alle 
# bilene hadde et avvik fra oppgitt rekkevidde på mere enn 10 
# prosent. Mange av de nye populære familiebilene rullet mellom 
# 10 og 15 prosent kortere enn lovet i vinterkulda. 
# Stopp på grafen sier noe om når bilen faktisk stoppet i testen, 
#mens wltp sier noe om rekkevidden bilen skulle ha kjørt ifølge 
# bilforhandlerne.


# Oppgave 2

# Benytter R sin lm() funksjon. Deretter bruker man “stopp” 
# som y-variabel og “wltp” som x-variabel, og spesifiser navnet 
# på datasettet ditt.lm(<navn på y-variabel> ~ <navn på x-variabel>, 
# data = <navn på datasettet>). 

lm(stopp ~ wltp_tall, data = first_table)
# Koeffisienter er vektene som minimerer summen av kvadratet av 
# feilene. Til syvende og sist ønsker vi å finne en avskjæring 
# og en helning slik at den resulterende tilpassede linjen er så 
# nært som mulig de 29 datapunktene i datasettet vårt.                    
# Dermed er (intercept) = -26.6450 avskjæringen og 
# (wltp_tall) = 0.8671 er helningen.

first_table %>% ggplot(aes(x = wltp_tall, y = stopp)) +
  geom_point(aes(colour = modell_temp_varierte_fra_0_til_10)) +
  geom_smooth(method= lm) +
  labs(title= "car length", 
       x= "wltp", 
       y= "stopp") +
  theme_gray() +
  scale_y_continuous(limits = c(200, 600)) +
  scale_x_continuous(limits = c(200, 600)) +
  geom_abline(col = "red",
              size = 1)
# Så plotter vi car_length på nytt, men denne gangen tar vi med 
# lm- funksjonen med i grafen. Dermed får vi en ny 'blå' linje 
# gående igjennom de 29 punktene som en slags gjennomsnitt linje.