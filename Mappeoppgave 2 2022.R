library(jsonlite)
library(tidyverse)
library(ggplot2)
library(dplyr)

#oppgave 1
# Laster inn .json dataen og kaller den jason1.
jason1 <- "https://static01.nyt.com/newsgraphics/2021/12/20/us-coronavirus-deaths-2021/ff0adde21623e111d8ce103fedecf7ffc7906264/scatter.json"

# Gj?r dataen om til en data.frame kallt df.
df <- fromJSON(jason1)

# Kopierer dataen fra df og lager et nytt data.frame som heter covid19.
covid19 <- df

# Lager en ny kolonne som heter fully_vaccinated, som inneholder 
# Antall fullt vaksinerte.
covid19 <- covid19 %>% 
  mutate(fully_vaccinated = fully_vaccinated_pct_of_pop*100)
# Lager en ny kolonne som heter short_name, som inneholder 
# Navnene til statene i USA, men forkortet.
covid19 <- covid19 %>%
  mutate(short_name = abbreviate(covid19$name, minlength=2))

# Plotter dataene fra covid19, med fully_vaccinated p? x-aksen og 
# Deaths_per_100k p? y-aksen.
covid19 %>% ggplot(aes(x= fully_vaccinated, y= deaths_per_100k)) +
  geom_point(aes(colour=name)) + 
  geom_smooth(method="loess", se=F) +
  labs(title= "Covid-19 relaterte d?dsfall i USA i forhold til vaksinerte og uvaksinerte", 
       x= "Prosent av populasjonen fullvaksinert", 
       y= "D?dsfall pr 100 000") +
  theme_gray()+ # Legger inn theme.
  # Legger inn short_name slik at stat navene ikke skal bli for lange.
  geom_text(aes(label = short_name), adj = -0.2)+
  annotate("text", x= 60, y =17, # Legger inn tekst "boks".
           label = "         Lower vaccination rate, 
higher death rate",
           col = "black",
           size = 4) +
  annotate("text", x= 73, y =9, # Legger inn tekst "boks".
           label = "           Higher vaccination rate, 
lower death rate",
           col = "black", # Legger inn farge p? tekst.
           size = 4)+ # Legger inn st?rrelse p? tekst.
  scale_x_continuous(breaks = c(45,50,55,60,65,70,75,80), # Bestemmer tallene p? x-aksen.
                     labels = function(x) paste0(x, "%"))+ # Endrer x-aksen til prosent.
  geom_segment(aes(x = 56.6, # Lager pil nr 1. (venstre hj?rne topp).
                   y = 17,
                   xend = 56,
                   yend = 18),
               arrow = arrow(length = unit(0.3, "cm")))+
  geom_segment(aes(x = 76, # Lager pil nr 2. (h?yre hj?rne bunn).
                   y = 8.9,
                   xend = 77,
                   yend = 8.5),
               arrow = arrow(length = unit(0.3, "cm")))
# Det vi kan se er at desto flere fullvaksinerte desto f?rre d?dsfall

#Oppgave 2

lm(deaths_per_100k ~ fully_vaccinated, data= covid19)

covid19 %>% ggplot(aes(x= fully_vaccinated, y= deaths_per_100k)) +
  geom_point(aes(colour=name)) + 
  geom_smooth(method= lm) +
  labs(title= "Covid-19 relaterte d?dsfall i USA i forhold til vaksinerte og uvaksinerte", 
       x= "Prosent av populasjonen fullvaksinert", # Tittel x-aksen 
       y= "D?dsfall pr 100 000") + # Tittel y-aksen.
  theme_gray() + # Legger inn theme.
  # Legger inn short_name slik at stat navene ikke skal bli for lange.
  geom_text(aes(label = short_name), adj = -0.2)+ 
  annotate("text", x= 60, y =17, # Legger inn tekst "boks".
           label = "         Lower vaccination rate, 
higher death rate",
           col = "black", # Farge p? tekst.
           size = 4) + # St?rrelse p? tekst.
  annotate("text", x= 73, y =9, # Legger inn tekst "boks".
           label = "           Higher vaccination rate, 
lower death rate",
           col = "black", # Farge p? tekst.
           size = 4)+ # St?rrelse p? tekst.
  scale_x_continuous(breaks = c(45,50,55,60,65,70,75,80), # Bestemmer tallene p? x-aksen.
                     labels = function(x) paste0(x, "%"))+ # Endrer x-aksen til prosent.
  geom_segment(aes(x = 56.6, # Lager pil nr 1. (venstre hj?rne topp).
                   y = 17,
                   xend = 56,
                   yend = 18),
               arrow = arrow(length = unit(0.3, "cm")))+
  geom_segment(aes(x = 76, # Lager pil nr 2. (h?yre hj?rne bunn).
                   y = 8.9,
                   xend = 77,
                   yend = 8.5),
               arrow = arrow(length = unit(0.3, "cm")))
# P? grafen nede, ser man at desto h?yere rate av "uvaksinerte" 
# d?r flere og de som er vaksinerte har lavere d?ds rate. 
# Jo flere som blir vaksinert jo lavere blir d?dsraten pr 100 000 
# kapita i prosent.
