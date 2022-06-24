# Atelier codons!
# 04 - Visualisation de donnees avec ggplot2
# 2022-06-30

# Charger le Tidyverse ----

library(tidyverse)

# Definir le repertoire de travail ----

setwd("D:/codons/C-04-VisualisationDonnees")

# Importer le jeu de donnees ----

pingouins <- read_csv("https://raw.githubusercontent.com/codons-blog/C-04-VisualisationDonnees/main/pingouins.csv")

# Exploration rapide des donnees ----

head(pingouins)  # affiche les premieres lignes
tail(pingouins)  # affiche les dernieres lignes
str(pingouins)  # structure du jeu de donnees
glimpse(pingouins)  # type de variables + premiers elements de chaque variable

# 1 - scatter plot ----

ggplot(data = pingouins,
       mapping = aes(x = bec_lngr_mm,
                     y = bec_htr_mm,
                     colour = espece)) +
  geom_point(size = 3,
             alpha = 0.5) +
  scale_colour_manual(values = c("darkorange", "purple", "cyan4")) +
  theme_minimal()

# 2 - histogram ----

ggplot(data = pingouins,
       mapping = aes(x = aile_lngr_mm)) +
  geom_histogram(aes(fill = espece),
                 position = "identity",
                 alpha = 0.5) +
  scale_fill_manual(values = c("darkorange", "purple", "cyan4")) +
  theme_minimal()

# 3 - box plot ----

ggplot(data = pingouins,
       mapping = aes(x = espece,
                     y = masse_g)) +
  geom_boxplot(aes(colour = espece),
               outlier.colour = NA) +
  scale_colour_manual(values = c("darkorange", "purple", "cyan4")) +
  theme_minimal()
  
# 4 - bar plot ----

pingouins %>% 
  mutate(espece = fct_rev(fct_infreq(espece))) %>% 
  ggplot(aes(y = espece, fill = espece)) +
  geom_bar() +
  scale_fill_manual(values = c("Adelie" = "darkorange",
                               "Chinstrap" = "purple",
                               "Gentoo" = "cyan4")) +
  theme_minimal()

ggplot(data = pingouins),
       mapping = aes(y = espece)) +
  geom_bar()

  coord_flip() +
  scale_fill_manual(values = c("darkorange", "purple", "cyan4")) +
  theme_minimal()
