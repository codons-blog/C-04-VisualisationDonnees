# Ateliers codons!
# 04 - Visualisation de donnees
# Jeudi 30/06/2022

# Charger le Tidyverse ----

library(tidyverse)

# Definir le repertoire de travail ----

setwd("D:/codons/C-04-VisualisationDonnees")

# Importer les donnees ----

pingouins <- readr::read_csv("https://raw.githubusercontent.com/codons-blog/C-04-VisualisationDonnees/main/data/pingouins.csv")

# Histogramme ----

ggplot(data = pingouins,
       aes(x = bec_lng_mm)) +
  geom_histogram(colour = "blue", fill = "lightblue") +
  geom_vline(aes(xintercept = mean(bec_lng_mm)),
             colour = "red", linetype = "dashed", size = 1) +
  labs(x = "Longueur des ailes (mm)",
       y = "Nombre")

# Nuage de points ----

ggplot(data = pingouins,
       aes(x = bec_lng_mm, y = bec_htr_mm)) +
  geom_point(aes(colour = espece),
             size = 2,
             alpha = 0.8) +
  geom_smooth(method = "lm",
              aes(colour = espece),
              se = FALSE, show.legend = FALSE) +
  scale_colour_manual(values = c("darkorange", "purple", "cyan4")) +
  labs(x = "Longueur du bec (mm)",
       y = "Hauteur du bec (mm)",
       title = "Dimensions du bec pour trois espèces de pingouins") +
  theme_light() +
  theme(legend.title = element_blank(),
        legend.position = "top",
        plot.title = element_text(hjust = 0.5))

# Boîtes à moustaches ----

ggplot(data = pingouins,
       aes(x = espece, y = aile_lng_mm)) +
  geom_boxplot(aes(fill = espece), show.legend = FALSE) +
  scale_fill_manual(values = c("darkorange", "purple", "cyan4")) +
  labs(x = "",
       y = "Longueur de l'aile (mm)",
       title = "Longueur des ailes pour trois espèces de pingouins") +
  theme_light() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(margin = margin(r = 10)),
        plot.title = element_text(hjust = 0.5,
                                  margin = margin(t = 10, b = 20)))
  
