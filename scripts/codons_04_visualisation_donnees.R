# Ateliers codons!
# 04 - Visualisation de donnees
# Jeudi 30/06/2022

# Charger le Tidyverse ----

library(animation)
library(tidyverse)

# Definir le repertoire de travail ----

setwd("D:/codons/C-04-VisualisationDonnees")

# Importer les donnees ----

pingouins <- readr::read_csv("https://raw.githubusercontent.com/codons-blog/C-04-VisualisationDonnees/main/data/pingouins.csv")

# Nuage de points - etape par etape ----

p1 <- ggplot(data = pingouins)

p2 <- ggplot(data = pingouins,
       mapping = aes(x = bec_lng_mm,
                     y = bec_htr_mm))

p3 <- ggplot(data = pingouins,
       mapping = aes(x = bec_lng_mm,
                     y = bec_htr_mm)) +
  geom_point()

p4 <- ggplot(data = pingouins,
       mapping = aes(x = bec_lng_mm,
                     y = bec_htr_mm)) +
  geom_point(mapping = aes(colour = espece))

p5 <- ggplot(data = pingouins,
       mapping = aes(x = bec_lng_mm,
                     y = bec_htr_mm)) +
  geom_point(mapping = aes(colour = espece),
             size = 2)

p6 <- ggplot(data = pingouins,
       mapping = aes(x = bec_lng_mm,
                     y = bec_htr_mm)) +
  geom_point(mapping = aes(colour = espece),
             size = 2,
             alpha = 0.8)

p7 <- ggplot(data = pingouins,
       mapping = aes(x = bec_lng_mm,
                     y = bec_htr_mm)) +
  geom_point(mapping = aes(colour = espece),
             size = 2,
             alpha = 0.8) +
  scale_colour_manual(values = c("darkorange", "purple", "cyan4"))

p8 <- ggplot(data = pingouins,
       mapping = aes(x = bec_lng_mm,
                     y = bec_htr_mm)) +
  geom_point(mapping = aes(colour = espece),
             size = 2,
             alpha = 0.8) +
  scale_colour_manual(values = c("darkorange", "purple", "cyan4")) +
  labs(title = "Dimensions du bec pour trois espèces de pingouins")

p9 <- ggplot(data = pingouins,
       mapping = aes(x = bec_lng_mm,
                     y = bec_htr_mm)) +
  geom_point(mapping = aes(colour = espece),
             size = 2,
             alpha = 0.8) +
  scale_colour_manual(values = c("darkorange", "purple", "cyan4")) +
  labs(title = "Dimensions du bec pour trois espèces de pingouins",
       subtitle = "Sur trois îles de l'archipel Palmer (Antarctique)")

p10 <- ggplot(data = pingouins,
       mapping = aes(x = bec_lng_mm,
                     y = bec_htr_mm)) +
  geom_point(mapping = aes(colour = espece),
             size = 2,
             alpha = 0.8) +
  scale_colour_manual(values = c("darkorange", "purple", "cyan4")) +
  labs(title = "Dimensions du bec pour trois espèces de pingouins",
       subtitle = "Sur trois îles de l'archipel Palmer (Antarctique)",
       x = "Longueur du bec (mm)")

p11 <- ggplot(data = pingouins,
       mapping = aes(x = bec_lng_mm,
                     y = bec_htr_mm)) +
  geom_point(mapping = aes(colour = espece),
             size = 2,
             alpha = 0.8) +
  scale_colour_manual(values = c("darkorange", "purple", "cyan4")) +
  labs(title = "Dimensions du bec pour trois espèces de pingouins",
       subtitle = "Sur trois îles de l'archipel Palmer (Antarctique)",
       x = "Longueur du bec (mm)",
       y = "Hauteur du bec (mm)")

p12 <- ggplot(data = pingouins,
       mapping = aes(x = bec_lng_mm,
                     y = bec_htr_mm)) +
  geom_point(mapping = aes(colour = espece),
             size = 2,
             alpha = 0.8) +
  scale_colour_manual(values = c("darkorange", "purple", "cyan4")) +
  labs(title = "Dimensions du bec pour trois espèces de pingouins",
       subtitle = "Sur trois îles de l'archipel Palmer (Antarctique)",
       x = "Longueur du bec (mm)",
       y = "Hauteur du bec (mm)") +
  theme_light()

p13 <- ggplot(data = pingouins,
       mapping = aes(x = bec_lng_mm,
                     y = bec_htr_mm)) +
  geom_point(mapping = aes(colour = espece),
             size = 2,
             alpha = 0.8) +
  scale_colour_manual(values = c("darkorange", "purple", "cyan4")) +
  labs(title = "Dimensions du bec pour trois espèces de pingouins",
       subtitle = "Sur trois îles de l'archipel Palmer (Antarctique)",
       x = "Longueur du bec (mm)",
       y = "Hauteur du bec (mm)") +
  theme_light() +
  theme(panel.grid.minor = element_blank())


p14 <- ggplot(data = pingouins,
       mapping = aes(x = bec_lng_mm,
                     y = bec_htr_mm)) +
  geom_point(mapping = aes(colour = espece),
             size = 2,
             alpha = 0.8) +
  scale_colour_manual(values = c("darkorange", "purple", "cyan4")) +
  labs(title = "Dimensions du bec pour trois espèces de pingouins",
       subtitle = "Sur trois îles de l'archipel Palmer (Antarctique)",
       x = "Longueur du bec (mm)",
       y = "Hauteur du bec (mm)") +
  theme_light() +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5))

p15 <- ggplot(data = pingouins,
       mapping = aes(x = bec_lng_mm,
                     y = bec_htr_mm)) +
  geom_point(mapping = aes(colour = espece),
             size = 2,
             alpha = 0.8) +
  scale_colour_manual(values = c("darkorange", "purple", "cyan4")) +
  labs(title = "Dimensions du bec pour trois espèces de pingouins",
       subtitle = "Sur trois îles de l'archipel Palmer (Antarctique)",
       x = "Longueur du bec (mm)",
       y = "Hauteur du bec (mm)") +
  theme_light() +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5,
                                  margin = margin(t = 10, b = 5)))

p16 <- ggplot(data = pingouins,
       mapping = aes(x = bec_lng_mm,
                     y = bec_htr_mm)) +
  geom_point(mapping = aes(colour = espece),
             size = 2,
             alpha = 0.8) +
  scale_colour_manual(values = c("darkorange", "purple", "cyan4")) +
  labs(title = "Dimensions du bec pour trois espèces de pingouins",
       subtitle = "Sur trois îles de l'archipel Palmer (Antarctique)",
       x = "Longueur du bec (mm)",
       y = "Hauteur du bec (mm)") +
  theme_light() +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5,
                                  margin = margin(t = 10, b = 5)),
        plot.subtitle = element_text(hjust = 0.5))

p17 <- ggplot(data = pingouins,
       mapping = aes(x = bec_lng_mm,
                     y = bec_htr_mm)) +
  geom_point(mapping = aes(colour = espece),
             size = 2,
             alpha = 0.8) +
  scale_colour_manual(values = c("darkorange", "purple", "cyan4")) +
  labs(title = "Dimensions du bec pour trois espèces de pingouins",
       subtitle = "Sur trois îles de l'archipel Palmer (Antarctique)",
       x = "Longueur du bec (mm)",
       y = "Hauteur du bec (mm)") +
  theme_light() +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5,
                                  margin = margin(t = 10, b = 5)),
        plot.subtitle = element_text(hjust = 0.5,
                                     margin = margin(b = 20)))

p18 <- ggplot(data = pingouins,
       mapping = aes(x = bec_lng_mm,
                     y = bec_htr_mm)) +
  geom_point(mapping = aes(colour = espece),
             size = 2,
             alpha = 0.8) +
  scale_colour_manual(values = c("darkorange", "purple", "cyan4")) +
  labs(title = "Dimensions du bec pour trois espèces de pingouins",
       subtitle = "Sur trois îles de l'archipel Palmer (Antarctique)",
       x = "Longueur du bec (mm)",
       y = "Hauteur du bec (mm)") +
  theme_light() +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5,
                                  margin = margin(t = 10, b = 5)),
        plot.subtitle = element_text(hjust = 0.5,
                                     margin = margin(b = 20)),
        axis.title.x = element_text(margin = margin(t = 10)))

p19 <- ggplot(data = pingouins,
       mapping = aes(x = bec_lng_mm,
                     y = bec_htr_mm)) +
  geom_point(mapping = aes(colour = espece),
             size = 2,
             alpha = 0.8) +
  scale_colour_manual(values = c("darkorange", "purple", "cyan4")) +
  labs(title = "Dimensions du bec pour trois espèces de pingouins",
       subtitle = "Sur trois îles de l'archipel Palmer (Antarctique)",
       x = "Longueur du bec (mm)",
       y = "Hauteur du bec (mm)") +
  theme_light() +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5,
                                  margin = margin(t = 10, b = 5)),
        plot.subtitle = element_text(hjust = 0.5,
                                     margin = margin(b = 20)),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10)))

p20 <- ggplot(data = pingouins,
       mapping = aes(x = bec_lng_mm,
                     y = bec_htr_mm)) +
  geom_point(mapping = aes(colour = espece),
             size = 2,
             alpha = 0.8) +
  scale_colour_manual(values = c("darkorange", "purple", "cyan4")) +
  labs(title = "Dimensions du bec pour trois espèces de pingouins",
       subtitle = "Sur trois îles de l'archipel Palmer (Antarctique)",
       x = "Longueur du bec (mm)",
       y = "Hauteur du bec (mm)") +
  theme_light() +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5,
                                  margin = margin(t = 10, b = 5)),
        plot.subtitle = element_text(hjust = 0.5,
                                     margin = margin(b = 20)),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10)),
        legend.position = "top")

p21 <- ggplot(data = pingouins,
       mapping = aes(x = bec_lng_mm,
                     y = bec_htr_mm)) +
  geom_point(mapping = aes(colour = espece),
             size = 2,
             alpha = 0.8) +
  scale_colour_manual(values = c("darkorange", "purple", "cyan4")) +
  labs(title = "Dimensions du bec pour trois espèces de pingouins",
       subtitle = "Sur trois îles de l'archipel Palmer (Antarctique)",
       x = "Longueur du bec (mm)",
       y = "Hauteur du bec (mm)") +
  theme_light() +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5,
                                  margin = margin(t = 10, b = 5)),
        plot.subtitle = element_text(hjust = 0.5,
                                     margin = margin(b = 20)),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10)),
        legend.position = "top",
        legend.title = element_blank())

saveGIF({
  print(p1)
  print(p2)
  print(p3)
  print(p4)
  print(p5)
  print(p6)
  print(p7)
  print(p8)
  print(p9)
  print(p10)
  print(p11)
  print(p12)
  print(p13)
  print(p14)
  print(p15)
  print(p16)
  print(p17)
  print(p18)
  print(p19)
  print(p20)
  print(p21)
}, interval = 1, movie.name = "test.gif", ani.width = 1800, ani.height = 1050, ani.res = 300)



# Histogramme ----

ggplot(data = pingouins,
       aes(x = bec_lng_mm)) +
  geom_histogram(colour = "blue", fill = "lightblue") +
  geom_vline(aes(xintercept = mean(bec_lng_mm)),
             colour = "red", linetype = "dashed", size = 1) +
  labs(x = "Longueur des ailes (mm)",
       y = "Nombre")

# Nuage de points ----

library(patchwork)

p1 <- ggplot(data = pingouins)

c1 <- ggplot() +
  geom_text(aes(x = 0, y = 10, label = "ggplot(data = pingouins)"),
            hjust = 0) +
  xlim(c(0, 10)) +
  ylim(c(0, 10)) +
  theme_void()

g1 <- c1 + p1 +
  patchwork::plot_layout(widths = c(1, 2))

p2 <- ggplot(data = pingouins,
             aes(x = bec_lng_mm, y = bec_htr_mm))

c2 <- ggplot() +
  geom_text(aes(x = 0, y = 10, label = "ggplot(data = pingouins, mapping = aes(x = bec_lng_mm, y = bec_htr_mm)"),
            hjust = 0) +
  geom_text(aes(x = 5, y = 9, label = ""),
            hjust = 0) +
  xlim(c(0, 10)) +
  ylim(c(0, 10)) +
  theme_void()

g2 <- c2 + p2 +
  patchwork::plot_layout(widths = c(1, 2))

p3 <- ggplot(data = pingouins,
       aes(x = bec_lng_mm, y = bec_htr_mm)) +
  geom_line()

p4 <- ggplot(data = pingouins,
             aes(x = bec_lng_mm, y = bec_htr_mm)) +
  geom_point(aes(colour = espece))

p5 <- ggplot(data = pingouins,
             aes(x = bec_lng_mm, y = bec_htr_mm)) +
  geom_point(aes(colour = espece),
             size = 2)

p6 <- ggplot(data = pingouins,
             aes(x = bec_lng_mm, y = bec_htr_mm)) +
  geom_point(aes(colour = espece),
             size = 2, alpha = 0.8)



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
  
