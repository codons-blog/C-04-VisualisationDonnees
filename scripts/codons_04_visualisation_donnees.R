# Ateliers codons!
# 04 - Visualisation de donnees
# Jeudi 30/06/2022

# Charger le Tidyverse ----

library(animation)
library(patchwork)
library(showtext)
library(tidyverse)

# Charger les polices ----

font_add_google(name = "Source Code Pro", family = "source")
showtext_auto()

# Definir le repertoire de travail ----

setwd("D:/codons/C-04-VisualisationDonnees")

# Importer les donnees ----

pingouins <- readr::read_csv("https://raw.githubusercontent.com/codons-blog/C-04-VisualisationDonnees/main/data/pingouins.csv")

# ggplot basique - etape par etape ----

# 1 - initialiser le plot

(f1 <- ggplot(data = pingouins))

(t1 <- ggplot() +
    geom_text(family = "source", hjust = 0, size = 6, aes(x = 0.05, y = 10, label = "ggplot(data = pingouins) + ")) +
    geom_text(family = "source", hjust = 0, size = 6, aes(x = 0.1, y = 9.5, label = "aes(x = bec_lng_mm, y = bec_htr_mm) +")) +
    geom_text(family = "source", hjust = 0, size = 6, aes(x = 0.1, y = 9, label = "geom_point() + ")) +
    geom_text(family = "source", hjust = 0, size = 6, aes(x = 0.1, y = 8.5, label = "theme_light()")) +
    geom_rect(aes(xmin = 0, xmax = 0.98, ymin = 9.8, ymax = 10.2), fill = "#00a3a6", colour = NA, alpha = 0.3) +
    xlim(c(0, 1)) +
    ylim(c(0, 10.5)) +
    theme_void())

(p1 <- t1 + f1 +
    patchwork::plot_layout(widths = c(1.3, 1.7)))

ggsave("fig1.png", p1, dpi = 320, width = 12, height = 6)

# 2 - aes()

(f2 <- ggplot(data = pingouins) +
  aes(x = bec_lng_mm,
      y = bec_htr_mm) +
    theme(axis.title = element_text(size = 20),
          axis.text = element_text(size = 20)))

(t2 <- ggplot() +
    geom_text(family = "source", hjust = 0, size = 6, aes(x = 0.05, y = 10, label = "ggplot(data = pingouins) + ")) +
    geom_text(family = "source", hjust = 0, size = 6, aes(x = 0.1, y = 9.5, label = "aes(x = bec_lng_mm, y = bec_htr_mm) +")) +
    geom_text(family = "source", hjust = 0, size = 6, aes(x = 0.1, y = 9, label = "geom_point() + ")) +
    geom_text(family = "source", hjust = 0, size = 6, aes(x = 0.1, y = 8.5, label = "theme_light()")) +
    geom_rect(aes(xmin = 0, xmax = 0.98, ymin = 9.3, ymax = 9.7), fill = "#00a3a6", colour = NA, alpha = 0.3) +
    xlim(c(0, 1)) +
    ylim(c(0, 10.5)) +
    theme_void())

(p2 <- t2 + f2 +
    patchwork::plot_layout(widths = c(1.3, 1.7)))

ggsave("fig2.png", p2, dpi = 320, width = 12, height = 6)

# 3 - geom()

(f3 <- ggplot(data = pingouins) +
    aes(x = bec_lng_mm,
        y = bec_htr_mm) +
    geom_point() +
    theme(axis.title = element_text(size = 20),
          axis.text = element_text(size = 20)))

(t3 <- ggplot() +
    geom_text(family = "source", hjust = 0, size = 6, aes(x = 0.05, y = 10, label = "ggplot(data = pingouins) + ")) +
    geom_text(family = "source", hjust = 0, size = 6, aes(x = 0.1, y = 9.5, label = "aes(x = bec_lng_mm, y = bec_htr_mm) +")) +
    geom_text(family = "source", hjust = 0, size = 6, aes(x = 0.1, y = 9, label = "geom_point() + ")) +
    geom_text(family = "source", hjust = 0, size = 6, aes(x = 0.1, y = 8.5, label = "theme_light()")) +
    geom_rect(aes(xmin = 0, xmax = 0.98, ymin = 8.8, ymax = 9.2), fill = "#00a3a6", colour = NA, alpha = 0.3) +
    xlim(c(0, 1)) +
    ylim(c(0, 10.5)) +
    theme_void())

(p3 <- t3 + f3 +
    patchwork::plot_layout(widths = c(1.3, 1.7)))

ggsave("fig3.png", p3, dpi = 320, width = 12, height = 6)

# 4 - theme()

(f4 <- ggplot(data = pingouins) +
    aes(x = bec_lng_mm,
        y = bec_htr_mm) +
    geom_point() +
    theme_light() +
    theme(axis.title = element_text(size = 20),
          axis.text = element_text(size = 20)))

(t4 <- ggplot() +
    geom_text(family = "source", hjust = 0, size = 6, aes(x = 0.05, y = 10, label = "ggplot(data = pingouins) + ")) +
    geom_text(family = "source", hjust = 0, size = 6, aes(x = 0.1, y = 9.5, label = "aes(x = bec_lng_mm, y = bec_htr_mm) +")) +
    geom_text(family = "source", hjust = 0, size = 6, aes(x = 0.1, y = 9, label = "geom_point() + ")) +
    geom_text(family = "source", hjust = 0, size = 6, aes(x = 0.1, y = 8.5, label = "theme_light()")) +
    geom_rect(aes(xmin = 0, xmax = 0.98, ymin = 8.3, ymax = 8.7), fill = "#00a3a6", colour = NA, alpha = 0.3) +
    xlim(c(0, 1)) +
    ylim(c(0, 10.5)) +
    theme_void())

(p4 <- t4 + f4 +
    patchwork::plot_layout(widths = c(1.3, 1.7)))

ggsave("fig4.png", p4, dpi = 320, width = 12, height = 6)

# GIF 

saveGIF({
  print(p1)
  print(p2)
  print(p3)
  print(p4)
}, interval = 2, movie.name = "ggplot_01.gif", ani.width = 1800, ani.height = 1050, ani.res = 300)



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
  
