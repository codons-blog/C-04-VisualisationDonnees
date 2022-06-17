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

# ggplot basique - etape par etape (GIF) ----

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



# Histogramme - a inclure dans le texte ----

(h1 <- ggplot(data = pingouins,
              aes(x = aile_lng_mm)) +
   geom_histogram())

ggsave("../codons.netlify/_posts/visualiser-donnees-ggplot2/img/hist1.png", h1, dpi = 320, width = 12, height = 6)

(h2 <- ggplot(data = pingouins,
              aes(x = aile_lng_mm)) +
    geom_histogram(colour = "blue", fill = "white"))

ggsave("../codons.netlify/_posts/visualiser-donnees-ggplot2/img/hist2.png", h2, dpi = 320, width = 12, height = 6)

(h3 <- ggplot(data = pingouins,
              aes(x = aile_lng_mm)) +
    geom_histogram(aes(fill = espece),
                   position = "identity",
                   alpha = 0.5))

ggplot(data = pingouins,
       aes(x = aile_lng_mm)) +
  geom_histogram(aes(fill = espece),
                 position = "identity",
                 alpha = 0.7)

ggsave("../codons.netlify/_posts/visualiser-donnees-ggplot2/img/hist3.png", h3, dpi = 320, width = 12, height = 6)

(h4 <- ggplot(data = pingouins,
              aes(x = aile_lng_mm)) +
    geom_histogram(aes(fill = espece),
                   position = "identity",
                   alpha = 0.5) +
    scale_fill_brewer(palette = "Set2"))
 

ggsave("../codons.netlify/_posts/visualiser-donnees-ggplot2/img/hist4.png", h4, dpi = 320, width = 12, height = 6)

(h5 <- ggplot(data = pingouins,
              aes(x = aile_lng_mm)) +
    geom_histogram(aes(fill = espece),
                   position = "identity",
                   alpha = 0.5) +
    scale_fill_manual(values = c("darkorange", "purple", "cyan4")))


ggsave("../codons.netlify/_posts/visualiser-donnees-ggplot2/img/hist5.png", h5, dpi = 320, width = 12, height = 6)

(h6 <- ggplot(data = pingouins,
              aes(x = aile_lng_mm)) +
    geom_histogram(aes(fill = espece),
                   position = "identity",
                   alpha = 0.5) +
    scale_fill_manual(values = c("darkorange", "purple", "cyan4")) +
    labs(title = "Longueur des ailes de pingouins",
         subtitle = "pour 3 espèces de l'archipel Palmer",
         x = "Longueur des ailes (mm)",
         y = "Frequence"))

ggsave("../codons.netlify/_posts/visualiser-donnees-ggplot2/img/hist6.png", h6, dpi = 320, width = 12, height = 6)

(h7 <- ggplot(data = pingouins,
              aes(x = aile_lng_mm)) +
    geom_histogram(aes(fill = espece),
                   position = "identity",
                   alpha = 0.5) +
    scale_fill_manual(values = c("darkorange", "purple", "cyan4")) +
    labs(title = "Longueur des ailes de pingouins",
         subtitle = "pour 3 espèces de l'archipel Palmer",
         x = "Longueur des ailes (mm)",
         y = "Frequence") +
    theme_minimal())

ggsave("../codons.netlify/_posts/visualiser-donnees-ggplot2/img/hist7.png", h7, dpi = 320, width = 12, height = 6)

# Histogramme - etape par etape (GIF) ----

# ggplot(data = pingouins,
#        aes(x = aile_lng_mm)) +
#   geom_histogram(
#     aes(fill = espece),
#     position = "identity",
#     alpha = 0.5) +
#   scale_fill_manual(values = c("darkorange", "purple", "cyan4")) +
#   labs(title = "Longueur des ailes de pingouins",
#        subtitle = "pour 3 espèces de l'archipel Palmer",
#        x = "Longueur des ailes (mm)",
#        y = "Frequence") +
#   theme_minimal()


(t1 <- ggplot() +
    geom_text(family = "source", hjust = 0, size = 4, aes(x = 0.05, y = 10, label = "ggplot(data = pingouins,")) +
    geom_text(family = "source", hjust = 0, size = 4, aes(x = 0.195, y = 9.5, label = "aes(x = aile_lng_mm)) +")) +
    geom_text(family = "source", hjust = 0, size = 4, aes(x = 0.1, y = 9, label = "geom_histogram(")) +
    geom_text(family = "source", hjust = 0, size = 4, aes(x = 0.14, y = 8.5, label = "aes(fill = espece),")) +
    geom_text(family = "source", hjust = 0, size = 4, aes(x = 0.14, y = 8, label = "position = \"identity\",")) +
    geom_text(family = "source", hjust = 0, size = 4, aes(x = 0.14, y = 7.5, label = "alpha = 0.5) +")) +
    geom_text(family = "source", hjust = 0, size = 4, aes(x = 0.1, y = 7, label = "scale_fill_manual = c(\"darkorange\", \"purple\", \"cyan4\")) +")) +
    geom_text(family = "source", hjust = 0, size = 4, aes(x = 0.1, y = 6.5, label = "labs(title = \"Longueur des ailes de pingouins\",")) +
    geom_text(family = "source", hjust = 0, size = 4, aes(x = 0.22, y = 6, label = "subtitle = \"pour 3 espèces de l'archipel Palmer\",")) +
    geom_text(family = "source", hjust = 0, size = 4, aes(x = 0.22, y = 5.5, label = "x = \"Longueur des ailes (mm)\",")) +
    geom_text(family = "source", hjust = 0, size = 4, aes(x = 0.22, y = 5, label = "y = \"Frequence\") +")) +
    geom_text(family = "source", hjust = 0, size = 4, aes(x = 0.1, y = 4.5, label = "theme_minimal()")) +
    geom_rect(aes(xmin = 0, xmax = 1, ymin = 9.75, ymax = 10.25), fill = "#00a3a6", colour = NA, alpha = 0.3) +
    xlim(c(0, 1)) +
    ylim(c(0, 10.5)) +
    theme_void())

(h1 <- ggplot(data = pingouins))

(p1 <- t1 + h1 +
    patchwork::plot_layout(widths = c(1.3, 1.7)))

(t2 <- ggplot() +
    geom_text(family = "source", hjust = 0, size = 4, aes(x = 0.05, y = 10, label = "ggplot(data = pingouins,")) +
    geom_text(family = "source", hjust = 0, size = 4, aes(x = 0.195, y = 9.5, label = "aes(x = aile_lng_mm)) +")) +
    geom_text(family = "source", hjust = 0, size = 4, aes(x = 0.1, y = 9, label = "geom_histogram(")) +
    geom_text(family = "source", hjust = 0, size = 4, aes(x = 0.14, y = 8.5, label = "aes(fill = espece),")) +
    geom_text(family = "source", hjust = 0, size = 4, aes(x = 0.14, y = 8, label = "position = \"identity\",")) +
    geom_text(family = "source", hjust = 0, size = 4, aes(x = 0.14, y = 7.5, label = "alpha = 0.5) +")) +
    geom_text(family = "source", hjust = 0, size = 4, aes(x = 0.1, y = 7, label = "scale_fill_manual = c(\"darkorange\", \"purple\", \"cyan4\")) +")) +
    geom_text(family = "source", hjust = 0, size = 4, aes(x = 0.1, y = 6.5, label = "labs(title = \"Longueur des ailes de pingouins\",")) +
    geom_text(family = "source", hjust = 0, size = 4, aes(x = 0.22, y = 6, label = "subtitle = \"pour 3 espèces de l'archipel Palmer\",")) +
    geom_text(family = "source", hjust = 0, size = 4, aes(x = 0.22, y = 5.5, label = "x = \"Longueur des ailes (mm)\",")) +
    geom_text(family = "source", hjust = 0, size = 4, aes(x = 0.22, y = 5, label = "y = \"Frequence\") +")) +
    geom_text(family = "source", hjust = 0, size = 4, aes(x = 0.1, y = 4.5, label = "theme_minimal()")) +
    geom_rect(aes(xmin = 0, xmax = 1, ymin = 9.25, ymax = 9.75), fill = "#00a3a6", colour = NA, alpha = 0.3) +
    xlim(c(0, 1)) +
    ylim(c(0, 10.5)) +
    theme_void())

(h2 <- ggplot(data = pingouins,
              aes(x = aile_lng_mm)))

(p2 <- t2 + h2 +
    patchwork::plot_layout(widths = c(1.3, 1.7)))

(t3 <- ggplot() +
    geom_text(family = "source", hjust = 0, size = 4, aes(x = 0.05, y = 10, label = "ggplot(data = pingouins,")) +
    geom_text(family = "source", hjust = 0, size = 4, aes(x = 0.195, y = 9.5, label = "aes(x = aile_lng_mm)) +")) +
    geom_text(family = "source", hjust = 0, size = 4, aes(x = 0.1, y = 9, label = "geom_histogram(")) +
    geom_text(family = "source", hjust = 0, size = 4, aes(x = 0.14, y = 8.5, label = "aes(fill = espece),")) +
    geom_text(family = "source", hjust = 0, size = 4, aes(x = 0.14, y = 8, label = "position = \"identity\",")) +
    geom_text(family = "source", hjust = 0, size = 4, aes(x = 0.14, y = 7.5, label = "alpha = 0.5) +")) +
    geom_text(family = "source", hjust = 0, size = 4, aes(x = 0.1, y = 7, label = "scale_fill_manual = c(\"darkorange\", \"purple\", \"cyan4\")) +")) +
    geom_text(family = "source", hjust = 0, size = 4, aes(x = 0.1, y = 6.5, label = "labs(title = \"Longueur des ailes de pingouins\",")) +
    geom_text(family = "source", hjust = 0, size = 4, aes(x = 0.22, y = 6, label = "subtitle = \"pour 3 espèces de l'archipel Palmer\",")) +
    geom_text(family = "source", hjust = 0, size = 4, aes(x = 0.22, y = 5.5, label = "x = \"Longueur des ailes (mm)\",")) +
    geom_text(family = "source", hjust = 0, size = 4, aes(x = 0.22, y = 5, label = "y = \"Frequence\") +")) +
    geom_text(family = "source", hjust = 0, size = 4, aes(x = 0.1, y = 4.5, label = "theme_minimal()")) +
    geom_rect(aes(xmin = 0, xmax = 1, ymin = 8.75, ymax = 9.25), fill = "#00a3a6", colour = NA, alpha = 0.3) +
    xlim(c(0, 1)) +
    ylim(c(0, 10.5)) +
    theme_void())

(h3 <- ggplot(data = pingouins,
              aes(x = aile_lng_mm)) +
  geom_histogram())

(p3 <- t3 + h3 +
    patchwork::plot_layout(widths = c(1.3, 1.7)))

(t4 <- ggplot() +
    geom_text(family = "source", hjust = 0, size = 4, aes(x = 0.05, y = 10, label = "ggplot(data = pingouins,")) +
    geom_text(family = "source", hjust = 0, size = 4, aes(x = 0.195, y = 9.5, label = "aes(x = aile_lng_mm)) +")) +
    geom_text(family = "source", hjust = 0, size = 4, aes(x = 0.1, y = 9, label = "geom_histogram(")) +
    geom_text(family = "source", hjust = 0, size = 4, aes(x = 0.14, y = 8.5, label = "aes(fill = espece),")) +
    geom_text(family = "source", hjust = 0, size = 4, aes(x = 0.14, y = 8, label = "position = \"identity\",")) +
    geom_text(family = "source", hjust = 0, size = 4, aes(x = 0.14, y = 7.5, label = "alpha = 0.5) +")) +
    geom_text(family = "source", hjust = 0, size = 4, aes(x = 0.1, y = 7, label = "scale_fill_manual = c(\"darkorange\", \"purple\", \"cyan4\")) +")) +
    geom_text(family = "source", hjust = 0, size = 4, aes(x = 0.1, y = 6.5, label = "labs(title = \"Longueur des ailes de pingouins\",")) +
    geom_text(family = "source", hjust = 0, size = 4, aes(x = 0.22, y = 6, label = "subtitle = \"pour 3 espèces de l'archipel Palmer\",")) +
    geom_text(family = "source", hjust = 0, size = 4, aes(x = 0.22, y = 5.5, label = "x = \"Longueur des ailes (mm)\",")) +
    geom_text(family = "source", hjust = 0, size = 4, aes(x = 0.22, y = 5, label = "y = \"Frequence\") +")) +
    geom_text(family = "source", hjust = 0, size = 4, aes(x = 0.1, y = 4.5, label = "theme_minimal()")) +
    geom_rect(aes(xmin = 0, xmax = 1, ymin = 8.25, ymax = 8.75), fill = "#00a3a6", colour = NA, alpha = 0.3) +
    xlim(c(0, 1)) +
    ylim(c(0, 10.5)) +
    theme_void())

(h4 <- ggplot(data = pingouins,
              aes(x = aile_lng_mm)) +
    geom_histogram(
      aes(fill = espece))
  )

(p4 <- t4 + h4 +
    patchwork::plot_layout(widths = c(1.3, 1.7)))

(t5 <- ggplot() +
    geom_text(family = "source", hjust = 0, size = 4, aes(x = 0.05, y = 10, label = "ggplot(data = pingouins,")) +
    geom_text(family = "source", hjust = 0, size = 4, aes(x = 0.195, y = 9.5, label = "aes(x = aile_lng_mm)) +")) +
    geom_text(family = "source", hjust = 0, size = 4, aes(x = 0.1, y = 9, label = "geom_histogram(")) +
    geom_text(family = "source", hjust = 0, size = 4, aes(x = 0.14, y = 8.5, label = "aes(fill = espece),")) +
    geom_text(family = "source", hjust = 0, size = 4, aes(x = 0.14, y = 8, label = "position = \"identity\",")) +
    geom_text(family = "source", hjust = 0, size = 4, aes(x = 0.14, y = 7.5, label = "alpha = 0.5) +")) +
    geom_text(family = "source", hjust = 0, size = 4, aes(x = 0.1, y = 7, label = "scale_fill_manual = c(\"darkorange\", \"purple\", \"cyan4\")) +")) +
    geom_text(family = "source", hjust = 0, size = 4, aes(x = 0.1, y = 6.5, label = "labs(title = \"Longueur des ailes de pingouins\",")) +
    geom_text(family = "source", hjust = 0, size = 4, aes(x = 0.22, y = 6, label = "subtitle = \"pour 3 espèces de l'archipel Palmer\",")) +
    geom_text(family = "source", hjust = 0, size = 4, aes(x = 0.22, y = 5.5, label = "x = \"Longueur des ailes (mm)\",")) +
    geom_text(family = "source", hjust = 0, size = 4, aes(x = 0.22, y = 5, label = "y = \"Frequence\") +")) +
    geom_text(family = "source", hjust = 0, size = 4, aes(x = 0.1, y = 4.5, label = "theme_minimal()")) +
    geom_rect(aes(xmin = 0, xmax = 1, ymin = 7.75, ymax = 8.25), fill = "#00a3a6", colour = NA, alpha = 0.3) +
    xlim(c(0, 1)) +
    ylim(c(0, 10.5)) +
    theme_void())

(h5 <- ggplot(data = pingouins,
              aes(x = aile_lng_mm)) +
    geom_histogram(
      aes(fill = espece),
      position = "identity")
  )

(p5 <- t5 + h5 +
    patchwork::plot_layout(widths = c(1.3, 1.7)))

(t6 <- ggplot() +
    geom_text(family = "source", hjust = 0, size = 4, aes(x = 0.05, y = 10, label = "ggplot(data = pingouins,")) +
    geom_text(family = "source", hjust = 0, size = 4, aes(x = 0.195, y = 9.5, label = "aes(x = aile_lng_mm)) +")) +
    geom_text(family = "source", hjust = 0, size = 4, aes(x = 0.1, y = 9, label = "geom_histogram(")) +
    geom_text(family = "source", hjust = 0, size = 4, aes(x = 0.14, y = 8.5, label = "aes(fill = espece),")) +
    geom_text(family = "source", hjust = 0, size = 4, aes(x = 0.14, y = 8, label = "position = \"identity\",")) +
    geom_text(family = "source", hjust = 0, size = 4, aes(x = 0.14, y = 7.5, label = "alpha = 0.5) +")) +
    geom_text(family = "source", hjust = 0, size = 4, aes(x = 0.1, y = 7, label = "scale_fill_manual = c(\"darkorange\", \"purple\", \"cyan4\")) +")) +
    geom_text(family = "source", hjust = 0, size = 4, aes(x = 0.1, y = 6.5, label = "labs(title = \"Longueur des ailes de pingouins\",")) +
    geom_text(family = "source", hjust = 0, size = 4, aes(x = 0.22, y = 6, label = "subtitle = \"pour 3 espèces de l'archipel Palmer\",")) +
    geom_text(family = "source", hjust = 0, size = 4, aes(x = 0.22, y = 5.5, label = "x = \"Longueur des ailes (mm)\",")) +
    geom_text(family = "source", hjust = 0, size = 4, aes(x = 0.22, y = 5, label = "y = \"Frequence\") +")) +
    geom_text(family = "source", hjust = 0, size = 4, aes(x = 0.1, y = 4.5, label = "theme_minimal()")) +
    geom_rect(aes(xmin = 0, xmax = 1, ymin = 7.25, ymax = 7.75), fill = "#00a3a6", colour = NA, alpha = 0.3) +
    xlim(c(0, 1)) +
    ylim(c(0, 10.5)) +
    theme_void())

(h6 <- ggplot(data = pingouins,
              aes(x = aile_lng_mm)) +
    geom_histogram(
      aes(fill = espece),
      position = "identity",
      alpha = 0.5)
  )

(p6 <- t6 + h6 +
    patchwork::plot_layout(widths = c(1.3, 1.7)))

(t7 <- ggplot() +
    geom_text(family = "source", hjust = 0, size = 4, aes(x = 0.05, y = 10, label = "ggplot(data = pingouins,")) +
    geom_text(family = "source", hjust = 0, size = 4, aes(x = 0.195, y = 9.5, label = "aes(x = aile_lng_mm)) +")) +
    geom_text(family = "source", hjust = 0, size = 4, aes(x = 0.1, y = 9, label = "geom_histogram(")) +
    geom_text(family = "source", hjust = 0, size = 4, aes(x = 0.14, y = 8.5, label = "aes(fill = espece),")) +
    geom_text(family = "source", hjust = 0, size = 4, aes(x = 0.14, y = 8, label = "position = \"identity\",")) +
    geom_text(family = "source", hjust = 0, size = 4, aes(x = 0.14, y = 7.5, label = "alpha = 0.5) +")) +
    geom_text(family = "source", hjust = 0, size = 4, aes(x = 0.1, y = 7, label = "scale_fill_manual = c(\"darkorange\", \"purple\", \"cyan4\")) +")) +
    geom_text(family = "source", hjust = 0, size = 4, aes(x = 0.1, y = 6.5, label = "labs(title = \"Longueur des ailes de pingouins\",")) +
    geom_text(family = "source", hjust = 0, size = 4, aes(x = 0.22, y = 6, label = "subtitle = \"pour 3 espèces de l'archipel Palmer\",")) +
    geom_text(family = "source", hjust = 0, size = 4, aes(x = 0.22, y = 5.5, label = "x = \"Longueur des ailes (mm)\",")) +
    geom_text(family = "source", hjust = 0, size = 4, aes(x = 0.22, y = 5, label = "y = \"Frequence\") +")) +
    geom_text(family = "source", hjust = 0, size = 4, aes(x = 0.1, y = 4.5, label = "theme_minimal()")) +
    geom_rect(aes(xmin = 0, xmax = 1, ymin = 6.75, ymax = 7.25), fill = "#00a3a6", colour = NA, alpha = 0.3) +
    xlim(c(0, 1)) +
    ylim(c(0, 10.5)) +
    theme_void())

(h7 <- ggplot(data = pingouins,
              aes(x = aile_lng_mm)) +
    geom_histogram(
      aes(fill = espece),
      position = "identity",
      alpha = 0.5) +
    scale_fill_manual(values = c("darkorange", "purple", "cyan4"))
  )

(p7 <- t7 + h7 +
    patchwork::plot_layout(widths = c(1.3, 1.7)))

(t8 <- ggplot() +
    geom_text(family = "source", hjust = 0, size = 4, aes(x = 0.05, y = 10, label = "ggplot(data = pingouins,")) +
    geom_text(family = "source", hjust = 0, size = 4, aes(x = 0.195, y = 9.5, label = "aes(x = aile_lng_mm)) +")) +
    geom_text(family = "source", hjust = 0, size = 4, aes(x = 0.1, y = 9, label = "geom_histogram(")) +
    geom_text(family = "source", hjust = 0, size = 4, aes(x = 0.14, y = 8.5, label = "aes(fill = espece),")) +
    geom_text(family = "source", hjust = 0, size = 4, aes(x = 0.14, y = 8, label = "position = \"identity\",")) +
    geom_text(family = "source", hjust = 0, size = 4, aes(x = 0.14, y = 7.5, label = "alpha = 0.5) +")) +
    geom_text(family = "source", hjust = 0, size = 4, aes(x = 0.1, y = 7, label = "scale_fill_manual = c(\"darkorange\", \"purple\", \"cyan4\")) +")) +
    geom_text(family = "source", hjust = 0, size = 4, aes(x = 0.1, y = 6.5, label = "labs(title = \"Longueur des ailes de pingouins\",")) +
    geom_text(family = "source", hjust = 0, size = 4, aes(x = 0.22, y = 6, label = "subtitle = \"pour 3 espèces de l'archipel Palmer\",")) +
    geom_text(family = "source", hjust = 0, size = 4, aes(x = 0.22, y = 5.5, label = "x = \"Longueur des ailes (mm)\",")) +
    geom_text(family = "source", hjust = 0, size = 4, aes(x = 0.22, y = 5, label = "y = \"Frequence\") +")) +
    geom_text(family = "source", hjust = 0, size = 4, aes(x = 0.1, y = 4.5, label = "theme_minimal()")) +
    geom_rect(aes(xmin = 0, xmax = 1, ymin = 6.25, ymax = 6.75), fill = "#00a3a6", colour = NA, alpha = 0.3) +
    xlim(c(0, 1)) +
    ylim(c(0, 10.5)) +
    theme_void())

(h8 <- ggplot(data = pingouins,
              aes(x = aile_lng_mm)) +
    geom_histogram(
      aes(fill = espece),
      position = "identity",
      alpha = 0.5) +
    scale_fill_manual(values = c("darkorange", "purple", "cyan4")) +
    labs(title = "Longueur des ailes de pingouins")
)

(p8 <- t8 + h8 +
    patchwork::plot_layout(widths = c(1.3, 1.7)))

(t9 <- ggplot() +
    geom_text(family = "source", hjust = 0, size = 4, aes(x = 0.05, y = 10, label = "ggplot(data = pingouins,")) +
    geom_text(family = "source", hjust = 0, size = 4, aes(x = 0.195, y = 9.5, label = "aes(x = aile_lng_mm)) +")) +
    geom_text(family = "source", hjust = 0, size = 4, aes(x = 0.1, y = 9, label = "geom_histogram(")) +
    geom_text(family = "source", hjust = 0, size = 4, aes(x = 0.14, y = 8.5, label = "aes(fill = espece),")) +
    geom_text(family = "source", hjust = 0, size = 4, aes(x = 0.14, y = 8, label = "position = \"identity\",")) +
    geom_text(family = "source", hjust = 0, size = 4, aes(x = 0.14, y = 7.5, label = "alpha = 0.5) +")) +
    geom_text(family = "source", hjust = 0, size = 4, aes(x = 0.1, y = 7, label = "scale_fill_manual = c(\"darkorange\", \"purple\", \"cyan4\")) +")) +
    geom_text(family = "source", hjust = 0, size = 4, aes(x = 0.1, y = 6.5, label = "labs(title = \"Longueur des ailes de pingouins\",")) +
    geom_text(family = "source", hjust = 0, size = 4, aes(x = 0.22, y = 6, label = "subtitle = \"pour 3 espèces de l'archipel Palmer\",")) +
    geom_text(family = "source", hjust = 0, size = 4, aes(x = 0.22, y = 5.5, label = "x = \"Longueur des ailes (mm)\",")) +
    geom_text(family = "source", hjust = 0, size = 4, aes(x = 0.22, y = 5, label = "y = \"Frequence\") +")) +
    geom_text(family = "source", hjust = 0, size = 4, aes(x = 0.1, y = 4.5, label = "theme_minimal()")) +
    geom_rect(aes(xmin = 0, xmax = 1, ymin = 5.75, ymax = 6.25), fill = "#00a3a6", colour = NA, alpha = 0.3) +
    xlim(c(0, 1)) +
    ylim(c(0, 10.5)) +
    theme_void())

(h9 <- ggplot(data = pingouins,
              aes(x = aile_lng_mm)) +
    geom_histogram(
      aes(fill = espece),
      position = "identity",
      alpha = 0.5) +
    scale_fill_manual(values = c("darkorange", "purple", "cyan4")) +
    labs(title = "Longueur des ailes de pingouins",
         subtitle = "pour 3 espèces de l'archipel Palmer")
)

(p9 <- t9 + h9 +
    patchwork::plot_layout(widths = c(1.3, 1.7)))

(t10 <- ggplot() +
    geom_text(family = "source", hjust = 0, size = 4, aes(x = 0.05, y = 10, label = "ggplot(data = pingouins,")) +
    geom_text(family = "source", hjust = 0, size = 4, aes(x = 0.195, y = 9.5, label = "aes(x = aile_lng_mm)) +")) +
    geom_text(family = "source", hjust = 0, size = 4, aes(x = 0.1, y = 9, label = "geom_histogram(")) +
    geom_text(family = "source", hjust = 0, size = 4, aes(x = 0.14, y = 8.5, label = "aes(fill = espece),")) +
    geom_text(family = "source", hjust = 0, size = 4, aes(x = 0.14, y = 8, label = "position = \"identity\",")) +
    geom_text(family = "source", hjust = 0, size = 4, aes(x = 0.14, y = 7.5, label = "alpha = 0.5) +")) +
    geom_text(family = "source", hjust = 0, size = 4, aes(x = 0.1, y = 7, label = "scale_fill_manual = c(\"darkorange\", \"purple\", \"cyan4\")) +")) +
    geom_text(family = "source", hjust = 0, size = 4, aes(x = 0.1, y = 6.5, label = "labs(title = \"Longueur des ailes de pingouins\",")) +
    geom_text(family = "source", hjust = 0, size = 4, aes(x = 0.22, y = 6, label = "subtitle = \"pour 3 espèces de l'archipel Palmer\",")) +
    geom_text(family = "source", hjust = 0, size = 4, aes(x = 0.22, y = 5.5, label = "x = \"Longueur des ailes (mm)\",")) +
    geom_text(family = "source", hjust = 0, size = 4, aes(x = 0.22, y = 5, label = "y = \"Frequence\") +")) +
    geom_text(family = "source", hjust = 0, size = 4, aes(x = 0.1, y = 4.5, label = "theme_minimal()")) +
    geom_rect(aes(xmin = 0, xmax = 1, ymin = 5.25, ymax = 5.75), fill = "#00a3a6", colour = NA, alpha = 0.3) +
    xlim(c(0, 1)) +
    ylim(c(0, 10.5)) +
    theme_void())

(h10 <- ggplot(data = pingouins,
              aes(x = aile_lng_mm)) +
    geom_histogram(
      aes(fill = espece),
      position = "identity",
      alpha = 0.5) +
    scale_fill_manual(values = c("darkorange", "purple", "cyan4")) +
    labs(title = "Longueur des ailes de pingouins",
         subtitle = "pour 3 espèces de l'archipel Palmer",
         x = "Longueur des ailes (mm)")
)

(p10 <- t10 + h10 +
    patchwork::plot_layout(widths = c(1.3, 1.7)))

(t11 <- ggplot() +
    geom_text(family = "source", hjust = 0, size = 4, aes(x = 0.05, y = 10, label = "ggplot(data = pingouins,")) +
    geom_text(family = "source", hjust = 0, size = 4, aes(x = 0.195, y = 9.5, label = "aes(x = aile_lng_mm)) +")) +
    geom_text(family = "source", hjust = 0, size = 4, aes(x = 0.1, y = 9, label = "geom_histogram(")) +
    geom_text(family = "source", hjust = 0, size = 4, aes(x = 0.14, y = 8.5, label = "aes(fill = espece),")) +
    geom_text(family = "source", hjust = 0, size = 4, aes(x = 0.14, y = 8, label = "position = \"identity\",")) +
    geom_text(family = "source", hjust = 0, size = 4, aes(x = 0.14, y = 7.5, label = "alpha = 0.5) +")) +
    geom_text(family = "source", hjust = 0, size = 4, aes(x = 0.1, y = 7, label = "scale_fill_manual = c(\"darkorange\", \"purple\", \"cyan4\")) +")) +
    geom_text(family = "source", hjust = 0, size = 4, aes(x = 0.1, y = 6.5, label = "labs(title = \"Longueur des ailes de pingouins\",")) +
    geom_text(family = "source", hjust = 0, size = 4, aes(x = 0.22, y = 6, label = "subtitle = \"pour 3 espèces de l'archipel Palmer\",")) +
    geom_text(family = "source", hjust = 0, size = 4, aes(x = 0.22, y = 5.5, label = "x = \"Longueur des ailes (mm)\",")) +
    geom_text(family = "source", hjust = 0, size = 4, aes(x = 0.22, y = 5, label = "y = \"Frequence\") +")) +
    geom_text(family = "source", hjust = 0, size = 4, aes(x = 0.1, y = 4.5, label = "theme_minimal()")) +
    geom_rect(aes(xmin = 0, xmax = 1, ymin = 4.75, ymax = 5.25), fill = "#00a3a6", colour = NA, alpha = 0.3) +
    xlim(c(0, 1)) +
    ylim(c(0, 10.5)) +
    theme_void())

(h11 <- ggplot(data = pingouins,
               aes(x = aile_lng_mm)) +
    geom_histogram(
      aes(fill = espece),
      position = "identity",
      alpha = 0.5) +
    scale_fill_manual(values = c("darkorange", "purple", "cyan4")) +
    labs(title = "Longueur des ailes de pingouins",
         subtitle = "pour 3 espèces de l'archipel Palmer",
         x = "Longueur des ailes (mm)",
         y = "Frequence")
)

(p11 <- t11 + h11 +
    patchwork::plot_layout(widths = c(1.3, 1.7)))

(t12 <- ggplot() +
    geom_text(family = "source", hjust = 0, size = 4, aes(x = 0.05, y = 10, label = "ggplot(data = pingouins,")) +
    geom_text(family = "source", hjust = 0, size = 4, aes(x = 0.195, y = 9.5, label = "aes(x = aile_lng_mm)) +")) +
    geom_text(family = "source", hjust = 0, size = 4, aes(x = 0.1, y = 9, label = "geom_histogram(")) +
    geom_text(family = "source", hjust = 0, size = 4, aes(x = 0.14, y = 8.5, label = "aes(fill = espece),")) +
    geom_text(family = "source", hjust = 0, size = 4, aes(x = 0.14, y = 8, label = "position = \"identity\",")) +
    geom_text(family = "source", hjust = 0, size = 4, aes(x = 0.14, y = 7.5, label = "alpha = 0.5) +")) +
    geom_text(family = "source", hjust = 0, size = 4, aes(x = 0.1, y = 7, label = "scale_fill_manual = c(\"darkorange\", \"purple\", \"cyan4\")) +")) +
    geom_text(family = "source", hjust = 0, size = 4, aes(x = 0.1, y = 6.5, label = "labs(title = \"Longueur des ailes de pingouins\",")) +
    geom_text(family = "source", hjust = 0, size = 4, aes(x = 0.22, y = 6, label = "subtitle = \"pour 3 espèces de l'archipel Palmer\",")) +
    geom_text(family = "source", hjust = 0, size = 4, aes(x = 0.22, y = 5.5, label = "x = \"Longueur des ailes (mm)\",")) +
    geom_text(family = "source", hjust = 0, size = 4, aes(x = 0.22, y = 5, label = "y = \"Frequence\") +")) +
    geom_text(family = "source", hjust = 0, size = 4, aes(x = 0.1, y = 4.5, label = "theme_minimal()")) +
    geom_rect(aes(xmin = 0, xmax = 1, ymin = 4.25, ymax = 4.75), fill = "#00a3a6", colour = NA, alpha = 0.3) +
    xlim(c(0, 1)) +
    ylim(c(0, 10.5)) +
    theme_void())

(h12 <- ggplot(data = pingouins,
               aes(x = aile_lng_mm)) +
    geom_histogram(
      aes(fill = espece),
      position = "identity",
      alpha = 0.5) +
    scale_fill_manual(values = c("darkorange", "purple", "cyan4")) +
    labs(title = "Longueur des ailes de pingouins",
         subtitle = "pour 3 espèces de l'archipel Palmer",
         x = "Longueur des ailes (mm)",
         y = "Frequence") +
    theme_minimal()
)

(p12 <- t12 + h12 +
    patchwork::plot_layout(widths = c(1.3, 1.7)))

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
}, interval = 2, movie.name = "ggplot_02.gif", ani.width = 1800, ani.height = 1050, ani.res = 300)

# Scatterplot - a inclure dans le texte ----

(sp1 <- ggplot(data = pingouins,
       aes(x = bec_lng_mm, y = bec_htr_mm)) +
  geom_point())

ggsave("../codons.netlify/_posts/visualiser-donnees-ggplot2/img/sp1.png", sp1, dpi = 320, width = 12, height = 6)

ggplot(data = pingouins,
       aes(x = bec_lng_mm, y = bec_htr_mm)) +
  geom_point(aes(colour = espece)) +
  scale_colour_manual(values = c("darkorange", "purple", "cyan4"))
