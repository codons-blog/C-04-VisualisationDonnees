# Ateliers codons!
# 04 - Visualisation de donnees
# Jeudi 30/06/2022

# Charger le Tidyverse -----

# library(animation)
# library(patchwork)
# library(showtext)
library(tidyverse)

# Charger les polices ----

# font_add_google(name = "Source Code Pro", family = "source")
# showtext_auto()

# Definir le repertoire de travail ----

setwd("D:/codons/C-04-VisualisationDonnees")

# Taille de génomes eucaryotes ----

# https://www.ncbi.nlm.nih.gov/genome/browse/#!/overview/
# https://ftp.ncbi.nlm.nih.gov/genomes/GENOME_REPORTS/

d1 <- read_delim("data-raw/eukaryotes.txt") %>% 
  

d1 <- read_csv("data-raw/genomes.csv") %>% 
  select(organism = `#Organism Name`,
         taxonomy = `Organism Groups`,
         size_mb = `Size(Mb)`) %>% 
  separate(col = taxonomy,
           into = c("n1", "n2", "n3"),
           sep = ";")

d1 <- read_delim("data-raw/genomes.csv")

plants <- read_delim("data-raw/eukaryotes.txt") %>% 
  filter(Group == "Plants") %>% 
  arrange(`#Organism/Name`)

ggplot(plants) +
  geom_point(aes(x = `Size (Mb)`, y = as.numeric(Genes)))

eukarya %>% 
  ggplot(aes(x = `Size (Mb)`, y = as.numeric(Genes))) +
  geom_point(aes(colour = Group))

plants <- read_delim("data-raw/eukaryotes.txt") %>% 
  select(organism = `#Organism/Name`,
         group = Group,
         subgroup = SubGroup,
         size_mb = `Size (Mb)`,
         gc_pct = `GC%`,
         genes = Genes) %>% 
  filter(group == "Plants",
         genes != "-") %>% 
  mutate(gc_pct = as.numeric(gc_pct),
         genes = as.numeric(genes))

ggplot(d1, aes(x = size_mb, y = as.numeric(Genes))) +
  geom_point(aes(colour = Group))



%>% 
  filter()

d1 <- read_csv("data-raw/genomes.csv") %>% 
  select(organism = `#Organism Name`,
         taxonomy = `Organism Groups`,
         size_mb = `Size(Mb)`) %>% 
  separate(col = taxonomy,
           into = c("n1", "n2", "n3"),
           sep = ";")

plants <- d1 %>% 
  filter(n2 == "Plants")

euk <- read_delim("data-raw/eukaryotes.txt") %>% 
  filter(Group == "Plants")

%>% 
  select(organism, group = n1, size_mb)

head(d1)

ggplot(d1,
       aes(x = size_mb, y = group)) +
  geom_point()


 # Importer les donnees ----

cereales <- read_csv("data-clean/cereales.csv")
hv_te <- read_csv("data-clean/hv_te_clean.txt")

# 1er type de graphique : scatter plot ----

orge <- cereales %>% 
  filter(cereale == "Orge")

ggplot(data = orge) +
  geom_line(mapping = aes(x = annee,
                          y = tonnes / 1e6),
            size = 1,
            colour = "#86d4ee") +
  # geom_point(mapping = aes(x = annee,
  #                          y = tonnes / 1e6),
  #            shape = 21, size = 5,
  #            colour = "white",
  #            fill = "#120c6e",
  #            stroke = 3) +
  scale_x_continuous(breaks = seq(1960, 2020, 5)) +
  labs(title = "Production d'orge en France (1961-2018)",
       subtitle = "en millions de tonnes",
       x = "",
       y = "") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "#1c3f55", colour = NA),
        plot.background = element_rect(fill = "#1c3f55", colour = NA),
        plot.title = element_text(hjust = 0.5, size = 25, colour = "#dff8f9",
                                  margin = margin(t = 10)),
        plot.subtitle = element_text(hjust = 0.5, size = 20, colour = "#dff8f9",
                                     margin = margin(b = 10)),
        plot.margin = margin(t = 10, r = 20, b = 10, l = 20),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = "#53839c", size = 0.05,
                                        linetype = "dotted"),
        axis.title = element_text(colour = "#dff8f9"),
        axis.text = element_text(colour = "#dff8f9"))

# 2e type de graphique : bar plot ----

hv_te_chr <- hv_te %>% 
  group_by(chromosome) %>% 
  summarise(total_size = sum(size)) %>% 
  arrange(total_size) %>% 
  mutate(chromosome = fct_inorder(chromosome))

ggplot(data = hv_te_chr) +
  geom_bar(mapping = aes(x = chromosome,
                         y = total_size / 1e9),
           stat = "identity",
           width = 0.5,
           fill = "#86d4ee") +
  geom_text(mapping = aes(x = chromosome,
                          y = ((total_size + 50e6)/ 1e9),
                          label = paste0(round(total_size / 1e9, digits = 3),  " Gb")),
            colour = "#dff8f9") +
  coord_flip() +
  labs(title = "Taille cumulée des éléments transposables chez l'orge",
       x = "", y = "") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "#1c3f55", colour = NA),
        plot.background = element_rect(fill = "#1c3f55", colour = NA),
        plot.title = element_text(hjust = 0, size = 25, colour = "#dff8f9",
                                  margin = margin(t = 10)),
        plot.subtitle = element_text(hjust = 0, size = 20, colour = "#dff8f9",
                                     margin = margin(b = 10)),
        plot.margin = margin(t = 10, r = 20, b = 10, l = 20),
        panel.grid = element_blank(),
        axis.title = element_text(colour = "#dff8f9"),
        axis.text.x = element_blank(),
        axis.text.y = element_text(colour = "#dff8f9"))

ggplot(data = hv_te,
       mapping = aes(x = chromosome, y = percent,
                     fill = order)) +
  geom_bar(stat = "identity") +
  theme_minimal()

ggplot(data = cereales,
       mapping = aes(x = annee, y = tonnes,
                     colour = cereale)) +
  # geom_point() +
  geom_line() +
  scale_x_continuous(breaks = seq(1960, 2020, 5)) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank())

# Nettoyer les donnees ----

hordeum_tes_clean <- hordeum_tes_raw %>% 
  select(chromosome = X1,
         te_classification = X3,
         start = X4,
         end = X5) %>% 
  filter(chromosome %in% 1:7) %>% 
  mutate(chromosome = paste0(chromosome, "H")) %>% 
  separate(col = te_classification,
           into = c("class", "order", "superfamily"),
           sep = "/") %>% 
  filter(!order %in% c("Maverick", "PLE", "DIRS")) %>% 
  mutate(class = case_when(grepl("^Class II", class) ~ "Class II",
                           TRUE ~ as.character(class))) %>% 
  mutate(class = na_if(class, "Unknown"),
         order = na_if(order, "-"),
         superfamily = na_if(superfamily, "-"),
         size = end - start)

write_csv(hordeum_tes_clean, "data/hordeum_vulgare_clean.txt")

# Calculer la taille cumulee des TEs par chromosome ----

d1 <- hv_tes %>% 
  group_by(chromosome, class, order) %>% 
  summarise(total_size = sum(size)) %>% 
  ungroup() %>% 
  group_by(chromosome) %>% 
  mutate(cumul_te_size = sum(total_size)) %>% 
  ungroup() %>% 
  mutate(percent = 100 * total_size / cumul_te_size) %>% 
  select(chromosome:order, percent)

write_csv(d1, "data/hv_te_ratios.csv")

# Rendement ----

# Source : https://ourworldindata.org/crop-yields

rdt <- read_csv("https://raw.githubusercontent.com/owid/owid-datasets/master/datasets/Attainable%20yields%20(Mueller%20et%20al.%202012)/Attainable%20yields%20(Mueller%20et%20al.%202012).csv")



# Importer les donnees nettoyees ----

hv_te <- read_csv("data/hv_te_clean.csv")

  
head(d1)

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
  geom_point(aes(shape = espece)) +
  scale_shape_manual(values = c(0, 6, 8))

ggplot(data = pingouins,
       aes(x = bec_lng_mm, y = bec_htr_mm)) +
  geom_point(aes(colour = espece)) +
  scale_colour_manual(values = c("darkorange", "purple", "cyan4"))


ggplot(data = pingouins,
       aes(x = bec_lng_mm, y = bec_htr_mm)) +
  geom_point(shape = 21, colour = "firebrick", fill = "red", size = 5, stroke = 3, alpha = 0.5) 


ggplot(data = pingouins,
       aes(x = bec_lng_mm, y = bec_htr_mm)) +
  geom_point(aes(size = masse_g))
