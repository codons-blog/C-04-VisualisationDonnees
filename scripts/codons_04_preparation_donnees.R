# Ateliers codons!
# 04 - Visualisation de donnees
# Jeudi 30/06/2022
# Preparation des donnees

# Charger le Tidyverse ----

library(tidyverse)

# Production de cereales en France ----

# Source : https://ourworldindata.org/agricultural-production

# Ble

ble <- read_csv("data-raw/wheat-production.csv") %>%
  rename(pays = Entity,
         code = Code,
         annee = Year,
         tonnes = `Crops - Wheat - 15 - Production - 5510 - tonnes`) %>% 
  filter(pays == "France") %>% 
  mutate(cereale = "Blé") %>% 
  select(pays, code, annee, cereale, tonnes)

# Mais

mais <- read_csv("data-raw/maize-production.csv") %>%
  rename(pays = Entity,
         code = Code,
         annee = Year,
         tonnes = `Crops - Maize - 56 - Production - 5510 - tonnes`) %>% 
  filter(pays == "France") %>% 
  mutate(cereale = "Maïs") %>% 
  select(pays, code, annee, cereale, tonnes)

# Riz

riz <- read_csv("data-raw/rice-production.csv") %>%
  rename(pays = Entity,
         code = Code,
         annee = Year,
         tonnes = `Crops - Rice, paddy - 27 - Production - 5510 - tonnes`) %>% 
  filter(pays == "France") %>% 
  mutate(cereale = "Riz") %>% 
  select(pays, code, annee, cereale, tonnes)

# Orge

orge <- read_csv("data-raw/barley-production.csv") %>%
  rename(pays = Entity,
         code = Code,
         annee = Year,
         tonnes = `Crops - Barley - 44 - Production - 5510 - tonnes`) %>% 
  filter(pays == "France") %>% 
  mutate(cereale = "Orge") %>% 
  select(pays, code, annee, cereale, tonnes)

# Seigle

seigle <- read_csv("data-raw/rye-production.csv") %>%
  rename(pays = Entity,
         code = Code,
         annee = Year,
         tonnes = `Crops - Rye - 71 - Production - 5510 - tonnes`) %>% 
  filter(pays == "France") %>% 
  mutate(cereale = "Seigle") %>% 
  select(pays, code, annee, cereale, tonnes)

# Regrouper l'ensemble des cereales dans un seul tableau

cereales <- rbind(ble, mais, riz, orge, seigle)

write_csv(cereales, "data-clean/cereales.csv")


# TE de l'orge ----

# Source : http://apte.cp.utfpr.edu.br/

hv_te_raw <- read_delim("data-raw/hordeum_vulgare_TEAnnotationFinal.gff3",
                        col_names = FALSE)

# Nettoyer les donnees

hv_te_clean <- hv_te_raw %>% 
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

write_csv(hv_te_clean, "data-clean/hv_te_clean.txt")


# Calculer la taille cumulee des TEs par chromosome

te_cumul <- hv_te_clean %>% 
  group_by(chromosome, class, order) %>% 
  summarise(total_size = sum(size)) %>% 
  ungroup() %>% 
  group_by(chromosome) %>% 
  mutate(cumul_te_size = sum(total_size)) %>% 
  ungroup() %>% 
  mutate(percent = 100 * total_size / cumul_te_size) %>% 
  select(chromosome:order, percent)

write_csv(te_cumul, "data-clean/hv_te_ratios.csv")
