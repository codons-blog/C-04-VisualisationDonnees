# Load packages ----

library(readxl)
library(tidyverse)

# Hordeum vulgare TEs ----

# http://apte.cp.utfpr.edu.br/

d1 <- read_delim("data/hordeum_vulgare_TEAnnotationFinal.gff3")

names(d1) <- c("Chr", "Source", "Class_Order_Superfamily", "Start", "End",
               "Score", "Strand", "Phase", "Attributes")

clean_data <- d1 %>% 
  select(Chr, Class_Order_Superfamily, Start, End) %>% 
  filter(Chr %in% 1:7) %>% 
  mutate(Chr = paste0(Chr, "H"))

subset <- clean_data[1:1000, ]

test <- subset %>% 
  separate(col = Class_Order_Superfamily,
           into = c("Class", "Order", "Superfamily"),
           sep = "/")



# Triticum durum transposable elements ----

d1 <- read.table("data/Tdur_PropLenTE_RepFraction_Genome_forR.dat") %>% 
  as_tibble() %>% 
  rename(chromosome = V1,
         te_superfam = V2,
         te_prop = V3,
         chrom_prop = V4) %>% 
  mutate(te_class = case_when(grepl("^(R|S)", te_superfam) ~ "class I",
                              grepl("^D", te_superfam) ~ "class II",
                              grepl("^X", te_superfam) ~ "Unclassified"))

te_classes <- d1 %>% 
  group_by(chromosome, te_class) %>% 
  summarise(total = sum(te_prop)) %>% 
  mutate(te_class = factor(te_class,
                           levels = c("class I", "class II", "Unclassified", "NA")))

ggplot(data = te_classes) +
  geom_bar(mapping = aes(x = chromosome, y = total, fill = te_class),
           stat = "identity", colour = "white")

names(d1) <- c("chromosome", "te_superfam", "te_prop", "chrom_prop")


%>% 




head(d1)

ggplot(data = d1) +
  geom_bar(mapping = aes(x = chromosome, y = chrom_prop, fill = te_superfam),
           stat = "identity", colour = "white")

# Sequencing costs ----

# https://www.genome.gov/about-genomics/fact-sheets/DNA-Sequencing-Costs-Data

d1 <- readxl::read_xls("data/Sequencing_Cost_Data_Table_Aug2021.xls") %>% 
  mutate(t_date = lubridate::year(Date))

ggplot(data = d1) +
  geom_line(mapping = aes(x = Date, y = log10(`Cost per Mb`)),
            size = 2, colour = "orange") +
  theme_light()
  
# Eukaryotes ----

euk <- read_delim("data/eukaryotes.txt")

fungi <- euk %>% 
  filter(Status == "Complete Genome",
         Genes != "-",
         Group == "Fungi")

ggplot(data = fungi,
       mapping = aes(x = `Size (Mb)`, y = as.numeric(Genes))) +
  geom_point(aes(colour = SubGroup))

# Import dataset ----

d1 <- read_delim("data/eukaryotes.txt")


test <- d1 %>% 
  filter(Genes != "-", Status == "Complete Genome") %>% 
  mutate(Genes = as.numeric(Genes))

test %>% 
  ggplot(aes(x = `Size (Mb)`, y = Genes)) +
  geom_point(aes(colour = Group))

ta <- d1 %>% filter(`#Organism/Name` == "Triticum aestivum")

