# Load packages ----

library(tidyverse)

# Import dataset ----

d1 <- read_delim("data/eukaryotes.txt")


test <- d1 %>% 
  filter(Genes != "-", Status == "Complete Genome") %>% 
  mutate(Genes = as.numeric(Genes))

test %>% 
  ggplot(aes(x = `Size (Mb)`, y = Genes)) +
  geom_point(aes(colour = Group))

ta <- d1 %>% filter(`#Organism/Name` == "Triticum aestivum")

