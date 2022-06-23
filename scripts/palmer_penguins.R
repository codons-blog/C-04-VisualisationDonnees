# Palmer penguins plots 
# Last updated 2022-06-23

# Load packages ----

library(palmerpenguins)
library(tidyverse)

# Import dataset ----

d1 <- palmerpenguins::penguins %>% 
  select(espece = species,
         ile = island,
         bec_lng_mm = bill_length_mm,
         bec_htr_mm = bill_depth_mm,
         aile_lng_mm = flipper_length_mm,
         masse_g = body_mass_g) %>% 
  filter(!is.na(bec_lng_mm))

# Scatter plot ----

d1 %>% 
  ggplot(aes(x = bec_lng_mm,
             y = bec_htr_mm,
             colour = espece)) +
  geom_point(shape = 19, size = 3, alpha = 0.8,
             aes(size = masse_g)) +
  scale_colour_manual(values = c("darkorange", "purple", "cyan4")) +
  theme_minimal()

# Histogram ----

d1 %>% 
  ggplot(aes(x = aile_lng_mm)) +
  geom_histogram(aes(fill = espece),
                 alpha = 0.5,
                 position = "identity") +
  scale_fill_manual(values = c("darkorange", "purple", "cyan4")) +
  theme_minimal()

# Bar plot ----

d1 %>% 
  ggplot(aes(x = espece)) +
  geom_bar(aes(fill = espece),
           width = 0.75) +
  coord_flip() +
  scale_fill_manual(values = c("darkorange", "purple", "cyan4")) +
  theme_minimal()

d1 %>% 
  mutate(espece = fct_infreq(espece)) %>% 
  ggplot(aes(x = espece)) +
  geom_bar(aes(fill = espece),
           width = 0.75) +
  coord_flip() +
  scale_fill_manual(values = c("darkorange", "purple", "cyan4")) +
  theme_minimal()

# Boxplot ----

d1 %>% 
  ggplot(aes(x = espece, y = masse_g)) +
  geom_boxplot()
