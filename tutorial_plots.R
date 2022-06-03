library(palmerpenguins)

penguins <- palmerpenguins::penguins

pingouins <- penguins %>% 
  rename(espece = species,
         ile = island,
         bec_longueur_mm = bill_length_mm,
         bec_profondeur_mm = bill_depth_mm,
         aile_longueur_mm = flipper_length_mm,
         masse_g = body_mass_g,
         sexe = sex,
         annee = year)

write_csv(pingouins, "pingouins.csv")

ggplot(data = pingouins) +
  geom_point(aes(x = aile_longueur_mm, y = masse_g))

head(pingouins)

ggplot(data = pingouins) +
  geom_histogram(aes(x = masse_g), bins = 10,
                 fill = "orange") +
  theme_void()

ggplot(data = pingouins) +
  geom_density(aes(x = masse_g), fill = "orange", colour = "orange") +
  theme_void()

ggplot(data = pingouins) +
  geom_boxplot(aes(x = espece, y = masse_g,
                   fill = espece),
               show.legend = FALSE, outlier.shape = NA) +
  theme_void()

ggplot(data = pingouins) +
  geom_dotplot(aes(x = masse_g),
               show.legend = FALSE) +
  theme_void()               

ggplot(data = pingouins) +
  geom_point(aes(x = aile_longueur_mm, y = masse_g),
                 colour = "orange") +
  theme_void()
