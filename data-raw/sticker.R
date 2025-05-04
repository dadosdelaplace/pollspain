library(tidyverse)
library(hexSticker)

# Bar plot
tb1 <- tibble("party" = c("PSOE", "PP", "SUMAR", "VOX"),
             "porc" = c(..., ..., ..., ...),
             "color" = c(..., ..., ..., ...))

p1 <-
  ggplot(tb1, aes(x = party, y = porc, fill = color)) +
  geom_col() +
  theme_minimal() +
  theme(legend.position = "none")

# Survey plot
tb2 <-
  tibble("party" = c(rep("PSOE", 10), rep("PP", 10),
                     rep("SUMAR", 10), rep("VOX", 10)),
         "surveys" = c(rnorm(n = 10, mean = 28),
                       rnorm(n = 10, mean = 32),
                       rnorm(n = 10, mean = 15, sd = 1.5),
                       rnorm(n = 10, mean = 13, sd = 1.5)),
         "color" = c(rep("PSOE", 10), rep("PP", 10),
                     rep("SUMAR", 10), rep("VOX", 10)))

# Crear sticker
sticker(p,
        package = "pollspain",
        p_size = 20,
        s_x = 1, s_y = 0.8, s_width = 1.3,
        h_fill = "#3a9078", h_color = "#9cd6c5",
        p_color = "white",
        filename = "pollspain_sticker.png")
