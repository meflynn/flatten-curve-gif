

library(tidyverse)
library(gganimate)
library(transformr)
library(conflicted)
library(magick)
library(gifski)
library(here)

conflict_prefer("View", "gganimate", "utils")

x <- seq(0, 100, length.out = 1000)

y1 <- tibble(y = dnorm(x, mean = 15, sd = 3))
y2 <- tibble(y = dnorm(x, mean = 20, sd = 6))
y3 <- tibble(y = dnorm(x, mean = 25, sd = 9))
y4 <- tibble(y = dnorm(x, mean = 30, sd = 12))
y5 <- tibble(y = dnorm(x, mean = 35, sd = 15))

y <- rbind(y1, y2, y3, y4, y5)

z <- rep(1:5, each = 1000)

df <- data.frame(x, 
                 rbind(y1, y2, y3, y4, y5), 
                 z) %>% 
  mutate(z = factor(z))

ymax <- max(df$y[df$z==5])

# Use area not density dummy!!!!
plot <- ggplot(data = df) +
  geom_area(aes(x = x, y = y), fill = "deepskyblue", alpha = 0.5, color = "black", size = 0.1) +
  geom_hline(yintercept = ymax, linetype = "dashed") +
  annotate("text", x = 70, y = ymax + 0.005, label = "Hospital capacity") +
  scale_x_continuous(limits = c(0, 100)) +
  theme_bw() +
  theme(axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        plot.title = element_text(face = "bold", size = 16)) +
  labs(title = "Flatten the Curve",
       subtitle = "Delaying the spread of the disease matters a lot",
       x = "Days",
       y = "Number of cases") +
  transition_states(states = z,
                    transition_length = 1,
                    state_length = 1)

plot

animate(plot, 
        nframes = 35, 
        renderer = gifski_renderer("flatten-curve.gif"))


