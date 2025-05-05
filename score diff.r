library(tidyverse)

results |>
  filter(Div == "D1") |>
  mutate(Diff = PF - PG) |>
  ggplot() +
  geom_vline(xintercept = 35, color = "orangered", linewidth = 1) +
  aes(x = Diff, y = reorder(Team, Diff, median)) +
  geom_boxplot(color = "grey50", linewidth = 1, alpha = .67) +
  geom_point(size = 2) +
  ggimage::geom_image(inherit.aes = TRUE, image = "fb_32.png", size = .025) +
  scale_x_continuous(name = "Punktedifferenz pro Spiel") +
  scale_y_discrete(name = NULL) +
  labs(title = "Punktedifferenzen alle Spiele", 
       subtitle = "2025 FLJ U13 Division I") +
  theme(plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 13),
        panel.background = element_rect(fill = "lightgreen"),
        panel.grid = element_line(color = "white", linewidth = 1.5)) -> p

windows(16, 16)
plot(p)

rm(p)