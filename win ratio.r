# LIBRARIES ----
library(tidyverse)

# DATA ----
data <- standings |> 
  filter(Saison == 2025, Stufe == "U13", Div == "D1") |> 
  left_join(teams, by = c("Saison", "Stufe", "Div", "Team")) |> 
  mutate(EWR = 1 / (1 + (PG / PF) ** 2.37),
         Delta = (as.numeric(Pct) - EWR))

# PLOT ----
ggplot(data) +
  aes(x = Pct, y = EWR) +
  geom_abline(slope = 1, intercept = 0, linetype = "solid") +
  geom_point(mapping = aes(color = Delta * 10), shape = 16, size = 13) +
  ggimage::geom_image(inherit.aes = TRUE, image = "fb_32.png", size = .03) +
  ggrepel::geom_label_repel(mapping = aes(label = Kurz), segment.linetype = 2,
                            box.padding = 1.25, label.padding = unit(.2, "lines"), point.padding = .25,
                            max.overlaps = 8, size = 3) +
  scale_x_continuous(name = "Wahre Win Ratio") +
  scale_y_continuous(name = "Erwartete Win Ratio (Pythagoreisch)") +
  scale_color_distiller(name = "Overwinning\npro 10 Spiele", palette = "RdBu", direction = -1, values = scales::rescale(c(range(data$Delta), 0)[c(1, 3, 2)])) +
  labs(title = "Wahre und erwartete Win Ratio",
       subtitle = "2025 FLJ U13 Division I") +
  # coord_fixed(xlim = c(0, 1), ylim = c(0, 1)) +
  theme(plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 13),
        panel.background = element_rect(fill = "lightgreen"),
        panel.grid = element_line(color = "white", linewidth = 1.5)) -> p

windows(16, 16)
plot(p)

rm(data, p)

