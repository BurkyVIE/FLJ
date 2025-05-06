# LIBRARIES ----
library(tidyverse)

# dat ----
dat <- results |> 
  filter(Saison == 2025, Stufe == "U13", Div == "D1") |> 
  left_join(teams, by = c("Saison", "Stufe", "Div", "Team")) |>
  mutate(Diff = PF - PG) |>
  group_by(Kurz, Diff) |>
  summarise(n = n(), .groups = "drop") |> 
  mutate(n = case_when(n == 1 ~ NA,
                       TRUE ~ n))

## aus all scores könnte man Marker für doppelte Ergebnisse übernehmen

# PLOT ----
ggplot(dat) +
  geom_vline(xintercept = 0, color = "orangered", linewidth = 1, lty = "dashed") +
  geom_vline(xintercept = c(-35, 35), color = "orangered", linewidth = 1, lty = "solid") +
  aes(x = Diff, y = reorder(Kurz, Diff, median)) +
  # geom_boxplot(color = "grey50", linewidth = 1, alpha = .67) +
  ggridges::geom_density_ridges(rel_min_height = .01, scale = .8, quantile_lines = TRUE, quantiles = 2,
                                color = "#945d4f", fill = "beige", lwd = 1, alpha = .67) +
  # geom_point(size = 2) +
  ggimage::geom_image(inherit.aes = TRUE, image = "fb_32.png", size = .025) +
  ggrepel::geom_text_repel(aes(label = n), na.rm = TRUE, box.padding = 1, point.padding = 0, nudge_x = 3, nudge_y = .25, size = 3, color = "grey50") +
  scale_x_continuous(name = "Punktedifferenz pro Spiel", expand = c(0, -21), breaks = 35 * (-2:2)) +
  scale_y_discrete(name = NULL) +
  labs(title = "Verteilung der Punktedifferenzen pro Team", 
       subtitle = "2025 FLJ U13 Division I") +
  theme(plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 13),
        panel.background = element_rect(fill = "lightgreen"),
        panel.grid = element_line(color = "white", linewidth = 1.5)) -> p

windows(16, 16)
plot(p)

rm(p)
