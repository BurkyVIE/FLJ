# LIBRARIES ----
library(tidyverse)

# dat ----
dat <- results |> 
  filter(Saison == 2025, Stufe == "U13", Div == "D1") |> 
  left_join(teams, by = c("Saison", "Stufe", "Div", "Team")) |>
  mutate(Diff = PF - PG) |>
  group_by(Team, PF, PG) |>
  summarise(n = n(), .groups = "drop") |> 
  mutate(n = case_when(n == 1 ~ NA,
                       TRUE ~ n))

# PLOT ----
ggplot(dat) +
  aes(x = PF, y = PG) +
  annotate(geom = "polygon", x = c(-Inf, Inf, Inf), y = c(-Inf, Inf, -Inf), fill = "forestgreen", alpha = .1) +
  annotate(geom = "polygon", x = c(-Inf, Inf, -Inf), y = c(-Inf, Inf, Inf), fill = "firebrick", alpha = .1) +
  geom_abline(slope = 1, intercept = 0, color = "white", lwd = 2) +
  geom_abline(slope = 1, intercept = c(-35, 35), color = "white", lwd = 1, linetype = "dashed") +
  # geom_point() +
  ggimage::geom_image(inherit.aes = TRUE, image = "fb_32.png", size = .07) +
  ggrepel::geom_text_repel(aes(label = n), na.rm = TRUE, box.padding = 1.2, point.padding = 0, size = 3, color = "grey50") +
  scale_x_continuous(name = "Punkte Team", breaks = 14 * (0:5)) +
  scale_y_continuous(name = "Punkte Gegner", breaks = 14 * (0:5)) +
  facet_wrap(~Team) +
  labs(title = "Spielergebnisse pro Team", 
       subtitle = "2025 FLJ U13 Division I") +
  theme_bw() +
  theme(plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 13),
        strip.background = element_rect(fill = "forestgreen"),
        strip.text = element_text(color = "white", face = "bold")) -> p

windows(16, 16)
plot(p)

rm(p)