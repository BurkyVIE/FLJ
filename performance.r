# LIBRARIES ----
library(tidyverse)

## DATA ----
data <- standings |> 
  left_join(teams, by = "Team") |> 
  mutate(Spiele = W - L,
         Punkte = PF - PG,
         Logo = paste0("logos/", Kurz, ".png"))
  
# plot ----
ggplot(data) +
  aes(x = Punkte, y = Spiele) +
  geom_hline(yintercept = 0, color = "orangered", linewidth = 1) +
  geom_vline(xintercept = 0, color = "orangered", linewidth = 1) +
  # ENTWEDER
  # ggrepel::geom_label_repel(mapping = aes(label = Kurz), segment.linetype = 2,
  #                           box.padding = 1.25, label.padding = unit(.2, "lines"), point.padding = .25,
  #                           max.overlaps = 8,
  #                           size = 4) +
  # ggimage::geom_image(data, inherit.aes = TRUE, image = "fb_32.png", size = .03) +
  # ODER
  ggrepel::geom_label_repel(mapping = aes(label = paste(Kurz, WLT, sep = "\n")), segment.linetype = 2,
                            box.padding = 2.25, label.padding = unit(.2, "lines"), point.padding = .25,
                            max.overlaps = 8,
                            color = "grey50", size = 3) +
  ggimage::geom_image(data, inherit.aes = TRUE, mapping = aes(image = Logo), size = .07) +
  # ENDE
  scale_x_continuous(name = "Punktedifferenz", breaks = function(x) seq(from = floor(x[1]/30)*30, to = ceiling(x[2]/30)*30, by = 30)) +
  scale_y_continuous(name = "Differenz (Siege - Niederlagen)", breaks = function(x) seq(from = floor(x[1]/2)*2, to = ceiling(x[2]/2)*2, by = 2),
                     minor_breaks = function(x) seq(from = floor(x[1]), to = ceiling(x[2]), by = 1)) +
  labs(title = "Vergleich der Teams", 
       subtitle = "2025 FLJ U13 Division I") +
  theme(plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 13),
        panel.background = element_rect(fill = "lightgreen"),
        panel.grid = element_line(color = "white", linewidth = 1.5)) -> p

windows(16,16)
plot(p)

rm(data, p)