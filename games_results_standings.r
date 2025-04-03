# LIBRARIES ----
library(tidyverse)

# DATA ----
## Import ----
data_raw <- read_csv("FLJ 2025 U13 D1.txt", lazy = FALSE,
                     col_types = cols(Datum = col_date(),
                                      Kickoff = col_time(),
                                      Ort = col_character(),
                                      Heim = col_character(),
                                      Gast = col_character(),
                                      P_H = col_integer(),
                                      P_G = col_integer())
)

## Teams ----
team_kurz <- c("Angels", "Dragons", "Eagles", "Giants", "Indians", "Raiders", "Spartans", "Steelsharks")
teams <- unique(c(data_raw$Heim, data_raw$Gast))
he <- map_int(team_kurz, ~which(str_detect(teams, .)))
teams <- tibble(Team = teams[he], Kurz = team_kurz)
rm(team_kurz, he)

# OUTPUT ----
## Games ----
games <- filter(data_raw, !is.na(P_H) | !is.na(P_G))

## Results ----
results <- bind_rows(rename(games, Team = Heim, Gegner = Gast, PF = P_H, PG = P_G),
                     rename(games, Team = Gast, Gegner = Heim, PF = P_G, PG = P_H)) |> 
  mutate(Ergebnis = factor(case_when(PF > PG ~ "W",
                                     PF < PG ~ "L",
                                     TRUE ~ "T"),
                           levels = c("W", "L", "T")))

## Standings ----
standings <- select(results, c(Team, PF, PG, Ergebnis)) |>
  mutate(one = 1L) |> 
  pivot_wider(names_from = Ergebnis, values_from = one, names_expand = TRUE, values_fill = list(one = 0)) |> 
  group_by(Team) |>
  summarise(across(c(PF, PG, W, L, T), ~sum(.))) |>
  ungroup() |> 
  mutate(Gs = W + L + T,
         WLT = case_when(T == 0 ~ paste0("(", W, "-", L, ")"),
                         TRUE ~ paste0("(", W, "-", L, "-", T, ")")),
         Pct = num((W + 1/2 * T) / Gs, digits = 3)) |> 
  relocate(Gs, .before = W) |> 
  arrange(desc(Pct), desc(PF), PG)
