# LIBRARIES ----
library(tidyverse)

# DATA ----
## Import ----
data_raw <- dir()[str_detect(dir(),pattern = "Spiele.txt")] |>
  enframe(name = NULL, value = "file") |>
  separate(file, into = c("Liga", "Saison", "Stufe", "Div"), remove = FALSE, extra = "drop") |>
  rowwise() |> 
  mutate(Data = map(.x = file,
                    .f = ~read_csv(file, lazy = FALSE,
                        col_types = cols(Datum = col_date(),
                                         Kickoff = col_time(),
                                         Ort = col_character(),
                                         Heim = col_character(),
                                         Gast = col_character(),
                                         P_H = col_integer(),
                                         P_G = col_integer()))))

data <- data_raw |> select(-(file:Liga)) |> unnest_longer(Data) |> unpack(Data)

# data_raw <- read_csv("FLJ 2025 U13 D1.txt", lazy = FALSE,
#                      col_types = cols(Datum = col_date(),
#                                       Kickoff = col_time(),
#                                       Ort = col_character(),
#                                       Heim = col_character(),
#                                       Gast = col_character(),
#                                       P_H = col_integer(),
#                                       P_G = col_integer())
# )

## Teams ----
team <- tribble(~Suchwort, ~Kurz,
                "Angels", "Angels",
                "Dragons", "Dragons",
                "Pann", "P.Eagles",
                "Giants", "Giants",
                "Indians", "Indians",
                "Raiders", "Raiders",
                "Spartans", "Spartans",
                "Steelsharks", "Steelsharks",
                "Badgers", "Badgers",
                "SG Most", "Bastards",
                "Carin", "C.Eagles",
                "Invaders", "Invaders",
                "Knights", "Knights",
                "Legionaries", "Legionaries",
                "Vikings", "Vikings",
                "Vipers", "Vipers")

teams <- data |> select(Saison, Stufe, Div, Heim) |> unique() |> rename(Team = Heim)
he <- map_int(team$Suchwort, ~which(str_detect(teams$Team, .)))
teams <- tibble(teams[he,], Kurz = team$Kurz)
rm(team, he)

# OUTPUT ----
## Games ----
games <- filter(data, !is.na(P_H) | !is.na(P_G))

## Results ----
results <- bind_rows(rename(games, Team = Heim, Gegner = Gast, PF = P_H, PG = P_G) |> 
                       bind_cols(Heim = TRUE),
                     rename(games, Team = Gast, Gegner = Heim, PF = P_G, PG = P_H) |> 
                       bind_cols(Heim = FALSE)) |> 
  mutate(Ergebnis = factor(case_when(PF > PG ~ "W",
                                     PF < PG ~ "L",
                                     TRUE ~ "T"),
                           levels = c("W", "L", "T"))) |>
  rowwise() |> 
  mutate(P35F = min(PF, PG + 35),
         P35G = min(PG, PF + 35)) |> 
  relocate(Heim, .after = Team) |> 
  relocate(Ergebnis, .after = last_col()) |> 
  arrange(Datum, Kickoff, SpielID, desc(Heim))

## Standings ----
standings <- results |> 
  mutate(one = 1L) |> 
  pivot_wider(names_from = Ergebnis, values_from = one, names_expand = TRUE, values_fill = list(one = 0L)) |> 
  group_by(Saison, Stufe, Div, Team) |>
  summarise(across(c(P35F, P35G, PF, PG, W, L, T), ~sum(.)), .groups = "drop") |>
  mutate(Gs = W + L + T,
         WLT = case_when(T == 0 ~ paste0("(", W, "-", L, ")"),
                         TRUE ~ paste0("(", W, "-", L, "-", T, ")")),
         Pct = num((W + 1/2 * T) / Gs, digits = 3)) |> 
  relocate(Gs, .before = W) |> 
  arrange(desc(Pct), desc(P35F-P35G))

## Vorteil ----
vorteil <- results |>
  group_by(Saison, Stufe, Div, Team, Gegner) |>
  summarise(Spiele = n(), P35F = sum(P35F), P35G = sum(P35G), .groups = "drop") |>
  filter(P35F > P35G) |>
  reframe(.by = c(Saison, Stufe, Div, Team), Vort_ggü = list(Gegner))
