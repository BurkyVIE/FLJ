kader <- read_csv("FLJ 2025 U13 Dark Angels Kader.txt", lazy = FALSE, col_types = cols(No = col_integer(), Name = col_character(), Jahrgang = col_integer()))

source("FLJ 2025 U13 Dark Angels Statistik.txt")
stats <- stats |> unnest(Data) |> left_join(kader, by = "No", relationship = "many-to-one") |> left_join(games, by = "SpielID")

filter(stats, Gruppe == "Def", Art == "Pulls") |>
  group_by(No, Name) |>
  summarise(Pulls = sum(Wert), Games = n()) |> 
  mutate(PullspG = Pulls/Games) |> 
  arrange(desc(PullspG), No)

stats |>
  select(SpielID:Name) |>
  mutate(Spielzug = paste(Gruppe, Art, sep = "_")) |>
  pivot_wider(names_from = Spielzug, values_from = Wert, values_fn = sum) |> 
  filter(Gruppe == "Def") |> 
  group_by(No, Name) |>
  summarise(Pulls = sum(Def_Pulls), Games = n_distinct(SpielID)) |> 
  mutate(PullspG = Pulls/Games) |> 
  arrange(desc(PullspG), No)
