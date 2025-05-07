kader <- read_csv("FLJ 2025 U13 Dark Angels Kader.txt", lazy = FALSE, col_types = cols(No = col_integer(), Name = col_character(), Jahrgang = col_integer()))

source("FLJ 2025 U13 Dark Angels Statistik.txt")
stats <- unnest(stats, Data) |>
  add_column(Active = TRUE) |>
  unnest_wider(Detail)
stats <- stats |> left_join(kader, by = "No", relationship = "many-to-one") |> left_join(games, by = "SpielID")

stats |>
  filter(Gruppe == "Def") |> 
  group_by(No, Name) |>
  summarise(Pulls = sum(Pull), Games = n_distinct(SpielID), .groups = "drop") |> 
  mutate(PullspG = Pulls/Games) |> 
  arrange(desc(PullspG), No)
