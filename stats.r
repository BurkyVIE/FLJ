kader <- read_csv("FLJ 2025 U13 Dark Angels Kader.txt", lazy = FALSE, col_types = cols(No = col_integer(), Name = col_character(), Jahrgang = col_integer()))

source("FLJ 2025 U13 Dark Angels Statistik.txt")
stats <- unnest(stats, Data) |>
  add_column(Active = TRUE) |>
  unnest_wider(Detail, )
stats <- stats |> left_join(kader, by = "No", relationship = "many-to-one") |> left_join(games, by = "SpielID")

stats |>
  filter(Gruppe == "Def") |> 
  group_by(No, Name) |>
  summarise(Games = n_distinct(SpielID),
            Pulls = sum(Pull, na.rm = TRUE),
            Ints = sum(Int, na.rm = TRUE),
            Pick6s = sum(Pick6, na.rm = TRUE),
            Sacks = sum(Sack, na.rm = TRUE),
            Safeties = sum(Safety, na.rm = TRUE),
            .groups = "drop") |> 
  # mutate(PullspG = Pulls/Games) |> 
  # arrange(desc(PullspG), No)
  gt::gt() |>
  gt::tab_header(title = "2025 FLJ U13", subtitle = "Dark Angels Defense")
