kader <- read_csv("FLJ 2025 U13 Dark Angels Kader.txt", lazy = FALSE, col_types = cols(No = col_integer(), Name = col_character(), Jahrgang = col_integer()))

source("FLJ 2025 U13 Dark Angels Statistik.txt")
statistik <- statistik |> unnest(Data) |> left_join(kader, by = "No", relationship = "many-to-one") |> left_join(games, by = "SpielID")
