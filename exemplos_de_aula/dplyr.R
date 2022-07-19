library(tidyverse)


# across -----------------------------------------------------------------

install.packages("dados")
casas <- dados::casas

# as vezes queremos fazer:
casas |>
  group_by(geral_qualidade) |>
  summarise(
    primeiro_andar_area = median(primeiro_andar_area),
    segundo_andar_area = median(segundo_andar_area)
  )

# mas para todas as colunas...

casas |>
  group_by(geral_qualidade) |>
  summarise(
    across(
      .cols = ends_with("area"),
      .fns = mean, na.rm = TRUE
    )
  )

troca_na_pela_media <- function(coluna){
  if_else(is.na(coluna), mean(coluna, na.rm = TRUE), as.numeric(coluna))
}

casas |>
  mutate(
    across(
      .cols = c(lote_fachada, ends_with("area")),
      .fns = troca_na_pela_media
    )
  ) |> View()

casa |>
  filter(
    across(
      .cols = c(lote_fachada),
      .fns = ~!is.na(.x)
      # isso é uma abreviação para function(x){!is.na(x)}
    )
  )

casas |>
  group_by(geral_qualidade) |>
  summarise(
    across(
      .cols = ends_with("area"),
      .fns = list("media" = mean, "median" = median)
    )
  )


