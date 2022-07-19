library(tidyverse)

imdb <- read_csv("https://raw.githubusercontent.com/curso-r/main-r4ds-1/master/dados/imdb.csv")

# separate ----------------------------------------------------------------

imdb |>
  separate(generos, into = c("genero1", "genero2", "genero3")) |>
  View()

imdb |>
  separate(idioma, into = paste0("idioma", 1:10),
           remove = FALSE) |>
  View()

# como descobrir qual é o maior numero possível de quebras?

imdb |>
  summarise(
    max(str_count(idioma, ", "), na.rm = TRUE)
  )

# unite -------------------------------------------------------------------

imdb_com_generos <- imdb |>
  separate(generos, sep = ", ", into = c("genero1", "genero2", "genero3"))

imdb_com_generos |>
  unite(coluna_nova, genero1, genero2, genero3, sep = ", ", na.rm = TRUE) |>
  select(coluna_nova)

# pivot_longer ------------------------------------------------------------

imdb_atores <- imdb |>
  select(id_filme, titulo, elenco, pais) |>
  separate(elenco, sep = ", ", into = c("elenco1", "elenco2", "elenco3"), extra = "drop")

imdb_atores |>
  pivot_longer(cols = starts_with("elenco"),
               names_to = "posicao_titulacao", values_to = "nome_pessoa") |>
  count(nome_pessoa) |>
  arrange(desc(n))

imdb_atores_long <- imdb_atores |>
  pivot_longer(cols = elenco1:elenco3, names_to = "posicao_titulacao",
               values_to = "nome_pessoa")

# pivot_wider -------------------------------------------------------------

imdb_atores_long |>
  pivot_wider(
    id_cols = id_filme:pais,
    names_from = "posicao_titulacao",
    values_from = "nome_pessoa"
  ) |>
  filter(titulo == "A Better Place")

imdb_atores_long |>
  filter(titulo == "A Better Place")


# coisas que podem acontecer ----------------------------------------------

# no pivot longer não tem muito o que rolar...
imdb_atores |>
  pivot_longer(cols = elenco1:elenco3, names_to = "posicao_titulacao",
               values_to = "nome_pessoa")

# já no pivot_wider

imdb_atores_long |>
  pivot_wider(
    id_cols = titulo:pais,
    names_from = "posicao_titulacao",
    values_from = "nome_pessoa",
    values_fn = function(x){paste0(x, collapse = "|")}
  ) |>
  filter(titulo == "A Better Place")

imdb_atores_long |>
  pivot_wider(
    id_cols = id_filme,
    names_from = "posicao_titulacao",
    values_from = c("pais", "nome_pessoa")
  )

imdb |>
  separate(elenco, into = c("elenco1", "elenco2", "elenco3"), extra = "drop") |>
  pivot_longer(
    starts_with("elenco"),
    names_to = "posicao_titulacao",
    values_to = "nome_pessoa"
  )


# fill --------------------------------------------------------------------

imdb |>
  arrange(direcao, ano) |>
  mutate(idioma2 = idioma) |>
  group_by(direcao) |>
  fill(idioma2) |>
  select(titulo, direcao, ano, idioma, idioma2) |>
  View()
