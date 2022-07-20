
# -------------------------------------------------------------------------

library(tidyverse)
library(dados)

# install.packages("dados")

# Base Star Wars

dados_starwars |> View()

# Motivação: explorar algumas funções do tidyr para lidar com NAs

# drop_na()

dados_starwars |>
  drop_na() |>
  View()

dados_starwars |>
  drop_na(massa) |>
  View()

# Motivação: substituir todos os NAs das variaveis categoricas
# por "sem informação"

dados_starwars |>
  mutate(
    across(
      .cols = where(is.character),
      .fns = replace_na,
      replace = "sem informacao"
    )
  ) |>
  View()

# Motivação: ver quais colunas possuem mais NAs

dados_starwars |>
  summarise(
    across(
      .cols = everything(),
      .fns = ~sum(is.na(.x))
    )
  ) |>
  View()


# transformando no formato longo

dados_starwars |>
  summarise(
    across(
      .cols = everything(),
      .fns = ~sum(is.na(.x))
    )
  ) |>
  pivot_longer(
    cols = everything(),
    names_to = "variaveis",
    values_to = "numero_nas"
  ) |>
  View()


# Motivação: ver o numero de categorias distintas em cada
# variavel categorica


dados_starwars |>
  select(where(is.character)) |>
  summarise(
    across(
      .fns = n_distinct
    )
  )


dados_starwars |>
  select(where(is.character)) |>
  summarise(
    across(
      .fns = n_distinct
    )
  ) |>
  pivot_longer(
    cols = everything(),
    names_to = "variavel",
    values_to = "num_categorias"
  ) |>
  arrange(desc(num_categorias)) |>
  View()



# -------------------------------------------------------------------------

# Base IMDB
imdb <- read_csv("https://raw.githubusercontent.com/curso-r/main-r4ds-1/master/dados/imdb.csv")

# Motivação: Descobrir o ator com o maior lucro médio na base IMDB


# supondo que temos uma coluna com cada pessoa do elenco:

imdb |>
  separate(col = elenco, sep = ", ", into = c("pessoa_1",
                                              "pessoa_2",
                                              "pessoa_3")) |>
  pivot_longer(
    cols = starts_with("pessoa"),
    names_to = "ordem",
    values_to = "nome_pessoa"
  ) |>
  mutate(
    lucro = receita - orcamento
  ) |>
  drop_na(lucro) |>
  group_by(nome_pessoa) |>
  summarise(lucro_medio = mean(lucro)) |>
  # arrange(desc(lucro_medio))
  slice_max(lucro_medio, n = 1)

imdb |>
  separate(col = elenco, sep = ", ", into = c("pessoa_1",
                                              "pessoa_2",
                                              "pessoa_3")) |>
  pivot_longer(
    cols = starts_with("pessoa"),
    names_to = "ordem",
    values_to = "nome_pessoa"
  ) |>
  mutate(
    lucro = receita - orcamento
  ) |>
  drop_na(lucro) |>
  group_by(nome_pessoa) |>
  summarise(
    lucro_medio = mean(lucro),
    n_filmes = n()) |>
  filter(n_filmes > 10) |>
  arrange(desc(lucro_medio)) |> View()


# Motivação: Descobrir o ator com o maior lucro médio na base IMDB


# separate_rows

imdb |>
  separate_rows(elenco, sep = ", ") |>
  mutate(
    lucro = receita - orcamento
  ) |>
  drop_na(lucro) |>
  group_by(elenco) |>
  summarise(
    lucro_medio = mean(lucro),
    n_filmes = n()) |>
  filter(n_filmes > 10) |>
  arrange(desc(lucro_medio)) |> View()



# Motivação: fazer gráficos de dispersão do lucro vs todas as
# outras variáveis numéricas da base IMDB

imdb |>
  mutate(lucro = receita - orcamento) |>
  drop_na(lucro) |>
  select(where(is.numeric)) |>
  pivot_longer(
    cols = -lucro,
    names_to = "variavel",
    values_to = "valor"
  ) |>
  sample_n(size = 3000) |>
  ggplot(aes(x = valor, y = lucro)) +
  geom_point() +
  facet_wrap(~variavel,
             scales = "free")















# Motivação: fazer uma tabela do lucro médio anual dos filmes
# de comedia, ação e romance (a partir de 2000)

















# Motivação: calcular o lucro medio por genero do filme na base IMDB















