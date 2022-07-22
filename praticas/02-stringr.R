
# -------------------------------------------------------------------------

library(tidyverse)

# Base IMDB

imdb <- read_csv("https://raw.githubusercontent.com/curso-r/main-r4ds-1/master/dados/imdb.csv")

# -------------------------------------------------------------------------

# Motivação: descobrir a quantos gêneros cada filme pertence

imdb |>
  mutate(
    num_generos = str_count(generos, ",") + 1
  ) |>
  select(generos, num_generos) |>
  View()

# Motivação: extrair o subtítulos dos filmes

imdb |>
  mutate(
    subtitulo = str_extract(titulo, ": .*"),
    subtitulo = str_remove(subtitulo, ": ")
  ) |>
  select(titulo, subtitulo) |>
  filter(!is.na(subtitulo)) |>
  View()


# Motivação: criar uma tabela apenas com filmes cujo
# título comece com um número

imdb |>
  filter(
    str_detect(titulo, "^[0-9]")
  ) |>
  View()

# mostrando diferença entre parentesis e colchetes

# parentesis:

imdb |>
  filter(
    str_detect(titulo, "^(10 )|^(12 )")
  ) |>
  View()

# colchetes:

imdb |>
  filter(
    str_detect(titulo, "^[12]")
  ) |>
  View()

# mesma coisa que

imdb |>
  filter(
    str_detect(titulo, "^[21]")
  ) |>
  View()


# Motivação: criar uma tabela apenas com filmes em que
# Josh Hutcherson faz parte do elenco

imdb |>
  filter(
    str_detect(elenco, "Josh Hutcherson")
  ) |>
  select(elenco) |>
  View()


# Motivação: criar uma tabela apenas com filmes em que
# Josh Hutcherson ou Jennifer Lawrence fazem parte do elenco

imdb |>
  filter(
    str_detect(elenco, "Josh Hutcherson") |
      str_detect(elenco, "Jennifer Lawrence")
  ) |>
  select(elenco) |>
  View()


# Motivação: criar uma tabela apenas com filmes em que
# Josh Hutcherson e Jennifer Lawrence fazem parte do elenco

imdb |>
  filter(
    str_detect(elenco, "Josh Hutcherson"),
      str_detect(elenco, "Jennifer Lawrence")
  ) |>
  select(titulo, elenco) |>
  View()


# Motivação: criar uma tabela apenas com filmes em que
# apenas um entre Josh Hutcherson e Jennifer Lawrence
# faz parte do elenco

imdb |>
  mutate(
    josh = str_detect(elenco, "Josh Hutcherson"),
    jennifer = str_detect(elenco, "Jennifer Lawrence")
  ) |>
  filter((!josh & jennifer) |
           (josh & !jennifer) ) |>
  select(josh, jennifer, elenco) |>
  View()


# -------------------------------------------------------------------------

# Motivação: Baixando e limpando dados do Rick and Morty

# ### scrape ####---------------------------------------------------------

url <- "https://en.wikipedia.org/wiki/List_of_Rick_and_Morty_episodes"

res <- httr::GET(url)

wiki_page <- httr::content(res)

lista_tab <- wiki_page |>
  xml2::xml_find_all(".//table")  |>
  magrittr::extract(2:6) |>
  rvest::html_table(fill = TRUE) |>
  purrr::map(janitor::clean_names) |>
  purrr::map(~dplyr::rename_with(.x, ~stringr::str_remove(.x, "_37|_3")))

num_temporadas <- 1:length(lista_tab)

tab <- lista_tab |>
  purrr::map2(num_temporadas, ~dplyr::mutate(.x, no_season = .y)) |>
  dplyr::bind_rows()

# -------------------------------------------------------------------------

rick_and_morty <- tab |>
  dplyr::relocate(no_season, .before = no_inseason) |>
  dplyr::mutate(
    # Removendo aspas do título
  title = str_remove_all(title, '"'),

    # Removendo colchetes da audiência
  u_s_viewers_millions = str_remove_all(u_s_viewers_millions, "\\[.*\\]"),
  u_s_viewers_millions = as.numeric(u_s_viewers_millions),

    # Extraindo data formatada
  original_air_date = str_extract(original_air_date, "\\(.*\\)"),
  original_air_date = str_remove_all(original_air_date, "\\(|\\)"),
  original_air_date = lubridate::as_date(original_air_date)
  )

  # renomeando colunas
  dplyr::select(
    num_episodio = no_overall,
    num_temporada = no_season,
    num_dentro_temporada = no_inseason,
    titulo = title,
    direcao = directed_by,
    roteiro = written_by,
    data_transmissao_original = original_air_date,
    qtd_espectadores_EUA = u_s_viewers_millions
  ) |>
  tibble::as_tibble()

View(rick_and_morty)


# datas -------------------------------------------------------------------

# voltando de data para texto no formato que eu quiser

format(Sys.Date(), "%m/%Y")

# transformando string em data

lubridate::dmy("03-07-2022")
