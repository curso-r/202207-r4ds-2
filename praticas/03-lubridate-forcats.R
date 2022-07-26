# Motivação: fazer uma análise descritiva do Ozônio

library(dplyr)
library(lubridate)
library(ggplot2)

cetesb <- readr::read_rds("data/cetesb.rds")

glimpse(cetesb)

# pegando só poluente ozonio da estacao pinheiros

cetesb |>
  filter(poluente == "O3", estacao_cetesb == "Pinheiros") |>
  group_by(data) |>
  summarise(media_concentracao = mean(concentracao, na.rm = TRUE)) |>
  ggplot(aes(x = data, y = media_concentracao)) +
  geom_line()

# criando novas colunas de ano, mes, dia do mes e dia da semana

ozonio <- cetesb |>
  filter(poluente == "O3", estacao_cetesb == "Pinheiros") |>
  mutate(
    ano = year(data),
    mes = month(data),
    dia_do_mes = day(data),
    dia_semana = wday(data)
  )

# medias por ano

ozonio |>
  group_by(ano) |>
  summarise(media = mean(concentracao, na.rm = TRUE))

# MEDIA POR MES

ozonio |>
  group_by(mes) |>
  summarise(media_concentracao = mean(concentracao, na.rm = TRUE)) |>
  ggplot(aes(x = mes, y = media_concentracao)) +
  geom_col()

# por dia da semana

ozonio |>
  mutate(
    dia_semana = wday(data, label = TRUE)
  ) |>
  group_by(dia_semana) |>
  summarise(media_concentracao = mean(concentracao, na.rm = TRUE)) |>
  ggplot(aes(x = dia_semana, y = media_concentracao)) +
  geom_col()

# levando em conta o ano para a media mensal

ozonio |>
  mutate(
    mes_ano = floor_date(data, "month")
  ) |>
  group_by(mes_ano) |>
  summarise(media_concentracao = mean(concentracao, na.rm = TRUE)) |>
  ggplot(aes(x = mes_ano, y = media_concentracao)) +
  geom_line() +
  scale_x_date(date_breaks = "6 months",
               date_labels = "%m-%y")


# concentracao pelo horario do dia

ozonio |>
  group_by(hora) |>
  summarise(media_concentracao = mean(concentracao, na.rm = TRUE)) |>
  ggplot(aes(x = hora, y = media_concentracao)) +
  geom_line()


# por dia da semana

ozonio |>
  mutate(
    dia_semana = wday(data, label = TRUE)
  ) |>
  group_by(hora, dia_semana) |>
  summarise(media_concentracao = mean(concentracao, na.rm = TRUE)) |>
  ggplot(aes(x = hora, y = media_concentracao)) +
  geom_line() +
  facet_wrap(vars(dia_semana))


# IMDB

library(dplyr)
library(tidyr)
library(stringr)
library(forcats)
library(ggplot2)

imdb <- readr::read_csv("https://raw.githubusercontent.com/curso-r/main-r4ds-1/master/dados/imdb.csv")

# Movitação: ver qual o gênero mais lucrativo na
# base IMDB

imdb_lucro <- imdb |>
  mutate(lucro = receita - orcamento)

# media de lucro por genero

imdb_lucro |>
  separate_rows(generos, sep = ", ") |>
  group_by(generos) |>
  summarise(media_lucro = mean(lucro, na.rm = TRUE)) |>
  arrange(desc(media_lucro))

# grafico de barras

imdb_lucro |>
  separate_rows(generos, sep = ", ") |>
  group_by(generos) |>
  summarise(media_lucro = mean(lucro, na.rm = TRUE)) |>
  ggplot(aes(y = generos, x = media_lucro)) +
  geom_col()

# ordenando colunas de acordo com o lucro medio

imdb_lucro |>
  separate_rows(generos, sep = ", ") |>
  group_by(generos) |>
  summarise(media_lucro = mean(lucro, na.rm = TRUE)) |>
  filter(!is.na(media_lucro)) |>
  mutate(generos = fct_reorder(generos, media_lucro)) |>
  ggplot(aes(y = generos, x = media_lucro)) +
  geom_col()

# quero os 10 generos mais frequentes

imdb_lucro |>
  separate_rows(generos, sep = ", ") |>
  mutate(generos = fct_lump_n(generos,
                              n = 10,
                              other_level = "outros")) |>
  count(generos) |>
  arrange(desc(n))

# grafico dos 10 generos mais frequentes (lucro medio)

imdb_lucro |>
  separate_rows(generos, sep = ", ") |>
  filter(!is.na(lucro), lucro > 0) |>
  mutate(generos = fct_lump_n(generos,
                                n = 10,
                              other_level = "outros")) |>
  group_by(generos) |>
  summarise(media_lucro = mean(lucro, na.rm = TRUE)) |>
  filter(!is.na(media_lucro)) |>
  mutate(generos = fct_reorder(generos, media_lucro)) |>
  ggplot(aes(y = generos, x = media_lucro)) +
  geom_col()

# generos que tenham mais de 100 filmes

imdb_lucro |>
  separate_rows(generos, sep = ", ") |>
  mutate(generos = fct_lump_min(generos,
                              min = 100,
                              other_level = "outros")) |>
  count(generos) |>
  arrange(desc(n))

# grafico generos que tem mais de 400 filmes (lucro medio)

imdb_lucro |>
  separate_rows(generos, sep = ", ") |>
  filter(!is.na(lucro), lucro > 0) |>
  mutate(generos = fct_lump_min(generos,
                              min = 400,
                              other_level = "outros")) |>
  group_by(generos) |>
  summarise(media_lucro = mean(lucro, na.rm = TRUE)) |>
  filter(!is.na(media_lucro)) |>
  mutate(generos = fct_reorder(generos, media_lucro)) |>
  ggplot(aes(y = generos, x = media_lucro)) +
  geom_col()

# agrupar os níveis menos frequentes, garantindo
# que "other" seja o nível menos frequente

imdb_lucro |>
  separate_rows(generos, sep = ", ") |>
  count(generos) |>
  arrange(desc(n))

imdb_lucro |>
  separate_rows(generos, sep = ", ") |>
  mutate(generos = fct_lump_lowfreq(generos,
                                other_level = "outros")) |>
  count(generos) |>
  arrange(desc(n))
