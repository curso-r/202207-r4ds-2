# EXEMPLO 1: Quero criar um conjunto fixo de colunas para várias aplicações diferentes

imdb <- read_csv("https://raw.githubusercontent.com/curso-r/main-r4ds-1/master/dados/imdb.csv")

inclui_variaveis_na_base <- function(dados, col){

  # quero string ou quero NSE??

  coluna_media = paste0(col, "_media")
  coluna_min = paste0(col, "_min")
  coluna_max = paste0(col, "_max")
  percentual_da_med = paste0(col, "_p_med")

  dados |>
    mutate(
      {{coluna_media}} := mean(.data[[col]], na.rm = TRUE),
      {{coluna_max}} := min(.data[[col]], na.rm = TRUE),
      {{coluna_min}} := max(.data[[col]], na.rm = TRUE),
      {{percentual_da_med}} := (.data[[col]]/.data[[coluna_media]]-1)*100
    )
}

imdb_tidy <- imdb |>
  inclui_variaveis_na_base("duracao") |>
  inclui_variaveis_na_base("receita") |>
  inclui_variaveis_na_base("receita_eua")

# EXEMPLO 2: Quero criar vários gráficos

# no geral, quando eu quiser iterar por várias variáveis
# vai ser mais fácil usar string do que NSE, mas o que
# a gente viu hoje é útil pra viabilizar o novo código abaixo

colunas <- names(mtcars)

cruzado <- cross2(colunas[1:3], colunas[1:3])

faz_grafico_dispersao <- function(dados, coluna1, coluna2, variavel_cor){

  dados |>
    mutate(
      X = .data[[coluna1]],
      Y = .data[[coluna2]]
    ) |>
    ggplot(aes(x = X, y = Y, color = {{variavel_cor}})) +
    geom_point() +
    labs(x = coluna1, y = coluna2)
}

todos_os_graficos_de_dispersao <-
  map(cruzado, ~ faz_grafico_dispersao(
    dados = mtcars, .x[[1]], .x[[2]], variavel_cor = cyl))

# COMENTÁRIO: aspas X sem aspas

inclui_variaveis_na_base_nse <- function(dados, col){

  dados |>
    mutate(
      coluna_media = mean({{col}}, na.rm = TRUE),
      coluna_max = min({{col}}, na.rm = TRUE),
      coluna_min = max({{col}}, na.rm = TRUE),
      percentual_da_med = ({{col}}/coluna_media-1)*100
    )
}

imdb_tidy <- imdb |>
  inclui_variaveis_na_base(duracao) |>
  inclui_variaveis_na_base(receita) |>
  inclui_variaveis_na_base(receita_eua)

# EXEMPLO 3: ALTERA UMA COLUNA DIRETAMENTE A PARTIR DE UM LAYOUT
# NSE é bem legal

padroniza_coluna <- function(dados, col, ...){

  dados %>%
    mutate(
      {{col}} := log({{col}}),
      {{col}} := ({{col}}-mean({{col}}, ...))/sd({{col}}, ...)
    )

}

imdb_tidy <- imdb %>%
  padroniza_coluna(duracao, na.rm = TRUE) %>%
  padroniza_coluna(orcamento, na.rm = TRUE)

# EXEMPLO 4: TIDYR

separate(imdb, "data_lancamento", into = c("ano", "mes", "dia")) %>%
  View()

# no geral funciona com texto

meu_separar <- function(dados, variavel){

  separador <- "[^[:alnum:]]+"

  numero_de_quebras_maximo <- dados %>%
    summarise(
      n_quebras = 1+max(str_count({{variavel}}, separador))
    ) %>%
    pull(n_quebras)

  variaveis_para_criar <- paste0("nova_",1:numero_de_quebras_maximo)

  dados %>%
    separate({{variavel}}, into = variaveis_para_criar)
}

meu_separar(imdb, generos) %>% View()

meu_separar_str <- function(dados, variavel){

  separador <- "[^[:alnum:]]+"

  numero_de_quebras_maximo <- dados %>%
    summarise(
      n_quebras = 1+max(str_count(.data[[variavel]], separador))
    ) %>%
    pull(n_quebras)

  variaveis_para_criar <- paste0(variavel, "_",1:numero_de_quebras_maximo)

  dados %>%
    separate(variavel, into = variaveis_para_criar)
}

meu_separar_str(imdb, "generos") %>% View()

# EXEMPLO 5: AGRUPAMENTOS brilham com NSE

inclui_variaveis_na_base <- function(dados, col, grupo){

  # quero string ou quero NSE??

  coluna_media = paste0(col, "_media")
  coluna_min = paste0(col, "_min")
  coluna_max = paste0(col, "_max")
  percentual_da_med = paste0(col, "_p_med")

  dados %>%
    group_by({{grupo}}) %>%
    mutate(
      {{coluna_media}} := mean(.data[[col]], na.rm = TRUE),
      {{coluna_max}} := max(.data[[col]], na.rm = TRUE),
      {{coluna_min}} := min(.data[[col]], na.rm = TRUE),
      {{percentual_da_med}} := (.data[[col]]/.data[[coluna_media]]-1)*100
    )
}

library(minhas.funcoes)


imdb_tidy <- imdb %>%
  filter(ano > 1950) %>%
  inclui_variaveis_na_base("duracao", direcao) %>%
  inclui_variaveis_na_base("receita", ano) %>%
  inclui_variaveis_na_base("receita_eua", ano)
