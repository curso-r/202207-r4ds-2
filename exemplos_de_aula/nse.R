library(tidyverse)

# criar uma coluna:
mtcars |>
  mutate(
    kmg = mpg*1.6
  )

# fora do mutate não funciona
kmg = mpg*1.6

# criar uma coluna:
mtcars$kmg = mtcars$mpg*1.6

meu_select <- function(tabela, coluna){select(tabela, coluna)}

library(dplyr)
library("dplyr")

# o install.packages não aceita "NSE":
install.packages(dplyr)
# errado

install.packages("dplyr")
# certo

# no caso dos filtros NSE quebra vários galhos, mas teria outro jeito
# de fazer as coisas


starwars |>
  filter(height > 170)
# tem NSE acontecendo. ou seja
# "height > 170" não é interpretado literalmente como um argumento

starwars[starwars$height > 170, ]
# aqui não tem NSE:
starwars$height > 170
# isso aqui é já é um vetor de verdadeiros e falsos e entra diretamente
# na função

height <- starwars$height

starwars[height > 170,]
# tá mais parecido, mas ainda não é a mesma coisa:

# por exemplo, mudando o height tem diferença

height <- 180
starwars[height > 170,]

starwars |>
  filter(height > 170, homeworld == "Tatooine")

height <- starwars$height
homeworld <- starwars$homeworld

starwars[height > 170 & homeworld == "Tatooine",]


starwars %>% filter(is.na(birth_year)) |> nrow()

conta_linhas_na_coluna <- function(dados, col){
  dados %>% filter(is.na(col)) |> nrow()
}

conta_linhas_na_coluna(starwars, birth_year)
# não funciona!

conta_linhas_na_coluna(starwars, "birth_year")

starwars %>% filter(is.na("birth_year")) |> nrow()
starwars %>% filter(FALSE) |> nrow()

nrow(starwars[is.na(starwars[["birth_year"]]),])

conta_linhas_na_coluna_2 <- function(dados, col){
  nrow(dados[is.na(dados[[col]]),])
}
# essa função parece um pouco melhor, porque pelo menos funcionou!

conta_linhas_na_coluna_2(starwars, "birth_year")
conta_linhas_na_coluna_2(starwars, "mass")

conta_linhas_na_coluna <- function(dados, col){
  dados %>% filter(is.na({{col}})) |> nrow()
}

conta_linhas_na_coluna(starwars, birth_year)
conta_linhas_na_coluna(starwars, mass)

codigo_para_ajustar_regressao <- function(dados, y, x){
  lm({{y}} ~ {{x}}, data = dados)
}
# não funciona ^

X = 10

str_glue("O valor de X é {X}")

tira_na_e_sumariza <- function(dados, ...){
  dados |>
    select(...) |>
    filter(
      if_all(everything(), ~!is.na(.x))
    ) |>
    summarise(
      across(everything(), mean)
    )
}

starwars |>
  tira_na_e_sumariza(mass, height)

nycflights13::flights |>
  tira_na_e_sumariza(distance, arr_delay)



# {{}} no group_by

agrupa_e_sumarisa <- function(dados, coluna_agrupamento){
  dados |>
    group_by({{coluna_agrupamento}}) |>
    summarise(
      across(c(height, mass), .fns = mean, na.rm = TRUE)
    )
}

starwars |>
  agrupa_e_sumarisa(hair_color)

starwars |>
  agrupa_e_sumarisa(homeworld)

starwars |>
  agrupa_e_sumarisa(sex)

tira_na_agrupa_e_sumarisa <- function(dados, coluna_filtro, coluna_agrupamento){
  dados |>
    filter(!is.na({{coluna_filtro}})) |>
    group_by({{coluna_agrupamento}}) |>
    summarise(
      across(c(height, mass), .fns = mean, na.rm = TRUE)
    )
}

tira_na_agrupa_e_sumarisa(starwars, height, homeworld)

procura_string_especifica_em_hmwrld <- function(dados, sua_string = ""){
  dados |>
    filter(str_detect(homeworld, sua_string))
}

string_para_procurar = "ooin"

procura_string_especifica_em_hmwrld(starwars, string_para_procurar)

procura_string_especifica_em_coluna <- function(dados, coluna, sua_string = ""){
  dados |>
    filter(str_detect({{coluna}}, sua_string))
}

procura_string_especifica_em_coluna(starwars, homeworld, "ooin")

procura_string_especifica_em_coluna(starwars, homeworld, "ooin")

funcao1 <- function(a, b, c, ...){
  funcao2(a,b,c,...)
}

funcao2 <- function(d,e,f){
  print(paste0(d,e,f))
}
# se for muito pequeno o conteúdo não é legal

fazer_para_muitos <- function(lista){
  purrr::map(lista, fazer_para_um)
}

fazer_para_um <- function(elemento){
  ???
}

baixar_paginas <- function(urls){
  map(urls, baixar_uma_pagina)
}

baixa_uma_pagina <- function(url){
  httr::GET(url)
}

agrupa_e_tira_medias <- function(dados, grupo, ...){
  dados |>
    group_by({{grupo}}) |>
    select({{grupo}}, ...) |>
    summarise(
      across(.fns = mean)
    )
}

mtcars |> agrupa_e_sumarisa(cyl, wt, mpg, vs)

agrupa_e_sumarisa <- function(dados, grupo, ...){
  dados |>
    group_by({{grupo}}) |>
    summarise(
      ...
    )
}

mtcars |>
  agrupa_e_sumarisa(cyl, mpg = mean(mpg), wt = max(wt))

# curly curly funciona bem com filter, group_by, select

# com mutate e com summarise será que funciona tb?

criar_coluna_vezes_10 <- function(dados, coluna_alvo){
  dados |>
    mutate(
      coluna_nova = {{coluna_alvo}}*10
    )
}

criar_coluna_vezes_10(mtcars, mpg)
# beleza, funcionou!

# será que o {{}} funcionaria antes do igual também???

criar_coluna_vezes_10 <- function(dados, coluna_alvo){
  dados |>
    mutate(
      {{coluna_alvo}} = {{coluna_alvo}}*10
    )
}
# nem deixa criar a função!!!!


criar_coluna_vezes_10 <- function(dados, coluna_alvo){
  dados |>
    mutate(
      {{coluna_alvo}} := {{coluna_alvo}}*10
    )
}

mtcars |> criar_coluna_vezes_10(mpg)

# com mutate e com summarise será que funciona tb?
# RESPOSTA: não só! precisa do := junto do summarise + mutate
# pra ficar bom

agrupa_e_calcula_media_focada <- function(dados, coluna_grupo, nome_coluna_resumo, coluna_foco){
  dados |>
    group_by({{coluna_grupo}}) |>
    summarise(
      {{nome_coluna_resumo}} := mean({{coluna_foco}})
    )
}

starwars |>
  filter(!is.na(height)) |>
  agrupa_e_calcula_media_focada(
    homeworld,
    height_media,
    height
  )

# será que serve usar walrus no select?

nossa_renomeia <- function(dados, nome_velho, nome_novo){
  dados |>
    select({{nome_novo}} := {{nome_velho}})
}

mtcars |> nossa_renomeia(mpg, mpg_2)
# serve!!!

grafico_de_pontos <- function(dados, coluna_no_x, coluna_no_y){

  dados |>
    mutate(
      X = {{coluna_no_x}},
      Y = {{coluna_no_y}}
    ) |>
    ggplot(aes(x = X, y = Y)) +
    geom_point()
}

grafico_de_pontos <- function(dados, coluna_no_x, coluna_no_y, ...){
  dados |>
    ggplot(aes(x = {{coluna_no_x}}, y = {{coluna_no_y}})) +
    geom_point()
}

grafico_de_pontos <- function(dados, coluna_no_x, coluna_no_y){
  dados |>
    ggplot(aes(x = {{coluna_no_x}}, y = {{coluna_no_y}})) +
    geom_point()
}

cria_coluna_nova <- function(dados, nova_coluna, coluna_velha){
  dados |>
    mutate(
      nova_coluna = coluna_velha
    )
}

mtcars |> cria_coluna_nova("nome", "variavel")
