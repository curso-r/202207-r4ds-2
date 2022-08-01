library(purrr)
library(tidyverse)

meu_vetor <- c(
  a = "a",
  b = 1,
  c = 2
)

minha_lista <- list(
  a = "a",
  b = 1,
  c = 2,
  d = c(1,2,3),
  e = c(TRUE, FALSE),
  f = list(1,2)
)

meu_vetor
minha_lista

# um exemplo real

imdb <- read_csv("https://raw.githubusercontent.com/curso-r/main-r4ds-1/master/dados/imdb.csv")

imdb_atores <- imdb |>
  select(titulo, elenco, pais) |>
  separate(elenco, sep = ", ", into = c("elenco1", "elenco2", "elenco3"), extra = "drop")

imdb_atores_long <- imdb_atores |>
  pivot_longer(cols = elenco1:elenco3, names_to = "posicao_titulacao",
               values_to = "nome_pessoa")

imdb_atores_long

tabela <- imdb_atores_long |>
  pivot_wider(
    id_cols = titulo:pais,
    names_from = "posicao_titulacao",
    values_from = "nome_pessoa"
  ) |>
  filter(titulo %in% c("A Better Place", "The Shade", "Nob Hill"))

tabelas <- list(imdb, imdb_atores, imdb_atores_long)

# Estrutura

meu_vetor[1]
minha_lista[[1]]

minha_lista$c

list(num_criticas_critica = c(1,2,3),
     titulo = c("a", "b", "c"))

as.list(imdb)
# casos parecidos! ^

# funções mais importantes do purrr:

# praticamente todas as funções foram pensadas ou se
# beneficiam de receber uma lista como entrada

lista_que_vai_entrar_no_map <- tabela$elenco1

# a função é a mais importante. ela aplica em todos os elementos
# de uma lista uma certa função e compila os resultados
# em uma nova lista:

junta_nomes_dos_atores <- function(nomes_de_atores){
  paste0(nomes_de_atores, collapse= ", ")
}

junta_nomes_dos_atores(lista_que_vai_entrar_no_map[[1]])
junta_nomes_dos_atores(lista_que_vai_entrar_no_map[[2]])
junta_nomes_dos_atores(lista_que_vai_entrar_no_map[[3]])

tabela

map(lista_que_vai_entrar_no_map, junta_nomes_dos_atores)

map_chr(lista_que_vai_entrar_no_map, junta_nomes_dos_atores)

tabela |>
  select(elenco1) |>
  mutate(
    elenco1_armd_0 = map(elenco1, junta_nomes_dos_atores),
    elenco1_armd_1 = map_chr(elenco1, junta_nomes_dos_atores)
  )

# mais sobre o map

# é te ajudar a fazer iteração:

lista_nova = NULL
for(ii in 1:3){
  lista_nova[[ii]] <- junta_nomes_dos_atores(tabela$elenco1[[ii]])
}

# ii = 1
# lista_nova[[1]] <- junta_nomes_dos_atores(tabela$elenco1[[1]])

# ii = 2
# lista_nova[[2]] <- junta_nomes_dos_atores(tabela$elenco1[[2]])

# ii = 3
# lista_nova[[3]] <- junta_nomes_dos_atores(tabela$elenco1[[3]])

map(tabela$elenco1, junta_nomes_dos_atores)

read_csv("varios_dados/mtcars.csv")
read_csv("varios_dados/mtcars2.csv")

arquivos <- c("varios_dados/mtcars.csv", "varios_dados/mtcars2.csv")

arquivos_lidos <- map(arquivos, read_csv)

library(ggplot2)

minha_funcao <- function(coluna){
  ggplot(tibble(X = coluna), aes(x = X)) + geom_histogram()}

graficos = map(mtcars, minha_funcao)

map(lista_que_vai_entrar_no_map, junta_nomes_dos_atores)

tabela |>
  select(elenco1) |>
  mutate(
    elenco1_armd_0 = map(elenco1, ~paste0(.x, collapse = ", ")),
    elenco1_armd_1 = map_chr(elenco1, ~paste0(.x, collapse = ", "))
  )

# notação com "~" chama notação lambda

# três exemplos de map (repetições usando funções):

# 1 tinha uma list columns e eu quis transformar ela em outra list column ou até
# em uma lista

# 2 tinha um vetor de caminhos de arquivo e eu queria mandar a mesma função
# de leitura ser aplicada a eles

# 3 a partir das colunas de uma tabela montar vários gráficos aplicando
# a mesma função (um histograma no caso)

verifica_se_tem_comedia <- function(generos){
  "Comedy" %in% str_split(generos, pattern = ", ", simplify = TRUE)
}

exemplos <- "Comedy, Drama, Romance"

verifica_se_tem_comedia(exemplos)

imdb |>
  select(generos) |>
  mutate(
    generos_splitados = str_split(generos, pattern = ", "),
    tem_comedia1 = map_lgl(generos_splitados, ~ "Comedy" %in% .x),
    tem_comedia2 = map_lgl(generos, verifica_se_tem_comedia)
  ) |>
  select(generos, generos_splitados, tem_comedia1, tem_comedia2)

# pensando com funções...

# sem função:
mtcars |>
  mutate(
    mpg_arrendondado = round(mpg, 1),
    drat_arredondado = round(drat, 1)
  )

# com função:

arredondamento_para_1_digito <- function(coluna){
  round(coluna, 1)
}

mtcars |>
  mutate(
    mpg_arrendondado = arredondamento_para_1_digito(mpg),
    drat_arredondado = arredondamento_para_1_digito(drat)
  )

meu_vetor[1]

# solução do purrr

minha_lista[1]
# se quero um pedaço da lista, faça ^

pluck(minha_lista, 1)
# minha_lista[[1]]
# se quero seleções aninhadas, faço ^

pluck(minha_lista, 4, 2)

#minha_lista[[6]][[1]][1]
pluck(minha_lista, 6, 1)

# foreach + %dopar% vs furrr

# purrr vs base

?lapply
# lapply vc passa X e recebe uma lista que é a aplicação de FUN em X
# que nem o map
# lapply é parecido com map

# sapply é que nem lapply mas se der ele transforma em vetor.
# sapply é parecido com map_chr, map_dbl e amigos, só que com menos controle

# apply não tem muito um equivalente no purrr, porque o apply foi feito
# pensando em matrizes

# o mapply é tipo o map2 ou pmap

# e o tapply não tem correspondencia ele é mais parecido com usar
# map + dplyr, across etc

nomes_dos_arquivos <- c("mtcars.csv", "mtcars2.csv", "iris.csv")
nomes_das_pastas <- c("varios_dados/", "varios_dados/", "varios_dados2/")

ler_arquivo <- function(pasta, nome){
  read_csv(paste0(pasta, nome))
}

map2(nomes_das_pastas, nomes_dos_arquivos, ler_arquivo)

# voltando pras list columns

nest
# criar list colums, quando eu quiser
unnest
# quebrar list columns, se o R conseguir

imdb |>
  select(id_filme, titulo, generos) |>
  mutate(
    generos_splitados = str_split(generos, pattern = ", ")) |>
  unnest(generos_splitados) |>
  filter(generos_splitados == "Drama")

imdb |>
  select(id_filme, titulo, generos) |>
  filter(str_detect(generos, "Drama"))

imdb |>
  group_by(ano) |>
  nest() |>
  ungroup() |>
  filter(ano > 1950) |>
  mutate(
    modelo_regressao = map(data, ~lm(nota_imdb ~ orcamento, data = .x)),
    coeficientes = map(modelo_regressao, ~coefficients(.x))
  ) |>
  unnest(coeficientes) |>
  mutate(
    tipo_variavel = rep(c("intercepto", "efeito_orcamento"), n()/2)
  ) |> ggplot(aes(x = ano, y = coeficientes, color = tipo_variavel)) +
  geom_point() +
  geom_line() + facet_wrap(~tipo_variavel, scales = 'free')


plota_nota_versus_duracao <- function(tabela){
  tabela |>
    ggplot(aes(x = duracao, y = nota_imdb)) +
    geom_point()
}

meus_graficos <- imdb |>
  group_by(ano) |>
  nest() |>
  ungroup() |>
  mutate(
    graficos_nota_duracao = map(data, plota_nota_versus_duracao)
  )
