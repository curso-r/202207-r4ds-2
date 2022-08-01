

faz_grafico_dispersao <- function(nome_x, nome_y){
  qplot(mtcars[[nome_x]],mtcars[[nome_y]]) +
    labs(x = nome_x, y = nome_y)
}

calcular_correlacao <- function(nome_x, nome_y){
  cor(mtcars[[nome_x]], mtcars[[nome_y]])
}

faz_grafico_dispersao("mpg", "wt")

colunas <- names(mtcars)

cruzado <- cross2(colunas, colunas)

todos_os_graficos_de_dispersao <- map(cruzado, ~faz_grafico_dispersao(.x[[1]], .x[[2]]))

todas_as_correlacoes  <- map_dbl(cruzado, ~calcular_correlacao(.x[[1]], .x[[2]]))

matrix(todas_as_correlacoes, nrow = length(colunas))
