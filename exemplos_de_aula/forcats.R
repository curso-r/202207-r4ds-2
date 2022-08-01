library(forcats)

# existe fator no R

meu_fator <- as_factor(c("alto", "baixo", "médio", "baixo", "médio", "alto", "médio"))

as.numeric(meu_fator)
sort(meu_fator)

sort(c("baixo", "médio", "alto", "médio"))

c("aaaaaaaaaaaaaaaaaaaaaaaaaaabbbbbbbbbbbbbbbbbbbbbbbbbbbbbb",
"cccccccccccccccccccccccccccccccccdddddddddddddddddddddddd")

lubridate::month(data_formato_certo + months(0:10), label = TRUE)

fct_drop(meu_fator[meu_fator != "médio"])

fct_drop(meu_fator[!meu_fator %in% c("médio", "alto")], "médio")

fct_relabel(meu_fator, stringr::str_to_upper)

fct_relevel(meu_fator, "baixo", "médio")

meu_fator2 <- meu_fator

levels(meu_fator2) <- c("baixo", "médio", "alto")

# forcats brilhando

fct_relevel(fct_c(meu_fator2, as_factor(c("ruim", "ótimo"))), "ruim")

meu_fator2 |>
  fct_c(as_factor(c("ruim", "ótimo"))) |>
  fct_relevel("ruim")

fator_com_na <- as_factor(c("ruim", "médio", "bom", NA))

fct_explicit_na(fator_com_na, "não quis responder")

fct_inorder(meu_fator2, ordered = TRUE)

# Exemplo em um grafico ---------------------------------------------------

library(ggplot2)

imdb |>
  select(titulo, nota_imdb) |>
  mutate(
    faixa = ntile(nota_imdb, 10),
    faixa2 = cut(nota_imdb, quantile(nota_imdb))
  )

starwars |>
  mutate(
    sex = as_factor(sex),
    sex = fct_relabel(sex, stringr::str_to_title),
    sex = fct_explicit_na(sex, "Não sei"),
    sex = lvls_revalue(sex, c("Masc", "Nenhum", "Fem", "Herma", "Não sei"))
  ) |>
  group_by(sex) |>
  summarise(n = n()) |>
  mutate(
   sex = fct_reorder(sex, n),
   sex = fct_rev(sex)
  ) |>
  ggplot(aes(sex, n)) +
  geom_col() +
  theme_minimal()
