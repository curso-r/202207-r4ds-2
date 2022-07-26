library(stringr)
library(tidyverse)

string_qualquer <- '"'

c("a", "b", "c")
paste0(c("a", "b", "c"), collapse = "")
# jeito chato de concatenar

str_c("a", "b")
# jeito legal de concatenar

imdb <- read_csv("https://raw.githubusercontent.com/curso-r/main-r4ds-1/master/dados/imdb.csv")

# nchar
# paste

imdb |>
  mutate(
    label_para_grafico = ano %>% str_c("Ano de lançamento: ", .)
  ) |>
  select(label_para_grafico)

str_detect(
  c("Angelina Jolie, Brad Pitt", "Angelina Jolie, Michal Caine"),
  pattern = "")

str_view_all(sentences, "the|The")

vetor_exemplo <- c("baaaaaaaac", "baaaac", "bc", "bac", "baac", "baaac", "beec")

str_view(vetor_exemplo, "ba{2,5}c")

regex_de_conjuntos1 <- "b(a|d){2,5}c"
regex_de_conjuntos2 <- "b[ad]{2,5}c"
regex_de_conjuntos_de_numeros <- "b[e-g]{2,5}c"

str_view(vetor_exemplo, regex_de_conjuntos_de_numeros)

regex_cep <- "[0-9]{5}-[0-9]{3}"

# regex serve pra representar qualquer texto por uma "mascara" ou versao
# generica.

# ideias principais:
# caracteres com posições fixas
# quantificadores
# classes

vetor_de_emails <- c("meu e-mail é fulano@bol.com.br", "nosso email é contato@bla.org.zip. VALEU")

regex_email1 <- "[a-zA-Z0-9_.-]{1,100}@[a-zA-Z0-9]{1,100}\\.com\\.br"
regex_email2 <- "[a-zA-Z0-9_.-]{1,100}@[a-zA-Z0-9]{1,100}\\.org"
regex_email3 <- "[a-zA-Z0-9_.-]{1,100}@[a-zA-Z0-9]{1,100}\\.com"

regex_final <- str_c(regex_email1, regex_email2, regex_email3, sep = "|")

str_view(vetor_de_emails, regex_final)

str_extract(vetor_de_emails, regex_final)

regex_email_lucas <- "[a-zA-Z0-9_.-]{1,100}@[^ ]+"

str_extract(vetor_de_emails, regex_email_lucas)
# primeira tentativa

regex_email_lucas2 <- "[a-zA-Z0-9_.-]+@[^ ]+[a-zA-Z0-9]"

str_view(vetor_de_emails, regex_email_lucas2)

str_extract(vetor_de_emails, regex_email_lucas2)
# primeira tentativa

regex_telefone <- "(\\+[0-9]{2}\\([0-9]{2}\\) )?[0-9]{4,5}-[0-9]{4}"

lista_de_telefones <- c("meu telefone é: 96563-3243",
                        "o telefone da minha mãe é 5322-1234",
                        "o telefone gringo é +78(32) 6783-5234",
                        "his telephone is +55(11) 93242-6431")

str_view(lista_de_telefones, regex_telefone)

str_extract(lista_de_telefones, regex_telefone)

str_replace(lista_de_telefones, regex_telefone, "TELEFONE")

texto_zoado <- c("texto com espaços         a mais",
                 "           texto que começa muito depois do que deveria",
                 "COMEÇO INUTIL: conteúdo")

str_view(texto_zoado, "  +")

texto_limpo <- texto_zoado |>
  str_replace("  +", " ") |>
  # esse aqui poderia ser str_squish()
  str_replace("^ +", "") |>
  # esse aqui poderia ser str_remove
  #str_remove("^ +")
  # ou
  #str_trim()
  str_replace("COMEÇO INUTIL: ", "")
  # poderia ser remove

str_split(texto_limpo, pattern = "[ ._@]+")

str_count(texto_limpo, c("o", "a", "t"))

str_view_all(texto_limpo, "o")

str_extract_all(texto_limpo, "o")

regex_telefone <- "(\\+[0-9]{2}\\([0-9]{2}\\) )?[0-9]{4,5}-[0-9]{4}"

lista_de_telefones <- c("meu telefone é: 96563-3243, já o do meu irmão é 96583-3243",
                        "o telefone da minha mãe é 5322-1234",
                        "o telefone gringo é +78(32) 6783-5234",
                        "his telephone is +55(11) 93242-6431")


tabela_com_telefones <- tibble(lista_de_telefones)

saida <- tabela_com_telefones |>
  mutate(
    telefones = str_extract_all(lista_de_telefones,
                                regex_telefone, simplify = TRUE))

str_replace_all(texto_limpo, "o", "!")


texto_bruto <- "Autor: \n\n\n\n\t\t\t\t Justiça Pública"

regex_que_pega_autor <- "Autor:? +([:space:]|\n)+"

str_remove_all(texto_bruto, regex_que_pega_autor)

str_to_lower(texto_limpo)

str_to_upper(texto_limpo)

str_to_title(texto_limpo)

str_to_lower("Eu AmuUuUh VuCeiX")
str_to_title("nivel de emergencia 3")
str_to_sentence("nivel de emergencia 3")

stringi::stri_trans_general("Váríös àçêntõs", "Latin-ASCII")

