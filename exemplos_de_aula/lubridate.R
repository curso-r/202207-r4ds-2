library(lubridate)

# convertendo texto em data no R Básico

data_em_texto <- "2021-01-01"

as.numeric(as.Date(data_em_texto))

data_em_texto2 <- "01/01/2021"

as.Date(data_em_texto2, format = "%d/%m/%Y")

# no lubridate

ymd(data_em_texto)
dmy(data_em_texto2)

dmy_hms("25/07/2022 19:14:00")
dmy_hm("25/07/2022 19:14")

dmy("15 de abril de 2021", locale = "Portuguese_Brazil.1252")
# windows

dmy("15 de abril de 2021", locale = "pt_BR.UTF-8")
# linux

dmy("15 de abril de 2021")
# por padrão normalmente vai

dmy("April 15th 2021", locale = "en_US.UTF-8")
# linux

# as.Date x as_date

data_em_numero <- 18732

as_date(data_em_numero)

as.Date(data_em_numero, origin = as.Date("1970-01-01"))

# CUIDADO. números que vem do excel são problemáticos:

data_que_veio_do_excel <- 44767

as_date(data_que_veio_do_excel)
# errado

janitor::excel_numeric_to_date(data_que_veio_do_excel)
# correto

dmy_hms("25/07/2022 19:14:00", tz = "Europe/London")

# O BRILHO do lubridate

data_formato_certo <- dmy_hms("25/07/2022 19:14:00")

# no lubridate
day(data_formato_certo)
month(data_formato_certo, label = TRUE)
year(data_formato_certo)
wday(data_formato_certo, label = TRUE)

# no Base
weekdays(data_formato_certo)
months(data_formato_certo, abbreviate = TRUE)

# contras do Base:

# não sai em número/fator
# não tem "years"

format(data_formato_certo, "mês %m e ano %Y")
# não tem format no lubridate, mas o do base continua funcionando

data_formato_certo + months(0:3)

data_formato_certo + c(0, 30, 60, 90)*(60*60*24)

data_formato_certo + years(0:3)

data_formato_certo - months(1)

data_formato_certo - weeks(1)

data_formato_certo - years(1)

dif <- dmy("15/04/2021")-dmy("24/08/2020")

as.period(dif)/minutes(1)

# a coisa MAIS ÚTIL do lubridate

imdb <- read_csv("https://raw.githubusercontent.com/curso-r/main-r4ds-1/master/dados/imdb.csv")

imdb |>
  mutate(
    data_lancamento = as.Date(data_lancamento),
    mes_de_lancamento = floor_date(data_lancamento, "month"),
    mes_de_lancamento2 = month(data_lancamento)
  ) |>
  group_by(mes_de_lancamento) |>
  summarise(
    nota_imdb = mean(nota_imdb)
  ) |>
  ggplot(aes(x = mes_de_lancamento, y = nota_imdb)) +
  geom_point()

# dias uteis

# o lubridate não pega...

library(bizdays)

data(holidaysANBIMA, package = 'bizdays')
# carrega a lista de feriados até 2079

create.calendar(
  name = "novo_calendario",
  holidays = c(holidaysANBIMA,
               # holidays é um vetor de calendarios. o vetor holidaysANBIMA
               # contém os feriados do calendário da ANBIMA.
               as.Date(paste0(2003:2079, "-11-20"))),
  # aqui incluímos os feriados do
  # Dia da Conciência Negra no dia 20 de Novembro de todos os anos
  # de 2003 a 2079
  weekdays = c("saturday", "sunday")
  # aqui definimos quais dias da semana NÃO são considerados dias úteis
)

bizdays(dmy("01/07/2022"), dmy("25/07/2022"), cal = "novo_calendario")

dmy("25/07/2022")-dmy("01/07/2022")
