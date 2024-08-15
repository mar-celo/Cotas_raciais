library(readr)
library(dplyr)

cotas <- read_csv("data/cotas.csv", locale = locale(encoding = "ISO-8859-1"))

# adicionar mais uma coluna na condicao de cota receberá o valor 1 e 0 caso contrário

cotas <- cotas %>% mutate(cota = ifelse(tipo_cota == "Cota Racial", 1, 0))

# sumarizar os dados para saber a quantidade DISTINTOS de gr_mat por NO_ORGAO E NO_CARGO

subdata <- cotas %>%
  group_by(NO_ORGAO, NO_CARGO_ORIGEM) %>%
  summarise(count_gr_mat = n_distinct(gr_mat),
            cotas = sum(cota),
            Percentual.cotas = round(cotas/count_gr_mat,2),
            qte_20 = count_gr_mat * 0.2,
            diferenca = qte_20 - cotas)

write_csv(subdata, "data/subdata.csv")

data <- cotas %>%
  group_by(NO_ORGAO) %>%
  summarise(count_gr_mat = n_distinct(gr_mat),
            cotas = sum(cota),
            Percentual.cotas = round(cotas/count_gr_mat,2),
            qte_20 = count_gr_mat * 0.2,
            diferenca = qte_20 - cotas) |>
  ungroup() |>
  # CRIAR uma coluna com NO_CARGO_ORIGEM vazia e realocar depois de NO_ORGAO
  mutate(NO_CARGO_ORIGEM = '') |>
  select(NO_ORGAO, NO_CARGO_ORIGEM, everything())


write_csv(data, "data/data.csv")
