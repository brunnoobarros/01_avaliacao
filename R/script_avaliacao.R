#==============================
# Teste da questão 01.
# Bruno Barros dos Passos
# bruno.barros79@yahoo.com.br
#==============================

# Carregando o pacote
library(tidyverse)

#Rodando a Questão 01.

dados_prm <- read_csv("dados/brutos/peixes_rio_madeira.csv")

dados_prm %>% view()

(a)

dados_prm %>%
  group_by(ordem) %>%
  summarise(n = n()) %>%
  arrange(n)

