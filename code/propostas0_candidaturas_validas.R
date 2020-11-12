# `propostas0` contem somente as candidaturas válidas 

# raw data ---------------------------------------------------------------------

library(tidyverse)
propostas <- read_csv("propostass.csv") %>% filter(!is.na(sg_partido))

# filter candidaturas ----------------------------------------------------------

candidaturas_validas <- c("DEFERIDO", "AGUARDANDO JULGAMENTO", "DEFERIDO COM RECURSO")

propostas0 <- propostas %>%
  filter(!is.na(texto)) %>%
  filter(ds_detalhe_situacao_cand %in% candidaturas_validas) %>% 
  select(index,
         sg_uf,
         nm_ue, #=nm_municipio
         sg_partido,
         nm_urna_candidato,
         texto) %>% 
  arrange(nm_ue)

# Inspeciona base --------------------------------------------------------------

propostas0

# candidatos sem leitura de pdf disponível: coluna texto é NA
propostas %>% filter(is.na(texto)) %>% count()

# leitura pdf não disponível pq não mandaram proposta: campo de url é vazio "[]"
propostas %>% filter(url_proposta == "[]") %>% count()

# leitura pdf não disponível pq a proposta está digitalizada: url não é vazio e texto é NA
propostas %>% filter(url_proposta != "[]" & is.na(texto)) %>% count()

# e também queremos somente candidaturas válidas
propostas %>% count(ds_detalhe_situacao_cand, ds_situacao_candidatura, sort = T)