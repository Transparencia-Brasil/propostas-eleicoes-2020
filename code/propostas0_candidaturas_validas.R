

# recebe `propostass.csv` e retorna `propostas0`
# `propostas0` contem somente as candidaturas válidas 

library(tidyverse)


# raw data ---------------------------------------------------------------------
propostas <- read_csv(here("raw_data", "propostass.csv")) %>% filter(!is.na(sg_partido))

# filter candidaturas ----------------------------------------------------------
candidaturas_validas <- c("DEFERIDO", "AGUARDANDO JULGAMENTO", "DEFERIDO COM RECURSO")

# UPDATE: candidaturas deferidas após a nossa coleta de dados:
deferidos_expost <- c("170000818682", "250001156020", "250000663708")

propostas0 <- propostas %>%
  filter(!is.na(texto)) %>%
  filter(ds_detalhe_situacao_cand %in% candidaturas_validas) %>% 
  bind_rows(
    
    propostas %>% filter(sq_candidato %in% deferidos_expost)
    
  ) %>% 
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

