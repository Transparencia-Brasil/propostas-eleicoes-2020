# Processamento do texto
# recebe `propostas0` prepara texto e guarda em `propostas1`
library(here)
library(tidyverse)

source(here("code", "propostas0_candidaturas_validas.R"))

propostas1 <- propostas0 %>% 
  mutate(
    
    # texto aparado, sem caracteres especiais e letra minúscula
    texto_tidy = texto %>% 
      str_replace_all("\r\n", " ") %>% 
      str_squish() %>% 
      tolower() %>% 
      abjutils::rm_accent()
    
  ) %>% 
  select(-texto)

# checkpoint (rds com 399mb)
saveRDS(propostas1, here("load_data", "propostas1.rds"))
