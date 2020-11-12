# Processamento do texto
# recebe `propostas0` prepara texto e guarda em `propostas1`

source("code/propostas0_candidaturas_validas.R")

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

# checkkpoint
saveRDS(propostas1, "propostas.rds")