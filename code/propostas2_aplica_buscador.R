
# Recebe `propostas1`, aplica função `buscador()` e retorna `proposta2`

propostas1 <- readRDS(here("load_data/propostas1.rds"))

buscador <- function(df) {
  
  df <- df %>% 
    mutate(
      
      # recebe `df` e aplica o buscador
      transparencia = str_count(texto_tidy, "transparen"),
      corrupcao = str_count(texto_tidy, "corrupcao"),
      integridade = str_count(texto_tidy, "integridade"),
      governo_aberto = str_count(texto_tidy, "governo aberto"),
      #inteligencia_artificial = str_count(texto_tidy, "inteligencia artificial"),
      acesso_a_informacao = str_count(texto_tidy, "acesso a informacao"),
      controle_social = str_count(texto_tidy, "controle social"),
      dados_abertos = str_count(texto_tidy, "dados* abertos*|abertura d..* dados*")
      
    )
  
  return(df)
}

propostas2 <- buscador(propostas1)
