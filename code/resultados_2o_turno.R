# pkgs -------------------------------------------------------------------------
library(tidyverse)
library(here)
library(rjson)

# sources ----------------------------------------------------------------------
source(here("code/propostas0_candidaturas_validas.R"))
source(here("code/propostas2_aplica_buscador.R"))

# Monta url do json, faz a query, retorna lista -------------------------------- 
pega_json_2t <- function(uf, cod_municipio) {
  
  uf <- tolower(uf)
  
  "https://resultados.tse.jus.br/oficial/ele2020/divulgacao/oficial/427/dados-simplificados/%s/%s%s-c0011-e000427-r.json" %>% 
    sprintf(uf, uf, cod_municipio) %>% 
    readLines(warn = FALSE) %>%
    paste(collapse = "") %>% 
    fromJSON() 
  
}

#  organiza resultados por município -------------------------------------------
candidatos <- function(uf, cod_municipio) {
  
  p1 <- pega_json_2t(uf, cod_municipio) %>% 
    pluck("cand") %>%
    flatten() %>% 
    enframe() %>% 
    unnest(col = value) %>%
    pivot_wider(values_fn = list) %>% 
    unnest(cols = c(seq, sqcand, n, nm, cc, nv, e, st, dvt, vap, pvap)) %>% 
    transmute(
      seq = seq,
      sq_candidato = sqcand,
      sg_ue = as.integer(cod_municipio),
      nr_candidato = as.double(n),
      nm_urna_candidato = nm,
      sg_partido = case_when(
        str_detect(cc, "[[:upper:]]\\s+-\\s+[[:upper:]]") ~ str_extract(cc, "^.+(?=\\s-\\s.+$)"),
        TRUE ~ cc
      ),
      ds_composicao_coligacao = cc,
      ds_situacao = st,
      qt_votos_candidato = as.integer(vap),
      pr_votos_candidato = pvap %>% 
        str_squish() %>% 
        str_replace(",", ".") %>% 
        as.numeric()
    )
  
  # dados das eleições:
  p2 <- pega_json_2t(uf, cod_municipio) %>% 
    enframe() %>% 
    filter(name != "cand") %>% 
    unnest(value) %>% 
    pivot_wider() %>%
    select(-starts_with("p")) %>%
    transmute(
      cd_eleicao = as.integer(ele),
      sg_ue = as.integer(cdabr),
      sg_abrangencia = ifelse(tpabr == "mu", "MUNICIPAL", NA),
      turno = t,
      cd_cargo = carper,
      qt_sessoes = as.integer(s),
      qt_sessoes_t = as.integer(st),
      qt_sessoes_nt = as.integer(snt),
      qt_sessoes_i = as.integer(si),
      qt_sessoes_ni = as.integer(sni),
      qt_sessoes_apuradas = as.integer(sa),
      qt_sessoes_nao_apuradas = as.integer(sna),
      qt_eleitores = as.integer(e),
      qt_eleitores_aptos = as.integer(ea),
      qt_eleitores_nao_aptos = as.integer(ena),
      qt_eleitores_si = as.integer(esi),
      qt_eleitores_sni = as.integer(esni),
      qt_eleitores_compareceram = as.integer(c),
      qt_abstencoes = as.integer(a),
      vscv = vscv,
      qt_votos_nominais = as.integer(vnom),
      qt_votos_computados = as.integer(vvc),
      qt_votos_brancos = as.integer(vb),
      qt_votos_nulos = as.integer(tvn),
      qt_votos_nulos_vn = as.integer(vn),
      qt_votos_nulos_vnt = as.integer(vnt),
      qt_votos_vp = as.integer(vp),
      qt_votos_validos = as.integer(vv),
      qt_votos_an = as.integer(van),
      qt_votos_anulados_sob_judice = as.integer(vansj),
      qt_votos_total = as.integer(tv)
    )
  
  p3 <- full_join(p1, p2)
  
  print(uf)
  print(cod_municipio)
  
  return(p3)
  
}

# Looping time ! ---------------------------------------------------------------

# iterações serão feitas nas cidades onde houve 2º turno
sg_ue_2t <- data.table::fread(here("load_data/resultados.csv")) %>%
  filter(ds_situacao == "2º turno") %>% 
  distinct(sg_ue) %>% 
  pull()

localidades_2t <- propostas %>% select(sg_ue, sg_uf) %>%
  filter(as.integer(sg_ue) %in% sg_ue_2t) %>% 
  distinct()

resultados <- map2(localidades_2t$sg_uf, localidades_2t$sg_ue, ~ candidatos(.x, .y))
resultados2 <- resultados %>% reduce(~ bind_rows(.x, .y))

# export to csv ----------------------------------------------------------------
write.csv2(resultados2, file = here("load_data/resultados_2o_turno.csv"), row.names = F)
