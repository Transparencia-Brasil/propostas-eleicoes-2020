# The unnest token function
# convert text to: `one-token-per-document-per-row`

# A token is a meaningful unit of text, most often a word,
# that we are interested in using for further analysis,
# and tokenization is the process of splitting text into tokens.

# SETUP ========================================================================
library(tidyverse)
library(here)
library(tidytext)
library(topicmodels)
library(tm)
`%notin%` <- function(x, y) !(x %in% y)

# . base propostas divulgacand -------------------------------------------------
{
  source(here("code/propostas0_candidaturas_validas.R"))
  source(here("code/propostas2_aplica_buscador.R"))
  ls()
  ls.str()
  
  # . de-para: propostas + resultados eleições -----------------------------------
  depara_propostas_prefs_eleitos <- propostas %>%
    transmute(index, sq_candidato = as.character(id_divulga))
  
  # . base eleição 2020 ----------------------------------------------------------
  # arquivos path
  resultado_eleicao_csv_path <- here("load_data") %>%
    list.files(pattern = "resultados", full.names = T)
  
  # abre arquivos 
  resultado_eleicao <- resultado_eleicao_csv_path %>% 
    map(data.table::fread) %>%
    reduce(bind_rows) %>% 
    as_tibble() %>% 
    mutate(sq_candidato = as.character(sq_candidato))
  
  # . base prefeitos eleitos -----------------------------------------------------
  prefs_eleitos <- resultado_eleicao %>% filter(ds_situacao == "Eleito")
  
  # . base propostas eleitas full ------------------------------------------------
  propostas_prefs_eleitos_full <- propostas2 %>% 
    left_join(depara_propostas_prefs_eleitos) %>%
    right_join(prefs_eleitos)
  
  # . base propostas eleitas -----------------------------------------------------
  propostas_prefs_eleitos <- propostas_prefs_eleitos_full %>% 
    filter(!is.na(texto_tidy)) %>%
    mutate(
      
      # TODO: subir mais esse tratamento no texto
      texto_tidy = str_replace_all(texto_tidy, "[[:punct:]]", " ") %>% str_squish(),
      texto_tidy = str_remove_all(texto_tidy, "[[:punct:]]"),
      texto_tidy = stringi::stri_trans_general(texto_tidy, "Latin-ASCII"),
      texto_tidy = iconv(texto_tidy, to = "ASCII//TRANSLIT"),
      texto_tidy = str_replace_all(texto_tidy, "pc do b", "pcdob")
      
    )
  
  # . guarda base --------------------------------------------------------------
  dados <- list(propostas2 = propostas2,
                depara_propostas_prefs_eleitos = depara_propostas_prefs_eleitos,
                resultado_eleicao_csv_path = resultado_eleicao_csv_path,
                resultado_eleicao = resultado_eleicao,
                prefs_eleitos = prefs_eleitos,
                propostas_prefs_eleitos_full = propostas_prefs_eleitos_full,
                propostas_prefs_eleitos = propostas_prefs_eleitos)
  
  
  myfile <- tempfile(fileext = ".rds")
  dados %>% saveRDS(myfile)

} # SETUP (rode 1 vez)

# . 
rm(list = ls()[ls() != "myfile"])
`%notin%` <- function(x, y) !(x %in% y)

# STOPWORDS ====================================================================

dados <- readRDS(myfile)

propostas_prefs_eleitos <- dados$propostas_prefs_eleitos %>% 
  mutate(texto_tidy = str_remove_all(texto_tidy, "[[:punct:]]"))

# . stopwords função -----------------------------------------------------------
df_stopwords_fun <- get_stopwords("pt") %>% 
  mutate(word = stringi::stri_trans_general(word, "Latin-ASCII"),
         word = iconv(word, to = "ASCII//TRANSLIT"))

# . stopwords custom -----------------------------------------------------------
df_stopwords_custom <- tibble(
  word = c("azc", "az", "io", "fim", "esf", "ano", "pra", "ser", "ars", "cid",
           "yi", "demais", "if", "aa", "pag"),
  lexicon = "Raul"
)

df_stopwords_extra <- tibble(
  word = c("municipio", "municipal", "cidade"),
  lexicon = "Extra"
)

# . stopwords partidos ---------------------------------------------------------
df_stopwords_partidos <- dados$propostas_prefs_eleitos %>% 
  distinct(sg_partido) %>% 
  transmute(
    word = sg_partido %>% 
      tolower() %>% 
      iconv(., to = "ASCII//TRANSLIT"),
    lexicon = "Partidos"
  )

# . junta stopwords ------------------------------------------------------------
df_stopwords <- bind_rows(
  
  df_stopwords_fun,
  df_stopwords_custom,
  df_stopwords_extra
  
)

# . unnest_tokens --------------------------------------------------------------
df_words <- propostas_prefs_eleitos %>% 
  select(sq_candidato, texto_tidy, sg_partido) %>% #sample_frac(.4) %>%  
  # . . remove stopwords ---------------
  unnest_tokens(word, texto_tidy) %>%
  mutate(
    
    word = word %>% 
      str_remove_all("\\d") %>%
      gsub("^(a)(a.+)$", "\\2", .) %>% 
      gsub("([^rs])\\1+", "\\1", .) %>% 
      gsub("^ss+|ss+$", "s", .) %>% 
      gsub("^rr+|rr+$", "r", .)
    
  ) %>% 
  filter(!str_detect(word, "^[[:alpha:]]$")) %>%
  filter(!str_detect(word, "^$")) %>% 
  anti_join(df_stopwords)

# . . remove textos muito sujo ---------
# alguns pdf estão ruins
textos_muito_sujos <- df_words %>% mutate(e = str_count(word)) %>%
  arrange(desc(e)) %>% 
  filter(e > 21) %>%
  #filter(sq_candidato == "50001028167") %>% 
  count(sq_candidato, sort = T) %>% 
  filter(n > 8) %>% 
  pull(sq_candidato)

# . . remove palavras muito grande ------
df_words <- df_words %>%
  filter(sq_candidato %notin% textos_muito_sujos) %>% 
  filter(str_count(word) < 21) %>% 
  group_by(word) %>% 
  filter(n() > 1) %>% ungroup()


# FREQUENCIA ===================================================================

# . candidato ------------------------------------------------------------------
contagem_prefs <- df_words %>% count(sq_candidato, word, sort = T)

contagem_prefs <- contagem_prefs %>% 
  left_join(dados$propostas_prefs_eleitos %>% select(sq_candidato, sg_partido))

# conta freq total no corpus
# contagem_full <- df_words %>% count(word, sort = T)

# limpa memória
# rm(list = ls()[ls() %notin% c("myfile", "%notin%", "contagem_cand", "contagem_partido", "coontagem_full")])

gc()

# TOPIC MODELING ==============================================================

my_topic_model <- function(df, id, partido) {
  
  id <- enquo(id)
  
  # cria uma document term matrix (matriz esparsa, necessária pra rodar o LDA)
  propostas_dtm <- df %>% cast_dtm(document = !!id, word, n)
  
  # roda LDA. No meu pc deu quase 10min
  propostas_lda <- LDA(propostas_dtm, k = 5, control = list(seed = 420))
  
  # extrai os topicos
  propostas_topics <- tidy(propostas_lda, matrix = "beta")
  
  # pega as 10 palavras mais associadas a cada tópico
  top_terms <- propostas_topics %>%
    group_by(topic) %>%
    top_n(10, beta) %>%
    ungroup() %>%
    arrange(topic, -beta)
  
  # gráfico com os top terms
  plot <- top_terms %>%
    mutate(term = reorder_within(term, beta, topic)) %>%
    ggplot(aes(term, beta, fill = factor(topic))) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ topic, scales = "free") +
    coord_flip() +
    scale_x_reordered() + labs(title = glue::glue("{partido}"))
  
  return(list(
    
    propostas_dtm = propostas_dtm,
    propostas_lda = propostas_lda,
    propostas_topics = propostas_topics,
    top_terms = top_terms,
    plot = plot
    
  ))
  
}

partidos <- contagem_prefs %>% distinct(sg_partido) %>% pull()

topic_model_partido <- function(partido) {
  
  message(glue::glue("\nProcessando propostas do partido: {partido}\n"))
  
  l <- contagem_prefs %>% 
    left_join(dados$propostas_prefs_eleitos %>% select(sq_candidato, sg_partido)) %>% 
    filter(sg_partido == partido) %>% 
    my_topic_model(sq_candidato, partido = partido)
  
  l$plot <- l$plot + labs(title = glue::glue("{partido}"))

  return(l)
  
}

prefeitos <- map(partidos, topic_model_partido)
