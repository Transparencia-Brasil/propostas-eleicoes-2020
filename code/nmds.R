
# Análise NMDS =================================================================

library(tidyverse)
library(vegan)
library(ggrepel)

source(here("code", "propostas0_candidaturas_validas.R"))
source(here("code", "propostas2_aplica_buscador.R"))


colunas <- c(
  "transparencia",
  "corrupcao",
  "integridade",
  "governo_aberto",
  "acesso_a_informacao", 
  "controle_social",
  "dados_abertos"
)


# define a semente para reproduzir resultado
# escolhe um valor aleatoreamente
# s <- sample(1:999, 1)
# s = 120 ajustou bem
set.seed(10)

# PARTIDOS =====================================================================
{
  # . Matriz input -------------------------------------------------------------
  partidos <- propostas2 %>% 
    select(-index) %>% 
    mutate(across(all_of(colunas), ~ ifelse(. == 0, ., 1))) %>% 
    group_by(sg_partido) %>%
    summarise(
      
      qt_candidaturas = n(),
      across(where(is.double), ~ sum(., na.rm = T))
      
    ) %>% 
    ungroup() %>% 
    
    rowwise() %>% 
    mutate(sumVar = sum(c_across(all_of(colunas)))) %>% 
    
    mutate(across(all_of(colunas), ~ (. / qt_candidaturas))) %>% 
    select(-c(qt_candidaturas, sumVar)) %>% 
    column_to_rownames("sg_partido") %>% 
    t()
  
  #inspec
  as_tibble(partidos)
  
  # . nmds ---------------------------------------------------------------------
  nmds_partido <-metaMDS(decostand(partidos, "hellinger"), 
                         distance = "euclidean", 
                         k = 4,
                         trymax = 10)
  # fit
  nmds_partido
  nmds_partido$stress
  stressplot(nmds_partido, main="Diagrama de Shepard")
  
  # . tidy nmds data -----------------------------------------------------------
  #Using the scores function from vegan to extract the site scores and convert to a data.frame
  site_scores_partido <- as.data.frame(scores(nmds_partido, "site"))
  site_scores_partido <- site_scores_partido %>% 
    mutate(
      site = rownames(site_scores_partido),
      site = str_replace_all(site, "_", " ") %>% 
        str_to_sentence() %>% 
        str_replace("cao", "ção") %>% 
        str_replace("en", "ên")
    )
  
  #Using the scores function from vegan to extract the species scores and convert to a data.frame
  species_scores_partido <- as.data.frame(scores(nmds_partido, "species"))
  species_scores_partido <- species_scores_partido %>% mutate(species = rownames(species_scores_partido))
} # Processa matriz para partidos

# CANDIDATOS ===================================================================
{
  # . Matriz input -------------------------------------------------------------
  candidatos <- propostas2 %>%
    # soma na linha
    rowwise() %>% 
    mutate(sumVar = sum(c_across(all_of(colunas)))) %>% 
    filter(sumVar > 0) %>% 
    
    
    group_by(sg_partido) %>% 
    mutate(qt_candidaturas_partido = n()) %>% 
    ungroup() %>%
    mutate(across(all_of(colunas),  ~ (. / qt_candidaturas_partido) * sumVar),
           index = paste0(sg_partido, index)) %>% 
    select(c("index", colunas)) %>%
    column_to_rownames("index") %>% 
    t()
  
  #inspec
  as_tibble(candidatos)
  
  # . nmds ---------------------------------------------------------------------
  nmds_candidato <-metaMDS(decostand(candidatos, "hellinger"),
                           distance = "euclidean",
                           k = 4,
                           trymax = 10)
  
  nmds_candidato
  nmds_candidato$stress
  stressplot(nmds_candidato, main = "Diagrama de Shepard")
  
  # . tidy nmds data -----------------------------------------------------------
  #Using the scores function from vegan to extract the site scores and convert to a data.frame
  site_scores_candidato <- as.data.frame(scores(nmds_candidato, "site"))
  site_scores_candidato <- site_scores_candidato %>% 
    mutate(
      site = rownames(site_scores_candidato),
      site = str_replace_all(site, "_", " ") %>% 
        str_to_sentence() %>% 
        str_replace("cao", "ção") %>% 
        str_replace("en", "ên")
    )
  site_scores_candidato
  
  #Using the scores function from vegan to extract the species scores and convert to a data.frame
  species_scores_candidato <- as.data.frame(scores(nmds_candidato, "species"))
  species_scores_candidato <- species_scores_candidato %>%
    mutate(species = rownames(species_scores_candidato))
  
  
  species_scores_candidato <- species_scores_candidato %>% 
    as_tibble() %>% 
    mutate(partido = gsub("(^\\D+)(\\d+$)", "\\1", species))
  
} # Processa NMDS para cada candidato

# VISUALIZAÇÂO =================================================================

{
  candidatos_grp <- species_scores_candidato %>% 
    left_join(
      
      # quantidade de candidaturas iguais (nas repetições de termos):
      propostas2 %>% 
        mutate(species = paste0(sg_partido, index)) %>% 
        group_by(
          transparencia,
          corrupcao,
          integridade,
          governo_aberto,
          acesso_a_informacao, 
          controle_social,
          dados_abertos
        ) %>%
        mutate(qt_candidaturas = n()) %>%
        ungroup() %>% 
        arrange(desc(qt_candidaturas)) %>% 
        select(species, qt_candidaturas)
      
    ) %>% 
    filter(!is.nan(NMDS1)) %>% 
    distinct(qt_candidaturas, NMDS1, NMDS2, NMDS3, NMDS4, .keep_all = T) %>% 
    mutate(
      fx = case_when(
        qt_candidaturas < 5 ~ "1 a 5",
        qt_candidaturas < 11 ~ "1 a 10",
        qt_candidaturas < 31 ~ "11 a 30",
        qt_candidaturas < 61 ~ "31 a 60",
        qt_candidaturas < 101 ~ "61 a 100",
        TRUE ~ "> 100"
        
      )
    )
  
  plot_nmds <- function(dim1, dim2) {
    
    d1 <- enquo(dim1)
    d2 <- enquo(dim2)
    
    #labs
    x_lbl <- rlang::as_name(d1) %>% str_extract("\\d")
    y_lbl <- rlang::as_name(d2) %>% str_extract("\\d")
    size_lbl <- c("1 a 5", "6 a 10", "11 a 30", "31 a 60",   "61 a 100", ">100")
    
    ggplot() +
      geom_jitter(data = candidatos_grp,
                  aes(
                    x = !! d1,
                    y = !! d2,
                    size = qt_candidaturas
                  ),
                  alpha = .1,
                  shape = 21,
                  fill = "seashell",
                  color = "black",
                  #stroke = 3,
                  #show.legend = F,
                  width = 0.02,
                  height = 0.02
      ) +
      geom_text(data = site_scores_candidato,
                aes(
                  x = !! d1,
                  y = !! d2,
                  label = site
                ),
                alpha = .8,
                fontface = "bold",
                size = 4.5) +
      geom_text_repel(data = species_scores_partido,
                      aes(
                        x = !! d1,
                        y = !! d2,
                        label = species,
                        color = cluster
                      ),
                      color = "red",
                      size = 3.5,
                      show.legend = F) +
      geom_point(data = species_scores_partido,
                 aes(
                   x = !! d1,
                   y = !! d2
                 ),
                 color = "firebrick",
                 size = 2,
                 show.legend = F) +
      theme_minimal() +
      scale_size_continuous(range = c(2, 30),
                            breaks = c(5, 10, 30, 60, 100, 500),
                            labels = size_lbl,
                            limits = c(0,2500)) +
      labs(
        title = "Análise NMDS:\nsimilaridade de partidos em torno do tema \"transparência\"",
        size = "Quantidade de\ncandidaturas\nna posição:",
        x = paste("Dimensão", x_lbl),
        y = paste("Dimensão", y_lbl),
        #subtitle = paste("SEED:", s),
        caption = "stress-value=0"
      ) +
      theme(plot.title = element_text(hjust = .5, vjust = .5, size = 18),
            axis.title = element_text(size = 10),
            axis.text = element_blank(),
            panel.grid = element_blank(),
            panel.border = element_rect(fill = "transparent"),
            legend.title = element_text(hjust = .5, vjust = .5, size = 12),
            legend.text = element_text(size = 12),
            plot.caption = element_text(size = 8, face = "italic"))
    
  }
} # Processa visualização

# Explora eixos
plot_nmds(dim1 = NMDS1, dim2 = NMDS2)
plot_nmds(dim1 = NMDS1, dim2 = NMDS3)
plot_nmds(dim1 = NMDS1, dim2 = NMDS4)

plot_nmds(dim1 = NMDS2, dim2 = NMDS3)
plot_nmds(dim1 = NMDS2, dim2 = NMDS4)
# 
plot_nmds(dim1 = NMDS3, dim2 = NMDS4)

candidatos_grp %>% count(qt_candidaturas) %>% print(n = Inf)

candidatos_grp %>% filter(qt_candidaturas == 4199)



