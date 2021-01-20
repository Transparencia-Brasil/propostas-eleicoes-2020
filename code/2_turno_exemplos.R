library(tidyverse)
library(here)
library(kableExtra)

# resultados -------------------------------------------------------------------
resultados <- read_delim(here("load_data", "resultados.csv"), 
                         ";",
                         escape_double = FALSE,
                         locale = locale(encoding = "Latin1"), 
                         trim_ws = TRUE) %>%
  select(sq_candidato,
         sg_ue,
         nm_urna_candidato,
         sg_partido,
         ds_situacao,
         qt_votos_candidato,
         qt_votos_computados,
         qt_votos_brancos,
         qt_votos_nulos)
 
# prefeitos2020 <- read_delim("https://raw.githubusercontent.com/marcofaga/eleicoes2020/master/prefeitos2020.csv", 
#                             ";", escape_double = FALSE, trim_ws = TRUE)

# propostas --------------------------------------------------------------------
source(here("code", "propostas2_aplica_buscador.R"))
source(here("code", "propostas0_candidaturas_validas.R"))

# propostas + resultados -------------------------------------------------------
sg_ue <- propostas %>% select(index, sg_ue, sq_candidato, url_proposta)

pr <- propostas2 %>% 
  left_join(sg_ue) %>%
  mutate(sg_ue = as.integer(sg_ue)) %>% 
  left_join(resultados) %>% 
  mutate(pr_votos_candidato = qt_votos_candidato / qt_votos_computados)

# total no 2º turno
res_2t <- resultados %>% 
  filter(ds_situacao == "2º turno") %>% 
  select(sg_ue, nm_urna_candidato, sg_partido, qt_votos_candidato, qt_votos_computados) %>%
  mutate(perc_votos_validos = qt_votos_candidato / qt_votos_computados) %>% 
  group_by(sg_ue) %>% 
  mutate(pos = ifelse(perc_votos_validos == max(perc_votos_validos), 1, 2)) %>% 
  ungroup()

# apresentação de propostas ----------------------------------------------------
res_2t_ <- pr %>% 
  filter(ds_situacao == "2º turno") %>% 
  select(sg_ue, nm_ue, nm_urna_candidato, qt_votos_candidato, qt_votos_computados, url_proposta) %>%
  mutate(perc_votos_validos = qt_votos_candidato / qt_votos_computados) %>% 
  full_join(res_2t) %>%
  mutate(proposta = ifelse(is.na(nm_ue), "Não apresentou\nproposta\nde governo", "Apresentou\nproposta\nde governo")) %>% 
  arrange(sg_ue) %>% 
  fill(nm_ue) %>% 
  arrange(sg_ue, pos) %>% 
  select(sg_ue:qt_votos_candidato, perc_votos_validos:proposta, url_proposta) %>% 
  print(n = Inf)

# res_2t_ %>% 
#   googlesheets4::write_sheet(ss = "1uH61t_mkKU-a5FySNRRqWe58Goqrif4pkqmlfaL9FYc", sheet = "2º turno2")

res_2t_  %>% 
  count(proposta) %>% 
  mutate(perc = n / sum(n)) %>% 
  ggplot(aes(x = fct_reorder(proposta, n),
             y = n,
             label = paste0(n, " (", scales::percent(perc), ")"))) +
  geom_col(fill = "#00AFBB", color = "black") +
  geom_text(hjust = -.3) +
  coord_flip() +
  scale_y_continuous(limits = c(0, 120), breaks = c(0, 10, 50, 100)) +
  labs(
    x = NULL,
    y = "Quantidade de candidaturas",
    title = "Candidaturas do 2º turno e apresentação\nde propostas pelos candidatos"
  ) +
  theme_bw() +
  theme(axis.text = element_text(size = 12),
        panel.grid = element_blank())

# colunas para operações -------------------------------------------------------
colunas <- c(
  "transparencia",
  "corrupcao",
  "integridade",
  "governo_aberto",
  "acesso_a_informacao", 
  "controle_social",
  "dados_abertos"
)

# cod cidades no 2ºT -----------------------------------------------------------
sg_ue_2t <- pr %>% 
  filter(ds_situacao == "2º turno") %>% 
  distinct(sg_ue, nm_ue) %>% 
  pull()

# uso dos termos: situação -----------------------------------------------------
situacao_freq <- pr %>%
  select(index:ds_situacao, -texto_tidy) %>%
  filter(sg_ue %in% sg_ue_2t) %>% 
  pivot_longer(col = all_of(colunas), names_to = "termo", values_to = "qt_mencoes") %>%
  filter(!is.na(ds_situacao)) %>% 
  mutate(
    termo = termo%>% 
      str_replace_all("_", " ") %>% 
      str_to_sentence() %>% 
      str_replace("cao", "ção") %>% 
      str_replace("en", "ên")
  ) %>% 
  group_by(termo, ds_situacao) %>% 
  summarise(qt_mencao_termo_situacao = sum(qt_mencoes),
            qt_candidatura_situacao = n(),
            freq_termo_situacao = mean(qt_mencoes)) %>% 
  ungroup() %>% 
  group_by(termo) %>% 
  mutate(freq_mencoes_termo = sum(qt_mencao_termo_situacao) / sum(qt_candidatura_situacao)) %>% 
  ungroup()

situacao_freq %>% 
  mutate(termo = fct_reorder(termo, freq_mencoes_termo),
         ds_situacao = factor(ds_situacao, levels = c("Não eleito", "2º turno", "Eleito"))) %>% 
  ggplot(aes(x = freq_termo_situacao, y = termo)) +
  geom_hline(aes(yintercept = termo), lty = 2, color = "gray60") +
  geom_point(aes(x = freq_mencoes_termo, color = "Média")) +
  geom_point(aes(fill = ds_situacao), shape = 21, alpha = .6, size = 8) +
  scale_color_manual(values = c("Média" = "black")) +
  scale_x_continuous(breaks = c(0, 1,  3),
                     labels = c(
                       "0",
                       "O termo é\ncitado 1 vez a cada\ncandidatura",
                       "O termo é\ncitado 3 vezes\n por candidatura")
  ) +
  scale_fill_manual(values = c("Não eleito" = "#FC4E07","Eleito" = "#00AFBB", "2º turno" = "#E7B800")) +
  labs(
    fill = NULL,
    color = NULL,
    x = NULL,
    y = NULL,
    title = "Frequência relativa dos termos\nnos municípios onde haverá 2º turno"
  )+
  theme_bw() +
  theme(legend.position = "top",
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_line(size = 2),
        plot.title = element_text(size = 18),
        legend.justification = c(0,1),
        axis.text.y = element_text(size = 13),
        axis.text.x = element_text(size = 12)) +
  guides(color = guide_legend(label.vjust = .2), fill = guide_legend(reverse = T))



# cod cidades no 2ºT -----------------------------------------------------------
sg_ue_2t <- pr %>% 
  filter(ds_situacao == "2º turno") %>% 
  distinct(sg_ue, nm_ue) %>% 
  pull()

pivot_termos <- pr %>% 
  mutate(across(transparencia:dados_abertos, ~ ifelse(index == 232917, . / 3, .))) %>% 
  filter(sg_ue %in% sg_ue_2t) %>%  
  select(index:ds_situacao, -texto_tidy) %>% 
  pivot_longer(col = all_of(colunas), names_to = "termo", values_to = "qt_mencoes") %>%
  filter(!is.na(ds_situacao)) %>% 
  mutate(
    termo = termo%>% 
      str_replace_all("_", " ") %>% 
      str_to_sentence() %>% 
      str_replace("cao", "ção") %>% 
      str_replace("en", "ên")
    
  ) %>% 
  group_by(termo, nm_ue) %>% 
  mutate(qt_termo = sum(qt_mencoes),
         qt_candidatura = n(),
         freq_termo = qt_termo / qt_candidatura) %>% 
  ungroup() %>% 
  mutate(
    grau = case_when(
      freq_termo < 0.5 ~ "menos de 1 menção em cada 2 propostas apresentada no município",
      freq_termo < 1.0 ~ "pelo menos 1 menção em cada 2 propostas apresentada no município",
      freq_termo >= 1.0 ~ "1 menção ou mais para cada proposta apresentada no município"
    ) %>%
      factor(
        levels = c(
          "menos de 1 menção em cada 2 propostas apresentada no município",
          "pelo menos 1 menção em cada 2 propostas apresentada no município",
          "1 menção ou mais para cada proposta apresentada no município"
        )
      )
  )

pivot_termos_candidato <- pr %>% 
  mutate(across(transparencia:dados_abertos, ~ ifelse(index == 232917, . / 3, .))) %>% 
  filter(sg_ue %in% sg_ue_2t) %>% 
  select(index:ds_situacao, -texto_tidy) %>% 
  pivot_longer(col = all_of(colunas), names_to = "termo", values_to = "qt_mencoes") %>%
  filter(!is.na(ds_situacao)) %>% 
  mutate(
    termo = termo%>% 
      str_replace_all("_", " ") %>% 
      str_to_sentence() %>% 
      str_replace("cao", "ção") %>% 
      str_replace("en", "ên")
    
  ) %>% 
  group_by(termo, nm_urna_candidato) %>% 
  mutate(qt_termo = sum(qt_mencoes),
         qt_candidatura = n(),
         freq_termo = qt_termo / qt_candidatura) %>% 
  ungroup() %>% 
  mutate(
    grau = case_when(
      freq_termo < 0.5 ~ "menos de 1 menção em cada 2 propostas apresentada no município",
      freq_termo < 1.0 ~ "pelo menos 1 menção em cada 2 propostas apresentada no município",
      freq_termo >= 1.0 ~ "1 menção ou mais para cada proposta apresentada no município"
    ) %>%
      factor(
        levels = c(
          "menos de 1 menção em cada 2 propostas apresentada no município",
          "pelo menos 1 menção em cada 2 propostas apresentada no município",
          "1 menção ou mais para cada proposta apresentada no município"
        )
      )
  )

munics_2t <- pivot_termos %>%
  filter(ds_situacao == "2º turno") %>% 
  distinct(nm_ue) %>%
  pull()

plot_freq_termos <- function(city) {
    
    pivot_termos %>% 
      filter(nm_ue == city) %>% 
      #filter(termo == token) %>% 
      mutate(nm_ue = fct_reorder(nm_ue, freq_termo),
             termo = fct_reorder(termo, freq_termo)) %>% 
      ggplot(aes(x = termo, y = freq_termo, fill = grau)) +
      geom_vline(aes(xintercept = termo), lty = 2, color = "gray60") + 
      geom_point(shape = 21, size = 6, alpha = .5) +
      geom_point(data = pivot_termos_candidato %>% filter(nm_ue == city),
                  aes(shape = ds_situacao, color = ds_situacao), size = 3) +
      # geom_hline(data = pivot_termos_brasil %>% filter(termo == token),
      #            aes(yintercept = freq_termo, color = "Média\nBrasil"),
      #            lty = 2,
      #            size = 1) +
      scale_fill_manual(values = c("#FC4E07", "#E7B800", "#00AFBB"), drop=FALSE) +
      scale_shape_manual(values = c(1, 4)) +
      facet_wrap(~ nm_ue, scales = "free_x") +
      labs(x = NULL,
           y = NULL,
           fill = NULL,
           color = NULL,
           shape = "Situação\ndo candidato") + 
      coord_flip() +
      theme_bw() +
      theme(plot.title = element_text(hjust = .5, vjust = .5, size = 18),
            legend.text = element_text(size = 13),
            axis.text = element_text(size = 10, face = "bold"),
            strip.text = element_text(size = 12, face = "bold"),
            strip.background = element_blank()
      ) + 
      guides(fill = guide_legend(ncol = 1, override.aes = list(shape = 21)), 
             color = FALSE,
             shape = guide_legend(nrow = 2, title.position = "top", title.hjust = .5))
    
  }

plot_freq_termos("SÃO PAULO") +
  plot_layout(guides = "collect") &
  theme(legend.position = 'top')



munics_2t[1:2]

p1 <- map(munics_2t[1:10], plot_freq_termos) %>%
  reduce(~ `+`(.x, .y)) +
  plot_layout(guides = "collect") &
  theme(legend.position = 'top')

(p1) + 
  plot_layout(nrow = 5) +
  plot_annotation(title = NULL) & 
  theme(plot.title = element_text(hjust = .5, vjust = .5, size = 18, face = "bold"))

# partidos no 2T ---------------------------------------------------------------

partidos_2t <- pr %>% 
  filter(ds_situacao == "2º turno") %>% 
  distinct(sg_partido) %>% 
  pull()

# total de partidos no 2T ------------------------------------------------------
pr %>% 
  filter(ds_situacao == "2º turno") %>% 
  count(sg_partido, sort = T) %>% 
  mutate(sg_partido = fct_reorder(sg_partido, n)) %>% 
  ggplot(aes(x = sg_partido, y = n, label = n)) +
  geom_col(fill = "#00AFBB", color = "black") +
  geom_text(hjust = -.3) +
  coord_flip() +
  labs(x = NULL,
       y = "Quantidade de candidaturas",
       title = "Quantidade de candidaturas no 2º turno - por partido") +
  theme_bw() +
  theme(axis.text = element_text(size = 12),
        panel.grid = element_blank())

# frequencia por partido no 2T -------------------------------------------------

pivot_termos <- pr %>% 
  filter(sg_partido %in% partidos_2t) %>% 
  select(-texto_tidy) %>% 
  pivot_longer(all_of(colunas), values_to = "qtd", names_to = "termo") %>% 
  mutate(
    termo = termo%>% 
      str_replace_all("_", " ") %>% 
      str_to_sentence() %>% 
      str_replace("cao", "ção") %>% 
      str_replace("en", "ên")
  ) %>% 
  group_by(termo, sg_partido) %>% 
  mutate(qt_termo_partido = sum(qtd),
         qt_candidatura = n(),
         freq_termo = qt_termo_partido / qt_candidatura) %>% 
  ungroup()  %>%
  mutate(
    grau = case_when(
      freq_termo < 0.5 ~ "menos de 1 menção em cada 2 propostas apresentada pelo partido",
      freq_termo < 1.0 ~ "pelo menos 1 menção em cada 2 propostas apresentada pelo partido",
      freq_termo >= 1.0 ~ "1 menção ou mais para cada proposta apresentada pelo partido"
    ) %>%
      factor(
        levels = c(
          "menos de 1 menção em cada 2 propostas apresentada pelo partido",
          "pelo menos 1 menção em cada 2 propostas apresentada pelo partido",
          "1 menção ou mais para cada proposta apresentada pelo partido"
        )
      )
  )

pivot_termos_2t <- pr %>% 
  filter(ds_situacao == "2º turno") %>%
  select(-texto_tidy) %>% 
  pivot_longer(all_of(colunas), values_to = "qtd", names_to = "termo") %>% 
  mutate(
    termo = termo%>% 
      str_replace_all("_", " ") %>% 
      str_to_sentence() %>% 
      str_replace("cao", "ção") %>% 
      str_replace("en", "ên")
  ) %>% 
  group_by(termo, sg_partido) %>% 
  mutate(qt_termo_partido = sum(qtd),
         qt_candidatura = n(),
         freq_termo = qt_termo_partido / qt_candidatura) %>% 
  ungroup()  %>%
  mutate(
    grau = case_when(
      freq_termo < 0.5 ~ "menos de 1 menção em cada 2 propostas apresentada pelo partido",
      freq_termo < 1.0 ~ "pelo menos 1 menção em cada 2 propostas apresentada pelo partido",
      freq_termo >= 1.0 ~ "1 menção ou mais para cada proposta apresentada pelo partido"
    ) %>%
      factor(
        levels = c(
          "menos de 1 menção em cada 2 propostas apresentada pelo partido",
          "pelo menos 1 menção em cada 2 propostas apresentada pelo partido",
          "1 menção ou mais para cada proposta apresentada pelo partido"
        )
      )
  )


termo <- pivot_termos %>% distinct(termo) %>% pull()

plot_freq_termos <- function(token) {
  
  pivot_termos <- pivot_termos %>%
    filter(termo == token) %>%
    mutate(sg_partido = fct_reorder(sg_partido, freq_termo),
           termo = fct_reorder(termo, freq_termo))
  
  pivot_termos_2t %>%
    filter(termo == token) %>%
    mutate(sg_partido = fct_reorder(sg_partido, freq_termo),
           termo = fct_reorder(termo, freq_termo)) %>%
    ggplot(aes(x = sg_partido, y = freq_termo, fill = grau)) +
    geom_vline(aes(xintercept = sg_partido), lty = 2, color = "gray60") +
    geom_point(shape = 21, size = 6) +
    geom_point(data = pivot_termos,
               aes(x = sg_partido, y = freq_termo, shape = "Média do partido no Brasil")) +
    scale_fill_manual(values = c("#FC4E07", "#E7B800", "#00AFBB"), drop=FALSE) +
    facet_wrap(~termo, scales = "free_x") +
    labs(x = NULL,
         y = NULL,
         fill = NULL) + 
    coord_flip() +
    theme_bw() +
    theme(plot.title = element_text(hjust = .5, vjust = .5, size = 18),
          legend.text = element_text(size = 13),
          axis.text = element_text(size = 12, face = "bold"),
          strip.text = element_text(size = 12, face = "bold"),
          strip.background = element_blank()
    ) + 
    guides(fill = guide_legend(ncol = 1))
  
  
}

plot_freq_termos("Transparência")


pr %>%
  select(-texto_tidy) %>% 
  filter(sg_ue %in% sg_ue_2t) %>% 
  filter(
    across(c(transparencia:controle_social), ~ . == 0)
  ) %>%
  filter(dados_abertos != 0) %>% 
  filter(ds_situacao == "2º turno") %>% 
  left_join(
    propostas %>% select(sq_candidato, url_proposta)
  ) %>% 
  googlesheets4::write_sheet(ss = "19d00xCoB0qxmUSiVKG5Au82vifdVoRq09X0X0Z-QSFs", sheet = "somente dados abertos")


library(magrittr)
propostas2 %>% 
  filter(str_detect(texto_tidy, "abertura d..* dados*"))

propostas2 %>% 
  filter(str_detect(texto_tidy,  "dados* abertos*|abertura d..* dados*"))

(1566/15731)*100
 (138/15731)*100
