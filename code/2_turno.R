resultados <- read_delim(here("load_data", "resultados.csv"), 
                         ";",
                         escape_double = FALSE,
                         locale = locale(encoding = "Latin1"), 
                         trim_ws = TRUE) %>%
  select(sg_ue,
         nm_urna_candidato,
         sg_partido,
         ds_situacao,
         qt_votos_candidato,
         qt_votos_computados,
         qt_votos_brancos,
         qt_votos_nulos)

prefeitos2020 <- read_delim("https://raw.githubusercontent.com/marcofaga/eleicoes2020/master/prefeitos2020.csv", 
                            ";", escape_double = FALSE, trim_ws = TRUE)

source(here("code", "propostas2_aplica_buscador.R"))

sg_ue <- propostas %>% select(index, sg_ue)

pr <- propostas2 %>% 
  left_join(sg_ue) %>%
  mutate(sg_ue = as.integer(sg_ue)) %>% 
  left_join(resultados) %>% 
  mutate(pr_votos_candidato = qt_votos_candidato / qt_votos_computados)


colunas <- c(
  "transparencia",
  "corrupcao",
  "integridade",
  "governo_aberto",
  "acesso_a_informacao", 
  "controle_social",
  "dados_abertos"
)

situacao_freq <- pr %>%
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
    title = "Frequência relativa dos termos - por situação"
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






# cidades onde há 2ºT
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
  
plot_freq_termos <- function(token) {
    
    pivot_termos %>% 
      filter(termo == token) %>% 
      mutate(nm_ue = fct_reorder(nm_ue, freq_termo),
             termo = fct_reorder(termo, freq_termo)) %>% 
      ggplot(aes(x = nm_ue, y = freq_termo, fill = grau)) +
      geom_vline(aes(xintercept = nm_ue), lty = 2, color = "gray60") + 
      geom_point(shape = 21, size = 6, alpha = .5) +
      # geom_hline(data = pivot_termos_brasil %>% filter(termo == token),
      #            aes(yintercept = freq_termo, color = "Média\nBrasil"),
      #            lty = 2,
      #            size = 1) +
      scale_fill_manual(values = c("#FC4E07", "#E7B800", "#00AFBB"), drop=FALSE) +
      scale_color_manual(values = "red") +
      facet_wrap(~termo, scales = "free_x") +
      labs(x = NULL,
           y = NULL,
           fill = NULL,
           color = NULL) + 
      coord_flip() +
      theme_bw() +
      theme(plot.title = element_text(hjust = .5, vjust = .5, size = 18),
            legend.text = element_text(size = 13),
            axis.text = element_text(size = 12, face = "bold"),
            strip.text = element_text(size = 12, face = "bold"),
            strip.background = element_blank()
      ) + 
      guides(fill = guide_legend(ncol = 1), 
             color = guide_legend(title.position = "top", title.hjust = .5))
    
  }

plot_freq_termos("Transparência")
  
propostas %>% filter(nm_ue == "PIRACICABA") %>% select(index, url_proposta)
pr %>% filter(index == 232917)
pr %>% filter(nm_ue == "PIRACICABA")

