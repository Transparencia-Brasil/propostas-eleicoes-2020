Relatório
================

  - [Total de candidaturas por UF](#total-de-candidaturas-por-uf)
      - [Prevalência dos termos](#prevalência-dos-termos)
      - [Frequência relativa dos
        termos](#frequência-relativa-dos-termos)
          - [Por partido](#por-partido)
          - [Por genero](#por-genero)
          - [Capitais](#capitais)
      - [NMDS](#nmds)

``` r
library(tidyverse)
library(vegan)
library(sf)
library(janitor)
```

``` r
source("../code/propostas0_candidaturas_validas.R")
source("../code/propostas2_aplica_buscador.R")
```

# Total de candidaturas por UF

``` r
propostas1 %>%
  count(sg_uf, sort = TRUE) %>% 
  ggplot(
    aes(x = fct_reorder(sg_uf, n, .desc = T),
        y = n,
        label = format(n, big.mark = ".", decimal.mark = ","))
    ) + 
  geom_col() + 
  geom_text(hjust = -.1, size =3) +
  coord_flip() +
  scale_y_continuous(breaks = c(0,1000,2000, 3000),
                     labels = format(c(0,1000,2000, 3000), big.mark = ".", decimal.mark = ","),
                     limits = c(0,2500),
                     expand = c(.005,0)) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  labs(
    x = NULL,
    y = NULL,
    title = "Total de candidaturas por Unidade da Federação",
    subtitle = "Eleições municipais 2020"
  )
```

<img src="visualizacoes_files/figure-gfm/unnamed-chunk-3-1.png" style="display: block; margin: auto;" />

## Prevalência dos termos

``` r

termos_resumo <- propostas2 %>% 
  select(-texto_tidy) %>% 
  pivot_longer(-c(index:nm_urna_candidato), names_to = "termo", values_to = "qtd_mencoes") %>% 
  group_by(termo) %>% 
  summarise(qtd_mencoes = sum(qtd_mencoes)) %>% 
  ungroup() %>% 
  mutate(
    termo = termo %>% 
      str_replace_all("_", " ") %>% 
      str_to_sentence() %>% 
      str_replace("cao", "ção") %>% 
      str_replace("en", "ên") %>% 
      fct_reorder(qtd_mencoes),
   per = qtd_mencoes / sum(qtd_mencoes)

  ) 

lbl <- paste0(format(termos_resumo$qtd_mencoes, decimal.mark = ",", big.mark = ".", justify = "centre"),
               " (",  scales::percent(termos_resumo$per, justify = "centre", accuracy = 1), ")")

termos_resumo %>% 
  ggplot(aes(x = termo, y = qtd_mencoes)) + 
  geom_col(fill = "#00AFBB", color = "black") + 
  geom_text(aes(label = lbl), hjust = -.1) +
  scale_y_continuous(breaks = seq(0,30000, 10000),
                     labels = format(seq(0,30000, 10000), decimal.mark = ",", big.mark = "."),
                     limits = c(0, 50000)) +
  coord_flip() +
  labs(x = NULL, y = NULL, title = "Quantidade de menções de cada termo em todas as candidaturas") +
  theme_bw() + 
  theme(panel.grid = element_blank(),
        axis.text = element_text(size = 12))
```

<img src="visualizacoes_files/figure-gfm/unnamed-chunk-4-1.png" style="display: block; margin: auto;" />

## Frequência relativa dos termos

### Por partido

``` r
pivot_termos <- propostas2 %>% 
  select(-texto_tidy) %>% 
  pivot_longer(transparencia:dados_abertos, values_to = "qtd", names_to = "termo") %>% 
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
  
  pivot_termos %>% 
    filter(termo == token) %>% 
    mutate(sg_partido = fct_reorder(sg_partido, freq_termo),
           termo = fct_reorder(termo, freq_termo)) %>% 
    ggplot(aes(x = sg_partido, y = freq_termo, fill = grau)) +
    geom_vline(aes(xintercept = sg_partido), lty = 2, color = "gray60") + 
    geom_point(shape = 21, size = 6, alpha = .5) +
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
```

``` r
library(patchwork)
p1 <- map(termo[1:2], plot_freq_termos) %>%
  reduce(~ `+`(.x, .y)) +
  plot_layout(guides = "collect") &
  theme(legend.position = 'top')

p2 <- map(termo[3:4], plot_freq_termos) %>% 
  reduce(~ `+`(.x, .y)) &
  theme(legend.position = 'none')

p1 / p2 + 
  plot_annotation(title = 'Frequência relativa dos termos - por partido') & 
  theme(plot.title = element_text(hjust = .5, vjust = .5, size = 18, face = "bold"))
```

<img src="visualizacoes_files/figure-gfm/unnamed-chunk-6-1.png" style="display: block; margin: auto;" />

``` r
p3 <- map(termo[5:6], plot_freq_termos) %>%
  reduce(~ `+`(.x, .y)) +
  plot_layout(guides = "collect") &
  theme(legend.position = 'top')

p4 <- map(termo[7], plot_freq_termos) %>%
  reduce(~ `+`(.x, .y)) &
  theme(legend.position = 'none')

(p3 + p4) + 
  plot_layout(nrow = 2) +
  plot_annotation(title = 'Frequência relativa dos termos - por partido') & 
  theme(plot.title = element_text(hjust = .5, vjust = .5, size = 18, face = "bold"))
```

<img src="visualizacoes_files/figure-gfm/unnamed-chunk-7-1.png" style="display: block; margin: auto;" />

### Por genero

``` r
caract <- propostas %>% select(index, ds_genero)

propostas3 <- left_join(propostas2, caract)

colunas <- c(
  "transparencia",
  "corrupcao",
  "integridade",
  "governo_aberto",
  "acesso_a_informacao", 
  "controle_social",
  "dados_abertos"
)

genero <- propostas3 %>% 
  count(ds_genero) %>%
  mutate(tot = sum(n), per = n / tot,
         proposta_legivel = "Propostas\nlegíveis\npor máquina") %>% 
  bind_rows(
    
    caract %>% 
      anti_join(.,select(propostas3, index, ds_genero)) %>% 
      count(ds_genero) %>% 
      mutate(tot = sum(n), per = n / tot,
             proposta_legivel = "Sem proposta ou\nproposta\nilegível")
    
  )

lbl <- paste0(format(genero$n, decimal.mark = ",", big.mark = ".", justify = "centre"),
              " (",  scales::percent(genero$per, justify = "centre", accuracy = 1), ")")

genero %>% 
  ggplot(aes(x = proposta_legivel, y = n, fill = ds_genero)) +
  geom_col(position = "dodge",
           color = "black") +
  geom_text(
    aes(label = lbl),
    size = 4.5,
    hjust = -.01,
    position=position_dodge(0.9)
  ) +
  coord_flip() +
  labs(x = NULL,
       y = NULL,
       title = "Número e percentual de candidaturas por gênero",
       fill = NULL) + 
  ylim(c(0,16000)) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        legend.position = "top",
        plot.title = element_text(size = 18),
        # plot.title = element_text(hjust = .5, vjust = .5),
        legend.justification = c(0,1),
        axis.text.y = element_text(size = 15)) +
  scale_fill_manual(values = c( "#00AFBB", "#E7B800"))
```

<img src="visualizacoes_files/figure-gfm/unnamed-chunk-8-1.png" style="display: block; margin: auto;" />

``` r
genero_freq <- propostas3 %>% 
  select(-texto_tidy) %>% 
  pivot_longer(col = all_of(colunas), names_to = "termo", values_to = "qt_mencoes") %>%
  mutate(
    termo = termo%>% 
      str_replace_all("_", " ") %>% 
      str_to_sentence() %>% 
      str_replace("cao", "ção") %>% 
      str_replace("en", "ên")
  ) %>% 
  group_by(termo, ds_genero) %>% 
  summarise(qt_mencao_termo_genero = sum(qt_mencoes),
            qt_candidatura_genero = n(),
            freq_termo_genero = mean(qt_mencoes)) %>% 
  ungroup() %>%
  group_by(termo) %>% 
  mutate(freq_mencoes_termo = sum(qt_mencao_termo_genero) / sum(qt_candidatura_genero)) %>% 
  ungroup()

genero_freq %>% 
  mutate(termo = fct_reorder(termo, freq_mencoes_termo)) %>% 
  ggplot(aes(x = freq_termo_genero, y = termo)) +
  geom_hline(aes(yintercept = termo), lty = 2, color = "gray60") +
  # geom_vline(aes(xintercept = .5), color = "gray60") +
  geom_point(aes(x = freq_mencoes_termo, color = "Média")) +
  geom_point(aes(fill = ds_genero), shape = 21, alpha = .6, size = 8) +
  scale_color_manual(values = c("Média" = "black")) +
  scale_x_continuous(breaks = c(0, .5,  2),
                     labels = c(
                       "0",
                       "O termo é\ncitado 1 vez a cada\n2 candidaturas",
                       "O termo é\ncitado 2 vezes\n por candidaturas")
                     ) +
  scale_fill_manual(values = c("#00AFBB", "#E7B800")) +
  labs(
    fill = NULL,
    color = NULL,
    x = NULL,
    y = NULL,
    title = "Frequência relativa dos termos - por gênero"
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
```

<img src="visualizacoes_files/figure-gfm/unnamed-chunk-9-1.png" style="display: block; margin: auto;" />

### Capitais

``` r
capitais <- toupper(
  c(
    "Rio Branco",
    "Maceió",
    "Macapá",
    "Manaus",
    "Salvador",
    "Fortaleza",
    "Brasília",
    "Vitória",
    "Goiânia",
    "São Luís",
    "Cuiabá",
    "Campo Grande",
    "Belo Horizonte",
    "Belém",
    "João Pessoa",
    "Curitiba",
    "Recife",
    "Teresina",
    "Rio de Janeiro",
    "Natal",
    "Porto Alegre",
    "Porto Velho",
    "Boa Vista",
    "Florianópolis",
    "São Paulo",
    "Aracaju",
    "Palmas"
  )
)

pivot_termos <- propostas2 %>% 
  filter(nm_ue %in% capitais) %>% 
  select(-texto_tidy) %>% 
  pivot_longer(transparencia:dados_abertos, values_to = "qtd", names_to = "termo") %>% 
  mutate(
    termo = termo%>% 
      str_replace_all("_", " ") %>% 
      str_to_sentence() %>% 
      str_replace("cao", "ção") %>% 
      str_replace("en", "ên")
  ) %>% 
  group_by(termo, nm_ue) %>% 
  mutate(qt_termo_capital = sum(qtd),
         qt_candidatura = n(),
         freq_termo = qt_termo_capital / qt_candidatura) %>% 
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

pivot_termos_brasil <- propostas2 %>% 
  select(-texto_tidy) %>% 
  pivot_longer(transparencia:dados_abertos, values_to = "qtd", names_to = "termo") %>% 
  mutate(
    termo = termo%>% 
      str_replace_all("_", " ") %>% 
      str_to_sentence() %>% 
      str_replace("cao", "ção") %>% 
      str_replace("en", "ên")
  ) %>% 
  group_by(termo) %>% 
  summarise(qt_termo_capital = sum(qtd),
         qt_candidatura = n(),
         freq_termo = qt_termo_capital / qt_candidatura) %>% 
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
      geom_hline(data = pivot_termos_brasil %>% filter(termo == token),
                 aes(yintercept = freq_termo, color = "Média\nBrasil"),
                 lty = 2,
                 size = 1) +
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
  
```

``` r
p1 <- map(termo[1:2], plot_freq_termos) %>%
  reduce(~ `+`(.x, .y)) +
  plot_layout(guides = "collect") &
  theme(legend.position = 'top')

p2 <- map(termo[3:4], plot_freq_termos) %>% 
  reduce(~ `+`(.x, .y)) &
  theme(legend.position = 'none')

p1 / p2 + 
  plot_annotation(title = 'Frequência relativa dos termos - por capital') & 
  theme(plot.title = element_text(hjust = .5, vjust = .5, size = 18, face = "bold"))
```

<img src="visualizacoes_files/figure-gfm/unnamed-chunk-11-1.png" style="display: block; margin: auto;" />

``` r
p3 <- map(termo[5:6], plot_freq_termos) %>%
  reduce(~ `+`(.x, .y)) +
  plot_layout(guides = "collect") &
  theme(legend.position = 'top')

p4 <- map(termo[7], plot_freq_termos) %>%
  reduce(~ `+`(.x, .y)) &
  theme(legend.position = 'none')

(p3 + p4) + 
  plot_layout(nrow = 2) +
  plot_annotation(title = 'Frequência relativa dos termos - por capital') & 
  theme(plot.title = element_text(hjust = .5, vjust = .5, size = 18, face = "bold"))
```

<img src="visualizacoes_files/figure-gfm/unnamed-chunk-12-1.png" style="display: block; margin: auto;" />

## NMDS

![](nmds.png)
