# Propostas - eleições municipais 2020

* **Coletor de propostas**:
  + Download do dado bruto (`.csv`) [clique aqui](https://drive.google.com/file/d/1EvqYC_yNo0FYg9J0x1jE4nxPH3p579dN/view?usp=sharing)
  + Repositório com método de coleta das propostas eleitorais [clique aqui](https://github.com/Transparencia-Brasil/coletor_propostas)
  
  </br>
  
* **Coletor de resutados eleitorais**: código para acessar [API do TSE](https://resultados.tse.jus.br/) extrair o `JSON` e converter para uma `tibble` estruturada
  + Resultados 1º turno [clique aqui](https://github.com/Transparencia-Brasil/propostas-eleicoes-2020/blob/main/code/resultados.R)
  + Resultados 2º turno [clique aqui](https://github.com/Transparencia-Brasil/propostas-eleicoes-2020/blob/main/code/resultados_2o_turno.R)

  </br>

* **report**: 
  + Todos os candidatos (1º e 2º turno) [clique aqui](https://github.com/Transparencia-Brasil/propostas-eleicoes-2020/blob/main/report/visualizacoes.md)
  + Prefeitos eleitos (1º e 2º turno) [clique aqui](https://github.com/Transparencia-Brasil/propostas-eleicoes-2020/blob/main/report/visualizacoes_prefs_eleitos.md)
  
  </br>
  
* **load**:
  + **filtro candidaturas válidas**: recebe `propostass.csv` e retorna `propostas0` [clique aqui](https://github.com/Transparencia-Brasil/propostas-eleicoes-2020/blob/main/code/propostas0_candidaturas_validas.R)
  + **limpeza de textos de propostas:** recebe `propostas0` e retorna `propostas1` [clique aqui](https://github.com/Transparencia-Brasil/propostas-eleicoes-2020/blob/main/code/propostas1_processa_texto.R) 
  + **contagem de termos:** recebe `propostas1`, aplica função `buscador()` e retorna `proposta2` [clique aqui](https://github.com/Transparencia-Brasil/propostas-eleicoes-2020/blob/main/code/propostas2_aplica_buscador.R)
  + **análise NMDS**: [clique aqui](https://github.com/Transparencia-Brasil/propostas-eleicoes-2020/blob/main/code/nmds.R)
