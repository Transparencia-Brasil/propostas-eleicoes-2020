# Propostas - eleições municipais 2020

* **extractor**: [clique aqui para download do dado bruto](https://drive.google.com/file/d/1EvqYC_yNo0FYg9J0x1jE4nxPH3p579dN/view?usp=sharing)
* **report** com visualizações: [clique aqui](https://github.com/Transparencia-Brasil/propostas-eleicoes-2020/blob/main/report/visualizacoes.md)
* **load**:
  + **filtro candidaturas válidas**: recebe `propostass.csv` e retorna `propostas0` [clique aqui](https://github.com/Transparencia-Brasil/propostas-eleicoes-2020/blob/main/code/propostas0_candidaturas_validas.R)
  + **limpeza de textos de propostas:** recebe `propostas0` e retorna `propostas1` [clique aqui](https://github.com/Transparencia-Brasil/propostas-eleicoes-2020/blob/main/code/propostas1_processa_texto.R) 
  + **contagem de termos:** Recebe `propostas1`, aplica função `buscador()` e retorna `proposta2` [clique aqui](https://github.com/Transparencia-Brasil/propostas-eleicoes-2020/blob/main/code/propostas2_aplica_buscador.R)
 * **análise NMDS**: [clique aqui](https://github.com/Transparencia-Brasil/propostas-eleicoes-2020/blob/main/code/nmds.R)
