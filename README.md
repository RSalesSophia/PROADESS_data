### O papel das características municipais na detecção da Sífilis Congênita no Brasil entre 2008 a 2018

Esse repositório possui dois scripts em R, sendo um referente ao tratamento de dados e outro relativo as estatísticas descritivas e a modelagem dos dados. 

Utilizou-se dados fornecidos pelo Projeto de Avaliação do Desempenho do Sistema de Saúde (PROADESS), CENSO de 2000 e 2010 do IBGE e TabNET do DataSUS. 

#### 1. Problema
<hr class="style1">

Analisar a incidência de sífilis congênita nos municípios brasileiros e sua relação com indicadores socioeconômicos

#### 2. Solução estratégica
<hr class="style1">

Utilizou-se um modelo de *Poisson* de dados em painel com efeitos fixos, em que a variável dependente corresponde a incidência de casos de sífilis congênita e as variáveis dependentes correspondem a renda per capita domiciliar, IDHM, taxa de analfabetismo, despesas municipais com saúde per capita, cobertura da Estratégia da Saúde da Família e percentual de mães com seis ou mais consultas de pré-natal estratificadas a partir de da mediana dos dados. 

#### 3. Resultados
<hr class="style1">

Observou-se que os municípios com maior IDHM e cobertura da estratégia da saúde da família apresentaram maiores taxas de incidência de sífilis congênita. Por outro lado, municípios com maiores despesas com saúde e maior percentual de 6 consultas no pré-natal têm maior sucesso na redução da taxa de incidência.
