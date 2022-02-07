#Pacotes
library(rvest)
library(readr)
library(tibble)
library(dplyr)
library(stringr)
library(magrittr)
library(data.table)
library(purrr)
library(tidyr)
library(readxl)
library(deflateBR)

#Configuraçao
options(scipen = 100)

#Extraçao de dados do site PROADESS.
#Esse site fornece diversos dados relativos a dados da saúde no Brasil.

#Primeiramente foi extraído dados municipais por estado relativos ao número 
#de casos novos de sífilis congênita em menores de 1 ano. 

#Por padrao os dados sao entendidos como xls, apesar de serem html. 

#Assim, faz-se a listagem dos arquivos com formato xls no diretório:
arquivos = list.files(pattern="*.xls")

#Criando uma lista com todos arquivos xls e lendo em html. 
#Por fim bind_rows serve para combinar por linhas. 
#Criando uma funçao para ler varios arquivos que apresentam esse mesmo erro:

leitura = function(arquivos){
  lapply(arquivos, function(x) {
    try(read_html(x)) %>% html_table(fill = TRUE)
  }) %>%
    bind_rows()
}

#Correndo a funçao criada anteriormente
leitura_arquivos = leitura(arquivos)

#Tratando a base de dados:
#Removendo as linhas com contem a palavra 'Abrangencia'
leitura_arquivos %<>%
  filter(!str_detect(X1, "Abrangência"))

#Para poder identificar qual estado cada cidade pertence de acordo com o
#arquivo de origem, necessita-se de alguns procedimentos.

#Primeiramente, numera-se as linhas da base de dados de forma a descobrir a 
#indexaçao de cada uma delas. 
leitura_arquivos %<>%
  mutate(numeracao = 1:nrow(leitura_arquivos))

#Cria-se uma nova base de dados apenas com as linhas que contem a palavra
#'Número'.
id_estado = leitura_arquivos %>%
  filter(str_detect(X1, "Número"))

#De acordo com a base anterior, cria-se um intervalo de linhas que corresponde
#ao estado dos municípios.
#Dado que o mesmo procedimento ira ser feito outras vezes e sempre
#sera a combinaçao do mesmo intervalo, cria-se uma funcao para isso
intervalo = function(x){x %<>%
  mutate(Estado = case_when(numeracao %in% 1:103 ~ "AL",
                            numeracao %in% 104:245 ~ "MT",
                            numeracao %in% 246:325 ~ "MS",
                            numeracao %in% 326:1179 ~ "MG",
                            numeracao %in% 1180:1324 ~ "PA",
                            numeracao %in% 1325:1548 ~ "PB",
                            numeracao %in% 1549:1949 ~ "PR",
                            numeracao %in% 1949:2134 ~ "PE",
                            numeracao %in% 2135:2359 ~ "PI",
                            numeracao %in% 2360:2452 ~ "RJ",
                            numeracao %in% 2453:2469 ~ "AP",
                            numeracao %in% 2470:2637 ~ "RN",
                            numeracao %in% 2638:3135 ~ "RS",
                            numeracao %in% 3136:3188 ~ "RO",
                            numeracao %in% 3189:3204 ~ "RR",
                            numeracao %in% 3205:3500 ~ "SC",
                            numeracao %in% 3501:4146 ~ "SP",
                            numeracao %in% 4147:4222 ~ "SE",
                            numeracao %in% 4223:4362 ~ "TO",
                            numeracao %in% 4363:4425 ~ "AM",
                            numeracao %in% 4426:4843 ~ "BA",
                            numeracao %in% 4844:5028 ~ "CE",
                            numeracao %in% 5029:5030 ~ "DF",
                            numeracao %in% 5031:5109 ~ "ES",
                            numeracao %in% 5110:5356 ~ "GO",
                            numeracao %in% 5357:5574 ~ "MA",
                            numeracao %in% 5575:5597 ~ "AC"))}

df = intervalo(leitura_arquivos)

#Removendo as linhas que contem a palavra 'Número' da base principal
#e excluinindo a coluna numeracao (não mais necessária)
df %<>%
  filter(!str_detect(X1, "Número")) %>%
  select(-numeracao)

#Renomeando as colunas
names = c("Municipios", "2007", "2008","2009","2010","2011","2012","2013",
          "2014","2015","2016","2017","2018")

df %<>%
  rename_at(1:13, ~ names)

#Transformando a coluna no formato long
df %<>% gather("Ano", "Casos_Sifilis", 2:13)

#Exportando essa base:
write.csv(df,'base_sifilis.csv', row.names =FALSE)

##########################################################################
#Segunda base: relativa aos nascidos vivos

#Todos arquivos que sao csv e estao no diretório
nascidos = list.files(pattern="*.csv")

#Unindo todos os arquivos e criando uma coluna para identificar o ano
#Para cria-se uma função:

id_base = function(x){lapply(x, function(x) {
  out <- fread(x, header = FALSE)
  out$source_file <- x
  return(out)
})}

arq_nascidos = id_base(nascidos)

#Unindo uma lista de arquivos em uma base de dados
df_nascidos = rbindlist(arq_nascidos) %>%
  as_tibble()

#Fazendo algumas mudanças:
#1 - Retirando as linhas que constam o Total do ano e relativo
#ao cabeçalhado do dataframe anterior
#2 - Separando a coluna V1 em codigo do municipio e nome do municipio
#3 - Criando uma coluna para contem apenas a informacao relativo ao ano
#4 - Renomeando algumas colunas
#5 - Excluindo a coluna 'source-file' que nao é mais necessaria

df_nascidos %<>%
  filter((!(str_detect(V1, "Total|Município") | 
                    str_detect(V2, "Nascim_p/resid.mae")))) %>%
  separate(V1, c("key", "value"), " ", extra = "merge") %>%
  mutate(Ano = substr(source_file,1, 4)) %>%
  rename(Codigo_Mun = key,
         Nome_Mun = value,
         Nascidos_Vivos = V2) %>%
  select(-source_file) 


#Exportando a base de nascidos
write.csv(df_nascidos,'base_nascidos.csv', row.names =FALSE)

#----------------------------------------------------------------------#
#Importando as bases relativas a taxa de analfabetismo
#Dado que é apenas dois arquivos, torna-se mais fácil o tratamento
#de dados importando cada arquivo de uma vez

educ_2000 = read.csv("taxa_analfabetismo_2000.csv", sep = ";", dec = ",",
                     row.names = NULL, skip = 3)

#Coluna que identifica o ano 
educ_2000 %<>%
  mutate(Ano = "2000")

educ_2010 = read.csv("taxa_analfabetismo_2010.csv", sep = ";", dec = ",",
                    row.names = NULL, skip = 3)

#Coluna que identifica o ano 
educ_2010 %<>%
  mutate(Ano = "2010")

#Unindo as duas tabelas anteriores
educ = rbind(educ_2000, educ_2010)

#Tratando os dados
educ %<>%
  filter((!(str_detect(Município, "Total|IBGE|Consulte")))) %>%
  separate(Município, c("key", "value"), " ", extra = "merge") %>%
  rename(Codigo_Mun = key,
         Nome_Mun = value)

#Exportando a base de analfabetismo
write.csv(educ,'base_analfabetismo.csv', row.names =FALSE)

#----------------------------------------------------------------------#
#Importando a base de dados relativo ao IDHM dos municipios

arq_idhm = list.files(pattern="*.csv")

#Extraindo os dados de arquivos
idhm = id_base(arq_idhm)

idhm = rbindlist(idhm) %>%
  as_tibble()

#Tratando a base de dados 
idhm %<>%
  filter((!(str_detect(V1, "Índice|Abrangências|PROADESS|URL")))) %>%
  mutate(UF = substr(source_file,1,2)) %>%
  rename(Municipio = V1,
         "2000" = V3,
         "2010" = V4) %>%
  select(-source_file,-V2) %>%
  gather("Ano", "IDHM", 2:3)


#Substituindo . por virgula 
idhm %<>%
  mutate(IDHM = as.numeric((gsub(",", ".", IDHM)))) %>%
  drop_na()

#Exportando a base de idhm
write.csv(idhm,'base_idhm2.csv', row.names =FALSE)
#----------------------------------------------------------------------#
#Importando a base de dados relativo a renda dos municipios
#Novamente dado o fato que só sao duas bases é mais fácil importar cada uma

rend_2000 = read.csv("renda_2000.csv", sep = ";", row.names = NULL,
                     skip = 3, dec = ",")

#Coluna que identifica o ano 
rend_2000 %<>%
  mutate(Ano = "2000")

rend_2010 = read.csv("renda_2010.csv", sep = ";", row.names = NULL,
                     skip = 3, dec = ",")

#Coluna que identifica o ano 
rend_2010 %<>%
  mutate(Ano = "2010")

#Unindo as bases anteriores
rend = rbind(rend_2000, rend_2010)

#Tratando a base de dados: 
rend %<>%
  filter(!Município == "Total") %>%
  separate(Município, c("key", "value"), " ", extra = "merge") %>%
  rename(Codigo_Mun = key,
         Nome_Mun = value,
         Renda_med = Renda_média_domic._per_capita) %>%
  drop_na()
  
#Exportando a base de renda media
write.csv(rend,'base_renda.csv', row.names =FALSE)

#----------------------------------------------------------------------#
#Dados relativos a informaçoes relativas ao gasto com saúde
#Esses dados foram retirados do mesmo site da primeira base trabalhada.
#Isto é, PROADESS. 
#Dessa forma, segue-se o mesmo procedimento feito anteriormente.
  
#Assim, faz-se a listagem dos arquivos com formato xls no diretório:
arquivos = list.files(pattern="*.xls")

#Lendo arquivos que tem consigo o mesmo erro da primeira base
leitura_arquivos = leitura(arquivos)

#Tratando a base de dados:
#Removendo as linhas com contem a palavra 'Abrangência'
leitura_arquivos %<>%
  filter(!str_detect(X1, "Abrangência"))

#Numerando as linhas da base de dados de forma a descobrir a 
#indexaçao de cada uma delas. 
leitura_arquivos %<>%
  mutate(numeracao = 1:nrow(leitura_arquivos))

#Cria-se uma nova base de dados apenas com as linhas que contem a palavra
#'Gasto'.
id_estado = leitura_arquivos %>%
  filter(str_detect(X1, "Gasto"))

#De acordo com a base anterior percebe-se que tem os mesmos intervalos
#da primeira base para definir o estado. Assim, utiliza-se a funcao
#'intervalo' para essa definicao
df_gastos = intervalo(leitura_arquivos)

#Removendo as linhas que contem a palavra 'Número' da base principal
#e excluinindo a coluna numeracao (nao mais necessária)
df_gastos %<>%
  filter(!str_detect(X1, "Gasto")) %>%
  select(-numeracao)

#Renomeando as colunas
names = c("Municipios", "2000", "2001","2002","2003","2004", "2005",
          "2006","2007", "2008","2009","2010","2011","2012","2013",
          "2014","2015","2016","2017","2018", "2019", "2020")

df_gastos %<>%
  rename_at(1:22, ~ names)

#Transformando a coluna no formato long
df_gastos %<>% gather("Ano", "Gasto_saude", 2:22)

#Deflacionado as dados dos Gastos com saúde para o ano de 2018:

df_gastos %<>%
  mutate(Gasto_saude = as.numeric((gsub(",", ".", Gasto_saude))),
         Dia = paste0("31-12-",Ano),
         Dia = as.Date(Dia, "%d-%m-%Y"),
         Gastos_defla = deflate(Gasto_saude, Dia, "01/2018")) %>%
  select(-Dia)

#Exportando essa base:
write.csv(df_gastos,'base_gastos.csv', row.names =FALSE)

#----------------------------------------------------------------------#
#Importando arquivos relativos ao percentual de Percentual de nascidos vivos 
#cujas maes fizeram mais de 6 consultas de pré-natal

arquivos = list.files(pattern="*.csv")

#Unindo os arquivos e criando uma coluna relativa ao estado
consultas = id_base(arquivos)

consultas = rbindlist(consultas) %>%
  as_tibble()

#Removendo as linhas com contem a palavra 'Abrangencia' e 'Percentual
consultas %<>%
  filter(!str_detect(V1, "Abrangência|Percentual|evitar|URL|Indicador"))

#Removendo linhas relativas a regioes, brasil e estados
#Regioes, Estados e do Brasil
Estados = c("Minas Gerais","Acre","Alagoas","Amazonas","Bahia",
            "Ceará","Espírito Santo","Goiás","Maranhão","Mato Grosso",
            "Mato Grosso do Sul","Minas Gerais","Pará","Paraíba", 
            "Paraná","Pernambuco" ,"Piauí" ,"Rio Grande do Norte",
            "Rio Grande do Sul" ,"Rondônia","Roraima","Santa Catarina",
            "Sergipe","Tocantins")


consultas %<>%
  filter(!((V1 == "Nordeste" & V2 == "31,3") |
             (V1 == "Brasil" & V2 == "43,7")|
             (V1 == "Sul" & V2 == "52,4") |
             (V1 == "Centro-Oeste" & V2 == "50,5") |
             (V1 == "Norte" & V2 == "24,8") |
             (V1 == "Sudeste" & V2 == "52,5") |
             (V1 %in% Estados) |
             (V1 == "São Paulo" & V2 == "54,5") |
             (V1 == "Amapá" & V2 == "22,0") |
             (V1 == "Rio de Janeiro" & V2 == "56,6")))

#Renomeando as colunas 
#Transformando a coluna no formato long
#Trocando . por ,:
#Ademais nesse base ** corresponde a NA

names = c("Municipios", "2000", "2001","2002","2003","2004", "2005",
          "2006","2007", "2008","2009","2010","2011","2012","2013",
          "2014","2015","2016","2017","2018", "2019")

consultas %<>%
  rename_at(1:21, ~ names) %>%
  gather("Ano", "Percentual_Consultas", 2:21) %>%
  mutate(Percentual_Consultas = as.numeric((gsub(",", ".",Percentual_Consultas))),
         Estado = substr(source_file,1, 2)) %>%
  select(-source_file) 
  

#Exportando a base de dados:
write.csv(consultas,'base_consultas.csv', row.names =FALSE)

#----------------------------------------------------------------------#
#Importando o percentual da populaçao coberta pela Estratégia Saúde da 
#Família (ESF)

arquivos = list.files(pattern="*.csv")

#Extraindo os dados de arquivos
esf = id_base(arquivos)

#Unindo o arquivo de listas
esf = rbindlist(esf) %>%
  as_tibble()

#Removendo as linhas com contem a palavra 'Abrangencia' e 'Percentual
esf %<>%
  filter(!str_detect(V1, "Abrangência|Percentual|URL|PROADESS"))

#Removendo linhas relativas a regioes, brasil e estados
esf %<>%
  filter(!((V1 == "Nordeste" & V2 == "22,3") |
             (V1 == "Brasil" & V2 == "14,8")|
             (V1 == "Sul" & V2 == "13,4") |
             (V1 == "Centro-Oeste" & V2 == "17,6") |
             (V1 == "Norte" & V2 == "15,0") |
             (V1 == "Sudeste" & V2 == "9,9") |
             (V1 %in% Estados) |
             (V1 == "São Paulo" & V2 == "5,3") |
             (V1 == "Amapá" & V2 == "2,8") |
             (V1 == "Rio de Janeiro" & V2 == "8,0")))

#Renomeando as colunas 
#Transformando a coluna no formato long
#Trocando . por ,:

names = c("Municipios", "2000", "2001","2002","2003","2004", "2005",
          "2006","2007", "2008","2009","2010","2011","2012","2013",
          "2014","2015","2016","2017","2018", "2019")

esf %<>%
  rename_at(1:21, ~ names) %>%
  gather("Ano", "Coberta_esf", 2:21) %>%
  mutate(Coberta_esf = as.numeric((gsub(",", ".", Coberta_esf))),
         Estado = substr(source_file,1, 2)) %>%
  select(-source_file)

#Exportando a base de dados:
write.csv(esf,'base_cobertura.csv', row.names =FALSE)
#----------------------------------------------------------------------#
#Unir todas as bases de dados 
#1 - Unir casos de sifilis e nascidos

sif = read.csv("base_sifilis.csv") %>%
  as_tibble()

#Códigos dos estados de acordo com o IBGE
codigos = read_excel("Tabela Estados IBGE.xlsx")

codigos %<>% select(-Estado) %>%
  rename(Codigo_UF = `Código da UF`)

#Unindo essa informaçao com a tabela que contem as informacoes
#sobre a quantidade de casos de sifilis. Para isso, utiliza-se
#da funçao de inner_join:

uniao = inner_join(codigos, sif, by = c("UF" = "Estado")) 

#Criando uma chave para a base uniao e selecionando apenas variaveis
#importantes:
uniao %<>%
  mutate(Chave = as.character(paste0(Codigo_UF, Municipios, Ano))) %>%
  select(Chave, Ano, Municipios, Casos_Sifilis, Codigo_UF)

#Fazendo a uniao com a tabela relativos ao n de nascimentos:
#Antes criando uma coluna que contem apenas informacoes sobre o cod_uf
#E criando uma chave

#OBS: dado que a base anterior tem no max 25 characters na columa
#nome dos municipios, cria-se uma coluna em nascidos considerando
#no maximo até 25 characters

nascidos = read.csv("base_nascidos.csv") 

nascidos %<>%
  mutate(Codigo_UF = str_sub(Codigo_Mun, end = 2),
         Nomes_Reduzidos = substr(Nome_Mun, start = 1, stop = 25),
         Chave = paste0(Codigo_UF, Nomes_Reduzidos, Ano)) %>%
  select(Nascidos_Vivos, Chave, Codigo_Mun)

uniao = inner_join(uniao, nascidos, by = "Chave")

#Criando a variável taxa de indicidencia:
uniao %<>%
  mutate(Tx_incidencia = round(((Casos_Sifilis*1000)/(Nascidos_Vivos))))

#----------------------------------------------------------------------#
#2 - Unir uniao com taxa de analfabetismo

educ = read.csv("base_analfabetismo.csv")

#A informaçao relativa ao ano corresponde ao ano cujo qual foi
#realizado o CENSO. 

#Assim na base uniao é necessario criar uma dummy que indica se o Ano
#é anterior ou nao a 2010:
uniao %<>% 
  mutate(D_Ano = if_else(Ano < 2010, "2000","2010"),
         Chave = paste0(Codigo_Mun, D_Ano))

#Criando uma chave em educ:
educ %<>%
  mutate(Chave = paste0(Codigo_Mun, Ano)) %>%
  select(Chave, Taxa_de_analfabetismo)

#Fazendo o merge deixando sempre as informacoes da base uniao
uniao = left_join(uniao, educ, by = "Chave")

#----------------------------------------------------------------------#
#3 - Unir uniao com renda per capta:
renda = read.csv("base_renda.csv")

renda %<>%
  mutate(Chave = paste0(Codigo_Mun, Ano)) %>%
  select(Chave, Renda_med)

uniao = left_join(uniao, renda, by = "Chave")

#----------------------------------------------------------------------#
#4 - Unir uniao com gastos com saúde:

gastos = read.csv("base_gastos.csv")

#Criando uma chave identificadora:
gastos = inner_join(codigos, gastos, by = c("UF" = "Estado"))

gastos %<>%
  filter(Ano %in% 2007:2018) %>%
  mutate(Chave = paste0(Codigo_UF, Municipios, Ano)) %>%
  select(Chave, Gastos_defla)

#Codificando a estrutura de chave em uniao:
uniao %<>%
  mutate(Chave = paste0(Codigo_UF, Municipios, Ano))

#Merge para o lado esquerdo que preserva as informacoes anteriores de
#uniao
uniao = left_join(uniao, gastos, by = "Chave")

#----------------------------------------------------------------------#
#5 - Unir uniao com gastos com saúde:

consultas = read.csv("base_consultas.csv")

consultas = inner_join(codigos, consultas, by = c("UF" = "Estado"))

consultas %<>%
  filter(Ano %in% 2007:2018) %>%
  mutate(Chave = paste0(Codigo_UF, Municipios, Ano)) %>%
  select(Chave, Percentual_Consultas)


uniao = left_join(uniao, consultas, by = "Chave")

#----------------------------------------------------------------------#
#6 - Unir uniao com taxa de cobertura
cobertura = read.csv("base_cobertura.csv")

cobertura = inner_join(codigos, cobertura, by = c("UF" = "Estado"))

#Filtrando os dados a partir de 2007:
cobertura %<>%
  filter(Ano %in% 2007:2018) %>%
  mutate(Chave = paste0(Codigo_UF, Municipios, Ano)) %>%
  select(Chave, Coberta_esf)

#Fazendo merge:
uniao = left_join(uniao,cobertura, by = "Chave")

#----------------------------------------------------------------------#
# 8 - Unindo a variavel IDHM
idhm = read.csv("base_idhm.csv")

idhm = inner_join(codigos, idhm, by = "UF")

idhm %<>%
  mutate(Chave = paste0(Codigo_UF, Municipio, Ano)) %>%
  select(Chave, IDHM)

#Codificando outra Chave em uniao
uniao %<>%
  mutate(Chave = paste0(Codigo_UF, Municipios, D_Ano))

#Unindo com a base uniao:
uniao = left_join(uniao,idhm, by = "Chave")

#----------------------------------------------------------------------#
#Criando a variável Regiao:
uniao %<>% mutate (REGIAO = case_when(Codigo_UF %in% 11:17 ~ "1",
                                      Codigo_UF %in% 21:29 ~ "2",
                                      Codigo_UF %in% 31:35 ~ "3",
                                      Codigo_UF %in% 41:43 ~ "4",
                                      Codigo_UF %in% 50:53 ~ "5"))
#----------------------------------------------------------------------#
#Foi observardo que os dados da PROADESS de 2018 apresentarem
#inconsistências, portanto foi necessário substitui-los pelos dados
#fornecidos diretamente pelo SINAN

#Importando o arquivo de incidência de sífilis em 2018 do SINAN
br_2018 = read.csv("2018.csv", sep = ";",row.names = NULL, skip = 3) %>%
  as_tibble()

br_2018 %<>%
  mutate(Codigo_Mun = as.numeric(substr(Município.de.residência,1, 6))) %>%
  select(-Município.de.residência)

#Filtrando na base uf os dados apenas de 2018. Isso é necessário pois a base
#do SINAN não fornece os municipios com nenhum caso. Dessa forma, precisa-se
#descobrir quais municipios não estão presentes na base do SINAN e inputar
#o valor de 0 casos. 
uniao_2018 = uniao %>%
  filter(Ano == 2018) 

merge = left_join(uniao_2018, br_2018)

merge[is.na(merge)] <- 0

merge %<>%
  select(-Casos_Sifilis) %>%
  rename(Casos_Sifilis = Casos_confirmados)

#Unindo a base certa de 2018 com uniao
uniao %<>%
  filter(Ano != 2018) 

uniao = rbind(uniao, merge)

#---------------------------------------------------------------------------#
#Filtrando a base uniao considerando dados a partir de 2009:
#Dado que muitas variaveis iniciam a partir desse ano. 
uniao %<>%
  filter(Ano > 2008) 
#---------------------------------------------------------------------------#
#Exportando a base uniao
write.csv(uniao,'base_uniao.csv', row.names =FALSE)
