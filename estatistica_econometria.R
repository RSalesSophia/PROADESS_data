#Pacotes
library(ggrepel)
library(ggplot2)
library(dplyr)
library(tidyr)
library(magrittr)
library(pglm)


#Configuraçoes 
windowsFonts(Times=windowsFont("TT Times New Roman"))
options(OutDec= ",")

#Importando o arquivo uniao:
uniao = read.csv("base_uniao.csv")

#Criando um grafico de linhas que mostra as informaçoes relativas
#a variaçao da taxa de incidiencia ao longo dos anos no Brasil e nas
#regioes

#Para isso, primeiro cria-se uma tabela calculando a taxa de incidencia
#por regiao e para o brasil

#Especificamente para as regioes
df = uniao %>%
  group_by(Ano, REGIAO) %>%
  summarise(Soma_Nascidos = sum(Nascidos_Vivos, na.rm = T),
            Soma_Incidencia = sum(Casos_Sifilis, na.rm = T),
            Total_Incidencia = round(((Soma_Incidencia/Soma_Nascidos)*1000),1)) %>%
  ungroup()
  
#Especificamente para o Brasil 
brasil = df %>%
  group_by(Ano) %>%
  summarise(Total_Incidencia = round((sum((Soma_Incidencia)/sum(Soma_Nascidos))*1000), 1)) %>%
  mutate(Soma_Nascidos = NA,
         Soma_Incidencia = NA,
         REGIAO = "Brasil") 

#Unindo os dois dados
df = rbind(df, brasil)

#Codificando os numeros das regioes para o nome das mesmas:
df %<>%
  mutate(REGIAO = case_when(REGIAO == 1 ~ "Norte",
                            REGIAO == 2 ~ "Nordeste",
                            REGIAO == 3 ~ "Sudeste",
                            REGIAO == 4 ~ "Sul",
                            REGIAO == 5 ~ "Centro-Oeste",
                            TRUE ~ as.character(REGIAO)),
         REGIAO = as.factor(REGIAO))


#Tipo de linhas: a intençao aqui é deixar a linha do Brasil diferentes
#das outras regioes

line_types <- c("Brasil" = 2, "Centro-Oeste"  = 1, "Sudeste" = 1,
                "Sul" = 1, "Nordeste" = 1, "Norte" = 1)

#Especificando as cores de cada linha
colors <- c("Brasil" = "black", "Centro-Oeste" = "#00AFBB", "Sudeste" = "#ED553B",
            "Sul" = "#52854C", "Nordeste" =  "#173F5F", "Norte" = "#F6D55C")


#Gráfico de linhas:
gf = ggplot(df, aes(x = Ano, y = Total_Incidencia, colour = REGIAO,
               linetype = REGIAO)) + 
  geom_line(aes(colour = REGIAO, group = REGIAO)) +
  geom_point(size = 1) + 
  scale_color_manual(values = colors, name = "") +
  scale_linetype_manual(values = line_types,  name = "") + 
  theme_classic() +
  labs(colour = "Regioes", y = "Taxa de Incidencia") +
  theme(text=element_text(size=24,  family="serif")) +
  theme(legend.title=element_blank())

#Para visualizar os números das taxas de incidencia de 2009 e 2018,
#faz necessario a criacao de tabelas que contem apenas essas informacoes
inf_ultima = df %>%
  filter(Ano == 2018)

#Na informacao relacao relativa a brasil de 2018, nao aparece de forma
#regular apenas um dígito, para solucionar isso:

inf_ultima %<>%
  mutate(Total = formatC(inf_ultima$Total_Incidencia, format="f", digits=1))


#Para que centro-oeste e sudeste iniciam com a mesma taxa de incidencia,
#elimina-se uma dessas informacoes
inf_primeira = df %>%
  filter(Ano == 2009 & REGIAO != "Sudeste") 

#Acrescentando as informacoes anteriores sobre taxa de incidencia em 2009
#e 2018 e exportando o grafico 

png("Graf_1.png", width = 16, height = 8, units = 'in', res = 300)

gf + 
  geom_text_repel(aes(label = Total_Incidencia), data = inf_primeira, 
                  hjust = "left", nudge_x = -0.35,  size = 5.2, 
                  color = "black", segment.color = NA) +
  geom_text_repel(aes(label = Total), data = inf_ultima, 
                  vjust = "right", size = 5.2, 
                  color = "black",  segment.color = NA) 
  
dev.off()   
#---------------------------------------------------------------------------#
#Criando variaveis para definir grupos tendo como parâmetro o valor mediano
#da variável analisado tendo em vista cada ano
uniao %<>%
  group_by(Ano) %>%
  mutate(GASTOS_med = if_else(Gastos_defla > median(Gastos_defla, na.rm =  T), 1,0),
         ANALF_med = if_else(Taxa_de_analfabetismo > median(Taxa_de_analfabetismo, 
                                                            na.rm = T), 1,0),
         ESF_med = if_else(Coberta_esf < median(Coberta_esf, na.rm = T),1,0),
         PRE_med = if_else(Percentual_Consultas >= median(Percentual_Consultas, 
                                                          na.rm = T),1,0),
         IDHM_med = if_else(IDHM >= median(IDHM, na.rm = T), 1,0),
         REND_mediana = if_else(Renda_med >= median(Renda_med, na.rm = T), 1,0))


#Estatística descritiva relativo a mediana das variáveis:
uniao %>% 
  group_by(Ano) %>%
  summarise(Gastos_med = across(Gastos_defla, median, na.rm = TRUE),
            Analf_med = across(Taxa_de_analfabetismo, median, na.rm = TRUE),
            Cobertura_med = across(Coberta_esf, median, na.rm = TRUE),
            Pre_med = across(Percentual_Consultas, median, na.rm = TRUE),
            Renda_med = across(Renda_med, median, na.rm = TRUE))



#Taxa de incidencia a partir de determinadas características:
#Exemplo: PRE_med
uniao %>%
  group_by(Ano, PRE_med) %>%
  summarise(Total_Incidencia = (sum(Casos_Sifilis, na.rm = T)/(sum(Nascidos_Vivos, na.rm = T)))*1000) %>%
  drop_na()
#----------------------------------------------------------------------------#
#Modelo econometrico adequado a contagem de incidencia de sífilis
#Modelo de Poisson tendo em vamos trabalhar com dados em painel:

data_painel = pdata.frame(uniao, index = c("Codigo_Mun", "Ano"), 
                   drop.index = FALSE)

#Com efeitos fixos
#Modelo completo

reg_poison = pglm(Tx_incidencia ~ REND_mediana + IDHM_med + ANALF_med + 
                    GASTOS_med + ESF_med + PRE_med +Ano, 
                  family = "poisson", model = "within",
                  method = "nr",data = data_painel)


#Resumo do modelo econometrico: 
summary(reg_poison)

#Razao de chances das estimativas:
(est <- cbind(Estimativa = coef(reg_poison), confint(reg_poison)))
exp(est)

#---------------------------------------------------------------------------#
#Comparando a qualidade do modelo

#Modelo reduzido
reg_red = pglm(Tx_incidencia ~ REND_mediana, 
               family = "poisson", model = "within",
               method = "nr",data = data_painel)

#Comparando o ajuste a partir do Akaike Information Criterion (AIC):
AIC(reg_poison)

AIC(reg_red)

#Dado o valor do AIC tem-se que o modelo completo apresenta um melhor
#melhor ajuste para os dados em comparaçao ao modelo reduzido

