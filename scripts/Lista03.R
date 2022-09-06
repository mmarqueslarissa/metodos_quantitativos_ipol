### Lista 03 - Métodos quantitativos 
## Larissa Martins Marques


#1. Indicar um conjunto de dados à sua escolha e ler este conjunto de dados no R

## Abrir bases e carregar pacotes ----
#install.packages("pacman")

pacman::p_load(tidyverse, ggthemes, plotly)

# definir diretorio de trabalho

setwd()

#carregar base

load("lapop_abcp.RData")

# Definir tema do grafico e seus ajustes

tema <- theme_fivethirtyeight() +
theme(title = element_text(size = 10),
      axis.title.y = element_text(size = 8),
      axis.text.y = element_text(size = 8),
      axis.text.x = element_text(size = 8),
      legend.text = element_text(size = 8),
      plot.caption = element_text(hjust = 0,size = 8),
      panel.background = element_rect(fill = "white", colour = "white", color = "white"),
      plot.background = element_rect(fill = "white", colour = "white", color = "white"),
      legend.background=element_rect(fill="white"),
      legend.position = "bottom",
      legend.key = element_rect(fill = "white", color = "white"),
      strip.background=element_rect(fill="white", colour="white"))

ggplot2::theme_set(tema)

#2. Realizar alguma manipulação em dplyr/tidyr neste conjunto de dados que envolva os conteúdos tratados em aula.

long_interesse <- lapop_abcp %>%
  filter(interesse <= 4) %>% # filtra apenas as possibilidades de resposta de 1 até 4
  group_by(Mulher, Ano, interesse) %>% 
  summarise(n_interesse = n()) %>% 
  group_by(Mulher, Ano) %>% 
  mutate(n = sum(n_interesse, na.rm = T), # constroi intervalos de confianca
         p = n_interesse/n, 
         erro = sqrt((p *(1 - p)/n)),
         perc = p *100,
         p_erro = erro * 100)


banco_interesse <- long_interesse %>% 
  mutate(Mulher = case_when(Mulher == 0 ~ "Homem", 
                            Mulher == 1 ~ "Mulher"), 
         interesse = case_when(interesse == 1 ~ "Muito", 
                               interesse == 2 ~ "Algo",
                               interesse == 3 ~ "Pouco",
                               interesse == 4 ~ "Nada"),
         extremo = if_else(interesse %in% c("Muito", "Nada"), 1, 0))


#3. Apresentar uma saída gráfica em ggplot (dito simples, fazer um gráfico) contendo ao menos uma escala de tamanho, cor, preenchimento etc. e utilizando algum tema (qualquer tema, inclusive os nativos do ggplot). A escolha do tipo de gráfico (geometria) também é livre. Comente em seu código, usando #, a razão da escolha deste gráfico.

grafico <- banco_interesse %>% 
ggplot(aes(x = as.numeric(Ano),
           y = perc, col = as_factor(interesse), 
           fill = Mulher, 
           linetype = Mulher))+
  geom_line(stat = "identity", size = 1)+
  geom_errorbar(aes(ymin = perc - p_erro, ymax = perc + p_erro), 
                position =  position_dodge(0.05), size = 1, width = 1.5)+
  scale_x_continuous(breaks = c(2006, 2008, 2010, 2012, 2014, 2016, 2018))+
  labs(title = "Interesse por política e sexo",
       fill = "", 
       col = "", 
       y = "", 
       linetype = "", 
       caption = "Elaborado pelos autores com base no LAPOP 2006-2018")+
  scale_color_manual(values = c("#0640bc", "#bd99cc",  "#8caef9" ,"#734488" ))+
  scale_y_continuous(labels=function(x) paste0(x,"%"))
  

m = list(
  l = 100,
  r = 40,
  b = 100,
  t = 50,
  pad = 0, x=-1, y= -2, 
  xanchor='left',
  yanchor='bottom',  
  orientation='h')

ggplotly(grafico) %>% 
  layout(autosize = F,
         width = 800,
         height = 600, 
         margin = m)


# Optou-se por um grafico de linha por se tratar de uma temporalidade

# Interessante analisar o gráfico apenas com as categorias extremas como
# "muito interesse" ou "nenhum interesse". Por este motivo optou-se por um grafico 
# interativo, onde as categrias podem ser retiradas para facilitar a visualizacao















