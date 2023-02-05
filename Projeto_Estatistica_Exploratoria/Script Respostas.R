#libraries
library('ggplot2')
library('readxl')
library('janitor')
library('tidyverse')
library(dplyr)
library(ggforce)

# setando a pasta de trabalho.
setwd("C:/Projeto_Estatistica_Exploratoria")

# importando e transformando a planilha em tabela
planilhaDados <- read_excel("sp_beaches.xlsx")

tabelaPraias <- as_tibble(planilhaDados)


# Questão 1: criando o sumário.
sumario <- tabelaPraias %>%
  filter(City == "SÃO SEBASTIÃO") %>%
  mutate(Enterococcus = as.numeric(Enterococcus)) %>%
  group_by(Beach) %>%
  summarise(
    media = mean(Enterococcus),
    desvio = sd(Enterococcus),
    Q1 = quantile(Enterococcus, 0.25),
    mediana = median(Enterococcus),
    Q3 = quantile(Enterococcus, 0.75),
    minimo = min(Enterococcus),
    maximo = max(Enterococcus)
  )


# Questão 2: criando um gráfico de barras com a % média de enterococcus por praia

ggplot(sumario, aes(x = reorder(Beach, -media), y = media, fill = Beach)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(media, 2))),
            position = position_dodge(width = 0.9), vjust = -0.5, size = 3.5) +
  ggtitle("Média de enterococcus por praia em São Sebastião") +
  xlab("Praia - São Sebastião") + ylab("Média de enterococcus") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), legend.position = "none")


# Questão 3: criando um gráfico em formato de pizza
graficoPizza <- sumario %>% 
  mutate(fim = 2 * pi * cumsum(media)/sum(media),
         comeco = lag(fim, default = 0),
         centro = 0.5 * (comeco + fim),
         hjust = ifelse(centro > pi, 1, 0),
         vjust = ifelse(centro < pi/2 | centro > 3 * pi/2, 0, 1))

ggplot(graficoPizza) + 
  geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 0, r = 1,
                   start = comeco, end = fim, fill = Beach)) +
  geom_text(aes(x = 1.05 * sin(centro), y = 1.05 * cos(centro), 
                label = paste(Beach, round(media, 2), sep = " - "),
                hjust = hjust, vjust = vjust),size = 2.5) +
  coord_fixed() +
  scale_x_continuous(limits = c(-1.5, 1.5),
                     name = "", breaks = NULL, labels = NULL) +
  scale_y_continuous(limits = c(-1.1, 1.1),
                     name = "", breaks = NULL, labels = NULL) +
  ggtitle("Porcentagem média de enterococcus por praia em São Sebastião") +
  theme(legend.position = "none")

########OBSERVAÇÃO########

# Professor, abaixo está o código inicial que eu fiz. Ele funciona, mas as
# legendas não estavam aparecendo como eu queria.O codigo acima é um snippet
# modificado para realmente mostrar os labels bonitinhos.
#
#
# ggplot(sumario, aes(x = "", y = media, fill = reorder(Beach, media))) +
#   geom_bar(width = 1, stat = "identity", color = "black") +
#   geom_text(aes(label = paste(Beach, round(media, 2), sep = " - ")),
#             position = position_stack(vjust = 0), color = "black")+
#   coord_polar("y", start = 0) +
# 
#   xlab("") + ylab("Enterococos (média)") +
#   labs(fill = "Praias")
#   theme(legend.position = "none")
  
  
# Questão 4:

ggplot(sumario, aes(x = media)) +
  geom_histogram(fill = "blue", color = "black", bins = 10) +
  scale_x_continuous(breaks = seq(0, max(sumario$media), 2)) +
  scale_y_continuous(breaks = seq(0, max(sumario$media), 1)) +
  ggtitle("Distribuição dos enterococcus nas Praias") +
  xlab("Médias de Enterococcus") +
  ylab("Frequência de praias")
  

# Questão 5:
boxPlotPraias <- tabelaPraias %>%
  filter(City == "SÃO SEBASTIÃO") %>%
  mutate(Enterococcus = as.numeric(Enterococcus)) %>%
  group_by(Beach)

ggplot(boxPlotPraias, aes(y = Beach, x = boxPlotPraias$Enterococcus)) +
  geom_boxplot() +
  scale_x_continuous(breaks = seq(0, max(boxPlotPraias$Enterococcus), 50)) +
  ggtitle("Quantidade de enterococcus nas Praias") +
  xlab("Enterococcus") +
  ylab("Praias") 
