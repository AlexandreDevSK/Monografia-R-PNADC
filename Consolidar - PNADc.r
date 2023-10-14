#__________________________________________________________________________________________________

# Processamento dos microdados da PNAD Contínua - IBGE 

# Autor: Alexandre Barros
# Email: alexandrepitstop@gmail.com

# --/12/2022
#__________________________________________________________________________________________________

{ # PRÉ-CONFIG
  
#install.packages('ggthemes')
#install.packages("extrafont")

library(tidyverse)
library(magrittr)
library(scales, gdata)
library(ggthemes)
library(ggrepel)
library(writexl)
library(extrafont)
loadfonts(device = "win")

knitr::opts_chunk$set(echo = TRUE)
setwd('C:/Users/Alexandre/OneDrive/Documentos/Monografia/Microdados/Processados')



#-- grupos correspondentes
i = 1
}

#-- Funções para o cálculo dos efeitos nível e composição

{ # FUNÇÕES
efc_nivel <- function(a, b, c, d) {
  h <- (b-a)*((d+c)/2)
  return((round(h, 6)*100))
}

efc_comp <- function(a, b, c, d) {
  h <- ((a+b)/2)*(d-c)
  return((round(h, 6)*100))
}
}

# CONSOLIDANDO ARQUIVOS ------------------------------------------------------------------


repeat {
#-- Criando as novas variáveis derivadas 

# Gênero
  assign(
    x = str_glue('dados_sexo_{2015+i}'),
    readRDS(str_glue('TAB_sexo_{2015+i}.rds'))
  ) 
# Faixa Etária
  assign(
    x = str_glue('dados_Faixa_Etária_{2015+i}'),
    readRDS(str_glue('TAB_Faixa Etária_{2015+i}.rds')) 
  )
# Raça
  assign(
    x = str_glue('dados_Raça_{2015+i}'),
    readRDS(str_glue('TAB_Raça_{2015+i}.rds'))
  )
# Zona Habitada
  assign(
    x = str_glue('dados_Zona_Habitada_{2015+i}'),
    readRDS(str_glue('TAB_Zona Habitada_{2015+i}.rds')) 
  )
# Tipo de Região
  assign(
    x = str_glue('dados_Tipo_de_Região_{2015+i}'),
    readRDS(str_glue('TAB_Tipo de Região_{2015+i}.rds')) 
  )
# Setor
  assign(
    x = str_glue('dados_Setor_{2015+i}'), 
    readRDS(str_glue('TAB_Setor_{2015+i}.rds')) 
  )
# Ciclo Escolar
  assign(
    x = str_glue('dados_Ciclo_Escolar_{2015+i}'),
    readRDS(str_glue('TAB_Ciclo Escolar_{2015+i}.rds')) 
  )
# Grupo de Rendimento
  assign(
    x = str_glue('dados_Grupo_de_Rendimento_{2015+i}'),
    readRDS(str_glue('TAB_Grupo de Rendimento_{2015+i}.rds')) 
  )
# Experiência
  assign(
    x = str_glue('dados_Experiência_{2015+i}'),
    readRDS(str_glue('TAB_Experiência_{2015+i}.rds')) 
  )
# Faixa de horas trabalho
  assign(
    x = str_glue('dados_Faixa_horas_trabalho_{2015+i}'),
    readRDS(str_glue('TAB_Faixa de horas trabalho_{2015+i}.rds')) 
  )
# Condição
  assign(
    x = str_glue('dados_Condição_{2015+i}'),
    readRDS(str_glue('TAB_Condição_{2015+i}.rds'))  %>% na.omit()
  )
  
i = i+1
 
if (i > 7) { 

# SEXO
df_sexo <- bind_rows(dados_sexo_2016, dados_sexo_2017, dados_sexo_2018, 
                     dados_sexo_2019, dados_sexo_2020, dados_sexo_2021,
                     dados_sexo_2022)  

# Faixa_Etária
df_Faixa_Etária <- bind_rows(dados_Faixa_Etária_2016, dados_Faixa_Etária_2017, dados_Faixa_Etária_2018, 
                             dados_Faixa_Etária_2019, dados_Faixa_Etária_2020, dados_Faixa_Etária_2021,
                             dados_Faixa_Etária_2022)  
# Raça
df_Raça <- bind_rows(dados_Raça_2016, dados_Raça_2017, dados_Raça_2018, 
                     dados_Raça_2019, dados_Raça_2020, dados_Raça_2021,
                     dados_Raça_2022)  
# Zona_Habitada
df_Zona_Habitada <- bind_rows(dados_Zona_Habitada_2016, dados_Zona_Habitada_2017, dados_Zona_Habitada_2018, 
                     dados_Zona_Habitada_2019, dados_Zona_Habitada_2020, dados_Zona_Habitada_2021,
                     dados_Zona_Habitada_2022)  
# Tipo_de_Região
df_Tipo_de_Região <- bind_rows(dados_Tipo_de_Região_2016, dados_Tipo_de_Região_2017, dados_Tipo_de_Região_2018, 
                     dados_Tipo_de_Região_2019, dados_Tipo_de_Região_2020, dados_Tipo_de_Região_2021,
                     dados_Tipo_de_Região_2022)  
# Setor
df_Setor <- bind_rows(dados_Setor_2016, dados_Setor_2017, dados_Setor_2018, 
                      dados_Setor_2019, dados_Setor_2020, dados_Setor_2021,
                      dados_Setor_2022)  
# Ciclo_Escolar
df_Ciclo_Escolar <- bind_rows(dados_Ciclo_Escolar_2016, dados_Ciclo_Escolar_2017, 
                              dados_Ciclo_Escolar_2018, dados_Ciclo_Escolar_2019, 
                              dados_Ciclo_Escolar_2020, dados_Ciclo_Escolar_2021,
                              dados_Ciclo_Escolar_2022)  
# Grupo_de_Rendimento
df_Grupo_de_Rendimento <- bind_rows(
                     dados_Grupo_de_Rendimento_2016, dados_Grupo_de_Rendimento_2017, 
                     dados_Grupo_de_Rendimento_2018, dados_Grupo_de_Rendimento_2019, 
                     dados_Grupo_de_Rendimento_2020, dados_Grupo_de_Rendimento_2021,
                     dados_Grupo_de_Rendimento_2022)  
# Experiência
df_Experiência <- bind_rows(dados_Experiência_2016, dados_Experiência_2017, dados_Experiência_2018, 
                            dados_Experiência_2019, dados_Experiência_2020, dados_Experiência_2021,
                            dados_Experiência_2022)  
# Faixa_horas_trabalho
df_Faixa_horas_trabalho <- bind_rows(
                     dados_Faixa_horas_trabalho_2016, dados_Faixa_horas_trabalho_2017, 
                     dados_Faixa_horas_trabalho_2018, dados_Faixa_horas_trabalho_2019, 
                     dados_Faixa_horas_trabalho_2020, dados_Faixa_horas_trabalho_2021,
                     dados_Faixa_horas_trabalho_2022)  
# Condição
df_Condição <- bind_rows(dados_Condição_2016, dados_Condição_2017, dados_Condição_2018, 
                         dados_Condição_2019, dados_Condição_2020, dados_Condição_2021,
                         dados_Condição_2022)  

i=1

rm(list=ls(pattern="dados_"))

break() 
}
}

# GRÁFICO INICIAL ------------------------------------------------------------------------

{
desocupação = sidrar::get_sidra(api = '/t/6397/n1/all/n2/2/n3/22/v/4099/p/last%2019/c58/95253/d/v4099%201')
desocupação %<>% select(10,7,5) %<>% as_tibble()

names(desocupação) = c('Trimestre','regiao','Taxa')

desocupação %<>% mutate(Ano = substr(Trimestre, 1, 4)) %>%
  group_by(Ano, regiao) %>%
  summarise(Taxa = mean(Taxa))

#c('2021','Brasil', 13.2)
#c('2021','Nordeste', 17.1)
#c('2021','Piauí', 13.6)
desocupação[19:21,1] = rep('2021',3)
desocupação[19:21,2] = c('Brasil','Nordeste','Piauí')
desocupação[19:21,3] = c(13.2,17.1,13.6)


#-- B

informalidade = df_Condição %>% group_by(regiao, Ano) %>% 
  summarise(informais = sum(informais, na.rm = T), ocupados = sum(ocupados, na.rm = T)) %>%
  mutate(Infor = (informais/ocupados)*100) %>% select(1,2,5)

df_inicial = informalidade %>% merge(desocupação, by = c('Ano', 'regiao'))
}

# GRÁFICOS -------------------------------------------------------------------------------

#-- GRÁFICO 00 DESOCUP X INFORM

#-- limpar bases
rm(list=ls(pattern="graf"))

{

names(df_inicial) <- c('Ano','Regiao','Informalidade', 'Desocupação')
  
graf <- df_inicial %>% pivot_longer(cols = c(Informalidade, Desocupação),
                          values_to = 'Taxa',
                          names_to = 'Indicador')

graf$Taxa[graf$Indicador == 'Desocupação'] = graf$Taxa[graf$Indicador == 'Desocupação'] * 3
graf$Taxa <- round(graf$Taxa,1)

graf %<>% mutate(Regiao = case_when(Regiao == 'Brasil' ~ '(a) Brasil',
                                    Regiao == 'Nordeste' ~ '(b) Nordeste',
                                    Regiao == 'Piauí' ~ '(c) Piauí'))

graf0 = 
  graf %>%
  ggplot(aes(x = Ano, color = `Indicador`, group = `Indicador`, y=Taxa), 
         stat = 'identity', position = "dodge") + 
  geom_line(alpha = 0.8, stroke = 0, size = 1) + 
  geom_point(aes(shape=`Indicador`)) +
  scale_y_continuous(
    #Informalidade
    name = "Taxa de informalidade (%)",
    #Desocupação
    sec.axis = sec_axis(~.*.3, name="Taxa de desocupação (%)",breaks = seq(9,20,2)),
    
    breaks = seq(30,60,5)
  ) +
  labs(title = "Tabela: informalidade e desocupação",
       x = 'Período',
       color = "Taxa:") +
  scale_color_excel_new() +
  facet_wrap( ~ Regiao, nrow = 1) +
  theme_bw(16) +
  geom_text_repel(data = graf %>% filter(Ano == 2016), 
                  aes(label = paste(ifelse(Indicador == 'Desocupação', round(Taxa/3,1), Taxa),'%',sep='')), 
                  size=3.4, direction='y', force=.5, color = "black", nudge_x = -.5)+#, hjust = "left") + 
  geom_text_repel(data = graf %>% filter(Ano == 2022), 
                  aes(label = paste(ifelse(Indicador == 'Desocupação', round(Taxa/3,1), Taxa),'%',sep='')),
                  size=3.4, direction='y', force=.5, color = "black", nudge_x = .5)+#, hjust = "right") + 
  scale_x_discrete() +
  theme(axis.title = element_text(size = 12),
        axis.title.y = element_text(size = 10.5),                 
        axis.text.x = element_text(size = 10, color = 'black'),
        axis.text.y = element_text(size = 9.5, face = 'bold', color = 'black'),
        legend.title = element_text(size = 12),
        legend.position = 'bottom',
        legend.text=element_text(size=12),
        legend.background = element_rect(fill="ghostwhite", size=1),
        plot.title = element_blank(),
        axis.ticks = element_line(colour = "grey70", size = 0.2),
        panel.grid.major = element_line(colour = "grey70", size = 0.1),
        panel.grid.minor = element_blank(),
        #panel.grid.major.x = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 12)) + 
  guides(
    colour = guide_legend(""),
    shape = guide_legend(""))
  
graf0
  
  
ggsave(plot = graf0, file = "Imgs/graf_inicial.png", dpi = 200, width = 9.1, height = 4.2)
}


#-- GRÁFICO POR GÊNERO

{
graf <- df_sexo %>%
  mutate(regiao = case_when(regiao == 'Brasil' ~ '(a) Brasil',
                            regiao == 'Nordeste' ~ '(b) Nordeste',
                            regiao == 'Piauí' ~ '(c) Piauí')) %>%
  group_by(Ano, sexo, regiao) %>%
  summarise(Taxa = mean(informais/ocupados, na.rm = T))

#graf <- df_sexo %>% mutate(Ano = as.Date(format(paste(Ano, Trimestre, sep = '/')), '%YYYY/%Q'))

graf2 <- 
graf %>%
ggplot(stat = 'identity', position = "dodge",
       aes(x = Ano, y = Taxa, color = `sexo`, group = `sexo`)) +
  geom_line(alpha = 0.8, stroke = 0, size = 1.1) +
  geom_point(aes(shape=`sexo`)) +
  theme_bw(16) +
  scale_size(range=c(2,10), guide="none") +
  labs(title = "Evolução da informalidade por gênero",
       x = "Período",
       y = "Taxa de Informalidade (%)") +
  facet_wrap( ~ regiao, nrow = 1) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    geom_text_repel(aes(label = percent(Taxa, accuracy = 0.1, decimal.mark = ',')),
                        data = graf %>% filter(Ano %in% c(2022, 2016)), color = 'black', #vjust=-.05,
                        size=3.3, direction='y', force=7, position = position_dodge(.1)) +
    guides(fill = FALSE, linetype = FALSE, shape = FALSE) +
      scale_color_excel_new()

graf2 <- 
        graf2 + theme(axis.title = element_text(size = 12),
        axis.title.y = element_text(size = 10.5),                 
        axis.text.x = element_text(size = 10.2, color = 'black'),
        axis.text.y = element_text(size = 10.2, color = 'black', face = 'bold'),
        legend.title = element_text(size = 12),
        legend.position=c(.5,-.3),plot.margin = unit(c(.5,.5,3.5,.5), "lines"), legend.direction = "horizontal",
        legend.text=element_text(size=11.5),
        legend.background = element_rect(fill="ghostwhite", size=1),
        plot.title = element_blank(),
        axis.ticks = element_line(colour = "grey70", size = 0.2),
        panel.grid.major = element_line(colour = "grey70", size = 0.2),
        panel.grid.minor = element_blank(),
        #panel.grid.major.x = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 12)) + 
        guides(
           colour = guide_legend(""),
           size = guide_legend(""),
           shape = guide_legend(""))

graf2

ggsave(plot = graf2, file = "Imgs/genero.png", dpi = 200, width = 8.6, height = 4)
}

#-- GRÁFICO RAÇA

{
graf <- df_Raça %>%
  filter(`Raça` != 'Outras') %>%
  mutate(regiao = case_when(regiao == 'Brasil' ~ '(a) Brasil',
                            regiao == 'Nordeste' ~ '(b) Nordeste',
                            regiao == 'Piauí' ~ '(c) Piauí')) %>%
  group_by(Ano, `Raça`, regiao) %>%
  summarise(Taxa = mean(informais/ocupados, na.rm = T))

graf3 <- 
graf %>%
ggplot(stat = 'identity', position = "dodge",
       aes(x=Ano, y=Taxa, color = `Raça`, group = `Raça`)) +
  geom_line(alpha = 0.8, stroke = 0, size = 1.1) +
  geom_point(aes(shape=`Raça`)) +
  theme_bw(16) +
  scale_size(range=c(2,10), guide="none") +
  labs(title = "Evolução da informalidade por raça/etnia",
       x = "Período",
       y = "Taxa de Informalidade (%)") +
  facet_wrap( ~ regiao, nrow = 1) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), n.breaks = 6) +
    geom_text_repel(aes(label = percent(Taxa, accuracy = 0.1, decimal.mark = ',')),
                        data = graf %>% filter(Ano %in% c(2022, 2016)), color = 'black', #vjust=-.05,
                        size=3, direction='y', force=7, position = position_dodge(.1)) +
    guides(fill = FALSE, linetype = FALSE, shape = FALSE) +
      scale_color_excel_new()

graf3 <- 
        graf3 + theme(axis.title = element_text(size = 12),
        axis.title.y = element_text(size = 10.5),                 
        axis.text.x = element_text(size = 10.2, color = 'black'),
        axis.text.y = element_text(size = 10.2, color = 'black', face = 'bold'),
        legend.title = element_text(size = 12),
        legend.position=c(.5,-.3),plot.margin = unit(c(.5,.5,3.5,.5), "lines"), legend.direction = "horizontal",
        legend.text=element_text(size=11.5),
        legend.background = element_rect(fill="ghostwhite", size=1),
        plot.title = element_blank(),
        axis.ticks = element_line(colour = "grey70", size = 0.2),
        panel.grid.major = element_line(colour = "grey70", size = 0.2),
        panel.grid.minor = element_blank(),
        #panel.grid.major.x = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size=12)) + 
        guides(
           colour = guide_legend(""),
           size = guide_legend(""),
           shape = guide_legend(""))

graf3

ggsave(plot = graf3, file = "Imgs/raça_etnia.png", dpi = 200, width = 8.6, height = 4)
}

#-- GRÁFICO ZONA

{
  graf <- df_Tipo_de_Região %>%
    mutate(regiao = case_when(regiao == 'Brasil' ~ '(a) Brasil',
                              regiao == 'Nordeste' ~ '(b) Nordeste',
                              regiao == 'Piauí' ~ '(c) Piauí')) %>%
    group_by(Ano, `Tipo de Região`, regiao) %>%
    summarise(Taxa = mean(informais/ocupados, na.rm = T))
  
  graf3 <- 
    graf %>%
    ggplot(stat = 'identity', position = "dodge",
           aes(x=Ano, y=Taxa, color = `Tipo de Região`, group = `Tipo de Região`)) +
    geom_line(alpha = 0.8, stroke = 0, size = 1.1) +
    geom_point(aes(shape=`Tipo de Região`)) +
    theme_bw(16) +
    scale_size(range=c(2,10), guide="none") +
    labs(title = "Evolução da informalidade por `Tipo de Região`",
         x = "Período",
         y = "Taxa de Informalidade (%)") +
    facet_wrap( ~ regiao, nrow = 1) + 
    scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = seq(.35,.7,.1)) +
    geom_text_repel(aes(label = percent(Taxa, accuracy = 0.1, decimal.mark = ',')),
                    data = graf %>% filter(Ano %in% c(2022, 2016)), color = 'black', #vjust=-.05,
                    size=3, direction='y', force=1, position = position_dodge(.1)) +
    guides(group = FALSE, linetype = FALSE, shape = FALSE) +
    scale_color_excel_new()
  
  graf3 <- 
    graf3 + theme(axis.title = element_text(size = 12),
                  axis.title.y = element_text(size = 10.5),                 
                  axis.text.x = element_text(size = 10.2, color = 'black'),
                  axis.text.y = element_text(size = 10.2, color = 'black', face = 'bold'),
                  legend.title = element_text(size = 12),
                  legend.position=c(.5,-.3),plot.margin = unit(c(.5,.5,3.5,.5), "lines"), legend.direction = "horizontal",
                  legend.text=element_text(size=11.7),
                  legend.background = element_rect(fill="ghostwhite", size=1),
                  plot.title = element_blank(),
                  axis.ticks = element_line(colour = "grey70", size = 0.2),
                  panel.grid.major = element_line(colour = "grey70", size = 0.2),
                  panel.grid.minor = element_blank(),
                  #panel.grid.major.x = element_blank(),
                  strip.background = element_blank(),
                  strip.text = element_text(size=12)) + 
    guides(
      colour = guide_legend(""),
      size = guide_legend(""),
      shape = guide_legend(""))
  
  graf3
  
  ggsave(plot = graf3, file = "Imgs/area.png", dpi = 200, width = 8.6, height = 4)
}


#-- GRÁFICO ZONA

{
graf <- df_Zona_Habitada %>%
  mutate(regiao = case_when(regiao == 'Brasil' ~ '(a) Brasil',
                            regiao == 'Nordeste' ~ '(b) Nordeste',
                            regiao == 'Piauí' ~ '(c) Piauí')) %>%
  group_by(Ano, `Zona Habitada`, regiao) %>%
  summarise(Taxa = mean(informais/ocupados, na.rm = T))

graf4 <- 
graf %>%
ggplot(stat = 'identity', position = "dodge",
       aes(x=Ano, y=Taxa, color = `Zona Habitada`, group = `Zona Habitada`)) +
  geom_line(alpha = 0.8, stroke = 0, size = 1.1) +
  geom_point(aes(shape=`Zona Habitada`)) +
  theme_bw(16) +
  scale_size(range=c(2,10), guide="none") +
  labs(title = "Evolução da informalidade por zona",
       x = "Período",
       y = "Taxa de Informalidade (%)") +
  facet_wrap( ~ regiao, nrow = 1) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), n.breaks = 6) +
    geom_text_repel(aes(label = percent(Taxa, accuracy = 0.1, decimal.mark = ',')),
                        data = graf %>% filter(Ano %in% c(2022, 2016)), color = 'black', #vjust=-.05,
                        size=3.1, direction='y', force=7, position = position_dodge(.1)) +
    guides(fill = FALSE, linetype = FALSE, shape = FALSE) +
      scale_color_excel_new()

graf4 <- 
        graf4 + theme(axis.title = element_text(size = 12),
        axis.title.y = element_text(size = 10.5),                 
        axis.text.x = element_text(size = 10.2, color = 'black'),
        axis.text.y = element_text(size = 10.2, color = 'black', face = 'bold'),
        legend.title = element_text(size = 12),
        legend.position=c(.5,-.3),plot.margin = unit(c(.5,.5,3.5,.5), "lines"), legend.direction = "horizontal",
        legend.text=element_text(size=11.7),
        legend.background = element_rect(fill="ghostwhite", size=1),
        plot.title = element_blank(),
        axis.ticks = element_line(colour = "grey70", size = 0.2),
        panel.grid.major = element_line(colour = "grey70", size = 0.2),
        panel.grid.minor = element_blank(),
        #panel.grid.major.x = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size=12)) + 
        guides(
           colour = guide_legend(""),
           size = guide_legend(""),
           shape = guide_legend(""))

graf4

ggsave(plot = graf4, file = "Imgs/habitacao.png", dpi = 200, width = 8.6, height = 4)
}


#-- GRÁFICO FAIXA ETÁRIA

{
graf <- df_Faixa_Etária %>%
  mutate(regiao = case_when(regiao == 'Brasil' ~ '(a) Brasil',
                            regiao == 'Nordeste' ~ '(b) Nordeste',
                            regiao == 'Piauí' ~ '(c) Piauí')) %>%
  group_by(Ano, `Faixa Etária`, regiao) %>%
  summarise(Taxa = mean(informais/ocupados, na.rm = T)) %>% 
filter(`Faixa Etária` != 'Até 14 anos') %>%
mutate(`Faixa Etária` = factor(`Faixa Etária`, levels=c('14 a 19 anos',
                                                        '20 a 29 anos',
                                                        '30 a 39 anos',
                                                        '40 a 49 anos',
                                                        '50 a 59 anos',
                                                        '60 a 64 anos',
                                                        '65 anos ou mais')))  

graf6 <- graf %>%
  ggplot(stat = 'identity', position = "dodge",
         aes(x=Ano, y=Taxa, color = `Faixa Etária`, group = `Faixa Etária`)) +
  geom_line(alpha = 0.8, stroke = 0, size = 1) +
  geom_point(aes(shape=`Faixa Etária`)) +
  theme_bw(16) +
  scale_size(range=c(2,10), guide="none") +
  labs(title = "Evolução da informalidade por gênero",
       x = "Período",
       y = "Taxa de Informalidade (%)") +
  facet_wrap( ~ regiao, nrow = 1) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), n.breaks = 6) +
  scale_x_discrete(expand = expansion(mult = 0.12)) +
  geom_text_repel(data = graf %>% filter(Ano == 2016), 
                  aes(label = percent(Taxa, accuracy = .1, suffix = '', decimal.mark = ',')), max.overlaps = 5,
                  size=2.9, direction='y', force=.5, color = "black", nudge_x = -.5, hjust = "left") + 
  geom_text_repel(data = graf %>% filter(Ano == 2022), 
                  aes(label = percent(Taxa, accuracy = .1, suffix = '', decimal.mark = ',')),
                  size=2.9, direction='y', force=.5, color = "black", nudge_x = .5, hjust = "right") + 
  guides(fill = FALSE, linetype = FALSE, shape = FALSE) +
  scale_fill_hue(c = 30)


graf6 <- 
  graf6 + theme(axis.title = element_text(size = 12),
                axis.title.y = element_text(size = 10.5),                 
                axis.text.x = element_text(size = 10.2, color = 'black'),
                axis.text.y = element_text(size = 10.2, color = 'black', face = 'bold'),
                legend.title = element_text(size = 12),
                #legend.position=c(.5,-.4),plot.margin = unit(c(.5,.5,3.7,.5), "lines"), legend.direction = "horizontal",
                legend.position='bottom',
                legend.text=element_text(size=11.7),
                legend.background = element_rect(fill="ghostwhite", size=1),
                plot.title = element_blank(),
                axis.ticks = element_line(colour = "grey70", size = 0.2),
                panel.grid.major = element_line(colour = "grey70", size = 0.2),
                panel.grid.minor = element_blank(),
                #panel.grid.major.x = element_blank(),
                strip.background = element_blank(),
                strip.text = element_text(size=12)) + 
  guides(
    colour = guide_legend(""),
    size = guide_legend(""),
    shape = guide_legend(""))  

graf6

ggsave(plot = graf6, file = "Imgs/faixa_etaria.png", dpi = 200, width = 9.1, height = 4.5)
}

#-- GRÁFICO SETOR

{
graf <- df_Setor %>%
filter(!is.na(`Setor`)) %>%
  mutate(regiao = case_when(regiao == 'Brasil' ~ '(a) Brasil',
                            regiao == 'Nordeste' ~ '(b) Nordeste',
                            regiao == 'Piauí' ~ '(c) Piauí')) %>%
  group_by(Ano, `Setor`, regiao) %>%
  summarise(Taxa = mean(informais/ocupados, na.rm = T)) %>% 
mutate(`Setor` = factor(`Setor`, levels=c("Agropecuária",
                                     "Indústria Extrativa Mineral",
                                     "Indústria de Transformação",
                                     "Construção",
                                     'Serviços de Comércio',
                                     'Serviços de Utilidade Pública', 
                                     "Demais Serviços Privados"))) 


  graf7 <- 
    graf %>%
    ggplot(stat = 'identity', position = "dodge",
           aes(x=Ano, y=Taxa, color = `Setor`, group = `Setor`)) +
    geom_line(alpha = 0.8, stroke = 0, size = .9) +
    geom_point(aes(shape=Setor)) +
    theme_bw(16) +
    scale_size(range=c(2,10), guide="none") +
    labs(title = "Evolução da informalidade por gênero",
         x = "Período",
         y = "Taxa de Informalidade (%)") +
    facet_wrap( ~ regiao, nrow = 1) + 
    scale_y_continuous(labels = scales::percent_format(accuracy = 1), n.breaks = 6) +
    scale_x_discrete(expand = expansion(mult = 0.12)) +
    geom_text_repel(data = graf %>% filter(Ano == 2016), 
                    aes(label = percent(Taxa, accuracy = .1, suffix = '', decimal.mark = ',')), max.overlaps = 5,
                    size=2.9, direction='y', force=.5, color = "black", nudge_x = -.5, hjust = "left") + 
    geom_text_repel(data = graf %>% filter(Ano == 2022), 
                    aes(label = percent(Taxa, accuracy = .1, suffix = '', decimal.mark = ',')),
                    size=2.9, direction='y', force=.5, color = "black", nudge_x = .5, hjust = "right") + 
    guides(fill = FALSE, linetype = FALSE, shape = FALSE) +
    scale_fill_hue(c = 30)
  
  
  graf7 <- 
    graf7 + theme(axis.title = element_text(size = 12),
                  axis.title.y = element_text(size = 10.5),                 
                  axis.text.x = element_text(size = 10.2, color = 'black'),
                  axis.text.y = element_text(size = 10.2, color = 'black', face = 'bold'),
                  legend.title = element_text(size = 12),
                  #legend.position=c(.5,-.4),plot.margin = unit(c(.7,.7,5,.7), "lines"), 
                  legend.position='bottom',
                  legend.direction = "horizontal", 
                  legend.text=element_text(size=10.5),
                  legend.background = element_rect(fill="ghostwhite", size=1),
                  plot.title = element_blank(),
                  axis.ticks = element_line(colour = "grey70", size = 0.2),
                  panel.grid.major = element_line(colour = "grey70", size = 0.2),
                  panel.grid.minor = element_blank(),
                  #panel.grid.major.x = element_blank(),
                  strip.background = element_blank(),
                  strip.text = element_text(size=12)) + 
            guides(
                  colour = guide_legend("",nrow=2,byrow=TRUE),
                  size = guide_legend("",nrow=2,byrow=TRUE),
                  shape = guide_legend("",nrow=2,byrow=TRUE))  
  
graf7

ggsave(plot = graf7, file = "Imgs/Setor.png", dpi = 200, width = 9.1, height = 4.5)
}

#-- GRÁFICO ESCOLARIDADE


{
  
df_Ciclo_Escolar %>% na.omit()
graf <- df_Ciclo_Escolar %>%
filter(!is.na(`Ciclo Escolar`)) %>%
  mutate(regiao = case_when(regiao == 'Brasil' ~ '(a) Brasil',
                            regiao == 'Nordeste' ~ '(b) Nordeste',
                            regiao == 'Piauí' ~ '(c) Piauí')) %>%
  group_by(Ano, `Ciclo Escolar`, regiao) %>%
  summarise(Taxa = mean(informais/ocupados, na.rm = T)) %>% 
mutate(`Ciclo Escolar` = factor(`Ciclo Escolar`, levels=c('Sem instrução',
                                          'Fundamental incompleto',
                                          'Fundamental completo',
                                          'Médio incompleto',
                                          'Médio completo',
                                          'Superior incompleto',
                                          'Superior completo')))  


graf8 <- 
  graf %>%
  ggplot(stat = 'identity', position = "dodge",
         aes(x=Ano, y=Taxa, color = `Ciclo Escolar`, group = `Ciclo Escolar`)) +
  geom_line(alpha = 0.8, stroke = 0, size = .9) +
  geom_point(aes(shape=`Ciclo Escolar`)) +
  theme_bw(16) +
  scale_size(range=c(2,10), guide="none") +
  labs(title = "Evolução da informalidade por gênero",
       x = "Período",
       y = "Taxa de Informalidade (%)") +
  facet_wrap( ~ regiao, nrow = 1) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), n.breaks = 6) +
  scale_x_discrete(expand = expansion(mult = 0.13)) +
  geom_text_repel(data = graf %>% filter(Ano == 2016), 
                  aes(label = percent(Taxa, accuracy = .1, suffix = '', decimal.mark = ',')), max.overlaps = 5,
                  size=2.9, direction='y', force=.5, color = "black", nudge_x = -.5, hjust = "left") + 
  geom_text_repel(data = graf %>% filter(Ano == 2022), 
                  aes(label = percent(Taxa, accuracy = .1, suffix = '', decimal.mark = ',')),
                  size=2.9, direction='y', force=.5, color = "black", nudge_x = .5, hjust = "right") + 
  guides(fill = FALSE, linetype = FALSE, shape = FALSE) +
  scale_fill_hue(c = 30)


graf8 <- 
  graf8 + theme(axis.title = element_text(size = 12),
                axis.title.y = element_text(size = 10.5),                 
                axis.text.x = element_text(size = 10.2, color = 'black'),
                axis.text.y = element_text(size = 10.2, color = 'black', face = 'bold'),
                legend.title = element_text(size = 12),
                legend.position='bottom',
                legend.direction = "horizontal", 
                legend.text=element_text(size=10.5),
                legend.background = element_rect(fill="ghostwhite", size=1),
                plot.title = element_blank(),
                axis.ticks = element_line(colour = "grey70", size = 0.1),
                panel.grid.major = element_line(colour = "grey70", size = 0.2),
                panel.grid.minor = element_blank(),
                #panel.grid.major.x = element_blank(),
                strip.background = element_blank(),
                strip.text = element_text(size=12)) + 
  guides(
    colour = guide_legend("",nrow=2,byrow=TRUE),
    size = guide_legend("",nrow=2,byrow=TRUE),
    shape = guide_legend("",nrow=2,byrow=TRUE))  

graf8

ggsave(plot = graf8, file = "Imgs/ciclo_escolar.png", dpi = 200, width = 9.1, height = 4.5)
}


#--  GRÁFICO RENDIMENTO

{

graf <- df_Grupo_de_Rendimento %>%
filter(!is.na(`Grupo de Rendimento`)) %>%
  mutate(regiao = case_when(regiao == 'Brasil' ~ '(a) Brasil',
                            regiao == 'Nordeste' ~ '(b) Nordeste',
                            regiao == 'Piauí' ~ '(c) Piauí')) %>%
  group_by(Ano, `Grupo de Rendimento`, regiao) %>%
  summarise(Taxa = mean(informais/ocupados, na.rm = T)) %>% 
mutate(`Grupo de Rendimento` = factor(`Grupo de Rendimento`, 
                                      levels=c('Até 1 SM',
                                               'Mais de 1 a 2 SM',
                                               'Mais de 2 a 3 SM',
                                               'Mais de 3 a 5 SM',
                                               'Mais de 5 a 10 SM',
                                               'Mais de 10 SM'))) %>% na.omit()

graf11 <- 
  graf %>%
  ggplot(stat = 'identity', position = "dodge",
         aes(x=Ano, y=Taxa, color = `Grupo de Rendimento`, group = `Grupo de Rendimento`)) +
  geom_line(alpha = 0.8, stroke = 0, size = .9) +
  geom_point(aes(shape=`Grupo de Rendimento`)) +
  theme_bw(16) +
  scale_size(range=c(2,10), guide="none") +
  labs(title = "Evolução da informalidade por gênero",
       x = "Período",
       y = "Taxa de Informalidade (%)") +
  facet_wrap( ~ regiao, nrow = 1) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), n.breaks = 6) +
  scale_x_discrete(expand = expansion(mult = 0.13)) +
  geom_text_repel(data = graf %>% filter(Ano == 2016), 
                  aes(label = percent(Taxa, accuracy = .1, suffix = '', decimal.mark = ',')), max.overlaps = 5,
                  size=2.9, direction='y', force=.5, color = "black", nudge_x = -.5, hjust = "left") + 
  geom_text_repel(data = graf %>% filter(Ano == 2022), 
                  aes(label = percent(Taxa, accuracy = .1, suffix = '', decimal.mark = ',')),
                  size=2.9, direction='y', force=.5, color = "black", nudge_x = .5, hjust = "right") + 
  guides(fill = FALSE, linetype = FALSE, shape = FALSE) +
  scale_fill_hue(c = 30)


graf11 <- 
  graf11 + theme(axis.title = element_text(size = 12),
                axis.title.y = element_text(size = 10.5),                 
                axis.text.x = element_text(size = 10.2, color = 'black'),
                axis.text.y = element_text(size = 10.2, color = 'black', face = 'bold'),
                legend.title = element_text(size = 12),
                legend.position='bottom',
                legend.direction = "horizontal", 
                legend.text=element_text(size=10.5),
                legend.background = element_rect(fill="ghostwhite", size=1),
                plot.title = element_blank(),
                axis.ticks = element_line(colour = "grey70", size = 0.2),
                panel.grid.major = element_line(colour = "grey70", size = 0.2),
                panel.grid.minor = element_blank(),
                #panel.grid.major.x = element_blank(),
                strip.background = element_blank(),
                strip.text = element_text(size=12)) + 
  guides(
    colour = guide_legend("",nrow=2,byrow=TRUE),
    size = guide_legend("",nrow=2,byrow=TRUE),
    shape = guide_legend("",nrow=2,byrow=TRUE))  

graf11

ggsave(plot = graf11, file = "Imgs/grupoderendimento.png", dpi = 200, width = 9.1, height = 4.5)
}

#-- GRÁFICO EXPERIÊNCIA

{
graf <- df_Experiência %>%
filter(!is.na(`Experiência`)) %>%
  mutate(regiao = case_when(regiao == 'Brasil' ~ '(a) Brasil',
                            regiao == 'Nordeste' ~ '(b) Nordeste',
                            regiao == 'Piauí' ~ '(c) Piauí')) %>%
  group_by(Ano, `Experiência`, regiao) %>%
  summarise(Taxa = mean(informais/ocupados, na.rm = T)) %>% 
mutate(`Experiência` = factor(`Experiência`, levels=c("Até 5 Anos",
                                                      "5 a 9 anos",
                                                      "10 a 14 anos",
                                                      "15 a 19 anos",
                                                      "20 a 24 anos",
                                                      "25 a 29 anos",
                                                      "Mais de 29 Anos")))  %>% na.omit()

graf10 <- 
  graf %>%
  ggplot(stat = 'identity', position = "dodge",
         aes(x=Ano, y=Taxa, color = `Experiência`, group = `Experiência`)) +
  geom_line(alpha = 0.8, stroke = 0, size = .9) +
  geom_point(aes(shape=`Experiência`)) +
  theme_bw(16) +
  scale_size(range=c(2,10), guide="none") +
  labs(title = "Evolução da informalidade por gênero",
       x = "Período",
       y = "Taxa de Informalidade (%)") +
  facet_wrap( ~ regiao, nrow = 1) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), n.breaks = 6) +
  scale_x_discrete(expand = expansion(mult = 0.13)) +
  geom_text_repel(data = graf %>% filter(Ano == 2016), 
                  aes(label = percent(Taxa, accuracy = .1, suffix = '', decimal.mark = ',')), 
                  size=2.9, direction='y', force=.5, color = "black", nudge_x = -.5, hjust = "left") + 
  geom_text_repel(data = graf %>% filter(Ano == 2022), 
                  aes(label = percent(Taxa, accuracy = .1, suffix = '', decimal.mark = ',')),
                  size=2.9, direction='y', force=.5, color = "black", nudge_x = .5, hjust = "right") + 
  guides(group = FALSE, linetype = FALSE, shape = FALSE) +
  scale_fill_hue(c = 30)


graf10 <- 
  graf10 + theme(axis.title = element_text(size = 12),
                 axis.title.y = element_text(size = 10.5),                 
                 axis.text.x = element_text(size = 10.2, color = 'black'),
                 axis.text.y = element_text(size = 10.2, color = 'black', face = 'bold'),
                 legend.title = element_text(size = 12),
                 legend.position='bottom',
                 legend.direction = "horizontal", 
                 legend.text=element_text(size=10.5),
                 legend.background = element_rect(fill="ghostwhite", size=1),
                 plot.title = element_blank(),
                 axis.ticks = element_line(colour = "grey70", size = 0.2),
                 panel.grid.major = element_line(colour = "grey70", size = 0.2),
                 panel.grid.minor = element_blank(),
                 #panel.grid.major.x = element_blank(),
                 strip.background = element_blank(),
                 strip.text = element_text(size=12)) + 
  guides(
    colour = guide_legend("",nrow=2,byrow=TRUE),
    size = guide_legend("",nrow=2,byrow=TRUE),
    shape = guide_legend("",nrow=2,byrow=TRUE))  

graf10

ggsave(plot = graf10, file = "Imgs/experiência.png", dpi = 200,  width = 9.1, height = 4.8)
}

#-- GRÁFICO POR OCUPAÇÃO

unique(df_Condição$Condição)

{
graf <- df_Condição %>%
  mutate(regiao = case_when(regiao == 'Brasil' ~ '(a) Brasil',
                            regiao == 'Nordeste' ~ '(b) Nordeste',
                            regiao == 'Piauí' ~ '(c) Piauí')) %>%
  group_by(Ano, `Condição`, regiao) %>%
  summarise(Taxa = mean(informais/ocupados, na.rm = T)) %>%
mutate(`Condição` = factor(`Condição`, levels=c('Trabalhador setor privado',
                                                'Trabalhador doméstico',
                                                'Empregador',
                                                'Conta própria'))) %>%  
filter(`Condição` != 'Trabalhador setor público' | `Condição` != 'Trabalhador auxiliar') 

graf2 <- 
  graf %>%
  ggplot(stat = 'identity', position = "dodge",
         aes(x=Ano, y=Taxa, color = `Condição`, group = `Condição`)) +
  geom_line(alpha = 0.8, stroke = 0, size = .9) +
  geom_point(aes(shape=`Condição`)) +
  theme_bw(16) +
  scale_size(range=c(2,10), guide="none") +
  labs(title = "Evolução da informalidade por gênero",
       x = "Período",
       y = "Taxa de Informalidade (%)") +
  facet_wrap( ~ regiao, nrow = 1) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),breaks = seq(.2,.9,.1)) +
  scale_x_discrete(expand = expansion(mult = 0.13)) +
  geom_text_repel(data = graf %>% filter(Ano == 2016), 
                  aes(label = percent(Taxa, accuracy = .1, suffix = '%', decimal.mark = ',')), max.overlaps = 5,
                  size=3.1, direction='y', force=.5, color = "black", nudge_x = -.5, hjust = "left") + 
  geom_text_repel(data = graf %>% filter(Ano == 2022), 
                  aes(label = percent(Taxa, accuracy = .1, suffix = '%', decimal.mark = ',')),
                  size=3.1, direction='y', force=.5, color = "black", nudge_x = .5, hjust = "right") + 
  guides(group = FALSE, linetype = FALSE, shape = FALSE) +
  scale_fill_hue(c = 30)


graf2 <- 
  graf2 + theme(axis.title = element_text(size = 12),
                 axis.title.y = element_text(size = 10.5),                 
                 axis.text.x = element_text(size = 10.2, color = 'black'),
                 axis.text.y = element_text(size = 10.2, color = 'black', face = 'bold'),
                 legend.title = element_text(size = 12),
                 legend.position='bottom',
                 legend.direction = "horizontal", 
                 legend.text=element_text(size=10.5),
                 legend.background = element_rect(fill="ghostwhite", size=1),
                 plot.title = element_blank(),
                 axis.ticks = element_line(colour = "grey70", size = 0.2),
                 panel.grid.major = element_line(colour = "grey70", size = 0.2),
                 panel.grid.minor = element_blank(),
                 #panel.grid.major.x = element_blank(),
                 strip.background = element_blank(),
                 strip.text = element_text(size=12)) + 
  guides(
    colour = guide_legend("",nrow=1,byrow=TRUE),
    size = guide_legend("",nrow=1,byrow=TRUE),
    shape = guide_legend("",nrow=1,byrow=TRUE))  

graf2

ggsave(plot = graf2, file = "Imgs/condição.png", dpi = 200, width = 9.1, height = 4.3)
}

#-- GRÁFICO HORAS TRABALHADAS

unique(df_Faixa_horas_trabalho$`Faixa de horas trabalho`)

{
  graf <- df_Faixa_horas_trabalho %>%
    group_by(Ano, `Faixa de horas trabalho`, regiao) %>%
    summarise(Taxa = mean(informais/ocupados, na.rm = T)) %>%
    mutate(regiao = case_when(regiao == 'Brasil' ~ '(a) Brasil',
                              regiao == 'Nordeste' ~ '(b) Nordeste',
                              regiao == 'Piauí' ~ '(c) Piauí'), 
           `Faixa de horas trabalho` = ifelse(is.na(`Faixa de horas trabalho`), "Mais de 45h", `Faixa de horas trabalho`),
           `Faixa de horas trabalho` = factor(`Faixa de horas trabalho`, 
                                              levels=c("Até 14 horas",
                                                       "Mais de 15 a 39h",
                                                       "Mais de 40 a 44h",
                                                       "Mais de 45h"))) %>% na.omit()
  
graf12 <- 
  graf %>%
  ggplot(stat = 'identity', position = "dodge",
         aes(x=Ano, y=Taxa, color = `Faixa de horas trabalho`, group = `Faixa de horas trabalho`)) +
  geom_line(alpha = 0.8, stroke = 0, size = .9) +
  geom_point(aes(shape=`Faixa de horas trabalho`)) +
  theme_bw(16) +
  scale_size(range=c(2,10), guide="none") +
  labs(title = "Evolução da informalidade por gênero",
       x = "Período",
       y = "Taxa de Informalidade (%)") +
  facet_wrap( ~ regiao, nrow = 1) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = seq(.3,1,.1)) +
  scale_x_discrete(expand = expansion(mult = 0.13)) +
  geom_text_repel(data = graf %>% filter(Ano == 2016), 
                  aes(label = percent(Taxa, accuracy = .1, suffix = '%', decimal.mark = ',')), max.overlaps = 5,
                  size=3.1, direction='y', force=.5, color = "black", nudge_x = -.5, hjust = "left") + 
  geom_text_repel(data = graf %>% filter(Ano == 2022), 
                  aes(label = percent(Taxa, accuracy = .1, suffix = '%', decimal.mark = ',')),
                  size=3.1, direction='y', force=.5, color = "black", nudge_x = .5, hjust = "right") + 
  guides(group = FALSE, linetype = FALSE, shape = FALSE) +
  scale_color_excel_new()


graf12 <- 
  graf12 + theme(axis.title = element_text(size = 12),
                axis.title.y = element_text(size = 10.5),                 
                axis.text.x = element_text(size = 10.2, color = 'black'),
                axis.text.y = element_text(size = 10.2, color = 'black', face = 'bold'),
                legend.title = element_text(size = 12),
                legend.position='bottom',
                legend.direction = "horizontal", 
                legend.text=element_text(size=12),
                legend.background = element_rect(fill="ghostwhite", size=1),
                plot.title = element_blank(),
                axis.ticks = element_line(colour = "grey70", size = 0.2),
                panel.grid.major = element_line(colour = "grey70", size = 0.2),
                panel.grid.minor = element_blank(),
                #panel.grid.major.x = element_blank(),
                strip.background = element_blank(),
                strip.text = element_text(size=12)) + 
  guides(
    colour = guide_legend("",nrow=1,byrow=TRUE),
    size = guide_legend("",nrow=1,byrow=TRUE),
    shape = guide_legend("",nrow=1,byrow=TRUE))  

graf12

ggsave(plot = graf12, file = "Imgs/horas.png", dpi = 200, width = 9.1, height = 4.3)
}


# TABELAS -------------------------------------------------------------------------------------------


# TABELA CONDIÇÃO

{
  
  tab_percent <- `df_Condição` %>%
    filter(!is.na(`Condição`) & Ano %in% c(2016, 2022)) %>%
    group_by(Ano, regiao, `Condição`) %>%
    summarise_at(.vars = c('formais', 'informais', 'ocupados'), mean)
  
  tab_percent %<>% 
    inner_join(tab_percent %>% group_by(Ano, regiao) %>% 
                 summarise(ocupados_total = sum(ocupados)), 
               by = c('Ano', 'regiao'))
  
  tab_efeitos <- tab_percent %>% 
    mutate(TI = (informais/ocupados), TP = (ocupados/ocupados_total)) %>%
    pivot_wider(names_from = Ano,
                values_from = formais:TP) %>%
    mutate('Efeito-Nível' = efc_nivel(TI_2016, TI_2022, TP_2016, TP_2022),
           'Efeito-Composição' = efc_comp(TI_2016, TI_2022, TP_2016, TP_2022),
           'Efeito-Total' = `Efeito-Nível` + `Efeito-Composição`)  
  #%>% select('regiao', `Condição`, TI_2016:'Efeito-Total')
  
  write_xlsx(
    tab_efeitos,
    "Tabs/tabela_por_condição.xlsx",
    col_names = TRUE,
    format_headers = TRUE
  )
}

# TABELA SEXO

{ 
  
tab_percent <- df_sexo %>%
    filter(Ano %in% c(2016, 2022)) %>%
    group_by(Ano, regiao, sexo) %>%
    summarise_at(.vars = c('formais', 'informais', 'ocupados'), mean)%>%
    mutate(formais = round(formais/1000, 4),
           informais = round(informais/1000, 4),
           ocupados = round(ocupados/1000, 4)) 
  
tab_percent %<>% inner_join(tab_percent %>% group_by(Ano, regiao) %>% 
                                summarise(ocupados_total = sum(ocupados)), 
                              by = c('Ano', 'regiao'))
  
tab_efeitos <- tab_percent %>% 
    mutate(TI = (informais/ocupados), TP = (ocupados/ocupados_total)) %>%
    pivot_wider(names_from = Ano,
                values_from = formais:TP) %>%
    mutate('Efeito-Nível' = efc_nivel(TI_2016, TI_2022, TP_2016, TP_2022),
           'Efeito-Composição' = efc_comp(TI_2016, TI_2022, TP_2016, TP_2022),
           'Efeito-Total' = `Efeito-Nível` + `Efeito-Composição`)  #%>%
  #select('regiao', sexo, TI_2016:'Efeito-Total')
  
write_xlsx(
    tab_efeitos,
    "Tabs/tabela_por_sexo.xlsx",
    col_names = TRUE,
    format_headers = TRUE
  )
  
}


# TABELA RAÇA

{
tab_percent <- df_Raça %>%
  filter(Ano %in% c(2016, 2022)) %>%
  group_by(Ano, regiao, Raça) %>%
  summarise_at(.vars = c('formais', 'informais', 'ocupados'), mean)%>%
  mutate(formais = round(formais/1000, 4),
         informais = round(informais/1000, 4),
         ocupados = round(ocupados/1000, 4)) 

tab_percent %<>% inner_join(tab_percent %>% group_by(Ano, regiao) %>% 
                              summarise(ocupados_total = sum(ocupados)), 
                            by = c('Ano', 'regiao'))

tab_efeitos <- tab_percent %>% 
  mutate(TI = (informais/ocupados), TP = (ocupados/ocupados_total)) %>%
  pivot_wider(names_from = Ano,
              values_from = formais:TP) %>%
  mutate('Efeito-Nível' = efc_nivel(TI_2016, TI_2022, TP_2016, TP_2022),
         'Efeito-Composição' = efc_comp(TI_2016, TI_2022, TP_2016, TP_2022),
         'Efeito-Total' = `Efeito-Nível` + `Efeito-Composição`) 

write_xlsx(
  tab_efeitos,
  "Tabs/tabela_por_Raça.xlsx",
  col_names = TRUE,
  format_headers = TRUE
)

}

# TABELA LOCALIDADE 

{
tab_percent <- df_Zona_Habitada %>%
    filter(Ano %in% c(2016, 2022)) %>%
    group_by(Ano, regiao, `Zona Habitada`) %>%
    summarise_at(.vars = c('formais', 'informais', 'ocupados'), mean)%>%
    mutate(formais = round(formais/1000, 4),
           informais = round(informais/1000, 4),
           ocupados = round(ocupados/1000, 4)) 
  
tab_percent %<>% inner_join(tab_percent %>% group_by(Ano, regiao) %>% 
                                summarise(ocupados_total = sum(ocupados)), 
                              by = c('Ano', 'regiao'))
  
tab_efeitos <- tab_percent %>% 
    mutate(TI = (informais/ocupados), TP = (ocupados/ocupados_total)) %>%
    pivot_wider(names_from = Ano,
                values_from = formais:TP) %>%
mutate('Efeito-Nível' = efc_nivel(TI_2016, TI_2022, TP_2016, TP_2022),
           'Efeito-Composição' = efc_comp(TI_2016, TI_2022, TP_2016, TP_2022),
           'Efeito-Total' = `Efeito-Nível` + `Efeito-Composição`) 
  
write_xlsx(
    tab_efeitos,
    "Tabs/tabela_por_zona.xlsx",
    col_names = TRUE,
    format_headers = TRUE
  )
}

# TABELA TIPO DE LOCAL

{
  
  tab_percent <- df_Tipo_de_Região %>%
    filter(Ano %in% c(2016, 2022)) %>%
    group_by(Ano, regiao, `Tipo de Região`) %>%
    summarise_at(.vars = c('formais', 'informais', 'ocupados'), mean)%>%
    mutate(formais = round(formais/1000, 4),
           informais = round(informais/1000, 4),
           ocupados = round(ocupados/1000, 4)) 
  
  tab_percent %<>% inner_join(tab_percent %>% group_by(Ano, regiao) %>% 
                                summarise(ocupados_total = sum(ocupados)), 
                              by = c('Ano', 'regiao'))
  
  tab_efeitos <- tab_percent %>% 
    mutate(TI = (informais/ocupados), TP = (ocupados/ocupados_total)) %>%
    pivot_wider(names_from = Ano,
                values_from = formais:TP) %>%
    mutate('Efeito-Nível' = efc_nivel(TI_2016, TI_2022, TP_2016, TP_2022),
           'Efeito-Composição' = efc_comp(TI_2016, TI_2022, TP_2016, TP_2022),
           'Efeito-Total' = `Efeito-Nível` + `Efeito-Composição`) 
  
  write_xlsx(
    tab_efeitos,
    "Tabs/tabela_por_área.xlsx",
    col_names = TRUE,
    format_headers = TRUE
  )
}


# TABELA SETOR 

{
  tab_percent <- df_Setor %>%
    filter(Ano %in% c(2016, 2022)) %>%
    group_by(Ano, regiao, `Setor`) %>%
    summarise_at(.vars = c('formais', 'informais', 'ocupados'), mean)%>%
    mutate(formais = round(formais/1000, 4),
           informais = round(informais/1000, 4),
           ocupados = round(ocupados/1000, 4)) 
  
  tab_percent %<>% inner_join(tab_percent %>% group_by(Ano, regiao) %>% 
                                summarise(ocupados_total = sum(ocupados)), 
                              by = c('Ano', 'regiao'))
  
  tab_efeitos <- tab_percent %>% 
    mutate(TI = (informais/ocupados), TP = (ocupados/ocupados_total)) %>%
    pivot_wider(names_from = Ano,
                values_from = formais:TP) %>%
    mutate('Efeito-Nível' = efc_nivel(TI_2016, TI_2022, TP_2016, TP_2022),
           'Efeito-Composição' = efc_comp(TI_2016, TI_2022, TP_2016, TP_2022),
           'Efeito-Total' = `Efeito-Nível` + `Efeito-Composição`) 
  
  write_xlsx(
    tab_efeitos,
    "Tabs/tabela_por_setor.xlsx",
    col_names = TRUE,
    format_headers = TRUE
  )
}

# TABELA EXPERIÊNCIA

{
  tab_percent <- df_Experiência %>%
    filter(Ano %in% c(2016, 2022)) %>%
    group_by(Ano, regiao, `Experiência`) %>%
    summarise_at(.vars = c('formais', 'informais', 'ocupados'), mean)%>%
    mutate(formais = round(formais/1000, 4),
           informais = round(informais/1000, 4),
           ocupados = round(ocupados/1000, 4)) 
  
  tab_percent %<>% inner_join(tab_percent %>% group_by(Ano, regiao) %>% 
                                summarise(ocupados_total = sum(ocupados)), 
                              by = c('Ano', 'regiao'))
  
  tab_efeitos <- tab_percent %>% 
    mutate(TI = (informais/ocupados), TP = (ocupados/ocupados_total)) %>%
    pivot_wider(names_from = Ano,
                values_from = formais:TP) %>%
    mutate('Efeito-Nível' = efc_nivel(TI_2016, TI_2022, TP_2016, TP_2022),
           'Efeito-Composição' = efc_comp(TI_2016, TI_2022, TP_2016, TP_2022),
           'Efeito-Total' = `Efeito-Nível` + `Efeito-Composição`) 
  
  write_xlsx(
    tab_efeitos,
    "Tabs/tabela_por_experiência.xlsx",
    col_names = TRUE,
    format_headers = TRUE)
}


# TABELA FAIXA ETÁRIA 

{
  tab_percent <- df_Faixa_Etária %>%
#    mutate(`Faixa Etária` = ifelse(is.na(`Faixa Etária`), "60-64 anos", `Faixa Etária`)) %>%
    filter(Ano %in% c(2016, 2022)) %>%
    group_by(Ano, regiao, `Faixa Etária`) %>%
    summarise_at(.vars = c('formais', 'informais', 'ocupados'), mean)%>%
    filter(`Faixa Etária` != '< 14 anos') %>%
    mutate(
           formais = round(formais/1000, 4),
           informais = round(informais/1000, 4),
           ocupados = round(ocupados/1000, 4))  
  
  tab_percent %<>% inner_join(tab_percent %>% group_by(Ano, regiao) %>% 
                                summarise(ocupados_total = sum(ocupados)), 
                              by = c('Ano', 'regiao'))
  
  tab_efeitos <- tab_percent %>% 
    mutate(TI = (informais/ocupados), TP = (ocupados/ocupados_total)) %>%
    pivot_wider(names_from = Ano,
                values_from = formais:TP) %>%
    mutate('Efeito-Nível' = efc_nivel(TI_2016, TI_2022, TP_2016, TP_2022),
           'Efeito-Composição' = efc_comp(TI_2016, TI_2022, TP_2016, TP_2022),
           'Efeito-Total' = `Efeito-Nível` + `Efeito-Composição`) 
  
  write_xlsx(
    tab_efeitos,
    "Tabs/tabela_por_faixa_etaria.xlsx",
    col_names = TRUE,
    format_headers = TRUE
  )
}

# TABELA RENDIMENTO

{
  tab_percent <- df_Grupo_de_Rendimento %>%
    filter(Ano %in% c(2016, 2022)) %>%
    group_by(Ano, regiao, `Grupo de Rendimento`) %>%
    summarise_at(.vars = c('formais', 'informais', 'ocupados'), mean)%>%
    mutate(formais = round(formais/1000, 4),
           informais = round(informais/1000, 4),
           ocupados = round(ocupados/1000, 4)) 
  
  tab_percent %<>% inner_join(tab_percent %>% group_by(Ano, regiao) %>% 
                                summarise(ocupados_total = sum(ocupados)), 
                              by = c('Ano', 'regiao'))
  
  tab_efeitos <- tab_percent %>% 
    mutate(TI = (informais/ocupados), TP = (ocupados/ocupados_total)) %>%
    pivot_wider(names_from = Ano,
                values_from = formais:TP) %>%
    mutate('Efeito-Nível' = efc_nivel(TI_2016, TI_2022, TP_2016, TP_2022),
           'Efeito-Composição' = efc_comp(TI_2016, TI_2022, TP_2016, TP_2022),
           'Efeito-Total' = `Efeito-Nível` + `Efeito-Composição`) 
  
  write_xlsx(
    tab_efeitos,
    "Tabs/tabela_por_rendimento.xlsx",
    col_names = TRUE,
    format_headers = TRUE)
}

# TABELA CICLO ESCOLAR

{
  tab_percent <- df_Ciclo_Escolar %>%
    filter(Ano %in% c(2016, 2022)) %>%
    group_by(Ano, regiao, `Ciclo Escolar`) %>%
    summarise_at(.vars = c('formais', 'informais', 'ocupados'), mean)%>%
    mutate(formais = round(formais/1000, 4),
           informais = round(informais/1000, 4),
           ocupados = round(ocupados/1000, 4)) 
  
  tab_percent %<>% inner_join(tab_percent %>% group_by(Ano, regiao) %>% 
                                summarise(ocupados_total = sum(ocupados)), 
                              by = c('Ano', 'regiao'))
  
  tab_efeitos <- tab_percent %>% 
    mutate(TI = (informais/ocupados), TP = (ocupados/ocupados_total)) %>%
    pivot_wider(names_from = Ano,
                values_from = formais:TP) %>%
    mutate('Efeito-Nível' = efc_nivel(TI_2016, TI_2022, TP_2016, TP_2022),
           'Efeito-Composição' = efc_comp(TI_2016, TI_2022, TP_2016, TP_2022),
           'Efeito-Total' = `Efeito-Nível` + `Efeito-Composição`) 
  
  write_xlsx(
    tab_efeitos,
    "Tabs/tabela_por_escolaridade.xlsx",
    col_names = TRUE,
    format_headers = TRUE)
}

# TABELA HORAS TRABALHO

{
  tab_percent <- df_Faixa_horas_trabalho %>%
    filter(Ano %in% c(2016, 2022)) %>%
    group_by(Ano, regiao, `Faixa de horas trabalho`) %>%
    summarise_at(.vars = c('formais', 'informais', 'ocupados'), mean)%>%
    mutate(formais = round(formais/1000, 4),
           informais = round(informais/1000, 4),
           ocupados = round(ocupados/1000, 4)) 
  
  tab_percent %<>% inner_join(tab_percent %>% group_by(Ano, regiao) %>% 
                                summarise(ocupados_total = sum(ocupados)), 
                              by = c('Ano', 'regiao'))
  
  tab_efeitos <- tab_percent %>% 
    mutate(TI = (informais/ocupados), TP = (ocupados/ocupados_total)) %>%
    pivot_wider(names_from = Ano,
                values_from = formais:TP) %>%
    mutate('Efeito-Nível' = efc_nivel(TI_2016, TI_2022, TP_2016, TP_2022),
           'Efeito-Composição' = efc_comp(TI_2016, TI_2022, TP_2016, TP_2022),
           'Efeito-Total' = `Efeito-Nível` + `Efeito-Composição`) 
  
  write_xlsx(
    tab_efeitos,
    "Tabs/tabela_por_horas.xlsx",
    col_names = TRUE,
    format_headers = TRUE)
}
