#__________________________________________________________________________________________________

# Processamento dos microdados da PNAD Contínua - IBGE 

# Autor: Alexandre Barros
# Email: alexandrepitstop@gmail.com

# --/01/2023
#__________________________________________________________________________________________________


#> Pacotes -------------------------------------------------------------------
libs <- c("PNADcIBGE", "gdata" , "magrittr" , "magrittr", "Cairo", "srvyr", "writexl")
libs.novas <- libs[ !( libs %in% installed.packages()[ , "Package" ] ) ]
if( length( libs.novas ) ) install.packages( libs.novas )


# carregando dependências 
library(tidyverse)
library(srvyr) 
library(Cairo)
library(survey)
library(gdata)
library(PNADcIBGE)
library(magrittr)
library(writexl)

# ! script !
setwd('C:/Users/Alexandre/OneDrive/Documentos/Monografia/Microdados')
#setwd('C:/Users/Usuario/Documents/Microdados')
options(survey.lonely.psu="remove")

# ! PRÉ-CONFIGS ----------------------------------------------------------------


# NOTA: automatização do Run do script, para todos os anos

ano = 2016

repeat{

{ # no repeat
# ETAPA 1 #

{
#-- Nome dos Arquivos
arquivos <- c(str_glue('PNADC_01{ano}.txt'), 
              str_glue('PNADC_02{ano}.txt'), 
              str_glue('PNADC_03{ano}.txt'), 
              str_glue('PNADC_04{ano}.txt'),
#-- Arquivo input - config
              'input_PNADC_trimestral.txt') 

#-- Variáveis a importar
variaveis <- c('UF', 'UPA', 'Estrato', 'posest', 'posest_sxi', 'V1008', 'V1014', 
               'V1028', 'V2007', 'V2009', 'VD4001', 'VD4002', 'VD4009', 'V4046', 
               'V4019', 'V1029', 'V1033', 'V403311', 'VD4014', 'V1023', 'V1022', 
               'V4013', 'VD3004', 'VD3005', 'VD2006', 'V2010', 'VD4017', 'V4043')

#-- variáveis numéricas - conversão
cols.num <- c('V2007', 'VD2006', 'V1022', 'V2010', 'V403311', 'V2009', 'VD3005',
              'VD4014', 'VD4001', 'VD4002', 'VD4009', 'V4046', 'V4019', 'V4013',
              'VD3004', 'VD4017', 'V4043')

#-- grupos correspondentes
grupos <- c('sexo', 'Faixa Etária', 'Raça', 'Zona Habitada', 'Tipo de Região',
            'Setor', 'Ciclo Escolar', 'Grupo de Rendimento', 'Experiência',
            'Faixa de horas trabalho', 'Condição')

#-- n_loops
k=1
j=1
}

# ! IMPORTAÇÃO -----------------------------------------------------------------

# ETAPA 2 #

{
#-- Importação das bases - por ano
dados_pnad_1t <- read_pnadc(arquivos[1], input_txt = arquivos[5], vars = variaveis) 
system.time(Sys.sleep(10))
dados_pnad_2t <- read_pnadc(arquivos[2], input_txt = arquivos[5], vars = variaveis)
system.time(Sys.sleep(10))
dados_pnad_3t <- read_pnadc(arquivos[3], input_txt = arquivos[5], vars = variaveis)
system.time(Sys.sleep(10))
dados_pnad_4t <- read_pnadc(arquivos[4], input_txt = arquivos[5], vars = variaveis)
system.time(Sys.sleep(10))

#-- Seleção das variáveis 
dados_pnad_1t %<>% select("Ano":"VD4017") 
dados_pnad_2t %<>% select("Ano":"VD4017")
dados_pnad_3t %<>% select("Ano":"VD4017") 
dados_pnad_4t %<>% select("Ano":"VD4017") 

#-- Conversão de variáveis numéricas
dados_pnad_1t[cols.num] <- sapply(dados_pnad_1t[cols.num],as.numeric)
dados_pnad_2t[cols.num] <- sapply(dados_pnad_2t[cols.num],as.numeric)
dados_pnad_3t[cols.num] <- sapply(dados_pnad_3t[cols.num],as.numeric)
dados_pnad_4t[cols.num] <- sapply(dados_pnad_4t[cols.num],as.numeric)

gc()
}
system.time(Sys.sleep(30))

# ! CRIAÇÃO DOS IDENTIFICADORES DE VARIÁVEIS -----------------------------------

# ETAPA 3 #

for(i in 1:4){
#-- Criando as novas variáveis derivadas 
  
assign(str_glue('dados_pnad_{i}t'), # ! Base original 

      mutate(get(str_glue('dados_pnad_{i}t')), # Base alterada
                 one = 1, # numerador, para projeção de toda a base
                 Verificador = as.numeric(substr(V4013, 1, 2))
,                 
                 'sexo' = case_when(
                      V2007 == 1 ~ "Masculino",
                      V2007 == 2 ~ "Feminino")
,                 
                 'Faixa Etária' = case_when(
                      VD2006 < 4 ~ 'Até 14 anos',
                      VD2006 == 4 ~ '14 a 19 anos',
                      VD2006 %in% c(5,6) ~ '20 a 29 anos',
                      VD2006 %in% c(7,8) ~ '30 a 39 anos',
                      VD2006 %in% c(9,10) ~ '40 a 49 anos',
                      VD2006 %in% c(11,12) ~ '50 a 59 anos',
                      VD2006 == 13 ~ '60 a 64 anos',
                      VD2006 >= 14 ~ '65 anos ou mais')
,                      
                 'Raça' = case_when(
                      V2010 == 1 ~ "Brancos",
                      V2010 %in% c(2,4) ~ "Pretos e Pardos",
                      TRUE ~ "Outras" )
,                      
                 'Zona Habitada' = case_when(
                      V1022 == 1 ~ "Zona Urbana",
                      V1022 == 2 ~ "Zona Rural")
,                      
                 'Tipo de Região' = case_when(
                      V1023 %in% c(1) ~ "Capital",
                      V1023 %in% c(3,2) ~ "RM e RIDE",
                      V1023 == 4 ~ "Resto da UF")
,                      
                 'Setor' = case_when(
                      Verificador %in% c(1:3) ~ "Agropecuária",
                      Verificador %in% c(5:9) ~ "Indústria Extrativa Mineral",
                      Verificador %in% c(10:33) ~ "Indústria de Transformação",
                      Verificador %in% c(41:43) ~ "Construção",
                      Verificador %in% c(45:47) ~ 'Comércio',
                      Verificador %in% c(35:39) | 
                      Verificador %in% c(84) ~ 'Serviço Público', 
                      TRUE ~ "Serviço Privado")
,                      
                 'Ciclo Escolar' = case_when(
                      VD3004 == 1 ~ 'Sem instrução',
                      VD3004 == 2 ~ 'Fundamental incompleto',
                      VD3004 == 3 ~ 'Fundamental completo',
                      VD3004 == 4 ~ 'Médio incompleto',
                      VD3004 == 5 ~ 'Médio completo',
                      VD3004 == 6 ~ 'Superior incompleto',
                      VD3004 == 7 ~ 'Superior completo')
,                  
                 'Grupo de Rendimento' = case_when(
                      V403311 == 2 | V403311 == 1 ~ 'Até 1 SM',
                      V403311 == 3 ~ 'Mais de 1 a 2 SM',
                      V403311 == 4 ~ 'Mais de 2 a 3 SM',
                      V403311 == 5 ~ 'Mais de 3 a 5 SM',
                      V403311 == 6 ~ 'Mais de 5 a 10 SM',
                      V403311 >= 7 ~ 'Mais de 10 SM')
,                 
                 'Experiência' = case_when(
                      (V2009 - VD3005 - 5) < 5 ~ "Até 5 Anos",
                      (V2009 - VD3005 - 5) %in% c(5:9) ~ "5 a 9 anos",
                      (V2009 - VD3005 - 5) %in% c(10:14) ~ "10 a 14 anos",
                      (V2009 - VD3005 - 5) %in% c(15:19) ~ "15 a 19 anos",
                      (V2009 - VD3005 - 5) %in% c(20:24) ~ "20 a 24 anos",
                      (V2009 - VD3005 - 5) %in% c(25:29) ~ "25 a 29 anos",
                      (V2009 - VD3005 - 5) >= 30 ~ "Mais de 29 Anos")
,                 
                 'CO' = case_when(
                      VD4009 %in% c(1,3,5,6,7) | V4046 == 1 | V4019 == 1 ~ "Formal",
                      VD4009 %in% c(2,4,10) | V4046 == 2 | V4019 == 2 ~ "Informal",
                      VD4002 == 2 ~ "Desocupado", 
                      VD4001 == 2 ~ "Fora da PEA")
,                 
                 'Faixa de horas trabalho' = case_when(
                      VD4014 == 1 ~ "Até 14 horas",
                      VD4014 == 2 ~ "Mais de 15 a 39h",
                      VD4014 == 3 ~ "Mais de 40 a 44h",
                      VD4014 == 4 ~ "Mais de 45 a 48h",
                      VD4014 == 5 ~ "49h ou mais")
,
                 'Condição' = case_when(
                      V4043 == 1 ~ 'Trabalhador doméstico',
                      V4043 %in% c(2,4) ~ 'Trabalhador setor público',
                      V4043 == 3 ~ 'Trabalhador setor privado',
                      V4043 == 7 ~ 'Trabalhador auxiliar',
                      V4043 == 6 ~ 'Conta própria',
                      V4043 == 5 ~ 'Empregador',
                 ) 
                 ))
gc() 
}
system.time(Sys.sleep(30))

# ! CRIAÇÃO DOS PAINÉIS DE DADOS -----------------------------------------------

# ETAPA 4 # 

{

#-- 1º TRIMESTRE 
painel_pnadc_1t <- dados_pnad_1t %>%
  as_survey_design(ids = UPA, 
                   strata = Estrato, 
                   weights = V1028, 
                   nest = TRUE) 
#-- 2º TRIMESTRE 
painel_pnadc_2t <- dados_pnad_2t %>%
  as_survey_design(ids = UPA, 
                   strata = Estrato, 
                   weights = V1028, 
                   nest = TRUE) 
#-- 3º TRIMESTRE 
painel_pnadc_3t <- dados_pnad_3t %>%
  as_survey_design(ids = UPA, 
                   strata = Estrato, 
                   weights = V1028, 
                   nest = TRUE) 
#-- 4º TRIMESTRE 
painel_pnadc_4t <- dados_pnad_4t %>%
  as_survey_design(ids = UPA, 
                   strata = Estrato, 
                   weights = V1028, 
                   nest = TRUE) 

}
system.time(Sys.sleep(30))
gc()

# ! ESTIMATIVAS COM OS DADOS ---------------------------------------------------

# ETAPA 5 #

#-- objetos a utilizar
rendiment_1t <- data.frame()
rendiment_2t <- data.frame()
rendiment_3t <- data.frame()
rendiment_4t <- data.frame()

repeat{

#-- Brasil

for(i in 1:4){
  assign(str_glue('rendimento_{i}t_{grupos[ j ]}_br'), # ! Base original 
         
    mutate(get(str_glue('painel_pnadc_{i}t')) %>%
             #-- base  
             filter(CO %in% c('Informal', 'Formal')) %>%
             #-- grupo de estimação
             group_by(Ano, Trimestre, get(grupos[j]), CO) %>%
             #-- estimação
             summarise(renda_media = survey_mean(VD4017, 
                                                 na.rm = TRUE, 
                                                 vartype = NULL)) %>% 
             #-- ajuste das variáveis
             pivot_wider(names_from = CO, values_from = renda_media) %>%
             #-- consolidação 
             mutate(Variavel = grupos[j])
           
           ) %>% mutate(regiao = 'Brasil')
    )
  system.time(Sys.sleep(20))
  gc()
} 


#-- Nordeste

for(i in 1:4){
  assign(str_glue('informalidade_{i}t_{grupos[ j ]}_ne'), # ! Base original 
         
         mutate(get(str_glue('painel_pnadc_{i}t')) %>%
                  filter(UF %in% c(21:29)) %>%
                  
                  filter(CO %in% c('Informal', 'Formal')) %>%
                  #-- grupo de estimação
                  group_by(Ano, Trimestre, get(grupos[j]), CO) %>%
                  #-- estimação
                  summarise(renda_media = survey_mean(VD4017, 
                                                      na.rm = TRUE, 
                                                      vartype = NULL)) %>% 
                  #-- ajuste das variáveis
                  pivot_wider(names_from = CO, values_from = renda_media) %>%
                  #-- consolidação 
                  mutate(Variavel = grupos[j])
                
                  ) %>% mutate(regiao = 'Nordeste')
         )
system.time(Sys.sleep(20))
gc()
}

#-- Estado do Piauí

for(i in 1:4){
  assign(str_glue('informalidade_{i}t_{grupos[ j ]}_pi'), # ! Base original 
         
         mutate(get(str_glue('painel_pnadc_{i}t')) %>%
                  filter(UF == 22) %>%
                  #-- base  
                  filter(CO %in% c('Informal', 'Formal')) %>%
                  #-- grupo de estimação
                  group_by(Ano, Trimestre, get(grupos[j]), CO) %>%
                  #-- estimação
                  summarise(renda_media = survey_mean(VD4017, 
                                                      na.rm = TRUE, 
                                                      vartype = NULL)) %>% 
                  #-- ajuste das variáveis
                  pivot_wider(names_from = CO, values_from = renda_media) %>%
                  #-- consolidação 
                  mutate(Variavel = grupos[j])
                
                %>% mutate(regiao = 'Piauí')
         ))
  system.time(Sys.sleep(20))
  gc()
}

# ! CONSOLIDANDO OS DADOS ------------------------------------------------------

#-- consolidando
rendiment_1t_br = rbind(rendiment_1t, get(str_glue('rendimento_1t_{grupos[j]}_br')))
rendiment_2t_br = rbind(rendiment_2t, get(str_glue('rendimento_2t_{grupos[j]}_br')))
rendiment_3t_br = rbind(rendiment_3t, get(str_glue('rendimento_3t_{grupos[j]}_br')))
rendiment_4t_br = rbind(rendiment_4t, get(str_glue('rendimento_4t_{grupos[j]}_br')))

#-- consolidando
rendiment_1t_ne = rbind(rendiment_1t, get(str_glue('rendimento_1t_{grupos[j]}_ne')))
rendiment_2t_ne = rbind(rendiment_2t, get(str_glue('rendimento_2t_{grupos[j]}_ne')))
rendiment_3t_ne = rbind(rendiment_3t, get(str_glue('rendimento_3t_{grupos[j]}_ne')))
rendiment_4t_ne = rbind(rendiment_4t, get(str_glue('rendimento_4t_{grupos[j]}_ne')))

#-- consolidando
rendiment_1t_pi = rbind(rendiment_1t, get(str_glue('rendimento_1t_{grupos[j]}_pi')))
rendiment_2t_pi = rbind(rendiment_2t, get(str_glue('rendimento_2t_{grupos[j]}_pi')))
rendiment_3t_pi = rbind(rendiment_3t, get(str_glue('rendimento_3t_{grupos[j]}_pi')))
rendiment_4t_pi = rbind(rendiment_4t, get(str_glue('rendimento_4t_{grupos[j]}_pi')))

j=j+1
  
if (j >= 11) { 
    
    rm(list=ls(pattern="rendimento")) 
    
    df.com_2022 <- rbind(rendiment_1t, rendiment_2t, rendiment_3t, rendiment_4t) %>%
      group_by(Ano, Grupos, Variavel) %>%
      summarise(
        S_informal = mean(Informal, na.rm = T),
        S_formal = mean(Formal, na.rm = T)
      ) %>% mutate(Proporção = S_informal/S_formal)
    
    rm(list=ls(pattern="rendiment"))
    
    saveRDS(df.com_2022, "df.com_2022.RDS")
    
    break() 
  }

}
system.time(Sys.sleep(80))

} # no repeat
  
ano = ano + 6

if(ano >= 2022) {break()}

}




# ! ESTIMATIVAS DOS RENDIMENTOS ------------------------------------------------




repeat {
  
#-- 1º TRIMESTRE
  assign(str_glue('rendimento_1t_{ grupos[ j ] }'),
         
         painel_pnadc_1t %>%
           #-- base  
           filter(CO %in% c('Informal', 'Formal')) %>%
           #-- grupo de estimação
           group_by(Ano, Trimestre, get(grupos[j]), CO) %>%
           #-- estimação
           summarise(renda_media = survey_mean(VD4017, 
                                               na.rm = TRUE, 
                                               vartype = NULL)) %>% 
           #-- ajuste das variáveis
           pivot_wider(names_from = CO, values_from = renda_media) %>%
           #-- consolidação 
           mutate(Variavel = grupos[j])
         
  )
  
#-- 2º TRIMESTRE
assign(str_glue('rendimento_2t_{ grupos[ j ] }'),
       
       painel_pnadc_2t %>%
         #-- base  
         filter(CO %in% c('Informal', 'Formal')) %>%
         #-- grupo de estimação
         group_by(Ano, Trimestre, get(grupos[j]), CO) %>%
         #-- estimação
         summarise(renda_media = survey_mean(VD4017, 
                                             na.rm = TRUE, 
                                             vartype = NULL)) %>% 
         #-- ajuste das variáveis
         pivot_wider(names_from = CO, values_from = renda_media) %>%
         #-- consolidação 
         mutate(Variavel = grupos[j])
       
)

#-- 3º TRIMESTRE
assign(str_glue('rendimento_3t_{grupos[ j ]}'),
       
       painel_pnadc_3t %>%
         #-- base  
         filter(CO %in% c('Informal', 'Formal')) %>%
         #-- grupo de estimação
         group_by(Ano, Trimestre, get(grupos[j]), CO) %>%
         #-- estimação
         summarise(renda_media = survey_mean(VD4017, 
                                             na.rm = TRUE, 
                                             vartype = NULL)) %>% 
         #-- ajuste das variáveis
         pivot_wider(names_from = CO, values_from = renda_media) %>%
         #-- consolidação 
         mutate(Variavel = grupos[j])
       
)

#-- 4º TRIMESTRE
assign(str_glue('rendimento_4t_{grupos[ j ]}'),
       
       painel_pnadc_4t %>%
         #-- base  
         filter(CO %in% c('Informal', 'Formal')) %>%
         #-- grupo de estimação
         group_by(Ano, Trimestre, get(grupos[j]), CO) %>%
         #-- estimação
         summarise(renda_media = survey_mean(VD4017, 
                                             na.rm = TRUE, 
                                             vartype = NULL)) %>% 
         #-- ajuste das variáveis
         pivot_wider(names_from = CO, values_from = renda_media) %>%
         #-- consolidação 
         mutate(Variavel = grupos[j])
       
)



  
}

# ! SALVAR E CONSOLIDAR BASE ---------------------------------------------------


for(i in c(1,7)) {
  
  
  assign(str_glue('df.com_20{15+i}'),
         read_rds(str_glue('df.com_20{15+i}.RDS'))
  )
  
#  df.consolidada = rbind(df_a) %>%
#    pivot_wider(names_from = 'Ano', values_from = c('S_informal', 'S_formal', 'Proporção'))

}

rm(list=ls(pattern="df"))


  
  df.consolidada = rbind(df.com_2016, df.com_2022) %>%
    pivot_wider(names_from = 'regiao', values_from = c('S_informal', 'S_formal', 'Proporção'),
                id_expand = F) #%>% 
    unnest(cols = S_informal_Brasil:Proporção_Piauí)

  
  write_xlsx(
    df.consolidada,
    "Processados/df.consolidada.xlsx",
    col_names = TRUE,
    format_headers = TRUE
  )
  
  assign(str_glue('df.com_2016'),
         read_rds(str_glue('df.com_2016.RDS'))
  )
  assign(str_glue('df.com_2022'),
         read_rds(str_glue('df.com_2022.RDS'))
  )
  