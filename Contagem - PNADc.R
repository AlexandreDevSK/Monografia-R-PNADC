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
ano = 2016
# ! PRÉ-CONFIGS ----------------------------------------------------------------


# NOTA: automatização do Run do script, para todos os anos



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
               'V1028', 'V2007', 'V2009', 'V1023', 'V1022')

#-- variáveis numéricas - conversão
cols.num <- c('V1008', 'V1014','V1028', 'V2007', 'V2009', 'V1023', 'V1022')


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
dados_pnad_1t %<>% select("Ano":"V2009") 
dados_pnad_2t %<>% select("Ano":"V2009")
dados_pnad_3t %<>% select("Ano":"V2009") 
dados_pnad_4t %<>% select("Ano":"V2009") 

#-- Conversão de variáveis numéricas
dados_pnad_1t[cols.num] <- sapply(dados_pnad_1t[cols.num],as.numeric)
dados_pnad_2t[cols.num] <- sapply(dados_pnad_2t[cols.num],as.numeric)
dados_pnad_3t[cols.num] <- sapply(dados_pnad_3t[cols.num],as.numeric)
dados_pnad_4t[cols.num] <- sapply(dados_pnad_4t[cols.num],as.numeric)

gc()
}
system.time(Sys.sleep(5))


# ! CRIAÇÃO DOS PAINÉIS DE DADOS -----------------------------------------------

# ETAPA 3 # 

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
system.time(Sys.sleep(5))
gc()

# ! ESTIMATIVAS COM OS DADOS ---------------------------------------------------

# ETAPA 5 #

#-- objetos a utilizar
for(i in 1:1){
  assign(str_glue('estimativa_br'), data.frame())
  assign(str_glue('estimativa_pi'), data.frame())
  assign(str_glue('estimativa_ne'), data.frame())
}

repeat{

#-- Brasil

for(i in 1:4){
  assign(str_glue('contagem_{i}t_br'), # ! Base original 
         
    mutate(get(str_glue('painel_pnadc_{i}t')) %>%
             #-- grupo de estimação
             group_by(Ano, Trimestre) %>%
             #-- estimação
             summarise(Projeção = survey_total(na.rm = TRUE, 
                                               vartype = "se"),
                       
                       Observações = n())  
             #-- consolidação 
           
           ) %>% mutate(Região = 'Brasil')
    )
#-- consolidando  
  estimativa_br = rbind(estimativa_br, get(str_glue('contagem_{i}t_br')))
#-- clean  
  rm(list=ls(pattern="contagem"))  
  
  system.time(Sys.sleep(3))
  gc()
} 


#-- Nordeste

for(i in 1:4){
  assign(str_glue('contagem_{i}t_ne'), # ! Base original 
         
         mutate(get(str_glue('painel_pnadc_{i}t')) %>%
                  filter(UF %in% c(21:29)) %>%
                  
                  #-- grupo de estimação
                  group_by(Ano, Trimestre) %>%
                  #-- estimação
                  summarise(Projeção = survey_total(vartype = "se"),
                            
                            Observações = n()) 
                  #-- consolidação 
                
                  ) %>% mutate(Região = 'Nordeste')
  )
  
#-- consolidando
  estimativa_ne = rbind(estimativa_ne, get(str_glue('contagem_{i}t_ne')))
#-- clean  
  rm(list=ls(pattern="contagem")) 
  

system.time(Sys.sleep(3))
gc()
}

#-- Estado do Piauí

for(i in 1:4){
  assign(str_glue('contagem_{i}t_pi'), # ! Base original 
         
         mutate(get(str_glue('painel_pnadc_{i}t')) %>%
                  #-- base  
                  filter(UF == 22) %>%
                  #-- grupo de estimação
                  group_by(Ano, Trimestre) %>%
                  #-- estimação
                  summarise(Projeção = survey_total(na.rm = TRUE,
                                                    vartype = "se"),
                            
                            Observações = n())  
                  #-- consolidação 
                
                %>% mutate(Região = 'Piauí')
         ))
#-- consolidando
  estimativa_pi = rbind(estimativa_pi, get(str_glue('contagem_{i}t_pi')))  
#-- clean  
  rm(list=ls(pattern="contagem")) 
  
  system.time(Sys.sleep(3))
  gc()
}

# ! CONSOLIDANDO OS DADOS ------------------------------------------------------

  
if (T) { 
    
  rm(list=ls(pattern="contagem")) 
    
  assign(str_glue('df.estimativa_{ano}'), 
                 rbind(estimativa_br, 
                       estimativa_ne,  
                       estimativa_pi))
    
  saveRDS(get(str_glue('df.estimativa_{ano}')), 
          str_glue('Processados/df.estimativa_{ano}.rds'))
    
    break() 
  }

}
system.time(Sys.sleep(10))

} # no repeat ===     
  
  
  
  
  
  
  
  
  
  
  
  
  
  
ano = ano + 1

if(ano > 2022) {break()}

}
i=1
TAB_consolidada = data.frame()
repeat{
  
  
  assign(str_glue('df.estimativa_20{15+i}'),
         read_rds(str_glue('Processados/df.estimativa_20{15+i}.rds'))
  )


  TAB_consolidada = rbind(TAB_consolidada, get(str_glue('df.estimativa_20{15+i}'))) 

i=1+i

if(i==8){
  
rm(list=ls(pattern="df"))  
write_xlsx(
    TAB_consolidada,
    'Processados/TAB_consolidada.xlsx',
    col_names = TRUE,
    format_headers = TRUE)

break()}
}
  