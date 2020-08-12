# Script para importação dos dados das Tábuas do IBGE
library(readxl)
library(purrr)
library(here)
library(dplyr)

setwd("/cloud/project/Tabuas IBGE")
arquivos <- list.files(pattern = "xlsx?$")

# Feminino
ibge_fem <- map(arquivos, read_excel, sheet=1, range= "A5:D116", col_names = c("idade", "lx", "qx", "ex"))
names(ibge_fem) <-substr(arquivos, 1, 4)

# Masculino
ibge_masc <- map(arquivos, read_excel, sheet=2, range= "A5:D116", col_names = c("idade", "lx", "qx", "ex"))
names(ibge_masc) <- substr(arquivos, 1, 4)

# Ambos Sexos
ibge_ambos <- map(arquivos, read_excel, sheet=3, range= "A5:D116", col_names = c("idade", "lx", "qx", "ex"))
names(ibge_ambos) <- substr(arquivos, 1, 4)


# Exportar
saveRDS(ibge_fem,   file="ibge_fem.Rds")
saveRDS(ibge_masc,  file="ibge_mas.Rds")
saveRDS(ibge_ambos, file="ibge_ambos.Rds")



##
## Converter para o formato 
setwd("/cloud/project/fgb_app/dados")

# Masculino
ibge_mas <- readRDS("ibge_mas.Rds")
ibge_mas <- map2(ibge_mas, names(ibge_mas), ~mutate(., ano = .y, sexo = "Masculino")) 
ibge_mas <- bind_rows(ibge_mas)

# Feminino
ibge_fem <- readRDS("ibge_fem.Rds")
ibge_fem <- map2(ibge_fem, names(ibge_fem), ~mutate(., ano = .y, sexo = "Feminino")) 
ibge_fem <- bind_rows(ibge_fem)

# Juntar as duas bases de dados
ibge <- bind_rows(ibge_fem, ibge_mas)

# Excluir e renomear colunas
ibge <- ibge %>% 
          rename(x = idade) %>% 
          select(-ex)


# Salvar os dados
saveRDS(ibge, file="ibge.Rds")



