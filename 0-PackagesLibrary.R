#Instalação dos pacotes
pacotes <- c("tidytext",
             "flextable",
             "ggplot2",
             "kableExtra",
             "dplyr",
             "tibble",
             "gutenbergr",
             "wordcloud",
             "stringr",
             "SnowballC",
             "widyr",
             "qcc",
             "janitor")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

#Chamada de bibliotecas
library("dplyr")
library("tidytext")
library("ggplot2")
library("tibble")
library("readxl")
library("qcc")
library("kableExtra")
library("flextable")


citation("janitor")
