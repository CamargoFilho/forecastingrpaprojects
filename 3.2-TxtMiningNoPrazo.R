#Atribui todos comentários dos cartões do Kanban para variável "del"
del <- data.frame(Delay = c(dbRPA$IsDelayed), Text = c(dbRPA$TextDiscussion))
del$Delay[is.na(del$Delay)] <- 0


#Deleta todos comentários de cartões do Kanban de projetos entregues no prazo
for (x in 1:69) {
  del <- del[del$Delay == 1 , ]
}

#Atribui comentários de cartões do Kanban para um data frame
text <- del$Text
text_df <- tibble(line = 1:20, text = text)

#Muda o encoding para ASCII
Encoding(text) <- "ASCII"

#Desmembra as frases em palavras únicas
allWords <- text_df %>%
  unnest_tokens(word, text)


# # #  DATA WRANGLING # # #

#Remove stopwords
df_sem_stop_words <- allWords %>% anti_join(stop_words)
df_sem_stop_words <- df_sem_stop_words %>% anti_join(get_stopwords(language = 'pt'))

#Contagem por frequência de palavras
conta <- df_sem_stop_words %>%  count(word, sort = TRUE) 
conta

#2475 observações ***
#Remover valores menores ou igual a 1
#Resultado = 1582 linhas removidas
for (x in 1:2475) {
  conta <- conta[conta$n > 1 , ]
}

#1163 observações ***
#Valida quem é o 3º quartil, ele será a primeira linha de corte da amostra: 15 é o primeiro quartil
summary(conta) 

#Remove valores maiores que 15 
#Resultado = 294 linhas removidas
for (x in 1:1163) {
  conta <- conta[conta$n < 15 , ]
}

#869 observações ***
#Remove observações do tipo número 
#Resultado = 116 linhas removidas
conta$word <- gsub("[0-9]+", "", conta$word)
for (x in 1:869) {
  conta <- conta[conta$word != "" , ]
}

#753 observações ***
#Remove observações do tipo caractere especial (virgula, ponto, dois pontos) 
#Resultado =  1 linha removida
for (x in 1:753) {
  conta <- conta[conta$word != "," , ]
}
conta


#752 observações ***
#Resultado =  6 linhas removidas
for (x in 1:752) {
  conta <- conta[conta$word != ".," , ]
}
conta

#746 observações ***
#Remover valores com textos que não agregam sentido a pesquisa
#Resultado = 106 linhas removidas
dfw <- data.frame(
  swn = c(
    "ad.ad",
    "cbbe.cbbe",
    "cd.cd",
    "d.d",
    "dff.dff",
    "e.e",
    "efb.efb",
    "faf.faf",
    "fb.fb",
    "fbd.fbd",
    ".in",
    ".pt",
    ".px",
    ".z",
    "_boards",
    "_layouts",
    "_workitems",
    "a",
    "aa",
    "aac",
    "aad.mdazgnmndetmgeymindalweotatmzcmuxnzvmywe",
    "aad.mdcngjlyzgtodexzsyzhltgzzgitnmuzmzmzdjknzux",
    "aad.mdcyzmjkmzmtmwzhyiognilwiyyzmtmddkyjumdmmwyz",
    "aad.mdzkmjezmtatogvhzsmtalwezjqtmdiyzlhotuyju",
    "aad.mguogyyjmtywyocmtnhltgztctnmuwotcxnmeyjcz",
    "aad.mmuyyzkwmzqtowuosnjklwfhztetzduzyyxndeogzi",
    "aad.mrmnzgwowytytljnsnjlmlwjmzjytyjcxmznkzgiyzdi",
    "aad.mzaxngyyjgtztbinsmgyltliogetnzkzmjqotezmgzk",
    "aad.mzkzgiztutyweyyzweltlhotgtodqzdczmmunjvl",
    "aad.mzqyzfknditztqmyyjmltknmmtntrhmjcxnwrhywu",
    "aad.njyzntqywqtnjkmymdmlwjjzdutmdfjntgztrlyi",
    "aad.nzkmjllodgtywizyiyznklthhndatmtrjyjgywungyx",
    "aad.yiwyzeytytmtqysoduzlwfiymmtymfhotllnwymjc",
    "aad.yzexzdvmnjetmwqyizgmltgwmjgtzginzyzmmqytdi",
    "aad.yzqzjcyzmtotgmcyzilweotatzjriyjlhnzfhogi",
    "aad.zjvmztiyymqtyzvjnymtewltgzztityzyngixzdjkztq",
    "aae",
    "aberto",
    "abertura",
    "acordado",
    "aeecb",
    "agora",
    "alguns",
    "amp",
    "apenas",
    "após",
    "atualização",
    "atualizado",
    "busca",
    "buscar",
    "button",
    "calibri",
    "caso",
    "centro",
    "clickable",
    "col",
    "colocar",
    "criada",
    "dar",
    "dbcea",
    "dbe",
    "dcffb",
    "dee",
    "dentro",
    "deve",
    "durante",
    "efb",
    "encontrado",
    "entrar",
    "enviado",
    "envio",
    "existe",
    "feira",
    "feita",
    "feitas",
    "ficou",
    "fim",
    "final",
    "forma",
    "gentileza",
    "hora",
    "http",
    "informar",
    "informou",
    "inicial",
    "iniciar",
    "início",
    "interno",
    "irá",
    "junto",
    "leitura",
    "mesma",
    "necessário",
    "nessa",
    "novamente",
    "novo",
    "novos",
    "outra",
    "partir",
    "pode",
    "precisamos",
    "próxima",
    "radius",
    "referente",
    "rendered",
    "retornar",
    "rgba",
    "roman",
    "segue",
    "seguinte",
    "seguintes",
    "segunda",
    "sobre",
    "spent",
    "start",
    "tarde",
    "ter",
    "todos",
    "total",
    "unicode",
    "utilizado",
    "vai",
    "verificar",
    "voltar",
    "vou",
    ".,ad",
    "px",
    "decoration:underline",
    ""
    
  )
)

for (x in 1:129) {
  conta <- conta[conta$word != dfw$swn[x] , ]
}
conta


#640 observações ***
#remove observações com menos de 6 frequências (3° quartil)
#Resultado = 495 linhas removidas
summary(conta) 
for (x in 1:640) {
  conta <- conta[conta$n > 6 , ]
}
conta


#145 observações ***
#remove observações com menos de 3 caracteres
#Resultado = 24 linhas removidas
for (x in 1:145) {
  vNum <- str_length(conta$word[x])
  conta$cw[x] <- c(vNum)
  
}

for (x in 1:145) {
  conta <- conta[conta$cw > 2 , ]
}
conta


#121 observações ***
#remove observações que contanham caracteres especiais (.,:&)
#Resultado = 6  linhas removidas
for (x in 1:121) {
  vExist <- str_detect(conta$word[x], ":")#1000 observações
  conta$exist[x] <- c(vExist)
}

for (x in 1:121) {
  conta <- conta[conta$exist == FALSE , ]
}

#115 observações ***
#Resultado = 1 linha removida
for (x in 1:115) {
  vExist <- str_detect(conta$word[x], ",")
  conta$exist[x] <- c(vExist)
}

for (x in 1:115) {
  conta <- conta[conta$exist == FALSE , ]
}

conta


#114 observações ***
#Resultado = nenhuma linha removida
for (x in 1:114) {
  vExist <- str_detect(conta$word[x], "&")
  conta$exist[x] <- c(vExist)
}

for (x in 1:114) {
  conta <- conta[conta$exist == FALSE , ]
}

conta

#114 observações ***
#Resultado = nenhuma linha removida
#remove observações com mais de 20 caracteres
for (x in 1:114) {
  vNum <- str_length(conta$word[x]) #1000 observações
  conta$cw[x] <- c(vNum)
  
}

for (x in 1:114) {
  conta <- conta[conta$cw < 19 , ]
}

conta

#remove acentos e outros termos da lingua portuguesa
rm_accent <- function(str,pattern="all") {
  if(!is.character(str))
    str <- as.character(str)
  pattern <- unique(pattern)
  if(any(pattern=="Ç"))
    pattern[pattern=="Ç"] <- "ç"
  symbols <- c(
    acute = "áéíóúÁÉÍÓÚýÝ",
    grave = "àèìòùÀÈÌÒÙ",
    circunflex = "âêîôûÂÊÎÔÛ",
    tilde = "ãõÃÕñÑ",
    umlaut = "äëïöüÄËÏÖÜÿ",
    cedil = "çÇ"
  )
  nudeSymbols <- c(
    acute = "aeiouAEIOUyY",
    grave = "aeiouAEIOU",
    circunflex = "aeiouAEIOU",
    tilde = "aoAOnN",
    umlaut = "aeiouAEIOUy",
    cedil = "cC"
  )
  accentTypes <- c("´","`","^","~","¨","ç")
  if(any(c("all","al","a","todos","t","to","tod","todo")%in%pattern)) # opcao retirar todos
    return(chartr(paste(symbols, collapse=""), paste(nudeSymbols, collapse=""), str))
  for(i in which(accentTypes%in%pattern))
    str <- chartr(symbols[i],nudeSymbols[i], str)
  return(str)
}

conta <- rm_accent(conta$word)
conta <- data.frame(conta)

#114 observações ***
conta

#salvar observações em ".csv"
write.csv(conta,"~/TCC/ContaNoPrazo.csv", row.names = FALSE)

#Realizar ajuste manual no arquivo ".csv", categorizando cada palavra dentro dos 6 M's do Diagrama de Ishikawa (Máquina, Materiais, Mão de obra, Meio ambiente, Método, Medida) com variáveis "Dummy" (0 ou)


# # #  DATA VISUALIZATION # # #

#Carrega a base de dados, extraída do Kanban
dbFreqPrazo <- read.csv("~/TCC/ContaNoPrazo.csv")

#Converte "Excel" para "RData"
save.image("~/TCC/dbFreqPrazo.RData")

dbFreqPrazo

#Troca "NA" por zero
dbFreqPrazo[is.na(dbFreqPrazo)] <- 0

#Deleta a coluna "WORD"
dbFreqPrazo$WORD <- NULL

#Pareto
fpMaquina <- sum(dbFreqPrazo$MACHINE)
fpMateriais <- sum(dbFreqPrazo$MATERIAL)
fpMaoObra <- sum(dbFreqPrazo$LABOR)
fpMeioAmb <- sum(dbFreqPrazo$ENVIROMENT)
fpMetodo <- sum(dbFreqPrazo$METHOD)
fpMedidas <- sum(dbFreqPrazo$MEASURE)

NP <- c(fpMaquina, fpMateriais, fpMaoObra, fpMeioAmb, fpMetodo, fpMedidas)


cnP <- c(fpMaquina, fpMateriais, fpMaoObra, fpMeioAmb, fpMetodo, fpMedidas)
names(cnP) <- c("Máquina", "Materiais", "Mão de obra", "Meio ambiente", "Método", "Medidas")

pareto.chart(cnP,
             main='Prováveis Causas vs. Projetos entregues no prazo',
             ylab = 'Frequência',
             ylab2 = "Porcentagem acumulada",
             col=terrain.colors(12))
