#Atribui todos comentários dos cartões do Kanban para variável "del"
del <- data.frame(Delay = c(fRPA$IsDelayed), Text = c(fRPA$TextDiscussion))
del$Delay[is.na(del$Delay)] <- 0

#63 projetos ***
#Deleta todos comentários de cartões do Kanban de projetos entregues no prazo
for (x in 1:63) {
  del <- del[del$Delay == 0 , ]
}

#43 projetos ***
#Atribui comentários de cartões do Kanban para um data frame
text <- del$Text
text_df <- tibble(line = 1:43, text = text)


#Muda o encoding para ASCII
Encoding(text) <- "ASCII"

#Desmembra as frases em palavras únicas
allWords <- text_df %>%
  unnest_tokens(word, text)


# # #  DATA WRANGLING # # #

#29927 observações ***
#Remove stopwords
#Resultado = 1348 linhas removidas
df_sem_stop_words <- allWords %>% anti_join(stop_words) 

#28579 observações ***
#Resultado = 924 linhas removidas
df_sem_stop_words <- df_sem_stop_words %>% anti_join(get_stopwords(language = 'pt'))

#27655 observações ***
#Contagem por frequência de palavras
conta <- df_sem_stop_words %>%  count(word, sort = TRUE) 
conta

#2164 observações ***
#Remover valores menores ou igual a 1
#Resultado = 1099 linhas removidas
for (x in 1:2164) {
  conta <- conta[conta$n > 1 , ]
}


#1065 observações ***
#Remove observações do tipo número 
#Resultado = 176 linhas removidas
conta$word <- gsub("[0-9]+", "", conta$word)
for (x in 1:1065) {
  conta <- conta[conta$word != "" , ]
}

#889 observações ***
#Remove observações do tipo caractere especial (virgula, ponto, dois pontos) 
#Resultado =  1 linha removida
for (x in 1:889) {
  conta <- conta[conta$word != "," , ]
}
conta


#888 observações ***
#Resultado = 1 linha removida
for (x in 1:888) {
  conta <- conta[conta$word != ".," , ]
}
conta

#887 observações ***
#Resultado = 23 linha removida
for (x in 1:887) {
  #1000 observações 
  vExist <- str_detect(conta$word[x], ":")
  conta$exist[x] <- c(vExist)
}

for (x in 1:887) {
  conta <- conta[conta$exist == FALSE , ]
}

#864 observações ***
#Resultado = 7 linha removida
for (x in 1:864) {
  vExist <- str_detect(conta$word[x], ",")
  conta$exist[x] <- c(vExist)
}

for (x in 1:864) {
  conta <- conta[conta$exist == FALSE , ]
}

conta


#857 observações ***
#Resultado = nenhuma linha removida
for (x in 1:857) {
  vExist <- str_detect(conta$word[x], "&")
  conta$exist[x] <- c(vExist)
}

for (x in 1:857) {
  conta <- conta[conta$exist == FALSE , ]
}

conta

#857 observações ***
#Conta quantos caracteres existem em cada palavra
for (x in 1:857) {
  vNum <- str_length(conta$word[x]) #1000 observações
  conta$cw[x] <- c(vNum)
  
}

summary(conta$cw)

#remove observações com menos de 3 caracteres (1° quartil)
#Resultado = 169 linhas removidas
for (x in 1:857) {
  conta <- conta[conta$cw > 2 , ]
}
conta


#688 observações ***
#Resultado =  linha removida
#remove observações com mais de 8 caracteres  (3° quartil)
for (x in 1:688) {
  vNum <- str_length(conta$word[x]) #1000 observações
  conta$cw[x] <- c(vNum)
  
}

for (x in 1:688) {
  conta <- conta[conta$cw < 9 , ]
}

conta


#540 observações ***
#Remover valores com textos que não agregam sentido a pesquisa
#Resultado = 150 linhas removidas
dfw <- data.frame(
  swn = c(
    "ad.ad",
    "fdca",
    "revision",
    "add",
    "data",
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
    "https",
    "_apis",
    "tfsAMS",
    "_apis",
    "div",
    "url",
    "href",
    "totalCount",
    "fromRevisionCount",
    "count",
    "tfsams",
    "comments",
    "baeca",
    "text",
    "wit",
    "_links",
    "abb",
    "avatar",
    "dbdbccc",
    "imageurl",
    "span",
    "nbsp",
    "style",
    "dec",
    "edcd",
    "mention",
    "box",
    "caac",
    "ffffff",
    "version",
    "vss",
    "quot",
    "true",
    "font",
    "inactive",
    "ceecb",
    "bcd",
    "cbbe",
    "count",
    "dia",
    "margin",
    "dbf",
    "fab",
    "fbd",
    "bddbcb",
    "bff",
    "serif",
    "cab",
    "eae",
    "eebf",
    "ada",
    "fcd",
    "dcf",
    "date",
    "dbc",
    "ecea",
    "sans",
    "alt",
    "family",
    "filename",
    "image",
    "img",
    "padding",
    "src",
    "dff",
    "flex",
    "week",
    "ceeaf",
    "emoji",
    "fdbfa",
    "abbc",
    "acc",
    "baaef",
    "border",
    "cbca",
    "dce",
    "desde",
    "edit",
    "efa",
    "favor",
    "parte",
    "pois",
    "segoe",
    "ser",
    "tabindex",
    "solid",
    "height",
    "ainda",
    "size",
    "width",
    ""
    
  )
)

for (x in 1:222) {
  conta <- conta[conta$word != dfw$swn[x] , ]
}
conta

#390 observações ***
#Valida quem é o 3º quartil (frequência de "word"), ele será a primeira linha de corte da amostra: 7 é o primeiro quartil
summary(conta$n) 

#Remove valores menores que 6 
#Resultado = 299 linhas removidas
for (x in 1:390) {
  conta <- conta[conta$n > 6 , ]
}
conta

#91 observações ***

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
    accentTypes <- c("´","`","^","~","¨","ç",".","_","	_")
    if(any(c("all","al","a","todos","t","to","tod","todo")%in%pattern)) # opcao retirar todos
      return(chartr(paste(symbols, collapse=""), paste(nudeSymbols, collapse=""), str))
    for(i in which(accentTypes%in%pattern))
      str <- chartr(symbols[i],nudeSymbols[i], str)
    return(str)
  }

conta <- rm_accent(conta$word)
conta <- data.frame(conta)


#salvar observações em ".csv"
write.csv(conta,"~/TCC/ContaF.csv", row.names = FALSE)

#Realizar ajuste manual no arquivo ".csv", categorizando cada palavra dentro dos 6 M's do Diagrama de Ishikawa (Máquina, Materiais, Mão de obra, Meio ambiente, Método, Medida) com variáveis "Dummy" (0 ou)
# 1º - Concaternar palavras que são nomes completos dos colaboradores: 38 palavras concatenadas -> 16 novas palavras + 51 palavras = 67 palavras
# 2° - Criar 6 colunas: MACHINE,MATERIAL,LABOR, ENVIRONMENT,METHOD,MEASURE -> 
# 3° - Assinalar 1 caso seja uma palavra da categoria da coluna, ou 0 caso não seja
# 4° - Salvar como um novo arquvio CSV -> ContaFok.csv

# # #  DATA VISUALIZATION # # #

#Carrega a base de dados, extraída do Kanban
dbFreq <- read.csv("~/TCC/ContaFok.csv")

#Converte "Excel" para "RData"
save.image("~/TCC/dbFreq.RData")

dbFreq

#Troca "NA" por zero
dbFreq[is.na(dbFreq)] <- 0

#Deleta a coluna "WORD"
#dbFreq$WORD <- NULL

#Pareto
fMaquina <- sum(dbFreq$MACHINE)
fMateriais <- sum(dbFreq$MATERIAL)
fMaoObra <- sum(dbFreq$LABOR)
fMeioAmb <- sum(dbFreq$ENVIRONMENT)
fMetodo <- sum(dbFreq$METHOD)
fMedidas <- sum(dbFreq$MEASURE)

FP <- c(fMaquina, fMateriais, fMaoObra, fMeioAmb, fMetodo, fMedidas)

cn <- c(fMaquina, fMateriais, fMaoObra, fMeioAmb, fMetodo, fMedidas)
names(cn) <- c("Máquina", "Materiais", "Mão de obra", "Meio ambiente", "Método", "Medidas")


pareto.chart(cn)

sink("~/TCC/FrqTab.csv")# Create empty csv file
pareto.chart(cn,
             main='Prováveis Causas vs. Projetos atrasados',
             ylab = 'Frequência',
             ylab2 = "Porcentagem acumulada",
             col=heat.colors(12))

sink()# Close connection to file


#Realiza ajuste manual, para adequar os dados para uma base de dados no R
# 1º - Tabula e adiciona virgula entre os dados
# 2° - Salva como um novo CSV: "FrqTabCSV.csv"

#Carrega a base de dados, extraída do Kanban
tabFreq <- read.csv("~/TCC/FrqTabCSV.csv")

#Converte "Excel" para "RData"
save.image("~/TCC/FrqTab.RData")

tabFreq


#Cria uma visualização de tabela para impressão

cnTab <- c( 
     "Prováveis Causas",
     "Fi",
     "Fri (%)",
     "Fac",
     "Fac(%)")

colnames(tabFreq) <- cnTab

tabFreq %>%
  
  kable() %>%

  kable_styling(bootstrap_options = "striped",
                full_width = F, 
                font_size = 12)




#### Procurar as palavras mais comuns nos cartões "Atrasados" ###
#Criar as variáveis para cada palavras mais frequente nos comentários
w1 <-   dbFreq$WORD[1]
w2 <-   dbFreq$WORD[2]
w3 <-   dbFreq$WORD[3]
w4 <-   dbFreq$WORD[4]
w5 <-   dbFreq$WORD[5]
w6 <-   dbFreq$WORD[6]
w7 <-   dbFreq$WORD[7]
w8 <-   dbFreq$WORD[8]
w9 <-   dbFreq$WORD[9]
w10 <-  dbFreq$WORD[10]
w11 <-  dbFreq$WORD[11]
w12 <-  dbFreq$WORD[12]
w13 <-  dbFreq$WORD[13]
w14 <-  dbFreq$WORD[14]
w15 <-  dbFreq$WORD[15]
w16 <-  dbFreq$WORD[16]
w17 <-  dbFreq$WORD[17]
w18 <-  dbFreq$WORD[18]
w19 <-  dbFreq$WORD[19]
w20 <-  dbFreq$WORD[20]
w21 <-  dbFreq$WORD[21]
w22 <-  dbFreq$WORD[22]
w23 <-  dbFreq$WORD[23]
w24 <-  dbFreq$WORD[24]
w25 <-  dbFreq$WORD[25]
w26 <-  dbFreq$WORD[26]
w27 <-  dbFreq$WORD[27]
w28 <-  dbFreq$WORD[28]
w29 <-  dbFreq$WORD[29]
w30 <-  dbFreq$WORD[30]
w31 <-  dbFreq$WORD[31]
w32 <-  dbFreq$WORD[32]
w33 <-  dbFreq$WORD[33]
w34 <-  dbFreq$WORD[34]
w35 <-  dbFreq$WORD[35]
w36 <-  dbFreq$WORD[36]
w37 <-  dbFreq$WORD[37]
w38 <-  dbFreq$WORD[38]
w39 <-  dbFreq$WORD[39]
w40 <-  dbFreq$WORD[40]
w41 <-  dbFreq$WORD[41]
w42 <-  dbFreq$WORD[42]
w43 <-  dbFreq$WORD[43]
w44 <-  dbFreq$WORD[44]
w45 <-  dbFreq$WORD[45]
w46 <-  dbFreq$WORD[46]
w47 <-  dbFreq$WORD[47]
w48 <-  dbFreq$WORD[48]
w49 <-  dbFreq$WORD[49]
w50 <-  dbFreq$WORD[50]
w51 <-  dbFreq$WORD[51]
w52 <-  dbFreq$WORD[52]
w53 <-  dbFreq$WORD[53]
w54 <-  dbFreq$WORD[54]
w55 <-  dbFreq$WORD[55]
w56 <-  dbFreq$WORD[56]
w57 <-  dbFreq$WORD[57]
w58 <-  dbFreq$WORD[58]
w59 <-  dbFreq$WORD[59]
w60 <-  dbFreq$WORD[60]
w61 <-  dbFreq$WORD[61]
w62 <-  dbFreq$WORD[62]
w63 <-  dbFreq$WORD[63]
w64 <-  dbFreq$WORD[64]
w65 <-  dbFreq$WORD[65]
w66 <-  dbFreq$WORD[66]
w67 <-  dbFreq$WORD[67]
# w68 <-  dbFreq$WORD[68]
# w69 <-  dbFreq$WORD[69]
# w70 <-  dbFreq$WORD[70]
# w71 <-  dbFreq$WORD[71]
# w72 <-  dbFreq$WORD[72]
# w73 <-  dbFreq$WORD[73]
# w74 <-  dbFreq$WORD[74]
# w75 <-  dbFreq$WORD[75]
# w76 <-  dbFreq$WORD[76]
# w77 <-  dbFreq$WORD[77]
# w78 <-  dbFreq$WORD[78]
# w79 <-  dbFreq$WORD[79]
# w80 <-  dbFreq$WORD[80]
# w81 <-  dbFreq$WORD[81]
# w82 <-  dbFreq$WORD[82]
# w83 <-  dbFreq$WORD[83]
# w84 <-  dbFreq$WORD[84]
# w85 <-  dbFreq$WORD[85]
# w86 <-  dbFreq$WORD[86]
# w87 <-  dbFreq$WORD[87]
# w88 <-  dbFreq$WORD[88]
# w89 <-  dbFreq$WORD[89]
# w90 <-  dbFreq$WORD[90]
# w91 <-  dbFreq$WORD[91]

#Criar as colunas para cada palavras mais frequente nos comentários
del$w1 <- ""
del$w2 <- ""
del$w3 <- ""
del$w4 <- ""
del$w5 <- ""
del$w6 <- ""
del$w7 <- ""
del$w8 <- ""
del$w9 <- ""
del$w10 <- ""
del$w11 <- ""
del$w12 <- ""
del$w13 <- ""
del$w14 <- ""
del$w15 <- ""
del$w16 <- ""
del$w17 <- ""
del$w18 <- ""
del$w19 <- ""
del$w20 <- ""
del$w21 <- ""
del$w22 <- ""
del$w23 <- ""
del$w24 <- ""
del$w25 <- ""
del$w26 <- ""
del$w27 <- ""
del$w28 <- ""
del$w29 <- ""
del$w30 <- ""
del$w31 <- ""
del$w32 <- ""
del$w33 <- ""
del$w34 <- ""
del$w35 <- ""
del$w36 <- ""
del$w37 <- ""
del$w38 <- ""
del$w39 <- ""
del$w40 <- ""
del$w41 <- ""
del$w42 <- ""
del$w43 <- ""
del$w44 <- ""
del$w45 <- ""
del$w46 <- ""
del$w47 <- ""
del$w48 <- ""
del$w49 <- ""
del$w50 <- ""
del$w51 <- ""
del$w52 <- ""
del$w53 <- ""
del$w54 <- ""
del$w55 <- ""
del$w56 <- ""
del$w57 <- ""
del$w58 <- ""
del$w59 <- ""
del$w60 <- ""
del$w61 <- ""
del$w62 <- ""
del$w63 <- ""
del$w64 <- ""
del$w65 <- ""
del$w66 <- ""
del$w67 <- ""
# del$w68 <- ""
# del$w69 <- ""
# del$w70 <- ""
# del$w71 <- ""
# del$w72 <- ""
# del$w73 <- ""
# del$w74 <- ""
# del$w75 <- ""
# del$w76 <- ""
# del$w77 <- ""
# del$w78 <- ""
# del$w79 <- ""
# del$w80 <- ""
# del$w81 <- ""
# del$w82 <- ""
# del$w83 <- ""
# del$w84 <- ""
# del$w85 <- ""
# del$w86 <- ""
# del$w87 <- ""
# del$w88 <- ""
# del$w89 <- ""
# del$w90 <- ""
# del$w91 <- ""


#Pesquisa dentro de cada uma das células dos 43 projetos de RPA atrasados se existe a incidendencia de uma palavra que é frequente em alguns comentários dos projetos
for (x in 1:43){

  del$w1 <- grepl(w1, del$Text[x])
  del$w2 <- grepl(w2, del$Text[x])
  del$w3 <- grepl(w3, del$Text[x])
  del$w4 <- grepl(w4, del$Text[x])
  del$w5 <- grepl(w5, del$Text[x])
  del$w6 <- grepl(w6, del$Text[x])
  del$w7 <- grepl(w7, del$Text[x])
  del$w8 <- grepl(w8, del$Text[x])
  del$w9 <- grepl(w9, del$Text[x])
  del$w10 <- grepl(w10, del$Text[x])
  del$w11 <- grepl(w11, del$Text[x])
  del$w12 <- grepl(w12, del$Text[x])
  del$w13 <- grepl(w13, del$Text[x])
  del$w14 <- grepl(w14, del$Text[x])
  del$w15 <- grepl(w15, del$Text[x])
  del$w16 <- grepl(w16, del$Text[x])
  del$w17 <- grepl(w17, del$Text[x])
  del$w18 <- grepl(w18, del$Text[x])
  del$w19 <- grepl(w19, del$Text[x])
  del$w20 <- grepl(w20, del$Text[x])
  del$w21 <- grepl(w21, del$Text[x])
  del$w22 <- grepl(w22, del$Text[x])
  del$w23 <- grepl(w23, del$Text[x])
  del$w24 <- grepl(w24, del$Text[x])
  del$w25 <- grepl(w25, del$Text[x])
  del$w26 <- grepl(w26, del$Text[x])
  del$w27 <- grepl(w27, del$Text[x])
  del$w28 <- grepl(w28, del$Text[x])
  del$w29 <- grepl(w29, del$Text[x])
  del$w30 <- grepl(w30, del$Text[x])
  del$w31 <- grepl(w31, del$Text[x])
  del$w32 <- grepl(w32, del$Text[x])
  del$w33 <- grepl(w33, del$Text[x])
  del$w34 <- grepl(w34, del$Text[x])
  del$w35 <- grepl(w35, del$Text[x])
  del$w36 <- grepl(w36, del$Text[x])
  del$w37 <- grepl(w37, del$Text[x])
  del$w38 <- grepl(w38, del$Text[x])
  del$w39 <- grepl(w39, del$Text[x])
  del$w40 <- grepl(w40, del$Text[x])
  del$w41 <- grepl(w41, del$Text[x])
  del$w42 <- grepl(w42, del$Text[x])
  del$w43 <- grepl(w43, del$Text[x])
  del$w44 <- grepl(w44, del$Text[x])
  del$w45 <- grepl(w45, del$Text[x])
  del$w46 <- grepl(w46, del$Text[x])
  del$w47 <- grepl(w47, del$Text[x])
  del$w48 <- grepl(w48, del$Text[x])
  del$w49 <- grepl(w49, del$Text[x])
  del$w50 <- grepl(w50, del$Text[x])
  del$w51 <- grepl(w51, del$Text[x])
  del$w52 <- grepl(w52, del$Text[x])
  del$w53 <- grepl(w53, del$Text[x])
  del$w54 <- grepl(w54, del$Text[x])
  del$w55 <- grepl(w55, del$Text[x])
  del$w56 <- grepl(w56, del$Text[x])
  del$w57 <- grepl(w57, del$Text[x])
  del$w58 <- grepl(w58, del$Text[x])
  del$w59 <- grepl(w59, del$Text[x])
  del$w60 <- grepl(w60, del$Text[x])
  del$w61 <- grepl(w61, del$Text[x])
  del$w62 <- grepl(w62, del$Text[x])
  del$w63 <- grepl(w63, del$Text[x])
  del$w64 <- grepl(w64, del$Text[x])
  del$w65 <- grepl(w65, del$Text[x])
  del$w66 <- grepl(w66, del$Text[x])
  del$w67 <- grepl(w67, del$Text[x])
  # del$w68 <- grepl(w68, del$Text[x])
  # del$w69 <- grepl(w69, del$Text[x])
  # del$w70 <- grepl(w70, del$Text[x])
  # del$w71 <- grepl(w71, del$Text[x])
  # del$w72 <- grepl(w72, del$Text[x])
  # del$w73 <- grepl(w73, del$Text[x])
  # del$w74 <- grepl(w74, del$Text[x])
  # del$w75 <- grepl(w75, del$Text[x])
  # del$w76 <- grepl(w76, del$Text[x])
  # del$w77 <- grepl(w77, del$Text[x])
  # del$w78 <- grepl(w78, del$Text[x])
  # del$w79 <- grepl(w79, del$Text[x])
  # del$w80 <- grepl(w80, del$Text[x])
  # del$w81 <- grepl(w81, del$Text[x])
  # del$w82 <- grepl(w82, del$Text[x])
  # del$w83 <- grepl(w83, del$Text[x])
  # del$w84 <- grepl(w84, del$Text[x])
  # del$w85 <- grepl(w85, del$Text[x])
  # del$w86 <- grepl(w86, del$Text[x])
  # del$w87 <- grepl(w87, del$Text[x])
  # del$w88 <- grepl(w88, del$Text[x])
  # del$w89 <- grepl(w89, del$Text[x])
  # del$w90 <- grepl(w90, del$Text[x])
  # del$w91 <- grepl(w91, del$Text[x])
  
}


#Renomear TRUE para 1 e FALSE para 0
del$w1 <- as.integer(del$w1)
del$w2 <- as.integer(del$w2)
del$w3 <- as.integer(del$w3)
del$w4 <- as.integer(del$w4)
del$w5 <- as.integer(del$w5)
del$w6 <- as.integer(del$w6)
del$w7 <- as.integer(del$w7)
del$w8 <- as.integer(del$w8)
del$w9 <- as.integer(del$w9)
del$w10 <- as.integer(del$w10)
del$w11 <- as.integer(del$w11)
del$w12 <- as.integer(del$w12)
del$w13 <- as.integer(del$w13)
del$w14 <- as.integer(del$w14)
del$w15 <- as.integer(del$w15)
del$w16 <- as.integer(del$w16)
del$w17 <- as.integer(del$w17)
del$w18 <- as.integer(del$w18)
del$w19 <- as.integer(del$w19)
del$w20 <- as.integer(del$w20)
del$w21 <- as.integer(del$w21)
del$w22 <- as.integer(del$w22)
del$w23 <- as.integer(del$w23)
del$w24 <- as.integer(del$w24)
del$w25 <- as.integer(del$w25)
del$w26 <- as.integer(del$w26)
del$w27 <- as.integer(del$w27)
del$w28 <- as.integer(del$w28)
del$w29 <- as.integer(del$w29)
del$w30 <- as.integer(del$w30)
del$w31 <- as.integer(del$w31)
del$w32 <- as.integer(del$w32)
del$w33 <- as.integer(del$w33)
del$w34 <- as.integer(del$w34)
del$w35 <- as.integer(del$w35)
del$w36 <- as.integer(del$w36)
del$w37 <- as.integer(del$w37)
del$w38 <- as.integer(del$w38)
del$w39 <- as.integer(del$w39)
del$w40 <- as.integer(del$w40)
del$w41 <- as.integer(del$w41)
del$w42 <- as.integer(del$w42)
del$w43 <- as.integer(del$w43)
del$w44 <- as.integer(del$w44)
del$w45 <- as.integer(del$w45)
del$w46 <- as.integer(del$w46)
del$w47 <- as.integer(del$w47)
del$w48 <- as.integer(del$w48)
del$w49 <- as.integer(del$w49)
del$w50 <- as.integer(del$w50)
del$w51 <- as.integer(del$w51)
del$w52 <- as.integer(del$w52)
del$w53 <- as.integer(del$w53)
del$w54 <- as.integer(del$w54)
del$w55 <- as.integer(del$w55)
del$w56 <- as.integer(del$w56)
del$w57 <- as.integer(del$w57)
del$w58 <- as.integer(del$w58)
del$w59 <- as.integer(del$w59)
del$w60 <- as.integer(del$w60)
del$w61 <- as.integer(del$w61)
del$w62 <- as.integer(del$w62)
del$w63 <- as.integer(del$w63)
del$w64 <- as.integer(del$w64)
del$w65 <- as.integer(del$w65)
del$w66 <- as.integer(del$w66)
del$w67 <- as.integer(del$w67)
# del$w68 <- as.integer(del$w68)
# del$w69 <- as.integer(del$w69)
# del$w70 <- as.integer(del$w70)
# del$w71 <- as.integer(del$w71)
# del$w72 <- as.integer(del$w72)
# del$w73 <- as.integer(del$w73)
# del$w74 <- as.integer(del$w74)
# del$w75 <- as.integer(del$w75)
# del$w76 <- as.integer(del$w76)
# del$w77 <- as.integer(del$w77)
# del$w78 <- as.integer(del$w78)
# del$w79 <- as.integer(del$w79)
# del$w80 <- as.integer(del$w80)
# del$w81 <- as.integer(del$w81)
# del$w82 <- as.integer(del$w82)
# del$w83 <- as.integer(del$w83)
# del$w84 <- as.integer(del$w84)
# del$w85 <- as.integer(del$w85)
# del$w86 <- as.integer(del$w86)
# del$w87 <- as.integer(del$w87)
# del$w88 <- as.integer(del$w88)
# del$w89 <- as.integer(del$w89)
# del$w90 <- as.integer(del$w90)
# del$w91 <- as.integer(del$w91)


#Criar a coluna para armazenar a quantidade de palavras frequentes em todos os projetos, nos projetos atrasados
del$wCount <- ""

for (x in 1:43){
  del$wCount[x] <- sum(
    del$w1[x],
    del$w2[x],
    del$w3[x],
    del$w4[x],
    del$w5[x],
    del$w6[x],
    del$w7[x],
    del$w8[x],
    del$w9[x],
    del$w10[x],
    del$w11[x],
    del$w12[x],
    del$w13[x],
    del$w14[x],
    del$w15[x],
    del$w16[x],
    del$w17[x],
    del$w18[x],
    del$w19[x],
    del$w20[x],
    del$w21[x],
    del$w22[x],
    del$w23[x],
    del$w24[x],
    del$w25[x],
    del$w26[x],
    del$w27[x],
    del$w28[x],
    del$w29[x],
    del$w30[x],
    del$w31[x],
    del$w32[x],
    del$w33[x],
    del$w34[x],
    del$w35[x],
    del$w36[x],
    del$w37[x],
    del$w38[x],
    del$w39[x],
    del$w40[x],
    del$w41[x],
    del$w42[x],
    del$w43[x],
    del$w44[x],
    del$w45[x],
    del$w46[x],
    del$w47[x],
    del$w48[x],
    del$w49[x],
    del$w50[x],
    del$w51[x],
    del$w52[x],
    del$w53[x],
    del$w54[x],
    del$w55[x],
    del$w56[x],
    del$w57[x],
    del$w58[x],
    del$w59[x],
    del$w60[x],
    del$w61[x],
    del$w62[x],
    del$w63[x],
    del$w64[x],
    del$w65[x],
    del$w66[x],
    del$w67[x]
    # del$w68[x],
    # del$w69[x],
    # del$w70[x],
    # del$w71[x],
    # del$w72[x],
    # del$w73[x],
    # del$w74[x],
    # del$w75[x],
    # del$w76[x],
    # del$w77[x],
    # del$w78[x],
    # del$w79[x],
    # del$w80[x],
    # del$w81[x],
    # del$w82[x],
    # del$w83[x],
    # del$w84[x],
    # del$w85[x],
    # del$w86[x],
    # del$w87[x],
    # del$w88[x],
    # del$w89[x],
    # del$w90[x],
    # del$w91[x]
                    )
}

#Deletar colunas "w" caso não possua valores "1"
sw1 <- sum(del$w1)
sw2 <- sum(del$w2)
sw3 <- sum(del$w3)
sw4 <- sum(del$w4)
sw5 <- sum(del$w5)
sw6 <- sum(del$w6)
sw7 <- sum(del$w7)
sw8 <- sum(del$w8)
sw9 <- sum(del$w9)
sw10 <- sum(del$w10)
sw11 <- sum(del$w11)
sw12 <- sum(del$w12)
sw13 <- sum(del$w13)
sw14 <- sum(del$w14)
sw15 <- sum(del$w15)
sw16 <- sum(del$w16)
sw17 <- sum(del$w17)
sw18 <- sum(del$w18)
sw19 <- sum(del$w19)
sw20 <- sum(del$w20)
sw21 <- sum(del$w21)
sw22 <- sum(del$w22)
sw23 <- sum(del$w23)
sw24 <- sum(del$w24)
sw25 <- sum(del$w25)
sw26 <- sum(del$w26)
sw27 <- sum(del$w27)
sw28 <- sum(del$w28)
sw29 <- sum(del$w29)
sw30 <- sum(del$w30)
sw31 <- sum(del$w31)
sw32 <- sum(del$w32)
sw33 <- sum(del$w33)
sw34 <- sum(del$w34)
sw35 <- sum(del$w35)
sw36 <- sum(del$w36)
sw37 <- sum(del$w37)
sw38 <- sum(del$w38)
sw39 <- sum(del$w39)
sw40 <- sum(del$w40)
sw41 <- sum(del$w41)
sw42 <- sum(del$w42)
sw43 <- sum(del$w43)
sw44 <- sum(del$w44)
sw45 <- sum(del$w45)
sw46 <- sum(del$w46)
sw47 <- sum(del$w47)
sw48 <- sum(del$w48)
sw49 <- sum(del$w49)
sw50 <- sum(del$w50)
sw51 <- sum(del$w51)
sw52 <- sum(del$w52)
sw53 <- sum(del$w53)
sw54 <- sum(del$w54)
sw55 <- sum(del$w55)
sw56 <- sum(del$w56)
sw57 <- sum(del$w57)
sw58 <- sum(del$w58)
sw59 <- sum(del$w59)
sw60 <- sum(del$w60)
sw61 <- sum(del$w61)
sw62 <- sum(del$w62)
sw63 <- sum(del$w63)
sw64 <- sum(del$w64)
sw65 <- sum(del$w65)
sw66 <- sum(del$w66)
sw67 <- sum(del$w67)

if(sw1 < 1){

    del$w1 <- NULL
    
}

if(sw2 < 1){
  
  del$w2 <- NULL
  
}

if(sw3 < 1){
  
  del$w3 <- NULL
  
}

if(sw4 < 1){
  
  del$w4 <- NULL
  
}

if(sw5 < 1){
  
  del$w5 <- NULL
  
}

if(sw6 < 1){
  
  del$w6 <- NULL
  
}

if(sw7 < 1){
  
  del$w7 <- NULL
  
}

if(sw8 < 1){
  
  del$w8 <- NULL
  
}

if(sw9 < 1){
  
  del$w9 <- NULL
  
}

if(sw10 < 1){
  
  del$w10 <- NULL
  
}


if(sw11 < 1){
  
  del$w11 <- NULL
  
}

if(sw12 < 1){
  
  del$w12 <- NULL
  
}

if(sw13 < 1){
  
  del$w13 <- NULL
  
}

if(sw14 < 1){
  
  del$w14 <- NULL
  
}

if(sw15 < 1){
  
  del$w15 <- NULL
  
}

if(sw16 < 1){
  
  del$w16 <- NULL
  
}

if(sw17 < 1){
  
  del$w17 <- NULL
  
}

if(sw18 < 1){
  
  del$w18 <- NULL
  
}

if(sw19 < 1){
  
  del$w19 <- NULL
  
}

if(sw20 < 1){
  
  del$w20 <- NULL
  
}

if(sw21 < 1){
  
  del$w21 <- NULL
  
}

if(sw22 < 1){
  
  del$w22 <- NULL
  
}

if(sw23 < 1){
  
  del$w23 <- NULL
  
}

if(sw24 < 1){
  
  del$w24 <- NULL
  
}

if(sw25 < 1){
  
  del$w25 <- NULL
  
}

if(sw26 < 1){
  
  del$w26 <- NULL
  
}

if(sw27 < 1){
  
  del$w27 <- NULL
  
}

if(sw28 < 1){
  
  del$w28 <- NULL
  
}

if(sw29 < 1){
  
  del$w29 <- NULL
  
}

if(sw30 < 1){
  
  del$w30 <- NULL
  
}

if(sw31 < 1){
  
  del$w31 <- NULL
  
}

if(sw32 < 1){
  
  del$w32 <- NULL
  
}

if(sw33 < 1){
  
  del$w33 <- NULL
  
}

if(sw34 < 1){
  
  del$w34 <- NULL
  
}

if(sw35 < 1){
  
  del$w35 <- NULL
  
}

if(sw36 < 1){
  
  del$w36 <- NULL
  
}

if(sw37 < 1){
  
  del$w37 <- NULL
  
}

if(sw38 < 1){
  
  del$w38 <- NULL
  
}

if(sw39 < 1){
  
  del$w39 <- NULL
  
}

if(sw40 < 1){
  
  del$w40 <- NULL
  
}

if(sw41 < 1){
  
  del$w41 <- NULL
  
}

if(sw42 < 1){
  
  del$w42 <- NULL
  
}

if(sw43 < 1){
  
  del$w43 <- NULL
  
}

if(sw44 < 1){
  
  del$w44 <- NULL
  
}

if(sw45 < 1){
  
  del$w45 <- NULL
  
}

if(sw46 < 1){
  
  del$w46 <- NULL
  
}

if(sw47 < 1){
  
  del$w47 <- NULL
  
}

if(sw48 < 1){
  
  del$w48 <- NULL
  
}

if(sw49 < 1){
  
  del$w49 <- NULL
  
}

if(sw50 < 1){
  
  del$w50 <- NULL
  
}
if(sw51 < 1){
  
  del$w51 <- NULL
  
}

if(sw52 < 1){
  
  del$w52 <- NULL
  
}

if(sw53 < 1){
  
  del$w53 <- NULL
  
}

if(sw54 < 1){
  
  del$w54 <- NULL
  
}

if(sw55 < 1){
  
  del$w55 <- NULL
  
}

if(sw56 < 1){
  
  del$w56 <- NULL
  
}

if(sw57 < 1){
  
  del$w57 <- NULL
  
}

if(sw58 < 1){
  
  del$w58 <- NULL
  
}

if(sw59 < 1){
  
  del$w59 <- NULL
  
}

if(sw60 < 1){
  
  del$w60 <- NULL
  
}

if(sw61 < 1){
  
  del$w61 <- NULL
  
}

if(sw62 < 1){
  
  del$w62 <- NULL
  
}

if(sw63 < 1){
  
  del$w63 <- NULL
  
}


if(sw64 < 1){
  
  del$w64 <- NULL
  
}


if(sw65 < 1){
  
  del$w65 <- NULL
  
}


if(sw66 < 1){
  
  del$w66 <- NULL
  
}



if(sw67 < 1){
  
  del$w67 <- NULL
  
}


#Trocar valores "1" das colunas "w" restantes para as palavras correspondentes


for (x in 1:43){
if(del$w21[x] == "1"){
  
  del$w21[x] <- w21
  
}}

for (x in 1:43){
  if(del$w26[x] == "1"){
    
    del$w26[x] <- w26
    
  }}

for (x in 1:43){
  if(del$w27[x] == "1"){
    
    del$w27[x] <- w27
    
  }}

