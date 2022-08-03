#Carregar a base de dados, extraída do Quadro Kanban do time de RPA
dbRPA <- read_excel("~/TCC/dbRPA.xlsx")

#Converter de "Excel" para "RData"
save.image("~/TCC/dbRPA.RData")

#Soma a quantidade total de projetos em 17 meses
fRPA <- dbRPA %>%
  filter(dbRPA$RealEffort != "NULL" & dbRPA$RealEffort!=0)

allRPA <- dplyr::count(fRPA, wt = NULL, sort = FALSE, name = NULL)
allRPA <- as.numeric(allRPA)

#Soma a quantidade de projetos atrasados
delayRPA <- sum(fRPA$IsDelayed)
delayRPA <- as.numeric(delayRPA)
delayRPA

#Calcula a quantidade de projetos entregues no prazo
ontimeRPA <- allRPA-delayRPA
ontimeRPA

#Cria um gráfico de barras comparativo
data <- data.frame(
  Projeto=c("Não atrasou","Atrasou") ,  
  Quantidade=c(ontimeRPA,delayRPA)
)

ggplot(data, aes(x = Projeto, y = Quantidade, fill = Projeto, label = Quantidade)) +
  geom_bar(stat = "identity") +
  geom_text(size = 5, position = position_stack(vjust = 0.5))+
  coord_flip()
