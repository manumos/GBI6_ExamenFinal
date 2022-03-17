#pregunta 1.1
getwd() #comprobar directorio actual
setwd("/Users/Manuela/Documents/GitHub/GBI6_ExamenFinal/")#definir directorio actual

#pregunta 1.2
df<- read.csv("data/mRNA_expr.csv")

#pregunta 1.3
#definir funcion en base a un vector que contenga los argumentos
long_df <- function(a,b,c,d,e) {
  y=a %>% pivot_longer(!c(b,c),names_to =d, values_to = e)
  return(y)
}
#pregunta 1.4
df_long <- long_df(df,"bcr_patient_barcode","dataset","gen","expresion_level") %>% select(!(bcr_patient_barcode))

#pregunta 1.5
library(dplyr)
df_long %>%
  group_by(dataset,gen)%>%
  descr(out="viewer",file="results/mRNA_expr_summary.doc")
#pregunta 1.6
#interpretacion
#pregunta 2.1
library("ggplot2")
tcga_boxplots<- function (x) {
    st<-df_long[df_long$dataset==x,]
    grafica=ggplot(st,aes(dataset,expresion_level)) +
    geom_boxplot() + geom_jitter(alpha=0.2, size = 0.2)+
    labs(x = "Genes",y = "Nivel de expresion",title="Nivel de expresion tipos de cancer")+
    theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold"))
    return(grafica)
}
#prueba
tcga_boxplots('KIPAN')

#pregunta 2.2
sets<-c('BRCA','KIPAN','KIRP','LUSC','OV','UCEC')
cancertype_boxplots<-lapply(sets,tcga_boxplots)  
cancertype_boxplots[3]

#pregunta 2.3
KIRP_plot<-cancertype_boxplots[3]
plot.new()
KIRP_plot
ggsave("boxplot3.png",plot=KIRP_plot,device="png",path="results",width=10, height=10)

#pregunta 2.4
genes<- c('GATA3','PTEN','XBP1','ESR1','MUC1','FN1','GAPDH')
reg_gen_expression<- function(G) {
  elem<-c(1:7)#lista del 1 al 7
  pares<-combn(elem,m=2,simplify=F)# combinaciones posibles de 7 elementos
  df_par<-data.frame(t(sapply(pares,c)))#dataframe de la combinaciones
  fila<-c(1:21)#total combinaciones
  for (i in fila){
    a<-as.numeric(df_par$X1[i])
    b<-as.numeric(df_par$X2[i])
    gen1<-factor(G[a])
    gen2<-factor(G[b])
    gep<-ggplot(df)+geom_point(aes(gen1,gen2))
  }
  gep
}


reg_fun<- function (H){
  figura=ggplot(df,aes(H[1],H[2]))+geom_point()
  return(figura)
}
#pregunta 2.5
#pregunta 2.6
#pregunta 2.7




