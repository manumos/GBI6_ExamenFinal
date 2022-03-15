#pregunta 1.1
getwd() #comprobar directorio actual
setwd("/Users/Manuela/Documents/GitHub/GBI6_ExamenFinal/")#definir directorio actual
#pregunta 1.2
df<- read.csv("data/mRNA_expr.csv")
#pregunta 1.3
install.packages("tidyverse") #instale tydiverse
library("tidyverse")
library("tidyr")

#definir funcion en base a un vector que contenga los argumentos
long_df <- function(a,b,c,d,e) {
  y=a %>% pivot_longer(!c(b,c),names_to =d, values_to = e)
  return(y)
}
#pregunta 1.4
df_long <- long_df(df,"bcr_patient_barcode","dataset","gen","expresion_level") %>% select(!(bcr_patient_barcode))

#pregunta 1.5
library("sjPlot")
#pregutna 1.6

#pregunta 2.1
library("ggplot2")
tcga_boxplots<- function (x) {
  grafica=ggplot(df_long ,aes(df_long(sample(c(df_long[df_long$dataset == x,])),expresion_level)) +
    geom_boxplot() + geom_jitter(alpha=0.2, size = 0.2)+
    labs(x = "Tipo de cancer",y = "Nivel de expresion",title="Nivel de expresion")+
    theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold"))
    return(grafica)
}

#prueba
tcga_boxplots("BRCA")

#sets<-c('BCRA','KIPAN','KIPR','LUSC','OV','UCEC')
#pregunta 2.2

