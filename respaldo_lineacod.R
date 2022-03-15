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
df_long <- long_df(df,"bcr_patient_barcode","dataset","gen","expresion_level") %>% select(dataset,gen,expresion_level)
