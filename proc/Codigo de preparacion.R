#-----Codigo de Preparacion de datos------

#  Cargar librer?as 
pacman::p_load(dplyr,stargazer, car, sjlabelled, summarytools)
options(scipen=999)

#  Abrir base de datos 
Desiguales <- read.csv("input/data/original/PNUD_DES_2016_publica.csv", sep = ",", dec = ",")
View(Desiguales)

#  Nombres de las variables
names(Desiguales)

# Seleccion variables
proc_Desiguales <- Desiguales %>% select("p53_1","p54_1")
View(proc_Desiguales)


#-------Recodificacion y ajustes------

#  Renombrar las variables 
proc_Desiguales <- proc_Desiguales %>% rename("Pos_soc_Y"=p53_1)
proc_Desiguales <- proc_Desiguales %>% rename("Pos_ninez_x"=p54_1)

#  Eliminar casos perdidos 
proc_Desiguales$Pos_soc_Y <- recode(proc_Desiguales$Pos_soc_Y, "c(88,99)=NA")
proc_Desiguales$Pos_ninez_x <- recode(proc_Desiguales$Pos_ninez_x, "c(88,99)=NA")

dim(proc_Desiguales)
sum(is.na(proc_Desiguales))
proc_Desiguales <-na.omit(proc_Desiguales)
sum(is.na(proc_Desiguales))
dim(proc_Desiguales)

#  Verificar etiquetas de la base
sjlabelled::get_label(proc_Desiguales)

#  Generar etiquetas
proc_Desiguales$Pos_soc_Y <- set_label(x = proc_Desiguales$Pos_soc_Y,label = "Posicion social actual")
proc_Desiguales$Pos_ninez_x <- set_label(x = proc_Desiguales$Pos_ninez_x,label = "Posicion social en el hogar de infancia")

#  Verificar etiquetas 
get_label(proc_Desiguales$Pos_soc_Y)
get_label(proc_Desiguales$Pos_ninez_x)

#----Descriptivos para la base creada-------
stargazer(proc_Desiguales,type = "text")

dfSummary(proc_Desiguales, plain.ascii = FALSE)
view(dfSummary(proc_Desiguales, headings=FALSE))


#---- Guardar Base procesada
save(proc_Desiguales,file= "input/data/proc/proc_Desiguales.RData")





