#----Codigo de analisis----

#  Cargar librerías 
pacman::p_load(dplyr, sjmisc, summarytools, sjPlot, ggplot2, webshot)
webshot::install_phantomjs( force = T)

#  Abrir base de datos 
load(file = "input/data/proc/proc_Desiguales.RData")
View(proc_Desiguales)

# --Generar tabla de estadisticos y guardar--
dfSummary(proc_Desiguales, plain.ascii = FALSE)
view(dfSummary(proc_Desiguales, headings=FALSE), file = "output/tablas/tabla1.html")
webshot("output/tablas/tabla1.html","output/tablas/tabla1.png")

#--- Graficar asociacion entre variables
Grafico1=ggplot(proc_Desiguales, aes(x=Pos_ninez_x, y=Pos_soc_Y)) +
  geom_point()
Grafico1

#---Correlacion 
#Valor de correlacion
Correlacion <- cor(proc_Desiguales,use="complete.obs")
Correlacion

#Correlacion en la tabla y guardar
sjt.corr(proc_Desiguales, triangle = "lower", file = "output/tablas/tabla2.html")
webshot("output/tablas/tabla2.html","output/tablas/tabla2.png", vwidth = 550, vheight = 220)

#Grafico de nube
Grafico2 <- plot_scatter(proc_Desiguales, Pos_ninez_x, Pos_soc_Y, dot.size =0.5,
                         colors = "black",
                         title ="Gráfico 2. Relación entre posición social actual y posicion social de infancia", 
                         jitter = .35, fit.line = lm)

Grafico2
dev.copy(png,"output/graficos/grafico2.png",width=685,
         height=480); dev.off()



#Para visualizar la recta 

Grafico3=ggplot(proc_Desiguales, aes(x=Pos_ninez_x, y=Pos_soc_Y)) +
  geom_point(colour = "black", size = 2) +
  geom_smooth(method=lm, se=FALSE, colour = "coral") + labs(title = "Gráfico 1. Relación entre posición social actual y
posicion social de infancia", caption = "Fuente: Elaboración propia en base a encuesta 
                                               PNUD Desiguales(2016)", y = "Posición social actual",
                                                            x ="Posición social en el hogar de infancia")

Grafico3

dev.copy(png,"output/graficos/grafico3.png",width=685,
         height=480); dev.off()


#-------Modelo de regresion----
##Estimar regresion
Regresion=lm(Pos_soc_Y ~ Pos_ninez_x, data = proc_Desiguales)
Regresion

#Modelo en la tabla
sjPlot::tab_model(Regresion, show.ci=FALSE, file = "output/tablas/regresion_tab.html")
webshot("output/tablas/regresion_tab.html","output/tablas/regresion_tab.png", vwidth = 300, vheight = 250)

#----Bondad de ajuste
#Variable de valores predichos
estimado <- proc_Desiguales$estimado<- ( 3.24 + proc_Desiguales$Pos_ninez_x* 0.42)
estimado
#Estimamos el residuo
residuo <- proc_Desiguales$residuo <- proc_Desiguales$Pos_soc_Y - proc_Desiguales$estimado
residuo
proc_Desiguales %>% select( estimado, residuo)
#para visualizar los residuos agregando tamaño y colores

Grafico4 <- ggplot(proc_Desiguales, aes(x=Pos_ninez_x, y=Pos_soc_Y))+
  geom_smooth(method="lm", se=FALSE, color="coral") +
  geom_segment(aes(xend=Pos_ninez_x, yend=estimado), alpha = .2) + 
  geom_point(aes(color = abs(residuo), size = abs(residuo))) +
  scale_color_continuous(low = "black", high = "coral") +
  guides(color = FALSE, size = FALSE) +
  geom_point(aes(y=estimado), shape =1) + labs(title= "Grafico 3. Modelo de regresión", 
                                               caption = "Fuente: Elaboración propia en base a encuesta 
                                               PNUD Desiguales(2016)", y = "Posición social actual",
                                               x ="Posición social en el hogar infancia")
Grafico4

dev.copy(png,"output/graficos/grafico4.png",width=685,
         height=480); dev.off()





