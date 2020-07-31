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
tab_corr(proc_Desiguales, triangle = "lower", file = "output/tablas/tabla2.html")
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



sessionInfo()
#R version 4.0.2 (2020-06-22)
#Platform: x86_64-w64-mingw32/x64 (64-bit)
#Running under: Windows 10 x64 (build 18362)

#Matrix products: default

#locale:
#  [1] LC_COLLATE=Spanish_Chile.1252  LC_CTYPE=Spanish_Chile.1252   
#[3] LC_MONETARY=Spanish_Chile.1252 LC_NUMERIC=C                  
#[5] LC_TIME=Spanish_Chile.1252    

#attached base packages:
#  [1] stats     graphics  grDevices utils     datasets  methods   base     

#loaded via a namespace (and not attached):
#  [1] tinytex_0.25      statmod_1.4.34    tidyselect_1.1.0  sjlabelled_1.1.6 
#[5] xfun_0.16         sjPlot_2.8.4      performance_0.4.8 purrr_0.3.4      
#[9] splines_4.0.2     lattice_0.20-41   parameters_0.8.2  colorspace_1.4-1 
#[13] vctrs_0.3.2       generics_0.0.2    htmltools_0.5.0   yaml_2.2.1       
#[17] rlang_0.4.7       pillar_1.4.6      nloptr_1.2.2.2    glue_1.4.1       
#[21] effectsize_0.3.2  modelr_0.1.8      emmeans_1.4.8     lifecycle_0.2.0  
#[25] sjmisc_2.8.5      munsell_0.5.0     gtable_0.3.0      bayestestR_0.7.2 
#[29] mvtnorm_1.1-1     evaluate_0.14     knitr_1.29        broom_0.7.0      
#[33] Rcpp_1.0.5        xtable_1.8-4      backports_1.1.7   scales_1.1.1     
#[37] ggeffects_0.15.1  webshot_0.5.2     lme4_1.1-23       ggplot2_3.3.2    
#[41] digest_0.6.25     insight_0.9.0     dplyr_1.0.0       grid_4.0.2       
#[45] tools_4.0.2       sjstats_0.18.0    magrittr_1.5      tibble_3.0.3     
#[49] pacman_0.5.1      crayon_1.3.4      tidyr_1.1.0       pkgconfig_2.0.3  
#[53] MASS_7.3-51.6     ellipsis_0.3.1    Matrix_1.2-18     estimability_1.3 
#[57] minqa_1.2.4       rmarkdown_2.3     rstudioapi_0.11   R6_2.4.1         
#[61] boot_1.3-25       nlme_3.1-148      compiler_4.0.2 

getwd()

"C:/Users/Catalina/Documents/TrabajoCSA"
