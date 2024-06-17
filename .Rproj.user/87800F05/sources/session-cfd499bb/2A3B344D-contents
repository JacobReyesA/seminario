pacman::p_load(dplyr, 
               car, 
               sjmisc, 
               sjPlot, 
               sjlabelled, 
               stargazer, 
               kableExtra, 
               corrplot, 
               texreg, 
               ggplot2, 
               ggpubr,
               haven)
data <- read_sav("cepinmigracion.sav")
frq(data$iden_nacional_16)
view(data$P2_1_1)
colnames(data)
#Gráfico nivel de acuerdo en que politicamente Chile debería prohibir todo tipo de inmigración.
graph2 <- cepnacional %>% ggplot(aes(x = iden_nacional_16)) + 
  geom_bar(fill = "red")+
  labs(title = "Nivel de acuerdo con prohibir la inmigración",
       x = "Nivel de acuerdo",
       y = "Frecuencia") +
  theme_bw()
graph1
frq(data$iden_nacional_23_a)
cepnacional <- read_sav("base_90.sav")
graph2
cepnacional <-  cepnacional %>% set_na(., na = c(-9, -8))

#Gráfico variable de percepción sobre el aumento del indice de criminalidad.
frq(data$iden_nacional_8_a)
graph3 <- cepnacional %>% ggplot(aes(x = iden_nacional_8_a)) + 
  geom_bar(fill = "red")+
  labs(title = "Nivel de acuerdo con que la inmigración aumenta los índices de criminalidad",
       x = "Nivel de acuerdo",
       y = "Frecuencia") +
  theme_bw()
graph3

#Orgullo de la nacionalidad
frq(cepnacional$iden_nacional_14)
frq(cepnacional$iden_nacional_14_b)

#En cuanto a salud, deben tener acceso a salud publica:
frq(cepnacional$iden_nacional_18)

#Razón por la cual emigraron a Chile
frq(cepnacional$iden_nacional_22_b)
#Nivel de acuerdo con las siguientes afirmaciones, Los ingmirantes...:
frq(cepnacional$iden_nacional_8_a) #Elevan los indices de criminalidad
frq(cepnacional$iden_nacional_8_k) #Contribuyen con nuevas ideas y cultura
frq(cepnacional$iden_nacional_8_c) #Quitan LOH tshraajo
frq(cepnacional$iden_nacional_8_i) #Inmigrantes REGULARES debieran tener el mismo acceso a la educación pública
frq(cepnacional$iden_nacional_8_l) #Son un aporte para la econonomía de Chile

graph4 <- cepnacional %>% ggplot(aes(x = iden_nacional_8_l)) + 
  geom_bar(fill = "red")+
  labs(title = "Los inmigrantes son un aporte a la economía de Chile",
       x = "Nivel de acuerdo",
       y = "Frecuencia") +
  theme_bw()

#De los inmigrantes que han llegado en los últimos 5 años cuantos de ellos usted diría que son:
frq(cepnacional$iden_nacional_20_a) #Trabajadores
frq(cepnacional$iden_nacional_20_b) #Honestos
frq(cepnacional$iden_nacional_20_c) #Respetan las costumbres de los chilenos
frq(cepnacional$iden_nacional_20_d) #Cumplen la ley

#Imagen de los inmigrantes que han llegado hace 5 años es mejor o peor que la que tenía sobre los inmigrantes que llegaron antes que ellos:
frq(cepnacional$iden_nacional_21) 
##Gráfico de esta variable
graph5 <- cepnacional %>% ggplot(aes(x = iden_nacional_21)) + 
  geom_bar(fill = "red") + 
  labs(title = "Imagen que se tiene de los inmigrantes que han llegado 
       en los últimos 5 años con respecto a los que llegaron 5 años antes",
       x = "Nivel de acuerdo",
       y = "Frecuencia") +
  theme_bw()
graph5
#Qué tan de acuerdo está con las siguientes afirmaciones
frq(cepnacional$iden_nacional_23_a)
frq(cepnacional$iden_nacional_23_b)
frq(cepnacional$iden_nacional_23_c) #Perjudican a personas como yo (sería interesante hacer una tabla de contigencia con estrato social).
frq(cepnacional$iden_nacional_23_d) #Los inmigrantes realizan trabajos que los chilenos no quieren realizar.
frq(cepnacional$gse)

#Costumbres y tradiciones
frq(cepnacional$iden_nacional_24) #Mayoría en que debieran abandonar sus propias costumbres y tradiciones.
##Gráfico de esta variable
graph5 <- cepnacional %>% ggplot(aes(x = iden_nacional_24)) + 
  geom_bar(fill = "red") + 
  labs(title = "Costumbres y tradiciones",
       x = "-",
       y = "Frecuencia") +
  theme_bw()
graph5

#Nivel de conflicto que hay entre diferentes grupos de la sociedad:
frq(cepnacional$pobreza_39_m) #Chilenos e inmigrantes
frq(cepnacional$pobreza_39_a) #Pobres y ricos
frq(cepnacional$pobreza_39_k) #Oficialismo y oposición
frq(cepnacional$pobreza_39_j) #Los mapuche y no mapuche
frq(cepnacional$pobreza_39_n) #Derecha e izquierda

#Qué tanto le agradaría o molestaría a usted que:
frq(cepnacional$iden_nacional_25_a) #Incorporar a un inmigrante a la familia
frq(cepnacional$iden_nacional_25_b) #Tener de vecino a un inmigrante
frq(cepnacional$iden_nacional_25_c) #Trabajar junto a un inmigrante


#Frecuencia con la que se relaciona con personas inmigrantes
frq(cepnacional$iden_nacional_28) 

#Valoración de la experiencia al relacionarse con los inmigrantes
frq(cepnacional$iden_nacional_29) #Positiva
frq(cepnacional$iden_nacional_30) #Negativa

frq(cepnacional$educacion_104_a)
#Tablas de contingencia
tabla6 <- sjt.xtab(cepnacional$iden_nacional_16, cepnacional$esc_nivel_1_c, encoding = "UTF-8")
tabla6
