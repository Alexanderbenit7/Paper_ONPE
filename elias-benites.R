##### La relación entre el apoyo a la democracia y las preferencias
##### hacia el ejercicio iliberal y autoritario del poder político en
##### Perú (2023)
##### María Belén Elías y Alexander Benites
##### Revista Elecciones

library(rio)
library(car)
library(ggplot2)
library(modelsummary)
library(margins)
library(stringr)
library(ggeffects)

# -----------------------------Preprocesamiento de datos ---------------------------
# ----------------------------------------------------------------------------------

#Apertura de datos
lapop23 = import("PER_2023_LAPOP_AmericasBarometer_v1.0_w.dta")
#Subset de datos
data = lapop23[,c("idnum","wt","wave","estratopri","upm","ing4","q2","ur",
                  "q1tc_r","soct2","idio2","jc10","jc13","jc15a","jc16a",
                  "cses6n","pop101","pop107","dem30","d3","d4","edre")]
#Nombres de variables
names(data) = c("idnum","wt","wave","estratopri","upm","democracy","edad",
                "ur","genero","sit_pais","economia","militares_delincuencia",
                "militares_corrupcion","presidente_congreso","presidente_suprema",
                "mano_dura","pop1","pop2","per_democracy","op_cargos","op_television",
                "educacion")

#Variable de respuesta
data$democracyDIC = ifelse(data$democracy>4,1,0)
#Género:
data$genero = factor(data$genero, levels = c(1:2), labels = c("Hombre","Mujer"))
#Urbano y rural:
data$ur = factor(data$ur, levels = c(1:2), labels = c("Urbano","Rural"))
#Situación económica del país:
data$sit_pais = factor(data$sit_pais,
                       levels = c(1:3),
                       labels = c("Mejor","Igual","Peor"))
#Situación económica personal:
data$economia = factor(data$economia,
                       levels = c(1:3),
                       labels = c("Mejor","Igual","Peor"))
#Militares:
data$militares_delincuencia=car::recode(data$militares_delincuencia, "1=1 ; 2=0")
data$militares_delincuencia = factor(data$militares_delincuencia,
                                     levels = c(0:1),
                                     labels = c("No se justifica","Sí se justifica"))
data$militares_corrupcion=car::recode(data$militares_corrupcion, "1=1 ; 2=0")
data$militares_corrupcion = factor(data$militares_corrupcion,
                                   levels = c(0:1),
                                   labels = c("No se justifica","Sí se justifica"))
#Presidente y tomas de poder:
data$presidente_congreso=car::recode(data$presidente_congreso, "1=1 ; 2=0")
data$presidente_congreso = factor(data$presidente_congreso,
                                  levels = c(0:1),
                                  labels = c("No se justifica","Sí se justifica"))
data$presidente_suprema=car::recode(data$presidente_suprema, "1=1 ; 2=0")
data$presidente_suprema = factor(data$presidente_suprema,
                                 levels = c(0:1),
                                 labels = c("No se justifica","Sí se justifica"))
#Mano dura:
data$mano_duraT = factor(ifelse(data$mano_dura < 3,1,
                                ifelse(data$mano_dura == 3,2,
                                       ifelse(data$mano_dura>3,3,0))))
data$mano_duraT = factor(data$mano_duraT, levels = c(1:3), labels = c("Bueno","Ni bueno ni malo",
                                                                      "Malo"))  
data$mano_dura = car::recode(data$mano_dura, "1:2=1; 3:5=0")
data$mano_dura = factor(data$mano_dura,
                        levels = c(0:1),
                        labels = c("Malo","Bueno"))
#PER es una democracia:
data$per_democracy = factor(data$per_democracy, levels = c(1:2), labels = c("Sí","No"))
#Populismo dummies:
data$pop1DIC = factor(ifelse(data$pop1 >4,1,0))
data$pop1DIC = factor(data$pop1DIC, levels = c(0:1), labels = c("No","Sí"))
data$pop2DIC = factor(ifelse(data$pop2 >4,1,0))
data$pop2DIC = factor(data$pop2DIC, levels = c(0:1), labels = c("No","Sí"))


# ----------------------------- Análisis de regresión ----------------------------------
# --------------------------------------------------------------------------------------

### Preferencias por el ejercicio autoritario del poder (1):
### Golpes militares
set.seed(2019)

#H:
h1=formula(democracyDIC~militares_delincuencia)
h2=formula(democracyDIC~militares_delincuencia+economia+educacion+edad+genero+ur)
h3=formula(democracyDIC~militares_corrupcion)
h4=formula(democracyDIC~militares_corrupcion+economia+educacion+edad+genero+ur)

### Modelo:
rlog1=glm(h1, data=data,family = binomial)
rlog2=glm(h2, data=data,family = binomial)
rlog3=glm(h3, data=data,family = binomial)
rlog4=glm(h4, data=data,family = binomial)

modelrl=list('Golpes militares [Delincuencia]'=rlog1,
             'Golpes militares [Delincuencia y controles]'=rlog2,
             'Golpes militares [Corrupción]'=rlog3,
             'Golpes militares [Corrupción y controles]'=rlog4)

summary(rlog2)
summary(rlog4)


formatoNumero = function(x) format(x, digits = 4, scientific = FALSE)
modelsummary(modelrl,
             fmt=formatoNumero, # usa función que creé antes
             exponentiate = T, # coeficientes con logaritmo
             statistic = 'conf.int', # mostrar ICs
             title = "Regresión Logística (Coeficientes Exponenciados)",
             stars = TRUE,
             output = "kableExtra")

### Guardando efectos marginales
ame01 = data.frame(summary(margins(rlog2)))
ame02 = data.frame(summary(margins(rlog4)))
ame01$type = "Militares-Delincuencia" 
ame02$type = "Militares-Corrupción" 
ame01$type2 = "Autoritario" 
ame02$type2 = "Autoritario" 


### Preferencias por el ejercicio autoritario del poder (2):
### Engrandecimiento del Ejecutivo
set.seed(2019)
data$economia=relevel(data$economia,ref = "Peor") #Categría de regerencia 

### H:
h1=formula(democracyDIC~presidente_suprema)
h2=formula(democracyDIC~presidente_suprema+economia+educacion+edad+genero+ur)
h3=formula(democracyDIC~presidente_congreso)
h4=formula(democracyDIC~presidente_congreso+economia+educacion+edad+genero+ur)

### Modelos:
rlog1=glm(h1, data=data,family = binomial)
rlog2=glm(h2, data=data,family = binomial)
rlog3=glm(h3, data=data,family = binomial)
rlog4=glm(h4, data=data,family = binomial)

modelrl=list('Concentración el poder [Corte Suprema]'=rlog1,
             'Concentración el poder [Corte Suprema y controles]'=rlog2,
             'Concentración el poder [Congreso]'=rlog3,
             'Concentración el poder [Congreso y controles]'=rlog4)

summary(rlog2)
summary(rlog4)

modelsummary(modelrl,
             fmt=formatoNumero, # usa función que creé antes
             exponentiate = T, 
             statistic = 'conf.int', # mostrar ICs
             title = "Regresión Logística (Coeficientes Exponenciados)",
             stars = TRUE,
             output = "kableExtra")

### Probabilidades predicas:
### Control de la Corte Suprema por parte del Presidente
ggpredict(rlog2, terms = "presidente_suprema") %>% 
  plot()
ggpredict(rlog2, terms = "presidente_suprema")

### Guardando efectos marginales
ame11 = data.frame(summary(margins(rlog2)))
ame12 = data.frame(summary(margins(rlog4)))
ame11$type = "Engrandecimiento - Corte Suprema"
ame12$type = "Engrandecimiento - Congreso"
ame11$type2 = "Autoritario" 
ame12$type2 = "Autoritario" 


### Preferencias por el ejercicio iliberal del poder (1):
### Mano dura
set.seed(2019)
data$economia=relevel(data$economia,ref = "Peor") #Categoría de referencia

### H:
h1=formula(democracyDIC~mano_dura)
h2=formula(democracyDIC~mano_dura+economia+educacion+edad+genero+ur)

### Modelo:
rlog1=glm(h1, data=data,family = binomial)
rlog2=glm(h2, data=data,family = binomial)

summary(rlog1)
summary(rlog2)

modelrl=list('Mano dura'=rlog1,
             'Mano dura y controles'=rlog2)

modelsummary(modelrl,
             fmt=formatoNumero, # usa función que creé antes
             exponentiate = T, # coeficientes sin logaritmo
             statistic = 'conf.int', # mostrar ICs
             title = "Regresión Logística (Coeficientes Exponenciados)",
             stars = TRUE,
             output = "kableExtra")

### Probabilidades predichas:
ggpredict(rlog2, terms = "mano_dura")
ggpredict(rlog2, terms = "mano_dura") %>% 
  plot()

### Guardando efectos marginales:
ame2=data.frame(summary(margins(rlog2)))
ame2$type = "Mano dura"
ame2$type2 = "Iliberal" 


### Preferencias por el ejercicio iliberal del poder (2):
### Populismo
set.seed(2019)
data$economia=relevel(data$economia,ref = "Peor") #Categoría de referencia

### H:
h1=formula(democracyDIC~pop1DIC)
h2=formula(democracyDIC~pop1DIC+economia+educacion+edad+genero+ur)
h3=formula(democracyDIC~pop2DIC)
h4=formula(democracyDIC~pop2DIC+economia+educacion+edad+genero+ur)

### Modelo:
rlog1=glm(h1, data=data,family = binomial)
rlog2=glm(h2, data=data,family = binomial)
rlog3=glm(h3, data=data,family = binomial)
rlog4=glm(h4, data=data,family = binomial)

modelrl=list('Populismo [Limitar oposición]'=rlog1,
             'Populismo [Limitar oposición y controles]'=rlog2,
             'Populismo [Gobierno directo]'=rlog3,
             'Populismo [Gobierno directo y controles]'=rlog4)

summary(rlog2)
summary(rlog4)

modelsummary(modelrl,
             fmt=formatoNumero, # usa función que creé antes
             exponentiate = T, # coeficientes sin logaritmo
             statistic = 'conf.int', # mostrar ICs
             title = "Regresión Logística (Coeficientes Exponenciados)",
             stars = TRUE,
             output = "kableExtra")

#Probabilidades predichas
ggpredict(rlog2, terms = "pop1DIC")
ggpredict(rlog2, terms = "pop1DIC") %>% 
  plot()

ggpredict(rlog4, terms = "pop2DIC")
ggpredict(rlog4, terms = "pop2DIC") %>% 
  plot()

### Guardando efectos marginales
ame3=data.frame(summary(margins(rlog2)))
ame4=data.frame(summary(margins(rlog4)))
ame3$type = "Limitar oposición"
ame4$type = "Gobierno directo"
ame3$type2 = "Iliberal" 
ame4$type2 = "Iliberal" 


# ----------------------------- Visualización comparada --------------------------------
# --------------------------------------------------------------------------------------
ameFINAL = rbind(ame01,ame02)
ameFINAL = rbind(ameFINAL,ame11)
ameFINAL = rbind(ameFINAL,ame12)
ameFINAL = rbind(ameFINAL,ame2)
ameFINAL = rbind(ameFINAL,ame3)
ameFINAL = rbind(ameFINAL,ame4)

### Solo con significancia estadística:
ameFINAL = ameFINAL[c(6,13,20,27,34,41,48),]

### Gráfica final:
ggplot(ameFINAL ,aes(x=type, y=AME, group=type2, linetype = type2)) + 
  geom_point() +
  geom_errorbar(width=0, aes(ymin=lower, ymax=upper)) +
  
  geom_point(shape=21, size=2)+
  
  geom_hline(yintercept=0, linetype="dashed", 
             color = "red", size=1) +
  
  ylim(-0.2,0.3) +
  
  labs(x=" ", y="Average Marginal Effects", linetype=" ") +
  
  scale_x_discrete(labels = function(x) str_wrap(x, width = 13)) +
  
  theme_bw() + theme(legend.position="bottom") 
