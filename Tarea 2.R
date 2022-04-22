library(tidyverse)
library(haven)
library(rlang)
library(foreign)
library(dplyr)
library(jtools)
library(stargazer)
library(AER)
library(schoolmath)

#=============== CREACIÓN/LIMPIEZA DE LA BASE DE DATOS ==================

complete <- read.csv("/Users/migueln/Desktop/P-21/Microecon/LIC - Tarea 2/frmgham2.csv")
df0 <- data.frame(complete)

endline <- df0 %>% 
  group_by(RANDID) %>% 
  filter(PERIOD == max(PERIOD)) %>%
  arrange(RANDID)

#Hacemos de educ una variable dummy

endline <- endline %>%
  mutate( ELEMENTARY = 
            ifelse (EDUC == 1, 1,0 ),
          HIGHSCHOOL = 
            ifelse (EDUC == 2, 1,0 ),
          VOCATIONAL = 
            ifelse (EDUC == 3, 1,0 ),
          COLLEGE_MORE= 
            ifelse (EDUC == 4, 1,0 ),
          )

#Seleccionamos solo las variables que vamos a utilizar en la tarea

endline <- endline %>% select(
  RANDID,WOMEN,TOTCHOL,AGE,SYSBP,DIABP,SMKR,CIGDAY,BMI,DIABETES,BPMEDS,GLUCOSE,EDUC,HDLC,LDLC,DEATH,PREVCHD,PREVHYP,ANYCHD,CVD,HYPERTEN, ELEMENTARY, HIGHSCHOOL, VOCATIONAL, COLLEGE_MORE)

#Creamos el archivo en .dta y .csv

write.csv(endline,"frmgham.csv",row.names = F)
write.dta(endline,"frmgham.dta")

#=============== SOLUCIONES TAREA ==================

#Hacemos la edad una variable dummy pa' las personas menores a 50 años 
endline <- endline %>%
  mutate( menor_50 = 
            ifelse (AGE < 50, 1,0 ))

# ==== // Pregunta 2 \\ ====

#Especificación (1)

#Creamos la variable menor_50*WOMEN
menor_50xWOMEN= endline$menor_50 * endline$WOMEN

mco_1 <- lm(CVD ~ AGE + WOMEN + menor_50 + menor_50xWOMEN + TOTCHOL + HDLC + SYSBP + BPMEDS + SMKR + DIABETES, 
            data = endline)
es_mco_1<- sqrt(diag(vcovHC(mco_1, type = "HC1")))

#Especificación (2)
mco_2 <- lm(CVD ~ AGE + WOMEN + TOTCHOL + HDLC + SYSBP + BPMEDS + SMKR + DIABETES, 
            data = endline)
es_mco_2<- sqrt(diag(vcovHC(mco_2, type = "HC1")))

#Especificación (3)
mco_3 <- lm(CVD ~ AGE + WOMEN + HDLC + SYSBP + BPMEDS + SMKR + DIABETES + log(BMI), 
            data = endline)
es_mco_3<- sqrt(diag(vcovHC(mco_3, type = "HC1")))

#Especificación (4)

#Creamos la variable AGE^2
AGE_2= endline$AGE^2 

mco_4 <- lm(CVD ~ AGE + AGE_2 + WOMEN + TOTCHOL + HDLC + SYSBP + PREVCHD + BPMEDS + SMKR + DIABETES, 
            data = endline)
es_mco_4<- sqrt(diag(vcovHC(mco_4, type = "HC1")))

#Especificación (5)

#Creamos la variable SMKR*CIGDAY
SMKRxCIGDAY= endline$SMKR * endline$CIGDAY

mco_5 <- lm( log(TOTCHOL) ~ AGE + AGE_2 + WOMEN + SMKR + SMKRxCIGDAY + log(BMI), 
            data = endline)
es_mco_5 <- sqrt(diag(vcovHC(mco_5, type = "HC1")))

#Tabla 
stargazer(mco_1,mco_2,mco_3,mco_4, mco_5, 
          digits = 2,
          title = "Estimaciones de MCO",
          label = "tab:tabla_regresiones",
          omit.stat = c("f","adj.rsq","ser"), 
          font.size = "footnotesize", 
          column.sep.width = "1pt", 
          single.row = F,
          no.space = F,
          se=list(es_mco_1,es_mco_2,es_mco_3,es_mco_4, es_mco_5), 
          out = "tabla_regs.tex")

# ==== // Pregunta 7 \\ ====

#Especificación (2)
mco_2 <- lm(CVD ~ AGE + WOMEN + TOTCHOL + HDLC + SYSBP + BPMEDS + SMKR + DIABETES, 
            data = endline)
es_mco_2<- sqrt(diag(vcovHC(mco_2, type = "HC1")))

#Especificación (2.BIS)
mco_2_bis <- lm(CVD ~ AGE + WOMEN + TOTCHOL + HDLC + SYSBP + BPMEDS + SMKR + DIABETES + HIGHSCHOOL + VOCATIONAL + COLLEGE_MORE, 
            data = endline)
es_mco_2_bis<- sqrt(diag(vcovHC(mco_2_bis, type = "HC1")))

#Especificación (3.BIS)
mco_3_bis <- lm(CVD ~ AGE + WOMEN + TOTCHOL + HDLC + SYSBP + BPMEDS + SMKR + DIABETES + PREVHYP, 
                data = endline)
es_mco_3_bis<- sqrt(diag(vcovHC(mco_2_bis, type = "HC1")))

stargazer(mco_2,mco_2_bis,mco_3_bis, 
          digits = 2,
          title = "Estimaciones de MCO",
          label = "tab:tabla_regresiones",
          omit.stat = c("f","adj.rsq","ser"), 
          font.size = "footnotesize", 
          column.sep.width = "1pt", 
          single.row = F,
          no.space = F,
          se=list(es_mco_2,es_mco_2_bis,es_mco_3_bis), 
          out = "tabla_regs.tex")
