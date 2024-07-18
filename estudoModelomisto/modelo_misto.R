
---------------------------------------------------------------------
  
############# modelos mistos ################

library(lme4) 
library(bbmle) 

dado=read.csv("mod_formacoes.csv")
dado1=read.csv("class1.csv")
dado2=read.csv("class2.csv")
dado3=read.csv("class3.csv")
dado4=read.csv("class4.csv")

###testes com efeito aleatorio das formacoes 
##altitude sendo fixo junto com densidade de indiv�duos por classes (sugestao Luciana)
###teste com todos os dados alterando os efeitos fixos e mantendo formacoes como aleatorio
## resultados -  so as classes sendo significativo para todos os modelos e aicc menor do modelo nulo


dado$alt2=(dado$altitude)^2 #altitude quadrado
dado$altscale=scale(dado$alt2) #padronizando altitude quadrado
dado$fform=as.factor(dado$formacoes) #formacoes como fatores


tm1=lmer(Biomass_Tot ~ High + (1|site), data = s)
summary (tm1)
tm2=lmer(Biomass_Tot ~ High +  Rain+ (1|site), data = s)
summary (tm2)
tm3=lmer(Biomass_Tot ~ High + Rain+ Temp+ (1|site), data = s)
summary (tm3)
tm4=lmer(Biomass_Tot ~ High + Rain+ Temp+ Driest_Month+(1|site), data = s)
summary (tm4)
tm5=lmer(Biomass_Tot~ 1 + (1|site), data = s)
summary (tm5)

AICctab(tm1,tm2,tm3,tm4,tm5, base = T, weights = T) #mostra o Aicc de todos modelos 

anova(tm1,tm2,tm3,tm4,tm5,tm6) # quais par�metros s�o significativos pela anova

summary(m2) #summary dos modelos


am1=lmer(Biomass_Ang ~ High + (1|site), data = s)
summary (am1)
am2=lmer(Biomass_Ang ~ High +  (1|site), data = s)
summary (am2)
am3=lmer(Biomass_Ang ~ High +  Rain+ (1|site), data = s)
summary (am3)
am4=lmer(Biomass_Ang ~ High + Rain+ Temp+ (1|site), data = s)
summary (am4)
am5=lmer(Biomass_Ang ~ High + Rain+ Temp+ Driest_Month+(1|site), data = s)
summary (am5)
anova(am4,am3, refit=FALSE)
am6=lmer(Biomass_Ang~ 1 + (1|site), data = s)
summary (am6)


AICctab(am1,am2,am3,am4,am5,am6, base = T, weights = T) #mostra o Aicc de todos modelos 

anova(m1,m2,m3,m4,m5,m6) # quais par�metros s�o significativos pela anova


gm1=lmer(Biomass_Gim ~ High + (1|site), data = s)
summary (gm1)
gm2=lmer(Biomass_Gim ~ High +  (1|site), data = s)
summary (gm2)
gm3=lmer(Biomass_Gim ~ High +  Rain+ (1|site), data = s)
summary (gm3)
gm4=lmer(Biomass_Gim ~ High + Rain+ Temp+ (1|site), data = s)
summary (gm4)
gm5=lmer(Biomass_Gim ~ High + Rain+ Temp+ Driest_Month+(1|site), data = s)
summary (gm5)
anova(am5,am4, refit=FALSE)
gm6=lmer(Biomass_Ang~ 1 + (1|site), data = s)
summary (gm6)

AICctab(gm1,gm2,gm3,gm4,gm5,gm6, base = T, weights = T) #mostra o Aicc de todos modelos 



trm1=lmer(Biomass_Trop ~ High + (1|site), data = s)
summary (trm1)
trm2=lmer(Biomass_Trop ~ High +  Rain+ (1|site), data = s)
summary (trm2)
trm3=lmer(Biomass_Trop ~ High + Rain+ Temp+ (1|site), data = s)
summary (trm3)
trm3.2=lmer(Biomass_Trop ~ High  + Temp + Driest_Month+ (1|site), data = s)
summary (trm3.2)
trm3.3=lmer(Biomass_Trop ~  Rain   + Temp + Driest_Month+ (1|site), data = s)
summary (trm3.3)
trm4=lmer(Biomass_Trop ~ High + Rain + Temp + Driest_Month+ (1|site), data = s)
summary (trm4)
trm5=lmer(Biomass_Ang~ 1 + (1|site), data = s)
summary (trm5)

AICctab(trm1,trm2,trm3,trm3.2,trm3.3,trm4,trm5, base = T, weights = T) #mostra o Aicc de todos modelos 
anova(trm3,trm3.2, refit=FALSE)


plot (Biomass_Gim ~ Rain, data=s)




