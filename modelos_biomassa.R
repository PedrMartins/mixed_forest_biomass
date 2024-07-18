##################### Teste normalidade e modelos ********************
library(vegan)
library(ggplot2)
library(jtools)
library(plotly)


dado=read.csv("mod_formacoes.csv")
dado1=read.csv("class1.csv")
dado2=read.csv("class2.csv")
dado3=read.csv("class3.csv")
dado4=read.csv("class4.csv")


####gr�fico de dispers�o --- s� para visualizar os dados de biomassa em rela��o � altitude ##
ggplotly(
  ggplot(dado, aes(x = altitude , y = biomassa)) +
    geom_point(color = "#39568CFF", size = 2.5) +
    geom_smooth(aes(color = "Fitted Values"),
                method = "lm", formula = y ~ x, se = F, linewidth = 2) +
    labs(x = "Altitude",
         y = "Diversidade",
         title = paste("R�:", round(((cor(dado$biomassa, dado$altitude))^2),3))) +
    scale_color_manual("Legenda:",values = "grey50") +
    theme_classic())


#### modelo linear e teste normalidade dos res�duos ####
normal=dado$biomassa
teste_linear=lm(normal~altitude, dado)
summary(teste_linear)
qqnorm(residuals(teste_linear))

####shapiro 

?shapiro.test(normal) ### teste da distribui��o da vari�vel
shapiro.test(teste_linear$residuals) ## teste dos res�duos, aderiu � normalidade p>0.05


############# regressoes multiplas (Ex artigo Alves, 2010)
### altitude n�o se mostrou significativa em nenhum dos casos, inclusive no quadr�tico

##modelo geral total

modelo <- lm(formula = biomassa ~ . - formacoes ,data = dado) #biomassa total, 
#quando colocamos ~. � s� para n�o ter que digitar todos os par�metros, 
#mas a� tem que colocar o #-formacoes para nao incluir 
#no caso ou qualquer outro que n�o queira no modelo


## demais modelos com as classes e altitude 
m1 <- lm(formula = biomassa ~ . - formacoes, data = dado1) #classe1

m2=lm(formula = biomassa ~ . - formacoes, data = dado2) #classe2

m3=lm(formula = biomassa ~ . - formacoes, data = dado3) #classe3

m4=lm(formula = biomassa ~ . - formacoes, data = dado4) #classe4

m5=lm(formula = biomassa ~ I(altitude^2) + Class1 + Class2 + Class3 + Class4 ,data = dado) #quadr�tico

summary(m1)

### forma de ver o summary dos modelos e comparar - pacote jtools
#summ(modelo, confint = T, digits=4, ci.width = .95)
export_summs(modelo, m1, m2, m3, m4, m5, model.names = c("Total","Classe1","Classe2","Classe3", "Classe4","Quadr�tico"),
             scale = F, digits=4)




