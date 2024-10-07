########################################################

pkg <- c('BIOMASS',"tidyverse", "timetk","dplyr","corrplot",
         "vegan",'stringr',"viridis")

pkg <- pkg[!pkg%in%installed.packages()]
pkg
install.packages (pkg)

########################################################

source("Function_biomass.R")

library (BIOMASS)
library(tidyverse)
library(dplyr)
library(stringr)
library (corrplot)
library (vegan) #funcao disponibiliza os pacotes


bio.cj<- import_biomass_rawdata (site="cj")
bio.bp<- import_biomass_rawdata (site="bp")
bio.Fbar<- import_biomass_rawdata (site="fb")
bio.Fsf<- import_biomass_rawdata (site="fsf")
bio.It<- import_biomass_rawdata (site="it")
bio.BC<- import_biomass_rawdata (site="bc")

bio.cj <- data_processing (bio.cj)
bio.bp <- data_processing (bio.bp)
bio.Fbar <- data_processing (bio.Fbar)
bio.Fsf <- data_processing (bio.Fsf)
bio.It <- data_processing (bio.It)
bio.BC <- data_processing (bio.BC)



############################################################
############################################################
############################################################
############################################################
#####################CAMPOS DO JORDAO#######################

#############
###Limpeza dados/processing data
#############
bio.cj <- bio.cj [!str_ends(bio.cj$Gen,"aceae"),]
dads.gim.cj<- bio.cj[bio.cj$Filo=="Gim",]#separa gimnosperma
dads.ang.cj<- bio.cj[bio.cj$Filo!="Gim",]#retirando gimnosperma
dads.ang.cj<- dads.ang.cj[dads.ang.cj$Filo!="Saman",]#retirando samambaia


#View (dads.ang.cj)


##############################################################
################DBH class/classes DAP#########################
#for DBH class < 10 cm / >= 10 to < 30 / >= 30 to < 50 />= 50#


clas_gim_cj.10<-dads.gim.cj [dads.gim.cj$DAP<10,] #DBH < 10
g.smal=length(clas_gim_cj.10$DAP)
clas_gim_cj.10.30<-dads.gim.cj [dads.gim.cj$DAP>=10 & dads.gim.cj$DAP<30,] #DBH >= 10 to <30
g.med=length(clas_gim_cj.10.30$DAP)
clas_gim_cj.30.50<-dads.gim.cj [dads.gim.cj$DAP>=30 & dads.gim.cj$DAP<50,] #DBH >= 30 to <50
g.lar=length(clas_gim_cj.30.50$DAP)
clas_gim_cj.50<-dads.gim.cj [dads.gim.cj$DAP>=50,] #DBH >= 50
g.x.larg=length(clas_gim_cj.50$DAP)

clas_ang_cj.10<-dads.ang.cj [dads.ang.cj$DAP<10,]
smal=length(clas_ang_cj.10$DAP)
clas_ang_cj.10.30<-dads.ang.cj [dads.ang.cj$DAP>=10 & dads.ang.cj$DAP<30,]
med=length(clas_ang_cj.10.30$DAP)
clas_ang_cj.30.50<-dads.ang.cj [dads.ang.cj$DAP>=30 & dads.ang.cj$DAP<50,]
lar=length(clas_ang_cj.30.50$DAP)
clas_ang_cj.50<-dads.ang.cj [dads.ang.cj$DAP>=50,]
x.lar=length(clas_ang_cj.50$DAP)
s.a=sum(smal,med,lar,x.lar)
s.g=sum(g.smal,g.med,g.lar,g.x.larg)
p.a=(c(smal,med,lar,x.lar)/s.a)*100
p.g=(c(g.smal,g.med,g.lar,g.x.larg)/s.g)*100
a.b.a=sum(dads.ang.cj$DAP)
a.b.g=sum(dads.gim.cj$DAP)
(c(a.b.a,a.b.g)/sum(a.b.a,a.b.g))*100


############################################################
############################################################
############################################################
##Est.Altura / equation to estimate tree height
#Ang
#weibull
a=27.188
b=0.091
c.1=0.738
D.1=dads.ang.cj$DAP
dads.ang.cj$Alt.E= a*(1-exp(-b*(D.1^c.1)))

###
###
#Gim
a.g=25.9889
b.g=19.9290
g.D.1=dads.gim.cj$DAP
dads.gim.cj$Alt.E = (1.3+a.g*exp(-(b.g/g.D.1)))


############################################################
############################################################
############################################################
##Densidade / wood density

#density from GWD
Dens.cj= getWoodDensity(genus=dads.ang.cj$Gen,
                        species=dads.ang.cj$Spp)

dads.ang.cj$DensM <- Dens.cj$meanWD
dads.ang.cj$Lvl.D <- Dens.cj$levelWD

#write.table (dads.ang.cj,file="dads.ang.cj.csv",
# dec=",",col.names=TRUE)#função exporta a tabela
#caso queira comparar densidade
#com outro banco de dados

########################################################
#####inserindo densidade / density from (Doutorado / PhD
##### Gabriel Marcos Oliveira "Densidade da madeira
#####em minas gerais")
#####<http://repositorio.ufla.br/jspui/bitstream/1/4880/1/TESE_Densidade%20da%20madeira%20em%20Minas%20Gerais%20%20amostragem%2C%20espacializa%C3%A7%C3%A3o%20e%20rela%C3%A7%C3%A3o%20com%20vari%C3%A1veis%20ambientais.pdf>
########################################################

#Density from Gabriel Marcos Oliveira PhD
dads.ang.cj [dads.ang.cj$Gen == "Callisthene",11] <- 0.604  # pg 48 linha 55
dads.ang.cj [dads.ang.cj$Gen == "Casearia",11] <- 0.492 #pg 48 linha 63
dads.ang.cj [dads.ang.cj$Gen == "Clethra",11] <- 0.404 #pg 49 linha 78
dads.ang.cj [dads.ang.cj$Gen == "Myrsine" & dads.ang.cj$Spp == "coriacea",11] <- 0.545 #pg 52 linha 218
dads.ang.cj [dads.ang.cj$Gen == "Myrsine" & dads.ang.cj$Spp == "umbellata",11] <- 0.602   #pg 52 linha 220
dads.ang.cj [dads.ang.cj$Gen == "Ocotea" & dads.ang.cj$Spp == "bicolor",11] <- 0.628   #pg 52 linha 223
dads.ang.cj [dads.ang.cj$Gen == "Ocotea" & dads.ang.cj$Spp == "indecora",11] <- 0.663   #pg 53 linha 227
dads.ang.cj [dads.ang.cj$Gen == "Ocotea" & dads.ang.cj$Spp == "pulchella",11] <- 0.593   #pg 53 linha 230
dads.ang.cj [dads.ang.cj$Gen == "Persea" & dads.ang.cj$Spp == "willdenovii",11] <- 0.656   #pg 53 linha 239
dads.ang.cj [dads.ang.cj$Gen == "Piptocarpha" & dads.ang.cj$Spp == "macropoda",11] <- 0.601   #pg 53 linha 241
dads.ang.cj [dads.ang.cj$Gen == "Psychotria" & dads.ang.cj$Spp == "vellosiana",11] <- 0.379   #pg 53 linha 241
dads.ang.cj [dads.ang.cj$Gen == "Roupala" ,11] <- 0.626   #pg 54 linha 278
dads.ang.cj [dads.ang.cj$Gen == "Solanum" & dads.ang.cj$Spp == "swartzianum",11] <- 0.567   #pg 54 linha 294
dads.ang.cj [dads.ang.cj$Gen == "Symplocos" & dads.ang.cj$Spp == "celastrinea",11] <- 0.427   #pg 54 linha 294
dads.ang.cj [dads.ang.cj$Gen == "Zanthoxylum" ,11] <- 0.537   #pg 55 linha 338

###################
######média ponderada (Por indivíduo) caso tenha
######inserido novos dados
###################



dads.s.datset.cj <- dads.ang.cj [dads.ang.cj$Lvl.D != "dataset" ,]

dads.s.datset.cj$bino <- paste(dads.s.datset.cj$Gen,dads.s.datset.cj$Spp)

dens <- dads.s.datset.cj[!duplicated (dads.s.datset.cj$bino),]
dens <- arrange(dens,bino)
spp.n =count (dads.s.datset.cj,bino)
spp.n <- arrange(spp.n,bino)

dads.ang.cj [dads.ang.cj$Lvl.D == "dataset" ,11] <- meanp (dens$DensM,spp.n$n
                                                           ,dads.s.datset.cj$bino,
                                                           as_numeric=TRUE)

#View(dads.ang.cj)
###Dens gimnosperma

Dens.cj.2= getWoodDensity(genus=dads.gim.cj$Gen,
                          species=dads.gim.cj$Spp)
#head (Dens.cj.2)
#tail (Dens.cj.2)
dads.gim.cj$DensM <- Dens.cj.2$meanWD
dads.gim.cj$Lvl.D <- Dens.cj.2$levelWD
#tail (Dens.cj.2)



############################################################
############################################################
############################################################
##Biomassa


##Gymnosperms
x=0.4141 #converstion to dry biomass (Souza e Longhi)
a= 111.7988
b= -15.5317
c.2= 0.8544
d = 0.0180
g.D.1= c(dads.gim.cj$DAP)
g.H.1=c(dads.gim.cj$Alt)
dads.gim.cj$biom= (x*(a+b*c(g.D.1)+c.2*c(g.D.1^2)+d*((c(g.D.1^2))*g.H.1))) #alometric equation biomass (Sanqueta )

##Angiosperms
#alometric equation Chaves et. al 2015 from BIOMASS package
dads.ang.cj$biom= (computeAGB(D=dads.ang.cj$DAP,
                              WD=dads.ang.cj$DensM,
                              H=dads.ang.cj$Alt.E))*1000
#head (dads.gim.cj)
#head (dads.ang.cj)


bio.cj =rbind (dads.ang.cj,dads.gim.cj)


#head (bio.cj)

######################
######################
######################
######################
######################DAP xBIO

bio.gim.cj<-bio.cj[bio.cj$Filo=="Gim",]
bio.ang.cj <-bio.cj[bio.cj$Filo!="Gim",]
#tail (bio.ang.cj)

b.clas_gim_cj.10<-bio.gim.cj [bio.gim.cj$DAP<10,]
b.g.smal=sum(b.clas_gim_cj.10$biom)
b.clas_gim_cj.10.30<-bio.gim.cj [bio.gim.cj$DAP>=10 & bio.gim.cj$DAP<30,]
b.g.med=sum(b.clas_gim_cj.10.30$biom)
b.clas_gim_cj.30.50<-bio.gim.cj [bio.gim.cj$DAP>=30 & bio.gim.cj$DAP<50,]
b.g.lar=sum(b.clas_gim_cj.30.50$biom)
b.clas_gim_cj.50<-bio.gim.cj [bio.gim.cj$DAP>=50,]
b.g.x.larg=sum(b.clas_gim_cj.50$biom)

b.clas_ang_cj.10<-bio.ang.cj [bio.ang.cj$DAP<10,]
b.smal=sum(b.clas_ang_cj.10$biom)
b.clas_ang_cj.10.30<-bio.ang.cj [bio.ang.cj$DAP>=10 & bio.ang.cj$DAP<30,]
b.med=sum(b.clas_ang_cj.10.30$biom)
b.clas_ang_cj.30.50<-bio.ang.cj [bio.ang.cj$DAP>=30 & bio.ang.cj$DAP<50,]
b.lar=sum(b.clas_ang_cj.30.50$biom)
b.clas_ang_cj.50<-bio.ang.cj [bio.ang.cj$DAP>=50,]
b.x.lar=sum(b.clas_ang_cj.50$biom)


b.s.a=sum(bio.ang.cj$biom)
b.s.g=sum(bio.gim.cj$biom)
b.p.a=(c(b.smal,b.med,b.lar,b.x.lar)/b.s.a)*100
b.p.g=(c(b.g.smal,b.g.med,b.g.lar,b.g.x.larg)/b.s.g)*100


############################################################
############################################################
############################################################
############################################################
#####################BAEPENDI###############################

#############
###Limpeza dados
#############
bio.bp <- bio.bp [!str_ends(bio.bp$Gen,"aceae"),]
dads.gim.bp<- bio.bp[bio.bp$Filo=="Gim",]#separa gimnosperma
dads.ang.bp<- bio.bp[bio.bp$Filo!="Gim",]#retirando gimnosperma
dads.ang.bp<- dads.ang.bp[dads.ang.bp$Filo!="Saman",]#retirando samambaia


#############################################################
##########################classes DAP########################


clas_gim_bp.10<-dads.gim.bp [dads.gim.bp$DAP<10,]
g.smal_2=length(clas_gim_bp.10$DAP)
clas_gim_bp.10.30<-dads.gim.bp [dads.gim.bp$DAP>=10 & dads.gim.bp$DAP<30,]
g.med_2=length(clas_gim_bp.10.30$DAP)
clas_gim_bp.30.50<-dads.gim.bp [dads.gim.bp$DAP>=30 & dads.gim.bp$DAP<50,]
g.lar_2=length(clas_gim_bp.30.50$DAP)
clas_gim_bp.50<-dads.gim.bp [dads.gim.bp$DAP>=50,]
g.x.larg_2=length(clas_gim_bp.50$DAP)

clas_ang_bp.10<-dads.ang.bp [dads.ang.bp$DAP<10,]
smal_2=length(clas_ang_bp.10$DAP)
clas_ang_bp.10.30<-dads.ang.bp [dads.ang.bp$DAP>=10 & dads.ang.bp$DAP<30,]
med_2=length(clas_ang_bp.10.30$DAP)
clas_ang_bp.30.50<-dads.ang.bp [dads.ang.bp$DAP>=30 & dads.ang.bp$DAP<50,]
lar_2=length(clas_ang_bp.30.50$DAP)
clas_ang_bp.50<-dads.ang.bp [dads.ang.bp$DAP>=50,]
x.lar_2=length(clas_ang_bp.50$DAP)

s.a_2=sum(smal_2,med_2,lar_2,x.lar_2)
s.g_2=sum(g.smal_2,g.med_2,g.lar_2,g.x.larg_2)
p.a_2=(c(smal_2,med_2,lar_2,x.lar_2)/s.a_2)*100
p.g_2=(c(g.smal_2,g.med_2,g.lar_2,g.x.larg_2)/s.g_2)*100
a.b.a_2=sum(dads.ang.bp$DAP)
a.b.g_2=sum(dads.gim.bp$DAP)
(c(a.b.a_2,a.b.g_2)/sum(a.b.a_2,a.b.g_2))*100



############################################################
############################################################
############################################################
##Est.Altura
#Ang
#weibull
a_2=27.188
b_2=0.091
c_2=0.738
D_2=dads.ang.bp$DAP
dads.ang.bp$Alt.E= a_2*(1-exp(-b_2*(D_2^c_2)))

###
###
#Gim
a.g_2=25.9889
b.g_2=19.9290
g.D_2=dads.gim.bp$DAP
dads.gim.bp$Alt.E = (1.3+a.g_2*exp(-(b.g_2/g.D_2)))


############################################################
############################################################
############################################################
##Densidade

Dens.bp= getWoodDensity(genus=dads.ang.bp$Gen,
                        species=dads.ang.bp$Spp)

dads.ang.bp$DensM <- Dens.bp$meanWD
dads.ang.bp$Lvl.D <- Dens.bp$levelWD

#write.table (dads.ang.bp,file="dads.ang.bp.csv",
# dec=",",col.names=TRUE)#função exporta a tabela
#caso queira comparar densidade
#com outro banco de dados

##################################################
#####inserindo densidade (Doutorado Gabriel
#####Martocos Oliveira "Densidade da madeira
#####em minas gerais")
##################################################



dads.ang.bp [dads.ang.bp$Gen == "Clethra",11] <- 0.404  # pg 49 linha 78
dads.ang.bp [dads.ang.bp$Gen == "Myrcia" & dads.ang.bp$Spp == "guianensis"  ,11] <- 0.545  # pg 52 linha 212
dads.ang.bp [dads.ang.bp$Gen == "Myrsine" & dads.ang.bp$Spp == "coriacea"  ,11] <- 0.545  # pg 52 linha 218
dads.ang.bp [dads.ang.bp$Gen == "Myrsine" & dads.ang.bp$Spp == "umbellata"  ,11] <- 0.602  # pg 52 linha 220
dads.ang.bp [dads.ang.bp$Gen == "Ocotea" & dads.ang.bp$Spp == "pulchella"  ,11] <- 0.593  # pg 53 linha 230
dads.ang.bp [dads.ang.bp$Gen == "Symplocos" & dads.ang.bp$Spp == "celastrinea"  ,11] <- 0.427  # pg 55 linha 305

###################
######média ponderada (Por indivíduo) caso tenha
######inserido novos dados
###################

dads.s.datset.bp <- dads.ang.bp [dads.ang.bp$Lvl.D != "dataset" ,]

dads.s.datset.bp$bino <- paste(dads.s.datset.bp$Gen,dads.s.datset.bp$Spp)

dens_2 <- dads.s.datset.bp[!duplicated (dads.s.datset.bp$bino),]
dens_2 <- arrange(dens_2,bino)
spp.n_2 =count (dads.s.datset.bp,bino)
spp.n_2 <- arrange(spp.n_2,bino)

dads.ang.bp [dads.ang.bp$Lvl.D == "dataset" ,11] <- meanp (dens_2$DensM,spp.n_2$n
                                                           ,dads.s.datset.bp$bino,
                                                           as_numeric=TRUE)


#View(dads.ang.bp)
###Dens gimnosperma

Dens.bp.2= getWoodDensity(genus=dads.gim.bp$Gen,
                          species=dads.gim.bp$Spp)
#head (Dens.bp.2)
#tail (Dens.bp.2)
dads.gim.bp$DensM <- Dens.bp.2$meanWD
dads.gim.bp$Lvl.D <- Dens.bp.2$levelWD
#tail (Dens.bp.2)


############################################################
############################################################
############################################################
##Biomassa


##Gim
x_2=0.4141 #Souza e Longhi
a_2= 111.7988
b_2= -15.5317
c_2= 0.8544
d_2 = 0.0180
g.D_2= c(dads.gim.bp$DAP)
g.H_2=c(dads.gim.bp$Alt)
dads.gim.bp$biom= (x_2*(a_2+b_2*c(g.D_2)+c_2*c(g.D_2^2)+d_2*((c(g.D_2^2))*g.H_2)))

##Ang

dads.ang.bp$biom= (computeAGB(D=dads.ang.bp$DAP,
                              WD=dads.ang.bp$DensM,
                              H=dads.ang.bp$Alt.E))*1000
head (dads.gim.bp)
head (dads.ang.bp)


bio.bp =rbind (dads.ang.bp,dads.gim.bp)
sum (bio.bp$biom)
######################
######################
######################
######################
######################DAP xBIO

bio.gim.bp <-bio.bp[bio.bp$Filo=="Gim",]
bio.ang.bp <-bio.bp[bio.bp$Filo!="Gim",]

#View (bio.ang.bp)

b.clas_gim_bp.10<-bio.gim.bp [bio.gim.bp$DAP<10,]
b.g.smal_2=sum(b.clas_gim_bp.10$biom)
b.clas_gim_bp.10.30<-bio.gim.bp [bio.gim.bp$DAP>=10 & bio.gim.bp$DAP<30,]
b.g.med_2=sum(b.clas_gim_bp.10.30$biom)
b.clas_gim_bp.30.50<-bio.gim.bp [bio.gim.bp$DAP>=30 & bio.gim.bp$DAP<50,]
b.g.lar_2=sum(b.clas_gim_bp.30.50$biom)
b.clas_gim_bp.50<-bio.gim.bp [bio.gim.bp$DAP>=50,]
b.g.x.larg_2=sum(b.clas_gim_bp.50$biom)

b.clas_ang_bp.10<-bio.ang.bp [bio.ang.bp$DAP<10,]
b.smal_2=sum(b.clas_ang_bp.10$biom)
b.clas_ang_bp.10.30<-bio.ang.bp [bio.ang.bp$DAP>=10 & bio.ang.bp$DAP<30,]
b.med_2=sum(b.clas_ang_bp.10.30$biom)
b.clas_ang_bp.30.50<-bio.ang.bp [bio.ang.bp$DAP>=30 & bio.ang.bp$DAP<50,]
b.lar_2=sum(b.clas_ang_bp.30.50$biom)
b.clas_ang_bp.50<-bio.ang.bp [bio.ang.bp$DAP>=50,]
b.x.lar_2=sum(b.clas_ang_bp.50$biom)


b.s.a_2=sum(bio.ang.bp$biom)
b.s.g_2=sum(bio.gim.bp$biom)
b.p.a_2=(c(b.smal_2,b.med_2,b.lar_2,b.x.lar_2)/b.s.a_2)*100
b.p.g_2=(c(b.g.smal_2,b.g.med_2,b.g.lar_2,b.g.x.larg_2)/b.s.g_2)*100


############################################################
############################################################
############################################################
############################################################
###################FAZ. BARTIRA#############################

#############
###Limpeza dados
#############
bio.Fbar <- bio.Fbar [!str_ends(bio.Fbar$Gen,"aceae"),]
dads.gim.Fbar<- bio.Fbar[bio.Fbar$Filo=="Gim",]#separa gimnosperma
dads.ang.Fbar<- bio.Fbar[bio.Fbar$Filo!="Gim",]#retirando gimnosperma
dads.ang.Fbar<- dads.ang.Fbar[dads.ang.Fbar$Filo!="Saman",]#retirando samambaia


#############################################################
##########################classes DAP########################


clas_gim_Fbar.10<-dads.gim.Fbar [dads.gim.Fbar$DAP<10,]
g.smal_3=length(clas_gim_Fbar.10$DAP)
clas_gim_Fbar.10.30<-dads.gim.Fbar [dads.gim.Fbar$DAP>=10 & dads.gim.Fbar$DAP<30,]
g.med_3=length(clas_gim_Fbar.10.30$DAP)
clas_gim_Fbar.30.50<-dads.gim.Fbar [dads.gim.Fbar$DAP>=30 & dads.gim.Fbar$DAP<50,]
g.lar_3=length(clas_gim_Fbar.30.50$DAP)
clas_gim_Fbar.50<-dads.gim.Fbar [dads.gim.Fbar$DAP>=50,]
g.x.larg_3=length(clas_gim_Fbar.50$DAP)

clas_ang_Fbar.10<-dads.ang.Fbar [dads.ang.Fbar$DAP<10,]
smal_3=length(clas_ang_Fbar.10$DAP)
clas_ang_Fbar.10.30<-dads.ang.Fbar [dads.ang.Fbar$DAP>=10 & dads.ang.Fbar$DAP<30,]
med_3=length(clas_ang_Fbar.10.30$DAP)
clas_ang_Fbar.30.50<-dads.ang.Fbar [dads.ang.Fbar$DAP>=30 & dads.ang.Fbar$DAP<50,]
lar_3=length(clas_ang_Fbar.30.50$DAP)
clas_ang_Fbar.50<-dads.ang.Fbar [dads.ang.Fbar$DAP>=50,]
x.lar_3=length(clas_ang_Fbar.50$DAP)

s.a_3=sum(smal_3,med_3,lar_3,x.lar_3)
s.g_3=sum(g.smal_3,g.med_3,g.lar_3,g.x.larg_3)
p.a_3=(c(smal_3,med_3,lar_3,x.lar_3)/s.a_3)*100
p.g_3=(c(g.smal_3,g.med_3,g.lar_3,g.x.larg_3)/s.g_3)*100
a.b.a_3=sum(dads.ang.Fbar$DAP)
a.b.g_3=sum(dads.gim.Fbar$DAP)
(c(a.b.a_3,a.b.g_3)/sum(a.b.a_3,a.b.g_3))*100



############################################################
############################################################
############################################################
##Est.Altura
#Ang
#weibull
a_3=27.188
b_3=0.091
c_3=0.738
D_3=dads.ang.Fbar$DAP
dads.ang.Fbar$Alt.E= a_3*(1-exp(-b_3*(D_3^c_3)))

###
###
#Gim
a.g_3=25.9889
b.g_3=19.9290
g.D_3=dads.gim.Fbar$DAP
dads.gim.Fbar$Alt.E = (1.3+a.g_3*exp(-(b.g_3/g.D_3)))


############################################################
############################################################
############################################################
##Densidade

Dens.Fbar= getWoodDensity(genus=dads.ang.Fbar$Gen,
                          species=dads.ang.Fbar$Spp)

dads.ang.Fbar$DensM <- Dens.Fbar$meanWD
dads.ang.Fbar$Lvl.D <- Dens.Fbar$levelWD

#write.table (dads.ang.Fbar,file="dads.ang.Fbar.csv",
# dec=",",col.names=TRUE)#função exporta a tabela
#caso queira comparar densidade
#com outro banco de dados

##################################################
#####inserindo densidade (Doutorado Gabriel
#####Martocos Oliveira "Densidade da madeira
#####em minas gerais")
##################################################



dads.ang.Fbar [dads.ang.Fbar$Gen == "Cabralea",11] <- 0.468  # pg 48 linha 54
dads.ang.Fbar [dads.ang.Fbar$Gen == "Calyptranthes" & dads.ang.Fbar$Spp == "widgreniana"  ,11] <- 0.657  # pg 48 linha 59
dads.ang.Fbar [dads.ang.Fbar$Gen == "Casearia" & dads.ang.Fbar$Spp == "decandra"  ,11] <- 0.492  # pg 48 linha 63
dads.ang.Fbar [dads.ang.Fbar$Gen == "Casearia" & dads.ang.Fbar$Spp == "obliqua"  ,11] <- 0.603  # pg 48 linha 65
dads.ang.Fbar [dads.ang.Fbar$Gen == "Casearia" & dads.ang.Fbar$Spp == "sylvestris"  ,11] <- 0.592  # pg 48 linha 67
dads.ang.Fbar [dads.ang.Fbar$Gen == "Cedrela",11] <- 0.678  # pg 48 linha 72
dads.ang.Fbar [dads.ang.Fbar$Gen == "Clethra",11] <- 0.404  # pg 49 linha 78
dads.ang.Fbar [dads.ang.Fbar$Gen == "Guapira",11] <- 0.349  # pg 50 linha 142
dads.ang.Fbar [dads.ang.Fbar$Gen == "Handroanthus",11] <- 0.672  # pg 50 linha 142
dads.ang.Fbar [dads.ang.Fbar$Gen == "Ilex" & dads.ang.Fbar$Spp == "dumosa"  ,11] <- 0.527  # pg 48 linha 67
dads.ang.Fbar [dads.ang.Fbar$Gen == "Ilex" & dads.ang.Fbar$Spp == "sylvestris"  ,11] <- 0.592  # pg 48 linha 67
dads.ang.Fbar [dads.ang.Fbar$Gen == "Lafoensia",11] <- 0.575  # pg 51 linha 174
dads.ang.Fbar [dads.ang.Fbar$Gen == "Leucochloron",11] <- 0.601  # pg 51 linha 179
dads.ang.Fbar [dads.ang.Fbar$Gen == "Machaerium",11] <- 0.684  # pg 52 linha 195
dads.ang.Fbar [dads.ang.Fbar$Gen == "Miconia" & dads.ang.Fbar$Spp == "pusiliflora"  ,11] <- 0.593  # pg 53 linha 204
dads.ang.Fbar [dads.ang.Fbar$Gen == "Mollinedia",11] <- 0.507  # pg 52 linha 208
dads.ang.Fbar [dads.ang.Fbar$Gen == "Myrcia" & dads.ang.Fbar$Spp == "guianensis"  ,11] <- 0.545 # pg 53 linha 204
dads.ang.Fbar [dads.ang.Fbar$Gen == "Myrcia" & dads.ang.Fbar$Spp == "splendens"  ,11] <- 0.629 # pg 53 linha 204
dads.ang.Fbar [dads.ang.Fbar$Gen == "Myrsine",11] <- 0.602  # pg 52 linha 208
dads.ang.Fbar [dads.ang.Fbar$Gen == "Nectandra" & dads.ang.Fbar$Spp == "oppositifolia"  ,11] <- 0.452 # pg 53 linha 204
dads.ang.Fbar [dads.ang.Fbar$Gen == "Ocotea" & dads.ang.Fbar$Spp == "bicolor"  ,11] <- 0.628 # pg 52 linha 223
dads.ang.Fbar [dads.ang.Fbar$Gen == "Ocotea" & dads.ang.Fbar$Spp == "corymbosa"  ,11] <- 0.574 # pg 53 linha 225
dads.ang.Fbar [dads.ang.Fbar$Gen == "Ocotea" & dads.ang.Fbar$Spp == "diospyrifolia"  ,11] <- 0.565 # pg 53 linha 225
dads.ang.Fbar [dads.ang.Fbar$Gen == "Ocotea" & dads.ang.Fbar$Spp == "odorifera"  ,11] <- 0.511 # pg 53 linha 229
dads.ang.Fbar [dads.ang.Fbar$Gen == "Ocotea" & dads.ang.Fbar$Spp == "pulchella"  ,11] <- 0.593 # pg 53 linha 229
dads.ang.Fbar [dads.ang.Fbar$Gen == "Persea",11] <- 0.656  # pg 53 linha 240
dads.ang.Fbar [dads.ang.Fbar$Gen == "Piptocarpha" & dads.ang.Fbar$Spp == "macropoda"  ,11] <- 0.601 # pg 53 linha 241
dads.ang.Fbar [dads.ang.Fbar$Gen == "Psychotria",11] <- 0.379  # pg 53 linha 240
dads.ang.Fbar [dads.ang.Fbar$Gen == "Roupala",11] <- 0.626  # pg 53 linha 240
dads.ang.Fbar [dads.ang.Fbar$Gen == "Schefflera",11] <- 0.531  # pg 54 linha 283
dads.ang.Fbar [dads.ang.Fbar$Gen == "Siphoneugena" & dads.ang.Fbar$Spp == "densiflora"  ,11] <- 0.679 # pg 53 linha 241
dads.ang.Fbar [dads.ang.Fbar$Gen == "Siphoneugena" & dads.ang.Fbar$Spp == "densiflora"  ,12] <- "species"
dads.ang.Fbar [dads.ang.Fbar$Gen == "Solanum",11] <- 0.462  # pg 54 linha 292
dads.ang.Fbar [dads.ang.Fbar$Gen == "Vernonanthura",11] <- 0.374  # pg 55 linha 326
dads.ang.Fbar [dads.ang.Fbar$Gen == "Zanthoxylum",11] <- 0.537  # pg 55 linha 338

##################################################

###################
######média ponderada (Por indivíduo) caso tenha
######inserido novos dados
###################

dads.s.datset.Fbar <- dads.ang.Fbar [dads.ang.Fbar$Lvl.D != "dataset" ,]

dads.s.datset.Fbar$bino <- paste(dads.s.datset.Fbar$Gen,dads.s.datset.Fbar$Spp)

dens_3 <- dads.s.datset.Fbar[!duplicated (dads.s.datset.Fbar$bino),]
dens_3 <- arrange(dens_3,bino)
spp.n_3 =count (dads.s.datset.Fbar,bino)
spp.n_3 <- arrange(spp.n_3,bino)

dads.ang.Fbar [dads.ang.Fbar$Lvl.D == "dataset" ,11] <- meanp (dens_3$DensM,spp.n_3$n
                                                               ,dads.s.datset.Fbar$bino,
                                                               as_numeric=TRUE)


#View(dads.ang.bp)
###Dens gimnosperma

Dens.Fbar.2= getWoodDensity(genus=dads.gim.Fbar$Gen,
                            species=dads.gim.Fbar$Spp)
#head (Dens.bp.2)
#tail (Dens.bp.2)
dads.gim.Fbar$DensM <- Dens.Fbar.2$meanWD
dads.gim.Fbar$Lvl.D <- Dens.Fbar.2$levelWD
#tail (Dens.bp.2)


############################################################
############################################################
############################################################
##Biomassa


##Gim
x_3=0.4141 #Souza e Longhi
a_3= 111.7988
b_3= -15.5317
c_3= 0.8544
d_3 = 0.0180
g.D_3= c(dads.gim.Fbar$DAP)
g.H_3=c(dads.gim.Fbar$Alt)
dads.gim.Fbar$biom= (x_3*(a_3+b_3*c(g.D_3)+c_3*c(g.D_3^2)+d_3*((c(g.D_3^2))*g.H_3)))

##Ang

dads.ang.Fbar$biom= (computeAGB(D=dads.ang.Fbar$DAP,
                                WD=dads.ang.Fbar$DensM,
                                H=dads.ang.Fbar$Alt.E))*1000
#head (dads.gim.Fbar)
#head (dads.ang.Fbar)


bio.Fbar =rbind (dads.ang.Fbar,dads.gim.Fbar)

#tail (bio.Fbar)
######################
######################
######################
######################
######################DAP xBIO

bio.gim.Fbar<-bio.Fbar[bio.Fbar$Filo=="Gim",]
bio.ang.Fbar<-bio.Fbar[bio.Fbar$Filo!="Gim",]

#View (bio.ang.bp)

b.clas_gim_Fbar.10<-bio.gim.Fbar[bio.gim.Fbar$DAP<10,]
b.g.smal_3=sum(b.clas_gim_Fbar.10$biom)
b.clas_gim_Fbar.10.30<-bio.gim.Fbar[bio.gim.Fbar$DAP>=10 & bio.gim.Fbar$DAP<30,]
b.g.med_3=sum(b.clas_gim_Fbar.10.30$biom)
b.clas_gim_Fbar.30.50<-bio.gim.Fbar[bio.gim.Fbar$DAP>=30 & bio.gim.Fbar$DAP<50,]
b.g.lar_3=sum(b.clas_gim_Fbar.30.50$biom)
b.clas_gim_Fbar.50<-bio.gim.Fbar[bio.gim.Fbar$DAP>=50,]
b.g.x.larg_3=sum(b.clas_gim_Fbar.50$biom)

b.clas_ang_Fbar.10<-bio.ang.Fbar[bio.ang.Fbar$DAP<10,]
b.smal_3=sum(b.clas_ang_Fbar.10$biom)
b.clas_ang_Fbar.10.30<-bio.ang.Fbar[bio.ang.Fbar$DAP>=10 & bio.ang.Fbar$DAP<30,]
b.med_3=sum(b.clas_ang_Fbar.10.30$biom)
b.clas_ang_Fbar.30.50<-bio.ang.Fbar[bio.ang.Fbar$DAP>=30 & bio.ang.Fbar$DAP<50,]
b.lar_3=sum(b.clas_ang_Fbar.30.50$biom)
b.clas_ang_Fbar.50<-bio.ang.Fbar[bio.ang.Fbar$DAP>=50,]
b.x.lar_3=sum(b.clas_ang_Fbar.50$biom)


b.s.a_3=sum(bio.ang.Fbar$biom)
b.s.g_3=sum(bio.gim.Fbar$biom)
b.p.a_3=(c(b.smal_3,b.med_3,b.lar_3,b.x.lar_3)/b.s.a_3)*100
b.p.g_3=(c(b.g.smal_3,b.g.med_3,b.g.lar_3,b.g.x.larg_3)/b.s.g_3)*100



############################################################
############################################################
############################################################
############################################################
###################FAZ. SÃO FRANCISCO#######################
#############
###Limpeza dados
#############
bio.Fsf <- bio.Fsf [!str_ends(bio.Fsf$Gen,"aceae"),]
dads.gim.Fsf<- bio.Fsf[bio.Fsf$Filo=="Gim",]#separa gimnosperma
dads.ang.Fsf<- bio.Fsf[bio.Fsf$Filo!="Gim",]#retirando gimnosperma
dads.ang.Fsf<- dads.ang.Fsf[dads.ang.Fsf$Filo!="Saman",]#retirando samambaia


#############################################################
##########################classes DAP########################


clas_gim_Fsf.10<-dads.gim.Fsf [dads.gim.Fsf$DAP<10,]
g.smal_4=length(clas_gim_Fsf.10$DAP)
clas_gim_Fsf.10.30<-dads.gim.Fsf [dads.gim.Fsf$DAP>=10 & dads.gim.Fsf$DAP<30,]
g.med_4=length(clas_gim_Fsf.10.30$DAP)
clas_gim_Fsf.30.50<-dads.gim.Fsf [dads.gim.Fsf$DAP>=30 & dads.gim.Fsf$DAP<50,]
g.lar_4=length(clas_gim_Fsf.30.50$DAP)
clas_gim_Fsf.50<-dads.gim.Fsf [dads.gim.Fsf$DAP>=50,]
g.x.larg_4=length(clas_gim_Fsf.50$DAP)


clas_ang_Fsf.10<-dads.ang.Fsf [dads.ang.Fsf$DAP<10,]
smal_4=length(clas_ang_Fsf.10$DAP)
clas_ang_Fsf.10.30<-dads.ang.Fsf [dads.ang.Fsf$DAP>=10 & dads.ang.Fsf$DAP<30,]
med_4=length(clas_ang_Fsf.10.30$DAP)
clas_ang_Fsf.30.50<-dads.ang.Fsf [dads.ang.Fsf$DAP>=30 & dads.ang.Fsf$DAP<50,]
lar_4=length(clas_ang_Fsf.30.50$DAP)
clas_ang_Fsf.50<-dads.ang.Fsf [dads.ang.Fsf$DAP>=50,]
x.lar_4=length(clas_ang_Fsf.50$DAP)
s.a_4=sum(smal_4,med_4,lar_4,x.lar_4)
s.g_4=sum(g.smal_4,g.med_4,g.lar_4,g.x.larg_4)
p.a_4=(c(smal_4,med_4,lar_4,x.lar_4)/s.a_4)*100
p.g_4=(c(g.smal_4,g.med_4,g.lar_4,g.x.larg_4)/s.g_4)*100
a.b.a_4=sum(dads.ang.Fsf$DAP)
a.b.g_4=sum(dads.gim.Fsf$DAP)
(c(a.b.a_4,a.b.g_4)/sum(a.b.a_4,a.b.g_4))*100



############################################################
############################################################
############################################################
##Est.Altura
#Ang
#weibull
a_4=27.188
b_4=0.091
c_4=0.738
D_4=dads.ang.Fsf$DAP
dads.ang.Fsf$Alt.E= a_4*(1-exp(-b_4*(D_4^c_4)))

###
###
#Gim
a.g_4=25.9889
b.g_4=19.9290
g.D_4=dads.gim.Fsf$DAP
dads.gim.Fsf$Alt.E = (1.3+a.g_4*exp(-(b.g_4/g.D_4)))


############################################################
############################################################
############################################################
##Densidade

Dens.Fsf= getWoodDensity(genus=dads.ang.Fsf$Gen,
                         species=dads.ang.Fsf$Spp)

dads.ang.Fsf$DensM <- Dens.Fsf$meanWD
dads.ang.Fsf$Lvl.D <- Dens.Fsf$levelWD

#write.table (dads.ang.Fsf,file="dads.ang.Fsf.csv",
# dec=",",col.names=TRUE)#função exporta a tabela
##################################################
#####inserindo densidade (Doutorado Gabriel
#####Martocos Oliveira "Densidade da madeira
#####em minas gerais")
##################################################



dads.ang.Fsf [dads.ang.Fsf$Gen == "Calyptranthes",11] <- 0.657  # pg 48 linha 54
dads.ang.Fsf [dads.ang.Fsf$Gen == "Casearia",11] <- 0.492  # pg 48 linha 54
dads.ang.Fsf [dads.ang.Fsf$Gen == "Cedrella",11] <- 0.678  # pg 48 linha 54
dads.ang.Fsf [dads.ang.Fsf$Gen == "Clethra",11] <- 0.404  # pg 48 linha 54
dads.ang.Fsf [dads.ang.Fsf$Gen == "Clethra",11] <- 0.404  # pg 48 linha 54
dads.ang.Fsf [dads.ang.Fsf$Gen == "Ilex" & dads.ang.Fsf$Spp == "dumosa"  ,11] <- 0.527 # pg 53 linha 229
dads.ang.Fsf [dads.ang.Fsf$Gen == "Myrcia" & dads.ang.Fsf$Spp == "splendens"  ,11] <- 0.629 # pg 53 linh
dads.ang.Fsf [dads.ang.Fsf$Gen == "Myrsine",11] <- 0.602 # pg 48 linha 54
dads.ang.Fsf [dads.ang.Fsf$Gen == "Ocotea" & dads.ang.Fsf$Spp == "bicolor"  ,11] <- 0.628 # pg 53 linh
dads.ang.Fsf [dads.ang.Fsf$Gen == "Ocotea" & dads.ang.Fsf$Spp == "pulchella"  ,11] <- 0.593 # pg 53 linh
dads.ang.Fsf [dads.ang.Fsf$Gen == "Roupala",11] <- 0.626 # pg 48 linha 54
dads.ang.Fsf [dads.ang.Fsf$Gen == "Solanum" & dads.ang.Fsf$Spp == "swartzianum"  ,11] <- 0.567 # pg 53 linh

##################################################

###################

dads.s.datset.Fsf <- dads.ang.Fsf [dads.ang.Fsf$Lvl.D != "dataset" ,]

dads.s.datset.Fsf$bino <- paste(dads.s.datset.Fsf$Gen,dads.s.datset.Fsf$Spp)

dens_4 <- dads.s.datset.Fsf[!duplicated (dads.s.datset.Fsf$bino),]
dens_4 <- arrange(dens_4,bino)
spp.n_4 =count (dads.s.datset.Fsf,bino)
spp.n_4 <- arrange(spp.n_4,bino)

dads.ang.Fsf [dads.ang.Fsf$Lvl.D == "dataset" ,11] <- meanp (dens_4$DensM,spp.n_4$n
                                                             ,dads.s.datset.Fsf$bino,
                                                             as_numeric=TRUE)

#View(dads.ang.bp)
###Dens gimnosperma

Dens.Fsf.2= getWoodDensity(genus=dads.gim.Fsf$Gen,
                           species=dads.gim.Fsf$Spp)
#head (Dens.bp.2)
#tail (Dens.bp.2)
dads.gim.Fsf$DensM <- Dens.Fsf.2$meanWD
dads.gim.Fsf$Lvl.D <- Dens.Fsf.2$levelWD
#tail (Dens.bp.2)

###########################################################
############################################################
##Biomassa


##Gim
x_4=0.4141 #Souza e Longhi
a_4= 111.7988
b_4= -15.5317
c_4= 0.8544
d_4 = 0.0180
g.D_4= c(dads.gim.Fsf$DAP)
g.H_4=c(dads.gim.Fsf$Alt)
dads.gim.Fsf$biom= (x_4*(a_4+b_4*c(g.D_4)+c_4*c(g.D_4^2)+d_4*((c(g.D_4^2))*g.H_4)))

##Ang

dads.ang.Fsf$biom= (computeAGB(D=dads.ang.Fsf$DAP,
                               WD=dads.ang.Fsf$DensM,
                               H=dads.ang.Fsf$Alt.E))*1000


bio.Fsf =rbind (dads.ang.Fsf,dads.gim.Fsf)


######################
######################
######################
######################
######################DAP xBIO

bio.gim.Fsf<-bio.Fsf[bio.Fsf$Filo=="Gim",]
bio.ang.Fsf<-bio.Fsf[bio.Fsf$Filo!="Gim",]

#View (bio.ang.bp)

b.clas_gim_Fsf.10<-bio.gim.Fsf[bio.gim.Fsf$DAP<10,]
b.g.smal_4=sum(b.clas_gim_Fsf.10$biom)
b.clas_gim_Fsf.10.30<-bio.gim.Fsf[bio.gim.Fsf$DAP>=10 & bio.gim.Fsf$DAP<30,]
b.g.med_4=sum(b.clas_gim_Fsf.10.30$biom)
b.clas_gim_Fsf.30.50<-bio.gim.Fsf[bio.gim.Fsf$DAP>=30 & bio.gim.Fsf$DAP<50,]
b.g.lar_4=sum(b.clas_gim_Fsf.30.50$biom)
b.clas_gim_Fsf.50<-bio.gim.Fsf[bio.gim.Fsf$DAP>=50,]
b.g.x.larg_4=sum(b.clas_gim_Fsf.50$biom)

b.clas_ang_Fsf.10<-bio.ang.Fsf[bio.ang.Fsf$DAP<10,]
b.smal_4=sum(b.clas_ang_Fsf.10$biom)
b.clas_ang_Fsf.10.30<-bio.ang.Fsf[bio.ang.Fsf$DAP>=10 & bio.ang.Fsf$DAP<30,]
b.med_4=sum(b.clas_ang_Fsf.10.30$biom)
b.clas_ang_Fsf.30.50<-bio.ang.Fsf[bio.ang.Fsf$DAP>=30 & bio.ang.Fsf$DAP<50,]
b.lar_4=sum(b.clas_ang_Fsf.30.50$biom)
b.clas_ang_Fsf.50<-bio.ang.Fsf[bio.ang.Fsf$DAP>=50,]
b.x.lar_4=sum(b.clas_ang_Fsf.50$biom)


b.s.a_4=sum(bio.ang.Fsf$biom)
b.s.g_4=sum(bio.gim.Fsf$biom)
b.p.a_4=(c(b.smal_4,b.med_4,b.lar_4,b.x.lar_4)/b.s.a_4)*100
b.p.g_4=(c(b.g.smal_4,b.g.med_4,b.g.lar_4,b.g.x.larg_4)/b.s.g_4)*100

###########################################################
###########################################################
###################ITABERÁ#################################

#############
###Limpeza dados
#############
bio.It <- bio.It [!str_ends(bio.It$Gen,"aceae"),]
bio.It [bio.It$Fam=="Arecaceae",8] <- "Palm"
dads.gim.It<- bio.It[bio.It$Filo=="Gim",]#separa gimnosperma
dads.ang.It<- bio.It[bio.It$Filo!="Gim",]#retirando gimnosperma
dads.ang.It<- dads.ang.It[dads.ang.It$Filo!="Saman",]#retirando samambaia




#############################################################
##########################classes DAP########################


clas_gim_It.10<-dads.gim.It [dads.gim.It$DAP<10,]
g.smal_5=length(clas_gim_It.10$DAP)
clas_gim_It.10.30<-dads.gim.It [dads.gim.It$DAP>=10 & dads.gim.It$DAP<30,]
g.med_5=length(clas_gim_It.10.30$DAP)
clas_gim_It.30.50<-dads.gim.It [dads.gim.It$DAP>=30 & dads.gim.It$DAP<50,]
g.lar_5=length(clas_gim_It.30.50$DAP)
clas_gim_It.50<-dads.gim.It [dads.gim.It$DAP>=50,]
g.x.larg_5=length(clas_gim_It.50$DAP)


clas_ang_It.10<-dads.ang.It [dads.ang.It$DAP<10,]
smal_5=length(clas_ang_It.10$DAP)
clas_ang_It.10.30<-dads.ang.It [dads.ang.It$DAP>=10 & dads.ang.It$DAP<30,]
med_5=length(clas_ang_It.10.30$DAP)
clas_ang_It.30.50<-dads.ang.It [dads.ang.It$DAP>=30 & dads.ang.It$DAP<50,]
lar_5=length(clas_ang_It.30.50$DAP)
clas_ang_It.50<-dads.ang.It [dads.ang.It$DAP>=50,]
x.lar_5=length(clas_ang_It.50$DAP)
s.a_5=sum(smal_5,med_5,lar_5,x.lar_5)
s.g_5=sum(g.smal_5,g.med_5,g.lar_5,g.x.larg_5)
p.a_5=(c(smal_5,med_5,lar_5,x.lar_5)/s.a_5)*100
p.g_5=(c(g.smal_5,g.med_5,g.lar_5,g.x.larg_5)/s.g_5)*100
a.b.a_5=sum(dads.ang.It$DAP)
a.b.g_5=sum(dads.gim.It$DAP)
(c(a.b.a_5,a.b.g_5)/sum(a.b.a_5,a.b.g_5))*100



############################################################
############################################################
############################################################
##Est.Altura
#Ang
#weibull
a_5=27.188
b_5=0.091
c_5=0.738
D_5=dads.ang.It$DAP
dads.ang.It$Alt.E= a_5*(1-exp(-b_5*(D_5^c_5)))

###
###
#Gim
a.g_5=25.9889
b.g_5=19.9290
g.D_5=dads.gim.It$DAP
dads.gim.It$Alt.E = (1.3+a.g_5*exp(-(b.g_5/g.D_5)))


############################################################
############################################################
############################################################
##Densidade

Dens.It= getWoodDensity(genus=dads.ang.It$Gen,
                        species=dads.ang.It$Spp)

dads.ang.It$DensM <- Dens.It$meanWD
dads.ang.It$Lvl.D <- Dens.It$levelWD

#write.table (dads.ang.It,file="dads.ang.It.csv",
# dec=",",col.names=TRUE)#função exporta a tabela
##################################################
#####inserindo densidade (Doutorado Gabriel
#####Martocos Oliveira "Densidade da madeira
#####em minas gerais")
##################################################



dads.ang.It [dads.ang.It$Gen == "Alchornea" & dads.ang.It$Spp == "glandulosa",11] <- 0.380  # pg 48 linha 54
dads.ang.It [dads.ang.It$Gen == "Alchornea" & dads.ang.It$Spp == "triplinervia",11] <- 0.440  # pg 48 linha 54
dads.ang.It [dads.ang.It$Gen == "Allophylus",11] <- 0.518  # pg 48 linha 54
dads.ang.It [dads.ang.It$Gen == "Aspidosperma" & dads.ang.It$Spp == "polyneurum",11] <- 0.643  # pg 48 linha 54
dads.ang.It [dads.ang.It$Gen == "Astronium" & dads.ang.It$Spp == "graveolens",11] <- 0.531 # pg 48 linha 54
dads.ang.It [dads.ang.It$Gen == "Cabralea",11] <- 0.468 # pg 48 linha 54
dads.ang.It [dads.ang.It$Gen == "Casearia" & dads.ang.It$Spp == "decandra",11] <- 0.492 # pg 48 linha 54
dads.ang.It [dads.ang.It$Gen == "Casearia" & dads.ang.It$Spp == "obliqua",11] <- 0.603 # pg 48 linha 54
dads.ang.It [dads.ang.It$Gen == "Casearia" & dads.ang.It$Spp == "sylvestris",11] <- 0.592 # pg 48 linha 54
dads.ang.It [dads.ang.It$Gen == "Cedrela" ,11] <- 0.678 # pg 48 linha 54
dads.ang.It [dads.ang.It$Gen == "Chrysophyllum" & dads.ang.It$Spp == "gonocarpum",11] <- 0.722 # pg 48 linha 54
dads.ang.It [dads.ang.It$Gen == "Clethra" ,11] <- 0.404 # pg 48 linha 54
dads.ang.It [dads.ang.It$Gen == "Copaifera" & dads.ang.It$Spp == "langsdorffii",11] <- 0.584 # pg 48 linha 54
dads.ang.It [dads.ang.It$Gen == "Croton" ,11] <- 0.475 # pg 48 linha 54
dads.ang.It [dads.ang.It$Gen == "Cupania" & dads.ang.It$Spp == "vernalis",11] <- 0.723 # pg 48 linha 54
dads.ang.It [dads.ang.It$Gen == "Guapira" & dads.ang.It$Spp == "opposita",11] <- 0.349 # pg 48 linha 54
dads.ang.It [dads.ang.It$Gen == "Inga" & dads.ang.It$Spp == "marginata",11] <- 0.462 # pg 48 linha 54
dads.ang.It [dads.ang.It$Gen == "Machaerium" & dads.ang.It$Spp == "nyctitans",11] <- 0.634 # pg 48 linha 54
dads.ang.It [dads.ang.It$Gen == "Miconia" & dads.ang.It$Spp == "tristis",11] <- 0.594 # pg 48 linha 54
dads.ang.It [dads.ang.It$Gen == "Mollinedia" & dads.ang.It$Spp == "argyrogyna",11] <- 0.507 # pg 48 linha 54
dads.ang.It [dads.ang.It$Gen == "Mollinedia" & dads.ang.It$Spp == "argyrogyna",11] <- 0.507 # pg 48 linha 54
dads.ang.It [dads.ang.It$Gen == "Nectandra" & dads.ang.It$Spp == "lanceolata",11] <- 0.433 # pg 48 linha 54
dads.ang.It [dads.ang.It$Gen == "Nectandra" & dads.ang.It$Spp == "oppositifolia",11] <- 0.452 # pg 48 linha 54
dads.ang.It [dads.ang.It$Gen == "Ocotea" & dads.ang.It$Spp == "bicolor",11] <- 0.628 # pg 48 linha 54
dads.ang.It [dads.ang.It$Gen == "Ocotea" & dads.ang.It$Spp == "corymbosa",11] <- 0.574 # pg 48 linha 54
dads.ang.It [dads.ang.It$Gen == "Ocotea" & dads.ang.It$Spp == "diospyrifolia",11] <- 0.565 # pg 48 linha 54
dads.ang.It [dads.ang.It$Gen == "Ocotea" & dads.ang.It$Spp == "indecora",11] <- 0.663 # pg 48 linha 54
dads.ang.It [dads.ang.It$Gen == "Pera" ,11] <- 0.634 # pg 48 linha 54
dads.ang.It [dads.ang.It$Gen == "Persea" ,11] <- 0.656 # pg 48 linha 54
dads.ang.It [dads.ang.It$Gen == "Piptadenia" ,11] <- 0.659 # pg 48 linha 54
dads.ang.It [dads.ang.It$Gen == "Pouteria" ,11] <- 0.707 # pg 48 linha 54
dads.ang.It [dads.ang.It$Gen == "Pseudobombax" & dads.ang.It$Spp == "grandiflorum",11] <- 0.373 # pg 48 linha 54
dads.ang.It [dads.ang.It$Gen == "Schefflera" & dads.ang.It$Spp == "angustissima",11] <- 0.454 # pg 48 linha 54
dads.ang.It [dads.ang.It$Gen == "Schefflera" & dads.ang.It$Spp == "calva",11] <- 0.531 # pg 48 linha 54
dads.ang.It [dads.ang.It$Gen == "Solanum" & dads.ang.It$Spp == "bullatum",11] <- 0.462 # pg 48 linha 54
dads.ang.It [dads.ang.It$Gen == "Solanum" & dads.ang.It$Spp == "swartzianum",11] <- 0.567 # pg 48 linha 54
dads.ang.It [dads.ang.It$Gen == "Styrax" & dads.ang.It$Spp == "camporum",11] <- 0.788 # pg 48 linha 54
dads.ang.It [dads.ang.It$Gen == "Tapirira" ,11] <- 0.520 # pg 48 linha 54
dads.ang.It [dads.ang.It$Gen == "Trichilia" & dads.ang.It$Spp == "catigua",11] <- 0.609 # pg 48 linha 54
dads.ang.It [dads.ang.It$Gen == "Trichilia" & dads.ang.It$Spp == "pallida",11] <- 0.686 # pg 48 linha 54
dads.ang.It [dads.ang.It$Gen == "Vochysia" ,11] <- 0.462 # pg 48 linha 54

##################################################

###################

dads.s.datset.It <- dads.ang.It [dads.ang.It$Lvl.D != "dataset" ,]

dads.s.datset.It$bino <- paste(dads.s.datset.It$Gen,dads.s.datset.It$Spp)

dens_5 <- dads.s.datset.It[!duplicated (dads.s.datset.It$bino),]
dens_5 <- arrange(dens_5,bino)
spp.n_5 =count (dads.s.datset.It,bino)
spp.n_5 <- arrange(spp.n_5,bino)

dads.ang.It [dads.ang.It$Lvl.D == "dataset" ,11] <- meanp (dens_5$DensM,spp.n_5$n
                                                           ,dads.s.datset.It$bino,
                                                           as_numeric=TRUE)

#View(dads.ang.bp)
###Dens gimnosperma


Dens.It.2= getWoodDensity(genus=dads.gim.It$Gen,
                          species=dads.gim.It$Spp)
#head (Dens.bp.2)
#tail (Dens.bp.2)
dads.gim.It$DensM <- Dens.It.2$meanWD
dads.gim.It$Lvl.D <- Dens.It.2$levelWD
#tail (Dens.bp.2)




###########################################################
############################################################
##Biomassa


##Gim
x_5=0.4141 #Souza e Longhi
a_5= 111.7988
b_5= -15.5317
c_5= 0.8544
d_5 = 0.0180
g.D_5= c(dads.gim.It$DAP)
g.H_5=c(dads.gim.It$Alt)
dads.gim.It$biom= (x_5*(a_5+b_5*c(g.D_5)+c_5*c(g.D_5^2)+d_5*((c(g.D_5^2))*g.H_5)))

##Ang
ang.It <-dads.ang.It [dads.ang.It$Gen != "Euterpe" & dads.ang.It$Gen != "Syagrus",]

ang.It$biom= (computeAGB(D=ang.It$DAP,
                         WD=ang.It$DensM,
                         H=ang.It$Alt.E))*1000
##Palm

palm.It <-dads.ang.It [dads.ang.It$Gen == "Euterpe" | dads.ang.It$Gen == "Syagrus",]


a.p= -3.3488
b.p= 2.7483
D.p=palm.It$DAP



palm.It$biom = exp(a.p+b.p*log(D.p))

bio.It =rbind (ang.It,dads.gim.It,palm.It)

######################
######################
######################
######################
######################DAP xBIO

bio.gim.It<-bio.It[bio.It$Filo=="Gim",]
bio.ang.It<-bio.It[bio.It$Filo!="Gim",]

#View (bio.ang.bp)

b.clas_gim_It.10<-bio.gim.It[bio.gim.It$DAP<10,]
b.g.smal_5=sum(b.clas_gim_It.10$biom)
b.clas_gim_It.10.30<-bio.gim.It[bio.gim.It$DAP>=10 & bio.gim.It$DAP<30,]
b.g.med_5=sum(b.clas_gim_It.10.30$biom)
b.clas_gim_It.30.50<-bio.gim.It[bio.gim.It$DAP>=30 & bio.gim.It$DAP<50,]
b.g.lar_5=sum(b.clas_gim_It.30.50$biom)
b.clas_gim_It.50<-bio.gim.It[bio.gim.It$DAP>=50,]
b.g.x.larg_5=sum(b.clas_gim_It.50$biom)

b.clas_ang_It.10<-bio.ang.It[bio.ang.It$DAP<10,]
b.smal_5=sum(b.clas_ang_It.10$biom)
b.clas_ang_It.10.30<-bio.ang.It[bio.ang.It$DAP>=10 & bio.ang.It$DAP<30,]
b.med_5=sum(b.clas_ang_It.10.30$biom)
b.clas_ang_It.30.50<-bio.ang.It[bio.ang.It$DAP>=30 & bio.ang.It$DAP<50,]
b.lar_5=sum(b.clas_ang_It.30.50$biom)
b.clas_ang_It.50<-bio.ang.It[bio.ang.It$DAP>=50,]
b.x.lar_5=sum(b.clas_ang_It.50$biom)


b.s.a_5=sum(bio.ang.It$biom)
b.s.g_5=sum(bio.gim.It$biom)
b.p.a_5=(c(b.smal_5,b.med_5,b.lar_5,b.x.lar_5)/b.s.a_5)*100
b.p.g_5=(c(b.g.smal_5,b.g.med_5,b.g.lar_5,b.g.x.larg_5)/b.s.g_5)*100



############################################################
############################################################
############################################################
############################################################
#####################BARRA DO CHAPÉU########################

#############
###Limpeza dados
#############
bio.BC <- bio.BC [!str_ends(bio.BC$Gen,"aceae"),]
bio.BC[bio.BC$Filo=="Gem",8] <- "Gim"
bio.BC [bio.BC$Fam=="Arecaceae",8] <- "Palm"
dads.gim.bc<- bio.BC[bio.BC$Filo=="Gim",]#separa gimnosperma
dads.ang.bc<- bio.BC[bio.BC$Filo!="Gim",]#retirando gimnosperma
dads.ang.bc<- dads.ang.bc[dads.ang.bc$Filo!="Saman",]#retirando samambaia


#head (bio.BC)


#############################################################
##########################classes DAP########################


clas_gim_bc.10<-dads.gim.bc [dads.gim.bc$DAP<10,]
g.smal_6=length(clas_gim_bc.10$DAP)
clas_gim_bc.10.30<-dads.gim.bc [dads.gim.bc$DAP>=10 & dads.gim.bc$DAP<30,]
g.med_6=length(clas_gim_bc.10.30$DAP)
clas_gim_bc.30.50<-dads.gim.bc [dads.gim.bc$DAP>=30 & dads.gim.bc$DAP<50,]
g.lar_6=length(clas_gim_bc.30.50$DAP)
clas_gim_bc.50<-dads.gim.bc [dads.gim.bc$DAP>=50,]
g.x.larg_6=length(clas_gim_bc.50$DAP)

clas_ang_bc.10<-dads.ang.bc [dads.ang.bc$DAP<10,]
smal_6=length(clas_ang_bc.10$DAP)
clas_ang_bc.10.30<-dads.ang.bc [dads.ang.bc$DAP>=10 & dads.ang.bc$DAP<30,]
med_6=length(clas_ang_bc.10.30$DAP)
clas_ang_bc.30.50<-dads.ang.bc [dads.ang.bc$DAP>=30 & dads.ang.bc$DAP<50,]
lar_6=length(clas_ang_bc.30.50$DAP)
clas_ang_bc.50<-dads.ang.bc [dads.ang.bc$DAP>=50,]
x.lar_6=length(clas_ang_bc.50$DAP)
s.a_6=sum(smal_6,med_6,lar_6,x.lar_6)
s.g_6=sum(g.smal_6,g.med_6,g.lar_6,g.x.larg_6)
p.a_6=(c(smal_6,med_6,lar_6,x.lar_6)/s.a_6)*100
p.g_6 =(c(g.smal_6,g.med_6,g.lar_6,g.x.larg_6)/s.g_6)*100
a.b.a_6=sum(dads.ang.bc$DAP)
a.b.g_6=sum(dads.gim.bc$DAP)
(c(a.b.a_6,a.b.g_6)/sum(a.b.a_6,a.b.g_6))*100


############################################################
############################################################
############################################################
#####Est.Altura######
#Ang
#weibull
a_5=27.188
b_5=0.091
c_5=0.738
D_6=dads.ang.bc$DAP
dads.ang.bc$Alt.E= a_5*(1-exp(-b_5*(D_6^c_5)))

###
###
#Gim
a.g_5=25.9889
b.g_5=19.9290
g.D_6=dads.gim.bc$DAP
dads.gim.bc$Alt.E = (1.3+a.g_5*exp(-(b.g_5/g.D_6)))


############################################################
############################################################
############################################################
########Densidade######

Dens.bc= getWoodDensity(genus=dads.ang.bc$Gen,
                        species=dads.ang.bc$Spp)

dads.ang.bc$DensM <- Dens.bc$meanWD
dads.ang.bc$Lvl.D <- Dens.bc$levelWD


#write.table (dads.ang.bc,file="dads.ang.bc.csv",
# dec=",",col.names=TRUE)#função exporta a tabela

##################################################
#####inserindo densidade (Doutorado Gabriel
#####Martocos Oliveira "Densidade da madeira
#####em minas gerais")
##################################################

dads.ang.bc [dads.ang.bc$Gen == "Allophylus" & dads.ang.bc$Spp == "edulis",11] <- 0.518
dads.ang.bc [dads.ang.bc$Gen == "Cabralea" & dads.ang.bc$Spp == "canjerana",11] <- 0.468
dads.ang.bc [dads.ang.bc$Gen == "Casearia" & dads.ang.bc$Spp == "decandra",11] <- 0.492
dads.ang.bc [dads.ang.bc$Gen == "Casearia" & dads.ang.bc$Spp == "obliqua",11] <- 0.603
dads.ang.bc [dads.ang.bc$Gen == "Casearia" & dads.ang.bc$Spp == "sylvestris",11] <- 0.592
dads.ang.bc [dads.ang.bc$Gen == "Cedrela" & dads.ang.bc$Spp == "fissilis",11] <- 0.678
dads.ang.bc [dads.ang.bc$Gen == "Chrysophyllum" & dads.ang.bc$Spp == "marginatum",11] <- 0.568
dads.ang.bc [dads.ang.bc$Gen == "Cupania" & dads.ang.bc$Spp == "vernalis",11] <- 0.723
dads.ang.bc [dads.ang.bc$Gen == "Erythrina" & dads.ang.bc$Spp == "falcata",11] <- 0.305
dads.ang.bc [dads.ang.bc$Gen == "Eugenia" & dads.ang.bc$Spp == "involucrata",11] <- 0.567
dads.ang.bc [dads.ang.bc$Gen == "Guapira" & dads.ang.bc$Spp == "opposita",11] <- 0.349
dads.ang.bc [dads.ang.bc$Gen == "Guazuma" & dads.ang.bc$Spp == "ulmifolia",11] <- 0.337
dads.ang.bc [dads.ang.bc$Gen == "Machaerium" & dads.ang.bc$Spp == "nyctitans",11] <- 0.634
dads.ang.bc [dads.ang.bc$Gen == "Myrcia" & dads.ang.bc$Spp == "tomentosa",11] <- 0.637
dads.ang.bc [dads.ang.bc$Gen == "Ocotea" & dads.ang.bc$Spp == "bicolor",11] <- 0.628
dads.ang.bc [dads.ang.bc$Gen == "Ocotea" & dads.ang.bc$Spp == "brachybotra",11] <- 0.433
dads.ang.bc [dads.ang.bc$Gen == "Ocotea" & dads.ang.bc$Spp == "odorifera",11] <- 0.511
dads.ang.bc [dads.ang.bc$Gen == "Piptadenia" & dads.ang.bc$Spp == "gonoacantha",11] <- 0.659
dads.ang.bc [dads.ang.bc$Gen == "Pouteria" & dads.ang.bc$Spp == "gardneri",11] <- 0.707
dads.ang.bc [dads.ang.bc$Gen == "Senna" & dads.ang.bc$Spp == "multijuga",11] <- 0.527
dads.ang.bc [dads.ang.bc$Gen == "Vitex" & dads.ang.bc$Spp == "polygama",11] <- 0.652
dads.ang.bc [dads.ang.bc$Gen == "Zeyheria" & dads.ang.bc$Spp == "Zeyheria",11] <- 0.607



##################################################

###################

dads.s.datset.bc <- dads.ang.bc [dads.ang.bc$Lvl.D != "dataset" ,]

dads.s.datset.bc$bino <- paste(dads.s.datset.bc$Gen,dads.s.datset.bc$Spp)

dens_6 <- dads.s.datset.bc[!duplicated (dads.s.datset.bc$bino),]
dens_6 <- arrange(dens_6,bino)
spp.n_6 =count (dads.s.datset.bc,bino)
spp.n_6 <- arrange(spp.n_6,bino)

dads.ang.bc [dads.ang.bc$Lvl.D == "dataset" ,11] <- meanp (dens_6$DensM,spp.n_6$n
                                                           ,dads.s.datset.bc$bino,
                                                           as_numeric=TRUE)


##################################################


Dens.bc.2= getWoodDensity(genus=dads.gim.bc$Gen,
                          species=dads.gim.bc$Spp)
#head (Dens.bp.2)
#tail (Dens.bp.2)
dads.gim.bc$DensM <- Dens.bc.2$meanWD
dads.gim.bc$Lvl.D <- Dens.bc.2$levelWD
#tail (Dens.bp.2)





###########################################################
############################################################
########Biomassa######


##Gim
x_5=0.4141 #Souza e Longhi
a_5= 111.7988
b_5= -15.5317
c_5= 0.8544
d_5 = 0.0180
g.D_6= c(dads.gim.bc$DAP)
g.H_6=c(dads.gim.bc$Alt)
dads.gim.bc$biom= (x_5*(a_5+b_5*c(g.D_6)+c_5*c(g.D_6^2)+d_5*((c(g.D_6^2))*g.H_6)))

##Ang
ang.bc <-dads.ang.bc [dads.ang.bc$Gen != "Euterpe" & dads.ang.bc$Gen != "Syagrus",]

ang.bc$biom= (computeAGB(D=ang.bc$DAP,
                         WD=ang.bc$DensM,
                         H=ang.bc$Alt.E))*1000
##Palm

palm.bc <-dads.ang.bc [dads.ang.bc$Gen == "Euterpe" | dads.ang.bc$Gen == "Syagrus",]


a.p= -3.3488
b.p= 2.7483
D.p_2=palm.bc$DAP

palm.bc$biom = exp(a.p+b.p*log(D.p_2))
log(3)
exp (3)

#head (dads.gim.bc)
#head (ang.bc)

bio.bc =rbind (ang.bc, dads.gim.bc ,palm.bc)

######################
######################
######################
######################
######################DAP xBIO

bio.gim.bc<-bio.bc[bio.bc$Filo=="Gim",]
bio.ang.bc<-bio.bc[bio.bc$Filo!="Gim",]

#View (bio.ang.bp)

b.clas_gim_bc.10<-bio.gim.bc[bio.gim.bc$DAP<10,]
b.g.smal_6=sum(b.clas_gim_bc.10$biom)
b.clas_gim_bc.10.30<-bio.gim.bc[bio.gim.bc$DAP>=10 & bio.gim.bc$DAP<30,]
b.g.med_6=sum(b.clas_gim_bc.10.30$biom)
b.clas_gim_bc.30.50<-bio.gim.bc[bio.gim.bc$DAP>=30 & bio.gim.bc$DAP<50,]
b.g.lar_6=sum(b.clas_gim_bc.30.50$biom)
b.clas_gim_bc.50<-bio.gim.bc[bio.gim.bc$DAP>=50,]
b.g.x.larg_6=sum(b.clas_gim_bc.50$biom)

b.clas_ang_bc.10<-bio.ang.bc[bio.ang.bc$DAP<10,]
b.smal_6=sum(b.clas_ang_bc.10$biom)
b.clas_ang_bc.10.30<-bio.ang.bc[bio.ang.bc$DAP>=10 & bio.ang.bc$DAP<30,]
b.med_6=sum(b.clas_ang_bc.10.30$biom)
b.clas_ang_bc.30.50<-bio.ang.bc[bio.ang.bc$DAP>=30 & bio.ang.bc$DAP<50,]
b.lar_6=sum(b.clas_ang_bc.30.50$biom)
b.clas_ang_bc.50<-bio.ang.bc[bio.ang.bc$DAP>=50,]
b.x.lar_6=sum(b.clas_ang_bc.50$biom)


b.s.a_6=sum(bio.ang.bc$biom)
b.s.g_6=sum(bio.gim.bc$biom)
b.p.a_6=(c(b.smal_6,b.med_6,b.lar_6,b.x.lar_6)/b.s.a_6)*100
b.p.g_6=(c(b.g.smal_6,b.g.med_6,b.g.lar_6,b.g.x.larg_6)/b.s.g_6)*100














