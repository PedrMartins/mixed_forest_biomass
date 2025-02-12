
source("load_packages.R")
source("Equations.R")
source("Function_biomass.R")


#########data import#######
bio.cj<- import_biomass_rawdata (site="cj")
bio.bp<- import_biomass_rawdata (site="bp")
bio.Fbar<- import_biomass_rawdata (site="fb")
bio.Fsf<- import_biomass_rawdata (site="fsf")
bio.It<- import_biomass_rawdata (site="it")
bio.BC<- import_biomass_rawdata (site="bc")

########data processing#######
bio.cj <- data_processing (bio.cj)
bio.bp <- data_processing (bio.bp)
bio.Fbar <- data_processing (bio.Fbar)
bio.Fsf <- data_processing (bio.Fsf)
bio.It <- data_processing (bio.It)
bio.BC <- data_processing (bio.BC)

######CAMPOS DO JORDAO########

###Limpeza dados/processing data#######

dads.gim.cj<- separate_by_filo (bio.cj, choice = ("gim"))#separa gimnosperma
dads.ang.cj<- separate_by_filo (bio.cj, choice = ("ang"))


#View (dads.ang.cj)


########DBH class/classes DAP (CJ)######
#for DBH class < 10 cm / >= 10 to < 30
#/ >= 30 to < 50 />= 50

ind_gim_cj_sep_by_DHB <- class_DBH_bio_ind (dads.gim.cj, class = c(10,30,50) )
ind_ang_cj_sep_by_DHB <- class_DBH_bio_ind (dads.ang.cj, class = c(10,30,50) )

######Est.Altura / equation to estimate tree height (CJ)######

dads.ang.cj <- Estimating_higth_Ang (dads.ang.cj)

#Gim
dads.gim.cj <-  Estimating_higth_Gim (dads.gim.cj)

###########Densidade / wood density#########

#density from GWD
Dens.cj= getWoodDensity(genus=dads.ang.cj$Gen,
                        species=dads.ang.cj$Spp)

dads.ang.cj$DensM <- Dens.cj$meanWD
dads.ang.cj$Lvl.D <- Dens.cj$levelWD

#write.table (dads.ang.cj,file="dads.ang.cj.csv",
# dec=",",col.names=TRUE)#função exporta a tabela
#caso queira comparar densidade
#com outro banco de dados


#####inserindo densidade / density from (Doutorado / PhD
##### Gabriel Marcos Oliveira "Densidade da madeira
#####em minas gerais")
#####<http://repositorio.ufla.br/jspui/bitstream/1/4880/1/TESE_Densidade%20da%20madeira%20em%20Minas%20Gerais%20%20amostragem%2C%20espacializa%C3%A7%C3%A3o%20e%20rela%C3%A7%C3%A3o%20com%20vari%C3%A1veis%20ambientais.pdf>


#Density from Gabriel Marcos Oliveira PhD
dads.ang.cj [dads.ang.cj$Gen == "Callisthene",  11] <- 0.604  # pg 48 linha 55
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


######weighted average / média ponderada
######per individual plant / Por indivíduo




dads.s.datset.cj <- dads.ang.cj [dads.ang.cj$Lvl.D
                                 != "dataset" ,]

dads.s.datset.cj$bino <- paste(dads.s.datset.cj$Gen,
                               dads.s.datset.cj$Spp)

dens <- dads.s.datset.cj[!duplicated
                         (dads.s.datset.cj$bino),]
dens <- arrange(dens,bino)
head (dens)
spp.n =count (dads.s.datset.cj,bino)
spp.n <- arrange(spp.n,bino)

dads.ang.cj [dads.ang.cj$Lvl.D == "dataset",
             11] <- meanp (dens$DensM,spp.n$n
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

########Biomassa (CJ)######


##Gymnosperms
dads.gim.cj <- computeAGB_gim (dads.gim.cj)

##Angiosperms
#alometric equation Chaves et. al 2015
#from BIOMASS package
dads.ang.cj$biom= (computeAGB(D=dads.ang.cj$DAP,
                              WD=dads.ang.cj$DensM,
                              H=dads.ang.cj$Alt.E))*1000
#head (dads.gim.cj)
#head (dads.ang.cj)


bio.cj =rbind (dads.ang.cj,dads.gim.cj)

############DAP x BIO (CJ)##########

bio.gim.cj<-bio.cj[bio.cj$Filo=="Gim",]
bio.ang.cj <-bio.cj[bio.cj$Filo!="Gim",]
#tail (bio.ang.cj)

<<<<<<< HEAD
class_DBH_bio_ind  (bio.gim.cj)

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
=======
biomass_gim_cj_sep_by_DHB <- class_DBH_bio_ind (bio.gim.cj, class = c(10,30,50), choice = "bio" )
biomass_ang_cj_sep_by_DHB <- class_DBH_bio_ind (bio.ang.cj, class = c(10,30,50), choice = "bio" )
>>>>>>> 8ffbd159e53f74d151c51ef3431dd23cda3eb26f

#####################BAEPENDI###############################
###Limpeza dados

dads.gim.bp<- separate_by_filo(bio.bp, choice = "gim")
dads.ang.bp<- separate_by_filo(bio.bp, choice = "ang")


############classes DAP (BP)###########

ind_gim_bp_sep_by_DHB <- class_DBH_bio_ind (dads.gim.bp, class = c(10,30,50) )
ind_ang_bp_sep_by_DHB <- class_DBH_bio_ind (dads.ang.bp, class = c(10,30,50) )

############Est.Altura (BP) ########

dads.ang.bp <- Estimating_higth_Ang (dads.ang.bp)

#Gim
dads.gim.bp <- Estimating_higth_Gim (dads.gim.bp)

#########Densidade (BP)#######

Dens.bp= getWoodDensity(genus=dads.ang.bp$Gen,
                        species=dads.ang.bp$Spp)

dads.ang.bp$DensM <- Dens.bp$meanWD
dads.ang.bp$Lvl.D <- Dens.bp$levelWD

#write.table (dads.ang.bp,file="dads.ang.bp.csv",
# dec=",",col.names=TRUE)#função exporta a tabela
#caso queira comparar densidade
#com outro banco de dados


#####inserindo densidade (Doutorado Gabriel
#####Martocos Oliveira "Densidade da madeira
#####em minas gerais")

dads.ang.bp [dads.ang.bp$Gen == "Clethra",11] <- 0.404  # pg 49 linha 78
dads.ang.bp [dads.ang.bp$Gen == "Myrcia" & dads.ang.bp$Spp == "guianensis"  ,11] <- 0.545  # pg 52 linha 212
dads.ang.bp [dads.ang.bp$Gen == "Myrsine" & dads.ang.bp$Spp == "coriacea"  ,11] <- 0.545  # pg 52 linha 218
dads.ang.bp [dads.ang.bp$Gen == "Myrsine" & dads.ang.bp$Spp == "umbellata"  ,11] <- 0.602  # pg 52 linha 220
dads.ang.bp [dads.ang.bp$Gen == "Ocotea" & dads.ang.bp$Spp == "pulchella"  ,11] <- 0.593  # pg 53 linha 230
dads.ang.bp [dads.ang.bp$Gen == "Symplocos" & dads.ang.bp$Spp == "celastrinea"  ,11] <- 0.427  # pg 55 linha 305


######média ponderada (Por indivíduo) caso tenha
######inserido novos dados


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



########Biomassa (BP)########

##Gim
dads.gim.bp <- computeAGB_gim (dads.gim.bp)

##Ang

dads.ang.bp$biom= (computeAGB(D=dads.ang.bp$DAP,
                              WD=dads.ang.bp$DensM,
                              H=dads.ang.bp$Alt.E))*1000


bio.bp =rbind (dads.ang.bp,dads.gim.bp)
sum (bio.bp$biom)

#########DAP xBIO (BP)########

bio.gim.bp <-bio.bp[bio.bp$Filo=="Gim",]
bio.ang.bp <-bio.bp[bio.bp$Filo!="Gim",]

#View (bio.ang.bp)
biomass_gim_bp_sep_by_DHB <- class_DBH_bio_ind(bio.gim.bp, choice = "bio", class = c(10,30,50))
biomass_ang_bp_sep_by_DHB <- class_DBH_bio_ind(bio.ang.bp, choice = "bio", class = c(10,30,50))

###########FAZ. BARTIRA#######


###Limpeza dados

dads.gim.Fbar<- separate_by_filo(bio.Fbar, choice = "gim")
dads.ang.Fbar<- separate_by_filo(bio.Fbar, choice = "ang")

#########classes DAP (Faz. Bart)#########

ind_gim_Fbar_sep_by_DHB <- class_DBH_bio_ind(dads.gim.Fbar, class = c(10,30,50))
ind_ang_Fbar_sep_by_DHB <- class_DBH_bio_ind(dads.ang.Fbar, class = c(10,30,50))

########Est.Altura (Faz. Bart)######

dads.ang.Fbar <- Estimating_higth_Ang (dads.ang.Fbar)

#Gim
dads.gim.Fbar <- Estimating_higth_Gim (dads.gim.Fbar)

##########Densidade (Faz. Bart)########

Dens.Fbar= getWoodDensity(genus=dads.ang.Fbar$Gen,
                          species=dads.ang.Fbar$Spp)

dads.ang.Fbar$DensM <- Dens.Fbar$meanWD
dads.ang.Fbar$Lvl.D <- Dens.Fbar$levelWD

#write.table (dads.ang.Fbar,file="dads.ang.Fbar.csv",
# dec=",",col.names=TRUE)#função exporta a tabela
#caso queira comparar densidade
#com outro banco de dados


#####inserindo densidade (Doutorado Gabriel
#####Martocos Oliveira "Densidade da madeira
#####em minas gerais")

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


######média ponderada (Por indivíduo) caso tenha
######inserido novos dados

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



########Biomassa (Faz. Bart)########


##Gim
dads.gim.Fbar <- computeAGB_gim (dads.gim.Fbar)


##Ang

dads.ang.Fbar$biom= (computeAGB(D=dads.ang.Fbar$DAP,
                                WD=dads.ang.Fbar$DensM,
                                H=dads.ang.Fbar$Alt.E))*1000


bio.Fbar =rbind (dads.ang.Fbar,dads.gim.Fbar)


######################DAP x BIO (Faz. Bart)#########

bio.gim.Fbar<-bio.Fbar[bio.Fbar$Filo=="Gim",]
bio.ang.Fbar<-bio.Fbar[bio.Fbar$Filo!="Gim",]

#View (bio.ang.bp)

biomass_gim_Fbar_sep_by_DHB <- class_DBH_bio_ind(bio.gim.Fbar, choice = "bio", class = c(10,30,50))
biomass_ang_Fbar_sep_by_DHB <- class_DBH_bio_ind(bio.ang.Fbar, choice = "bio", class = c(10,30,50))


###################FAZ. SÃO FRANCISCO#######################

###Limpeza dados


dads.gim.Fsf<- separate_by_filo(bio.Fsf, choice = "gim")
dads.ang.Fsf<- separate_by_filo(bio.Fsf, choice = "ang")


###############classes DAP (Faz. SF)#############

ind_gim_Fsf_sep_by_DHB <- class_DBH_bio_ind(dads.gim.Fsf, class = c(10,30,50))
ind_ang_Fsf_sep_by_DHB <- class_DBH_bio_ind(dads.ang.Fsf, class = c(10,30,50))

######Est.Altura######

#Ang
dads.ang.Fsf <- Estimating_higth_Ang(dads.ang.Fsf)

#Gim
dads.gim.Fsf <- Estimating_higth_Gim(dads.gim.Fsf)

##########Densidade (Faz. SF)########

Dens.Fsf= getWoodDensity(genus=dads.ang.Fsf$Gen,
                         species=dads.ang.Fsf$Spp)

dads.ang.Fsf$DensM <- Dens.Fsf$meanWD
dads.ang.Fsf$Lvl.D <- Dens.Fsf$levelWD

#write.table (dads.ang.Fsf,file="dads.ang.Fsf.csv",
# dec=",",col.names=TRUE)#função exporta a tabela

#####inserindo densidade (Doutorado Gabriel
#####Martocos Oliveira "Densidade da madeira
#####em minas gerais")

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

###

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

#####Biomassa (Faz. SF)######


##Gim
dads.gim.Fsf <- computeAGB_gim(dads.gim.Fsf)

##Ang

dads.ang.Fsf$biom= (computeAGB(D=dads.ang.Fsf$DAP,
                               WD=dads.ang.Fsf$DensM,
                               H=dads.ang.Fsf$Alt.E))*1000

bio.Fsf =rbind (dads.ang.Fsf,dads.gim.Fsf)



##############DAP x BIO (Faz. SF)########

bio.gim.Fsf<-bio.Fsf[bio.Fsf$Filo=="Gim",]
bio.ang.Fsf<-bio.Fsf[bio.Fsf$Filo!="Gim",]

biomass_gim_Fsf_sep_by_DHB <- class_DBH_bio_ind(dads.gim.Fsf, choice = "bio", class = c(10,30,50))
biomass_ang_Fsf_sep_by_DHB <- class_DBH_bio_ind(dads.ang.Fsf, choice = "bio", class = c(10,30,50))

###################ITABERÁ#################################

###Limpeza dados

dads.gim.It<- separate_by_filo(bio.It, choice = "gim")
dads.ang.It<- separate_by_filo(bio.It, choice = "ang")
dads.palm.It <- separate_by_filo(bio.It, choice = "palm")

###########classes DAP (IT)############
ind_gim_It_sep_by_DHB <- class_DBH_bio_ind(dads.gim.It, class = c(10,30,50))
ind_ang_It_sep_by_DHB <- class_DBH_bio_ind(dads.ang.It, class = c(10,30,50))
ind_palm_It_sep_by_DHB <- class_DBH_bio_ind(dads.palm.It, class = c(10,30,50))

##########Est.Altura (IT)##########

#Ang
dads.ang.It <- Estimating_higth_Ang(dads.ang.It)

#Gim
dads.gim.It <- Estimating_higth_Gim(dads.gim.It)

#Gim
dads.palm.It$Alt.E <- seq (1:length(dads.palm.It$D))

########Densidade (IT)########

Dens.It= getWoodDensity(genus=dads.ang.It$Gen,
                        species=dads.ang.It$Spp)

dads.ang.It$DensM <- Dens.It$meanWD
dads.ang.It$Lvl.D <- Dens.It$levelWD

#write.table (dads.ang.It,file="dads.ang.It.csv",
# dec=",",col.names=TRUE)#função exporta a tabela

#####inserindo densidade (Doutorado Gabriel
#####Martocos Oliveira "Densidade da madeira
#####em minas gerais")

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

##

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
#######Dens gimnosperma (IT)######


Dens.It.2= getWoodDensity(genus=dads.gim.It$Gen,
                          species=dads.gim.It$Spp)
#head (Dens.bp.2)
#tail (Dens.bp.2)
dads.gim.It$DensM <- Dens.It.2$meanWD
dads.gim.It$Lvl.D <- Dens.It.2$levelWD
#tail (Dens.bp.2)

Dens.It.3= getWoodDensity(genus=dads.palm.It$Gen,
                          species=dads.palm.It$Spp)

dads.palm.It$DensM <- Dens.It.3$meanWD
dads.palm.It$Lvl.D <- Dens.It.3$levelWD



######Biomassa (IT)######


##Gim
dads.gim.It <- computeAGB_gim(dads.gim.It)

##Ang

dads.ang.It$biom= (computeAGB(D=dads.ang.It$DAP,
                         WD=dads.ang.It$DensM,
                         H=dads.ang.It$Alt.E))*1000
##Palm

dads.palm.It <- computeAGB_palm(dads.palm.It)

bio.It =rbind (dads.ang.It, dads.gim.It, dads.palm.It)


############DAP xBIO (IT)#########

bio.gim.It<-bio.It[bio.It$Filo=="Gim",]
bio.ang.It<-bio.It[bio.It$Filo!="Gim" & bio.It$Filo!="Palm" ,]
bio.palm.It<-bio.It[bio.It$Filo=="Palm" ,]

biomass_gim_It_sep_by_DHB <- class_DBH_bio_ind(bio.gim.It,
                                               choice = "bio",
                                               class = c(10,30,50))
biomass_ang_It_sep_by_DHB <- class_DBH_bio_ind(bio.ang.It,
                                               choice = "bio",
                                               class = c(10,30,50)) #checar!
biomass_palm_It_sep_by_DHB <- class_DBH_bio_ind(bio.palm.It,
                                                choice = "bio",
                                                class = c(10,30,50))

############BARRA DO CHAPÉU##########


###Limpeza dados


dads.gim.bc<- separate_by_filo(bio.BC, choice = "gim")
dads.ang.bc<- separate_by_filo(bio.BC, choice = "ang")
dads.palm.bc <- separate_by_filo(bio.BC, choice = "palm")

#head (bio.BC)


##########################classes DAP########################

ind_gim_bc_sep_by_DHB <- class_DBH_bio_ind(dads.gim.bc, class = c(10,30,50))
ind_ang_bc_sep_by_DHB <- class_DBH_bio_ind(dads.ang.bc, class = c(10,30,50))
ind_palm_bc_sep_by_DHB <- class_DBH_bio_ind(dads.palm.bc, class = c(10,30,50))

#####Est.Altura (BC)######
#Ang
dads.ang.bc <- Estimating_higth_Ang(dads.ang.bc)

#Gim
dads.gim.bc <- Estimating_higth_Gim(dads.gim.bc)

dads.palm.bc$Alt.E <- seq (1:length(dads.palm.bc$D))

########Densidade (BC)######

Dens.bc= getWoodDensity(genus=dads.ang.bc$Gen,
                        species=dads.ang.bc$Spp)

dads.ang.bc$DensM <- Dens.bc$meanWD
dads.ang.bc$Lvl.D <- Dens.bc$levelWD


#write.table (dads.ang.bc,file="dads.ang.bc.csv",
# dec=",",col.names=TRUE)#função exporta a tabela


#####inserindo densidade (Doutorado Gabriel
#####Martocos Oliveira "Densidade da madeira
#####em minas gerais")


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

###

dads.s.datset.bc <- dads.ang.bc [dads.ang.bc$Lvl.D != "dataset" ,]

dads.s.datset.bc$bino <- paste(dads.s.datset.bc$Gen,dads.s.datset.bc$Spp)

dens_6 <- dads.s.datset.bc[!duplicated (dads.s.datset.bc$bino),]
dens_6 <- arrange(dens_6,bino)
spp.n_6 =count (dads.s.datset.bc,bino)
spp.n_6 <- arrange(spp.n_6,bino)

dads.ang.bc [dads.ang.bc$Lvl.D == "dataset" ,11] <- meanp (dens_6$DensM,spp.n_6$n
                                                           ,dads.s.datset.bc$bino,
                                                           as_numeric=TRUE)
##

Dens.bc.2= getWoodDensity(genus=dads.gim.bc$Gen,
                          species=dads.gim.bc$Spp)
#head (Dens.bp.2)
#tail (Dens.bp.2)
dads.gim.bc$DensM <- Dens.bc.2$meanWD
dads.gim.bc$Lvl.D <- Dens.bc.2$levelWD

Dens.bc.3= getWoodDensity(genus=dads.palm.bc$Gen,
                          species=dads.palm.bc$Spp,
                          family = dads.palm.bc$Fam)

dads.palm.bc$DensM <- Dens.bc.3$meanWD
dads.palm.bc$Lvl.D <- Dens.bc.3$levelWD

########Biomassa######


##Gim
dads.gim.bc <- computeAGB_gim(dads.gim.bc)

##Ang

dads.ang.bc$biom= (computeAGB(D=dads.ang.bc$DAP,
                         WD=dads.ang.bc$DensM,
                         H=dads.ang.bc$Alt.E))*1000
##Palm
dads.palm.bc <- computeAGB_palm(dads.palm.bc)


#head (dads.gim.bc)
#head (ang.bc)

bio.bc =rbind (dads.ang.bc, dads.gim.bc, dads.palm.bc)


############DAP xBIO (BC)##############

bio.gim.bc<-bio.bc[bio.bc$Filo=="Gim",]
bio.ang.bc<-bio.bc[bio.bc$Filo!="Gim" & bio.bc$Filo!="Palm"  ,]
bio.palm.bc<-bio.bc[bio.bc$Filo=="Palm",]
#View (bio.ang.bp)

biomass_gim_bc_sep_by_DHB <- class_DBH_bio_ind(bio.gim.bc, choice = "bio", class = c(10,30,50))
biomass_ang_bc_sep_by_DHB <- class_DBH_bio_ind(bio.ang.bc, choice = "bio", class = c(10,30,50))
biomass_palm_bc_sep_by_DHB <- class_DBH_bio_ind(bio.palm.bc, choice = "bio", class = c(10,30,50))
