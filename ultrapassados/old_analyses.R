#################ANALISES CORRELAÇÃO#############

#### dads.env -> está no script "Dados de cj estudos" utilizado para o CCA similaridade
#### similaridade florística. coluna 3 = altitude, 15 = precipitação anual, 4= temperatura média
#### 8 = temperatura mês mais quente, 9= temp. mes mais frio, 16= mes chuvoso, 17 = mes seco


s=data.frame (
  Biot=c(sum(bio.cj$biom),sum(bio.Fsf$biom),sum(bio.Fbar$biom),
         sum(bio.bp$biom),sum(bio.bc$biom),sum(bio.It$biom)),
  Bio_angt= c(sum (bio.ang.cj$biom),sum (bio.ang.Fsf$biom),sum (bio.ang.Fbar$biom),
              sum (bio.ang.bp$biom),sum (bio.ang.bc$biom),sum (bio.ang.It$biom)),
  Bio_angTr= c(sum(bio.trop.cj$biom),sum(bio.trop.Fsf$biom),sum(bio.trop.Fbar$biom),
               sum(bio.trop.bp$biom),sum(bio.trop.bc$biom),sum(bio.trop.It$biom)),
  Bio_angTe= c(sum(bio.ang.temp.cj$biom),sum(bio.ang.temp.Fsf$biom),sum(bio.ang.temp.Fbar$biom),
               sum(bio.ang.temp.bp$biom),sum(bio.ang.temp.bc$biom),sum(bio.ang.temp.It$biom)),
  Biogim= c( sum(gim.temp.cj),sum(gim.temp.Fsf),sum(gim.temp.Fbar)
             ,sum(gim.temp.bp),sum(gim.temp.bc),sum(gim.temp.It)),
  Alt=as.numeric(c(dads.env [7,3],dads.env [2,3],dads.env [3,3],
                   dads.env [8,3],dads.env [5,3],dads.env [6,3])),
  Prec=as.numeric(c(dads.env [7,15],dads.env [2,15],dads.env [3,15],
                    dads.env [8,15],dads.env [5,15],dads.env [6,15])),
  Temp= as.numeric(c(dads.env [7,4],dads.env [2,4],dads.env [3,4],
                     dads.env [8,4],dads.env [5,4],dads.env [6,4])),
  MS=as.numeric(c (dads.env [7,17],dads.env [2,17],dads.env [3,17],
                   dads.env [8,17],dads.env [5,17],dads.env [6,17])),
  RD=as.numeric(c (dads.env [7,5],dads.env [2,5],dads.env [3,5],
                   dads.env [8,5],dads.env [5,5],dads.env [6,5])),
  I_M=as.numeric(c (dads.env [7,6],dads.env [2,6],dads.env [3,6],
                    dads.env [8,6],dads.env [5,6],dads.env [6,6])),
  T_S=as.numeric(c (dads.env [7,7],dads.env [2,7],dads.env [3,7],
                    dads.env [8,7],dads.env [5,7],dads.env [6,7])),
  MQ=as.numeric(c (dads.env [7,8],dads.env [2,8],dads.env [3,8],
                   dads.env [8,8],dads.env [5,8],dads.env [6,8])),
  MF=as.numeric(c (dads.env [7,9],dads.env [2,9],dads.env [3,9],
                   dads.env [8,9],dads.env [5,9],dads.env [6,9])),
  RA_T=as.numeric(c (dads.env [7,10],dads.env [2,10],dads.env [3,10],
                     dads.env [8,10],dads.env [5,10],dads.env [6,10])),
  T_Qu=as.numeric(c (dads.env [7,11],dads.env [2,11],dads.env [3,11],
                     dads.env [8,11],dads.env [5,11],dads.env [6,11])),
  T_Qs=as.numeric(c (dads.env [7,12],dads.env [2,12],dads.env [3,12],
                     dads.env [8,12],dads.env [5,12],dads.env [6,12])),
  T_Qm=as.numeric(c (dads.env [7,13],dads.env [2,13],dads.env [3,13],
                     dads.env [8,13],dads.env [5,13],dads.env [6,13])),
  T_Qf=as.numeric(c (dads.env [7,14],dads.env [2,14],dads.env [3,14],
                     dads.env [8,14],dads.env [5,14],dads.env [6,14])),
  P_mesumi=as.numeric(c (dads.env [7,16],dads.env [2,16],dads.env [3,16],
                         dads.env [8,16],dads.env [5,16],dads.env [6,16])),
  Pres_sasonal=as.numeric(c (dads.env [7,18],dads.env [2,18],dads.env [3,18],
                             dads.env [8,18],dads.env [5,18],dads.env [6,18])),
  PQ_U=as.numeric(c (dads.env [7,19],dads.env [2,19],dads.env [3,19],
                     dads.env [8,19],dads.env [5,19],dads.env [6,19])),
  PQ_S=as.numeric(c (dads.env [7,20],dads.env [2,20],dads.env [3,20],
                     dads.env [8,20],dads.env [5,20],dads.env [6,20])),
  PQ_Q=as.numeric(c (dads.env [7,21],dads.env [2,21],dads.env [3,21],
                     dads.env [8,21],dads.env [5,21],dads.env [6,21])),
  PQ_F=as.numeric(c (dads.env [7,22],dads.env [2,22],dads.env [3,22],
                     dads.env [8,22],dads.env [5,22],dads.env [6,22]))
)


teste= decostand( s, "log")

dim (teste)

teste_m = teste [,-c(1:4,9:25)]
head(teste_m)
m_t=lm (teste_m [,1]~.,data=teste_m)
summary (m_t)
Mt_v=vif (m_t)
print (Mt_v)

############################################
#shapiro.test(s)
#wood=head (s,10)
#write.table (wood,file="wood.csv",
# dec=",",col.names=TRUE)#função exporta a tabela
#dim (s)
#x = c("Bio_angt ","Bio_angTr "," Bio_angTe ","Biogim")
#vif (lm(Biot~.-(),data=s))
#str (s)
#teste2=PCA (teste [,-seq(1:5)],grap=FALSE)
#print (teste2$eig)
#fviz_pca_var(teste2)
############################################

str (s)




rownames(s) <- c("CJ","F. SãoFran","F. Bar","BP","BC", "It")
colnames(s) <- c("Biomass_Tot","Biomass_Ang",
                 "Biomass_Trop","Biomass_Temp",
                 "Biomass_Gim","High","Rain",
                 "Temp","Driest_Month")


summary (s.2)
s
s.2=cor (s)
str (s.2)
str (s)
View(round (s.2, 2))



summary (s.2)

###################################################################################################
###################################################################################################

jpeg(filename = "mat.cor_biomass.jpg", width = 1000, height = 1200, # fun��o salva gr�ficos em .jpg
     units = "px", quality = 100,
     bg = "white", restoreConsole = TRUE)

par(mfrow=c(1,1),mar=c(3,5,2,2), cex.axis=2, cex.lab=2.5, mgp=c(3.5,1.6,0),
    family="serif",las=1, tcl=0.3)


corrplot(cor(s), method ='number', diag=FALSE, tl.cex=1.5,
         cl.cex=0.8, number.cex=2.5)

corrplot(cor(s), method ='square', diag=FALSE)
dev.off()

####################################
