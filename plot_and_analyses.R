########################GRÁFICOS BIOMASSA##############################
###################rodar script biomass_calc antes#####################


############################################################
###DAP class porcentagem ind########
###Suplementar########

source ("import_processing_biomass_data.R")


jpeg(filename = "Classe DAP_ind.jpg", width = 1050, height = 700, # function to save plots salva .jpg
     units = "px", quality = 75,
     bg = "white")

par(mfrow=c(2,3),mar=c(5,5,3,2), cex.axis=1.3, cex.lab=1.5, mgp=c(3,1.3,0.3),
    family="serif",las=1, tcl=0.3, bg= "white")
color= colorRampPalette(c("lightgreen", "sandybrown"))

barplot (c(p.a_6[1],p.g_6[1],p.a_6[2],p.g_6[2],p.a_6[3],p.g_6[3],
           p.a_6[4],p.g_6[4]), ylab="Individuals (%)",
         col=color (2), ylim=c(0,70), main="Barra do Chapéu (MF1)")#bc


mtext( #fun��o plota textos nas �reas ao redor do gr�fico
  c("0-10", "10-30","30-50", ">50"), #primeiro argumento refere oa texto plotado
  side= 1, #argumento localiza no grafico "1" abaixo
  cex=1,line=0.9,
  at=c(1.4,3.5,6,8.5))

legend("topright" #fun��o adiciona um texto ao gr�fico,
       #arg 1� define a localiza��o, usa-se a fun��o locator para
       #adicionar de uma forma interativa
       ,c("Angiosperms", "Gymnosperms") #texto a ser escrito
       ,col=color (2)
       ,cex=1.3		#tamanho da fonte
       , pch=c(15,15)
       ,bty = "n") #tipo da fonte
barplot (c(p.a_5[1],p.g_5[1],p.a_5[2],p.g_5[2],p.a_5[3],p.g_5[3]
           ,p.a_5[4],p.g_5[4]),
         col=color (2), ylim=c(0,70),main="Itaberá (MF2)")#it


mtext( #fun��o plota textos nas �reas ao redor do gr�fico
  c("0-10", "10-30","30-50", ">50"), #primeiro argumento refere oa texto plotado
  side= 1, #argumento localiza no gr�fico "1" abaixo
  cex=1,line=0.9,
  at=c(1.4,3.5,6,8.5))

legend("topright" #fun��o adiciona um texto ao gr�fico,
       #arg 1� define a localiza��o, usa-se a fun��o locator para
       #adicionar de uma forma interativa
       ,c("Angiosperms", "Gymnosperms") #texto a ser escrito
       ,col=color (2)
       ,cex=1.3		#tamanho da fonte
       , pch=c(15,15)
       ,bty = "n") #tipo da fonte

barplot (c(p.a[1],p.g[1],p.a[2],p.g[2],p.a[3],p.g[3],p.a[4],p.g[4]),
         col=color (2), ylim=c(0,70),
         main="Campos do Jordão (MF3)")



mtext( #fun��o plota textos nas �reas ao redor do gr�fico
  c("0-10", "10-30","30-50", ">50"), #primeiro argumento refere oa texto plotado
  side= 1, #argumento localiza no gr�fico "1" abaixo
  cex=1,line=0.9,
  at=c(1.4,3.5,6,8.5))

legend("topright" #fun��o adiciona um texto ao gr�fico,
       #arg 1� define a localiza��o, usa-se a fun��o locator para
       #adicionar de uma forma interativa
       ,c("Angiosperms", "Gymnosperms") #texto a ser escrito
       ,col=color (2)
       ,cex=1.3		#tamanho da fonte
       , pch=c(15,15)
       ,bty = "n") #tipo da fonte


barplot (c(p.a_3[1],p.g_3[1],p.a_3[2],p.g_3[2],p.a_3[3],
           p.g_3[3],p.a_3[4],p.g_3[4]),
         col=color (2), ylim=c(0,70), ylab="Individuals (%)",
         xlab="DBH Class (cm)",
         main="Delfim Moreira \n Faz. Bart. (MF4)")#fb


mtext( #fun��o plota textos nas �reas ao redor do gr�fico
  c("0-10", "10-30","30-50", ">50"), #primeiro argumento refere oa texto plotado
  side= 1, #argumento localiza no gr�fico "1" abaixo
  cex=1,line=0.9,
  at=c(1.4,3.5,6,8.5))

legend("topright" #fun��o adiciona um texto ao gr�fico,
       #arg 1� define a localiza��o, usa-se a fun��o locator para
       #adicionar de uma forma interativa
       ,c("Angiosperms", "Gymnosperms") #texto a ser escrito
       ,col=color (2)
       ,cex=1.3		#tamanho da fonte
       , pch=c(15,15)
       ,bty = "n") #tipo da fonte

barplot (c(p.a_4[1],p.g_4[1],p.a_4[2],p.g_4[2],p.a_4[3],
           p.g_4[3],p.a_4[4],p.g_4[4]),
         col=color (2), ylim=c(0,70),
         xlab="DBH Class (cm)",
         main="Delfim Moreira \n Faz. São Fran. (MF5)")#fsf


mtext( #fun��o plota textos nas �reas ao redor do gr�fico
  c("0-10", "10-30","30-50", ">50"), #primeiro argumento refere oa texto plotado
  side= 1, #argumento localiza no gr�fico "1" abaixo
  cex=1,line=0.9,
  at=c(1.4,3.5,6,8.5))

legend("topright" #fun��o adiciona um texto ao gr�fico,
       #arg 1� define a localiza��o, usa-se a fun��o locator para
       #adicionar de uma forma interativa
       ,c("Angiosperms", "Gymnosperms") #texto a ser escrito
       ,col=color (2)
       ,cex=1.3		#tamanho da fonte
       , pch=c(15,15)
       ,bty = "n") #tipo da fonte

barplot (c(p.a_2[1],p.g_2[1],p.a_2[2],p.g_2[2],p.a_2[3],
           p.g_2[3],p.a_2[4],p.g_2[4]),ylab="",
         col=color (2),ylim=c(0,70),xlab="DBH Class (cm)",
         main="Baependi (MF6)")#bp

mtext( #fun��o plota textos nas �reas ao redor do gr�fico
  c("0-10", "10-30","30-50", ">50"), #primeiro argumento refere oa texto plotado
  side= 1, #argumento localiza no gr�fico "1" abaixo
  cex=1,line=0.9,
  at=c(1.4,3.5,6,8.5))

legend("topright" #fun��o adiciona um texto ao gr�fico,
       #arg 1� define a localiza��o, usa-se a fun��o locator para
       #adicionar de uma forma interativa
       ,c("Angiosperms", "Gymnosperms") #texto a ser escrito
       ,col=color (2)
       ,cex=1.3		#tamanho da fonte
       , pch=c(15,15)
       ,bty = "n") #tipo da fonte

dev.off()


########################################
###########DAP Class porcentage BIO########


jpeg(filename = "Classe DAP_biomas.jpg", width = 1050, height = 700, # fun��o salva gr�ficos em .jpg
     units = "px", quality = 95,
     bg = "white")

par(mfrow=c(2,3),mar=c(5,5,3,2), cex.axis=1.3, cex.lab=1.5, mgp=c(3,1.3,0.3),
    family="serif",las=1, tcl=0.3, bg="white")

color= colorRampPalette(c("lightgreen", "sandybrown"))

barplot (c(b.p.a_6[1],b.p.g_6[1],b.p.a_6[2],b.p.g_6[2],b.p.a_6[3],b.p.g_6[3],
           b.p.a_6[4],b.p.g_6[4]), ylab="Biomass %",
         col=color (2), ylim=c(0,100), main="Barra do Chapéu (MF1)")#bc


mtext( #fun��o plota textos nas �reas ao redor do gr�fico
  c("0-10", "10-30","30-50", ">50"), #primeiro argumento refere oa texto plotado
  side= 1, #argumento localiza no gr�fico "1" abaixo
  cex=1,line=0.9,
  at=c(1.4,3.5,6,8.5))

legend("topright" #fun��o adiciona um texto ao gr�fico,
       #arg 1� define a localiza��o, usa-se a fun��o locator para
       #adicionar de uma forma interativa
       ,c("Angiosperms", "Gymnosperms") #texto a ser escrito
       ,col=color (2)
       ,cex=1.3		#tamanho da fonte
       , pch=c(15,15)
       ,bty = "n") #tipo da fonte


barplot (c(b.p.a_5[1],b.p.g_5[1],b.p.a_5[2],b.p.g_5[2],b.p.a_5[3],b.p.g_5[3]
           ,b.p.a_5[4],b.p.g_5[4]),
         col=color (2), ylim=c(0,100),
         main="Itaberá (MF2)")#it


mtext( #fun��o plota textos nas �reas ao redor do gr�fico
  c("0-10", "10-30","30-50", ">50"), #primeiro argumento refere oa texto plotado
  side= 1, #argumento localiza no grafico "1" abaixo
  cex=1,line=0.9,
  at=c(1.4,3.5,6,8.5))

legend("topright" #fun��o adiciona um texto ao gr�fico,
       #arg 1� define a localiza��o, usa-se a fun��o locator para
       #adicionar de uma forma interativa
       ,c("Angiosperms", "Gymnosperms") #texto a ser escrito
       ,col=color (2)
       ,cex=1.3		#tamanho da fonte
       , pch=c(15,15)
       ,bty = "n") #tipo da fonte

barplot (c(b.p.a[1],b.p.g[1],b.p.a[2],b.p.g[2],b.p.a[3],b.p.g[3],b.p.a[4],b.p.g[4]),
         col=color (2), ylim=c(0,100), main="Campos do Jordão (MF3)")#cj


mtext( #fun��o plota textos nas �reas ao redor do gr�fico
  c("0-10", "10-30","30-50", ">50"), #primeiro argumento refere oa texto plotado
  side= 1, #argumento localiza no gr�fico "1" abaixo
  cex=1,line=0.9,
  at=c(1.4,3.5,6,8.5))

legend("topright" #fun��o adiciona um texto ao gr�fico,
       #arg 1� define a localiza��o, usa-se a fun��o locator para
       #adicionar de uma forma interativa
       ,c("Angiosperms", "Gymnosperms") #texto a ser escrito
       ,col=color (2)
       ,cex=1.3		#tamanho da fonte
       , pch=c(15,15)
       ,bty = "n") #tipo da fonte



barplot (c(b.p.a_3[1],b.p.g_3[1],b.p.a_3[2],b.p.g_3[2],b.p.a_3[3],
           b.p.g_3[3],b.p.a_3[4],b.p.g_3[4]),
         col=color (2), ylim=c(0,100), ylab="Biomass %", xlab="DBH Class (cm)",
         main="Delfim Moreira \n Faz. Bart. (MF4)")#fb

mtext( #fun��o plota textos nas �reas ao redor do gr�fico
  c("0-10", "10-30","30-50", ">50"), #primeiro argumento refere oa texto plotado
  side= 1, #argumento localiza no gr�fico "1" abaixo
  cex=1,line=0.9,
  at=c(1.4,3.5,6,8.5))

legend("topright" #fun��o adiciona um texto ao gr�fico,
       #arg 1� define a localiza��o, usa-se a fun��o locator para
       #adicionar de uma forma interativa
       ,c("Angiosperms", "Gymnosperms") #texto a ser escrito
       ,col=color (2)
       ,cex=1.3		#tamanho da fonte
       , pch=c(15,15)
       ,bty = "n") #tipo da fonte

barplot (c(b.p.a_4[1],b.p.g_4[1],b.p.a_4[2],b.p.g_4[2],b.p.a_4[3],
           b.p.g_4[3],b.p.a_4[4],b.p.g_4[4]),
         col=color (2), ylim=c(0,100),
         xlab="DBH Class (cm)",
         main="Delfim Moreira \n Faz. São Fran. (MF5)") #fsf

mtext( #fun��o plota textos nas �reas ao redor do gr�fico
  c("0-10", "10-30","30-50", ">50"), #primeiro argumento refere oa texto plotado
  side= 1, #argumento localiza no gr�fico "1" abaixo
  cex=1,line=0.9,
  at=c(1.4,3.5,6,8.5))

legend("topright" #fun��o adiciona um texto ao gr�fico,
       #arg 1� define a localiza��o, usa-se a fun��o locator para
       #adicionar de uma forma interativa
       ,c("Angiosperms", "Gymnosperms") #texto a ser escrito
       ,col=color (2)
       ,cex=1.3		#tamanho da fonte
       , pch=c(15,15)
       ,bty = "n") #tipo da fonte

barplot (c(b.p.a_2[1],b.p.g_2[1],b.p.a_2[2],b.p.g_2[2],b.p.a_2[3],
           b.p.g_2[3],b.p.a_2[4],b.p.g_2[4]),
         col=color (2), ylim=c(0,100),ylab="Biomass %", xlab="DBH Class (cm)",
         main="Baependi (MF6)")#bp

mtext( #fun��o plota textos nas �reas ao redor do gr�fico
  c("0-10", "10-30","30-50", ">50"), #primeiro argumento refere oa texto plotado
  side= 1, #argumento localiza no gr�fico "1" abaixo
  cex=1,line=0.9,
  at=c(1.4,3.5,6,8.5))

legend("topright" #fun��o adiciona um texto ao gr�fico,
       #arg 1� define a localiza��o, usa-se a fun��o locator para
       #adicionar de uma forma interativa
       ,c("Angiosperms", "Gymnosperms") #texto a ser escrito
       ,col=color (2)
       ,cex=1.3		#tamanho da fonte
       , pch=c(15,15)
       ,bty = "n") #tipo da fonte

dev.off()


##########################
###
### bio gmin/ arauc#######

dads.ara.cj<- dads.gim.cj [dads.gim.cj$Gen!="Podocarpus",]
dads.pod.cj<- dads.gim.cj [dads.gim.cj$Gen!="Araucaria",]

dads.ara.fb<- dads.gim.Fbar [dads.gim.Fbar$Gen!="Podocarpus",]
dads.pod.fb<- dads.gim.Fbar [dads.gim.Fbar$Gen!="Araucaria",]

dads.ara.fsf<- dads.gim.Fsf [dads.gim.Fsf$Gen!="Podocarpus",]
dads.pod.fsf<- dads.gim.Fsf [dads.gim.Fsf$Gen!="Araucaria",]

dads.ara.bp<- dads.gim.bp [dads.gim.bp$Gen!="Podocarpus",]
dads.pod.bp<- dads.gim.bp [dads.gim.bp$Gen!="Araucaria",]


#####################################
########prop. arauc. podo#######


bio.podo.ara.pro = data.frame(
  BC_SP=c((sum(dads.gim.bc$biom)/sum(bio.bc$biom))*100,0),

  IT_SP=c((sum(dads.gim.It$biom)/sum(bio.It$biom))*100,0),

  CJ_SP=c((sum(dads.ara.cj$biom)/sum(bio.cj$biom))*100,(sum (dads.pod.cj$biom)/sum(bio.cj$biom))*100),

  FSF_MG=c((sum(dads.ara.fsf$biom)/sum(bio.Fsf$biom))*100,(sum(dads.pod.fsf$biom)/sum(bio.Fsf$biom))*100),

  FB_MG=c((sum(dads.ara.fb$biom)/sum(bio.Fbar$biom))*100,(sum(dads.pod.fb$biom)/sum(bio.Fbar$biom))*100),


  BP_MG=c((sum(dads.ara.bp$biom)/sum(bio.bp$biom))*100,(sum(dads.pod.bp$biom)/sum(bio.bp$biom))*100)

  )

rownames(bio.podo.ara.pro) <- c("Araucaria","Podocarpus")
colnames(bio.podo.ara.pro) <- c("Barra \n do Chapéu"="MF1"
                                ,"Itaberá" = "MF2",
                                "Campos do Jordão"="MF3",
                                "Delfim Moreira \n Faz. São Fran."="MF5",
                                "Delfim Moreira \n Faz. Bart."="MF4",
                                "Baependi"="MF6"
                                )
bio.podo.ara.pro= as.matrix (bio.podo.ara.pro)


jpeg(filename = "biomas_arau_podo_prop.jpg", width = 850, height = 900, # fun��o salva gr�ficos em .jpg
     units = "px", quality = 75,
     bg = "white")

par(mfrow=c(1,1), mar=c(3,5,2,2), cex.axis=1.3, cex.lab=1.5, mgp=c(3.5,1.6,0),
    family="serif",las=1, tcl=0.3, bg="white")

color= colorRampPalette(c("rosybrown1", "sandybrown"))
barplot (bio.podo.ara.pro, col=color (2), ylim=c(0,100),ylab="Biomass %")



legend("topright" #fun��o adiciona um texto ao gr�fico,
       #arg 1� define a localiza��o, usa-se a fun��o locator para
       #adicionar de uma forma interativa
       ,c("Araucaria","Podocarpus") #texto a ser escrito
       ,col=color (2)
       ,cex=1.5		#tamanho da fonte
       , pch=c(15,15)
       ,bty = "n") #tipo da fonte

#text(0.5,95 #fun��o adiciona um texto ao gr�fico,
     #arg 1� define a localiza��o, usa-se a fun��o locator para
     #adicionar de uma forma interativa
#    ,"a" #texto a ser escrito
#     ,cex=1.2		#tamanho da fonte
#     , family = "mono") #tipo da fonte


dev.off()


######
bio.podo.ara = data.frame(
  BC_SP=c(sum(dads.gim.bc$biom)/1000,0),
  IT_SP=c(sum(dads.gim.It$biom)/1000,0),
  CJ_SP=c(sum(dads.ara.cj$biom)/1000,sum (dads.pod.cj$biom)/1000),
  FSF_MG=c(sum(dads.ara.fsf$biom)/1000,sum(dads.pod.fsf$biom)/1000),
  FB_MG=c(sum(dads.ara.fb$biom)/1000,sum(dads.pod.fb$biom)/1000),
  BP_MG=(c(sum(dads.ara.bp$biom)/1000,sum(dads.pod.bp$biom)/1000))/0.5
  )


rownames(bio.podo.ara) <- c("Araucaria","Podocarpus")
colnames(bio.podo.ara) <- c(
                              "Barra \n do Chapéu"="MF1"
                              ,"Itaberá" = "MF2",
                                "Campos do Jordão"="MF3",
                                "Delfim Moreira \n Faz. São Fran."="MF5",
                                "Delfim Moreira \n Faz. Bart."="MF4",
                                "Baependi"="MF6"
                                )
bio.gim= as.matrix (bio.podo.ara)


#####



########################################################################
######bio_filo#####
######bio_geral#####

bio.ang.temp.cj <- bio.cj [bio.cj$Distri == "Temp" & bio.cj$Filo != "Gim",]
bio.gim.a_cj <- bio.cj [bio.cj$Gen == "Araucaria",]
bio.gim.p_cj <- bio.cj [bio.cj$Gen == "Podocarpus",]
bio.trop.cj <- bio.cj [bio.cj$Distri == "Trop",]
ang.temp.cj=sum(bio.ang.temp.cj$biom)
gim.temp.cj=c(sum(bio.gim.a_cj$biom),sum(bio.gim.p_cj$biom))
trop.cj=sum(bio.trop.cj$biom)
por.t.t.cj= (c(gim.temp.cj,ang.temp.cj,trop.cj)/
               sum(gim.temp.cj,ang.temp.cj,trop.cj)*100)


bio.ang.temp.bp <- bio.bp [bio.bp$Distri == "Temp" & bio.bp$Filo != "Gim",]
bio.gim.a_bp <- bio.bp [bio.bp $Gen == "Araucaria",]
bio.gim.p_bp <- bio.bp [bio.bp $Gen == "Podocarpus",]
bio.trop.bp <- bio.bp [bio.bp$Distri == "Trop",]
ang.temp.bp =sum(bio.ang.temp.bp $biom)
gim.temp.bp =c(sum(bio.gim.a_bp $biom),sum(bio.gim.p_bp $biom))
trop.bp=sum(bio.trop.bp$biom)
por.t.t.bp= (c(gim.temp.bp,ang.temp.bp,trop.bp)/
               sum(gim.temp.bp,ang.temp.bp,trop.bp)*100)



bio.ang.temp.Fsf <- bio.Fsf [bio.Fsf$Distri == "Temp" & bio.Fsf$Filo != "Gim",]
bio.gim.a_Fsf<- bio.Fsf [bio.Fsf$Gen == "Araucaria",]
bio.gim.p_Fsf<- bio.Fsf [bio.Fsf$Gen == "Podocarpus",]
bio.trop.Fsf<- bio.Fsf[bio.Fsf$Distri == "Trop",]
ang.temp.Fsf=sum(bio.ang.temp.Fsf$biom)
gim.temp.Fsf=c(sum(bio.gim.a_Fsf$biom),sum(bio.gim.p_Fsf$biom))
trop.Fsf=sum(bio.trop.Fsf$biom)
por.t.t.Fsf= (c(gim.temp.Fsf,ang.temp.Fsf,trop.Fsf)/
                sum(gim.temp.Fsf,ang.temp.Fsf,trop.Fsf)*100)

bio.ang.temp.Fbar<- bio.Fbar[bio.Fbar$Distri == "Temp" & bio.Fbar$Filo != "Gim",]
bio.gim.a_Fbar<- bio.Fbar[bio.Fbar$Gen == "Araucaria",]
bio.gim.p_Fbar<- bio.Fbar[bio.Fbar$Gen == "Podocarpus",]
bio.trop.Fbar<- bio.Fbar[bio.Fbar$Distri == "Trop",]
ang.temp.Fbar=sum(bio.ang.temp.Fbar$biom)
gim.temp.Fbar=c(sum(bio.gim.a_Fbar$biom),sum(bio.gim.p_Fbar$biom))
trop.Fbar=sum(bio.trop.Fbar$biom)
por.t.t.Fbar= (c(gim.temp.Fbar,ang.temp.Fbar,trop.Fbar)/
                 sum(gim.temp.Fbar,ang.temp.Fbar,trop.Fbar)*100)

bio.ang.temp.bc<- bio.bc[bio.bc$Distri == "Temp" & bio.bc$Filo != "Gim",]
bio.gim.temp.bc<- bio.bc[bio.bc$Filo == "Gim",]
bio.trop.bc<- bio.bc[bio.bc$Distri == "Trop",]
ang.temp.bc=sum(bio.ang.temp.bc$biom)
gim.temp.bc=sum(bio.gim.temp.bc$biom)
trop.bc=sum(bio.trop.bc$biom)
por.t.t.bc= (c(gim.temp.bc,0,ang.temp.bc,trop.bc)/
               sum(gim.temp.bc,ang.temp.bc,trop.bc)*100)


bio.ang.temp.It<- bio.It[bio.It$Distri == "Temp" & bio.It$Filo != "Gim",]
bio.gim.temp.It<- bio.It[bio.It$Filo == "Gim",]
bio.trop.It<- bio.It[bio.It$Distri == "Trop",]
ang.temp.It=sum(bio.ang.temp.It$biom)
gim.temp.It=sum(bio.gim.temp.It$biom)
trop.It=sum(bio.trop.It$biom)
por.t.t.It= (c(gim.temp.It,0,ang.temp.It,trop.It)/
               sum(gim.temp.It,ang.temp.It,trop.It)*100)

bio.tem.trop.pro = data.frame(
  BC_SP=c(por.t.t.bc),
  IT_SP=c(por.t.t.It),
  CJ_SP=c(por.t.t.cj),
  FSF_MG=c(por.t.t.Fsf),
  FB_MG=c(por.t.t.Fbar),
  BP_MG=c(por.t.t.bp)
  )


rownames(bio.tem.trop.pro) <- c("Araucaria","Podocarpus","Ang_Temp","Ang_Trop")
colnames(bio.tem.trop.pro) <- c("Barra \n do Chapéu"="MF1"
                                ,"Itaberá" = "MF2",
                                "Campos do Jordão"="MF3",
                                "Delfim Moreira \n Faz. São Fran."="MF5",
                                "Delfim Moreira \n Faz. Bart."="MF4",
                                "Baependi"="MF6"
                                )

#colnames(bio.tem.trop.pro) <- c("MF1","MF5",
#                                "MF4","MF6",
#                                "MF3","MF2")


bio.temp.trop= as.matrix (bio.tem.trop.pro)
barplot(bio.temp.trop)

######

bio.tem.trop.ab = data.frame(
  BC_SP=c(gim.temp.bc, 0, ang.temp.bc, trop.bc)/1000,
  IT_SP=c(gim.temp.It, 0, ang.temp.It, trop.It)/1000,
  CJ_SP=c(gim.temp.cj, ang.temp.cj, trop.cj)/1000,
  FSF_MG=c(gim.temp.Fsf, ang.temp.Fsf, trop.Fsf)/1000,
  FB_MG=c(gim.temp.Fbar, ang.temp.Fbar, trop.Fbar)/1000,
  BP_MG=(c(gim.temp.bp, ang.temp.bp, trop.bp)/1000)/0.5
)

rownames(bio.tem.trop.ab) <- c("Araucaria","Podocarpus","Ang_Temp","Ang_Trop")
colnames(bio.tem.trop.ab) <- c("Barra \n do Chapéu"="MF1"
                                ,"Itaberá" = "MF2",
                                "Campos do Jordão"="MF3",
                                "Delfim Moreira \n Faz. São Fran."="MF5",
                                "Delfim Moreira \n Faz. Bart."="MF4",
                                "Baependi"="MF6"
)

bio.tem.trop.ab= as.matrix (bio.tem.trop.ab)
barplot(bio.tem.trop.ab)
########



jpeg(filename = "biomas_temp__xtrop.jpg", width = 700, height = 900, # fun��o salva gr�ficos em .jpg
     units = "px", quality = 75,
     bg = "white")

par(mfrow=c(1,1),mar=c(3,6,2,2), cex.axis=1, cex.lab=1.5, mgp=c(3.5,1.6,0),
    family="serif",las=1, tcl=0.3, bg="white")

color <- colorRampPalette(c("sandybrown","lightgreen"))

barplot (bio.temp.trop, col=color (4),
         ylim=c(0,120), ylab="Biomass %")


legend("topright" #fun��o adiciona um texto ao gr�fico,
       #arg 1� define a localiza��o, usa-se a fun��o locator para
       #adicionar de uma forma interativa
       ,c("Araucaria","Podocarpus","Ang_Temp","Ang_Trop") #texto a ser escrito
       ,col=color (4)
       ,cex=1.3		#tamanho da fonte
       , pch=c(15,15)
       ,bty = "n")


dev.off()



jpeg(filename = "biomas_temp__xtrop_ab.jpg", width = 700, height = 900, # fun��o salva gr�ficos em .jpg
     units = "px", quality = 75,
     bg = "white")

par(mfrow=c(1,1),mar=c(3,6,2,2), cex.axis=1, cex.lab=1.5, mgp=c(3.5,1.6,0),
    family="serif",las=1, tcl=0.3, bg="white")

color <- colorRampPalette(c("sandybrown","lightgreen"))

barplot (bio.tem.trop.ab, col=color (4),
         ylim=c(0,350), ylab=expression(
           paste (
           "Biomass ", Mg.ha^-1)
           )
         )

legend("topright" #fun��o adiciona um texto ao gr�fico,
       #arg 1� define a localiza��o, usa-se a fun��o locator para
       #adicionar de uma forma interativa
       ,c("Araucaria","Podocarpus","Ang_Temp","Ang_Trop") #texto a ser escrito
       ,col=color (4)
       ,cex=1.3		#tamanho da fonte
       , pch=c(15,15)
       ,bty = "n")


dev.off()

#################################


bio.gim.cj <- bio.cj [bio.cj$Filo == "Gim",]
bio.eud.cj <- bio.cj [bio.cj$Filo == "Eud",]
bio.mag.cj <- bio.cj [bio.cj$Filo == "Mag",]
gim.cj=sum(bio.gim.cj$biom)
eud.cj=sum(bio.eud.cj$biom)
mag.cj=sum(bio.mag.cj$biom)
por.b.cj= (c(gim.cj,eud.cj,mag.cj)/
             sum(gim.cj,eud.cj,mag.cj)*100)

bio.gim.bp <- bio.bp[bio.bp$Filo == "Gim",]
bio.eud.bp<- bio.bp[bio.bp$Filo == "Eudi",]
bio.mag.bp<- bio.bp[bio.bp$Filo == "Mag",]
gim.bp=sum(bio.gim.bp$biom)
eud.bp=sum(bio.eud.bp$biom)
mag.bp=sum(bio.mag.bp$biom)
por.b.bp= (c(gim.bp,eud.bp,mag.bp)/
             sum(gim.bp,eud.bp,mag.bp)*100)

bio.gim.Fsf <- bio.Fsf[bio.Fsf$Filo == "Gim",]
bio.eud.Fsf<- bio.Fsf[bio.Fsf$Filo == "Eud",]
bio.mag.Fsf<- bio.Fsf[bio.Fsf$Filo == "Mag",]
gim.Fsf=sum(bio.gim.Fsf$biom)
eud.Fsf=sum(bio.eud.Fsf$biom)
mag.Fsf=sum(bio.mag.Fsf$biom)
por.b.Fsf= (c(gim.Fsf,eud.Fsf,mag.Fsf)/
              sum(gim.Fsf,eud.Fsf,mag.Fsf)*100)

bio.gim.Fbar <- bio.Fbar[bio.Fbar$Filo == "Gim",]
bio.eud.Fbar<- bio.Fbar[bio.Fbar$Filo == "Eud",]
bio.mag.Fbar<- bio.Fbar[bio.Fbar$Filo == "Mag",]
gim.Fbar=sum(bio.gim.Fbar$biom)
eud.Fbar=sum(bio.eud.Fbar$biom)
mag.Fbar=sum(bio.mag.Fbar$biom)
por.b.Fbar= (c(gim.Fbar,eud.Fbar,mag.Fbar)/
               sum(gim.Fbar,eud.Fbar,mag.Fbar)*100)

bio.gim.It <- bio.It[bio.It$Filo == "Gim",]
bio.eud.It<- bio.It[bio.It$Filo == "Eud",]
bio.mag.It<- bio.It[bio.It$Filo == "Mag",]
gim.It=sum(bio.gim.It$biom)
eud.It=sum(bio.eud.It$biom)
mag.It=sum(bio.mag.It$biom)
por.b.It= (c(gim.It,eud.It,mag.It)/
             sum(gim.It,eud.It,mag.It)*100)

bio.gim.bc <- bio.bc[bio.bc$Filo == "Gim",]
bio.eud.bc<- bio.bc[bio.bc$Filo == "Eud",]
bio.mag.bc<- bio.bc[bio.bc$Filo == "Mag",]
gim.bc=sum(bio.gim.bc$biom)
eud.bc=sum(bio.eud.bc$biom)
mag.bc=sum(bio.mag.bc$biom)
por.b.bc= (c(gim.bc,eud.bc,mag.bc)/
             sum(gim.bc,eud.bc,mag.bc)*100)

bio.filo.pro = data.frame(
  BC_SP=c(por.b.bc),
  IT_SP=c(por.b.It),
  CJ_SP=c(por.b.cj),
  FSF_MG=c(por.b.Fsf),
  FB_MG=c(por.b.Fbar),
  BP_MG=c(por.b.bp)
  )

bio.filo.ab = data.frame(
  BC_SP=c(gim.bc, eud.bc, mag.bc)/1000,
  IT_SP=c(gim.It, eud.It, mag.It)/1000,
  CJ_SP=c(gim.cj, eud.cj, mag.cj)/1000,
  FSF_MG=c(gim.Fsf, eud.Fsf, mag.Fsf)/1000,
  FB_MG=c(gim.Fbar, eud.Fbar, mag.Fbar)/1000,
  BP_MG=(c(gim.bp, eud.bp, mag.bp)/1000)/0.5
)


rownames(bio.filo.pro) <- c("Gimnosperma","Eudicotiledonia","Magnoliidea")
colnames(bio.filo.pro) <- c("Barra \n do Chapéu"="MF1"
                            ,"Itaberá" = "MF2",
                            "Campos do Jordão"="MF3",
                            "Delfim Moreira \n Faz. São Fran."="MF5",
                            "Delfim Moreira \n Faz. Bart."="MF4",
                            "Baependi"="MF6"
                                )
bio.filo.pro= as.matrix (bio.filo.pro)

rownames(bio.filo.ab) <- c("Gimnosperma","Eudicotiledonia","Magnoliidea")
colnames(bio.filo.ab) <- c("Barra \n do Chapéu"="MF1"
                            ,"Itaberá" = "MF2",
                            "Campos do Jordão"="MF3",
                            "Delfim Moreira \n Faz. São Fran."="MF5",
                            "Delfim Moreira \n Faz. Bart."="MF4",
                            "Baependi"="MF6"
)
bio.filo.ab= as.matrix (bio.filo.ab)



jpeg(filename = "biomas_filo.jpg", width = 700, height = 850, # fun��o salva gr�ficos em .jpg
     units = "px", quality = 75,
     bg = "white")

par(mfrow=c(1,1),mar=c(3,5,2,2), cex.axis=1, cex.lab=1.5, mgp=c(3.5,1.6,0),
    family="serif",las=1, tcl=0.3, bg="white")

color <- colorRampPalette(c("sandybrown","lightgreen"))

barplot (bio.filo.pro, col=color (3),
         ylim=c(0,120), ylab="Biomass %")


legend("topright" #funcao adiciona um texto ao gr�fico,
       #arg 1� define a localizacao, usa-se a funcao locator para
       #adicionar de uma forma interativa
       ,c("Gymnosperms", "Eudicotyledonia", "Magnoliidea ") #texto a ser escrito
       ,col=color (3)
       ,cex=1.3		#tamanho da fonte
       , pch=c(15,15)
       ,bty = "n")


dev.off()


jpeg(filename = "biomas_filo_ab.jpg", width = 700, height = 850, # fun��o salva gr�ficos em .jpg
     units = "px", quality = 75,
     bg = "white")

par(mfrow=c(1,1),mar=c(3, 6, 2, 2), cex.axis=1, cex.lab=1.5, mgp=c(3.5,1.6,0),
    family="serif",las=1, tcl=0.3, bg="white")

color <- colorRampPalette(c("sandybrown","lightgreen"))

barplot (bio.filo.ab, col=color (3),
         ylim=c(0,350),
         ylab=expression (
           paste ("Biomass ", Mg.ha^-1)
           ) )


legend("topright" #funcao adiciona um texto ao gr�fico,
       #arg 1� define a localizacao, usa-se a funcao locator para
       #adicionar de uma forma interativa
       ,c("Gymnosperms", "Eudicotyledonia", "Magnoliidea ") #texto a ser escrito
       ,col=color (3)
       ,cex=1.3		#tamanho da fonte
       , pch=c(15,15)
       ,bty = "n")


dev.off()



#################ANALISES CORRELAÇÃO#############

#### dads.env -> está no script "Dados de cj estudos" utilizado para o CCA similaridade
#### similaridade florística. coluna 3 = altitude, 15 = precipitação anual, 4= temperatura média
#### 8 = temperatura mês mais quente, 9= temp. mes mais frio, 16= mes chuvoso, 17 = mes seco

#install.packages (c("lme4","bbmle"))
library(lme4)
library(bbmle)






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

###################################################################################################
#####################################Densidade Madeira#############################################

jpeg(filename = "Boxplot densmad.jpg", width = 850, height = 500, # fun��o salva gr�ficos em .jpg
     units = "px", quality = 75,
     bg = "white")

par(mfrow=c(2,3),mar=c(5,6,3,2), cex.axis=1.3, cex.lab=1.5, mgp=c(3,1.3,0.3),
      family="serif",las=1, tcl=0.3, bty = "n", xaxt="n")

color <- colorRampPalette(c("sandybrown","lightgreen"))

boxplot (DensM ~ Distri, data=bio.bc,horizontal = FALSE,
         main="Barra do Chapéu (MF1)",
         col = color(2), xlab=" ",
         ylab= expression (
           paste ("Wood density ", g.cm^-3)))

mtext( #fun��o plota textos nas �reas ao redor do gr�fico
  c("temperate","tropical"), #primeiro argumento refere oa texto plotado
  side= 1, #argumento localiza no gr�fico "1" abaixo
  cex=1,line=0.9,
  at=c(1,2))

boxplot (DensM ~ Distri, data=bio.It,horizontal = FALSE, main="Itaberá (MF2)",
         col = color(2), ylab= "", xlab=" ")
mtext( #fun��o plota textos nas �reas ao redor do gr�fico
  c("temperate","tropical"), #primeiro argumento refere oa texto plotado
  side= 1, #argumento localiza no gr�fico "1" abaixo
  cex=1,line=0.9,
  at=c(1,2))


boxplot (DensM ~ Distri, data=bio.cj, horizontal = FALSE,
         main="Campos do Jordão (MF3)", xlab =" ", ylab = " ",
         col = color(2))


mtext( #fun��o plota textos nas �reas ao redor do gr�fico
  c("temperate","tropical"), #primeiro argumento refere oa texto plotado
  side= 1, #argumento localiza no gr�fico "1" abaixo
  cex=1,line=0.9,
  at=c(1,2))

boxplot (DensM ~ Distri, data=bio.Fbar,horizontal = FALSE,
         main="Delfim Moreira \n Faz. Bartira (MF4)",
         xlab ="Phytogeographic distribution",
         col = color(2), ylab= expression (paste ("Wood density ", g.cm^-3)))



mtext( #fun��o plota textos nas �reas ao redor do gr�fico
  c("temperate","tropical"), #primeiro argumento refere oa texto plotado
  side= 1, #argumento localiza no gr�fico "1" abaixo
  cex=1,line=0.9,
  at=c(1,2))


boxplot (DensM ~ Distri, data=bio.Fsf,horizontal = FALSE,
         main="Delfim Moreira \n Faz. S. Fran. (MF5)"
         , xlab ="Phytogeographic distribution",
         col = color(2), ylab= "")


mtext( #fun��o plota textos nas �reas ao redor do gr�fico
  c("temperate","tropical"), #primeiro argumento refere oa texto plotado
  side= 1, #argumento localiza no gr�fico "1" abaixo
  cex=1,line=0.9,
  at=c(1,2))

boxplot (DensM ~ Distri, data=bio.bp,horizontal = FALSE,
         main="Baependi(MF6)",
         xlab ="Phytogeographic distribution", ylab=" ",
         col = color(2)
         )

mtext( #fun��o plota textos nas �reas ao redor do gr�fico
  c("temperate","tropical"), #primeiro argumento refere oa texto plotado
  side= 1, #argumento localiza no gr�fico "1" abaixo
  cex=1,line=0.9,
  at=c(1,2))


dev.off()
###################################################################################################
###################################################################################################
################vioplot biomassa################################################


jpeg(filename = "Boxplot biomass.jpg", width = 850, height = 500, # fun��o salva gr�ficos em .jpg
     units = "px", quality = 75,
     bg = "white")

par(mfrow=c(2,3),mar=c(5,5,3,2), cex.axis=1.3, cex.lab=1.5, mgp=c(3,1.3,0.3),
    family="serif",las=1, tcl=0.3, bty = "n", xaxt="n")


color <- colorRampPalette(c("sandybrown","lightgreen"))


vioplot (biom/1000  ~ Distri, data=bio.bc,horizontal = FALSE,
         main="Barra do Chapéu (MF1)",
         xlab =" ",
         col = color(2), ylab= "Biomass (Mg)")


mtext( #fun��o plota textos nas �reas ao redor do gr�fico
  c("temperate","tropical"), #primeiro argumento refere oa texto plotado
  side= 1, #argumento localiza no gr�fico "1" abaixo
  cex=1,line=0.9,
  at=c(1,2))

vioplot (biom/1000 ~ Distri, data=bio.It,horizontal = FALSE, main="Itaberá (MF2)",
         xlab =" ",
         col = color(2), ylab= "")


mtext( #fun��o plota textos nas �reas ao redor do gr�fico
  c("temperate","tropical"), #primeiro argumento refere oa texto plotado
  side= 1, #argumento localiza no gr�fico "1" abaixo
  cex=1,line=0.9,
  at=c(1,2))

vioplot (biom/1000  ~ Distri, data=bio.cj, horizontal = FALSE,
         main="Campos do Jordão (MF3)", xlab =" ",
         col = color(2), ylab= " ")


mtext( #fun��o plota textos nas �reas ao redor do gr�fico
  c("temperate","tropical"), #primeiro argumento refere oa texto plotado
  side= 1, #argumento localiza no gr�fico "1" abaixo
  cex=1,line=0.9,
  at=c(1,2))

vioplot (biom/1000  ~ Distri, data=bio.Fbar,horizontal = FALSE,
         main="Delfim Moreira \n Faz. Bartira (MF4)",
         xlab ="Phytogeographic distribution",
         col = color(2), ylab= "Biomass (Mg)")



mtext( #fun��o plota textos nas �reas ao redor do gr�fico
  c("temperate","tropical"), #primeiro argumento refere oa texto plotado
  side= 1, #argumento localiza no gr�fico "1" abaixo
  cex=1,line=0.9,
  at=c(1,2))


vioplot (biom/1000  ~ Distri, data=bio.Fsf,horizontal = FALSE,
         main="Delfim Moreira \n Faz. S. Fran. (MF5)"
         , xlab ="Phytogeographic distribution",
         col = color(2), ylab= "")
mtext( #fun��o plota textos nas �reas ao redor do gr�fico
  c("temperate","tropical"), #primeiro argumento refere oa texto plotado
  side= 1, #argumento localiza no gr�fico "1" abaixo
  cex=1,line=0.9,
  at=c(1,2))



vioplot (biom/1000 ~ Distri, data=bio.bp,horizontal = FALSE,
         main="Baependi(MF6)",
         xlab ="Phytogeographic distribution",
         col = color(2), ylab= " ", yaxt = "n"
)

axis (2, at = seq (0,3,by=1))

mtext( #fun��o plota textos nas �reas ao redor do gr�fico
  c("temperate","tropical"), #primeiro argumento refere oa texto plotado
  side= 1, #argumento localiza no gr�fico "1" abaixo
  cex=1,line=0.9,
  at=c(1,2))


dev.off()

################################################################
################################################################
################################################################
################vioplot biomassa filo###########################


jpeg(filename = "Boxplot biomass_filo.jpg", width = 850, height = 500, # fun��o salva gr�ficos em .jpg
     units = "px", quality = 75,
     bg = "white")

par(mfrow=c(2,3),mar=c(5,5,3,2), cex.axis=1.3, cex.lab=1.5, mgp=c(3,1.3,0.3),
    family="serif",las=1, tcl=0.3, bty = "n", xaxt="n")


color <- colorRampPalette(c("sandybrown","lightgreen"))


vioplot (biom/1000  ~ Filo, data=bio.bc,horizontal = FALSE,
         main="Barra do Chapéu (MF1)",
         xlab =" ",
         col = color(4), ylab= "Biomass (Mg)")


mtext( #fun��o plota textos nas �reas ao redor do gr�fico
  c("eud","con","mag", "palm"), #primeiro argumento refere oa texto plotado
  side= 1, #argumento localiza no gr�fico "1" abaixo
  cex=1,line=0.9,
  at=c(1,2,3,4))

vioplot (biom/1000 ~ Filo, data=bio.It,horizontal = FALSE, main="Itaberá (MF2)",
         xlab =" ",
         col = color(4), ylab= "")

mtext( #fun��o plota textos nas �reas ao redor do gr�fico
  c("eud","con","mag", "palm"), #primeiro argumento refere oa texto plotado
  side= 1, #argumento localiza no gr�fico "1" abaixo
  cex=1,line=0.9,
  at=c(1,2,3,4))

vioplot (biom/1000  ~ Filo, data=bio.cj, horizontal = FALSE,
         main="Campos do Jordão (MF3)", xlab =" ",
         col = color(3), ylab= " ")


mtext( #fun��o plota textos nas �reas ao redor do gr�fico
  c("eud","con","mag"), #primeiro argumento refere oa texto plotado
  side= 1, #argumento localiza no gr�fico "1" abaixo
  cex=1,line=0.9,
  at=c(1,2,3))

vioplot (biom/1000  ~ Filo, data=bio.Fbar,horizontal = FALSE,
         main="Delfim Moreira \n Faz. Bartira (MF4)",
         xlab ="Phytogeographic distribution",
         col = color(3), ylab= "Biomass (Mg)")


mtext( #fun��o plota textos nas �reas ao redor do gr�fico
  c("eud","con","mag"), #primeiro argumento refere oa texto plotado
  side= 1, #argumento localiza no gr�fico "1" abaixo
  cex=1,line=0.9,
  at=c(1,2,3))

vioplot (biom/1000  ~ Filo, data=bio.Fsf,horizontal = FALSE,
         main="Delfim Moreira \n Faz. S. Fran. (MF5)"
         , xlab ="Phytogeographic distribution",
         col = color(3), ylab= " ")


mtext( #fun��o plota textos nas �reas ao redor do gr�fico
  c("eud","con","mag"), #primeiro argumento refere oa texto plotado
  side= 1, #argumento localiza no gr�fico "1" abaixo
  cex=1,line=0.9,
  at=c(1,2,3))

vioplot (biom/1000 ~ Filo, data=bio.bp,horizontal = FALSE,
         main="Baependi(MF6)",
         xlab ="Phytogeographic distribution",
         col = color(3), ylab= " ", yaxt = "n"
)
axis (2, at = seq (0,3,by=1))

mtext( #fun��o plota textos nas �reas ao redor do gr�fico
  c("eud","con","mag"), #primeiro argumento refere oa texto plotado
  side= 1, #argumento localiza no gr�fico "1" abaixo
  cex=1,line=0.9,
  at=c(1,2,3))


dev.off()

################################################################


bio.cj$biom

chisq.test (bio.temp.trop)
chisq.test (bio.tem.trop.pro)

ang=sum(bio.temp.trop[c(3,4),])
gim=sum(bio.temp.trop[c(1,2),])
anova (ang,gim)
med=mean (bio.temp.trop)
dbio=bio.temp.trop-med
dqbio=dbio^2
sqbio=sum(dqbio)

###########################################
### Estrutura ####

cjsoma <- sum (count (bio.cj, Lvl.D) [2])
Itsoma <- sum(count (bio.It, Lvl.D)[2])
bcsoma <- sum (count (bio.bc, Lvl.D)[2])
bpsoma <- sum (count (bio.bp, Lvl.D)[2])
fsfsoma <- sum (count (bio.Fsf, Lvl.D)[2])
fbarsoma <- sum (count (bio.Fbar, Lvl.D)[2])

count (bio.cj, Lvl.D) [2]/cjsoma * 100
count (bio.It, Lvl.D) [2]/Itsoma * 100
count (bio.bc, Lvl.D) [2]/bcsoma * 100
count (bio.bp, Lvl.D) [2]/bpsoma * 100
count (bio.Fsf, Lvl.D) [2]/fsfsoma * 100
count (bio.Fbar, Lvl.D) [2]/fbarsoma * 100



range(bio.cj$biom) / 1000
mean(bio.cj$biom) /1000
sum (bio.cj$biom) /1000

range(bio.It$DAP)
mean(bio.It$DAP)
range(bio.It$biom)/ 1000
mean(bio.It$biom)/ 1000
sum (bio.It$biom) /1000


range(bio.bc$DAP)
mean(bio.bc$DAP)
range(bio.bc$biom)/1000
mean(bio.bc$biom)/1000
sum (bio.bc$biom)/1000

range(bio.bp$DAP)
mean(bio.bp$DAP)
range(bio.bp$biom)/1000
mean(bio.bp$biom)/1000
(sum (bio.bp$biom)/1000) / 0.5

range(bio.Fsf$DAP)
mean(bio.Fsf$DAP)
range(bio.Fsf$biom)/1000
mean(bio.Fsf$biom)/1000
sum (bio.Fsf$biom)/1000

range(bio.Fbar$DAP)
mean(bio.Fbar$DAP)
range(bio.Fbar$biom)/1000
mean(bio.Fbar$biom)/1000
sum(bio.Fbar$biom)/1000



total_individuos <- sum (length (bio.cj$Spp),length(bio.It$Spp),length(bio.bc$Spp),
     length(bio.bp$Spp),length(bio.Fsf$Spp),length(bio.Fbar$Spp))

individuos_campos <- length (bio.cj$Spp)
individuos_itabera <- length (bio.It$Spp)
individuos_barra <- length (bio.bc$Spp)
individuos_baependi <- length (bio.bp$Spp)
individuos_faz_saofrancisco <- length (bio.Fsf$Spp)
individuos_faz_bartira <- length (bio.Fbar$Spp)

(individuos_campos/total_individuos) * 100
(individuos_itabera/total_individuos) * 100
(individuos_barra/total_individuos) * 100
(individuos_faz_bartira/total_individuos) * 100
(individuos_faz_saofrancisco/total_individuos) * 100
(individuos_baependi/total_individuos) * 100

sum (bio.cj$biom)/1000
sum(bio.It$biom)/1000
sum (bio.bc$biom)/1000
sum ((bio.bp$biom)/1000) /0.5
sum (bio.Fsf$biom)/1000
sum (bio.Fbar$biom)/1000

head (bio.cj)
summary (bio.Fbar)
summary (bio.Fsf)
summary (bio.bp)
summary (bio.It)
summary (bio.bc)


g_cj=count (bio.cj, Gen, sort=TRUE)
length (g_cj$Gen)
length (g_cj$Gen)/length (all_g$n) *100

g_it=count (bio.It, Gen, sort=TRUE)
length (g_it$Gen)
length (g_it$Gen)/length (all_g$n) *100

g_bc=count (bio.bc, Gen, sort=TRUE)
length (g_bc$Gen)
length (g_bc$Gen)/length (all_g$n) *100


g_Fbar=count (bio.Fbar, Gen, sort=TRUE)
length (g_Fbar$Gen)
length (g_Fbar$Gen)/length (all_g$n) *100


g_Fsf=count (bio.Fsf, Gen, sort=TRUE)
length (g_Fsf$Gen)
length (g_Fsf$Gen)/length (all_g$n) *100


g_bp=count (bio.bp, Gen, sort=TRUE)
length (g_bp$Gen)
length (g_bp$Gen)/length (all_g$n) *100

F_G=rbind (g_cj,g_it,g_bc,g_bp,g_Fsf,g_Fbar)
all_g=count (F_G, Gen)
length (all_g$n)

All=rbind (bio.cj,bio.It,bio.bc,bio.bp,bio.Fsf,bio.Fbar)

All_g=count (All,Gen, Spp)

All_g [order (All_g$n,decreasing = TRUE),]

length(All_g$n)


F_G[order (F_G$n, decreasing = TRUE),]

F_gs <- F_G[!duplicated  (F_G$Gen),]

length(F_gs$n)

cj=count (bio.cj, Gen, Spp, sort =TRUE)
head (cj)
length (cj$Spp)
length (cj$Spp)/length(All_g$n)

diversity (cj$n)
diversity (cj$n, "simpson")

it=count (bio.It, Gen, Spp, sort =TRUE)
head (it)
length (it$Spp)
length (it$Spp)/length(All_g$n)


diversity (it$n)
diversity (it$n, "simpson")

bc=count (bio.bc, Gen, Spp, sort =TRUE)
head (bc)
length (bc$Spp)
length (bc$Spp)/length(All_g$n)

diversity (bc$n)
diversity (bc$n, "simpson")

bp=count (bio.bp, Gen, Spp, sort =TRUE)
head (bp)
length (bp$Spp)
length (bp$Spp)/length(All_g$n)


diversity (bp$n)
diversity (bp$n, "simpson")

Fsf=count (bio.Fsf, Gen, Spp, sort =TRUE)
head (Fsf)
length (Fsf$Spp)
length (Fsf$Spp)/length(All_g$n)


diversity (Fsf$n)
diversity (Fsf$n, "simpson")

Fbar=count (bio.Fbar, Gen, Spp, sort =TRUE)
head (Fbar)
length (Fbar$Spp)
length (Fbar$Spp)/length(All_g$n)


diversity (Fbar$n)
diversity (Fbar$n, "simpson")


F_E=rbind (cj,it,bc,bp,Fsf,Fbar)
G=count (F_E, Gen)
G <- G[order (G$n),]
F_es <- F_E[!duplicated  (paste(F_E$Gen,F_E$Spp)),]
length(F_es$Gen)



F1=count (bio.cj, Fam,  sort =TRUE)
length (F1$Fam)

F2=count (bio.It, Fam, sort =TRUE)
length (F2$Fam)

F3=count (bio.bc, Fam,  sort =TRUE)
length (F3$Fam)

F4=count (bio.bp, Fam, sort =TRUE)
length (F4$Fam)

F5=count (bio.Fsf, Fam, sort =TRUE)
length (F5$Fam)

F6=count (bio.Fbar, Fam, sort =TRUE)
length (F6$Fam)

F_=rbind (F1,F2,F3,F4,F5,F6)

F_fe=count (F_, Fam)
F_fe <- F_fe[order (F_fe$n, decreasing = TRUE),]
length(F_fe$Fam)

F_s <- F_[!duplicated  (F_$Fam),]
length(F_s$Fam)

palm =bio.It [bio.It$Fam=="Arecaceae",]

palm |>
  group_by(Alt)|>
  count (Spp)

palm |>
  group_by(DAP)|>
  count (Spp)


