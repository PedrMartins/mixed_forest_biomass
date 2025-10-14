########################GRÁFICOS BIOMASSA##############################
###################rodar script biomass_calc antes#####################


############################################################
###Suplementar########

source ("import_processing_biomass_data.R")
source("processing_to_table.R")


###DAP class porcentagem ind########

jpeg(filename = "Class_DAP_ind_distribution.jpg", width = 1050, height = 700, # function to save plots salva .jpg
     units = "px", quality = 75,
     bg = "white")

par(mfrow=c(2,3),mar=c(5,5,3,2), cex.axis=1.3, cex.lab=2.5,
    cex.main = 2,  mgp=c(3,1.3,0.3),
    family="serif",las=1, tcl=0.3, bg= "white")
color= colorRampPalette(c("lightgreen","sandybrown"))

barplot (ind_all_bc_sep_by_DHB_distribution$Ind_percentage,
         col=color (2), ylim=c(0,70),
         main="Barra do Chapéu (MF1)",
         ylab = "Individuals (%)" )


mtext( #fun��o plota textos nas �reas ao redor do gr�fico
  c("0-10", "10-30","30-50", ">50"), #primeiro argumento refere oa texto plotado
  side= 1, #argumento localiza no grafico "1" abaixo
  cex=1,line=0.9,
  at=c(1.4,3.5,6,8.5))


barplot (ind_all_it_sep_by_DHB_distribution$Ind_percentage,
         col=color (2), ylim=c(0,70),
         main="Itaberá (MF2)")

mtext( #fun��o plota textos nas �reas ao redor do gr�fico
  c("0-10", "10-30","30-50", ">50"), #primeiro argumento refere oa texto plotado
  side= 1, #argumento localiza no gr�fico "1" abaixo
  cex=1,line=0.9,
  at=c(1.4,3.5,6,8.5))


barplot (ind_all_cj_sep_by_DHB_distribution$Ind_percentage,
         col=color (2), ylim=c(0,70),
         main="Campos do Jordão (MF3)")

mtext( #fun��o plota textos nas �reas ao redor do gr�fico
  c("0-10", "10-30","30-50", ">50"), #primeiro argumento refere oa texto plotado
  side= 1, #argumento localiza no gr�fico "1" abaixo
  cex=1,line=0.9,
  at=c(1.4,3.5,6,8.5))


barplot (ind_all_Fbar_sep_by_DHB_distribution$Ind_percentage,
         col=color (2), ylim=c(0,70),
         main="Delfim Moreira \n São Francisco Farm (MF4)",
         ylab = "Individuals (%)",
         xlab="DBH Class (cm)")

mtext( #fun��o plota textos nas �reas ao redor do gr�fico
  c("0-10", "10-30","30-50", ">50"), #primeiro argumento refere oa texto plotado
  side= 1, #argumento localiza no gr�fico "1" abaixo
  cex=1,line=0.9,
  at=c(1.4,3.5,6,8.5))

barplot (ind_all_Fsf_sep_by_DHB_distribution$Ind_percentage,
         col=color (2), ylim=c(0,70),
         main="Delfim Moreira \n Bartira Farm (MF5)",
         xlab="DBH Class (cm)")


mtext( #fun��o plota textos nas �reas ao redor do gr�fico
  c("0-10", "10-30","30-50", ">50"), #primeiro argumento refere oa texto plotado
  side= 1, #argumento localiza no gr�fico "1" abaixo
  cex=1,line=0.9,
  at=c(1.4,3.5,6,8.5))


barplot (ind_all_bp_sep_by_DHB_distribution$Ind_percentage,
         col=color (2), ylim=c(0,70),
         main="Baependi (MF6)",
         xlab="DBH Class (cm)")

mtext( #fun��o plota textos nas �reas ao redor do gr�fico
  c("0-10", "10-30","30-50", ">50"), #primeiro argumento refere oa texto plotado
  side= 1, #argumento localiza no gr�fico "1" abaixo
  cex=1,line=0.9,
  at=c(1.4,3.5,6,8.5))

legend("topleft" #fun��o adiciona um texto ao gr�fico,
       #arg 1� define a localiza��o, usa-se a fun��o locator para
       #adicionar de uma forma interativa
       ,c("Tropical","Temperate") #texto a ser escrito
       ,col=color (2)
       ,cex=1.5		#tamanho da fonte
       , pch=c(15,15)
       ,bty = "n") #tipo da fonte

dev.off()


###########DAP Class porcentage BIO########


jpeg(filename = "Class_DAP_biomas2.jpg", width = 1050, height = 700, # fun��o salva gr�ficos em .jpg
     units = "px", quality = 95,
     bg = "white")

par(mfrow=c(2,3),mar=c(5,5,3,2), cex.axis=1.3, cex.lab=2.5,
    cex.main = 2, mgp=c(3,1.3,0.3),
    family="serif",las=1, tcl=0.3, bg="white")

color= colorRampPalette(c("lightgreen","sandybrown"))

barplot (biomass_all_bc_sep_by_DHB_distribution$Biomass_percentage,
         col=color (2), ylim=c(0,60),
         main="Barra do Chapéu (MF1)",
         ylab = "Biomass (%)")

mtext( #fun��o plota textos nas �reas ao redor do gr�fico
  c("0-10", "10-30","30-50", ">50"), #primeiro argumento refere oa texto plotado
  side= 1, #argumento localiza no gr�fico "1" abaixo
  cex=1,line=0.9,
  at=c(1.4,3.5,6,8.5))



barplot (biomass_all_it_sep_by_DHB_distribution$Biomass_percentage,
         col=color (2), ylim=c(0,60),
         main="Itaberá (MF2)")


mtext( #fun��o plota textos nas �reas ao redor do gr�fico
  c("0-10", "10-30","30-50", ">50"), #primeiro argumento refere oa texto plotado
  side= 1, #argumento localiza no grafico "1" abaixo
  cex=1,line=0.9,
  at=c(1.4,3.5,6,8.5))



barplot (biomass_all_cj_sep_by_DHB_distribution$Biomass_percentage,
         col=color (2), ylim=c(0,60),
         main="Campos do Jordão (MF3)")


mtext( #fun��o plota textos nas �reas ao redor do gr�fico
  c("0-10", "10-30","30-50", ">50"), #primeiro argumento refere oa texto plotado
  side= 1, #argumento localiza no gr�fico "1" abaixo
  cex=1,line=0.9,
  at=c(1.4,3.5,6,8.5))


barplot (biomass_all_Fsf_sep_by_DHB_distribution$Biomass_percentage,
         col=color (2), ylim=c(0,60),
         main="Delfim Moreira \nSão Francisco Farm (MF4)",
         ylab = "Biomass (%)",
         xlab="DBH Class (cm)")

mtext( #fun��o plota textos nas �reas ao redor do gr�fico
  c("0-10", "10-30","30-50", ">50"), #primeiro argumento refere oa texto plotado
  side= 1, #argumento localiza no gr�fico "1" abaixo
  cex=1,line=0.9,
  at=c(1.4,3.5,6,8.5))

barplot (biomass_all_Fbar_sep_by_DHB_distribution$Biomass_percentage,
         col=color (2), ylim=c(0,60),
         main="Delfim Moreira \nBartira Farm (MF5)",
         xlab="DBH Class (cm)")


mtext( #fun��o plota textos nas �reas ao redor do gr�fico
  c("0-10", "10-30","30-50", ">50"), #primeiro argumento refere oa texto plotado
  side= 1, #argumento localiza no gr�fico "1" abaixo
  cex=1,line=0.9,
  at=c(1.4,3.5,6,8.5))

barplot (biomass_all_bp_sep_by_DHB_distribution$Biomass_percentage,
         col=color (2), ylim=c(0,60),
         main="Baependi (MF6)",
         xlab="DBH Class (cm)")

mtext( #fun��o plota textos nas �reas ao redor do gr�fico
  c("0-10", "10-30","30-50", ">50"), #primeiro argumento refere oa texto plotado
  side= 1, #argumento localiza no gr�fico "1" abaixo
  cex=1,line=0.9,
  at=c(1.4,3.5,6,8.5))

legend("topleft" #fun��o adiciona um texto ao gr�fico,
       #arg 1� define a localiza��o, usa-se a fun��o locator para
       #adicionar de uma forma interativa
       ,c("Tropical","Temperate") #texto a ser escrito
       ,col=color (2)
       ,cex=1.5		#tamanho da fonte
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
                                "Delfim Moreira \n Faz. São Fran."="MF4",
                                "Delfim Moreira \n Faz. Bart."="MF5",
                                "Baependi"="MF6"
                                )
bio.podo.ara.pro= as.matrix (bio.podo.ara.pro)


jpeg(filename = "biomas_arau_podo_prop.jpg", width = 850, height = 900, # fun��o salva gr�ficos em .jpg
     units = "px", quality = 75,
     bg = "white")

par(mfrow=c(1,1), mar=c(3,6,2,2), cex.axis=1.3, cex.lab=1.5, mgp=c(3.5,1.6,0),
    family="serif",las=1, tcl=0.3, bg="white")

color= colorRampPalette(c("rosybrown1", "sandybrown"))
barplot (bio.podo.ara.pro, col=color (2),
         ylim=c(0,100),ylab="Biomass %", cex.lab = 2.5,
         cex.names = 2)



legend("topleft" #fun��o adiciona um texto ao gr�fico,
       #arg 1� define a localiza��o, usa-se a fun��o locator para
       #adicionar de uma forma interativa
       ,legend= c (expression (italic ("Araucaria")),
                   expression (italic ("Podocarpus"))) #texto a ser escrito
       ,col=color (2)
       ,cex=2.5		#tamanho da fonte
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
                                "Delfim Moreira \n Faz. São Fran."="MF4",
                                "Delfim Moreira \n Faz. Bart."="MF5",
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

ind.ang.temp.cj=length(bio.ang.temp.cj$biom)
ind.gim.temp.cj=c(length(bio.gim.a_cj$biom),length(bio.gim.p_cj$biom))
ind.trop.cj=length(bio.trop.cj$biom)
ind.por.t.t.cj= (c(ind.gim.temp.cj,ind.ang.temp.cj,ind.trop.cj)/
               sum(ind.gim.temp.cj,ind.ang.temp.cj,ind.trop.cj)*100)


bio.ang.temp.bp <- bio.bp [bio.bp$Distri == "Temp" & bio.bp$Filo != "Gim",]
bio.gim.a_bp <- bio.bp [bio.bp $Gen == "Araucaria",]
bio.gim.p_bp <- bio.bp [bio.bp $Gen == "Podocarpus",]
bio.trop.bp <- bio.bp [bio.bp$Distri == "Trop",]
ang.temp.bp =sum(bio.ang.temp.bp $biom)
gim.temp.bp =c(sum(bio.gim.a_bp $biom),sum(bio.gim.p_bp $biom))
trop.bp=sum(bio.trop.bp$biom)
por.t.t.bp= (c(gim.temp.bp,ang.temp.bp,trop.bp)/
               sum(gim.temp.bp,ang.temp.bp,trop.bp)*100)

ind.ang.temp.bp=length(bio.ang.temp.bp$biom)
ind.gim.temp.bp=c(length(bio.gim.a_bp$biom),length(bio.gim.p_bp$biom))
ind.trop.bp=length(bio.trop.bp$biom)
ind.por.t.t.bp= (c(ind.gim.temp.bp,ind.ang.temp.bp,ind.trop.bp)/
                   sum(ind.gim.temp.bp,ind.ang.temp.bp,ind.trop.bp)*100)




bio.ang.temp.Fsf <- bio.Fsf [bio.Fsf$Distri == "Temp" & bio.Fsf$Filo != "Gim",]
bio.gim.a_Fsf<- bio.Fsf [bio.Fsf$Gen == "Araucaria",]
bio.gim.p_Fsf<- bio.Fsf [bio.Fsf$Gen == "Podocarpus",]
bio.trop.Fsf<- bio.Fsf[bio.Fsf$Distri == "Trop",]
ang.temp.Fsf=sum(bio.ang.temp.Fsf$biom)
gim.temp.Fsf=c(sum(bio.gim.a_Fsf$biom),sum(bio.gim.p_Fsf$biom))
trop.Fsf=sum(bio.trop.Fsf$biom)
por.t.t.Fsf= (c(gim.temp.Fsf,ang.temp.Fsf,trop.Fsf)/
                sum(gim.temp.Fsf,ang.temp.Fsf,trop.Fsf)*100)

ind.ang.temp.Fsf=length(bio.ang.temp.Fsf$biom)
ind.gim.temp.Fsf=c(length(bio.gim.a_Fsf$biom),length(bio.gim.p_Fsf$biom))
ind.trop.Fsf=length(bio.trop.Fsf$biom)
ind.por.t.t.Fsf= (c(ind.gim.temp.Fsf,ind.ang.temp.Fsf,ind.trop.Fsf)/
                   sum(ind.gim.temp.Fsf,ind.ang.temp.Fsf,ind.trop.Fsf)*100)



bio.ang.temp.Fbar<- bio.Fbar[bio.Fbar$Distri == "Temp" & bio.Fbar$Filo != "Gim",]
bio.gim.a_Fbar<- bio.Fbar[bio.Fbar$Gen == "Araucaria",]
bio.gim.p_Fbar<- bio.Fbar[bio.Fbar$Gen == "Podocarpus",]
bio.trop.Fbar<- bio.Fbar[bio.Fbar$Distri == "Trop",]
ang.temp.Fbar=sum(bio.ang.temp.Fbar$biom)
gim.temp.Fbar=c(sum(bio.gim.a_Fbar$biom),sum(bio.gim.p_Fbar$biom))
trop.Fbar=sum(bio.trop.Fbar$biom)
por.t.t.Fbar= (c(gim.temp.Fbar,ang.temp.Fbar,trop.Fbar)/
                 sum(gim.temp.Fbar,ang.temp.Fbar,trop.Fbar)*100)

ind.ang.temp.Fbar=length(bio.ang.temp.Fbar$biom)
ind.gim.temp.Fbar=c(length(bio.gim.a_Fbar$biom),length(bio.gim.p_Fbar$biom))
ind.trop.Fbar=length(bio.trop.Fbar$biom)
ind.por.t.t.Fbar= (c(ind.gim.temp.Fbar,ind.ang.temp.Fbar,ind.trop.Fbar)/
                   sum(ind.gim.temp.Fbar,ind.ang.temp.Fbar,ind.trop.Fbar)*100)


bio.ang.temp.bc<- bio.bc[bio.bc$Distri == "Temp" & bio.bc$Filo != "Gim",]
bio.gim.temp.bc<- bio.bc[bio.bc$Filo == "Gim",]
bio.trop.bc<- bio.bc[bio.bc$Distri == "Trop",]
ang.temp.bc=sum(bio.ang.temp.bc$biom)
gim.temp.bc=sum(bio.gim.temp.bc$biom)
trop.bc=sum(bio.trop.bc$biom)
por.t.t.bc= (c(gim.temp.bc,0,ang.temp.bc,trop.bc)/
               sum(gim.temp.bc,ang.temp.bc,trop.bc)*100)

ind.ang.temp.bc=length(bio.ang.temp.bc$biom)
ind.gim.temp.bc=length(bio.gim.temp.bc$biom)
ind.trop.bc=length(bio.trop.bc$biom)
ind.por.t.t.bc= (c(ind.gim.temp.bc,0,ind.ang.temp.bc,ind.trop.bc)/
                   sum(ind.gim.temp.bc,ind.ang.temp.bc,ind.trop.bc)*100)


bio.ang.temp.It<- bio.It[bio.It$Distri == "Temp" & bio.It$Filo != "Gim",]
bio.gim.temp.It<- bio.It[bio.It$Filo == "Gim",]
bio.trop.It<- bio.It[bio.It$Distri == "Trop",]
ang.temp.It=sum(bio.ang.temp.It$biom)
gim.temp.It=sum(bio.gim.temp.It$biom)
trop.It=sum(bio.trop.It$biom)
por.t.t.It= (c(gim.temp.It,0,ang.temp.It,trop.It)/
               sum(gim.temp.It,ang.temp.It,trop.It)*100)

ind.ang.temp.It=length(bio.ang.temp.It$biom)
ind.gim.temp.It=length(bio.gim.temp.It$biom)
ind.trop.It=length(bio.trop.It$biom)
ind.por.t.t.It= (c(ind.gim.temp.It,0,ind.ang.temp.It,ind.trop.It)/
                   sum(ind.gim.temp.It,ind.ang.temp.It,ind.trop.It)*100)


bio.tem.trop.pro = data.frame(
  BC_SP=c(por.t.t.bc),
  IT_SP=c(por.t.t.It),
  CJ_SP=c(por.t.t.cj),
  FSF_MG=c(por.t.t.Fsf),
  FB_MG=c(por.t.t.Fbar),
  BP_MG=c(por.t.t.bp)
  )

ind.tem.trop.pro = data.frame(
  BC_SP=c(ind.por.t.t.bc),
  IT_SP=c(ind.por.t.t.It),
  CJ_SP=c(ind.por.t.t.cj),
  FSF_MG=c(ind.por.t.t.Fsf),
  FB_MG=c(ind.por.t.t.Fbar),
  BP_MG=c(ind.por.t.t.bp)
)



rownames(bio.tem.trop.pro) <- c("Araucaria","Podocarpus","Ang_Temp","Ang_Trop")
colnames(bio.tem.trop.pro) <- c("Barra \n do Chapéu"="MF1"
                                ,"Itaberá" = "MF2",
                                "Campos do Jordão"="MF3",
                                "Delfim Moreira \n Faz. São Fran."="MF4",
                                "Delfim Moreira \n Faz. Bart."="MF5",
                                "Baependi"="MF6"
                                )
rownames(ind.tem.trop.pro) <- c("Araucaria","Podocarpus","Ang_Temp","Ang_Trop")
colnames(ind.tem.trop.pro) <- c("Barra \n do Chapéu"="MF1"
                                ,"Itaberá" = "MF2",
                                "Campos do Jordão"="MF3",
                                "Delfim Moreira \n Faz. São Fran."="MF4",
                                "Delfim Moreira \n Faz. Bart."="MF5",
                                "Baependi"="MF6"
)

#colnames(bio.tem.trop.pro) <- c("MF1","MF5",
#                                "MF4","MF6",
#                                "MF3","MF2")


bio.temp.trop= as.matrix (bio.tem.trop.pro)
ind.temp.trop= as.matrix (ind.tem.trop.pro)

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
                                "Delfim Moreira \n Faz. São Fran."="MF4",
                                "Delfim Moreira \n Faz. Bart."="MF5",
                                "Baependi"="MF6"
)

bio.tem.trop.ab= as.matrix (bio.tem.trop.ab)

##########biomas_temp__xtrop#########



jpeg(filename = "biomas_temp__xtrop.jpg", width = 700, height = 900, # fun��o salva gr�ficos em .jpg
     units = "px", quality = 75,
     bg = "white")

par(mfrow=c(1,2),mar=c(3,6,2,2), cex.axis=1, cex.lab=1.5, mgp=c(3.5,1.6,0),
    family="serif",las=1, tcl=0.3, bg="white")

color <- colorRampPalette(c("sandybrown","lightgreen"))

barplot (bio.temp.trop, col=color (4),
         ylim=c(0,120), ylab="Biomass %",
         cex.lab= 2, cex.names = 2 )

barplot (ind.temp.trop, col=color (4),
         ylim=c(0,120), ylab="Individual %",
         cex.lab= 2, cex.names = 2 )


legend("topright" #fun��o adiciona um texto ao gr�fico,
       #arg 1� define a localiza��o, usa-se a fun��o locator para
       #adicionar de uma forma interativa
       ,legend = c(expression(italic ("Araucaria")),
                    expression(italic ("Podocarpus")),
                    "Temp.Ang","Trop.Ang") #texto a ser escrito
       ,col=color (4)
       ,cex=1.5		#tamanho da fonte
       , pch=c(15,15)
       ,bty = "n")


dev.off()

#############biomas_temp__xtrop_ab##############

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
           ), cex.lab= 2, cex.names = 2
         )

legend("topright" #fun��o adiciona um texto ao gr�fico,
       #arg 1� define a localiza��o, usa-se a fun��o locator para
       #adicionar de uma forma interativa
       ,legend = c(expression(italic ("Araucaria")),
                   expression(italic ("Podocarpus")),
                   "Temp.Ang","Trop.Ang") #texto a ser escrito
       ,col=color (4)
       ,cex=1.5		#tamanho da fonte
       , pch=c(15,15)
       ,bty = "n")


dev.off()

###############################################################
############ Biomass Distri DBH #####

jpeg(filename = "biomass_distri_DHB2.jpg",
     width = 850, height = 500, # fun��o salva gr�ficos em .jpg
     units = "px", quality = 75,
     bg = "white")

par(mfrow=c(2,3),mar=c(5,6,3,2), cex.axis=1.3, cex.lab=1.5, mgp=c(3,1.3,0.3),
    family="serif",las=1, tcl=0.3, bty = "n", xaxt="n")

color <- colorRampPalette(c("lightgreen","sandybrown"))

barplot (biomass_all_bc_sep_by_DHB_distribution$Biomass_ab/1000,
         col=color (2),
         main="Barra do Chapéu (MF1)",
         ylab = "Biomass (Mg)",
         ylim = c(0, 130))

mtext( #fun��o plota textos nas �reas ao redor do gr�fico
  c("0-10", "10-30","30-50", ">50"), #primeiro argumento refere oa texto plotado
  side= 1, #argumento localiza no grafico "1" abaixo
  cex=1,line=0.9,
  at=c(1.4,3.5,6,8.5))


barplot (biomass_all_it_sep_by_DHB_distribution$Biomass_ab/1000,
         col=color (2),
         main="Itaberá (MF2)",
         ylab = "",
         ylim=c(0, 100))

mtext( #fun��o plota textos nas �reas ao redor do gr�fico
  c("0-10", "10-30","30-50", ">50"), #primeiro argumento refere oa texto plotado
  side= 1, #argumento localiza no grafico "1" abaixo
  cex=1,line=0.9,
  at=c(1.4,3.5,6,8.5))


barplot(biomass_all_cj_sep_by_DHB_distribution$Biomass_ab/1000,
        las = 2, # Rotate x-axis labels for readability
        col = color (2),
        main = "Campos do Jordão (MF3)",
        ylab = " ",
        ylim = c(0,140))


mtext( #fun��o plota textos nas �reas ao redor do gr�fico
  c("0-10", "10-30","30-50", ">50"), #primeiro argumento refere oa texto plotado
  side= 1, #argumento localiza no grafico "1" abaixo
  cex=1,line=0.9,
  at=c(1.4,3.5,6,8.5))



barplot(biomass_all_Fsf_sep_by_DHB_distribution$Biomass_ab/1000,
        las = 2, # Rotate x-axis labels for readability
        col = color (2),
        main = "Delfim Moreira \nSão Francisco Farm (MF4)",
        ylab = "Biomass (Mg)",
        xlab = "DBH Class (cm)",
        ylim=c(0, 70))


mtext( #fun��o plota textos nas �reas ao redor do gr�fico
  c("0-10", "10-30","30-50", ">50"), #primeiro argumento refere oa texto plotado
  side= 1, #argumento localiza no grafico "1" abaixo
  cex=1,line=0.9,
  at=c(1.4,3.5,6,8.5))

barplot(biomass_all_Fbar_sep_by_DHB_distribution$Biomass_ab/1000,
        las = 2, # Rotate x-axis labels for readability
        col = color (2),
        main = "Delfim Moreira \nBartira Farm (MF5)",
        ylab = "",
        xlab = "DBH Class (cm)",
        ylim = c(0,100))


mtext( #fun��o plota textos nas �reas ao redor do gr�fico
  c("0-10", "10-30","30-50", ">50"), #primeiro argumento refere oa texto plotado
  side= 1, #argumento localiza no grafico "1" abaixo
  cex=1,line=0.9,
  at=c(1.4,3.5,6,8.5))


barplot(biomass_all_bp_sep_by_DHB_distribution$Biomass_ab/1000,
        las = 2, # Rotate x-axis labels for readability
        col = color (2),
        main = "Baependi (MF6)",
        ylab = "",
        xlab = "DBH Class (cm)",
        ylim=c(0,50))


mtext( #fun��o plota textos nas �reas ao redor do gr�fico
  c("0-10", "10-30","30-50", ">50"), #primeiro argumento refere oa texto plotado
  side= 1, #argumento localiza no grafico "1" abaixo
  cex=1,line=0.9,
  at=c(1.4,3.5,6,8.5))

legend("topleft" #fun��o adiciona um texto ao gr�fico,
       #arg 1� define a localiza��o, usa-se a fun��o locator para
       #adicionar de uma forma interativa
       ,c("Tropical","Temperate") #texto a ser escrito
       ,col=color (2)
       ,cex=1.3		#tamanho da fonte
       , pch=c(15,15)
       ,bty = "n") #tipo da fonte



dev.off()

############Biomass Distri DBH percentage #####

jpeg(filename = "biomass_distri_DHB_percente2.jpg",
     width = 850, height = 500, # fun��o salva gr�ficos em .jpg
     units = "px", quality = 75,
     bg = "white")

par(mfrow=c(2,3),mar=c(5,6,3,2), cex.axis=1.3, cex.lab=1.5, mgp=c(3,1.3,0.3),
    family="serif",las=1, tcl=0.3, bty = "n", xaxt="n")

color <- colorRampPalette(c("lightgreen","sandybrown"))

barplot (biomass_all_bc_sep_by_DHB_distribution$Biomass_percentage,
         col=color (2),
         main="Barra do Chapéu (MF1)",
         ylab = "Biomass (%)",
         ylim = c(0,50))

mtext( #fun��o plota textos nas �reas ao redor do gr�fico
  c("0-10", "10-30","30-50", ">50"), #primeiro argumento refere oa texto plotado
  side= 1, #argumento localiza no grafico "1" abaixo
  cex=1,line=0.9,
  at=c(1.4,3.5,6,8.5))



barplot (biomass_all_it_sep_by_DHB_distribution$Biomass_percentage,
         col=color (2),
         main="Itaberá (MF2)",
         ylab = "",
         ylim = c(0,40))

mtext( #fun��o plota textos nas �reas ao redor do gr�fico
  c("0-10", "10-30","30-50", ">50"), #primeiro argumento refere oa texto plotado
  side= 1, #argumento localiza no grafico "1" abaixo
  cex=1,line=0.9,
  at=c(1.4,3.5,6,8.5))



barplot(biomass_all_cj_sep_by_DHB_distribution$Biomass_percentage,
        las = 2, # Rotate x-axis labels for readability
        col = color (2),
        main = "Campos do Jordão (MF3)",
        ylab = " ",
        ylim = c(0,50))


mtext( #fun��o plota textos nas �reas ao redor do gr�fico
  c("0-10", "10-30","30-50", ">50"), #primeiro argumento refere oa texto plotado
  side= 1, #argumento localiza no grafico "1" abaixo
  cex=1,line=0.9,
  at=c(1.4,3.5,6,8.5))


barplot(biomass_all_Fsf_sep_by_DHB_distribution$Biomass_percentage,
        las = 2, # Rotate x-axis labels for readability
        col = color (2),
        main = "Delfim Moreira \nSão Francisco Farm (MF4)",
        ylab = "Biomass (%)",
        xlab = "DBH Class (cm)",
        ylim = c(0,40))


mtext( #fun��o plota textos nas �reas ao redor do gr�fico
  c("0-10", "10-30","30-50", ">50"), #primeiro argumento refere oa texto plotado
  side= 1, #argumento localiza no grafico "1" abaixo
  cex=1,line=0.9,
  at=c(1.4,3.5,6,8.5))


barplot(biomass_all_Fbar_sep_by_DHB_distribution$Biomass_percentage,
        las = 2, # Rotate x-axis labels for readability
        col = color (2),
        main = "Delfim Moreira \nBartira Farm (MF5)",
        ylab = "",
        xlab = "DBH Class (cm)",
        ylim = c(0,60))


mtext( #fun��o plota textos nas �reas ao redor do gr�fico
  c("0-10", "10-30","30-50", ">50"), #primeiro argumento refere oa texto plotado
  side= 1, #argumento localiza no grafico "1" abaixo
  cex=1,line=0.9,
  at=c(1.4,3.5,6,8.5))


barplot(biomass_all_bp_sep_by_DHB_distribution$Biomass_percentage,
        las = 2, # Rotate x-axis labels for readability
        col = color (2),
        main = "Baependi (MF6)",
        ylab = "",
        xlab = "DBH Class (cm)",
        ylim = c(0,40))


mtext( #fun��o plota textos nas �reas ao redor do gr�fico
  c("0-10", "10-30","30-50", ">50"), #primeiro argumento refere oa texto plotado
  side= 1, #argumento localiza no grafico "1" abaixo
  cex=1,line=0.9,
  at=c(1.4,3.5,6,8.5))

legend("topleft" #fun��o adiciona um texto ao gr�fico,
       #arg 1� define a localiza��o, usa-se a fun��o locator para
       #adicionar de uma forma interativa
       ,c("Tropical","Temperate") #texto a ser escrito
       ,col=color (2)
       ,cex=1.3		#tamanho da fonte
       , pch=c(15,15)
       ,bty = "n") #tipo da fonte



dev.off()

################ Indivuidual distribution DBH #########

jpeg(filename = "Ind_distri_DHB_percente2.jpg",
     width = 850, height = 500, # fun��o salva gr�ficos em .jpg
     units = "px", quality = 75,
     bg = "white")

par(mfrow=c(2,3),mar=c(5,6,3,2), cex.axis=1.3, cex.lab=1.5, mgp=c(3,1.3,0.3),
    family="serif",las=1, tcl=0.3, bty = "n", xaxt="n")

color <- colorRampPalette(c("lightgreen","sandybrown"))

barplot (ind_all_bc_sep_by_DHB_distribution$Ind_percentage,
         col=color (2),
         main="Barra do Chapéu (MF1)",
         ylab = "Individuals (%)", )

mtext( #fun��o plota textos nas �reas ao redor do gr�fico
  c("0-10", "10-30","30-50", ">50"), #primeiro argumento refere oa texto plotado
  side= 1, #argumento localiza no grafico "1" abaixo
  cex=1,line=0.9,
  at=c(1.4,3.5,6,8.5))



barplot (ind_all_it_sep_by_DHB_distribution$Ind_percentage,
         col=color (2),
         main="Itaberá (MF2)",
         ylab = "",
         ylim = c(0,60))

mtext( #fun��o plota textos nas �reas ao redor do gr�fico
  c("0-10", "10-30","30-50", ">50"), #primeiro argumento refere oa texto plotado
  side= 1, #argumento localiza no grafico "1" abaixo
  cex=1,line=0.9,
  at=c(1.4,3.5,6,8.5))


barplot(ind_all_cj_sep_by_DHB_distribution$Ind_percentage,
        las = 2, # Rotate x-axis labels for readability
        col = color (2),
        main = "Campos do Jordão (MF3)",
        ylab = " ",
        ylim = c(0,30))


mtext( #fun��o plota textos nas �reas ao redor do gr�fico
  c("0-10", "10-30","30-50", ">50"), #primeiro argumento refere oa texto plotado
  side= 1, #argumento localiza no grafico "1" abaixo
  cex=1,line=0.9,
  at=c(1.4,3.5,6,8.5))


barplot(ind_all_Fsf_sep_by_DHB_distribution$Ind_percentage,
        las = 2, # Rotate x-axis labels for readability
        col = color (2),
        main = "Delfim Moreira \nSão Francisco Farm (MF4)",
        ylab = "Individuals (%)",
        xlab = "DBH Class (cm)",
        ylim = c(0,50))


mtext( #fun��o plota textos nas �reas ao redor do gr�fico
  c("0-10", "10-30","30-50", ">50"), #primeiro argumento refere oa texto plotado
  side= 1, #argumento localiza no grafico "1" abaixo
  cex=1,line=0.9,
  at=c(1.4,3.5,6,8.5))


barplot(ind_all_Fbar_sep_by_DHB_distribution$Ind_percentage,
        las = 2, # Rotate x-axis labels for readability
        col = color (2),
        main = "Delfim Moreira \nBartira Farm (MF5)",
        ylab = "",
        xlab = "DBH Class (cm)",
        ylim = c(0,60))


mtext( #fun��o plota textos nas �reas ao redor do gr�fico
  c("0-10", "10-30","30-50", ">50"), #primeiro argumento refere oa texto plotado
  side= 1, #argumento localiza no grafico "1" abaixo
  cex=1,line=0.9,
  at=c(1.4,3.5,6,8.5))


barplot(ind_all_bp_sep_by_DHB_distribution$Ind_percentage,
        las = 2, # Rotate x-axis labels for readability
        col = color (2),
        main = "Baependi (MF6)",
        ylab = "",
        xlab = "DBH Class (cm)",
        ylim = c(0,40))


mtext( #fun��o plota textos nas �reas ao redor do gr�fico
  c("0-10", "10-30","30-50", ">50"), #primeiro argumento refere oa texto plotado
  side= 1, #argumento localiza no grafico "1" abaixo
  cex=1,line=0.9,
  at=c(1.4,3.5,6,8.5))

legend("topleft" #fun��o adiciona um texto ao gr�fico,
       #arg 1� define a localiza��o, usa-se a fun��o locator para
       #adicionar de uma forma interativa
       ,c("Tropical","Temperate") #texto a ser escrito
       ,col=color (2)
       ,cex=1.3		#tamanho da fonte
       , pch=c(15,15)
       ,bty = "n") #tipo da fonte



dev.off()

####### plot biomass species######
# Final dataset for plotting
MF_top5 <- table_final_to_anova %>%
  group_by(site) %>%
  slice_max(biomass_total, n = 5, with_ties = FALSE) %>%
  ungroup()
MF1_top_5 <- MF_top5 [MF_top5$site == "MF1",]
MF2_top_5 <- MF_top5 [MF_top5$site == "MF2",]
MF3_top_5 <- MF_top5 [MF_top5$site == "MF3",]
MF4_top_5 <- MF_top5 [MF_top5$site == "MF4",]
MF5_top_5 <- MF_top5 [MF_top5$site == "MF5",]
MF6_top_5 <- MF_top5 [MF_top5$site == "MF6",]


jpeg(filename = "top5_species.jpg",
     width = 850, height = 500, # fun��o salva gr�ficos em .jpg
     units = "px", quality = 75,
     bg = "white")

par(mfrow=c(2,3),mar=c(5,6,3,2), cex.axis=1.3, cex.lab=2, mgp=c(3,1.3,0.3),
    family="serif",las=1, tcl=0.3, bty = "n", xaxt="n")


barplot(MF1_top_5$biomass_total,
        ylim = c (0,range (MF1_top_5$biomass_total) [2]+10),
        col = c ("lightgreen",
                 "sandybrown",
                 rep ("lightgreen",3)),
        las = 2,                # Nomes na vertical
        ylab = "Biomass Mg",
        main= "MF1")

labels <- c("M. ela", "A. ang","O. ela", "O. pub", "C. fis")

text(x = c(0.7,2,3.1,4.3,5.5),
     y = par("usr")[3] - 3,   # below the axis
     labels = labels,
     srt = 45,                # rotation angle
     xpd = TRUE,              # allow text outside plot
     adj = 1,                 # text alignment
     cex = 2)


barplot(MF2_top_5$biomass_total,
        ylim = c (0,range (MF2_top_5$biomass_total) [2]+10),
        col = c (rep ("lightgreen",3),
                 "sandybrown",
                 "lightgreen"),
        las = 2,                # Nomes na vertical
        cex.names = 0.8,        # Tamanho dos nomes
        main= "MF2")

labels <- c("P. rig", "M. ela","L. div", "A. ang", "M. nyc")

text(x = c(0.7,2,3.1,4.3,5.5),
     y = par("usr")[3] - 3,   # below the axis
     labels = labels,
     srt = 45,                # rotation angle
     xpd = TRUE,              # allow text outside plot
     adj = 1,                 # text alignment
     cex = 2)


barplot(MF3_top_5$biomass_total,
        ylim = c (0,range (MF3_top_5$biomass_total) [2]+10),
        col = c (rep ("sandybrown",3),
                 rep ("lightgreen",2)),
        las = 2,                # Nomes na vertical
        cex.names = 0.8,        # Tamanho dos nomes
        main= "MF3")

labels <- c("P. lam", "A. ang","M. mie", "M. umb", "C. sel")

text(x = c(0.7,2,3.1,4.3,5.5),
     y = par("usr")[3] - 3,   # below the axis
     labels = labels,
     srt = 45,                # rotation angle
     xpd = TRUE,              # allow text outside plot
     adj = 1,                 # text alignment
     cex = 2)


barplot(MF4_top_5$biomass_total,
        ylim = c (0,range (MF4_top_5$biomass_total) [2]+10),
        col = c (rep ("sandybrown",4),
                 rep ("lightgreen",1)),
        las = 2,                # Nomes na vertical
        cex.names = 0.8,        # Tamanho dos nomes
        main= "MF4",               # Nomes na vertical
        ylab = "Biomass Mg")

labels <- c("A. ang", "P. lam","D. bra", "M. ruf", "C. sca")

text(x = c(0.7,2,3.1,4.3,5.5),
     y = par("usr")[3] - 3,   # below the axis
     labels = labels,
     srt = 45,                # rotation angle
     xpd = TRUE,              # allow text outside plot
     adj = 1,                 # text alignment
     cex = 2)

barplot(MF5_top_5$biomass_total,
        ylim = c (0,range (MF5_top_5$biomass_total) [2]+10),
        col = c ("lightgreen",
                 "sandybrown",
                 rep ("lightgreen",3)),
        las = 2,                # Nomes na vertical
        cex.names = 0.8,        # Tamanho dos nomes
        main= "MF5")

labels <- c("M. ela", "A. ang","S. rev", "L. pac", "S. ins")

text(x = c(0.7,2,3.1,4.3,5.5),
     y = par("usr")[3] - 3,   # below the axis
     labels = labels,
     srt = 45,                # rotation angle
     xpd = TRUE,              # allow text outside plot
     adj = 1,                 # text alignment
     cex = 2)

barplot(MF6_top_5$biomass_total,
        ylim = c (0,range (MF6_top_5$biomass_total) [2]+11),
        col = c (rep ("sandybrown",3),
                 rep ("lightgreen",2)),
        las = 2,                # Nomes na vertical
        cex.names = 0.8,        # Tamanho dos nomes
        main= "MF6")

labels <- c("P. lam", "A. ang","M. bra", "M. umb", "P. reg")

text(x = c(0.7,2,3.1,4.3,5.5),
     y = par("usr")[3] - 3,   # below the axis
     labels = labels,
     srt = 45,                # rotation angle
     xpd = TRUE,              # allow text outside plot
     adj = 1,                 # text alignment
     cex = 2)

legend("topright" #fun��o adiciona um texto ao gr�fico,
       #arg 1� define a localiza��o, usa-se a fun��o locator para
       #adicionar de uma forma interativa
       ,c("Tropical","Temperate") #texto a ser escrito
       ,col= c ("lightgreen", "sandybrown")
       ,cex=1.5		#tamanho da fonte
       , pch=c(15,15)
       ,bty = "n") #tipo da fonte

dev.off()


