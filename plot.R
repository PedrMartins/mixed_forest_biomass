########################GRÁFICOS BIOMASSA##############################
###################rodar script biomass_calc antes#####################


############################################################
###Suplementar########

source ("import_processing_biomass_data.R")
source("Analyses_and_export_table.R")

###DAP class porcentagem ind########

jpeg(filename = "Class_DAP_ind2.jpg", width = 1050, height = 700, # function to save plots salva .jpg
     units = "px", quality = 75,
     bg = "white")

par(mfrow=c(2,3),mar=c(5,5,3,2), cex.axis=1.3, cex.lab=1.5, mgp=c(3,1.3,0.3),
    family="serif",las=1, tcl=0.3, bg= "white")
color= colorRampPalette(c("lightgreen","sandybrown"))

barplot (ind_all_bc_sep_by_DHB_filo$Ind_percentage,
         col=color (2), ylim=c(0,70),
         main="Barra do Chapéu (MF1)",
         ylab = "Individuals (%)")


mtext( #fun��o plota textos nas �reas ao redor do gr�fico
  c("0-10", "10-30","30-50", ">50"), #primeiro argumento refere oa texto plotado
  side= 1, #argumento localiza no grafico "1" abaixo
  cex=1,line=0.9,
  at=c(1.4,3.5,6,8.5))


barplot (ind_all_it_sep_by_DHB_filo$Ind_percentage,
         col=color (2), ylim=c(0,70),
         main="Itaberá (MF2)")

mtext( #fun��o plota textos nas �reas ao redor do gr�fico
  c("0-10", "10-30","30-50", ">50"), #primeiro argumento refere oa texto plotado
  side= 1, #argumento localiza no gr�fico "1" abaixo
  cex=1,line=0.9,
  at=c(1.4,3.5,6,8.5))


barplot (ind_all_cj_sep_by_DHB_filo$Ind_percentage,
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
       ,c("Angiosperms","Gymnosperms") #texto a ser escrito
       ,col=color (2)
       ,cex=1.3		#tamanho da fonte
       , pch=c(15,15)
       ,bty = "n") #tipo da fonte


barplot (ind_all_Fsf_sep_by_DHB_filo$Ind_percentage,
         col=color (2), ylim=c(0,70),
         main="Delfim Moreira \n Faz. São Fran. (MF4)",
         ylab = "Individuals (%)",
         xlab="DBH Class (cm)")

mtext( #fun��o plota textos nas �reas ao redor do gr�fico
  c("0-10", "10-30","30-50", ">50"), #primeiro argumento refere oa texto plotado
  side= 1, #argumento localiza no gr�fico "1" abaixo
  cex=1,line=0.9,
  at=c(1.4,3.5,6,8.5))

barplot (ind_all_Fbar_sep_by_DHB_filo$Ind_percentage,
         col=color (2), ylim=c(0,70),
         main="Delfim Moreira \n Faz. Bart. (MF5)",
         xlab="DBH Class (cm)")


mtext( #fun��o plota textos nas �reas ao redor do gr�fico
  c("0-10", "10-30","30-50", ">50"), #primeiro argumento refere oa texto plotado
  side= 1, #argumento localiza no gr�fico "1" abaixo
  cex=1,line=0.9,
  at=c(1.4,3.5,6,8.5))


barplot (ind_all_bp_sep_by_DHB_filo$Ind_percentage,
         col=color (2), ylim=c(0,70),
         main="Baependi (MF6)",
         xlab="DBH Class (cm)")

mtext( #fun��o plota textos nas �reas ao redor do gr�fico
  c("0-10", "10-30","30-50", ">50"), #primeiro argumento refere oa texto plotado
  side= 1, #argumento localiza no gr�fico "1" abaixo
  cex=1,line=0.9,
  at=c(1.4,3.5,6,8.5))

legend("topright" #fun��o adiciona um texto ao gr�fico,
       #arg 1� define a localiza��o, usa-se a fun��o locator para
       #adicionar de uma forma interativa
       ,c("Angiosperms","Gymnosperms") #texto a ser escrito
       ,col=color (2)
       ,cex=1.3		#tamanho da fonte
       , pch=c(15,15)
       ,bty = "n") #tipo da fonte

dev.off()


###########DAP Class porcentage BIO########


jpeg(filename = "Class_DAP_biomas2.jpg", width = 1050, height = 700, # fun��o salva gr�ficos em .jpg
     units = "px", quality = 95,
     bg = "white")

par(mfrow=c(2,3),mar=c(5,5,3,2), cex.axis=1.3, cex.lab=1.5, mgp=c(3,1.3,0.3),
    family="serif",las=1, tcl=0.3, bg="white")

color= colorRampPalette(c("lightgreen","sandybrown"))

barplot (biomass_all_bc_sep_by_DHB_filo$Biomass_percentage,
         col=color (2), ylim=c(0,100),
         main="Barra do Chapéu (MF1)",
         ylab = "Biomass (%)")

mtext( #fun��o plota textos nas �reas ao redor do gr�fico
  c("0-10", "10-30","30-50", ">50"), #primeiro argumento refere oa texto plotado
  side= 1, #argumento localiza no gr�fico "1" abaixo
  cex=1,line=0.9,
  at=c(1.4,3.5,6,8.5))



barplot (biomass_all_it_sep_by_DHB_filo$Biomass_percentage,
         col=color (2), ylim=c(0,100),
         main="Itaberá (MF2)")


mtext( #fun��o plota textos nas �reas ao redor do gr�fico
  c("0-10", "10-30","30-50", ">50"), #primeiro argumento refere oa texto plotado
  side= 1, #argumento localiza no grafico "1" abaixo
  cex=1,line=0.9,
  at=c(1.4,3.5,6,8.5))



barplot (biomass_all_cj_sep_by_DHB_filo$Biomass_percentage,
         col=color (2), ylim=c(0,100),
         main="Campos do Jordão (MF3)")


mtext( #fun��o plota textos nas �reas ao redor do gr�fico
  c("0-10", "10-30","30-50", ">50"), #primeiro argumento refere oa texto plotado
  side= 1, #argumento localiza no gr�fico "1" abaixo
  cex=1,line=0.9,
  at=c(1.4,3.5,6,8.5))

legend("topleft" #fun��o adiciona um texto ao gr�fico,
       #arg 1� define a localiza��o, usa-se a fun��o locator para
       #adicionar de uma forma interativa
       ,c("Angiosperms","Gymnosperms") #texto a ser escrito
       ,col=color (2)
       ,cex=1.3		#tamanho da fonte
       , pch=c(15,15)
       ,bty = "n") #tipo da fonte

barplot (biomass_all_Fsf_sep_by_DHB_filo$Biomass_percentage,
         col=color (2), ylim=c(0,100),
         main="Delfim Moreira \n Faz. São Fran. (MF4)",
         ylab = "Biomass (%)",
         xlab="DBH Class (cm)")

mtext( #fun��o plota textos nas �reas ao redor do gr�fico
  c("0-10", "10-30","30-50", ">50"), #primeiro argumento refere oa texto plotado
  side= 1, #argumento localiza no gr�fico "1" abaixo
  cex=1,line=0.9,
  at=c(1.4,3.5,6,8.5))

barplot (biomass_all_Fbar_sep_by_DHB_filo$Biomass_percentage,
         col=color (2), ylim=c(0,100),
         main="Delfim Moreira \n Faz. Bart. (MF5)",
         xlab="DBH Class (cm)")


mtext( #fun��o plota textos nas �reas ao redor do gr�fico
  c("0-10", "10-30","30-50", ">50"), #primeiro argumento refere oa texto plotado
  side= 1, #argumento localiza no gr�fico "1" abaixo
  cex=1,line=0.9,
  at=c(1.4,3.5,6,8.5))

barplot (biomass_all_bp_sep_by_DHB_filo$Biomass_percentage,
         col=color (2), ylim=c(0,100),
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
       ,c("Angiosperms","Gymnosperms") #texto a ser escrito
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
                                "Delfim Moreira \n Faz. São Fran."="MF4",
                                "Delfim Moreira \n Faz. Bart."="MF5",
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
       ,legend= c (expression (italic ("Araucaria")),
                   expression (italic ("Podocarpus"))) #texto a ser escrito
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
                                "Delfim Moreira \n Faz. São Fran."="MF4",
                                "Delfim Moreira \n Faz. Bart."="MF5",
                                "Baependi"="MF6"
                                )

#colnames(bio.tem.trop.pro) <- c("MF1","MF5",
#                                "MF4","MF6",
#                                "MF3","MF2")


bio.temp.trop= as.matrix (bio.tem.trop.pro)

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
       ,legend = c(expression(italic ("Araucaria")),
                    expression(italic ("Podocarpus")),
                    "Ang_Temp","Ang_Trop") #texto a ser escrito
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
       ,legend = c(expression(italic ("Araucaria")),
                   expression(italic ("Podocarpus")),
                   "Ang_Temp","Ang_Trop") #texto a ser escrito
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
                            "Delfim Moreira \n Faz. São Fran."="MF4",
                            "Delfim Moreira \n Faz. Bart."="MF5",
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

jpeg(filename = "Boxplot_densmad.jpg", width = 850, height = 500, # fun��o salva gr�ficos em .jpg
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
  c("Temperate","Tropical"), #primeiro argumento refere oa texto plotado
  side= 1, #argumento localiza no gr�fico "1" abaixo
  cex=1,line=0.9,
  at=c(1,2))

boxplot (DensM ~ Distri, data=bio.It,horizontal = FALSE, main="Itaberá (MF2)",
         col = color(2), ylab= "", xlab=" ")
mtext( #fun��o plota textos nas �reas ao redor do gr�fico
  c("Temperate","Tropical"), #primeiro argumento refere oa texto plotado
  side= 1, #argumento localiza no gr�fico "1" abaixo
  cex=1,line=0.9,
  at=c(1,2))


boxplot (DensM ~ Distri, data=bio.cj, horizontal = FALSE,
         main="Campos do Jordão (MF3)", xlab =" ", ylab = " ",
         col = color(2))


mtext( #fun��o plota textos nas �reas ao redor do gr�fico
  c("Temperate","Tropical"), #primeiro argumento refere oa texto plotado
  side= 1, #argumento localiza no gr�fico "1" abaixo
  cex=1,line=0.9,
  at=c(1,2))


boxplot (DensM ~ Distri, data=bio.Fsf,horizontal = FALSE,
         main="Delfim Moreira \n Faz. S. Fran. (MF4)"
         , xlab ="Phytogeographic distribution",
         col = color(2), ylab= expression (
           paste ("Wood density ", g.cm^-3)))


mtext( #fun��o plota textos nas �reas ao redor do gr�fico
  c("Temperate","Tropical"), #primeiro argumento refere oa texto plotado
  side= 1, #argumento localiza no gr�fico "1" abaixo
  cex=1,line=0.9,
  at=c(1,2))

boxplot (DensM ~ Distri, data=bio.Fbar,horizontal = FALSE,
         main="Delfim Moreira \n Faz. Bartira (MF5)",
         xlab ="Phytogeographic distribution",
         col = color(2), ylab= " ")



mtext( #fun��o plota textos nas �reas ao redor do gr�fico
  c("Temperate","Tropical"), #primeiro argumento refere oa texto plotado
  side= 1, #argumento localiza no gr�fico "1" abaixo
  cex=1,line=0.9,
  at=c(1,2))


boxplot (DensM ~ Distri, data=bio.bp,horizontal = FALSE,
         main="Baependi(MF6)",
         xlab ="Phytogeographic distribution", ylab=" ",
         col = color(2)
         )

mtext( #fun��o plota textos nas �reas ao redor do gr�fico
  c("Temperate","Tropical"), #primeiro argumento refere oa texto plotado
  side= 1, #argumento localiza no gr�fico "1" abaixo
  cex=1,line=0.9,
  at=c(1,2))


dev.off()
###################################################################################################
################vioplot biomassa################################################


jpeg(filename = "vioplot_biomass.jpg", width = 850, height = 500, # fun��o salva gr�ficos em .jpg
     units = "px", quality = 75,
     bg = "white")

par(mfrow=c(2,3),mar=c(5,7,3,2), cex.axis=1.3, cex.lab=1.5, mgp=c(3,1.3,0.3),
    family="serif",las=1, tcl=0.3, bty = "n", xaxt="n")


color <- colorRampPalette(c("sandybrown","lightgreen"))


vioplot (biom/1000  ~ Distri, data=bio.bc,horizontal = FALSE,
         main="Barra do Chapéu (MF1)",
         xlab =" ",
         col = color(2), ylab= "Biomass (Mg)", pch="*")

mtext( #fun��o plota textos nas �reas ao redor do gr�fico
  c("Temperate","Tropical"), #primeiro argumento refere oa texto plotado
  side= 1, #argumento localiza no gr�fico "1" abaixo
  cex=1,line=0.9,
  at=c(1,2))

vioplot (biom/1000 ~ Distri, data=bio.It,horizontal = FALSE, main="Itaberá (MF2)",
         xlab =" ",
         col = color(2), ylab= "", pch="*")


mtext( #fun��o plota textos nas �reas ao redor do gr�fico
  c("Temperate","Tropical"), #primeiro argumento refere oa texto plotado
  side= 1, #argumento localiza no gr�fico "1" abaixo
  cex=1,line=0.9,
  at=c(1,2))

vioplot (biom/1000  ~ Distri, data=bio.cj, horizontal = FALSE,
         main="Campos do Jordão (MF3)", xlab =" ", pch="*",col = color(2),
         ylab= " ")


mtext( #fun��o plota textos nas �reas ao redor do gr�fico
  c("Temperate","Tropical"), #primeiro argumento refere oa texto plotado
  side= 1, #argumento localiza no gr�fico "1" abaixo
  cex=1,line=0.9,
  at=c(1,2))


vioplot (biom/1000  ~ Distri, data=bio.Fsf,horizontal = FALSE,
         main="Delfim Moreira \n Faz. S. Fran. (MF4)"
         , xlab ="Phytogeographic distribution",
         col = color(2), ylab= "Biomass (Mg)", pch="*")

mtext( #fun��o plota textos nas �reas ao redor do gr�fico
  c("Temperate","Tropical"), #primeiro argumento refere oa texto plotado
  side= 1, #argumento localiza no gr�fico "1" abaixo
  cex=1,line=0.9,
  at=c(1,2))


vioplot (biom/1000  ~ Distri, data=bio.Fbar,horizontal = FALSE,
         main="Delfim Moreira \n Faz. Bartira (MF5)",
         xlab ="Phytogeographic distribution",
         col = color(2), ylab= "", pch="*")


mtext( #fun��o plota textos nas �reas ao redor do gr�fico
  c("Temperate","Tropical"), #primeiro argumento refere oa texto plotado
  side= 1, #argumento localiza no gr�fico "1" abaixo
  cex=1,line=0.9,
  at=c(1,2))


vioplot (biom/1000 ~ Distri, data=bio.bp,horizontal = FALSE,
         main="Baependi(MF6)",
         xlab ="Phytogeographic distribution",
         col = color(2), ylab= " ", pch="*")


mtext( #fun��o plota textos nas �reas ao redor do gr�fico
  c("Temperate","Tropical"), #primeiro argumento refere oa texto plotado
  side= 1, #argumento localiza no gr�fico "1" abaixo
  cex=1,line=0.9,
  at=c(1,2))


dev.off()

################boxplot biomassa################################################

jpeg(filename = "boxplot_biomass.jpg", width = 850, height = 500, # fun��o salva gr�ficos em .jpg
     units = "px", quality = 75,
     bg = "white")

par(mfrow=c(2,3),mar=c(5,7,3,2), cex.axis=1.3, cex.lab=1.5, mgp=c(3,1.3,0.3),
    family="serif",las=1, tcl=0.3, bty = "n", xaxt="n")


color <- colorRampPalette(c("sandybrown","lightgreen"))


boxplot (biom/1000  ~ Distri, data=bio.bc,horizontal = FALSE,
         main="Barra do Chapéu (MF1)",
         xlab =" ",
         col = color(2), ylab= " ",
         log="y", pch="*")

mtext( #fun��o plota textos nas �reas ao redor do gr�fico
  "Log Biomass (Mg)", #primeiro argumento refere oa texto plotado
  side= 2, #argumento localiza no gr�fico "1" abaixo
  cex=1,line=4, las = 3)

mtext( #fun��o plota textos nas �reas ao redor do gr�fico
  c("Temperate","Tropical"), #primeiro argumento refere oa texto plotado
  side= 1, #argumento localiza no gr�fico "1" abaixo
  cex=1,line=0.9,
  at=c(1,2))

boxplot (biom/1000 ~ Distri, data=bio.It,horizontal = FALSE, main="Itaberá (MF2)",
         xlab =" ",
         col = color(2), ylab= "",
         log ="y", pch="*")


mtext( #fun��o plota textos nas �reas ao redor do gr�fico
  c("Temperate","Tropical"), #primeiro argumento refere oa texto plotado
  side= 1, #argumento localiza no gr�fico "1" abaixo
  cex=1,line=0.9,
  at=c(1,2))

boxplot (biom/1000  ~ Distri, data=bio.cj, horizontal = FALSE,
         main="Campos do Jordão (MF3)", xlab =" ",
         col = color(2), ylab= " ",
         log="y", pch="*")




mtext( #fun��o plota textos nas �reas ao redor do gr�fico
  c("Temperate","Tropical"), #primeiro argumento refere oa texto plotado
  side= 1, #argumento localiza no gr�fico "1" abaixo
  cex=1,line=0.9,
  at=c(1,2))


boxplot (biom/1000  ~ Distri, data=bio.Fsf,horizontal = FALSE,
         main="Delfim Moreira \n Faz. S. Fran. (MF4)"
         , xlab ="Phytogeographic distribution",
         col = color(2), ylab= " ",
         log ="y", pch="*")
mtext( #fun��o plota textos nas �reas ao redor do gr�fico
  "Log Biomass (Mg)", #primeiro argumento refere oa texto plotado
  side= 2, #argumento localiza no gr�fico "1" abaixo
  cex=1,line=4, las = 3)

mtext( #fun��o plota textos nas �reas ao redor do gr�fico
  c("Temperate","Tropical"), #primeiro argumento refere oa texto plotado
  side= 1, #argumento localiza no gr�fico "1" abaixo
  cex=1,line=0.9,
  at=c(1,2))


boxplot (biom/1000  ~ Distri, data=bio.Fbar,horizontal = FALSE,
         main="Delfim Moreira \n Faz. Bartira (MF5)",
         xlab ="Phytogeographic distribution",
         col = color(2), ylab= "",
         log="y", pch="*")


mtext( #fun��o plota textos nas �reas ao redor do gr�fico
  c("Temperate","Tropical"), #primeiro argumento refere oa texto plotado
  side= 1, #argumento localiza no gr�fico "1" abaixo
  cex=1,line=0.9,
  at=c(1,2))


boxplot (biom/1000 ~ Distri, data=bio.bp,horizontal = FALSE,
         main="Baependi(MF6)",
         xlab ="Phytogeographic distribution",
         col = color(2), ylab= " ",
         log="y", pch="*")


mtext( #fun��o plota textos nas �reas ao redor do gr�fico
  c("Temperate","Tropical"), #primeiro argumento refere oa texto plotado
  side= 1, #argumento localiza no gr�fico "1" abaixo
  cex=1,line=0.9,
  at=c(1,2))


dev.off()


################################################################
################vioplot biomassa filo###########################


jpeg(filename = "vioplot_biomass_filo.jpg", width = 850, height = 500, # fun��o salva gr�ficos em .jpg
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
  c("Eud","Gym","Mag", "Palm"), #primeiro argumento refere oa texto plotado
  side= 1, #argumento localiza no gr�fico "1" abaixo
  cex=1,line=0.9,
  at=c(1,2,3,4))

vioplot (biom/1000 ~ Filo, data=bio.It,horizontal = FALSE, main="Itaberá (MF2)",
         xlab =" ",
         col = color(4), ylab= "")

mtext( #fun��o plota textos nas �reas ao redor do gr�fico
  c("Eud","Gym","Mag", "Palm"), #primeiro argumento refere oa texto plotado
  side= 1, #argumento localiza no gr�fico "1" abaixo
  cex=1,line=0.9,
  at=c(1,2,3,4))

vioplot (biom/1000  ~ Filo, data=bio.cj, horizontal = FALSE,
         main="Campos do Jordão (MF3)", xlab =" ",
         col = color(3), ylab= " ")


mtext( #fun��o plota textos nas �reas ao redor do gr�fico
  c("Eud","Gym","Mag"), #primeiro argumento refere oa texto plotado
  side= 1, #argumento localiza no gr�fico "1" abaixo
  cex=1,line=0.9,
  at=c(1,2,3))

vioplot (biom/1000  ~ Filo, data=bio.Fsf,horizontal = FALSE,
         main="Delfim Moreira \n Faz. S. Fran. (MF4)"
         , xlab ="Phytogeographic distribution",
         col = color(3), ylab= "Biomass (Mg)")


mtext( #fun��o plota textos nas �reas ao redor do gr�fico
  c("Eud","Gym","Mag"), #primeiro argumento refere oa texto plotado
  side= 1, #argumento localiza no gr�fico "1" abaixo
  cex=1,line=0.9,
  at=c(1,2,3))


vioplot (biom/1000  ~ Filo, data=bio.Fbar,horizontal = FALSE,
         main="Delfim Moreira \n Faz. Bartira (MF5)",
         xlab ="Phytogeographic distribution", ylab="",
         col = color(3))


mtext( #fun��o plota textos nas �reas ao redor do gr�fico
  c("Eud","Gym","Mag"), #primeiro argumento refere oa texto plotado
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
  c("Eud","Gym","Mag"), #primeiro argumento refere oa texto plotado
  side= 1, #argumento localiza no gr�fico "1" abaixo
  cex=1,line=0.9,
  at=c(1,2,3))


dev.off()

################################################################

jpeg(filename = "boxplot_biomass_filo.jpg", width = 850, height = 500, # fun��o salva gr�ficos em .jpg
     units = "px", quality = 75,
     bg = "white")

par(mfrow=c(2,3),mar=c(5,6,3,2), cex.axis=1.3, cex.lab=1.5, mgp=c(3,1.3,0.3),
    family="serif",las=1, tcl=0.3, bty = "n", xaxt="n")


color <- colorRampPalette(c("sandybrown","lightgreen"))


boxplot (biom/1000  ~ Filo, data=bio.bc,horizontal = FALSE,
         main="Barra do Chapéu (MF1)",
         xlab =" ",
         col = color(4), ylab= "",
         log ="y", pch ="*")
mtext( #fun��o plota textos nas �reas ao redor do gr�fico
  "Log Biomass (Mg)", #primeiro argumento refere oa texto plotado
  side= 2, #argumento localiza no gr�fico "1" abaixo
  cex=1,line=4, las=3)

mtext( #fun��o plota textos nas �reas ao redor do gr�fico
  c("Eud","Gym","Mag", "Palm"), #primeiro argumento refere oa texto plotado
  side= 1, #argumento localiza no gr�fico "1" abaixo
  cex=1,line=0.9,
  at=c(1,2,3,4))

boxplot (biom/1000 ~ Filo, data=bio.It,
         horizontal = FALSE, main="Itaberá (MF2)",
         xlab =" ",
         col = color(4), ylab= "",
         log ="y", pch ="*")

mtext( #fun��o plota textos nas �reas ao redor do gr�fico
  c("Eud","Gym","Mag", "Palm"), #primeiro argumento refere oa texto plotado
  side= 1, #argumento localiza no gr�fico "1" abaixo
  cex=1,line=0.9,
  at=c(1,2,3,4))

boxplot (biom/1000  ~ Filo, data=bio.cj, horizontal = FALSE,
         main="Campos do Jordão (MF3)", xlab =" ",
         col = color(3), ylab= " ",
         log ="y", pch ="*")


mtext( #fun��o plota textos nas �reas ao redor do gr�fico
  c("Eud","Gym","Mag"), #primeiro argumento refere oa texto plotado
  side= 1, #argumento localiza no gr�fico "1" abaixo
  cex=1,line=0.9,
  at=c(1,2,3))



boxplot (biom/1000  ~ Filo, data=bio.Fsf,horizontal = FALSE,
         main="Delfim Moreira \n Faz. S. Fran. (MF4)"
         , xlab ="Phylogenetic Groups",
         col = color(3), ylab= " ",
         log ="y", pch ="*")

mtext( #fun��o plota textos nas �reas ao redor do gr�fico
  c("Log Biomass (Mg)"), #primeiro argumento refere oa texto plotado
  side= 2, #argumento localiza no gr�fico "1" abaixo
  cex=1,line=4, las=3)

mtext( #fun��o plota textos nas �reas ao redor do gr�fico
  c("Eud","Gym","Mag"), #primeiro argumento refere oa texto plotado
  side= 1, #argumento localiza no gr�fico "1" abaixo
  cex=1,line=0.9,
  at=c(1,2,3))

boxplot (biom/1000  ~ Filo, data=bio.Fbar,horizontal = FALSE,
         main="Delfim Moreira \n Faz. Bartira (MF5)",
         xlab ="Phylogenetic Groups",
         col = color(3), ylab= " ",
         log ="y", pch ="*")



mtext( #fun��o plota textos nas �reas ao redor do gr�fico
  c("Eud","Gym","Mag"), #primeiro argumento refere oa texto plotado
  side= 1, #argumento localiza no gr�fico "1" abaixo
  cex=1,line=0.9,
  at=c(1,2,3))

boxplot (biom/1000 ~ Filo, data=bio.bp,horizontal = FALSE,
         main="Baependi(MF6)",
         xlab ="Phylogenetic Groups",
         col = color(3), ylab= " ",
         log ="y", pch ="*")

#axis (2, at = seq (0,3,by=1))
#yaxt = "n",

mtext( #fun��o plota textos nas �reas ao redor do gr�fico
  c("Eud","Gym","Mag"), #primeiro argumento refere oa texto plotado
  side= 1, #argumento localiza no gr�fico "1" abaixo
  cex=1,line=0.9,
  at=c(1,2,3))


dev.off()
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

legend("topleft" #fun��o adiciona um texto ao gr�fico,
       #arg 1� define a localiza��o, usa-se a fun��o locator para
       #adicionar de uma forma interativa
       ,c("Tropical","Temperate") #texto a ser escrito
       ,col=color (2)
       ,cex=1.3		#tamanho da fonte
       , pch=c(15,15)
       ,bty = "n") #tipo da fonte



barplot(biomass_all_Fsf_sep_by_DHB_distribution$Biomass_ab/1000,
        las = 2, # Rotate x-axis labels for readability
        col = color (2),
        main = "Delfim Moreira \nFaz. São Fran. (MF4)",
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
        main = "Delfim Moreira \nFaz. São Fran. (MF5)",
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

legend("topleft" #fun��o adiciona um texto ao gr�fico,
       #arg 1� define a localiza��o, usa-se a fun��o locator para
       #adicionar de uma forma interativa
       ,c("Tropical","Temperate") #texto a ser escrito
       ,col=color (2)
       ,cex=1.3		#tamanho da fonte
       , pch=c(15,15)
       ,bty = "n") #tipo da fonte



barplot(biomass_all_Fsf_sep_by_DHB_distribution$Biomass_percentage,
        las = 2, # Rotate x-axis labels for readability
        col = color (2),
        main = "Delfim Moreira \nFaz. São Fran. (MF4)",
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
        main = "Delfim Moreira \nFaz. São Fran. (MF5)",
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

legend("topright" #fun��o adiciona um texto ao gr�fico,
       #arg 1� define a localiza��o, usa-se a fun��o locator para
       #adicionar de uma forma interativa
       ,c("Tropical","Temperate") #texto a ser escrito
       ,col=color (2)
       ,cex=1.3		#tamanho da fonte
       , pch=c(15,15)
       ,bty = "n") #tipo da fonte



barplot(ind_all_Fsf_sep_by_DHB_distribution$Ind_percentage,
        las = 2, # Rotate x-axis labels for readability
        col = color (2),
        main = "Delfim Moreira \nFaz. São Fran. (MF4)",
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
        main = "Delfim Moreira \nFaz. São Fran. (MF5)",
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
plot_df <- rbind(top5[, c("Species", "biomass_total")], other)

# Colors
color <- colorRampPalette(c("sandybrown", "lightgreen"))
bar_colors <- color(nrow(plot_df))

# Bar plot
bp <- barplot(
  plot_df$biomass_total,
  names.arg = plot_df$Species,
  las = 2,
  col = bar_colors,
  main = "Top 5 Species Biomass and Others",
  ylab = "Biomass Total",
  cex.names = 0.8
)

# Add labels on top of bars (only for top 5)
#text(bp[1:5], plot_df$biomass_total[1:5] + 500,  # position a bit above
#     labels = plot_df$Species[1:5], cex = 0.7, pos = 3
