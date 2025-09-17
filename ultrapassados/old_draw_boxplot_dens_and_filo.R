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



##############filo processing data###################


bio.gim.cj <- bio.cj [bio.cj$Filo == "Gim",]
bio.eud.cj <- bio.cj [bio.cj$Filo == "Eud",]
bio.mag.cj <- bio.cj [bio.cj$Filo == "Mag",]
gim.cj=sum(bio.gim.cj$biom)
eud.cj=sum(bio.eud.cj$biom)
mag.cj=sum(bio.mag.cj$biom)
por.b.cj= (c(gim.cj,eud.cj,mag.cj)/
             sum(gim.cj,eud.cj,mag.cj)*100)

bio.gim.bp <- bio.bp[bio.bp$Filo == "Gim",]
bio.eud.bp<- bio.bp[bio.bp$Filo == "Eud",]
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

##############filo draw###################

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


