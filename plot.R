########################GRÁFICOS BIOMASSA##############################
###################rodar script biomass_calc antes#####################


############################################################
###Suplementar########

source ("import_processing_biomass_data.R")
source("processing_to_table.R")


########################################################################
##########biomas_temp__xtrop#########



jpeg(filename = "biomas_temp__xtrop.jpg", width = 1000,
     height = 800, # fun��o salva gr�ficos em .jpg
     units = "px", quality = 75,
     bg = "white")


par(mfrow=c(2,2),mar=c(4,6,3,2),
     mgp=c(3.5,1.6,0),
    family="serif",las=1, tcl=0.3,
    bg="white")

color <- colorRampPalette(c("sandybrown","lightgreen"))

barplot (bio.temp.trop, col=color (2),
         ylim=c(0,100), ylab="Biomass %",
         cex.lab= 2, cex.names = 2 ,
         xaxt="n", cex =1.5)

mtext("a", side= 3, line =1, adj = 1, cex = 1.5)

barplot (ind.temp.trop, col=color (2),
         ylim=c(0,100), ylab="Individual %",
         cex.lab= 2, cex.names = 2, xaxt="n", cex =1.5 )
mtext("b", side= 3, line =1, adj = 1, cex = 1.5)

barplot (bio.temp/1000, col=color (2),
         ylim=c(0,350),
         ylab= expression("Biomass Mg·ha"^-1),
         cex.lab= 2, cex.names = 2, cex =1.5 )
mtext("c", side= 3, line =1, adj = 1, cex = 1.5)



par(mgp=c(3.5,0.75,0))

barplot (ind.temp, col=color (2),
         ylim=c(0,2500)
         , ylab=expression ("Individual.ha"^-1),
         cex.lab= 2, cex.names = 2, cex =1.5 )
mtext("d", side= 3, line =1, adj = 1, cex = 1.5)

legend("topright" #fun��o adiciona um texto ao gr�fico,
       #arg 1� define a localiza��o, usa-se a fun��o locator para
       #adicionar de uma forma interativa
       ,legend = c("Temperate","Tropical") #texto a ser escrito
       ,col= c("sandybrown","lightgreen")
       ,cex=1.5		#tamanho da fonte
       ,pch=c(15,15)
       ,bty = "n",
       horiz = TRUE)



dev.off()

############Biomass Distri DBH percentage #####

jpeg(filename = "biomass_distri_DHB_percente.jpg",
     width = 850, height = 500, # fun��o salva gr�ficos em .jpg
     units = "px", quality = 75,
     bg = "white")

par(mfrow=c(2,3),mar=c(5,6,3,2), cex.axis=1.5,
    cex.lab=2, mgp=c(3,1,0.3),
    family="serif",las=1, tcl=0.3, bty = "n", xaxt="n")


color <- colorRampPalette(c("lightgreen","sandybrown"))

barplot (biomass_all_bc_sep_by_DHB_distribution$Biomass_percentage,
         col=color (2),
         ylab = "Biomass (%)",
         ylim = c(0,50),
         main = "MF1")

mtext( #fun��o plota textos nas �reas ao redor do gr�fico
  c("0-10", "10-30","30-50", ">50"), #primeiro argumento refere oa texto plotado
  side= 1, #argumento localiza no grafico "1" abaixo
  cex=1.5,line=0.9,
  at=c(1.4,3.5,6,8.5))



barplot (biomass_all_it_sep_by_DHB_distribution$Biomass_percentage,
         col=color (2),
         ylab = "",
         ylim = c(0,40),
         main = "MF2")


mtext( #fun��o plota textos nas �reas ao redor do gr�fico
  c("0-10", "10-30","30-50", ">50"), #primeiro argumento refere oa texto plotado
  side= 1, #argumento localiza no grafico "1" abaixo
  cex=1.5,line=0.9,
  at=c(1.4,3.5,6,8.5))



barplot(biomass_all_cj_sep_by_DHB_distribution$Biomass_percentage,
        las = 2, # Rotate x-axis labels for readability
        col = color (2),
        ylab = "",
        ylim = c(0,50),
        main = "MF3")

mtext( #fun��o plota textos nas �reas ao redor do gr�fico
  c("0-10", "10-30","30-50", ">50"), #primeiro argumento refere oa texto plotado
  side= 1, #argumento localiza no grafico "1" abaixo
  cex=1.5,line=0.9,
  at=c(1.4,3.5,6,8.5))


barplot(biomass_all_Fsf_sep_by_DHB_distribution$Biomass_percentage,
        las = 2, # Rotate x-axis labels for readability
        col = color (2),
        ylab = "Biomass (%)",
        xlab = "DBH Class (cm)",
        ylim = c(0,40),
        main = "MF4")

mtext( #fun��o plota textos nas �reas ao redor do gr�fico
  c("0-10", "10-30","30-50", ">50"), #primeiro argumento refere oa texto plotado
  side= 1, #argumento localiza no grafico "1" abaixo
  cex=1.5,line=0.9,
  at=c(1.4,3.5,6,8.5))


barplot(biomass_all_Fbar_sep_by_DHB_distribution$Biomass_percentage,
        las = 2, # Rotate x-axis labels for readability
        col = color (2),
        ylab = "",
        xlab = "DBH Class (cm)",
        ylim = c(0,60),
        main = "MF5")

mtext( #fun��o plota textos nas �reas ao redor do gr�fico
  c("0-10", "10-30","30-50", ">50"), #primeiro argumento refere oa texto plotado
  side= 1, #argumento localiza no grafico "1" abaixo
  cex=1.5,line=0.9,
  at=c(1.4,3.5,6,8.5))


barplot(biomass_all_bp_sep_by_DHB_distribution$Biomass_percentage,
        las = 2, # Rotate x-axis labels for readability
        col = color (2),
        ylab = "",
        xlab = "DBH Class (cm)",
        ylim = c(0,40),
        main = "MF6")

mtext( #fun��o plota textos nas �reas ao redor do gr�fico
  c("0-10", "10-30","30-50", ">50"), #primeiro argumento refere oa texto plotado
  side= 1, #argumento localiza no grafico "1" abaixo
  cex=1.5,line=0.9,
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

################ Indivuidual distribution DBH #########

jpeg(filename = "Ind_distri_DHB_percente.jpg",
     width = 850, height = 500, # fun��o salva gr�ficos em .jpg
     units = "px", quality = 75,
     bg = "white")

par(mfrow=c(2,3),mar=c(5,6,3,2), cex.axis=1.5,
    cex.lab=2, mgp=c(3,1,0.3),
    family="serif",las=1, tcl=0.3, bty = "n", xaxt="n")

color <- colorRampPalette(c("lightgreen","sandybrown"))

barplot (ind_all_bc_sep_by_DHB_distribution$Ind_percentage,
         ylim = c(0,60),
         col=color (2),
         ylab = "Individuals (%)",
         main = "MF1")


mtext( #fun��o plota textos nas �reas ao redor do gr�fico
  c("0-10", "10-30","30-50", ">50"), #primeiro argumento refere oa texto plotado
  side= 1 #argumento localiza no grafico "1" abaixo
  , cex = 1.5, line=0.9,
  at=c(1.4,3.5,6,8.5))



barplot (ind_all_it_sep_by_DHB_distribution$Ind_percentage,
         col=color (2),
         ylab = "",
         ylim = c(0,60),
         main = "MF2")



mtext( #fun��o plota textos nas �reas ao redor do gr�fico
  c("0-10", "10-30","30-50", ">50"), #primeiro argumento refere oa texto plotado
  side= 1, #argumento localiza no grafico "1" abaixo
  cex = 1.5,line=0.9,
  at=c(1.4,3.5,6,8.5))


barplot(ind_all_cj_sep_by_DHB_distribution$Ind_percentage,
        las = 2, # Rotate x-axis labels for readability
        col = color (2),
        ylab = " ",
        ylim = c(0,35),
        main = "MF3")

mtext( #fun��o plota textos nas �reas ao redor do gr�fico
  c("0-10", "10-30","30-50", ">50"), #primeiro argumento refere oa texto plotado
  side= 1, #argumento localiza no grafico "1" abaixo
  cex=1.5,line=0.9,
  at=c(1.4,3.5,6,8.5))


barplot(ind_all_Fsf_sep_by_DHB_distribution$Ind_percentage,
        las = 2, # Rotate x-axis labels for readability
        col = color (2),
        ylab = "Individuals (%)",
        xlab = "DBH Class (cm)",
        ylim = c(0,35),
        main = "MF4")

mtext( #fun��o plota textos nas �reas ao redor do gr�fico
  c("0-10", "10-30","30-50", ">50"), #primeiro argumento refere oa texto plotado
  side= 1, #argumento localiza no grafico "1" abaixo
  cex=1.5,line=0.9,
  at=c(1.4,3.5,6,8.5))


barplot(ind_all_Fbar_sep_by_DHB_distribution$Ind_percentage,
        las = 2, # Rotate x-axis labels for readability
        col = color (2),
        ylab = "",
        xlab = "DBH Class (cm)",
        ylim = c(0,60),
        main = "MF5")

mtext( #fun��o plota textos nas �reas ao redor do gr�fico
  c("0-10", "10-30","30-50", ">50"), #primeiro argumento refere oa texto plotado
  side= 1, #argumento localiza no grafico "1" abaixo
  cex=1.5,line=0.9,
  at=c(1.4,3.5,6,8.5))


barplot(ind_all_bp_sep_by_DHB_distribution$Ind_percentage,
        las = 2, # Rotate x-axis labels for readability
        col = color (2),
        ylab = "",
        xlab = "DBH Class (cm)",
        ylim = c(0,30),
        main = "MF6")

mtext( #fun��o plota textos nas �reas ao redor do gr�fico
  c("0-10", "10-30","30-50", ">50"), #primeiro argumento refere oa texto plotado
  side= 1, #argumento localiza no grafico "1" abaixo
  cex=1.5,line=0.9,
  at=c(1.4,3.5,6,8.5))

legend("topright" #fun��o adiciona um texto ao gr�fico,
       #arg 1� define a localiza��o, usa-se a fun��o locator para
       #adicionar de uma forma interativa
       ,c("Tropical","Temperate") #texto a ser escrito
       ,col=color (2)
       ,cex=1.5		#tamanho da fonte
       , pch=c(15,15)
       ,bty = "n") #tipo da fonte

dev.off()

####### plot biomass species######
####### Final dataset for plotting bio ######
MF_top5 <- table_final_to_anova %>%
  group_by(site) %>%
  slice_max(biomass_total,
            n = 5,
            with_ties = FALSE) %>%
  ungroup()
MF1_top_5 <- MF_top5 [MF_top5$site == "MF1",]
MF2_top_5 <- MF_top5 [MF_top5$site == "MF2",]
MF3_top_5 <- MF_top5 [MF_top5$site == "MF3",]
MF4_top_5 <- MF_top5 [MF_top5$site == "MF4",]
MF5_top_5 <- MF_top5 [MF_top5$site == "MF5",]
MF6_top_5 <- MF_top5 [MF_top5$site == "MF6",]


jpeg(filename = "top5_species_bio.jpg",
     width = 850, height = 500, # fun��o salva gr�ficos em .jpg
     units = "px", quality = 75,
     bg = "white")

par(mfrow=c(2,3),mar=c(5,6,3,3), cex.axis=1.3, cex.lab=2, mgp=c(3,0.75,0.3),
    family="serif",las=1, tcl=0.3, bty = "l", xaxt="n")


barplot(MF1_top_5$biomass_total,
        ylim = c (0,range (MF1_top_5$biomass_total) [2]+10),
        col = c ("lightgreen",
                 "sandybrown",
                 rep ("lightgreen",3)),
        las = 2,                # Nomes na vertical
        ylab = expression("Biomass Mg.ha"^-1),
        main = "MF1")

labels <- c( "M. ela", "A. ang" ,"O. ela", "O. pub" ,"C. fis")

text(x = c(0.7,2,3.1,4.3,5.5),
     y = par("usr")[3] - 3,   # below the axis
     labels = labels,
     srt = 45,                # rotation angle
     xpd = TRUE,              # allow text outside plot
     adj = 1,                 # text alignment
     cex = 2)


barplot(MF2_top_5$biomass_total,
        ylim = c (0,range (MF2_top_5$biomass_total) [2]+5),
        col = c (rep ("lightgreen",3),
                 "sandybrown",
                 "lightgreen"),
        las = 2,                # Nomes na vertical
        cex.names = 0.8,
        main = "MF2")

labels <- c("P. rig", "M. ela","L. div", "A. ang", "M. nyc")

text(x = c(0.7,2,3.1,4.3,5.5),
     y = par("usr")[3] - 2.5,   # below the axis
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
        cex.names = 0.8,
        main = "MF3")

labels <- c("P. lam", "A. ang","M. mie", "M. umb", "C. sel")

text(x = c(0.7,2,3.1,4.3,5.5),
     y = par("usr")[3]- 4,   # below the axis
     labels = labels,
     srt = 45,                # rotation angle
     xpd = TRUE,              # allow text outside plot
     adj = 1,                 # text alignment
     cex = 2)


barplot(MF4_top_5$biomass_total,
        ylim = c (0,range (MF4_top_5$biomass_total) [2]+10),
        col = c (rep ("sandybrown",4),
                 "lightgreen"),
        las = 2,                # Nomes na vertical
        cex.names = 0.8,        # Tamanho dos nomes
        ylab = expression("Biomass Mg.ha"^-1),
        main = "MF4")

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
        cex.names = 0.8,
        main = "MF5")

labels <- c("M. ela", "A. ang","S. rev", "L. pac", "S. ins")

text(x = c(0.7,2,3.1,4.3,5.5),
     y = par("usr")[3] - 3,   # below the axis
     labels = labels,
     srt = 45,                # rotation angle
     xpd = TRUE,              # allow text outside plot
     adj = 1,                 # text alignment
     cex = 2)

barplot(MF6_top_5$biomass_total,
        ylim = c (0,range (MF6_top_5$biomass_total) [2]+10),
        col = c (rep ("sandybrown",3),
                 rep ("lightgreen",2)),
        las = 2,                # Nomes na vertical
        cex.names = 0.8,
        main = "MF6")

labels <- c("P. lam","A. ang","M. bra", "M. umb",  "p. reg")

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




####### Final dataset for plotting ind ######

MF_top5 <- table_final_to_anova %>%
  group_by(site) %>%
  slice_max(n,
            n = 5,
            with_ties = FALSE) %>%
  ungroup()
MF1_top_5 <- MF_top5 [MF_top5$site == "MF1",]
MF2_top_5 <- MF_top5 [MF_top5$site == "MF2",]
MF3_top_5 <- MF_top5 [MF_top5$site == "MF3",]
MF4_top_5 <- MF_top5 [MF_top5$site == "MF4",]
MF5_top_5 <- MF_top5 [MF_top5$site == "MF5",]
MF6_top_5 <- MF_top5 [MF_top5$site == "MF6",]



jpeg(filename = "top5_species_ind.jpg",
     width = 850, height = 500, # fun��o salva gr�ficos em .jpg
     units = "px", quality = 75,
     bg = "white")

par(mfrow=c(2,3),mar=c(5,6,3,2), cex.axis=1.3, cex.lab=2, mgp=c(3,0.75,0.3),
    family="serif",las=1, tcl=0.3, bty = "l", xaxt="n")


barplot(MF1_top_5$n,
        ylim = c (0,range (MF1_top_5$n) [2]+50),
        col = c (rep ("lightgreen",4),
                 "sandybrown"),
        las = 2,                # Nomes na vertical
        ylab = expression("Individual.ha"^-1),
        main = "MF1")

labels <- c("G. ulm", "M. ela","O. ela", "S. acu", "A. ang")

text(x = c(0.7,2,3.1,4.3,5.5),
     y = par("usr")[3] - 10,   # below the axis
     labels = labels,
     srt = 45,                # rotation angle
     xpd = TRUE,              # allow text outside plot
     adj = 1,                 # text alignment
     cex = 2)


barplot(MF2_top_5$n,
        ylim = c (0,range (MF2_top_5$n) [2]+100),
        col = c (rep ("lightgreen",5)),
        las = 2,                # Nomes na vertical
        cex.names = 0.8,
        main = "MF2")

labels <- c("S. bon", "E. lig","E. edu", "R. jas", "S. ram")

text(x = c(0.7,2,3.1,4.3,5.5),
     y = par("usr")[3] - 10,   # below the axis
     labels = labels,
     srt = 45,                # rotation angle
     xpd = TRUE,              # allow text outside plot
     adj = 1,                 # text alignment
     cex = 2)


barplot(MF3_top_5$n,
        ylim = c (0,range (MF3_top_5$n) [2]+100),
        col = c (rep ("sandybrown",2),
                 rep ("lightgreen",3)),
        las = 2,                # Nomes na vertical
        cex.names = 0.8,
        main = "MF3")


labels <- c("M. mir", "D. bra","M. har", "M. umb", "C. sel")

text(x = c(0.7,2,3.1,4.3,5.5),
     y = par("usr")[3]- 15,   # below the axis
     labels = labels,
     srt = 45,                # rotation angle
     xpd = TRUE,              # allow text outside plot
     adj = 1,                 # text alignment
     cex = 2)


barplot(MF4_top_5$n,
        ylim = c (0,range (MF4_top_5$n) [2]+100),
        col = c ("sandybrown",
                 rep ("lightgreen",4)),
        las = 2,                # Nomes na vertical
        cex.names = 0.8,        # Tamanho dos nomes
        ylab = expression("Individual.ha"^-1),
        main = "MF4")

labels <- c("D. bra", "M. har","S. var", "C. sca", "M. umb")

text(x = c(0.7,2,3.1,4.3,5.5),
     y = par("usr")[3] - 10,   # below the axis
     labels = labels,
     srt = 45,                # rotation angle
     xpd = TRUE,              # allow text outside plot
     adj = 1,                 # text alignment
     cex = 2)

barplot(MF5_top_5$n,
        ylim = c (0,range (MF5_top_5$n) [2]+150),
        col = c (rep ("lightgreen",5)),
        las = 2,                # Nomes na vertical
        cex.names = 0.8,
        main = "MF5")

labels <- c("M. ela", "L. pac","M. lar", "E. plu", "O. pul")

text(x = c(0.7,2,3.1,4.3,5.5),
     y = par("usr")[3] - 15,   # below the axis
     labels = labels,
     srt = 45,                # rotation angle
     xpd = TRUE,              # allow text outside plot
     adj = 1,                 # text alignment
     cex = 2)

barplot(MF6_top_5$n,
        ylim = c (0,range (MF6_top_5$n) [2]+150),
        col = c (rep ("sandybrown",2),
                 "lightgreen",
                 "sandybrown"
                 ,"lightgreen"),
        las = 2,                # Nomes na vertical
        cex.names = 0.8,
        main = "MF6")

labels <- c("M. bra", "P. lam","M. umb", "A. ang", "M. lar")

text(x = c(0.7,2,3.1,4.3,5.5),
     y = par("usr")[3] - 10,   # below the axis
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


