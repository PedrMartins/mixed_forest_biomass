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



barplot (ind_all_it_sep_by_DHB_distribution$Ind_percentage,
         col=color (2), ylim=c(0,70),
         main="Itaberá (MF2)")


barplot (ind_all_cj_sep_by_DHB_distribution$Ind_percentage,
         col=color (2), ylim=c(0,70),
         main="Campos do Jordão (MF3)")


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

########################################################################
######bio_filo#####
######bio_geral#####

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
         ylim=c(0,400),
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

############ Biomass Distri DBH #####

jpeg(filename = "biomass_distri_DHB2.jpg",
     width = 850, height = 500, # fun��o salva gr�ficos em .jpg
     units = "px", quality = 75,
     bg = "white")

par(mfrow=c(2,3),mar=c(5,6,3,2),
    cex.axis=1.3, cex.lab=1.5,
    mgp=c(3,1.3,0.3), family="serif",
    las=1, tcl=0.3, bty = "n", xaxt="n")


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

par(mfrow=c(2,3),mar=c(5,6,3,2), cex.axis=1.3,
    cex.lab=1.5, mgp=c(3,1.3,0.3),
    family="serif",las=1, tcl=0.3,
    bty = "n", xaxt="n")

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
####### Final dataset for plotting bio ######
MF_top5 <- table_final_to_anova %>%
  group_by(site) %>%
  slice_max(biomass_total,
            n = 10,
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

par(mfrow=c(2,3),mar=c(5,6,3,2), cex.axis=1.3, cex.lab=2, mgp=c(3,1.3,0.3),
    family="serif",las=1, tcl=0.3, bty = "l", xaxt="n")


barplot(MF1_top_5$n,
        ylim = c (0,range (MF1_top_5$n) [2]+50),
        col = c (rep ("lightgreen",4),
                 "sandybrown"),
        las = 2,                # Nomes na vertical
        ylab = "Biomass Mg",
        main= "MF1")

labels <- c("G. ulm", "M. ela","O. ela", "S. acu", "A. ang")

text(x = c(0.7,2,3.1,4.3,5.5),
     y = par("usr")[3] - 3,   # below the axis
     labels = labels,
     srt = 45,                # rotation angle
     xpd = TRUE,              # allow text outside plot
     adj = 1,                 # text alignment
     cex = 2)

mtext ("topright", "a")

barplot(MF2_top_5$n,
        ylim = c (0,range (MF2_top_5$n) [2]+10),
        col = c (rep ("lightgreen",3),
                 "sandybrown",
                 "lightgreen"),
        las = 2,                # Nomes na vertical
        cex.names = 0.8,        # Tamanho dos nomes
        main= "MF2")

labels <- c("S. bon", "E. lig","E. edu", "R. jas", "S. ram")

text(x = c(0.7,2,3.1,4.3,5.5),
     y = par("usr")[3] - 3,   # below the axis
     labels = labels,
     srt = 45,                # rotation angle
     xpd = TRUE,              # allow text outside plot
     adj = 1,                 # text alignment
     cex = 2)


barplot(MF3_top_5$n,
        ylim = c (0,range (MF3_top_5$n) [2]+10),
        col = c (rep ("sandybrown",3),
                 rep ("lightgreen",2)),
        las = 2,                # Nomes na vertical
        cex.names = 0.8,        # Tamanho dos nomes
        main= "MF3")

labels <- c("M. mir", "D. bra","M. har", "M. umb", "C. sel")

text(x = c(0.7,2,3.1,4.3,5.5),
     y = par("usr")[3] - 3,   # below the axis
     labels = labels,
     srt = 45,                # rotation angle
     xpd = TRUE,              # allow text outside plot
     adj = 1,                 # text alignment
     cex = 2)


barplot(MF4_top_5$n,
        ylim = c (0,range (MF4_top_5$n) [2]+10),
        col = c (rep ("sandybrown",4),
                 rep ("lightgreen",1)),
        las = 2,                # Nomes na vertical
        cex.names = 0.8,        # Tamanho dos nomes
        main= "MF4",               # Nomes na vertical
        ylab = "Biomass Mg")

labels <- c("D. bra", "M. har","S. var", "C. sca", "M. umb")

text(x = c(0.7,2,3.1,4.3,5.5),
     y = par("usr")[3] - 3,   # below the axis
     labels = labels,
     srt = 45,                # rotation angle
     xpd = TRUE,              # allow text outside plot
     adj = 1,                 # text alignment
     cex = 2)

barplot(MF5_top_5$n,
        ylim = c (0,range (MF5_top_5$n) [2]+10),
        col = c ("lightgreen",
                 "sandybrown",
                 rep ("lightgreen",3)),
        las = 2,                # Nomes na vertical
        cex.names = 0.8,        # Tamanho dos nomes
        main= "MF5")

labels <- c("M. ela", "L. pac","M. lar", "E. plu", "O. pul")

text(x = c(0.7,2,3.1,4.3,5.5),
     y = par("usr")[3] - 3,   # below the axis
     labels = labels,
     srt = 45,                # rotation angle
     xpd = TRUE,              # allow text outside plot
     adj = 1,                 # text alignment
     cex = 2)

barplot(MF6_top_5$n,
        ylim = c (0,range (MF6_top_5$n) [2]+11),
        col = c (rep ("sandybrown",3),
                 rep ("lightgreen",2)),
        las = 2,                # Nomes na vertical
        cex.names = 0.8,        # Tamanho dos nomes
        main= "MF6")

labels <- c("M. bra", "P. lam","M. umb", "A. ang", "M. lar")

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
        ylab = expression("Individual.ha"^-1))

mtext("a", side= 3, line =1, adj = 1, cex = 1.5)

labels <- c("G. ulm", "M. ela","O. ela", "S. acu", "A. ang")

text(x = c(0.7,2,3.1,4.3,5.5),
     y = par("usr")[3] - 3,   # below the axis
     labels = labels,
     srt = 45,                # rotation angle
     xpd = TRUE,              # allow text outside plot
     adj = 1,                 # text alignment
     cex = 2)


barplot(MF2_top_5$n,
        ylim = c (0,range (MF2_top_5$n) [2]+100),
        col = c (rep ("lightgreen",5)),
        las = 2,                # Nomes na vertical
        cex.names = 0.8)

mtext("b", side= 3, line =1, adj = 1, cex = 1.5)

labels <- c("S. bon", "E. lig","E. edu", "R. jas", "S. ram")

text(x = c(0.7,2,3.1,4.3,5.5),
     y = par("usr")[3] - 3,   # below the axis
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
        cex.names = 0.8)

mtext("c", side= 3, line =1, adj = 1, cex = 1.5)

labels <- c("M. mir", "D. bra","M. har", "M. umb", "C. sel")

text(x = c(0.7,2,3.1,4.3,5.5),
     y = par("usr")[3] - 3,   # below the axis
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
        ylab = expression("Individual.ha"^-1))

mtext("d", side= 3, line =1, adj = 1, cex = 1.5)

labels <- c("D. bra", "M. har","S. var", "C. sca", "M. umb")

text(x = c(0.7,2,3.1,4.3,5.5),
     y = par("usr")[3] - 3,   # below the axis
     labels = labels,
     srt = 45,                # rotation angle
     xpd = TRUE,              # allow text outside plot
     adj = 1,                 # text alignment
     cex = 2)

barplot(MF5_top_5$n,
        ylim = c (0,range (MF5_top_5$n) [2]+150),
        col = c (rep ("lightgreen",5)),
        las = 2,                # Nomes na vertical
        cex.names = 0.8)

mtext("f", side= 3, line =1, adj = 1, cex = 1.5)

labels <- c("M. ela", "L. pac","M. lar", "E. plu", "O. pul")

text(x = c(0.7,2,3.1,4.3,5.5),
     y = par("usr")[3] - 3.5,   # below the axis
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
        cex.names = 0.8)

mtext("h", side= 3, line =1, adj = 1, cex = 1.5)

labels <- c("M. bra", "P. lam","M. umb", "A. ang", "M. lar")

text(x = c(0.7,2,3.1,4.3,5.5),
     y = par("usr")[3] - 4,   # below the axis
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


