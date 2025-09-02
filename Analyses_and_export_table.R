# source("processing_to_table.R")

############test analises###########
cotingency <- xtabs(biomass_total~site+Distri,
                     data = table_final_to_anova,
                     na.rm= TRUE)

aov()

head (table_final_to_excel)
View (cotingency)
summary (chisq.test (cotingency))


#####resultado massa#####

anova (aov(log(biomass_total)~Distri, #diferença estatística dentro dos sites
           data=MF6_bio_ab))

anova (aov(sqrt (biomass_total)~site, #diferença entre os sites
             data=table_final_to_anova))

anova (aov(sqrt(biomass_total)~site, #diferença entre os sites
           data=table_final_to_anova))

anova (aov(log(biomass_total)~Distri, #diferença entre os sites
           data=table_final_to_anova))
t.test(biomass_total~Distri,data=table_final_to_anova)
t.test(log(biomass_total)~Distri,data=MF6_bio_ab)
?formula


#######
?adonis2(table_to_permanova_biomass [,-1]~site ,
         method = "euclidean",
         data = table_to_permanova_biomass)

biomass_all_bc_sep_by_DHB_filo
names (table_to_permanova_biomass) [1]

ang=sum(bio.temp.trop[c(3,4),])
gim=sum(bio.temp.trop[c(1,2),])
med=mean (bio.temp.trop)
dbio=bio.temp.trop-med
dqbio=dbio^2
sqbio=sum(dqbio)

###########################################
### table lvl wood dens get ####

dens_table <- rbind(bc_dens_table,
                    it_dens_table,
                    cj_dens_table,
                    Fsf_dens_table,
                    Fbar_dens_table,
                    bp_dens_table)
#View (dens_table)

#write.csv(dens_table, "dens_table.csv")
#################table biomass and absolut number####

write.table(table_final_to_excel,
            "table_final_to_excel2.tsv",
            sep ="\t", dec = ",")

#View(table_final_to_excel)

##################

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

### soma de biomass






# Sort by biomass descending
df <- result[order(-result$biomass_total), ]
df$Species <- paste(df$Gen, df$Spp)

# Top 5
top5 <- df[1:10, ]

# "Other" category sums the rest
other <- data.frame(Species = "Other",
                    biomass_total = sum(df$biomass_total[11:nrow(df)]))

