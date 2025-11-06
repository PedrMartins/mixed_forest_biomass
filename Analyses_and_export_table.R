 source("processing_to_table.R")

############test analises###########
cotingency_MF1 <- xtabs(Biomass_percentage ~Class_DAP + Distri,
                     data = biomass_all_bc_sep_by_DHB_distribution,
                     na.rm= TRUE)

cotingency_MF1 <- xtabs(Ind_percentage ~Class_DAP + Distri,
                        data = ind_all_bc_sep_by_DHB_distribution,
                        na.rm= TRUE)

cotingency_MF2 <- xtabs(Biomass_percentage  ~Class_DAP + Distri,
                         data = biomass_all_it_sep_by_DHB_distribution,
                         na.rm= TRUE)

cotingency_MF2 <- xtabs(Ind_percentage  ~Class_DAP + Distri,
                        data = ind_all_it_sep_by_DHB_distribution,
                        na.rm= TRUE)


cotingency_MF3 <- xtabs(Biomass_percentage ~Class_DAP + Distri,
                         data = biomass_all_cj_sep_by_DHB_distribution,
                         na.rm= TRUE)

cotingency_MF3 <- xtabs(Ind_percentage  ~Class_DAP + Distri,
                        data = ind_all_cj_sep_by_DHB_distribution,
                        na.rm= TRUE)



cotingency_MF4 <- xtabs(Biomass_percentage ~Class_DAP + Distri,
                         data = biomass_all_Fsf_sep_by_DHB_distribution,
                         na.rm= TRUE)

cotingency_MF4 <- xtabs(Ind_percentage ~Class_DAP + Distri,
                        data = ind_all_Fsf_sep_by_DHB_distribution,
                        na.rm= TRUE)

cotingency_MF5 <- xtabs(Biomass_percentage ~Class_DAP + Distri,
                         data = biomass_all_Fbar_sep_by_DHB_distribution,
                         na.rm= TRUE)

cotingency_MF5 <- xtabs(Ind_percentage ~Class_DAP + Distri,
                        data = ind_all_Fbar_sep_by_DHB_distribution,
                        na.rm= TRUE)

cotingency_MF6 <- xtabs(Biomass_percentage ~Class_DAP + Distri,
                         data = biomass_all_bp_sep_by_DHB_distribution,
                         na.rm= TRUE)

cotingency_MF6 <- xtabs(Ind_percentage ~Class_DAP + Distri,
                        data = ind_all_bp_sep_by_DHB_distribution,
                        na.rm= TRUE)


chisq.test (cotingency_MF1,
            simulate.p.value = T,
            B= 1000)

chisq.test (cotingency_MF2,
            simulate.p.value = T,
            B= 1000)

chisq.test (cotingency_MF3,
            simulate.p.value = T,
            B= 1000)

chisq.test (cotingency_MF4,
            simulate.p.value = T,
            B= 1000)

chisq.test (cotingency_MF5,
            simulate.p.value = T,
            B= 1000)

chisq.test (cotingency_MF6,
            simulate.p.value = T,
            B= 1000)



### table lvl wood dens get ####
#
# dens_table <- rbind(bc_dens_table,
#                     it_dens_table,
#                     cj_dens_table,
#                     Fsf_dens_table,
#                     Fbar_dens_table,
#                     bp_dens_table)
#View (dens_table)

#write.csv(dens_table, "dens_table.csv")
#################table biomass and absolut number####
#
# write.table(table_final_to_excel,
#             "table_final_to_excel.csv",
#             sep =",", dec = ".")

#View(table_final_to_excel)

##################

range(bio.cj$biom) / 1000
mean(bio.cj$biom) /1000
sd (bio.cj$biom)/1000
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


