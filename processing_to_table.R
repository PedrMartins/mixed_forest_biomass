#########   Dens table      ######

cj_dens_table <- wood_dens_table (bio.cj)
bc_dens_table <- wood_dens_table (bio.bc, name = "bc")
it_dens_table <- wood_dens_table (bio.It, name = "it")
Fsf_dens_table <- wood_dens_table (bio.Fsf, name = "Fsf")
bp_dens_table <- wood_dens_table (bio.bp, name = "bp")
Fbar_dens_table <- wood_dens_table (bio.Fbar, name = "Fbar")


################Biomass and species abundance######

MF1_ind_ab=biomas_and_individuals (bio.bc, methods = "ind" ,name = "bc")
MF2_ind_ab=biomas_and_individuals (bio.It, methods = "ind" ,name = "it")
MF3_ind_ab=biomas_and_individuals (bio.cj, methods = "ind" ,name = "cj")
MF4_ind_ab=biomas_and_individuals (bio.Fsf, methods = "ind" ,name = "Fsf")
MF5_ind_ab=biomas_and_individuals (bio.Fbar, methods = "ind" ,name = "Fbar")
MF6_ind_ab=biomas_and_individuals (bio.bp, methods = "ind" ,name = "bp")
MF6_ind_ab$n <- MF6_ind_ab$n/0.5



species_number_absolute_site=rbind (MF1_ind_ab,
                                    MF2_ind_ab,
                                    MF3_ind_ab,
                                    MF4_ind_ab,
                                    MF5_ind_ab,
                                    MF6_ind_ab)


MF1_bio_ab=biomas_and_individuals (bio.bc, methods = "bio" ,name = "bc")
MF2_bio_ab=biomas_and_individuals (bio.It, methods = "bio" ,name = "it")
MF3_bio_ab=biomas_and_individuals (bio.cj, methods = "bio" ,name = "cj")
MF4_bio_ab=biomas_and_individuals (bio.Fsf, methods = "bio" ,name = "Fsf")
MF5_bio_ab=biomas_and_individuals (bio.Fbar, methods = "bio" ,name = "Fbar")
MF6_bio_ab=biomas_and_individuals (bio.bp, methods = "bio" ,name = "bp")
MF6_bio_ab$biomass_total <- MF6_bio_ab$biomass_total/0.5

all_sites_biomass_spp <- bind_rows(
  MF1_bio_ab,
  MF2_bio_ab,
  MF3_bio_ab,
  MF4_bio_ab,
  MF5_bio_ab,
  MF6_bio_ab
)

table_final_to_anova <- full_join(species_number_absolute_site,
                                  all_sites_biomass_spp,
                                  join_by(binom,site,Distri))

table_final_to_anova <- table_final_to_anova[,-c(1,2,8,9)]
table_final_to_anova$biomass_total<- table_final_to_anova [,6]/1000
table_final_to_excel <- pivot_wider(table_final_to_anova,
                                    names_from =site,
                                    values_from = c(
                                      biomass_total, n))
table_to_permanova_biomass <- pivot_wider(table_final_to_anova,
                                          names_from =binom,
                                          values_from = biomass_total)
table_to_permanova_biomass <- table_to_permanova_biomass[, -c(1:5,7,8)]
table_final_to_excel <- table_final_to_excel[order (table_final_to_excel$Fam,
                                                    table_final_to_excel$binom),]

# table_to_permanova_biomass[is.na(table_to_permanova_biomass)] <- 0
# table_final_to_anova[is.na(table_final_to_anova)] <- 0
# table_final_to_excel[is.na(table_final_to_excel)] <- 0
colnames(table_final_to_excel) <- c("Fam" ="Famyli", "Distri"="Fitogeography_origin",
                                    "binom"  = "Genus_and_species",
                                    "n_MF1",             "n_MF2",
                                    "n_MF3",             "n_MF4",
                                    "n_MF5",             "n_MF6",
                                    "biomass_total_MF1"= "biomass_total_MF1 (Mg)",
                                    "biomass_total_MF2"= "biomass_total_MF2(Mg)",
                                    "biomass_total_MF3"="biomass_total_MF3 (Mg)",
                                    "biomass_total_MF4"="biomass_total_MF4 (Mg)",
                                    "biomass_total_MF5"="biomass_total_MF5 (Mg)",
                                    "biomass_total_MF6"="biomass_total_MF6 (Mg)")
#
############structure##########

MF1_fam_number <-  length(unique(MF1_ind_ab$Fam))
MF1_gen_number <-  length(unique(MF1_ind_ab$Gen))

MF2_fam_number <-  length(unique(MF2_ind_ab$Fam))
MF2_gen_number <-  length(unique(MF2_ind_ab$Gen))

MF3_fam_number <-  length(unique(MF3_ind_ab$Fam))
MF3_gen_number <-  length(unique(MF3_ind_ab$Gen))

MF4_fam_number <-  length(unique(MF4_ind_ab$Fam))
MF4_gen_number <-  length(unique(MF4_ind_ab$Gen))

MF5_fam_number <-  length(unique(MF5_ind_ab$Fam))
MF5_gen_number <-  length(unique(MF5_ind_ab$Gen))

MF6_fam_number <-  length(unique(MF6_ind_ab$Fam))
MF6_gen_number <-  length(unique(MF6_ind_ab$Gen))

all_fam <- length (unique (species_number_absolute_site$Fam))
all_gen <- length (unique (species_number_absolute_site$Gen))




############plot processing##########

data_cj_sapareted <- separate_by_filo_distri(bio.cj, by ="distri")
data_bp_sapareted <- separate_by_filo_distri(bio.bp, by ="distri")
data_Fsf_sapareted <- separate_by_filo_distri(bio.Fsf, by ="distri")
data_Fbar_sapareted <- separate_by_filo_distri(bio.Fbar, by ="distri")
data_bc_sapareted <- separate_by_filo_distri(bio.bc, by ="distri")
data_it_sapareted <- separate_by_filo_distri(bio.It, by ="distri")

bio.tem.trop.pro = data.frame(
  BC_SP=c(data_bc_sapareted[,2]),
  IT_SP=c(data_it_sapareted[,2]),
  CJ_SP=c(data_cj_sapareted[,2]),
  FSF_MG=c(data_Fsf_sapareted[,2]),
  FB_MG=c(data_Fbar_sapareted[,2]),
  BP_MG=c(data_bp_sapareted[,2])
)

bio.tem.trop = data.frame(
  BC_SP=c(data_bc_sapareted[,1]),
  IT_SP=c(data_it_sapareted[,1]),
  CJ_SP=c(data_cj_sapareted[,1]),
  FSF_MG=c(data_Fsf_sapareted[,1]),
  FB_MG=c(data_Fbar_sapareted[,1]),
  BP_MG=c(data_bp_sapareted[,1]/0.5)
)

ind.tem.trop.pro = data.frame(
  BC_SP=c(data_bc_sapareted[,4]),
  IT_SP=c(data_it_sapareted[,4]),
  CJ_SP=c(data_cj_sapareted[,4]),
  FSF_MG=c(data_Fsf_sapareted[,4]),
  FB_MG=c(data_Fbar_sapareted[,4]),
  BP_MG=c(data_bp_sapareted[,4])
)

ind.tem.trop = data.frame(
  BC_SP=c(data_bc_sapareted[,3]),
  IT_SP=c(data_it_sapareted[,3]),
  CJ_SP=c(data_cj_sapareted[,3]),
  FSF_MG=c(data_Fsf_sapareted[,3]),
  FB_MG=c(data_Fbar_sapareted[,3]),
  BP_MG=c(data_bp_sapareted[,3]/0.5)
)



rownames(bio.tem.trop.pro) <- c("Tempearate Genera", "Tropical Genera")
colnames(bio.tem.trop.pro) <- c("Barra \n do Chapéu"="MF1"
                                ,"Itaberá" = "MF2",
                                "Campos do Jordão"="MF3",
                                "Delfim Moreira \n Faz. São Fran."="MF4",
                                "Delfim Moreira \n Faz. Bart."="MF5",
                                "Baependi"="MF6"
)
rownames(ind.tem.trop.pro) <- c("Tempearate Genera", "Tropical Genera")
colnames(ind.tem.trop.pro) <- c("Barra \n do Chapéu"="MF1"
                                ,"Itaberá" = "MF2",
                                "Campos do Jordão"="MF3",
                                "Delfim Moreira \n Faz. São Fran."="MF4",
                                "Delfim Moreira \n Faz. Bart."="MF5",
                                "Baependi"="MF6"
)

rownames(ind.tem.trop) <- c("Tempearate Genera", "Tropical Genera")
colnames(ind.tem.trop) <- c("Barra \n do Chapéu"="MF1"
                            ,"Itaberá" = "MF2",
                            "Campos do Jordão"="MF3",
                            "Delfim Moreira \n Faz. São Fran."="MF4",
                            "Delfim Moreira \n Faz. Bart."="MF5",
                            "Baependi"="MF6"
)

rownames(bio.tem.trop) <- c("Tempearate Genera", "Tropical Genera")
colnames(bio.tem.trop) <- c("Barra \n do Chapéu"="MF1"
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
bio.temp= as.matrix (bio.tem.trop)
ind.temp= as.matrix (ind.tem.trop)


