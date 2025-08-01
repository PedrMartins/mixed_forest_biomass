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

all_sites_biomass_spp <- bind_rows(
  MF1_bio_ab,
  MF2_bio_ab,
  MF3_bio_ab,
  MF4_bio_ab,
  MF5_bio_ab,
  MF6_bio_ab
)

table_final_to_excel <- full_join(species_number_absolute_site, all_sites_biomass_spp,
                                  join_by(binom,site))

table_final_to_excel <- table_final_to_excel[,-c(1,2,7,8)]
table_final_to_excel <- pivot_wider(table_final_to_excel, names_from =site,
                                    values_from = c(biomass_total, n))
table_final_trasnformed <- table_final_to_excel [,c(4:9)]/1000
table_final_to_excel <- cbind(table_final_to_excel[,c(1:3,10:15)], table_final_trasnformed)
table_final_to_excel <- table_final_to_excel[order (table_final_to_excel$Fam,
                                                    table_final_to_excel$binom),]

table_final_to_excel[is.na(table_final_to_excel)] <- "---"

colnames(table_final_to_excel) <- c("Fam" ="Famyli", "binom"  = "Genus and species",
                                    "n_MF1",             "n_MF2",
                                    "n_MF3",             "n_MF4",
                                    "n_MF5",             "n_MF6",
                                    "biomass_total_MF1"= "biomass_total_MF1 (Mg)",
                                    "biomass_total_MF2"= "biomass_total_MF2(Mg)",
                                    "biomass_total_MF3"="biomass_total_MF3 (Mg)",
                                    "biomass_total_MF4"="biomass_total_MF4 (Mg)",
                                    "biomass_total_MF5"="biomass_total_MF5 (Mg)",
                                    "biomass_total_MF6"="biomass_total_MF6 (Mg)")


################indice diversidade######

Barra_do_chapeu_diversity <- list ( "Shannon"=diversity (MF1_ind_ab$n),
                                    "Simpson"=diversity (MF1_ind_ab$n,
                                                         "simpson"))
Itabera_diversity <- list ("Shannon"=diversity (MF2_ind_ab$n),
                           "Simpson"=diversity (MF2_ind_ab$n,
                                      "simpson"))
Campos_Jordao_diversity <- list ("Shannon"=diversity (MF3_ind_ab$n),
                                 "Simpson"=diversity (MF3_ind_ab$n,
                                      "simpson"))
Faz_Sao_Fran_diversity <- list ("Shannon"=diversity (MF4_ind_ab$n),
                                "Simpson"=diversity (MF4_ind_ab$n,
                                      "simpson"))
Faz_Bar_diversity <- list ("Shannon"=diversity (MF5_ind_ab$n),
                           "Simpson"=diversity (MF5_ind_ab$n,
                                      "simpson"))
Baependi_diversity <- list ("Shannon"=diversity (MF6_ind_ab$n),
                            "Simpson"=diversity (MF6_ind_ab$n,
                                      "simpson"))

 #G=count (F_E, Gen)
 #G <- G[order (G$n),]
 #F_es <- F_E[!duplicated  (paste(F_E$Gen,F_E$Spp)),]
 #length(F_es$Gen)





