#########   Dens table      ######

cj_dens_table <- wood_dens_table (bio.cj)
bc_dens_table <- wood_dens_table (bio.bc, name = "bc")
it_dens_table <- wood_dens_table (bio.It, name = "it")
Fsf_dens_table <- wood_dens_table (bio.Fsf, name = "Fsf")
bp_dens_table <- wood_dens_table (bio.bp, name = "bp")
Fbar_dens_table <- wood_dens_table (bio.Fbar, name = "Fbar")


################Biomass and species abundance######
result_MF1 <- bio.bc %>%
  group_by(Gen, Spp) %>%
  summarise(
    biomass_total = sum(biom),
    .groups = "drop"
  )%>%
  mutate(binom=paste(Gen, Spp, sep = "_"))
result_MF1$site <- rep("MF1", length(result_MF1$Gen))

result_MF2 <- bio.It %>%
  group_by(Gen, Spp) %>%
  summarise(
    biomass_total = sum(biom),
    .groups = "drop"
  )%>%
  mutate(binom=paste(Gen, Spp, sep = "_"))
result_MF2$site <- rep("MF2", length(result_MF2$Gen))

result_MF3 <- bio.cj %>%
  group_by(Gen, Spp) %>%
  summarise(
    biomass_total = sum(biom),
    .groups = "drop"
  )%>%
  mutate(binom=paste(Gen, Spp, sep = "_"))
result_MF3$site <- rep("MF3", length(result_MF3$Gen))

result_MF4 <- bio.Fsf %>%
  group_by(Gen, Spp) %>%
  summarise(
    biomass_total = sum(biom),
    .groups = "drop"
  )%>%
  mutate(binom=paste(Gen, Spp, sep = "_"))
result_MF4$site <- rep("MF4", length(result_MF4$Gen))

result_MF5 <- bio.Fbar %>%
  group_by(Gen, Spp) %>%
  summarise(
    biomass_total = sum(biom),
    .groups = "drop"
  )%>%
  mutate(binom=paste(Gen, Spp, sep = "_"))
result_MF5$site <- rep("MF5", length(result_MF5$Gen))



result_MF6 <- bio.bp %>%
  group_by(Gen, Spp) %>%
  summarise(
    biomass_total = sum(biom),
    .groups = "drop"
  ) %>%
  mutate(binom=paste(Gen, Spp, sep = "_"))
result_MF6$site <- rep("MF6", length(result_MF6$Gen))

 all_sites_biomass_spp <- bind_rows(
  result_MF1,
  result_MF2,
  result_MF3,
  result_MF4,
  result_MF5,
  result_MF6
)



 cj=count (bio.cj, Gen, Spp,Fam,  sort =TRUE)
 cj$site <- rep("MF3", length(cj$Gen))
 length (cj$Spp)
 #length (cj$Spp)/length(All_g$n)

 diversity (cj$n)
 diversity (cj$n, "simpson")

 it=count (bio.It, Gen, Spp,Fam,  sort =TRUE)
 it$site <- rep("MF2", length(it$Gen))
 length (it$Spp)
 #length (it$Spp)/length(All_g$n)


 diversity (it$n)
 diversity (it$n, "simpson")

 bc=count (bio.bc, Gen, Spp, Fam, sort =TRUE)
 bc$site <- rep("MF1", length(bc$Gen))
 length (bc$Spp)
 #length (bc$Spp)/length(All_g$n)

 diversity (bc$n)
 diversity (bc$n, "simpson")

 bp=count (bio.bp, Gen, Spp, Fam, sort =TRUE)
 bp$site <- rep("MF6", length(bp$Gen))
 length (bp$Spp)
 #length (bp$Spp)/length(All_g$n)


 diversity (bp$n)
 diversity (bp$n, "simpson")

 Fsf=count (bio.Fsf, Gen, Spp, Fam, sort =TRUE)
 Fsf$site <- rep("MF4", length(Fsf$Gen))
 length (Fsf$Spp)
 #length (Fsf$Spp)/length(All_g$n)


 diversity (Fsf$n)
 diversity (Fsf$n, "simpson")

 Fbar=count (bio.Fbar, Gen, Spp, Fam, sort =TRUE)
 Fbar$site <- rep("MF5", length(Fbar$Gen))
 length (Fbar$Spp)
 #length (Fbar$Spp)/length(All_g$n)


 diversity (Fbar$n)
 diversity (Fbar$n, "simpson")


 species_number_absolute_site=rbind (cj,it,bc,bp,Fsf,Fbar)
 species_number_absolute_site$binom <- paste(species_number_absolute_site$Gen,
                                             species_number_absolute_site$Spp,
                                             sep= "_")
 #G=count (F_E, Gen)
 #G <- G[order (G$n),]
 #F_es <- F_E[!duplicated  (paste(F_E$Gen,F_E$Spp)),]
 #length(F_es$Gen)


 table_final_to_excel <- full_join(species_number_absolute_site, all_sites_biomass_spp,
                                   join_by(binom,site))
 table_final_to_excel <- table_final_to_excel[,-c(1,2,7,8)]
 table_final_to_excel <- pivot_wider(table_final_to_excel, names_from =site,
                                     values_from = c(biomass_total, n))
 table_final_trasnformed <- table_final_to_excel [,c(3:8)]/1000
 table_final_to_excel <- cbind(table_final_to_excel[,c(1,2,9:14)], table_final_trasnformed)
 table_final_to_excel <- table_final_to_excel[order (table_final_to_excel$Fam,
                                                     table_final_to_excel$binom),]


