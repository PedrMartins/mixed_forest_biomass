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
formula


########test#############
adonis2(table_to_permanova_biomass [,-1]~site ,
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
