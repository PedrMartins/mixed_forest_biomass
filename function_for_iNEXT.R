source("import_processing_biomass_data.R")
source("Function_biomass.R")



spp_site_campos_do_jordao <-  site_spp(bio.cj)
spp_site_baependi <- site_spp(bio.bp, site="bp")
spp_site_barra_do_chapeu<- site_spp(bio.bc, site = "bc")
spp_site_itabera<- site_spp(bio.It, site = "it")
spp_site_Delfim_Faz_saoFrancisco<- site_spp(bio.Fsf, site = "fsf")
spp_site_Delfim_Faz_bartira<- site_spp(bio.Fbar, site = "fb")

all_sites=merge (spp_site_campos_do_jordao,
         spp_site_baependi,
         all = TRUE)
all_sites=merge (all_sites,
                          spp_site_barra_do_chapeu,
                          all = TRUE)
all_sites=merge (all_sites,
                          spp_site_itabera,
                          all = TRUE)
all_sites=merge (all_sites,
                          spp_site_Delfim_Faz_saoFrancisco,
                          all = TRUE)
all_sites=merge (all_sites,
                          spp_site_Delfim_Faz_bartira,
                          all = TRUE)

all_sites[is.na(all_sites)] <- 0

rownames(all_sites) <- all_sites[,1]

all_sites <-  all_sites [,-1]

all_sites <- t (all_sites)

all_sites <- as.data.frame(all_sites)

head (all_sites)

colnames(all_sites) <- c("bc"="MF1", "bp"="MF6",
                         "cj"="MF3", "fb"="MF5"
                         ,"fsf"="MF4", "it"="MF2")

all_sites=all_sites|>
  relocate(MF2, .after =MF1)
all_sites=all_sites|>
  relocate(MF6, .after =MF5)


