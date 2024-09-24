source("import_processing_biomass_data.R")

site_spp = function(x, site = "cj"){

    sites <-  c("cj", "bp", "bc", "fsf","fb", "it")
    if (is.na (match(site,sites)) == TRUE)
      stop ("wrong spell site name")
    x$bino <- paste (x$Gen, x$Spp, sep = "_")
    spp_count = x|>
        group_by ( bino) |>
        count (bino)
    tranpon_spp_count= as_tibble(t(spp_count),.name_repair = "minimal")
    colnames(tranpon_spp_count) <-  tranpon_spp_count [1,]
    tranpon_spp_count <- tranpon_spp_count [-1,]
    tranpon_spp_count <- tranpon_spp_count |>
      mutate(across(everything(), as.numeric))
    tranpon_spp_count=as.data.frame(tranpon_spp_count)


    site <- site [site %in% sites]
    rownames(tranpon_spp_count) <- site
    tranpon_spp_count$id <- site
    tranpon_spp_count <- tranpon_spp_count |> relocate(id)
    return(tranpon_spp_count)
}

spp_site_campos_do_jordao <-  site_spp(bio.cj)
spp_site_baependi <- site_spp(bio.bp, site="bp")
spp_site_barra_do_chapeu<- site_spp(bio.BC, site = "bc")
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

colnames(all_sites) <- c("bc"="MF1", "bp"="MF6",
                         "cj"="MF3", "fb"="MF4"
                         ,"fsf"="MF5", "it"="MF2")

all_sites=all_sites|>
  relocate(MF2, .after =MF1)
all_sites=all_sites|>
  relocate(MF6, .after =MF5)


