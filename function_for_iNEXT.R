source("import_processing_biomass_data.R")

site_spp = function(x, site = "cj"){

    sites <-  c("cj", "bp", "bc", "fsf","fb", "it")
    if (is.na (match(site,sites)) == TRUE)
      stop ("nome do site escrito errado")
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
spp_site_Delfim_Faz_sãoFrancisco<- site_spp(bio.Fsf, site = "fsf")
spp_site_Delfim_Faz_bartira<- site_spp(bio.Fbar, site = "fb")

todos_sites_juntos=merge (spp_site_campos_do_jordao,
         spp_site_baependi,
         all = TRUE)
todos_sites_juntos=merge (todos_sites_juntos,
                          spp_site_barra_do_chapeu,
                          all = TRUE)
todos_sites_juntos=merge (todos_sites_juntos,
                          spp_site_itabera,
                          all = TRUE)
todos_sites_juntos=merge (todos_sites_juntos,
                          spp_site_Delfim_Faz_sãoFrancisco,
                          all = TRUE)
todos_sites_juntos=merge (todos_sites_juntos,
                          spp_site_Delfim_Faz_bartira,
                          all = TRUE)

todos_sites_juntos[is.na(todos_sites_juntos)] <- 0



