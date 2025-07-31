
##############weighted avarege##########



###########import data function#############

import_biomass_rawdata <-  function(site = NULL){

  sites <- c("cj","bp","bc", "it", "fsf","fb")
  site <- match(site, sites)
  source <- c ("cj"= "https://docs.google.com/spreadsheets/d/e/2PACX-1vRUXNWvjRvthnn29O8zSDdCDzN7F3ds2oR37xbcExjdzCLMk2TiZBxuBQuCpnNS5g/pub?output=csv",
               "bp"="https://docs.google.com/spreadsheets/d/e/2PACX-1vSKa8azcr4AD5GI0SG1ay27BVSHkzKAvHdZ0G_5tnzZkObrFy7kdCtLwiuSLPA_Bg/pub?output=csv",
               "bc" = "https://docs.google.com/spreadsheets/d/e/2PACX-1vSQpuHlnRP-dF01wjmY1jGIAZLQYfK6g2NX2cSPKj_TMV5IBHQwpJ-Fwjx0OPXVOQ/pub?output=csv",
               "it"="https://docs.google.com/spreadsheets/d/e/2PACX-1vSC559eIIhyIKCLOtPrVnnrp3JqoXd6AtJX345fURYEWuJpTw3QgyFnIHOp2DKO6Q/pub?output=csv",
               "fsf"= "https://docs.google.com/spreadsheets/d/e/2PACX-1vTfXCGR2nwhacCPwyN88NADG0g9KIH3KQ5KwO9luxWZz6YLlFTZIdrsLGAkGTHXoQ/pub?output=csv",
               "fb"="https://docs.google.com/spreadsheets/d/e/2PACX-1vQH6MiSv8vZDf5yWIIumiHWGTN3H0NF9yZ26P4Kw4J6nyUNe73PwHCJXLX33GUK4A/pub?output=csv"
  )
  site <- source [site]
  site <- read.csv(site[1], row.names = 1)
  colnames (site)<- c("D","Alt","Vol","Gen","Spp","Fam","Distri","Filo")
  site
}


########### data processing function#############


data_processing <-  function (x){
  c=100
  site = x
  site$DAP <-  c(site$D) * c
  site<- site[site$DAP>=4.7,]
  site <- site [!str_ends(site$Gen,"aceae"),]
  site<- site[site$Filo!="Saman",]
  site[site$Filo=="Gem",8] <- "Gim"
  site [site$Fam=="Arecaceae",8] <- "Palm"
  site [site$Gen == "Syagrus",5] <- "romanzoffiana"
  site [str_starts(site$Fam,"Melastomata"),6] <- "Melastomataceae"
  site [site$Gen == "Mollinedia" & str_starts(site$Spp,"argyr"), 5] <- "argyrogyna"
  site [site$Gen == "Sebastiana",4] <- "Sebastiania"
  site [site$Gen == "Cedrella",4] <- "Cedrela"
  site [site$Gen == "Citronellla",4] <- "Citronella"
  site [site$Gen == "Exostyles",6] <- "Fabaceae"
  site [site$Gen == "Guapira",6] <- "Nyctaginaceae"
  site [site$Gen == "Guazuma",6] <- "Malvaceae"
  site [site$Gen == "Rapanea",6] <- "Primulaceae"
  site [site$Gen == "Piptocarpha",5] <- "regnellii"
  site [site$Gen == "Sapium",5] <- "glandulosum"
  site [site$Gen == "Sorocea",5] <- "bonplandii"
  site [site$Gen == "Trichilia",6] <- "Meliaceae"
  site$Fam <- sub("-.*", "", site$Fam)
  site
}

########### data separating by phylo#############

separate_by_filo <- function (x, choice = "ang"){
  site <- x
  filo = c("ang", "gim", "palm")
  filo = match(choice, filo)
  if (filo==1){
    site<- site[site$Filo!="Gim",]
    site<- site[site$Filo!="Palm",]
    return(site)

  }else if (filo==2){
    site<- site[site$Filo=="Gim",]#separa gimnosperma
    return(site)
  }else{
    site<- site[site$Filo=="Palm",]
    return(site)

  }

}


########### data separating by DBH#############

class_DBH_bio_ind <- function (x, choice = "ind",
                               class =  10, distribution=FALSE){


  site <-  x
  choices <- c ("ind","bio")
  if (choice%in%choices==FALSE) {stop ("Speeled choice wrong. \n Use 'ind' or 'bio'")}
  choice <- match(choice, choices)



  if (choice==1) {
  if (length(class)==1) {
    if (distribution==TRUE){
      distri <- c("Temp", "Trop")
      data_sep_dist <- data.frame()
      for (i in distri){
      tag <- i
      site_sep_distri <- site[site$Distri==i,]
      site_class<-  site_sep_distri [site_sep_distri$DAP>class,]
      site_class_number=length(site_class$DAP)
      site_all_number= length(site$DAP)
      site_class_percentage = (site_class_number/
                                 site_all_number) *100
      data_dap<- data.frame("Class_DAP_cm"= class,
                            "Distri"= tag,
                            "Ind_number"=site_class_number,
                            "Ind_percentage"=site_class_percentage,
                            "Total_ind"=site_all_number)

      data_sep_dist <- rbind(data_dap,data_sep_dist)
      }
      data_dap <- data_sep_dist
    }else{
  site_class<-  site [site$DAP<class,]
  site_class_number=length(site_class$DAP)
  site_all_number= length(site$DAP)
  site_class_percentage = (site_class_number/
                             site_all_number) *100
  data_dap<- data.frame("Class_DAP"=class,
                        "Ind_number"=site_class_number,
                        "Ind_percentage"=site_class_percentage,
                        "Total_ind"=site_all_number)
}
} else {
  if (distribution==TRUE){
    distri <- c("Temp", "Trop")
    data_sep_dist <- data.frame()

    for (i in distri){

      tag <- i
      site_sep_distri <- site[site$Distri==i,]
      data_dap = data.frame()
      for (j in seq_along(class)){
        if (j==1){
          site_class<-  site_sep_distri [site_sep_distri$DAP<class[1],]
          site_class_number=length(site_class$DAP)
          site_all_number= length(site$DAP)
          site_class_percentage = (site_class_number/
                                     site_all_number) *100
          subset_data <- c(class[1], tag,
                           site_class_number,
                           site_class_percentage,
                           site_all_number)
          data_dap <- rbind(subset_data,data_dap)

        }
        lower_bound <- class[j]
        upper_bound <- class[j + 1]
        if (is.na(upper_bound)==TRUE) {
          subset_data <- site_sep_distri[site_sep_distri$DAP >= lower_bound,]
          site_class_number=length(subset_data$DAP)
          site_all_number= length(site$DAP)
          site_class_percentage = (site_class_number/
                                     site_all_number) *100
          subset_data <- c(class[j], tag, site_class_number,
                           site_class_percentage,
                           site_all_number)

        }else {
          subset_data <- site_sep_distri[site_sep_distri$DAP >= lower_bound &
                                site_sep_distri$DAP < upper_bound, ]
          site_class_number=length(subset_data$DAP)
          site_all_number= length(site$DAP)
          site_class_percentage = (site_class_number/
                                     site_all_number) *100
          subset_data <- c(paste (class[j], class[j+1], sep= "_"),
                           tag,
                           site_class_number,
                           site_class_percentage,
                           site_all_number)
        }

        data_dap <- rbind(subset_data,data_dap)
        colnames(data_dap) <- c("Class_DAP", "Distri","Ind_number",
                                     "Ind_percentage",
                                     "Total_ind")
      }

      data_sep_dist <- rbind(data_dap,data_sep_dist)

    }


    data_dap <- data_sep_dist[order(data_sep_dist$Class_DAP),]

  }else{
  data_dap <- data.frame ()
  for (i in seq_along(class)) {
    if (i==1){
      site_class<-  site [site$DAP<class[1],]
      site_class_number=length(site_class$DAP)
      site_all_number= length(site$DAP)
      site_class_percentage = (site_class_number/
                                 site_all_number) *100
      subset_data <- c(class[1],
                       site_class_number,
                       site_class_percentage,
                       site_all_number)
      data_dap <- rbind(subset_data,data_dap)
    }
    lower_bound <- class[i]
    upper_bound <- class[i + 1]
    if (is.na(upper_bound)==TRUE) {
      subset_data <- site[site$DAP >= lower_bound,]
      site_class_number=length(subset_data$DAP)
      site_all_number= length(site$DAP)
      site_class_percentage = (site_class_number/
                                 site_all_number) *100
      subset_data <- c(class[i], site_class_number,
                       site_class_percentage,
                       site_all_number)

    }else {
    subset_data <- site[site$DAP >= lower_bound &
                                  site$DAP < upper_bound, ]
    site_class_number=length(subset_data$DAP)
    site_all_number= length(site$DAP)
    site_class_percentage = (site_class_number/
                               site_all_number) *100
    subset_data <- c(paste (class[i], class[i+1], sep= "_"),
                    site_class_number,
                    site_class_percentage,
                    site_all_number)
    }

    data_dap <- rbind(subset_data,data_dap)

  }
  colnames(data_dap) <- c("Class_DAP","Ind_number",
             "Ind_percentage",
             "Total_ind")

  data_dap <- data_dap[order(data_dap$Class_DAP),]}
  }

    result <- data_dap
}

  if (choice==2){
    if (length(class)==1) {
      if (distribution==TRUE){
      distri <- c("Temp", "Trop")
      data_sep_dist <- data.frame()
      for (i in distri){
        tag <- i
        site_sep_distri <- site[site$Distri==i,]
        site_class<-  site_sep_distri [site_sep_distri$DAP>class,]
        site_class_biomass=sum(site_class$biom)
        site_all_biomass= sum(site$biom)
        site_biomass_percentage = (site_class_biomass/
                                     site_all_biomass) *100
        data_biomass<- data.frame("Class_DAP"=class,
                                  "Distri"= tag,
                                  "Biomass_ab"=site_class_biomass,
                                  "Biomass_percentage"=site_biomass_percentage,
                                  "Total_ind"=site_all_biomass)

        data_sep_dist <- rbind(data_biomass,data_sep_dist)
      }
      data_biomass <- data_sep_dist
    }else {
      site_class<-  site [site$DAP<class,]
      site_class_biomass=sum(site_class$biom)
      site_all_biomass= sum(site$biom)
      site_biomass_percentage = (site_class_biomass/
                                 site_all_biomass) *100
      data_biomass<- data.frame("Class_DAP"=class, "Biomass_ab"=site_class_biomass,
                            "Biomass_percentage"=site_biomass_percentage,
                            "Total_ind"=site_all_biomass)
}
    } else {
      if(distribution==TRUE){
        distri <- c("Temp", "Trop")
        data_sep_dist <- data.frame()
        for (i in distri){
          tag <- i
          site_sep_distri <- site[site$Distri==i,]
          data_biomass <- data.frame()
          for (j in seq_along(class)){
            if (j==1){
              site_class<-  site_sep_distri [site_sep_distri$DAP<class [1],]
              site_class_biomass=sum(site_class$biom)
              site_all_biomass= sum(site$biom)
              site_biomass_percentage = (site_class_biomass/
                                           site_all_biomass) *100


              subset_data_bio <- c(class[1], tag,
                                   site_class_biomass,
                                   site_biomass_percentage,
                                   site_all_biomass)
              data_biomass <- rbind(subset_data_bio,data_biomass)
            }
            lower_bound <- class[j]
            upper_bound <- class[j + 1]
            if (is.na(upper_bound)==TRUE) {
              subset_data <- site_sep_distri[site_sep_distri$DAP >= lower_bound,]
              site_class_biomass=sum(subset_data$biom)
              site_all_biomass= sum(site$biom)
              site_biomass_percentage = (site_class_biomass/
                                           site_all_biomass) *100


              subset_data_bio <- c(class[j], tag,
                                   site_class_biomass,
                                   site_biomass_percentage,
                                   site_all_biomass)

            }else {
              subset_data <- site_sep_distri[site_sep_distri$DAP >= lower_bound &
                                               site_sep_distri$DAP < upper_bound, ]
              range (subset_data$DAP)
              site_class_biomass=sum(subset_data$biom)
              site_all_biomass= sum(site$biom)
              site_biomass_percentage = (site_class_biomass/
                                           site_all_biomass) *100


              subset_data_bio <- c(paste (class[j], class[j+1], sep= "_"),
                                   tag,
                                   site_class_biomass,
                                   site_biomass_percentage,
                                   site_all_biomass)

            }
            data_biomass <- rbind(subset_data_bio,data_biomass)
          }
          colnames(data_biomass) <- c("Class_DAP",
                                      "Distri",
                                      "Biomass_ab",
                                      "Biomass_percentage",
                                      "Total_ind")
          data_sep_dist <- rbind(data_biomass,data_sep_dist)
        }
        data_biomass <- data_sep_dist[order(data_sep_dist$Class_DAP),]
    }else{
      data_biomass <- data.frame ()
      for (i in seq_along(class)) {
        if (i==1){
          site_class<-  site [site$DAP<class [1],]
          site_class_biomass=sum(site_class$biom)
          site_all_biomass= sum(site$biom)
          site_biomass_percentage = (site_class_biomass/
                                       site_all_biomass) *100


          subset_data_bio <- c(class[1],
                           site_class_biomass,
                           site_biomass_percentage,
                           site_all_biomass)
          data_biomass <- rbind(subset_data_bio,data_biomass)
        }
        lower_bound <- class[i]
        upper_bound <- class[i + 1]
        if (is.na(upper_bound)==TRUE) {
          subset_data <- site[site$DAP >= lower_bound,]
          site_class_biomass=sum(subset_data$biom)
          site_all_biomass= sum(site$biom)
          site_biomass_percentage = (site_class_biomass/
                                       site_all_biomass) *100


          subset_data_bio <- c(class[i],
                               site_class_biomass,
                               site_biomass_percentage,
                               site_all_biomass)

        }else {
          subset_data <- site[site$DAP >= lower_bound &
                                site$DAP < upper_bound, ]
          site_class_biomass=sum(subset_data$biom)
          site_all_biomass= sum(site$biom)
          site_biomass_percentage = (site_class_biomass/
                                       site_all_biomass) *100


          subset_data_bio <- c(paste (class[i], class[i+1], sep= "_"),
                               site_class_biomass,
                               site_biomass_percentage,
                               site_all_biomass)

        }

        data_biomass <- rbind(subset_data_bio,data_biomass)

      }
      colnames(data_biomass) <- c("Class_DAP", "Biomass_ab",
                                      "Biomass_percentage",
                                      "Total_ind")

      data_biomass <- data_biomass[order(data_biomass$Class_DAP),]

    }
      }

    result <- data_biomass
  }


  if (distribution==TRUE){result <- result %>%
    mutate(across(-c(Class_DAP, Distri), as.numeric))}else{result <- result %>%
      mutate(across(-c(Class_DAP), as.numeric))}
  return (result)

}

########### data spp x site #############

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


########### data wood dens table  #############

wood_dens_table <-  function (site, name="cj"){

  site_name<- switch(name,
                     "bc"="MF1",
                     "it"="MF2",
                     "cj"="MF3",
                     "Fsf"="MF4",
                     "Fbar"="MF5",
                     "bp"="MF6"
                     )

  site_count<- count (site, Lvl.D)
  site_soma <- sum (count (site, Lvl.D) [2])
  site_percentage=count (site, Lvl.D) [2]/site_soma * 100
  dens_table <- data.frame("Site"=site_name,
                           "Lvl.Dens" = site_count [,1],
                           "Wood_den_ab"= site_count [,2],
                           "Wood_den_percentage" =site_percentage [,1] )
  dens_table


}


############Biomass and species abundance##############

biomas_and_individuals <- function (site, methods = "bio", name ="cj") {

  if (methods == "bio"){
    result <- site %>%
      group_by(Gen, Spp, Distri) %>%
      summarise(
        biomass_total = sum(biom),
        .groups = "drop"
      )%>%
      mutate(binom=paste(Gen, Spp, sep = "_"))
    name_site <- switch (name,
                         "bc"="MF1",
                         "it"="MF2",
                         "cj"="MF3",
                         "Fsf"="MF4",
                         "Fbar"="MF5",
                         "bp"="MF6")
    result$site <- rep(name_site, length(result$Gen))

  }
  if (methods == "ind") {
    result=count (site, Gen, Spp,Fam,  sort =TRUE)
    name_site <- switch (name,
                         "bc"="MF1",
                         "it"="MF2",
                         "cj"="MF3",
                         "Fsf"="MF4",
                         "Fbar"="MF5",
                         "bp"="MF6")
    result$site <- rep(name_site, length(result$Gen))
    result$binom <- paste(result$Gen,
                          result$Spp,
                          sep= "_")

  }
  result
}

