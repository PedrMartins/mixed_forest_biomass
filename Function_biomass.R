
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
  site

}


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

vegan::vegdist


class_DBH_bio_ind <- function (x, choice = "ind", class = 10 ){

  site <-  x
  chioces <- c ("ind","bio")
  choice <- match(choice, choices)


clas_gim_cj.10<-dads.gim.cj [dads.gim.cj$DAP<10,] #DBH < 10
g.smal=length(clas_gim_cj.10$DAP)
clas_gim_cj.10.30<-dads.gim.cj [dads.gim.cj$DAP>=10 & dads.gim.cj$DAP<30,] #DBH >= 10 to <30
g.med=length(clas_gim_cj.10.30$DAP)
clas_gim_cj.30.50<-dads.gim.cj [dads.gim.cj$DAP>=30 & dads.gim.cj$DAP<50,] #DBH >= 30 to <50
g.lar=length(clas_gim_cj.30.50$DAP)
clas_gim_cj.50<-dads.gim.cj [dads.gim.cj$DAP>=50,] #DBH >= 50
g.x.larg=length(clas_gim_cj.50$DAP)

clas_ang_cj.10<-dads.ang.cj [dads.ang.cj$DAP<10,]
smal=length(clas_ang_cj.10$DAP)
clas_ang_cj.10.30<-dads.ang.cj [dads.ang.cj$DAP>=10 & dads.ang.cj$DAP<30,]
med=length(clas_ang_cj.10.30$DAP)
clas_ang_cj.30.50<-dads.ang.cj [dads.ang.cj$DAP>=30 & dads.ang.cj$DAP<50,]
lar=length(clas_ang_cj.30.50$DAP)
clas_ang_cj.50<-dads.ang.cj [dads.ang.cj$DAP>=50,]
x.lar=length(clas_ang_cj.50$DAP)
s.a=sum(smal,med,lar,x.lar)
s.g=sum(g.smal,g.med,g.lar,g.x.larg)
p.a=(c(smal,med,lar,x.lar)/s.a)*100
p.g=(c(g.smal,g.med,g.lar,g.x.larg)/s.g)*100
a.b.a=sum(dads.ang.cj$DAP)
a.b.g=sum(dads.gim.cj$DAP)
(c(a.b.a,a.b.g)/sum(a.b.a,a.b.g))*100
}
