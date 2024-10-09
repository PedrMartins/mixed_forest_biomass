
##############weighted avarege##########

meanp=function (v,w,pop,as_numeric=FALSE)	# v=variável					##
  #w=peso						                                            ##
  #pop=população					                                      ##
  #count-> caso esteja usando a função count para               ##
  #peso						                                              ##
{	if (class (v)!="numeric")								                      ##
{ stop("Variável não numérica")}						                    ##
  if (as_numeric==TRUE)									                        ##
  {w=as.numeric (w)}								                            ##
  if (class (w) !="numeric")								                    ##
  {stop ("Peso não numérico")}							                    ##
  if (length (v)!= length (w))								                  ##
  {stop("colunas não tem \n com tamananho diferente")}			  	##
  m_p=sum (v*w)/length (pop)                    								##
  return (m_p)									                                ##
  ##
}


###########import data function#############

import_biomass_rawdata <-  function(site = NULL){

  sites <- c("cj","bp","bc", "it", "fsf","fb")
  site <- match(site, sites)
  source <- c ()
  site <- source [site]
  site <- read.csv(site[1], row.names = 1,)
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
  site

}
