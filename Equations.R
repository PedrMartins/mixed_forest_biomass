
Estimating_higth_Ang <-  function (x){

  #weibull equation
  site <- x
  a=27.188
  b=0.091
  c.1=0.738
  D.1=site$DAP
  site$Alt.E= a*(1-exp(-b*(D.1^c.1)))
  site

}


Estimating_higth_Gim <-  function(x){
  site <- x
  a.g=25.9889
  b.g=19.9290
  g.D.1=site$DAP
  site$Alt.E = (1.3+a.g*exp(-(b.g/g.D.1)))
  site
}


computeAGB_gim <- function (x){

  site <- x
  z=0.4141 #conversion to dry biomass (Souza e Longhi, XXXX)
  a= 111.7988
  b= -15.5317
  c.2= 0.8544
  d = 0.0180
  g.D.1= c(site$DAP)
  g.H.1=c(site$Alt.E)
  site$biom= (z*(a+b*c(g.D.1)+c.2*c(g.D.1^2)+
                   d*((c(g.D.1^2))*g.H.1))) #alometric equation biomass (Sanqueta )
  site

}

computeAGB_palm <- function(x){

  site <- x
  a.p= -3.3488
  b.p= 2.7483
  D.p=site$DAP



  site$biom = exp(a.p+b.p*log(D.p))
  site
  }


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
