Estimating_higth_Ang <-  function (x){

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
