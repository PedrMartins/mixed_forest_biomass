source("function_for_iNEXT.R")


library(iNEXT)
library(ggplot2)
library(gridExtra)



m= seq(1, 1000, by=50)
rarefacaoRichiness = iNEXT (all_sites, q=0, size = m)
rarefacaoShannon = iNEXT (all_sites, q=1, size = m)
rarefacaoSimpson = iNEXT (all_sites, q=2, size = m)

rarefacao$DataInfo
rarefacao$AsyEst
View (rarefacao$iNextEst$coverage_based)

gRichiness<- ggiNEXT (rarefacaoRichiness, type= 1) +
  theme_classic() +
  theme(legend.position="right") +
  ylab("Species richiness")

gShannon<- ggiNEXT (rarefacaoShannon, type= 1) +
  theme_classic() +
  theme(legend.position="none") +
  ylab("Species diversity H'")

gSimpson<- ggiNEXT (rarefacaoSimpson, type= 1) +
  theme_classic() +
  theme(legend.position="botom") +
  ylab("Species diversity 1/D")

?ggsave ()

