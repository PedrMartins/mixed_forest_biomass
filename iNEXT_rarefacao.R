source("function_for_iNEXT.R")
install.packages('devtools')


library(iNEXT)
library(ggplot2)
library(gridExtra)

str (all_sites)
class(all_sites)
all_sites <- as.data.frame(all_sites)
m= seq(1, 700, by=10)
rarefacaoRichiness = iNEXT (all_sites, q=0, size = m)
rarefacaoShannon = iNEXT (all_sites, q=1, size = m)
rarefacaoSimpson = iNEXT (all_sites, q=2, size = m)

rarefacao$DataInfo
rarefacao$AsyEst
View (rarefacao$iNextEst$coverage_based)

gRichiness<- ggiNEXT (rarefacaoRichiness, type= 1) +
  theme_classic() +
  theme(legend.position="bottom") +
  ylab("Species richiness")

gShannon<- ggiNEXT (rarefacaoShannon, type= 1) +
  theme_classic() +
  theme(legend.position="bottom") +
  ylab("Species diversity H'")

gSimpson<- ggiNEXT (rarefacaoSimpson, type= 1) +
  theme_classic() +
  theme(legend.position="bottom") +
  ylab("Species diversity 1/D")

grid.arrange(gRichiness, gShannon, gSimpson)


ggsave()

data(ant)
t <- round(seq(10, 500, length.out=20))
out3 <- iNEXT(ant$h500m, q=1, datatype="incidence_freq", size=t, se=FALSE)
out3$iNextEst

