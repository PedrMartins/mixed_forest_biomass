source("function_for_iNEXT.R")

str (all_sites)
class(all_sites)
m= seq(1, 1000, by=50)
rarefacaoRichiness = iNEXT (all_sites, q=0, size = m)
rarefacaoShannon = iNEXT (all_sites, q=1, size = m)
rarefacaoSimpson = iNEXT (all_sites, q=2, size = m)

gRichiness<- ggiNEXT (rarefacaoRichiness, type= 1) +
  theme_classic() +
  theme(legend.position="right") +
  ylab("Species richiness")

gShannon<- ggiNEXT (rarefacaoShannon, type= 1) +
  theme_classic() +
  theme(legend.position="right") +
  ylab("Species diversity H'")

gSimpson<- ggiNEXT (rarefacaoSimpson, type= 1) +
  theme_classic() +
  theme(legend.position="right") +
  ylab("Species diversity 1/D")

ggsave ("rarefação_riqueza.jpg", gRichiness)
ggsave ("rarefação_Shannon.jpg", gShannon)
ggsave ("rarefação_Simpson.jpg", gSimpson)

