source("function_for_iNEXT.R")


rarefacaoRichiness = iNEXT (all_sites, q = c(0,1,2))


R_E_sites_mixedforest_by_ha <-  ggiNEXT (rarefacaoRichiness, facet.var = "Order.q") +
  theme_minimal(base_family = "serif") +
  facet_wrap(~Order.q, scale = "free", labeller = as_labeller(c(
    "0" = "Species richness",
    "1" = "Shannon diversity",
    "2" = "Simpson diversity"))) +
  labs(
    x = "Sample Size",
    y = "Estimated Diversity"
  )

ggsave ("R_E_mixed_by_ha.jpg", R_E_sites_mixedforest_by_ha, width = 2000,
         units = "px")



###########indice diversidade######
All_sites_richness <- ChaoRichness(all_sites)
All_sites_Shannon<- ChaoShannon(all_sites)
All_sites_Simpson <- ChaoSimpson(all_sites)

