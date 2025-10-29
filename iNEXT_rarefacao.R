source("function_for_iNEXT.R")


rarefacaoRichiness = iNEXT (all_sites, q = c(0,1,2))


R_E_sites_mixedforest_by_ha <-  ggiNEXT (rarefacaoRichiness, facet.var = "Order.q") +
  theme_minimal() +
  facet_wrap(~Order.q, scale = "free", labeller = as_labeller(c(
    "0" = "Species richness",
    "1" = "Shannon diversity",
    "2" = "Simpson diversity"))) +
  labs(
    title = "Rarefaction/Extrapolation Curves by Diversity Order",
    x = "Sample Size",
    y = "Estimated Diversity"
  )

  theme(
    strip.text = element_text(size = 12, face = "bold"),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
  )
ggsave ("rarefação_Simpson.jpg", gSimpson)



###########indice diversidade######
All_sites_richness <- ChaoRichness(all_sites)
All_sites_Shannon<- ChaoShannon(all_sites)
All_sites_Simpson <- ChaoSimpson(all_sites)

