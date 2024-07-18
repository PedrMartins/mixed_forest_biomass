gen="Myrsine"
spe="umbellata"
dens=0.602






x=list (cj=dads.ang.cj,bp=dads.ang.bp)
dimnames (x)
x$bp [,4]

View (x)

x$farb [x$farb$Gen == gen & x$farb$Spp == spe,]


res = list ()
	for (i in x ) 

	{
      	i [i$Gen == gen & i$Spp == spe,11] <- dens
	
	}
names (i)



# Criando os objetos com dados de exemplo
dados1 <- data.frame(A = c(1, 2, 3), B = c(4, 5, 6), C = c(7, 8, 9))
dados2 <- data.frame(A = c(10, 11, 12), B = c(13, 14, 15), C = c(16, 17, 18))

# Criando uma lista para armazenar os objetos de dados
lista_dados <- list(dados1, dados2)

# Indexando os dados em cada objeto
for (i in 1:length(lista_dados)) {
  # Indexando as colunas desejadas (por exemplo, colunas 1 e 3)
  dados_indexados <- lista_dados[[i]][, c(1, 3)]
  
  # Exibindo os dados indexados
  print(dados_indexados)
}
