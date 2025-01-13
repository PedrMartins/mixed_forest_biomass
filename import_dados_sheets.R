#### carregando pacotes ####

source("Loadpackges.R")
source("Function_tratamento_pipae.R")

# Visualizar dados

#####################import data####################

pipae7 <-import_pipae ("pipae7")

#pipae8 <-  import_pipae ("pipae8")

pipae2 <-  import_pipae ("pipae2")

pipae1 <-  import_pipae ("pipae1")



###########sorting data co2 ###########

pipae1_co2 = separate_variable(pipae1, 
                               var= "co2",
                               na.rm = TRUE, 
                               duplicated = TRUE)

pipae2_co2 = separate_variable(pipae2, 
                               var= "co2",
                               na.rm = TRUE, 
                               duplicated = TRUE)

pipae7_co2 = separate_variable(pipae7, 
                               var= "co2",
                               na.rm = TRUE, 
                               duplicated = TRUE)

#pipae8_dia26$CO2 <- c(pipae8_dia26$CO2 +200)
pipae1_days_20_31 <- get_data_by_month(pipae1_co2,
                                          month=1, 
                                          days= c(9:13), 
                                          order = TRUE)

pipae2_days_20_31 <- get_data_by_month(pipae2_co2,
                                       month=1, 
                                       days= c(9:13), 
                                       order = TRUE)

pipae7_days_20_31 <- get_data_by_month(pipae7_co2,
                                       month=1, 
                                       days= c(9:13), 
                                       order = TRUE)


#write.table(pipae1_days_20_31, "pipae1_december_20_31.csv",
#          sep = "\t", row.names = FALSE, dec= ",")



colnames(pipae1_days_20_31)[1] <- "CO2"
colnames(pipae2_days_20_31)[1] <- "CO2"
colnames(pipae7_days_20_31)[1] <- "CO2"
pipae1_days_20_31 <- pipae1_days_20_31[!pipae1_days_20_31$CO2 ==0,] 
pipae2_days_20_31 <- pipae2_days_20_31[!pipae2_days_20_31$CO2 ==0,] 
pipae7_days_20_31 <- pipae7_days_20_31[!pipae7_days_20_31$CO2 ==0,] 

mean_pipae1_co2 = pipae1_days_20_31 |>
group_by(D) |>
summarise(mean_co2=mean(CO2, na.rm=TRUE))

mean_pipae2_co2 = pipae2_days_20_31 |>
  group_by(D) |>
  summarise(mean_co2=mean(CO2, na.rm=TRUE))

mean_pipae7_co2 = pipae7_days_20_31 |>
  group_by(D) |>
  summarise(mean_co2=mean(CO2, na.rm=TRUE))


#summarise(pipae1_days_20_31, by =CO2, mean)
############# sorting temperature ######



pipae1_temp = separate_variable(pipae1, 
                               var= "temperatura",
                               na.rm = TRUE, 
                               duplicated = TRUE)

pipae2_temp = separate_variable(pipae2, 
                               var= "temperatura",
                               na.rm = TRUE, 
                               duplicated = TRUE)

pipae7_temp = separate_variable(pipae7, 
                               var= "temperatura",
                               na.rm = TRUE, 
                               duplicated = TRUE)


#pipae8_dia26$CO2 <- c(pipae8_dia26$CO2 +200)
pipae1_days_20_31_temp <- get_data_by_month(pipae1_temp,
                                       month=1, 
                                       days= c(9:13), 
                                       order = TRUE)

pipae2_days_20_31_temp <- get_data_by_month(pipae2_temp,
                                       month=1, 
                                       days= c(9:13), 
                                       order = TRUE)

pipae7_days_20_31_temp <- get_data_by_month(pipae7_temp,
                                       month=1, 
                                       days= c(9:13), 
                                       order = TRUE)

mean_pipae7_temp = pipae7_days_20_31_temp |>
  group_by(D) |>
  summarise(mean_temp=mean(Temperature, na.rm=TRUE))

mean_pipae2_temp = pipae2_days_20_31_temp |>
  group_by(D) |>
  summarise(mean_temp=mean(Temperature, na.rm=TRUE))

mean_pipae1_temp = pipae1_days_20_31_temp |>
  group_by(D) |>
  summarise(mean_temp=mean(Temperature, na.rm=TRUE))

############# sorting moisture ######

pipae1_hum = separate_variable(pipae1, 
                                var= "umidade",
                                na.rm = TRUE, 
                                duplicated = TRUE)

pipae2_hum = separate_variable(pipae2, 
                                var= "umidade",
                                na.rm = TRUE, 
                                duplicated = TRUE)

pipae7_hum = separate_variable(pipae7, 
                                var= "umidade",
                                na.rm = TRUE, 
                                duplicated = TRUE)


#pipae8_dia26$CO2 <- c(pipae8_dia26$CO2 +200)
pipae1_days_20_31_hum <- get_data_by_month(pipae1_hum,
                                            month=1, 
                                            days= c(9:13), 
                                            order = TRUE)

pipae2_days_20_31_hum <- get_data_by_month(pipae2_hum,
                                            month=1, 
                                            days= c(9:13), 
                                            order = TRUE)

pipae7_days_20_31_hum <- get_data_by_month(pipae7_hum,
                                            month=1, 
                                            days= c(9:13), 
                                            order = TRUE)

mean_pipae7_umi = pipae7_days_20_31_hum |>
  group_by(D) |>
  summarise(mean_hum=mean(Humidity, na.rm=TRUE))

mean_pipae2_umi = pipae2_days_20_31_hum |>
  group_by(D) |>
  summarise(mean_hum=mean(Humidity, na.rm=TRUE))

mean_pipae1_umi = pipae1_days_20_31_hum |>
  group_by(D) |>
  summarise(mean_hum=mean(Humidity, na.rm=TRUE))





