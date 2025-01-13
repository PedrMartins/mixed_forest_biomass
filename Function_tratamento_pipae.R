############Função testar planilhas#####################


t_dpi <- function(co2 = NULL, tem = NULL, umi = NULL, pre = NULL, na.rm = FALSE, ...) {
  
  data_list <- list(co2 = co2, tem = tem, umi = umi, pre = pre)
  
  if (na.rm) {
    data_list <- lapply(data_list, na.omit)
  } else if (any(sapply(data_list, anyNA))) {
    warning("have NA data in your data.frame \n use na.rm=TRUE")
  }
  
  ranges <- lapply(data_list, function(x) if (!is.null(x)) range(x, na.rm = TRUE) else c(NA, NA))
  means <- sapply(data_list, function(x) if (!is.null(x)) mean(x, na.rm = TRUE) else NA)
  
  output <- tibble(
    var = c("CO2", "Temp ºC", "Umid %", "Pressao"),
    rmin = sapply(ranges, function(x) x[1]),
    rmax = sapply(ranges, function(x) x[2]),
    mean= means
  )
  # function (x)
  # Remove rows with NA in rmin or rmax
  output <- output[complete.cases(output), ]
  
  return(output)
}



rm_sp_if_n <- function(x) {
  str_replace(x, "^(\\d+)\\s+(\\d+)", "\\1\\2")


#Explicação do código:
#  
#  ^(\\d)\\s+(\\d): Este regex corresponde a:
#  ^: Início da string.
#(\\d): Captura o primeiro caractere se for um dígito.
#\\s+: Captura um ou mais espaços.
#(\\d): Captura o segundo caractere se for um dígito.
#\\1\\2: Substitui a string correspondente com o primeiro e
#segundo dígitos, removendo o espaço entre eles.

}


import_pipae <-  function(pipae, concate=FALSE){
  
  pipae_all = data.frame()
  source <- c ("pipae7"="https://docs.google.com/spreadsheets/d/e/2PACX-1vTdOc4PMg1xC0qpUceE6BZV8L1oLn8D5zf-dALqqWiEQZBFJH23dzPiqwn7NOlFowHEis1N4eb7JvFZ/pub?output=csv"
               ,"pipae8"="https://docs.google.com/spreadsheets/d/e/2PACX-1vRezDFvNifmWuJUoVIIhyazBaD281lsr4qeV3EWROGEUH8CDBD01riMOWMfDbsPH0Z8wFkEmtQRbfEC/pub?output=csv"
               ,"pipae1" = "https://docs.google.com/spreadsheets/d/e/2PACX-1vQmbdkhDjts41TXdkpoghubvLQ_5waEms3_RSUXBa_JBg1Z0o2AEnjRpWlTe6lv8go3nouimqUEeklI/pub?output=csv"
               ,"pipae2" = "https://docs.google.com/spreadsheets/d/e/2PACX-1vQZb9G0b6kE_I91HK7KOtIA7XJ_-OpW6I4J4ibTU4v1ljzAlgplWdzLRpsMiLqr6reiV6Jol3yrvOkE/pub?output=csv"
  )
  
  #sensores <- paste("pipae",seq(1, length(source)), sep="")
  sensores <- paste("pipae", c(7,8,1,2), sep="")
  if (is.null(pipae)==TRUE) {stop ("inclua sensores")}
  
  if (length(setdiff(pipae,sensores))!= 0) {
    stop("não há sensores com essa tag ou há erros na tag",
         call. = FALSE)
  }
  
  pipaes <- match(pipae, sensores)
  
  if (concate==TRUE){
    
    
    
    for (pipae in pipaes){
      site <-  source[pipae]
      site <- read.csv(site [1], dec= ",")
      
      
      if (names(source[pipae]) %in%
          c(paste("pipae", 3, sep="")) == # saber sensores de cada parcela
          TRUE ){
        site$tag = rep (names(source[pipae]),
                        length(site$CO2))
        site$parcela = rep ("par2", length(site$CO2))
      } else if (names(source[pipae]) %in%
                 c(paste("pipae", 7,sep="")) ==
                 TRUE ){
        site$tag = rep (names(source[pipae]),
                        length(site$CO2))
        site$parcela = rep ("par1", length(site$CO2))
      }
      pipae_all <- rbind(pipae_all, site)
      
    }
    
    pipae_all$Data = dmy (pipae_all$Data)
    pipae_all$Hora = hms (pipae_all$Hora)
    pipae_all= pipae_all|>
      mutate(H=hour(pipae_all$Hora),
             D=day(pipae_all$Data),
             M= month(pipae_all$Data),
             Y= year(pipae_all$Data),
             m=minute(pipae_all$Hora))
    
    return (pipae_all)
  }
  
  
  pipae <- source[pipaes]
  site <- read.csv(pipae, dec= ",")
  names= colnames(site)
  
  if ("Date" %in% names == FALSE){
    colnames(site)[c(7,8)] <-  c("Date", "Time")
  }
  
  site$Date = dmy (site$Date)
  site$Time = hms (site$Time)
  site= site|>
    mutate(H=hour(site$Time),
           D=day(site$Date),
           M= month(site$Date),
           Y= year(site$Date),
           m=minute(site$Time))
  site$DateTime <- as.POSIXct(
    paste(site$Date, site$Time),
    format = "%Y-%m-%d %HH %MM %SS")
  
 return(site)
  
}





get_mean_by_day = function (x, 
                            variavel = "co2",
                            day =NULL, month= NULL, 
                            year =NULL) {
  
  
  days_p=sort (unique (day(x$Date)))
  months_p=sort (unique (month(x$Date)))
  years_p=sort (unique (year(x$Date)))
  
  day= match(day,days_p)
  day=days_p[day]
  month= match(month,months_p)
  month=months_p[month]
  year= match(year,years_p)
  year=years_p[year]
  
  x_day = x [x$D == day & x$M == month & x$Y == year,]
  names = colnames(x_day)
  teste= colSums (x_day [,c(1,2)], na.rm = TRUE)
  
  if ("GNSS.Date" %in% names == TRUE) {colnames(x_day)[c(1,3,4)] <- c("Temperatura", "Umidade","CO2")}
  if (teste [1] == 0){stop("não há coleta para data inserida") }
  
  var <- intersect(c ("co2","temperatura","umidade"), variavel)
  
  if (var == "co2"){
  medco2_x = x_day |>
    group_by(H) |>
    summarise( medc= mean(CO2, na.rm = TRUE))
  
  medco2_x=medco2_x|>
    left_join(x_day,medco2_x, by ="H")
  med_x <- medco2_x [!duplicated(medco2_x$H),]
  
  
  
  
  
  } else if (var == "temperatura") {
    medtemp_x = x_day |>
    group_by(H) |>
    summarise( medtemp= mean(Temperatura, na.rm = TRUE))
  
    medtemp_x=medtemp_x|>
    left_join(x_day,medtemp_x, by ="H")
    med_x  <- medtemp_x [!duplicated(medtemp_x$H),]
    
  } else {
    medumi_x = x_day |>
      group_by(H) |>
      summarise( medumi= mean(Umidade, na.rm = TRUE))
    
    medumi_x=medumi_x|>
      left_join(x_day,medumi_x, by ="H")
    med_x  <- medumi_x [!duplicated(medumi_x$H),]
    
  }
  
  if ("GNSS.Date" %in% names == TRUE) {
    excluir <- c("CO2", "Luminosity", "UV.Intensity",     
                  "UV.Index", "Latitude", "Longitude",        
                  "Speed" , "Altitude",  "GNSS.Time" ,       
                  "GNSS.Date","Temperatura" , "Umidade","Pressure")
    med_x <- med_x[,!(names(med_x)%in% excluir)]
    return(med_x)
  } else { 
      excluir <- c("UV...","Luminosidade...","CO2", 
               "Temperatura" , "Umidade","Pressão")
      med_x <- med_x[,!(names(med_x)%in% excluir)]
      return(med_x )
}  
  
  
  
  
  }


 


separate_variable <- function(x, variable = "co2", na.rm = FALSE, 
                              order=FALSE, duplicated= FALSE) {
  # Definindo um vetor nomeado que mapeia variáveis às suas colunas
  column_map <- list(
    co2 = c("CO2.Concentration", "Date", "DateTime", "Time","H","D", "M", "Y", "m"),
    temperatura = c("Temperature", "Date", "DateTime", "Time", "H","D", "M", "Y", "m"),
    umidade = c("Humidity", "Date", "DateTime", "Time", "H","D", "M", "Y", "m"),
    luminosidade = c("Luminosity", "Date", "DateTime", "Time","H", "D", "M", "Y", "m")
  )
  
  # Verifica se a variável está no mapa, caso contrário, retorna NULL
  selected_columns <- column_map[[variable]]
  
  if (is.null(selected_columns)) {
    stop("Variable not recognized.
         Please choose from 'co2', 
         'temperatura', 'umidade', or
         'luminosidade'.")
  }
  
  # Subconjunto dos dados com as colunas selecionadas
  x <- x[, selected_columns, drop = FALSE]
  
  # Remover valores NA, se solicitado
  if (na.rm) { x <- na.omit(x) }
  if (order){x <-  x[order(x$DateTime),]}
  if (duplicated) {x <- x [!duplicated(x$DateTime),]}
  return (x)
}

get_data_by_month <- function(x, month=NULL, 
                              days= NULL, order=TRUE){
  if (is.null(month)) {
    stop("choise a month")
  }
  if (is.null(days)) {
    x <- x[x$M==month,]
    if (order){x <-  x[order(x$DateTime),]}
    return(x)
  }else {
    x <- x[x$M==month,]
    pipae_month <- data.frame()
    for (i in days) {
      pipae_dia <- x[x$D==i,]
      pipae_month <- rbind(pipae_dia, pipae_month)
    }
    if (order){pipae_month <-  pipae_month[
                                  order(pipae_month$DateTime),]}
    return(pipae_month)
  }
  
  
}




