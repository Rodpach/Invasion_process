invasion_process  = function(layer, date_col, poligon_id_col, max_date_ymd = NA){
  
  require(dplyr)
  require(sf)
  require(lubridate)
  require(tibble)
  
  
  if(is.na(max_date_ymd)){
    max_date_ymd <- max(dplyr::pull(layer,"first_invasion"), na.rm = T)
  } else{max_date_ymd = lubridate::ymd(max_date_ymd)}
  
  
  
  layer_int <- st_intersects(layer)
  
  
  df =  lapply(as.list(1:dim(layer)[1]), function(x){
    
    plot_adj <- layer[layer_int[[x]],] #Seleccionamos los municips adjacentes
    
    obj <- layer[x,] #Seleccionamos el poligono objetivo
    
    fecha_1inv <- dplyr::pull(obj, date_col) #Seleccionamos la fecha de primera invasion en el municipio objetivo
    
    if(!is.na(fecha_1inv)){
      neigh_prev <- dplyr::pull(dplyr::filter(plot_adj, get(date_col) < fecha_1inv), date_col)
      neigh_num <- length(neigh_prev)
      
      if(neigh_num != 0 ){
        neigh_1inv <- interval(min(neigh_prev), fecha_1inv) %/% months(1)
      } else{
        neigh_1inv <- 0
      }
      
      invasion <- 1
    } else{
      invasion <- 0
      
      inv_0 <- layer[-x,]
      
      inv_0 <- layer[subset(layer_int[[x]], layer_int[[x]] != x),]
      
      neigh_prev <- na.omit(dplyr::pull(inv_0, date_col))
      neigh_num <- length(neigh_prev)
      
      if(neigh_num != 0){
        neigh_1inv <- interval(min(na.omit(dplyr::pull(plot_adj, date_col))), max_date_ymd) %/% months(1)
      } else{
        neigh_1inv <- 0
      }
    }
    
    tibble::tibble(poligon_id_col = pull(obj, poligon_id_col), period_months_first_invasion = neigh_1inv, invaded_neighbors = neigh_num, invaded = invasion)
  })
  
  df <- dplyr::bind_rows(df)
  
  colnames(df)[1] <- poligon_id_col
  
  
  df <- suppressMessages(dplyr::left_join(layer, df))
  
  return(df)
}