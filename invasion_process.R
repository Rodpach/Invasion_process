invasion_process  = function(layer, date_col, poligon_id_col, max_date_ymd = NA){
#'invasion_process' obtains data from a 'sf' multipolygon grided layer object that contains two columns: one of dates and the other containing ids.
#The dates of each subpolygon that constitute the grid represents the first record of an event (i.e.first invasion date of a species).
#'invasion_process' returns the input layer with data for each subpolygon constituting the grid, regarding their neighboring polygons with the event and the time (months) since it occur, relative to the focal polygon.

  #layer = a sf multipolygon grid object.
  #date_col = character vector of 'layer' attribute/column with dates of first invasion. data must be in date lubridate format. NA if not invaded.
  #poligon_id_col = character vector of 'layer' attribute/column with id.
  #max_date_ymd = date as character vector in yyyy/mm/dd format. Specify the last day of the invasion. If not specified, last day in 'date_col' will be used.
  
  library(dplyr)
  library(sf)
  library(lubridate)
  
  
  if(is.na(max_date_ymd)){ #If max
    max_date_ymd <- max(pull(layer, date_col), na.rm = T)
  } else{max_date_ymd = ymd(max_date_ymd)}
  
  
  
  layer_int <- st_intersects(layer)
  
  
  df <-  lapply(as.list(1:dim(layer)[1]), function(x){ #Obtain invasion data from each polygon and its adjacent polygons. returns a list of tibbles, one for each polygon.
    
    plot_adj <- layer[layer_int[[x]],] #Selects adjacent polygons.
    
    obj <- layer[x,] #Select focal polygon
    
    fecha_1inv <- pull(obj, date_col) #Select the first date of invasion in the focal polygon
    
    if(!is.na(fecha_1inv)){ #If the focal polygon is invaded
      neigh_prev <- filter(plot_adj, get(date_col) < fecha_1inv) %>% #obtain the date of the first invaded adjacent cell 
        pull(date_col)
      
      neigh_num <- length(neigh_prev) #number of adjacent with invaded polygons
      
      if(neigh_num != 0 ){ #if the number of adjacent invaded polygons is not zero, it calculates de interval in months of 
        neigh_1inv <- interval(min(neigh_prev), fecha_1inv) %/% months(1)
      } else{ #if the number of adjacent invaded polygons is zero, then it is 0.
        neigh_1inv <- 0 
      }
      
      invasion <- 1 #polygon defined as invaded
    } else{ #If the focal polygon is not invaded
      invasion <- 0 #polygon defined as not invaded
      
      inv_0 <- layer[subset(layer_int[[x]], layer_int[[x]] != x),] #Obtain adjacent polygons form focal polygon.
      
      neigh_prev <- na.omit(pull(inv_0, date_col)) #Obtain invaded adjacent polygons
      neigh_num <- length(neigh_prev) #Obtain number of invaded polygons.
      
      if(neigh_num != 0){  #if the number of adjacent invaded polygons is not zero, it calculates de interval in months of 
        neigh_1inv <- interval(min(na.omit(pull(plot_adj, date_col))), max_date_ymd) %/% months(1) 
      } else{ #if the number of adjacent invaded polygons is zero, then it is 0.
        neigh_1inv <- 0
      }
    }
    
    #return obtained data for each polygon.
    tibble(poligon_id_col = pull(obj, poligon_id_col), 
           period_months_first_invasion = neigh_1inv, 
           invaded_neighbors = neigh_num, 
           invaded = invasion)
  })
  
  #concatenate the list of obtained data from each polygon.
  df <- bind_rows(df)
  
  #Set column names as their ids
  colnames(df)[1] <- poligon_id_col
  
  #Join layer data with their invasion process.
  df <- suppressMessages(left_join(layer, df))
  
  return(df)
}
