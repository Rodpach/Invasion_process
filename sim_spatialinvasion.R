sim_spatialinvasion = function(layer_template, poligon_id_col, sim_name, invasion_period, invasion_seed, growth_rate = NA, print_gif = F, path_gif = NA){
  # 'sim_spatialinvasion' uses a 'sf' multipolygon grided layer object to simulate random and permanent expansions of an event (i.e.first invasion date of a species) 
  #  to adjacent spaces, with an specific growth rate.
  
  # The simulations can start with a specific number of subpolygons with the event (invasion seed), a specific growth rate, and the periods in 
  #  which the simulated expansion occurs (number expansion steps).
  
  # The event can only expand form active event subpolygons to adjacent subpolygons without active event. Once the event is active in a subpolygon, it will be for the rest of the simulation.
  
  #  Only the number neighboring subpolygons with active event, with adjacent subpolygons without event (expansion frontier) count to calculate the expansion, given the expansion rate.
  
  # 'sim_spatialinvasion' returns a data.frame with eigh valued for each simulation period (step): number of simulation period, current subpolygons with active event at the star of period,
  #  number of subpolygons with active events in the expansion frontier, number of subpolygons available for invasion (adjacent to the invasion frontier), number of subpolygons
  #  to expand the event (specified by 'growth_rate'), total number of subpolygons with active event after expansion, specified simulation id, and starting seed.
  
  # By specifyng 'print_gif' = T, 'sim_spatialinvasion' can produce an event expansion animation gif using the 'magick' package. The package is only required to create the animations, not for the simulation  data. 
  
  #ARGUMENTS:
  # layer_template = a 'sf' multipolygon grided layer object with an attribute/column specifying an id for each subpolygon.
  # poligon_id_col = character vector of 'id' attribute/column.
  # sim_name = simulation 'id' name as a character vector. it is specified by the user.
  # invasion_period = number of steps to run the simulation.
  # invasion_seed = starting number of active subpolygons.
  # growth_rate = value of growth rate (i.e. a growth rate of 25% is specified as a value of 1.25)
  # print_gif = F, Specify as TRUE to obtain an animation of the simulated expansion. The 'magick' package is required. 
  # path_gif = NA, Specify the path of the folder to save the gif animation.
  
  library(sf)
  library(ggplot2)
  library(dplyr)
  
  if(is.na(growth_rate)){stop(print("Specify growth rate"))}
  
  if(print_gif & is.na(path_gif)){
    stop(print("Specify a path directory for your gif on 'path_gif'")) 
  }
  
  if(is.na(path_gif)){path_gif = getwd()}
  
  #
  
  invasion_seed_id = sample(pull(layer_template, poligon_id_col), invasion_seed, replace = F)  #Selects random subpolygons in which to start the event.
  
  template_simulated_invasion = layer_template
  
  template_simulated_invasion$invaded = ifelse(pull(template_simulated_invasion, poligon_id_col) %in% invasion_seed_id, 1, NA) #Seed the events on selected subpolygons
  
  remaining = as.numeric(table(is.na(template_simulated_invasion$invaded))[1]) #Number subpolygons without active event
  
  #data.frame with starting values at the starting period 'zero'.
  invasion_resume = data.frame(invasion_period =  0, invasion_n = invasion_seed, 
                               invasors = 0, available = NA, 
                               invasion_gain = 0, invasion_n2 = 0)
  
  
  if(print_gif){#Saves first plot/frame of the animated gif.
    library(magick)
    
    #ggplot  
    gg_inavsion = ggplot()+
      geom_sf(data = template_simulated_invasion, aes(fill = as.factor(invaded)), color = "black", size = 0.05)+
      labs(fill = paste(sim_name," simulation \n", "Period: ", 0, sep = ""))+
      scale_fill_manual(values = "black", labels = c("Invaded", "Not invaded"), na.value = "transparent")+
      theme_classic()+
      theme(legend.position = "top", legend.key.size = unit(0.15, "cm"), legend.title.align = 0.5,
            legend.title = element_text(size = 4), legend.text = element_text(size = 3),
            text = element_text(size = 3))
    
    ggsave(plot = gg_inavsion, filename = paste(tempdir(), "/", sim_name, "_", 0, ".jpg", sep = ""), width = 6, height = 6, units = "cm")
    
    #gif
    gif_full = image_read(paste(tempdir(), "/", sim_name, "_", 0, ".jpg", sep = ""))
  }
  
  
  #Expansion loop. One loop for each step period.
  for(i in 1:invasion_period){
    
    invaded_grid = filter(template_simulated_invasion, !is.na(invaded)) #subpolygons with active event.
    noninvaded_grid = filter(template_simulated_invasion, is.na(invaded)) #subpolygons without active event.
    
    #Obtains number of neighboring subpolygons without active event, adjacent to subpolygons with active event.
    template_simulated_invasion_intersect = st_intersects(noninvaded_grid, 
                                                          invaded_grid, sparse = F)
    
    
    template_simulated_invasion_intersect = apply(template_simulated_invasion_intersect, 1, function(x){ #logical vector to filter intersect data.
      any(x)
    })
    
    template_simulated_invasion_adjacente = filter(noninvaded_grid, template_simulated_invasion_intersect, is.na(invaded)) # filters intersected subpolygons.
    
    # Obtains the number of subpolygons with active event at the expansion frontier, adjacent to subpolygons without active event.
    template_simulated_invasion_intersect_invadidos = st_intersects(invaded_grid,
                                                                    noninvaded_grid,
                                                                    sparse = F) #obtenicion de cuadros invadisos adjacentes y cuadrso no invadisos.
    template_simulated_invasion_intersect_invadidos = apply(template_simulated_invasion_intersect_invadidos, 1, function(x){ #logical vector to filter intersect data.
      any(x)
    })
    
    invaded_grid_adjacent = filter(invaded_grid, template_simulated_invasion_intersect_invadidos) #filters intersected subpolygons.
    #
    
    template_simulated_invasion_invadido_n = dim(invaded_grid)[1] #Total number of subpolygons with active event at star of period.
    template_simulated_invasion_invadido_n_adj = dim(invaded_grid_adjacent)[1] #Number of edjacent subpolygons with active event, adjacent to subpolygons without active event.
    template_simulated_invasion_invadido_n_ganado = ceiling(dim(invaded_grid_adjacent)[1]*(growth_rate-1)) #Calculate the number of new invaded subpolygons, based on 'layer_template_invadido_n_adj' * (growth_rate-1)
    template_simulated_invasion_invadido_n_n2 = template_simulated_invasion_invadido_n+template_simulated_invasion_invadido_n_ganado #Update number of total subpolygons with active event.
    
    
    #Two invasion options. The first option occurs If the number new active events is greater that the number of subpolygons without the event, available for expansion. 
    # the second option is the opposite: more available subpolygons than the number of new active events.
    
    #1
    if(dim(template_simulated_invasion_adjacente)[1] < template_simulated_invasion_invadido_n_ganado){ 
      template_new_invaded_fid = sample(pull(template_simulated_invasion_adjacente, poligon_id_col), template_simulated_invasion_invadido_n_ganado, replace = T) #Randomly selects subpolygons to update as active events with replacement
      
      template_simulated_invasion$invaded = ifelse(pull(template_simulated_invasion, poligon_id_col) %in% unique(template_new_invaded_fid) | 
                                                     template_simulated_invasion$invaded == 1, 1, NA) #updates new subpolygons as active events
    } else{
      template_new_invaded_fid = sample(pull(template_simulated_invasion_adjacente, poligon_id_col), template_simulated_invasion_invadido_n_ganado, replace = F) #Randomly selects subpolygons to update as active events.
      
      template_simulated_invasion$invaded = ifelse(pull(template_simulated_invasion, poligon_id_col) %in% template_new_invaded_fid | 
                                                     template_simulated_invasion$invaded == 1, 1, NA) #updates new subpolygons as active events
    }
    
    #DATA.FRAME UPDATE with new value, given the processed period.
    invasion_resume[i+1,1] = i
    invasion_resume[i+1,2] = template_simulated_invasion_invadido_n
    invasion_resume[i+1,3] = template_simulated_invasion_invadido_n_adj
    invasion_resume[i+1,4] = dim(template_simulated_invasion_adjacente)[1]
    invasion_resume[i+1,5] = template_simulated_invasion_invadido_n_ganado
    invasion_resume[i+1,6] = template_simulated_invasion_invadido_n_n2
    
    
    #GIFMAKE
    if(print_gif){
      
      #ggplot  
      gg_inavsion = ggplot()+
        geom_sf(data = template_simulated_invasion, aes(fill = as.factor(invaded)), color = "black", size = 0.05)+
        labs(fill = paste(sim_name," simulation \n", "Period: ", i, sep = ""))+
        scale_fill_manual(values = "black", labels = c("Invaded", "Not invaded"), na.value = "transparent")+
        theme_classic()+
        theme(legend.position = "top", legend.key.size = unit(0.15, "cm"), legend.title.align = 0.5,
              legend.title = element_text(size = 4), legend.text = element_text(size = 3),
              text = element_text(size = 3))
      
      ggsave(plot = gg_inavsion, filename = paste(tempdir(), "/", sim_name, "_", i, ".jpg", sep = ""), width = 6, height = 6, units = "cm")
      
      #gif
      gif_add = image_read(paste(tempdir(), "/", sim_name, "_", i, ".jpg", sep = ""))
      
      gif_full = c(gif_full, gif_add)
    }
  }
  
  if(print_gif){ #PRINT GIF
    animation = image_animate(gif_full, fps = 5, dispose = "previous")
    image_write(animation, paste(path_gif, "/", "simulated_invasion_", sim_name, ".gif", sep = ""))
    file.remove(list.files(tempdir(), pattern = sim_name,  full.names = T))
  }
  
  #Updates data.frame with simulation name and the invasion seed.
  invasion_resume$sim_name = sim_name
  invasion_resume$seed_n = invasion_seed
  
  return(invasion_resume)
}
