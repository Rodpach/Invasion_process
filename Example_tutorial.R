#EXAMPLE
library(sf)
library(lubridate)
library(dplyr)
#Invasion process
Svulg_30x30 = st_read("ES_30x30_mex.shp")
Svulg_30x30_ip = invasion_process(layer = Svulg_30x30, date_col = "frst_nv", 
                                  poligon_id_col = "FID", max_date_ymd = "2022-03-01")


Svulg_invasion_obsrate = Svulg_30x30[,1:2] 
st_geometry(Svulg_invasion_obsrate) = NULL
Svulg_invasion_obsrate$year = year(Svulg_invasion_obsrate$frst_nv)
Svulg_invasion_obsrate = filter(Svulg_invasion_obsrate, !is.na(year)) |> 
  count(year) |> mutate(n_cum = cumsum(n))                      


# simulation
Svulg_30x30_sim = Svulg_30x30[, 1] #Remove invasion values to obtain a clean layer template.

Svulg_invasion_obsrate_period = range(Svulg_invasion_obsrate$year)
Svulg_invasion_obsrate_period = Svulg_invasion_obsrate_period[2]-Svulg_invasion_obsrate_period[1]

Svulg_30x30_sim_df = sim_spatialinvasion(layer_template = Svulg_30x30_sim, poligon_id_col = "FID", sim_name = "Svulg_sim",
                                     invasion_period = Svulg_invasion_obsrate_period, invasion_seed = 2, 
                                     growth_rate = ((444-1) ^ (1 / Svulg_invasion_obsrate_period)), 
                                     print_gif = T, path_gif = getwd())


#contrast growth rates

Svulg_invasion_obsrate = left_join(Svulg_invasion_obsrate, data.frame(year = 1953:2021, invasion_period = 1:length(1953:2021)))


ggplot(Svulg_invasion_obsrate)+
  geom_line(aes(invasion_period, n_cum, color = "Observed"))+
  geom_line(data = Svulg_30x30_sim_df, aes(invasion_period, invasion_n2, color = "Simulated"))+
  scale_color_manual(values = c("red", "black"))+
  labs(x = "Period", y = "Grid-cells")+
  theme_classic()+
  theme(legend.position = c(0.12, .85), legend.title = element_blank())
  