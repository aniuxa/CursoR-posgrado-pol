# ==============================================================================
# Fecha: 2020-08-28 
# Sesión 6 - No tenemos libreta
# Autoras: Ana Escoto y Mónica Lara
# ==============================================================================


# Paquetes                        ----------------------------------------------
if (!require("pacman")) install.packages("pacman") # instala pacman si se requiere
pacman::p_load(tidyverse, readxl,
               haven, sjlabelled, 
               foreign, janitor, srvyr,
               esquisse, RColorBrewer,
               devtools) #carga los paquetes necesarios


# Un paquete en desarrollo        ----------------------------------------------

#if (!require("devtools")) {
#  install.packages("devtools")
# }
#devtools::install_github("diegovalle/mxmaps")

library(mxmaps)

# Datos                   -----------------------------------------------

# índice de competitividad estatal (no internacional)
url <- "https://github.com/aniuxa/CursoR-posgrado-pol/raw/master/datos/ICE_2018.xlsx"
destfile <- "ICE_2018.xlsx"
curl::curl_download(url, destfile)

ICE_2018 <- read_excel(destfile, sheet = "para_importar")
ICE_2018 <- clean_names(ICE_2018) # limpia los nombres

# Gráficos multivariados  -----------------------------------------------

#Iniciamos con un ggplot "scatter"

ICE_2018 %>% 
  ggplot(aes(homicidios,percepcion_de_seguridad)) +
  geom_point()

# geometría "jitter"

ICE_2018 %>% 
  ggplot(aes(homicidios,percepcion_de_seguridad)) +
  geom_jitter()

# geometría "text"

ICE_2018 %>% 
  ggplot(aes(homicidios,percepcion_de_seguridad)) +
  geom_text(aes(label=edo2))


# geometría "label"

ICE_2018 %>% 
  ggplot(aes(homicidios,percepcion_de_seguridad)) +
  geom_label(aes(label=edo2))


# Introducción de una tercera variable con color

ICE_2018 %>% 
  ggplot(aes(x=homicidios,
             y=percepcion_de_seguridad,
             color=region2)
         ) +
  geom_point()

# Introducción de una tercera variable con "shape"

ICE_2018 %>% 
  ggplot(aes(x=homicidios,
             y=percepcion_de_seguridad,
             shape=region2)
  ) +
  geom_point() # ojo, nos da un "warning"


# Introducción de una tercera variable con "facet"

ICE_2018 %>% 
  ggplot(aes(x=homicidios,
             y=percepcion_de_seguridad)) +
  geom_point() + facet_wrap(~region2)


ICE_2018 %>% 
  ggplot(aes(x=homicidios,
             y=percepcion_de_seguridad)) +
  geom_point() + facet_grid(.~region2)


ICE_2018 %>% 
  ggplot(aes(x=homicidios,
             y=percepcion_de_seguridad)) +
  geom_point() +
  facet_grid(region2~.)

# Smooth y múltiples geometrías

ICE_2018 %>% 
  ggplot(aes(x=homicidios,
             y=percepcion_de_seguridad)) +
  geom_point() +
  geom_smooth(method="lm") +
  facet_grid(region2~.)


ICE_2018 %>% 
  ggplot(aes(x=homicidios,
             y=percepcion_de_seguridad,
             color=region2)) +
  geom_text(aes(label=edo2)) +
  geom_smooth(method="lm") + scale_fill_brewer(palette = "Dark2") +
  theme_minimal()

# Introducción de una cuarta variable cuanti
  
ICE_2018 %>% 
  ggplot(aes(x=homicidios,
             y=percepcion_de_seguridad,
             color=region2)) +
  geom_point(aes(size=costos_del_delito))+ # ojo
  theme_minimal()

# Equivalente  

ICE_2018 %>% 
  ggplot(aes(x=homicidios,
             y=percepcion_de_seguridad,
             color=region2,
             size=costos_del_delito)) +
  geom_point()+ 
  theme_minimal()

# Agregaremos etiquetas

ICE_2018 %>% 
  ggplot(aes(x=homicidios,
             y=percepcion_de_seguridad,
             color=region2,
             size=costos_del_delito)) +
  geom_text(aes(label=edo2),
            check_overlap = TRUE)+
  theme_minimal()


# Mapas                   -----------------------------------------------

data("df_mxstate") # carga la base estatal del paquete
glimpse(df_mxstate)


# Cómo opera el paquete
df_mxstate$value <- df_mxstate$pop # paso esencial

glimpse(df_mxstate$region)


mxstate_choropleth(df_mxstate,
                   title = "Total población, por Estado") 

# Mientras haya una variable de region estatal
ICE_2018$value<- ICE_2018$homicidios

mxstate_choropleth(ICE_2018,
                   title = "Tasa de homicidios") 

mapa<-mxstate_choropleth(ICE_2018,
                         title = "Tasa de homicidios") 

ggsave(plot=mapa, #objeto donde está el gráfico
       device="png", # formato del gráfico
       filename = "mapa.png") # nombre el archivo de salida

# lo guardará en nuestro directorio y en formato ".png" de imagen
