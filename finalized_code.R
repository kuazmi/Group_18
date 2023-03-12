#-----LOAD PACKAGES----
library(tidyverse)
library(ggmap)
library(RColorBrewer)
library(mgcv)
#Set your API Key
ggmap::register_google(key = "AIzaSyAtqbh8XHPYxxub1WKwDaO2JXTtu95Lobw")
library(tidyverse)
#install.packages("ggmap")

# for maps
library(maps) 
#install.packages("rgdal")
library(rgdal)
#install.packages("maptools")
library(maptools) 

#---LOAD DATASET----
load("/Users/kumi/Documents/Education/group project/datasets_project.RData")
#----PLOT MAP-----
plot.map(TBdata$TB[TBdata$Year==2014],n.levels=7,main="TB counts for 2014")
#----SELECTED MODEL-----
nb_gam <-gam(TB ~ s(lon,lat,k=480) + s(Urbanisation) +
                s(Density, k=20) + s(Poverty) + 
                s(Poor_Sanitation, k=30) + 
                s(Unemployment) + s(Timeliness) ,
              offset = log(Population),
              family = nb(),
              data = TBdata)

#-----MODEL CHECKING----

# 1. model summary
summary(nb_gam)

# 2. AIC()
AIC(nb_gam,nb_gam4) # 12419.37 LOWER THAN nb_gam4

# 3. k.check()
k.check(nb_gam)

# 4. RESIDUAL PLOTS
gam.check(nb_gam)

# 5. visualize the non-linear interaction effect 
plot(nb_gam, page= 1,scheme = 2)
plot(nb_gam,scheme = 2)

# 6. interaction plot
vis.gam(x = nb_gam,                # GAM object
        view = c("Urbanisation","Density"),   # variables
        plot.type = "persp")    # kind of plot 



