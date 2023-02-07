library(tidyverse)
library(ggpubr)
library(rnaturalearth)
library(rnaturalearthdata)

##### Import messy data and cut out empty rows and resave ####
### Will only have to do this once ###
# setwd("C:/Users/kcn33/Box/Summer 2022")
# data = data.frame(read.csv("USGS_LOBS_StableIsotopeData_20210113.csv"))
# data
# toDelete <- seq(0, nrow(data), 2)
# data = data[ toDelete ,]
# # data
# data$Group = NA
# setwd("C:/Users/kcn33/Box/Ontario Isotopes")
# write.csv(data, file = "LOBS Isotope Dataset 20210113.csv")

##### Load in Data ####
setwd("C:/Users/kcn33/github/ON_Zoop_StableIsotopes/")
data = read.csv(file = "raw-data/LOBS Isotope Dataset 20210113.csv")
data$X = data$X/2

summary(data)
unique(data$sampleDescription)

# Load Map data
lakes10 = ne_download(scale = 10, type = "lakes", category = "physical",
                      returnclass = "sf")



##### MAPPING #####
mapofpoints = 
  ggplot(data = lakes10)+
  geom_sf()+
  geom_point(data = data, aes(x = longitude_decDeg, y = lattitude_decDeg), size = 1, 
             shape = 23, fill = "darkred")+
  coord_sf(xlim = c(-80, -75), ylim = c(42,45), expand = FALSE)

mapofpoints

#coord_sf(xlim = c-82), ylim = c(41,46), expand = FALSE)

##### Subsetting data ####

mysids = data %>%
  filter(sampleDescription == "Mysis diluviana") %>%
  as.data.frame()
mysids

zoops = data %>% 
  filter(Group == "Zooplankton") %>%
  as.data.frame()

#####



##### Plotting #####

ggplot(data = data, aes(x=year))+
  geom_bar()





mysis_CxN_plot = ggplot(mysids, aes(x = delta13C, y = delta15N)) +
  geom_point()
mysis_CxN_plot

ggplot(data, aes(x = delta13C, y = delta15N, color = (sampleDescription)))+
  geom_point()+
  xlim(-35,-10)

ggplot(data[data$sampleDescription == "POC (phyto)", ], aes(x = year, y = delta15N ))+
  geom_point()

ggplot(data[data$sampleDescription == "Deepwater sculpin", ], aes(x = delta15N, y = percentN/percentC ))+
  geom_point()


data %>%  
 # filter(sampleDescription == "Mysis diluviana" |
 #           sampleDescription == "Deepwater sculpin" |
 #           sampleDescription == "Calanoid copepod" |
 #           sampleDescription == "Cyclopoid copepod" |
 #           sampleDescription == "POC (phyto)") %>% 
  filter(year == 2013) %>%
  group_by(sampleDescription) %>% 
  #summarize(total_cases = sum(cases)) %>% 
  ggplot(aes(x = sampleDescription, y = delta15N)) +
  geom_boxplot()+
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5),
        text=element_text(size=10))

#### Mysis and sculpin/goby
mys_sculp = 
  data %>%  
  filter(sampleDescription == "Mysis diluviana" | 
           sampleDescription == "Deepwater sculpin" |
           sampleDescription == "Slimy sculpin" |
           sampleDescription == "Round goby") %>%
  group_by(sampleDescription) %>% 
  #summarize(total_cases = sum(cases)) %>% 
  ggplot(aes(x = sampleDescription, y = delta15N, group = sampleDescription, color = sampleDescription)) +
  geom_boxplot()+
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5),
        text=element_text(size=10))+
  facet_grid(cols = vars(year))

##### Dreissena and goby
dreissena_goby = 
  data %>%  
  filter(Group == "Dreissena" |
           sampleDescription == "Round goby") %>%
  group_by(sampleDescription) %>% 
  #summarize(total_cases = sum(cases)) %>% 
  ggplot(aes(x = sampleDescription, y = delta13C, group = sampleDescription, color = sampleDescription)) +
  geom_boxplot()+
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5),
        text=element_text(size=10))+
  facet_grid(cols = vars(year))


zooptopes = 
  ggplot(zoops, aes(delta13C, delta15N, color = sampleDescription))+
  geom_point()

mysis_CxN_plot+
  ggtitle("2013 Mysis")

mys_sculp+ggtitle("Mysis & Sculpin")

zooptopes+ggtitle("zoops")

sculp13C = 
  data %>%  
  filter(sampleDescription == "Deepwater sculpin") %>%
  group_by(sampleDescription) %>% 
  #summarize(total_cases = sum(cases)) %>% 
  ggplot(aes(x = sampleDescription, y = delta15N, group = sampleDescription, color = sampleDescription)) +
  geom_boxplot()+
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5),
        text=element_text(size=10))+
  facet_grid(cols = vars(year))
sculp13C






############### SAVE PLOTS TO FIGURES FOLDER ##############

ggsave(filename="Mysis_Sculpin Delta15N comparison.png",
       plot = mys_sculp,
       path = "C:/Users/kcn33/github/ON_Zoop_StableIsotopes/figures/")

ggsave(filename="Map of sample points.png",
       plot = mapofpoints,
       path = "C:/Users/kcn33/github/ON_Zoop_StableIsotopes/figures/")

