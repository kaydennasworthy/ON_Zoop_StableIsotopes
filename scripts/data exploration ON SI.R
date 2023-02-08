library(tidyverse)
library(ggpubr)
library(rnaturalearth)
library(rnaturalearthdata)
library(stringr)
library(ggforce)
library(lubridate)

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

# Edit Dates to be usable
data$dateCollected = mdy(data$dateCollected)

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

limnos = zoops %>%
  filter(sampleDescription == "Limnocalanus macrurus") %>%
  as.data.frame()

zoops_nohemi = zoops %>%
  filter(sampleDescription != "Hemimysis anomala") %>%
  filter(sampleDescription != "Hemimysis anomala-adult") %>%
  filter(sampleDescription != "Hemimysis anomala-juvenile") %>%
  as.data.frame()

only2013 = data %>% 
  filter(year == 2013) %>%
  as.data.frame()




#####

##### Plotting #####
mysis_CxN_plot = ggplot(mysids, aes(x = delta13C, y = delta15N)) +
  geom_point()
mysis_CxN_plot

ggplot(data[data$sampleDescription == "POC (phyto)", ], aes(x = year, y = delta15N ))+
  geom_point()

ggplot(data[data$sampleDescription == "Deepwater sculpin", ], aes(x = delta15N, y = percentN/percentC ))+
  geom_point()

zooptopes = 
  ggplot(zoops, aes(delta13C, delta15N, color = sampleDescription))+
  geom_boxplot()
zooptopes

zooptopes_box_15N = 
  ggplot(zoops_nohemi, aes(x = sampleDescription, y= delta15N))+
  geom_boxplot()+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))
zooptopes_box_15N

zooptopes_box_13C = 
  ggplot(zoops_nohemi, aes(x = sampleDescription, y= delta13C))+
  geom_boxplot()+
scale_x_discrete(labels = function(x) str_wrap(x, width = 8))
zooptopes_box_13C






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

dat = zoops_nohemi %>%
  filter(sampleDescription == "Mysis diluviana" |
         sampleDescription == "Limnocalanus macrurus" |
           sampleDescription == "Bythotrephes longimanus") %>%
  filter(year == 2013) %>%
  as.data.frame()

dat


zoop_CxN_plot = 
  ggplot(dat, aes(x = delta13C, y = delta15N, 
    fill = sampleDescription, color = sampleDescription,
    linetype = sampleDescription))+
  geom_mark_hull(concavity = 5,expand=0,radius=0)+
  geom_point(size = 4)#  stat_ellipse(type = "t", geom = "polygon")

zoop_CxN_plot



mys_15N_through_year = 
  ggplot(mysids, aes(x = dateCollected, y = delta15N))+
  geom_point()+
  geom_smooth(method = "lm")+
  stat_regline_equation(label.x = 15850, label.y = 18)+
  stat_cor(aes(label=..rr.label..), label.x=15850, label.y=17.5)
mys_15N_through_year

mys_13C_through_year = 
  ggplot(mysids, aes(x = dateCollected, y = delta13C))+
  geom_point()+
  geom_smooth(method = "lm")+
  stat_regline_equation(label.x = 15850, label.y = -21.5)+
  stat_cor(aes(label=..rr.label..), label.x=15850, label.y=-22)
mys_13C_through_year



lm(delta15N ~ dateCollected, mysids)


ggplot(limnos, aes(x = dateCollected, y = delta15N))+
  geom_point()+
  geom_smooth(method = "lm")

ggplot(only2013[only2013$sampleDescription == "Bosminidae",], aes(x = dateCollected, y = delta15N))+
  geom_point()


############### SAVE PLOTS TO FIGURES FOLDER ##############

ggsave(filename="Mysis_Sculpin Delta15N comparison.png",
       plot = mys_sculp,
       path = "C:/Users/kcn33/github/ON_Zoop_StableIsotopes/figures/")

ggsave(filename="Map of sample points.png",
       plot = mapofpoints,
       path = "C:/Users/kcn33/github/ON_Zoop_StableIsotopes/figures/")

