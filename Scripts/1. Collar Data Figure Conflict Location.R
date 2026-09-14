####Figure 1- Collar data
library(dplyr)
library(ggplot2)
library(sf)
library(ggmap)
library(rnaturalearth)
library(cowplot)
library(adehabitatHR)

#Start by generating stadiamap key. Follow this link: "https://docs.stadiamaps.com/authentication/." 
#Specific API key generated for this map by the contributors has been removed. 


#read key
Sys.setenv(GGMAP_STADIA_API_KEY = "replace with your API KEY")
register_stadiamaps("replace with your API KEY", write = FALSE)



#Define coordinate of killing
#banksia green -32.5479512004568, 152.30759963706402
#Sydney -33.874798, 151.203514
#BombahBoy dead -32.492778, 152.398889
namesBG<- c("Conflict", "Latitude", "Longitude")
Killingcoord <- data.frame(matrix(ncol = 3, nrow = 1))
colnames(Killingcoord) <- namesBG
Killingcoord$Conflict <- "Conflict"
Killingcoord$Latitude <- -32.5479512004568
Killingcoord$Longitude <- 152.30759963706402

namesBB<- c("Conflict", "Latitude", "Longitude")
KillingcoordBB <- data.frame(matrix(ncol = 3, nrow = 1))
colnames(KillingcoordBB) <- namesBB
KillingcoordBB$Conflict <- "Conflict"
KillingcoordBB$Latitude <- -32.492778
KillingcoordBB$Longitude <- 152.398889



#Sydney coordinate
namesSyd<- c("City", "Latitude", "Longitude")
Sydney <- data.frame(matrix(ncol = 3, nrow = 1))
colnames(Sydney) <- namesSyd
Sydney$City <- "Sydney"
Sydney$Latitude <- -33.874798
Sydney$Longitude <- 151.203514



##Read in collar data from cathy and red dog
SLpackpre <- read.csv(file = "Raw Data/CollarDataPre.csv")%>%filter(Individual == "SLF2003")
MBpackpre <- read.csv(file = "Raw Data/CollarDataPre.csv")%>%filter(Individual == "UOM2002")
SLpackpost <- read.csv(file = "Raw Data/postconflictdata.csv")%>%
  filter(pack == "SL")
MBpackpost <- read.csv(file = "Raw Data/postconflictdata.csv")%>%
  filter(pack == "MB")

  
#Some cleaning: 
SLpackpre$DateTime <- as.POSIXct(paste(2000 + SLpackpre$Year, "-", SLpackpre$Day, " ", SLpackpre$Hour, ":00:00", sep = ""), format = "%Y-%j %H:%M:%S")
MBpackpre$DateTime <- as.POSIXct(paste(2000 + MBpackpre$Year, "-", MBpackpre$Day, " ", MBpackpre$Hour, ":00:00", sep = ""), format = "%Y-%j %H:%M:%S")

SLpackpost$DateTime <- as.POSIXct(paste(SLpackpost$year, "-",SLpackpost$month_aedt,"-", SLpackpost$day_aedt, " ", SLpackpost$hour_aedt, ":00:00", sep = ""), format = "%Y-%m-%d %H:%M:%S")
MBpackpost$DateTime <- as.POSIXct(paste(MBpackpost$year, "-",MBpackpost$month_aedt,"-", MBpackpost$day_aedt, " ", MBpackpost$hour_aedt, ":00:00", sep = ""), format = "%Y-%m-%d %H:%M:%S")


#remove clear outliers
CleanSLpackpre <- SLpackpre[SLpackpre$Latitude < -30, ]
CleanMBpackpre <- MBpackpre[MBpackpre$Latitude < -30, ]
CleanSLpackpost <- SLpackpost[SLpackpost$Latitude < -30, ]
CleanMBpackpost <- MBpackpost[MBpackpost$Latitude < -30, ]


CleanSLpackpre <- CleanSLpackpre[CleanSLpackpre$Hdop <1.5,]
CleanMBpackpre <- CleanMBpackpre[CleanMBpackpre$Hdop <1.5,]
CleanSLpackpost <- CleanSLpackpost[CleanSLpackpost$Hdop <1.5,]
CleanMBpackpost <- CleanMBpackpost[CleanMBpackpost$Hdop <1.5,]

#Define dates
#Conservative ranges
Startdatepre <- as.POSIXct("2021-11-01 00:00:00")
Enddatepre <- as.POSIXct("2022-03-20 00:00:00")

Startdatepost <- as.POSIXct("2022-06-15 00:00:00")
Enddatepost <- as.POSIXct("2022-09-15 00:00:00")


CleanSLpackdatepre <- CleanSLpackpre[CleanSLpackpre$DateTime <= Enddatepre & CleanSLpackpre$DateTime >= Startdatepre,]
CleanMBpackdatepre <- CleanMBpackpre[CleanMBpackpre$DateTime <= Enddatepre & CleanMBpackpre$DateTime >= Startdatepre,]

CleanSLpackdatepost <- CleanSLpackpost[CleanSLpackpost$DateTime <= Enddatepost & CleanSLpackpost$DateTime >= Startdatepost,]
CleanMBpackdatepost <- CleanMBpackpost[CleanMBpackpost$DateTime <= Enddatepost & CleanMBpackpost$DateTime >= Startdatepost,]

CleanSLpackdatepre <- CleanSLpackdatepre[!is.na(CleanSLpackdatepre$Latitude),]
CleanMBpackdatepre <- CleanMBpackdatepre[!is.na(CleanMBpackdatepre$Latitude),]
CleanSLpackdatepost <- CleanSLpackdatepost[!is.na(CleanSLpackdatepost$Latitude),]#dont run as there are no na lats. 
CleanMBpackdatepost <- CleanMBpackdatepost[!is.na(CleanMBpackdatepost$Latitude),]#dont run as there are no na lats.  



CleanSLpackdatepre$Pack <- "SL"
CleanMBpackdatepre$Pack <- "MB"
CleanSLpackdatepost$Pack <- "SL"
CleanMBpackdatepost$Pack <- "MB"

CleanSLpackdatepre <- CleanSLpackdatepre%>%
  mutate(Period = "PreConflict")

CleanMBpackdatepre <- CleanMBpackdatepre%>%
  mutate(Period = "PreConflict")

CleanSLpackdatepost <- CleanSLpackdatepost%>%
  mutate(Period = "PostConflict")

CleanMBpackdatepost <- CleanMBpackdatepost%>%
  mutate(Period = "PostConflict")


CleanBothPrePost <- bind_rows(CleanSLpackdatepre%>%select(DateTime,Pack,Latitude,Longitude,Period),
                              CleanMBpackdatepre%>%select(DateTime,Pack,Latitude,Longitude,Period),
                              CleanSLpackdatepost%>%select(DateTime,Pack,Latitude,Longitude,Period),
                              CleanMBpackdatepost%>%select(DateTime,Pack,Latitude,Longitude,Period))


#Assign sf
CleanSF <- st_as_sf(CleanBothPrePost%>%filter(Period == "PreConflict"), coords = c("Longitude", "Latitude"), crs = 4326)
CleanAll <- st_as_sf(CleanBothPrePost, coords = c("Longitude", "Latitude"), crs = 4326)




Cleanpoint <-st_as_sf(Killingcoord, coords = c("Longitude","Latitude"), crs=4326)





ggplot()+
  geom_sf(data =CleanSF, aes(color=Pack), size = 1)+
  geom_sf(data = Cleanpoint, size = 5)+
  theme_minimal()

#We have the points ready now. 

#get base map
map <- get_stadiamap(bbox = c(left = 152.18, 
                              bottom = -32.68,
                              right = 152.47, 
                              top = -32.44),
                     crop = TRUE,
                     zoom = 10, maptype = "stamen_terrain_background")
basemapgg <- ggmap(map)


killing <- ggmap(map) +
  geom_point(data = CleanBoth, aes(x = Longitude, y = Latitude, color = Pack), size = 1) +
  scale_color_manual(values = c("MB" = "red", "SL" = "blue"))+
  geom_point(data = Killingcoord, aes(x = Longitude, y = Latitude), size = 7,shape = 25,color = "black",fill="lightblue") +
  geom_segment(aes(x = 152.25, y = -32.515, 
                                  xend = 152.3, yend = -32.545), 
                              arrow = arrow(length = unit(0.3, "cm")), 
                              color = "black", size = 1) +
  geom_text(aes(x = 152.25, y = -32.515), 
           label = "   2 SL\nmales\nkilled", 
           hjust = 1, vjust = -0.2, size = 7, fontface = "bold", color = "black") +
  
  geom_point(data = KillingcoordBB, aes(x = Longitude, y = Latitude), size = 7,shape = 25,color = "black",fill="darkred") +
  geom_segment(aes(x = 152.43, y = -32.53, 
                   xend = 152.402, yend = -32.5), 
               arrow = arrow(length = unit(0.3, "cm")), 
               color = "black", size = 1) +
  geom_text(aes(x = 152.45, y = -32.565), 
            label = "   MB male\nkilled", 
            hjust = 0.8, vjust = -0.8, size = 7, fontface = "bold", color = "black") +
  
  theme_minimal()+
  guides(color = guide_legend(override.aes = list(size =4)))+
  theme(legend.text = element_text(size = 17),
        legend.title = element_text(size =17),
        axis.text = element_text(size = 15),
        axis.title = element_text(size =17))+
  labs(x = "Longitude", y = "Latitude")

killing
#Done that, finally lets add the inset map



###Get aus inset map
australia_map <- ne_states(country = "Australia", returnclass = "sf")



australia_inset <- ggplot() +
  geom_sf(data = australia_map, fill = "lightgrey", color = "black") +
  geom_point(data = Killingcoord,aes(x = Longitude, y = Latitude), color = "black", size=6) + 
  geom_point(data = Sydney,aes(x = Longitude, y = Latitude), color = "black", size=4, shape = 17) + 
  coord_sf(xlim = c(140, 154), ylim = c(-39, -28)) +
  theme_minimal() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, size = 0.6),         panel.spacing = margin(10, 10, 10, 10))+
  geom_segment(aes(x = 149.15, y = -32.515, 
                   xend = 151.4, yend = -32.545), 
               arrow = arrow(length = unit(0.3, "cm")), 
               color = "black", size = 1) +
  geom_text(aes(x = 149, y = -32.515), 
            label = "Study site", 
            hjust = 1, vjust = 0.3, size = 4, fontface = "bold", color = "black")+
geom_segment(aes(x = 149.15, y = -33.875, 
                 xend = 150.6, yend = -33.875), 
             arrow = arrow(length = unit(0.3, "cm")), 
             color = "black", size = 1) +
  geom_text(aes(x = 149, y = -33.875), 
            label = "Sydney", 
            hjust = 1, vjust = 0.25, size = 4, fontface = "bold", color = "black")
  
  

australia_inset

location <- ggdraw()+
  draw_plot(killing)+
  draw_plot(australia_inset,0.58, 0.15, 0.3, 0.3)


png("Figures/CollarFigure.jpg", width = 11, height =11, res= 300, units = "in")

location
dev.off()

#End