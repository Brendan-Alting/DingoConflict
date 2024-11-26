####Figure 1- Collar data
library(dplyr)
library(ggplot2)
library(sf)
library(ggmap)
library(rnaturalearth)
library(cowplot)

#Start by generating stadiamap key


#read key
Sys.setenv(GGMAP_STADIA_API_KEY = "f9d511a5-33ca-42aa-a527-943c07b6fe0f")
register_stadiamaps("f9d511a5-33ca-42aa-a527-943c07b6fe0f", write = FALSE)


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
SLpack <- read.csv(file = "Raw Data/SLF2003.csv")
MBpack <- read.csv(file = "Raw Data/UOM2002.csv")

#Some cleaning: 
SLpack$DateTime <- as.POSIXct(paste(2000 + SLpack$Year, "-", SLpack$Day, " ", SLpack$Hour, ":00:00", sep = ""), format = "%Y-%j %H:%M:%S")
MBpack$DateTime <- as.POSIXct(paste(2000 + MBpack$Year, "-", MBpack$Day, " ", MBpack$Hour, ":00:00", sep = ""), format = "%Y-%j %H:%M:%S")

#remove clear outliers
CleanSLpack <- SLpack[SLpack$Latitude < -30, ]
CleanMBpack <- MBpack[MBpack$Latitude < -30, ]

CleanSLpack <- CleanSLpack[CleanSLpack$Hdop <1.5,]
CleanMBpack <- CleanMBpack[CleanMBpack$Hdop <1.5,]

#Define dates
#Conservative ranges
Startdate <- as.POSIXct("2021-11-01 00:00:00")
Enddate <- as.POSIXct("2022-03-20 00:00:00")

CleanSLpackdate <- CleanSLpack[CleanSLpack$DateTime <= Enddate & CleanSLpack$DateTime >= Startdate,]
CleanMBpackdate <- CleanMBpack[CleanMBpack$DateTime <= Enddate & CleanMBpack$DateTime >= Startdate,]

CleanSLpackdate <- CleanSLpackdate[!is.na(CleanSLpackdate$Latitude),]
CleanMBpackdate <- CleanMBpackdate[!is.na(CleanMBpackdate$Latitude),]

CleanSLpackdate$Pack <- "SL"
CleanMBpackdate$Pack <- "MB"

CleanBoth <- rbind(
  CleanSLpackdate[, c("DateTime", "Pack", "Latitude", "Longitude")],
  CleanMBpackdate[, c("DateTime", "Pack", "Latitude", "Longitude")])

#Assign sf
CleanSF <- st_as_sf(CleanBoth, coords = c("Longitude", "Latitude"), crs = 4326)
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