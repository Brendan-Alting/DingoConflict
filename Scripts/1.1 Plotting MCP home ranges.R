#1.1 getting supplementary figure for MCPs

##Interim to get 95% MCPs for all data
CleanSLpackdatepre_sp <- SpatialPointsDataFrame(
  coords = CleanSLpackdatepre[, c("Longitude", "Latitude")],
  data = CleanSLpackdatepre,
  proj4string = CRS("+proj=longlat +datum=WGS84")
)


CleanSLpackdatepre_utm <- spTransform(
  CleanSLpackdatepre_sp,
  CRS("+proj=utm +zone=56 +south +datum=WGS84 +units=m")
)


CleanMBpackdatepre_sp <- SpatialPointsDataFrame(
  coords = CleanMBpackdatepre[, c("Longitude", "Latitude")],
  data = CleanMBpackdatepre,
  proj4string = CRS("+proj=longlat +datum=WGS84")
)


CleanMBpackdatepre_utm <- spTransform(
  CleanMBpackdatepre_sp,
  CRS("+proj=utm +zone=56 +south +datum=WGS84 +units=m")
)

###now post

##Interim to get 95% MCPs for all data
CleanSLpackdatepost_sp <- SpatialPointsDataFrame(
  coords = CleanSLpackdatepost[, c("Longitude", "Latitude")],
  data = CleanSLpackdatepost,
  proj4string = CRS("+proj=longlat +datum=WGS84")
)


CleanSLpackdatepost_utm <- spTransform(
  CleanSLpackdatepost_sp,
  CRS("+proj=utm +zone=56 +south +datum=WGS84 +units=m")
)


CleanMBpackdatepost_sp <- SpatialPointsDataFrame(
  coords = CleanMBpackdatepost[, c("Longitude", "Latitude")],
  data = CleanMBpackdatepost,
  proj4string = CRS("+proj=longlat +datum=WGS84")
)


CleanMBpackdatepost_utm <- spTransform(
  CleanMBpackdatepost_sp,
  CRS("+proj=utm +zone=56 +south +datum=WGS84 +units=m")
)



MCPSLpre <- mcp(CleanSLpackdatepre_utm,percent = 95)
MCPMBpre <- mcp(CleanMBpackdatepre_utm,percent = 95)
MCPSLpost <- mcp(CleanSLpackdatepost_utm,percent = 95)
MCPMBpost <- mcp(CleanMBpackdatepost_utm,percent = 95)


#Now we have all of the points in cleanall. lets get convex hull and clip to this (will be clipped to land effectively.)

concave <- CleanAll %>%
  st_union() %>%
  st_concave_hull(ratio = 0.02)%>%
  st_transform(st_crs(CleanMBpackdatepost_utm))


MCPSLpresf <- st_as_sf(MCPSLpre)%>%
  mutate(Pack = "SL",
         Period = "Pre_Conflict")%>%
  st_intersection(concave)

MCPMBpresf <- st_as_sf(MCPMBpre)%>%
  mutate(Pack = "MB",
         Period = "Pre_Conflict")%>%
  st_intersection(concave)

MCPSLpostsf <- st_as_sf(MCPSLpost)%>%
  mutate(Pack = "SL",
         Period = "Post_Conflict")%>%
  st_intersection(concave)

MCPMBpostsf <- st_as_sf(MCPMBpost)%>%
  mutate(Pack = "MB",
         Period = "Post_Conflict")%>%
  st_intersection(concave)

allmcps <- bind_rows(MCPSLpresf,MCPMBpresf,MCPSLpostsf,MCPMBpostsf)%>%
  mutate(Period = factor(Period, levels = c("Pre_Conflict", "Post_Conflict")))



map <- get_stadiamap(bbox = c(left = 152.18, 
                              bottom = -32.68,
                              right = 152.47, 
                              top = -32.44),
                     crop = TRUE,
                     zoom = 10, maptype = "stamen_terrain_background")

basemapgg <- ggmap(map)




label_df <- data.frame(
  Period = factor(c("Pre_Conflict", "Post_Conflict"), 
                  levels = c("Pre_Conflict", "Post_Conflict")),
  label = c("a)", "b)"),
  Longitude = 152.19, 
  Latitude = -32.45   
)




basemapgg +
  geom_sf(data = allmcps, 
          aes(fill = Pack), 
          alpha = 0.4, 
          color = "black",
          inherit.aes = FALSE) +
  coord_sf(crs = st_crs(4326)) +
  facet_wrap(~Period,ncol=1) +
  labs(x = "Longitude", y = "Latitude")+
  theme_minimal() +
  theme(axis.text.x=element_text(angle =45),
        text = element_text(size = 20))+
  labs(fill = "Pack")+
  geom_text(data = label_df, 
            aes(x = Longitude, y = Latitude, label = label),
            inherit.aes = FALSE, 
            fontface = "bold", size = 5)

ggsave("Figures/mcp_plots.png", width = 12, height = 14, units = "in", dpi = 300)
