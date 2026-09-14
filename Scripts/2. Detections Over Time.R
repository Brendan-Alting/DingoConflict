##Figure 2: Dingo detections over time, who with who? 
library(tidyverse)
library(RColorBrewer)
library(ggplot2)
#Read in detections dataset, which contains dingo detections from relevant year and relevant pack

relevanttogether <- read.csv(file = "Raw Data/DingoDetections.csv", header=T)
relevanttogether$Date <- as.Date(relevanttogether$Date, format = "%Y-%m-%d",tz = "Australia/Sydney")

summarystats <- relevanttogether%>%
  mutate(CameraPack =case_when(
    Trap== "PS11"~"MB",
    Trap== "PS10"~"MB",
    Trap== "PS12"~"MB",
    Trap=="PS13"~"MB",
    Trap== "PS14"~"MB",
    Trap== "PS3"~"WT",
    Trap== "PS4"~"SL",
    Trap== "PS5"~"SL",
    Trap== "PS6"~"SL",
    Trap== "PS7"~"MB",
    Trap== "PS8"~"MB",
    Trap== "PS9"~"MB",
    Trap== "PS21"~"YG",
    Trap== "PS22"~"YG",
    Trap== "PS17"~"YG"
  ))

summarystats <- summarystats %>%
  mutate(Zone = case_when(
    Pack==CameraPack~"IHR",
    !Pack==CameraPack~"OOR"
  ))


###lets get summary statistics

summarystats2 <- summarystats%>%
  group_by(Period,Zone)%>%
  summarise(Count = n())%>%
  mutate(Proportion = (Count/sum(Count))*100)


days_per_period <- c("Before" = 43, "During" = 43, "After" = 22)

summarystats2$Count_per_day <- summarystats2$Count / days_per_period[summarystats2$Period]

summarystats2 <- summarystats2%>%
  mutate(detsperalive = case_when(Period=="After" ~ Count_per_day/7,
                   Period=="During" ~ Count_per_day/8,
                   Period=="Before" ~ Count_per_day/10),
         totalalive = case_when(Period=="After" ~ Count/7,
                                  Period=="During" ~ Count/8,
                                  Period=="Before" ~ Count/10))


#We'll also get a summary of the indiviudal counts. 
summaryindivs <- relevanttogether%>%
  group_by(Individual,Pack,Period)%>%
  summarise(Detections = n())%>%
  select(Individual,Pack,Detections,Period)%>%
  pivot_wider(names_from = Period,
              values_from = Detections,
              names_glue = "Detections{Period}",
              values_fill = 0)%>%
  ungroup()%>%
  mutate(Fate = c("Killed","RetainedDominant","Dispersed/Disappeared","RetainSubordinate","Killed","RetainedDominant","Killed","NewlyDominant","NewlyDominant"))%>%
  select(Individual,Pack,DetectionsBefore,DetectionsDuring,DetectionsAfter,Fate)

write.csv(summaryindivs,"Derived Data/individualsummary.csv",row.names=F)

#binomial test
binomial_results <- summarystats2 %>%
  group_by(Period) %>%
  summarise(
    Total = sum(Count),
    IHR_Count = sum(Count[Zone == "IHR"]),
    Proportion_IHR = IHR_Count / Total * 100,
    BinomTest = list(binom.test(IHR_Count, Total, p = 0.5)),
    p_value = BinomTest[[1]]$p.value
  ) %>%
  select(-BinomTest)



# Summarizing the total detections 
daily_detections <- summarystats %>%
  group_by(Date, Zone) %>%
  summarise(
    CountIndivs=n(),
    Trap = first(Trap),
    Period = first(Period),
    Zone = first(Zone)
  )



detsinout <- ggplot(data=daily_detections, aes(x = Date,y=CountIndivs,fill=Zone)) +
  geom_col()+
  labs(x = "Day",
       y = "Individuals detected" )+
  geom_vline(xintercept = as.Date("2022-03-27",tz = "Australia/Sydney"), linetype = "dashed", color = "black", size =1) +
  geom_vline(xintercept = as.Date("2022-05-09",tz = "Australia/Sydney"), linetype = "dashed", color = "black",size=1) +
  theme_minimal()+
  theme(axis.text = element_text(size = 22),
        axis.text.x = element_text(size = 22, angle =45),
        title = element_blank(),
        axis.title  = element_text(size = 22),
        legend.text = element_text(size = 22),
        legend.title = element_blank())+
  scale_fill_brewer(palette = "Set1",
                    labels = c("In pack range", "Out of pack range"))

detsinout

png("Figures/DetsInOut.jpg", width = 16, height =11, res= 300, units = "in")

detsinout
dev.off()


#DONE 

