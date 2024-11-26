##Figure 2: Dingo detections over time, who with who? 
library(tidyverse)
library(ggplot2)
#Also read in detections dataset, which contains all dingo detections from both years

Dingodets <- read.csv(file = "Raw Data/DingoDetections.csv", header = T)

#now need to merge some traps, as they were the same location, just moved slightly
trap_mapping <- c("PS24" = "PS4", "PS23" = "PS7", "PS25" = "PS10", "PS27" = "PS22")


Dingodets <- Dingodets %>%
  mutate(Trap = ifelse(Trap %in% names(trap_mapping), trap_mapping[Trap], Trap))

#and change two incorrect records
Dingodets <- Dingodets%>%
  mutate(Individual = case_when(Individual =="UOM1707" & Trap == "PS5"~"UOM1701",
                                TRUE~Individual))
Dingodets$DateTime <- as.POSIXct(Dingodets$DateTime,format = "%Y-%m-%d %H:%M")

#Define dates
startdatebefore <-  as.POSIXct("2022-02-12 00:00:00")
enddatebefore <- as.POSIXct("2022-03-27 00:00:00")
startdateduring <- as.POSIXct("2022-03-27 00:00:00")
enddateduring <- as.POSIXct("2022-05-09 00:00:00")
startdateafter <- as.POSIXct("2022-05-09 00:00:00")
enddateafter <- as.POSIXct("2022-05-31 00:00:00")

Dingodetstime <- Dingodets[Dingodets$DateTime <= enddateafter & Dingodets$DateTime >= startdatebefore,]

Cathy <- Dingodetstime[which(Dingodetstime$Individual == "SLF2003"),]
Socks <- Dingodetstime[which(Dingodetstime$Individual == "SLF1501"),]
Fabio <- Dingodetstime[which(Dingodetstime$Individual == "UOM2001"),]
RedDog <- Dingodetstime[which(Dingodetstime$Individual == "UOM2002"),]
BB <- Dingodetstime[which(Dingodetstime$Individual == "UOM1701"),]
Bombah <- Dingodetstime[which(Dingodetstime$Individual == "UOF1801"),]
Piper <- Dingodetstime[which(Dingodetstime$Individual == "PCM1601"),]
SLM2005 <- Dingodetstime[which(Dingodetstime$Individual == "SLM2005"),]
SLF2001 <- Dingodetstime[which(Dingodetstime$Individual == "SLF2001"),]
SLF2002 <- Dingodetstime[which(Dingodetstime$Individual == "SLF2002"),]

Cathy$Pack <- "SL"
Socks$Pack <- "SL"
Piper$Pack <- "SL"
SLM2005$Pack <- "SL"
Fabio$Pack <- "MB"
Bombah$Pack <- "MB"
RedDog$Pack <- "MB"
BB$Pack <- "MB"
SLF2001$Pack <- "SL"

relevanttogether <- rbind(Cathy, Socks,Fabio,RedDog,BB,Bombah,Piper,SLM2005,SLF2001)

relevanttogether$Period[relevanttogether$DateTime >= startdatebefore & relevanttogether$DateTime <= enddatebefore] <- "Before"
relevanttogether$Period[relevanttogether$DateTime >= startdateduring & relevanttogether$DateTime <= enddateduring] <- "During"
relevanttogether$Period[relevanttogether$DateTime >= startdateafter & relevanttogether$DateTime <= enddateafter] <- "After"


relevanttogether$Date <- as.Date(relevanttogether$DateTime)

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
  geom_col(position=position_dodge(width=0.1))+
  labs(title = "Dingo Detections Over Time",
       x = "Day",
       y = "Individuals Detected" )+
  geom_vline(xintercept = as.Date("2022-03-27"), linetype = "dashed", color = "black", size =1) +
  geom_vline(xintercept = as.Date("2022-05-09"), linetype = "dashed", color = "black",size=1) +
  theme_minimal()+
  theme(axis.text = element_text(size = 18),
        title = element_blank(),
        axis.title  = element_text(size = 18),
        legend.text = element_text(size = 18),
        legend.title = element_blank())+
  scale_fill_brewer(type = "qual",
                    labels = c("In Pack Range", "Out Of Pack Range"))


png("Figures/DetsInOut.jpg", width = 16, height =11, res= 300, units = "in")

detsinout
dev.off()


#DONE 

#From this point, I copied results directly into an csv file called 'datasubset' for ease of reading. this is in the 'data' folder. 