PKG <- c("foreign","stringr", "data.table","tidyr","dplyr","ggplot2",
         "ROracle")

for (p in PKG) {
  if(!require(p,character.only = TRUE)) {  
    install.packages(p)
    require(p,character.only = TRUE)}
}

#Recreational landings data pulled from National Summary Query at 
#https://www.st.nmfs.noaa.gov/recreational-fisheries/data-and-documentation/queries/index
Recreational_Catch <- read.csv(file="F:/MAFMC EAFM/Risk/2022 update/mrip_catch_snapshot.csv", 
                                  skip=42,
                                  fill = TRUE, header=TRUE, as.is=TRUE)
Recreational_Landings <- read.csv(file="F:/MAFMC EAFM/Risk/2022 update/mrip_harvest_snapshot.csv", 
                                  skip=42,
                                  fill = TRUE, header=TRUE, as.is=TRUE) %>% 
  merge(Recreational_Catch,all=TRUE)

Mid_catch <- Recreational_Landings %>% filter(Region%in%"MID-ATLANTIC") %>%
                  mutate(`Total.Harvest..A.B1.`=as.numeric(`Total.Harvest..A.B1.`),
                         `Total.Catch..A.B1.B2.`=as.numeric(`Total.Catch..A.B1.B2.`),
                         `Fishing Mode` = `Fishing.Mode`,
                         `Fishing Mode` = ifelse(`Fishing Mode` == "CHARTER BOAT",
                                                 "PARTY/CHARTER BOAT",
                                                 `Fishing Mode`),
                         `Fishing Mode` = ifelse(`Fishing Mode` == "PARTY BOAT",
                                                 "PARTY/CHARTER BOAT",
                                                 `Fishing Mode`))%>%
                  group_by(Year,`Fishing Mode`) %>%
                  summarise(Harvest = sum(`Total.Harvest..A.B1.`,na.rm = TRUE),
                            Catch = sum(`Total.Catch..A.B1.B2.`,na.rm = TRUE)) %>%
                    mutate(Discards = Catch-Harvest,
                           Discard_ratio = Discards/Catch) %>% ungroup()

ggplot(Mid_catch, aes(x=Year,y=Discard_ratio))+geom_line()+
  labs(title="Discard Ratio")+ylab("Discards/Catch")+
  facet_wrap(vars(`Fishing Mode`), dir="v")
ggsave("Discard_ratio.png", width = 6, height=4)
