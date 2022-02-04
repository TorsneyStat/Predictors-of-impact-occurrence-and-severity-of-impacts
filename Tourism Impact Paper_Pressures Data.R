setwd ("C:/Users/AndrewT/OneDrive - CAAS (Environmental Services) Ltd/Desktop/AndyPhD/PhD_Data_R/WAW")
SDF <- read.csv("C:/Users/AndrewT/OneDrive - CAAS (Environmental Services) Ltd/Desktop/AndyPhD/PhD_Data_R/WAW/EU_Threats_AllSites.csv", header=TRUE)
EU_Sites <- read.csv("C:/Users/AndrewT/OneDrive - CAAS (Environmental Services) Ltd/Desktop/AndyPhD/PhD_Data_R/WAW/EU_Site_Names.csv", header=TRUE)
Tourism_threatsand_pressures <- read.csv("C:/Users/AndrewT/OneDrive - CAAS (Environmental Services) Ltd/Desktop/AndyPhD/PhD_Data_R/WAW/Tourism_threatsand pressures.csv", header=TRUE)
source("AllPackages.R")

SDF$Site.Code <- SDF$ï..Site.Code
SDF <- na.omit(SDF)

SDF <- merge(SDF, EU_Sites, by.x = "Site.Code", by.y = "Site.Code")

##Threats and pressures thet relate to creation and/or tourism ditectly
AmenityPressures <- list("G", "G01", "G01.01", "G01.01.01", "G01.01.02", "G01.02", "G01.03", "G01.03.01", "G01.03.02", "G01.04", 
                         "G01.04.01", "G01.04.02", "G01.04.03", "G01.05", "G01.06", "G01.07", "G01.08", "G02", "G02.01", "G02.02", 
                         "G02.03", "G02.04", "G02.05", "G02.06", "G02.07", "G02.08", "G02.09", "G02.10", "G03", "G05", "G05.01", 
                         "G05.02", "G05.03", "G05.04", "G05.05", "G05.06", "G05.07", "G05.08", "G05.09", "G05.10", "G05.11", "D01.01", 
                         "D03.01.02", "D04", "D04.01", "D04.02", "D04.03", "D05", "F02.03", "F02.03.01", "F02.03.02", "F02.03.03", "F03.01", "F03.01.01")


##counting types of sites
spa2k <- SDF %>% filter(
  Site.Code %in% SPAsites)
totalSPA2kpressures2km <- as.numeric(length(unique(spa2k$Site.Code)))

#
sac2k <- SDF %>% filter(
  Site.Code %in% SACsites)
totalSAC2kpressires2km <- as.numeric(length(unique(sac2k$Site.Code)))

totalALL2km <- totalSAC2kpressires2km + totalSPA2kpressures2km

SAC_Tourism_SDF <- sac2k %>% filter(
  IMPACTCODE %in% AmenityPressures)
SAC_totalimpact <- as.numeric(length(unique(SAC_Tourism_SDF$Site.Code)))

SPA_Tourism_SDF <- spa2k %>% filter(
  IMPACTCODE %in% AmenityPressures)
SPA_totalimpact <- as.numeric(length(unique(SPA_Tourism_SDF$Site.Code)))

totalSPA2k <- 73
totalSAC2k <- 129
totalALL_ingeneral2km <- 202 
##calculations
SPA_totalimpact/totalSPA2k*100
SAC_totalimpact/totalSAC2k*100


totalALL2km/totalALL_ingeneral2km*100
###########################################

pressurecountTOTAL <- SDF
pressurecountTOTAL <- pressurecountTOTAL %>% group_by(Site.Code, IMPACTCODE) %>%
  summarise("Pressure" = paste(unique(OCCURRENCE), collapse = ", "),
  )
sitecounts <- pressurecountTOTAL %>%
  group_by(IMPACTCODE) %>%
  summarise(count=n()) 
sitecounts <- merge(sitecounts, Tourism_threatsand_pressures, by.x = "IMPACTCODE", by.y = "Pressure.Code")

pressurecountSAC <- SAC_Tourism_SDF
pressurecountSAC <- pressurecountSAC %>% group_by(Site.Code, IMPACTCODE) %>%
  summarise("Pressure" = paste(unique(OCCURRENCE), collapse = ", "),
  )
sitecounts_SAC <- pressurecountSAC %>%
  group_by(IMPACTCODE) %>%
  summarise(count=n()) 
sitecounts_SAC <- merge(sitecounts_SAC, Tourism_threatsand_pressures, by.x = "IMPACTCODE", by.y = "Pressure.Code")

pressurecountSPA <- SPA_Tourism_SDF
pressurecountSPA <- pressurecountSPA %>% group_by(Site.Code, IMPACTCODE) %>%
  summarise("Pressure" = paste(unique(OCCURRENCE), collapse = ", "),
  )
sitecounts_SPA <- pressurecountSPA %>%
  group_by(IMPACTCODE) %>%
  summarise(count=n()) 
sitecounts_SPA <- merge(sitecounts_SPA, Tourism_threatsand_pressures, by.x = "IMPACTCODE", by.y = "Pressure.Code")

write.csv(sitecounts_SPA, "C://Users//AndrewT//OneDrive - CAAS (Environmental Services) Ltd//Desktop//PhD_Data_R//WAW//test.csv", row.names = FALSE)
write.csv(sitecounts_SAC, "C://Users//AndrewT//OneDrive - CAAS (Environmental Services) Ltd//Desktop//PhD_Data_R//WAW//test2.csv", row.names = FALSE)

sitecountsNEW <- Tourism_SDF %>%
  group_by(Site.Code) %>%
  summarise(count=n()) 
mean(sitecountsNEW$count)


##Comparing proportions 
europeIreland <- prop.test(x = c(1682, 349), n = c(3444, 604))
allthreescales <- prop.test(x = c(1682, 349, 130), n = c(3444, 604, 202))
sac_spa_europe <- prop.test(x = c(1498, 471), n = c(3328, 789))
sac_spa_ireland <- prop.test(x = c(239, 110), n = c(439, 165))
sac_spa_waw <- prop.test(x = c(75, 55), n = c(129, 73))
sac_spa_waw
