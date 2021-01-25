library (dplyr)

#DELIVERABLE 1 ----
#***************************
#read data into R
MecaCarData<- read.csv (file="MechaCar_mpg.csv", check.names = F, stringsAsFactors = F)

#generate multiple linear regression model

lm( mpg~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD, data= MecaCarData)

#generate summary statistics
summary(lm( mpg~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD, data= MecaCarData))

cor(MecaCarData$mpg,MecaCarData$vehicle_length )
cor(MecaCarData$mpg, MecaCarData$vehicle_weight)
cor(MecaCarData$mpg, MecaCarData$spoiler_angle)
cor(MecaCarData$mpg, MecaCarData$ground_clearance)
cor(MecaCarData$mpg, MecaCarData$AWD)

#DELIVERABLE 2 ----
#****************************
#read suspension coil data into R
MecaSuspCoil<- read.csv(file = "Suspension_coil.csv", check.names = F, stringsAsFactors = F)

#create a total_summary df using the summarize() function

Total_Summary<- MecaSuspCoil %>% summarize(Count=n(),
                                           Mean_PSI=mean(PSI),
                                           Median_PSI=median(PSI), 
                                           Variance=var(PSI), 
                                           SD=sd(PSI), .groups="keep")

# create lot summary df using group by and summarize functions

Lot_summary<- MecaSuspCoil %>%group_by(Manufacturing_Lot)%>% summarize(Count=n(),
                                                            Mean_PSI=mean(PSI),
                                                            Median_PSI=median(PSI), 
                                                            Variance=var(PSI), 
                                                            SD=sd(PSI), .groups="keep")
  
#DELIVERABLE 3 ----

t.test ((MecaSuspCoil$PSI), mu=1500)


lot1 <-subset (MecaSuspCoil, Manufacturing_Lot =="Lot1")
t.test(lot1$PSI, mu=1500)

lot2 <-subset (MecaSuspCoil, Manufacturing_Lot =="Lot2")
t.test(lot2$PSI, mu=1500)

lot3 <-subset (MecaSuspCoil, Manufacturing_Lot =="Lot3")
t.test(lot3$PSI, mu=1500)
