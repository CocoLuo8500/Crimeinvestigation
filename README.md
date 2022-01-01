# Crimeinvestigation
#----------------------------------------------

library(ggplot2)
crime <- read.csv("Crime_data.csv")

#1
fit1 <- lm(Violent.Crime ~ Poverty.Rate, data = crime)
summary(fit1)
#2
ggplot(data = crime,aes(x = `Poverty.Rate`, y= `Violent.Crime`))+
  geom_point() +
  geom_smooth(method = "lm", formula = y~x)
#3
library(car)
crime$Violent.Crime1 <- crime$Violent.Crime
crime$Violent.Crime1[crime$Violent.Crime1 == 1242.6] <- NA
fit2 <- lm(Violent.Crime1 ~ Poverty.Rate, data = crime)
summary(fit2)

ggplot(data = crime,aes(x = `Poverty.Rate`, y= `Violent.Crime1`))+
  geom_point() +
  geom_smooth(method = "lm", formula = y~x)
#4

fit3<- lm(Violent.Crime1 ~ Poverty.Rate + Unemployment, data = crime)
summary(fit3)

summary(terrorism$SENT_MON)
table(terrorism$SENT_MON)

terrorism$SENT_MON2 <- terrorism$SENT_MON

# overwrite the numbers that should be NA
terrorism$SENT_MON2[terrorism$SENT_MON2 == 9999] <- NA
terrorism$SENT_MON2[terrorism$SENT_MON2 == -9] <- NA
terrorism$SENT_MON2[terrorism$SENT_MON2 == 9998] <- NA
terrorism$SENT_MON2[terrorism$SENT_MON2 == 9997] <- NA
terrorism$SENT_MON2[terrorism$SENT_MON2 == 8000] <- NA
terrorism$SENT_MON2[terrorism$SENT_MON2 == 9000] <- NA
terrorism$SENT_MON2[terrorism$SENT_MON2 == 0] <- NA


terrorism$SENT_MON2<-recode(terrorism$SENT_MON,"c(-9,9999,9998,9997,8000,9000,0) = NA")
summary(terrorism$SENT_MON2)

# Let's rerun our regression
fit3<- lm(SENT_MON2 ~ TOTALCTS, data = terrorism)
summary(fit3)

terrorism$years <- terrorism$SENT_MON2/12
summary(terrorism$years)

fit3 <- lm(years ~ TOTALCTS, data = terrorism)
summary(fit3)


fit4 <- lm(SENT_MON2 ~ RACE, data = terrorism)
summary(fit4)

is.factor(terrorism$RACE)
levels(terrorism$RACE)

# We'll set the baseline case to Black
terrorism$RACE <- factor(terrorism$RACE, levels = c("Black", "Hispanic", "Other", "White"))

fit5 <- lm(SENT_MON2 ~ RACE, data = terrorism)
summary(fit5)

#------------------------------------------------------------------------------
# Getting Pretty with Stargazer

library(stargazer)

#Compare the two models side by side
stargazer(fit1, fit2, type= "text", out = "models.txt")
stargazer(fit1, fit2, type= "html", out = "models1.htm")

