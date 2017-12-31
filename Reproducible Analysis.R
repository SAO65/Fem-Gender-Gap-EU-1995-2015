# Expansion of morbidity

#Years lived with disability (YLDs)
# Years of life lived with any short-term or long-term health loss.

# Healthy life expectancy, or health-adjusted life expectancy (HALE)
#The number of years that a person at a given age can expect to live in good health, 
# taking into account mortality and disability.

######################################################

library(data.table)
library(ggplot2)
library(car)

######################################################

# Raw Data
HALE.raw.data <- read.csv("./IHME-GBD_2015_DATA-7c514cb9-1/IHME-GBD_2015_DATA-7c514cb9-1.csv")
LE.raw.data <- read.csv("./IHME-GBD_2015_DATA-b8689aa2-1/IHME-GBD_2015_DATA-b8689aa2-1.csv")

head(LE.raw.data)
head(HALE.raw.data)

attach(LE.raw.data)
attach(HALE.raw.data)

DT.LE <- data.table(LE.raw.data)
DT.HALE <- data.table(HALE.raw.data)
DT.HALE

tables()

######################################################

# Tidy Data

(data.LE.1995.male <- DT.LE[DT.LE$year == "1995" & DT.LE$sex == "Male", ])
(data.HALE.1995.male <- DT.HALE[DT.HALE$year == "1995" & DT.HALE$sex == "Male", ])
(data.YLD.1995.male <- as.data.frame(data.LE.1995.male$val - data.HALE.1995.male$val))
(data.LE.2015.male <- DT.LE[DT.LE$year == "2015" & DT.LE$sex == "Male", ])
(data.HALE.2015.male <- DT.HALE[DT.HALE$year == "2015" & DT.HALE$sex == "Male", ])
(data.YLD.2015.male <- as.data.frame(data.LE.2015.male$val - data.HALE.2015.male$val))

data.LE.1995.female <- DT.LE[DT.LE$year == "1995" & DT.LE$sex == "Female", ]
data.HALE.1995.female <- DT.HALE[DT.HALE$year == "1995" & DT.HALE$sex == "Female", ]
data.YLD.1995.female <- as.data.frame(data.LE.1995.female$val - data.HALE.1995.female$val)
data.LE.2015.female <- DT.LE[DT.LE$year == "2015" & DT.LE$sex == "Female", ]
data.HALE.2015.female <- DT.HALE[DT.HALE$year == "2015" & DT.HALE$sex == "Female", ]
data.YLD.2015.female <- as.data.frame(data.LE.2015.female$val - data.HALE.2015.female$val)

tidy.data <- data.frame("HALE.M.95" = data.HALE.1995.male$val,
                        "YLD.M.95" = data.YLD.1995.male$`data.LE.1995.male$val - data.HALE.1995.male$val`,
                        "HALE.M.15" = data.HALE.2015.male$val,
                        "YLD.M.15" = data.YLD.2015.male$`data.LE.2015.male$val - data.HALE.2015.male$val`,
                        "HALE.F.95" = data.HALE.1995.female$val,
                        "YLD.F.95" = data.YLD.1995.female$`data.LE.1995.female$val - data.HALE.1995.female$val`,
                        "HALE.F.15" = data.HALE.2015.female$val,
                        "YLD.F.15" = data.YLD.2015.female$`data.LE.2015.female$val - data.HALE.2015.female$val`)
                        
tidy.data

# Saving Tidy Dataset to .csv file
write.csv(tidy.data, "TidyData.csv")


######################################################

# Training Data and Prediction
lm.fit.1995.male <- lm(data.YLD.1995.male$`data.LE.1995.male$val - data.HALE.1995.male$val` ~ data.HALE.1995.male$val)
summary(lm.fit.1995.male)
confint(lm.fit.1995.male)

lm.fit.1995.female <- lm(data.YLD.1995.female$`data.LE.1995.female$val - data.HALE.1995.female$val` ~ data.HALE.1995.female$val)
summary(lm.fit.1995.female)
confint(lm.fit.1995.female)


lm.fit.2015.male <- lm(data.YLD.2015.male$`data.LE.2015.male$val - data.HALE.2015.male$val` ~ data.HALE.2015.male$val)
summary(lm.fit.2015.male)
confint(lm.fit.2015.male)

lm.fit.2015.female <- lm(data.YLD.2015.female$`data.LE.2015.female$val - data.HALE.2015.female$val` ~ data.HALE.2015.female$val)
summary(lm.fit.2015.female)
confint(lm.fit.2015.female)

plot(data.HALE.2015.male$val, 
     data.YLD.2015.male$`data.LE.2015.male$val - data.HALE.2015.male$val`, 
     col="blue",
     cex=1,
     pch=16,
     xlim = c(50,75), 
     ylim=c(6, 13), 
     axes=F,
     main = "Test Data",
     xlab="Health-Adjusted Life expectancy (HALE)",
     ylab="Years Lived with Disability (YLD)")
abline(lm.fit.2015.male, col="blue", lty=1)

points(data.HALE.2015.female$val, 
       data.YLD.2015.female$`data.LE.2015.female$val - data.HALE.2015.female$val`, 
       col="red",
       cex=1,
       pch=16)
abline(lm.fit.2015.female, col="red", lty=1)

points(data.HALE.1995.male$val, 
       data.YLD.1995.male$`data.LE.1995.male$val - data.HALE.1995.male$val`, 
       col="blue",
       cex=.9,
       pch=1)
abline(lm.fit.1995.male, col="blue", lty=2)

points(data.HALE.1995.female$val, 
       data.YLD.1995.female$`data.LE.1995.female$val - data.HALE.1995.female$val`, 
       col="red",
       cex=.9,
       pch=1)
abline(lm.fit.1995.female, col="red", lty=2)

axis(1, col.axis="black", las=1, cex.axis=0.7, tck=-.01)
axis(4, col.axis="black", las=2, cex.axis=0.7, tck=-.01)

legend(52, 12,  
       c("Year 1995 Males", "Year 1995 Females", "Year 2015 Males", "Year 2015 Females"),
       pch=c(1,1,16,16),
       lty=c(2,2,1,1),
       cex=c(.9,.9,.9,.9),
       col=c("blue", "red","blue", "red"),
       bty = "n")


