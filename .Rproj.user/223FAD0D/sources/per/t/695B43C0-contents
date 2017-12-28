# Expansion of morbidity

#Years lived with disability (YLDs)
# Years of life lived with any short-term or long-term health loss.

# Healthy life expectancy, or health-adjusted life expectancy (HALE)
#The number of years that a person at a given age can expect to live in good health, 
# taking into account mortality and disability.

######################################################

# Raw Data
HALE.raw.data <- read.csv("./IHME-GBD_2015_DATA-7c514cb9-1/IHME-GBD_2015_DATA-7c514cb9-1.csv")
LE.raw.data <- read.csv("./IHME-GBD_2015_DATA-b8689aa2-1/IHME-GBD_2015_DATA-b8689aa2-1.csv")

head(LE.raw.data)
head(HALE.raw.data)

# Tidy Data
library(data.table)
library(ggplot2)
library(car)

attach(LE.raw.data)
attach(HALE.raw.data)

DT.LE <- data.table(LE.raw.data)
DT.HALE <- data.table(HALE.raw.data)

tables()

######################################################

# Tidy Data
data.LE.1995.male <- DT.LE[DT.LE$year == "1995" & DT.LE$sex == "Male", ]
data.LE.2015.male <- DT.LE[DT.LE$year == "2015" & DT.LE$sex == "Male", ]
data.LE.1995.female <- DT.LE[DT.LE$year == "1995" & DT.LE$sex == "Female", ]
data.LE.2015.female <- DT.LE[DT.LE$year == "2015" & DT.LE$sex == "Female", ]

# Training Data: Predictor
data.HALE.1995.male <- DT.HALE[DT.HALE$year == "1995" & DT.HALE$sex == "Male", ]
data.HALE.1995.female <- DT.HALE[DT.HALE$year == "1995" & DT.HALE$sex == "Female", ]

# Training Data: Response
data.YLD.1995.male <- data.LE.1995.male$val - data.HALE.1995.male$val
data.YLD.1995.female <- data.LE.1995.female$val - data.HALE.1995.female$val


# Test Data
data.HALE.2015.male <- DT.HALE[DT.HALE$year == "2015" & DT.HALE$sex == "Male", ]
data.HALE.2015.female <- DT.HALE[DT.HALE$year == "2015" & DT.HALE$sex == "Female", ]
data.YLD.2015.male <- data.LE.2015.male$val - data.HALE.2015.male$val
data.YLD.2015.female <- data.LE.2015.female$val - data.HALE.2015.female$val


######################################################

# Prediction

lm.fit.1995.male <- lm(data.YLD.1995.male ~ data.HALE.1995.male$val)
summary(lm.fit.1995.male)
confint(lm.fit.1995.male)

lm.fit.1995.female <- lm(data.YLD.1995.female ~ data.HALE.1995.female$val)
summary(lm.fit.1995.female)
confint(lm.fit.1995.female)

(pred.YLD.male <- predict(lm.fit.1995.male, 
                          data.frame(data.HALE.2015.male$val), 
                          interval="prediction"))

plot(data.HALE.1995.male$val, 
     data.YLD.1995.male, 
     col="blue",
     cex=1,
     pch=1,
     xlim = c(50,75), 
     ylim=c(6, 12), 
     axes=F,
     main = "Training datasets",
     xlab="Health-Adjusted Life expectancy (HALE)",
     ylab="Years Lived with Disability (YLD)")
abline(lm.fit.1995.male, col="blue", lty=2)

points(data.HALE.1995.female$val, 
       data.YLD.1995.female, 
       col="red",
       cex=1,
       pch=1)
abline(lm.fit.1995.female, col="red", lty=2)



axis(1, col.axis="black", las=1, cex.axis=0.7, tck=-.01)
axis(2, col.axis="black", las=2, cex.axis=0.7, tck=-.01)

legend(52, 11.5,  
       c("Year 1995 Males", "Year 1995 Females"),
       pch=c(1,1),
       lty=c(2, 2),
       cex=c(.9,.9),
       col=c("blue", "red"),
       bty = "n")

# Accuracy
plot(predict(lm.fit.1995.male), residuals(lm.fit.1995.male), col="blue")
plot(predict(lm.fit.1995.female), residuals(lm.fit.1995.female), col="red")
abline(h=0, col="red")


# Test dataset





abline(lm(data.YLD.2015.male ~ data.HALE.2015.male$val), col="blue", lty=1)
summary(lm(data.YLD.2015.male ~ data.HALE.2015.male$val))
confint(lm(data.YLD.2015.male ~ data.HALE.2015.male$val))

abline(lm(data.YLD.2015.female ~ data.HALE.2015.female$val), col="red", lty=1)
summary(lm(data.YLD.2015.female ~ data.HALE.2015.female$val))
confint(lm(data.YLD.2015.female ~ data.HALE.2015.female$val))


# Accuracy

