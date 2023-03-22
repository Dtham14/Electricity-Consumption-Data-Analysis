library(chron)
library(stats)
library(ggplot2)
library(ggbiplot)

path <- getwd()
setwd(path)
file <- read.table("./TermProjectData.txt", header = TRUE, sep = ",")

file$date <- as.POSIXlt(file$Date, format='%d/%m/%Y')

file$time <- chron(times = file$Time)

#2 Years
TrainingData <- with(file, file[(date >= '2006/12/16' & date < '2008/12/17'), ])
#1 Year
TestingData <- with(file, file[(date >= '2008/12/17' & date < '2009/12/01'), ])

#Scaled 2 year data set

FilteredTrainingData = subset(TrainingData, select = -c(Date, Time, date, time) )
scaleddata2 <- scale(FilteredTrainingData)

#Training Data set on Fridays 5-8pm

pca <- prcomp(na.omit(concatenatedTrainingWeeksPCA),center = TRUE, scale = TRUE)
print(pca)
summary(pca)  


pca.var <- pca$sdev^2
pca.var.per <- round(pca.var/sum(pca.var)*100, 1)
barplot(pca.var.per, main = "Scree Plot", xlab = "Principal Component", ylab = "Percent Variation")

bplot = ggbiplot(pcobj = pca,
                 choice = c(1,2),
                 obs.scale = 1, 
                 varname.size = 4,
                 var.axes = TRUE, 
                 labels.size=3, 
                 circle = TRUE,
                 ellipse = TRUE) 

TrainingPlot = bplot + labs(title = "PCA Plot of Electricity Consumption Data",
                            colour = "Features") + theme_get()

#Two Year Bulk Training Data set
pca2 <- prcomp(na.omit(scaleddata2),center = TRUE, scale = TRUE)
print(pca2)
summary(pca2)  

pca.var <- pca2$sdev^2
pca.var.per <- round(pca.var/sum(pca.var)*100, 1)
barplot(pca.var.per, main = "Scree Plot", xlab = "Principal Component", ylab = "Percent Variation")

bplot2 = ggbiplot(pcobj = pca2,
                  choice = c(1,2),
                  obs.scale = 1, 
                  varname.size = 4,
                  var.axes = TRUE, 
                  labels.size=3, 
                  circle = TRUE,
                  ellipse = TRUE) 

TwoYearPlot = bplot2 + labs(title = "PCA Plot of Electricity Consumption Data for 2 Years Every Day",
                            colour = "Features") + theme_get()

#this installs ggbiplot
install.packages("remotes")
remotes::install_github("vqv/ggbiplot")
Sys.setenv(R_REMOTES_NO_ERRORS_FROM_WARNINGS="true")
install_github("vqv/ggbiplot", force = TRUE)

#############################################################
#Training Data from 01/05/2007 to 12/13/2008 (about 2 years)#
#############################################################

# Week 1 
week1Friday <- with(TrainingData, TrainingData[(date >= '2007/01/05' & date < '2007/01/06'), ])
week1time <- with(week1Friday, week1Friday[(time >= '17:00:00' & time <= '20:00:00'), ])
week1data <- week1time[c(6,9)]
week1dataPCA <- week1time[3:9]

# Week 2 
week2Friday <- with(TrainingData, TrainingData[(date >= '2007/01/12' & date < '2007/01/13'), ])
week2time <- with(week2Friday, week2Friday[(time >= '17:00:00' & time <= '20:00:00'), ])
week2data <- week2time[c(6,9)]
week2dataPCA <- week2time[3:9]

# Week 3
week3Friday <- with(TrainingData, TrainingData[(date >= '2007/01/19' & date < '2007/01/20'), ])
week3time <- with(week3Friday, week3Friday[(time >= '17:00:00' & time <= '20:00:00'), ])
week3data <- week3time[c(6,9)]
week3dataPCA <- week3time[3:9]

# Week 4 
week4Friday <- with(TrainingData, TrainingData[(date >= '2007/01/26' & date < '2007/01/27'), ])
week4time <- with(week4Friday, week4Friday[(time >= '17:00:00' & time <= '20:00:00'), ])
week4data <- week4time[c(6,9)]
print(week4data)

# Week 5 
week5Friday <- with(TrainingData, TrainingData[(date >= '2007/02/02' & date < '2007/02/03'), ])
week5time <- with(week5Friday, week5Friday[(time >= '17:00:00' & time <= '20:00:00'), ])
week5data <- week5time[c(6,9)]
week5dataPCA <- week5time[3:9]

# Week 6
week6Friday <- with(TrainingData, TrainingData[(date >= '2007/02/09' & date < '2007/02/10'), ])
week6time <- with(week6Friday, week6Friday[(time >= '17:00:00' & time <= '20:00:00'), ])
week6data <- week6time[c(6,9)]
week6dataPCA <- week6time[3:9]

# Week 7 
week7Friday <- with(TrainingData, TrainingData[(date >= '2007/02/16' & date < '2007/02/17'), ])
week7time <- with(week7Friday, week7Friday[(time >= '17:00:00' & time <= '20:00:00'), ])
week7data <- week7time[c(6,9)]
week7dataPCA <- week7time[3:9]

# Week 8 
week8Friday <- with(TrainingData, TrainingData[(date >= '2007/02/23' & date < '2007/02/24'), ])
week8time <- with(week8Friday, week8Friday[(time >= '17:00:00' & time <= '20:00:00'), ])
week8data <- week8time[c(6,9)]
week8dataPCA <- week8time[3:9]

# Week 9
week9Friday <- with(TrainingData, TrainingData[(date >= '2007/03/02' & date < '2007/03/03'), ])
week9time <- with(week9Friday, week9Friday[(time >= '17:00:00' & time <= '20:00:00'), ])
week9data <- week9time[c(6,9)]
week9dataPCA <- week9time[3:9]

# Week 10
week10Friday <- with(TrainingData, TrainingData[(date >= '2007/03/09' & date < '2007/03/10'), ])
week10time <- with(week10Friday, week10Friday[(time >= '17:00:00' & time <= '20:00:00'), ])
week10data <- week10time[c(6,9)]
week10dataPCA <- week10time[3:9]

# Week 11
week11Friday <- with(TrainingData, TrainingData[(date >= '2007/03/16' & date < '2007/03/17'), ])
week11time <- with(week11Friday, week11Friday[(time >= '17:00:00' & time <= '20:00:00'), ])
week11data <- week11time[c(6,9)]
week11dataPCA <- week11time[3:9]

# Week 12 
week12Friday <- with(TrainingData, TrainingData[(date >= '2007/03/23' & date < '2007/03/24'), ])
week12time <- with(week12Friday, week12Friday[(time >= '17:00:00' & time <= '20:00:00'), ])
week12data <- week12time[c(6,9)]
week12dataPCA <- week12time[3:9]

# Week 13
week13Friday <- with(TrainingData, TrainingData[(date >= '2007/03/30' & date < '2007/03/31'), ])
week13time <- with(week13Friday, week13Friday[(time >= '17:00:00' & time <= '20:00:00'), ])
week13data <- week13time[c(6,9)]
week13dataPCA <- week13time[3:9]

# Week 14
week14Friday <- with(TrainingData, TrainingData[(date >= '2007/04/06' & date < '2007/04/07'), ])
week14time <- with(week14Friday, week14Friday[(time >= '17:00:00' & time <= '20:00:00'), ])
week14data <- week14time[c(6,9)]
week14dataPCA <- week14time[3:9]

# Week 15 
week15Friday <- with(TrainingData, TrainingData[(date >= '2007/04/13' & date < '2007/04/14'), ])
week15time <- with(week15Friday, week15Friday[(time >= '17:00:00' & time <= '20:00:00'), ])
week15data <- week15time[c(6,9)]
week15dataPCA <- week15time[3:9]

# Week 16
week16Friday <- with(TrainingData, TrainingData[(date >= '2007/04/20' & date < '2007/04/21'), ])
week16time <- with(week16Friday, week16Friday[(time >= '17:00:00' & time <= '20:00:00'), ])
week16data <- week16time[c(6,9)]
week16dataPCA <- week16time[3:9]

# Week 17
week17Friday <- with(TrainingData, TrainingData[(date >= '2007/04/27' & date < '2007/04/28'), ])
week17time <- with(week17Friday, week17Friday[(time >= '17:00:00' & time <= '20:00:00'), ])
week17data <- week17time[c(6,9)]
week17dataPCA <- week17time[3:9]

# Week 18 
week18Friday <- with(TrainingData, TrainingData[(date >= '2007/05/04' & date < '2007/05/05'), ])
week18time <- with(week18Friday, week18Friday[(time >= '17:00:00' & time <= '20:00:00'), ])
week18data <- week18time[c(6,9)]
week18dataPCA <- week18time[3:9]

# Week 19
week19Friday <- with(TrainingData, TrainingData[(date >= '2007/05/11' & date < '2007/05/12'), ])
week19time <- with(week19Friday, week19Friday[(time >= '17:00:00' & time <= '20:00:00'), ])
week19data <- week19time[c(6,9)]
week19dataPCA <- week19time[3:9]

# Week 20
week20Friday <- with(TrainingData, TrainingData[(date >= '2007/05/18' & date < '2007/05/19'), ])
week20time <- with(week20Friday, week20Friday[(time >= '17:00:00' & time <= '20:00:00'), ])
week20data <- week20time[c(6,9)]
week20dataPCA <- week20time[3:9]

# Week 21 
week21Friday <- with(TrainingData, TrainingData[(date >= '2007/05/25' & date < '2007/05/26'), ])
week21time <- with(week21Friday, week21Friday[(time >= '17:00:00' & time <= '20:00:00'), ])
week21data <- week21time[c(6,9)]
week21dataPCA <- week21time[3:9]

# Week 22 
week22Friday <- with(TrainingData, TrainingData[(date >= '2007/06/01' & date < '2007/06/02'), ])
week22time <- with(week22Friday, week22Friday[(time >= '17:00:00' & time <= '20:00:00'), ])
week22data <- week22time[c(6,9)]
week22dataPCA <- week22time[3:9]

# Week 23 
week23Friday <- with(TrainingData, TrainingData[(date >= '2007/06/08' & date < '2007/06/09'), ])
week23time <- with(week23Friday, week23Friday[(time >= '17:00:00' & time <= '20:00:00'), ])
week23data <- week23time[c(6,9)]
week23dataPCA <- week23time[3:9]

# Week 24
week24Friday <- with(TrainingData, TrainingData[(date >= '2007/06/15' & date < '2007/06/16'), ])
week24time <- with(week24Friday, week24Friday[(time >= '17:00:00' & time <= '20:00:00'), ])
week24data <- week24time[c(6,9)]
week24dataPCA <- week24time[3:9]

# Week 25 
week25Friday <- with(TrainingData, TrainingData[(date >= '2007/06/22' & date < '2007/06/23'), ])
week25time <- with(week25Friday, week25Friday[(time >= '17:00:00' & time <= '20:00:00'), ])
week25data <- week25time[c(6,9)]
week25dataPCA <- week25time[3:9]

# Week 26
week26Friday <- with(TrainingData, TrainingData[(date >= '2007/06/29' & date < '2007/06/30'), ])
week26time <- with(week26Friday, week26Friday[(time >= '17:00:00' & time <= '20:00:00'), ])
week26data <- week26time[c(6,9)]
week26dataPCA <- week26time[3:9]

# Week 27 
week27Friday <- with(TrainingData, TrainingData[(date >= '2007/07/06' & date < '2007/07/07'), ])
week27time <- with(week27Friday, week27Friday[(time >= '17:00:00' & time <= '20:00:00'), ])
week27data <- week27time[c(6,9)]
week27dataPCA <- week27time[3:9]

# Week 28 
week28Friday <- with(TrainingData, TrainingData[(date >= '2007/07/13' & date < '2007/07/14'), ])
week28time <- with(week28Friday, week28Friday[(time >= '17:00:00' & time <= '20:00:00'), ])
week28data <- week28time[c(6,9)]
week28dataPCA <- week28time[3:9]

# Week 29
week29Friday <- with(TrainingData, TrainingData[(date >= '2007/07/20' & date < '2007/07/21'), ])
week29time <- with(week29Friday, week29Friday[(time >= '17:00:00' & time <= '20:00:00'), ])
week29data <- week29time[c(6,9)]
week29dataPCA <- week29time[3:9]

# Week 30
week30Friday <- with(TrainingData, TrainingData[(date >= '2007/07/27' & date < '2007/07/28'), ])
week30time <- with(week30Friday, week30Friday[(time >= '17:00:00' & time <= '20:00:00'), ])
week30data <- week30time[c(6,9)]
week30dataPCA <- week30time[3:9]

# Week 31
week31Friday <- with(TrainingData, TrainingData[(date >= '2007/08/03' & date < '2007/08/04'), ])
week31time <- with(week31Friday, week31Friday[(time >= '17:00:00' & time <= '20:00:00'), ])
week31data <- week31time[c(6,9)]
week31dataPCA <- week31time[3:9]

# Week 32 
week32Friday <- with(TrainingData, TrainingData[(date >= '2007/08/10' & date < '2007/08/11'), ])
week32time <- with(week32Friday, week32Friday[(time >= '17:00:00' & time <= '20:00:00'), ])
week32data <- week32time[c(6,9)]
week32dataPCA <- week32time[3:9]

# Week 33
week33Friday <- with(TrainingData, TrainingData[(date >= '2007/08/17' & date < '2007/08/18'), ])
week33time <- with(week33Friday, week33Friday[(time >= '17:00:00' & time <= '20:00:00'), ])
week33data <- week33time[c(6,9)]
week33dataPCA <- week33time[3:9]

# Week 34
week34Friday <- with(TrainingData, TrainingData[(date >= '2007/08/24' & date < '2007/08/25'), ])
week34time <- with(week34Friday, week34Friday[(time >= '17:00:00' & time <= '20:00:00'), ])
week34data <- week34time[c(6,9)]
week34dataPCA <- week34time[3:9]

# Week 35 
week35Friday <- with(TrainingData, TrainingData[(date >= '2007/08/31' & date < '2007/09/01'), ])
week35time <- with(week35Friday, week35Friday[(time >= '17:00:00' & time <= '20:00:00'), ])
week35data <- week35time[c(6,9)]
week35dataPCA <- week35time[3:9]

# Week 36 
week36Friday <- with(TrainingData, TrainingData[(date >= '2007/09/07' & date < '2007/09/08'), ])
week36time <- with(week36Friday, week36Friday[(time >= '17:00:00' & time <= '20:00:00'), ])
week36data <- week36time[c(6,9)]
week36dataPCA <- week36time[3:9]

# Week 37 
week37Friday <- with(TrainingData, TrainingData[(date >= '2007/09/14' & date < '2007/09/15'), ])
week37time <- with(week37Friday, week37Friday[(time >= '17:00:00' & time <= '20:00:00'), ])
week37data <- week37time[c(6,9)]
week37dataPCA <- week37time[3:9]

# Week 38 
week38Friday <- with(TrainingData, TrainingData[(date >= '2007/09/21' & date < '2007/09/22'), ])
week38time <- with(week38Friday, week38Friday[(time >= '17:00:00' & time <= '20:00:00'), ])
week38data <- week38time[c(6,9)]
week38dataPCA <- week38time[3:9]

# Week 39 
week39Friday <- with(TrainingData, TrainingData[(date >= '2007/09/28' & date < '2007/09/29'), ])
week39time <- with(week39Friday, week39Friday[(time >= '17:00:00' & time <= '20:00:00'), ])
week39data <- week39time[c(6,9)]
week39dataPCA <- week39time[3:9]

# Week 40
week40Friday <- with(TrainingData, TrainingData[(date >= '2007/10/05' & date < '2007/10/06'), ])
week40time <- with(week40Friday, week40Friday[(time >= '17:00:00' & time <= '20:00:00'), ])
week40data <- week40time[c(6,9)]
week40dataPCA <- week40time[3:9]

# Week 41
week41Friday <- with(TrainingData, TrainingData[(date >= '2007/10/12' & date < '2007/10/13'), ])
week41time <- with(week41Friday, week41Friday[(time >= '17:00:00' & time <= '20:00:00'), ])
week41data <- week41time[c(6,9)]
week41dataPCA <- week41time[3:9]

# Week 42 
week42Friday <- with(TrainingData, TrainingData[(date >= '2007/10/19' & date < '2007/10/20'), ])
week42time <- with(week42Friday, week42Friday[(time >= '17:00:00' & time <= '20:00:00'), ])
week42data <- week42time[c(6,9)]
week42dataPCA <- week42time[3:9]

# Week 43
week43Friday <- with(TrainingData, TrainingData[(date >= '2007/10/26' & date < '2007/10/27'), ])
week43time <- with(week43Friday, week43Friday[(time >= '17:00:00' & time <= '20:00:00'), ])
week43data <- week43time[c(6,9)]
week43dataPCA <- week43time[3:9]

# Week 44
week44Friday <- with(TrainingData, TrainingData[(date >= '2007/11/02' & date < '2007/11/03'), ])
week44time <- with(week44Friday, week44Friday[(time >= '17:00:00' & time <= '20:00:00'), ])
week44data <- week44time[c(6,9)]
week44dataPCA <- week44time[3:9]

# Week 45 
week45Friday <- with(TrainingData, TrainingData[(date >= '2007/11/09' & date < '2007/11/10'), ])
week45time <- with(week45Friday, week45Friday[(time >= '17:00:00' & time <= '20:00:00'), ])
week45data <- week45time[c(6,9)]
week45dataPCA <- week45time[3:9]

# Week 46 
week46Friday <- with(TrainingData, TrainingData[(date >= '2007/11/16' & date < '2007/11/17'), ])
week46time <- with(week46Friday, week46Friday[(time >= '17:00:00' & time <= '20:00:00'), ])
week46data <- week46time[c(6,9)]
week46dataPCA <- week46time[3:9]

# Week 47
week47Friday <- with(TrainingData, TrainingData[(date >= '2007/11/23' & date < '2007/11/24'), ])
week47time <- with(week47Friday, week47Friday[(time >= '17:00:00' & time <= '20:00:00'), ])
week47data <- week47time[c(6,9)]
week47dataPCA <- week47time[3:9]

# Week 48 
week48Friday <- with(TrainingData, TrainingData[(date >= '2007/11/30' & date < '2007/12/01'), ])
week48time <- with(week48Friday, week48Friday[(time >= '17:00:00' & time <= '20:00:00'), ])
week48data <- week48time[c(6,9)]
week48dataPCA <- week48time[3:9]

# Week 49 
week49Friday <- with(TrainingData, TrainingData[(date >= '2007/12/07' & date < '2007/12/08'), ])
week49time <- with(week49Friday, week49Friday[(time >= '17:00:00' & time <= '20:00:00'), ])
week49data <- week49time[c(6,9)]
week49dataPCA <- week49time[3:9]

# Week 50 
week50Friday <- with(TrainingData, TrainingData[(date >= '2007/12/14' & date < '2007/12/15'), ])
week50time <- with(week50Friday, week50Friday[(time >= '17:00:00' & time <= '20:00:00'), ])
week50data <- week50time[c(6,9)]
week50dataPCA <- week50time[3:9]

# Week 51
week51Friday <- with(TrainingData, TrainingData[(date >= '2007/12/21' & date < '2007/12/22'), ])
week51time <- with(week51Friday, week51Friday[(time >= '17:00:00' & time <= '20:00:00'), ])
week51data <- week51time[c(6,9)]
week51dataPCA <- week51time[3:9]

# Week 52
week52Friday <- with(TrainingData, TrainingData[(date >= '2007/12/28' & date < '2007/12/29'), ])
week52time <- with(week52Friday, week52Friday[(time >= '17:00:00' & time <= '20:00:00'), ])
week52data <- week52time[c(6,9)]
week52dataPCA <- week52time[3:9]

# Week 53 
week53Friday <- with(TrainingData, TrainingData[(date >= '2008/01/04' & date < '2008/01/05'), ])
week53time <- with(week53Friday, week53Friday[(time >= '17:00:00' & time <= '20:00:00'), ])
week53data <- week53time[c(6,9)]
week53dataPCA <- week53time[3:9]

# Week 54
week54Friday <- with(TrainingData, TrainingData[(date >= '2008/01/11' & date < '2008/01/12'), ])
week54time <- with(week54Friday, week54Friday[(time >= '17:00:00' & time <= '20:00:00'), ])
week54data <- week54time[c(6,9)]
week54dataPCA <- week54time[3:9]

# Week 55
week55Friday <- with(TrainingData, TrainingData[(date >= '2008/01/18' & date < '2008/01/19'), ])
week55time <- with(week55Friday, week55Friday[(time >= '17:00:00' & time <= '20:00:00'), ])
week55data <- week55time[c(6,9)]
week55dataPCA <- week55time[3:9]

# Week 56 
week56Friday <- with(TrainingData, TrainingData[(date >= '2008/01/25' & date < '2008/01/26'), ])
week56time <- with(week56Friday, week56Friday[(time >= '17:00:00' & time <= '20:00:00'), ])
week56data <- week56time[c(6,9)]
week56dataPCA <- week56time[3:9]

# Week 57
week57Friday <- with(TrainingData, TrainingData[(date >= '2008/02/01' & date < '2008/02/02'), ])
week57time <- with(week57Friday, week57Friday[(time >= '17:00:00' & time <= '20:00:00'), ])
week57data <- week57time[c(6,9)]
week57dataPCA <- week57time[3:9]

# Week 58
week58Friday <- with(TrainingData, TrainingData[(date >= '2008/02/08' & date < '2008/02/09'), ])
week58time <- with(week58Friday, week58Friday[(time >= '17:00:00' & time <= '20:00:00'), ])
week58data <- week58time[c(6,9)]
week58dataPCA <- week58time[3:9]

# Week 59 
week59Friday <- with(TrainingData, TrainingData[(date >= '2008/02/15' & date < '2008/02/16'), ])
week59time <- with(week59Friday, week59Friday[(time >= '17:00:00' & time <= '20:00:00'), ])
week59data <- week59time[c(6,9)]
week59dataPCA <- week59time[3:9]

# Week 60 
week60Friday <- with(TrainingData, TrainingData[(date >= '2008/02/22' & date < '2008/02/23'), ])
week60time <- with(week60Friday, week60Friday[(time >= '17:00:00' & time <= '20:00:00'), ])
week60data <- week60time[c(6,9)]
week60dataPCA <- week60time[3:9]

# Week 61
week61Friday <- with(TrainingData, TrainingData[(date >= '2008/02/29' & date < '2008/03/01'), ])
week61time <- with(week61Friday, week61Friday[(time >= '17:00:00' & time <= '20:00:00'), ])
week61data <- week61time[c(6,9)]
week61dataPCA <- week61time[3:9]

# Week 62
week62Friday <- with(TrainingData, TrainingData[(date >= '2008/03/07' & date < '2008/03/08'), ])
week62time <- with(week62Friday, week62Friday[(time >= '17:00:00' & time <= '20:00:00'), ])
week62data <- week62time[c(6,9)]
week62dataPCA <- week62time[3:9]

# Week 63 
week63Friday <- with(TrainingData, TrainingData[(date >= '2008/03/14' & date < '2008/03/15'), ])
week63time <- with(week63Friday, week63Friday[(time >= '17:00:00' & time <= '20:00:00'), ])
week63data <- week63time[c(6,9)]
week63dataPCA <- week63time[3:9]

# Week 64
week64Friday <- with(TrainingData, TrainingData[(date >= '2008/03/21' & date < '2008/03/22'), ])
week64time <- with(week64Friday, week64Friday[(time >= '17:00:00' & time <= '20:00:00'), ])
week64data <- week64time[c(6,9)]
week64dataPCA <- week64time[3:9]

# Week 65
week65Friday <- with(TrainingData, TrainingData[(date >= '2008/03/28' & date < '2008/03/29'), ])
week65time <- with(week65Friday, week65Friday[(time >= '17:00:00' & time <= '20:00:00'), ])
week65data <- week65time[c(6,9)]
week65dataPCA <- week65time[3:9]

# Week 66 
week66Friday <- with(TrainingData, TrainingData[(date >= '2008/04/04' & date < '2008/04/05'), ])
week66time <- with(week66Friday, week66Friday[(time >= '17:00:00' & time <= '20:00:00'), ])
week66data <- week66time[c(6,9)]
week66dataPCA <- week66time[3:9]

# Week 67
week67Friday <- with(TrainingData, TrainingData[(date >= '2008/04/11' & date < '2008/04/12'), ])
week67time <- with(week67Friday, week67Friday[(time >= '17:00:00' & time <= '20:00:00'), ])
week67data <- week67time[c(6,9)]
week67dataPCA <- week67time[3:9]

# Week 68
week68Friday <- with(TrainingData, TrainingData[(date >= '2008/04/18' & date < '2008/04/19'), ])
week68time <- with(week68Friday, week68Friday[(time >= '17:00:00' & time <= '20:00:00'), ])
week68data <- week68time[c(6,9)]
week68dataPCA <- week68time[3:9]

# Week 69 
week69Friday <- with(TrainingData, TrainingData[(date >= '2008/04/25' & date < '2008/04/26'), ])
week69time <- with(week69Friday, week69Friday[(time >= '17:00:00' & time <= '20:00:00'), ])
week69data <- week69time[c(6,9)]
week69dataPCA <- week69time[3:9]

# Week 70 
week70Friday <- with(TrainingData, TrainingData[(date >= '2008/05/02' & date < '2008/05/03'), ])
week70time <- with(week70Friday, week70Friday[(time >= '17:00:00' & time <= '20:00:00'), ])
week70data <- week70time[c(6,9)]
week70dataPCA <- week70time[3:9]

# Week 71
week71Friday <- with(TrainingData, TrainingData[(date >= '2008/05/09' & date < '2008/05/10'), ])
week71time <- with(week71Friday, week71Friday[(time >= '17:00:00' & time <= '20:00:00'), ])
week71data <- week71time[c(6,9)]
week71dataPCA <- week71time[3:9]

# Week 72
week72Friday <- with(TrainingData, TrainingData[(date >= '2008/05/16' & date < '2008/05/17'), ])
week72time <- with(week72Friday, week72Friday[(time >= '17:00:00' & time <= '20:00:00'), ])
week72data <- week72time[c(6,9)]
week72dataPCA <- week72time[3:9]

# Week 73 
week73Friday <- with(TrainingData, TrainingData[(date >= '2008/05/23' & date < '2008/05/24'), ])
week73time <- with(week73Friday, week73Friday[(time >= '17:00:00' & time <= '20:00:00'), ])
week73data <- week73time[c(6,9)]
week73dataPCA <- week73time[3:9]

# Week 74
week74Friday <- with(TrainingData, TrainingData[(date >= '2008/05/30' & date < '2008/05/31'), ])
week74time <- with(week74Friday, week74Friday[(time >= '17:00:00' & time <= '20:00:00'), ])
week74data <- week74time[c(6,9)]
week74dataPCA <- week74time[3:9]

# Week 75
week75Friday <- with(TrainingData, TrainingData[(date >= '2008/06/06' & date < '2008/06/07'), ])
week75time <- with(week75Friday, week75Friday[(time >= '17:00:00' & time <= '20:00:00'), ])
week75data <- week75time[c(6,9)]
week75dataPCA <- week75time[3:9]

# Week 76 
week76Friday <- with(TrainingData, TrainingData[(date >= '2008/06/13' & date < '2008/06/14'), ])
week76time <- with(week76Friday, week76Friday[(time >= '17:00:00' & time <= '20:00:00'), ])
week76data <- week76time[c(6,9)]
week76dataPCA <- week76time[3:9]

# Week 77
week77Friday <- with(TrainingData, TrainingData[(date >= '2008/06/20' & date < '2008/06/21'), ])
week77time <- with(week77Friday, week77Friday[(time >= '17:00:00' & time <= '20:00:00'), ])
week77data <- week57time[c(6,9)]
week77dataPCA <- week77time[3:9]

# Week 78
week78Friday <- with(TrainingData, TrainingData[(date >= '2008/06/27' & date < '2008/06/28'), ])
week78time <- with(week78Friday, week78Friday[(time >= '17:00:00' & time <= '20:00:00'), ])
week78data <- week78time[c(6,9)]
week78dataPCA <- week78time[3:9]

# Week 79 
week79Friday <- with(TrainingData, TrainingData[(date >= '2008/07/04' & date < '2008/07/05'), ])
week79time <- with(week79Friday, week79Friday[(time >= '17:00:00' & time <= '20:00:00'), ])
week79data <- week79time[c(6,9)]
week79dataPCA <- week79time[3:9]

# Week 80 
week80Friday <- with(TrainingData, TrainingData[(date >= '2008/07/11' & date < '2008/07/12'), ])
week80time <- with(week80Friday, week80Friday[(time >= '17:00:00' & time <= '20:00:00'), ])
week80data <- week80time[c(6,9)]
week80dataPCA <- week80time[3:9]

# Week 81
week81Friday <- with(TrainingData, TrainingData[(date >= '2008/07/18' & date < '2008/07/19'), ])
week81time <- with(week81Friday, week81Friday[(time >= '17:00:00' & time <= '20:00:00'), ])
week81data <- week81time[c(6,9)]
week81dataPCA <- week81time[3:9]

# Week 82
week82Friday <- with(TrainingData, TrainingData[(date >= '2008/07/25' & date < '2008/07/26'), ])
week82time <- with(week82Friday, week82Friday[(time >= '17:00:00' & time <= '20:00:00'), ])
week82data <- week82time[c(6,9)]
week82dataPCA <- week82time[3:9]

# Week 83 
week83Friday <- with(TrainingData, TrainingData[(date >= '2008/08/01' & date < '2008/08/02'), ])
week83time <- with(week83Friday, week83Friday[(time >= '17:00:00' & time <= '20:00:00'), ])
week83data <- week83time[c(6,9)]
week83dataPCA <- week83time[3:9]

# Week 84
week84Friday <- with(TrainingData, TrainingData[(date >= '2008/08/08' & date < '2008/08/09'), ])
week84time <- with(week84Friday, week84Friday[(time >= '17:00:00' & time <= '20:00:00'), ])
week84data <- week84time[c(6,9)]
week84dataPCA <- week84time[3:9]

# Week 85
week85Friday <- with(TrainingData, TrainingData[(date >= '2008/08/15' & date < '2008/08/16'), ])
week85time <- with(week85Friday, week85Friday[(time >= '17:00:00' & time <= '20:00:00'), ])
week85data <- week85time[c(6,9)]
week85dataPCA <- week85time[3:9]

# Week 86 
week86Friday <- with(TrainingData, TrainingData[(date >= '2008/08/22' & date < '2008/08/23'), ])
week86time <- with(week86Friday, week86Friday[(time >= '17:00:00' & time <= '20:00:00'), ])
week86data <- week86time[c(6,9)]
week86dataPCA <- week86time[3:9]

# Week 87
week87Friday <- with(TrainingData, TrainingData[(date >= '2008/08/29' & date < '2008/08/30'), ])
week87time <- with(week87Friday, week87Friday[(time >= '17:00:00' & time <= '20:00:00'), ])
week87data <- week87time[c(6,9)]
week87dataPCA <- week87time[3:9]

# Week 88
week88Friday <- with(TrainingData, TrainingData[(date >= '2008/09/05' & date < '2008/09/06'), ])
week88time <- with(week88Friday, week88Friday[(time >= '17:00:00' & time <= '20:00:00'), ])
week88data <- week88time[c(6,9)]
week88dataPCA <- week88time[3:9]

# Week 89 
week89Friday <- with(TrainingData, TrainingData[(date >= '2008/09/12' & date < '2008/09/13'), ])
week89time <- with(week89Friday, week89Friday[(time >= '17:00:00' & time <= '20:00:00'), ])
week89data <- week89time[c(6,9)]
week89dataPCA <- week89time[3:9]

# Week 90 
week90Friday <- with(TrainingData, TrainingData[(date >= '2008/09/19' & date < '2008/09/20'), ])
week90time <- with(week90Friday, week90Friday[(time >= '17:00:00' & time <= '20:00:00'), ])
week90data <- week90time[c(6,9)]
week90dataPCA <- week90time[3:9]

# Week 91
week91Friday <- with(TrainingData, TrainingData[(date >= '2008/09/26' & date < '2008/09/27'), ])
week91time <- with(week91Friday, week91Friday[(time >= '17:00:00' & time <= '20:00:00'), ])
week91data <- week91time[c(6,9)]
week91dataPCA <- week91time[3:9]

# Week 92
week92Friday <- with(TrainingData, TrainingData[(date >= '2008/10/03' & date < '2008/10/04'), ])
week92time <- with(week92Friday, week92Friday[(time >= '17:00:00' & time <= '20:00:00'), ])
week92data <- week92time[c(6,9)]
week92dataPCA <- week92time[3:9]

# Week 93 
week93Friday <- with(TrainingData, TrainingData[(date >= '2008/10/12' & date < '2008/10/13'), ])
week93time <- with(week93Friday, week93Friday[(time >= '17:00:00' & time <= '20:00:00'), ])
week93data <- week93time[c(6,9)]
week93dataPCA <- week93time[3:9]

# Week 94
week94Friday <- with(TrainingData, TrainingData[(date >= '2008/10/10' & date < '2008/10/11'), ])
week94time <- with(week94Friday, week94Friday[(time >= '17:00:00' & time <= '20:00:00'), ])
week94data <- week94time[c(6,9)]
week94dataPCA <- week94time[3:9]

# Week 95
week95Friday <- with(TrainingData, TrainingData[(date >= '2008/10/17' & date < '2008/10/18'), ])
week95time <- with(week95Friday, week95Friday[(time >= '17:00:00' & time <= '20:00:00'), ])
week95data <- week95time[c(6,9)]
week95dataPCA <- week95time[3:9]

# Week 96 
week96Friday <- with(TrainingData, TrainingData[(date >= '2008/10/24' & date < '2008/10/25'), ])
week96time <- with(week96Friday, week96Friday[(time >= '17:00:00' & time <= '20:00:00'), ])
week96data <- week96time[c(6,9)]
week96dataPCA <- week96time[3:9]

# Week 97
week97Friday <- with(TrainingData, TrainingData[(date >= '2008/10/31' & date < '2008/11/01'), ])
week97time <- with(week97Friday, week97Friday[(time >= '17:00:00' & time <= '20:00:00'), ])
week97data <- week97time[c(6,9)]
week97dataPCA <- week97time[3:9]

# Week 98
week98Friday <- with(TrainingData, TrainingData[(date >= '2008/11/07' & date < '2008/11/08'), ])
week98time <- with(week98Friday, week98Friday[(time >= '17:00:00' & time <= '20:00:00'), ])
week98data <- week98time[c(6,9)]
week98dataPCA <- week98time[3:9]

# Week 99 
week99Friday <- with(TrainingData, TrainingData[(date >= '2008/11/14' & date < '2008/11/15'), ])
week99time <- with(week99Friday, week99Friday[(time >= '17:00:00' & time <= '20:00:00'), ])
week99data <- week99time[c(6,9)]
week99dataPCA <- week99time[3:9]

# Week 100 
week100Friday <- with(TrainingData, TrainingData[(date >= '2008/11/21' & date < '2008/11/22'), ])
week100time <- with(week100Friday, week100Friday[(time >= '17:00:00' & time <= '20:00:00'), ])
week100data <- week100time[c(6,9)]
week100dataPCA <- week100time[3:9]

# Week 101
week101Friday <- with(TrainingData, TrainingData[(date >= '2008/11/28' & date < '2008/11/29'), ])
week101time <- with(week101Friday, week101Friday[(time >= '17:00:00' & time <= '20:00:00'), ])
week101data <- week101time[c(6,9)]
week101dataPCA <- week101time[3:9]

# Week 102
week102Friday <- with(TrainingData, TrainingData[(date >= '2008/12/05' & date < '2008/12/06'), ])
week102time <- with(week102Friday, week102Friday[(time >= '17:00:00' & time <= '20:00:00'), ])
week102data <- week102time[c(6,9)]
week102dataPCA <- week102time[3:9]

# Week 103 
week103Friday <- with(TrainingData, TrainingData[(date >= '2008/12/12' & date < '2008/12/13'), ])
week103time <- with(week103Friday, week103Friday[(time >= '17:00:00' & time <= '20:00:00'), ])
week103data <- week103time[c(6,9)]
week103dataPCA <- week103time[3:9]



concatenatedTrainingWeeks = rbind(week1data,week2data,week3data,week4data,week5data,week6data,
                                  week7data,week8data,week9data,week10data,week11data,week12data,
                                  week13data,week14data,week15data,week16data,week17data,week18data,
                                  week19data,week20data,week21data,week22data,week23data,week24data,
                                  week25data,week26data,week27data,week28data,week29data,week30data,
                                  week31data,week32data,week33data,week34data,week35data,week36data,
                                  week37data,week38data,week39data,week40data,week41data,week42data,
                                  week43data,week44data,week45data,week46data,week47data,week48data,
                                  week49data,week50data,week51data,week52data,week53data,week54data,
                                  week55data,week56data,week57data,week58data,week59data,week60data,
                                  week61data,week62data,week63data,week64data,week65data,week66data,
                                  week57data,week58data,week59data,week60data,week51data,week52data,
                                  week63data,week64data,week65data,week66data,week67data,week68data,
                                  week69data,week70data,week71data,week72data,week73data,week74data,
                                  week75data,week76data,week77data,week78data,week79data,week80data,
                                  week81data,week82data,week83data,week84data,week85data,week86data,
                                  week87data,week88data,week89data,week90data,week91data,week92data,
                                  week93data,week94data,week95data,week96data,week97data,week98data,
                                  week99data,week100data,week101data,week102data,week103data)


concatenatedTrainingWeeksPCA = rbind(week1dataPCA,week2dataPCA,week3dataPCA,week4dataPCA,week5dataPCA,week6dataPCA,
                                  week7dataPCA,week8dataPCA,week9dataPCA,week10dataPCA,week11dataPCA,week12dataPCA,
                                  week13dataPCA,week14dataPCA,week15dataPCA,week16dataPCA,week17dataPCA,week18dataPCA,
                                  week19dataPCA,week20dataPCA,week21dataPCA,week22dataPCA,week23dataPCA,week24dataPCA,
                                  week25dataPCA,week26dataPCA,week27dataPCA,week28dataPCA,week29dataPCA,week30dataPCA,
                                  week31dataPCA,week32dataPCA,week33dataPCA,week34dataPCA,week35dataPCA,week36dataPCA,
                                  week37dataPCA,week38dataPCA,week39dataPCA,week40dataPCA,week41dataPCA,week42dataPCA,
                                  week43dataPCA,week44dataPCA,week45dataPCA,week46dataPCA,week47dataPCA,week48dataPCA,
                                  week49dataPCA,week50dataPCA,week51dataPCA,week52dataPCA,week53dataPCA,week54dataPCA,
                                  week55dataPCA,week56dataPCA,week57dataPCA,week58dataPCA,week59dataPCA,week60dataPCA,
                                  week61dataPCA,week62dataPCA,week63dataPCA,week64dataPCA,week65dataPCA,week66dataPCA,
                                  week57dataPCA,week58dataPCA,week59dataPCA,week60dataPCA,week51dataPCA,week52dataPCA,
                                  week63dataPCA,week64dataPCA,week65dataPCA,week66dataPCA,week67dataPCA,week68dataPCA,
                                  week69dataPCA,week70dataPCA,week71dataPCA,week72dataPCA,week73dataPCA,week74dataPCA,
                                  week75dataPCA,week76dataPCA,week77dataPCA,week78dataPCA,week79dataPCA,week80dataPCA,
                                  week81dataPCA,week82dataPCA,week83dataPCA,week84dataPCA,week85dataPCA,week86dataPCA,
                                  week87dataPCA,week88dataPCA,week89dataPCA,week90dataPCA,week91dataPCA,week92dataPCA,
                                  week93dataPCA,week94dataPCA,week95dataPCA,week96dataPCA,week97dataPCA,week98dataPCA,
                                  week99dataPCA,week100dataPCA,week101dataPCA,week102dataPCA,week103dataPCA)

###############################################
#             Testing Data                    #
###############################################

# Week 1 
week1FridayTest <- with(TestingData, TestingData[(date >= '2008/12/19' & date < '2008/12/20'), ])
week1timeTest <- with(week1FridayTest, week1FridayTest[(time >= '17:00:00' & time <= '20:00:00'), ])
week1dataTest <- week1timeTest[c(6,9)]
print(week1dataTest)

# Week 2
week2FridayTest <- with(TestingData, TestingData[(date >= '2008/12/26' & date < '2008/12/27'), ])
week2timeTest <- with(week2FridayTest, week2FridayTest[(time >= '17:00:00' & time <= '20:00:00'), ])
week2dataTest <- week2timeTest[c(6,9)]
print(week2dataTest)

# Week 3
week3FridayTest <- with(TestingData, TestingData[(date >= '2009/01/02' & date < '2009/01/03'), ])
week3timeTest <- with(week3FridayTest, week3FridayTest[(time >= '17:00:00' & time <= '20:00:00'), ])
week3dataTest <- week3timeTest[c(6,9)]
print(week3dataTest)

# Week 4
week4FridayTest <- with(TestingData, TestingData[(date >= '2009/01/09' & date < '2009/01/10'), ])
week4timeTest <- with(week4FridayTest, week4FridayTest[(time >= '17:00:00' & time <= '20:00:00'), ])
week4dataTest <- week4timeTest[c(6,9)]
print(week4dataTest)

# Week 5
week5FridayTest <- with(TestingData, TestingData[(date >= '2009/01/16' & date < '2009/01/17'), ])
week5timeTest <- with(week5FridayTest, week5FridayTest[(time >= '17:00:00' & time <= '20:00:00'), ])
week5dataTest <- week5timeTest[c(6,9)]
print(week5dataTest)

# Week 6
week6FridayTest <- with(TestingData, TestingData[(date >= '2009/01/23' & date < '2009/01/24'), ])
week6timeTest <- with(week6FridayTest, week6FridayTest[(time >= '17:00:00' & time <= '20:00:00'), ])
week6dataTest <- week6timeTest[c(6,9)]
print(week6dataTest)

# Week 7
week7FridayTest <- with(TestingData, TestingData[(date >= '2009/01/30' & date < '2009/01/31'), ])
week7timeTest <- with(week7FridayTest, week7FridayTest[(time >= '17:00:00' & time <= '20:00:00'), ])
week7dataTest <- week7timeTest[c(6,9)]
print(week7dataTest)

# Week 8
week8FridayTest <- with(TestingData, TestingData[(date >= '2009/02/06' & date < '2009/02/07'), ])
week8timeTest <- with(week8FridayTest, week8FridayTest[(time >= '17:00:00' & time <= '20:00:00'), ])
week8dataTest <- week8timeTest[c(6,9)]
print(week8dataTest)

# Week 9
week9FridayTest <- with(TestingData, TestingData[(date >= '2009/02/13' & date < '2009/02/14'), ])
week9timeTest <- with(week9FridayTest, week9FridayTest[(time >= '17:00:00' & time <= '20:00:00'), ])
week9dataTest <- week9timeTest[c(6,9)]
print(week9dataTest)

# Week 10
week10FridayTest <- with(TestingData, TestingData[(date >= '2009/02/20' & date < '2009/02/21'), ])
week10timeTest <- with(week10FridayTest, week10FridayTest[(time >= '17:00:00' & time <= '20:00:00'), ])
week10dataTest <- week10timeTest[c(6,9)]
print(week10dataTest)

# Week 11
week11FridayTest <- with(TestingData, TestingData[(date >= '2009/02/27' & date < '2009/02/28'), ])
week11timeTest <- with(week11FridayTest, week11FridayTest[(time >= '17:00:00' & time <= '20:00:00'), ])
week11dataTest <- week11timeTest[c(6,9)]
print(week11dataTest)

# Week 12
week12FridayTest <- with(TestingData, TestingData[(date >= '2009/03/06' & date < '2009/03/07'), ])
week12timeTest <- with(week12FridayTest, week12FridayTest[(time >= '17:00:00' & time <= '20:00:00'), ])
week12dataTest <- week12timeTest[c(6,9)]
print(week12dataTest)

# Week 13
week13FridayTest <- with(TestingData, TestingData[(date >= '2009/03/13' & date < '2009/03/14'), ])
week13timeTest <- with(week13FridayTest, week13FridayTest[(time >= '17:00:00' & time <= '20:00:00'), ])
week13dataTest <- week13timeTest[c(6,9)]
print(week13dataTest)

# Week 14
week14FridayTest <- with(TestingData, TestingData[(date >= '2009/03/20' & date < '2009/03/21'), ])
week14timeTest <- with(week14FridayTest, week14FridayTest[(time >= '17:00:00' & time <= '20:00:00'), ])
week14dataTest <- week14timeTest[c(6,9)]
print(week14dataTest)

# Week 15
week15FridayTest <- with(TestingData, TestingData[(date >= '2009/03/27' & date < '2009/03/28'), ])
week15timeTest <- with(week15FridayTest, week15FridayTest[(time >= '17:00:00' & time <= '20:00:00'), ])
week15dataTest <- week15timeTest[c(6,9)]
print(week15dataTest)

# Week 16
week16FridayTest <- with(TestingData, TestingData[(date >= '2009/04/03' & date < '2009/04/04'), ])
week16timeTest <- with(week16FridayTest, week16FridayTest[(time >= '17:00:00' & time <= '20:00:00'), ])
week16dataTest <- week16timeTest[c(6,9)]
print(week16dataTest)

# Week 17
week17FridayTest <- with(TestingData, TestingData[(date >= '2009/04/10' & date < '2009/04/11'), ])
week17timeTest <- with(week17FridayTest, week17FridayTest[(time >= '17:00:00' & time <= '20:00:00'), ])
week17dataTest <- week17timeTest[c(6,9)]
print(week17dataTest)

# Week 18
week18FridayTest <- with(TestingData, TestingData[(date >= '2009/04/17' & date < '2009/04/18'), ])
week18timeTest <- with(week18FridayTest, week18FridayTest[(time >= '17:00:00' & time <= '20:00:00'), ])
week18dataTest <- week18timeTest[c(6,9)]
print(week18dataTest)

# Week 19
week19FridayTest <- with(TestingData, TestingData[(date >= '2009/04/24' & date < '2009/04/25'), ])
week19timeTest <- with(week19FridayTest, week19FridayTest[(time >= '17:00:00' & time <= '20:00:00'), ])
week19dataTest <- week19timeTest[c(6,9)]
print(week19dataTest)

# Week 20
week20FridayTest <- with(TestingData, TestingData[(date >= '2009/05/01' & date < '2009/05/02'), ])
week20timeTest <- with(week20FridayTest, week20FridayTest[(time >= '17:00:00' & time <= '20:00:00'), ])
week20dataTest <- week20timeTest[c(6,9)]
print(week20dataTest)

# Week 21
week21FridayTest <- with(TestingData, TestingData[(date >= '2009/05/07' & date < '2009/05/08'), ])
week21timeTest <- with(week21FridayTest, week21FridayTest[(time >= '17:00:00' & time <= '20:00:00'), ])
week21dataTest <- week21timeTest[c(6,9)]
print(week21dataTest)

# Week 22
week22FridayTest <- with(TestingData, TestingData[(date >= '2009/05/14' & date < '2009/05/15'), ])
week22timeTest <- with(week22FridayTest, week22FridayTest[(time >= '17:00:00' & time <= '20:00:00'), ])
week22dataTest <- week22timeTest[c(6,9)]
print(week22dataTest)

# Week 23
week23FridayTest <- with(TestingData, TestingData[(date >= '2009/05/21' & date < '2009/05/22'), ])
week23timeTest <- with(week23FridayTest, week23FridayTest[(time >= '17:00:00' & time <= '20:00:00'), ])
week23dataTest <- week23timeTest[c(6,9)]
print(week23dataTest)

# Week 24
week24FridayTest <- with(TestingData, TestingData[(date >= '2009/05/28' & date < '2009/05/29'), ])
week24timeTest <- with(week24FridayTest, week24FridayTest[(time >= '17:00:00' & time <= '20:00:00'), ])
week24dataTest <- week24timeTest[c(6,9)]
print(week24dataTest)

# Week 25
week25FridayTest <- with(TestingData, TestingData[(date >= '2009/06/05' & date < '2009/06/06'), ])
week25timeTest <- with(week25FridayTest, week25FridayTest[(time >= '17:00:00' & time <= '20:00:00'), ])
week25dataTest <- week25timeTest[c(6,9)]
print(week25dataTest)

# Week 26
week26FridayTest <- with(TestingData, TestingData[(date >= '2009/06/12' & date < '2009/06/13'), ])
week26timeTest <- with(week26FridayTest, week26FridayTest[(time >= '17:00:00' & time <= '20:00:00'), ])
week26dataTest <- week26timeTest[c(6,9)]
print(week26dataTest)

# Week 27
week27FridayTest <- with(TestingData, TestingData[(date >= '2009/06/19' & date < '2009/06/20'), ])
week27timeTest <- with(week27FridayTest, week27FridayTest[(time >= '17:00:00' & time <= '20:00:00'), ])
week27dataTest <- week27timeTest[c(6,9)]
print(week27dataTest)

# Week 28
week28FridayTest <- with(TestingData, TestingData[(date >= '2009/06/26' & date < '2009/06/27'), ])
week28timeTest <- with(week28FridayTest, week28FridayTest[(time >= '17:00:00' & time <= '20:00:00'), ])
week28dataTest <- week28timeTest[c(6,9)]
print(week28dataTest)

# Week 29
week29FridayTest <- with(TestingData, TestingData[(date >= '2009/07/03' & date < '2009/07/04'), ])
week29timeTest <- with(week29FridayTest, week29FridayTest[(time >= '17:00:00' & time <= '20:00:00'), ])
week29dataTest <- week29timeTest[c(6,9)]
print(week29dataTest)

# Week 30
week30FridayTest <- with(TestingData, TestingData[(date >= '2009/07/10' & date < '2009/07/11'), ])
week30timeTest <- with(week30FridayTest, week30FridayTest[(time >= '17:00:00' & time <= '20:00:00'), ])
week30dataTest <- week30timeTest[c(6,9)]
print(week30dataTest)

# Week 31
week31FridayTest <- with(TestingData, TestingData[(date >= '2009/07/17' & date < '2009/07/18'), ])
week31timeTest <- with(week31FridayTest, week31FridayTest[(time >= '17:00:00' & time <= '20:00:00'), ])
week31dataTest <- week31timeTest[c(6,9)]
print(week31dataTest)

# Week 32
week32FridayTest <- with(TestingData, TestingData[(date >= '2009/07/24' & date < '2009/07/25'), ])
week32timeTest <- with(week32FridayTest, week32FridayTest[(time >= '17:00:00' & time <= '20:00:00'), ])
week32dataTest <- week32timeTest[c(6,9)]
print(week32dataTest)

# Week 33
week33FridayTest <- with(TestingData, TestingData[(date >= '2009/07/31' & date < '2009/08/01'), ])
week33timeTest <- with(week33FridayTest, week33FridayTest[(time >= '17:00:00' & time <= '20:00:00'), ])
week33dataTest <- week33timeTest[c(6,9)]
print(week33dataTest)

# Week 34
week34FridayTest <- with(TestingData, TestingData[(date >= '2009/08/07' & date < '2009/08/08'), ])
week34timeTest <- with(week34FridayTest, week34FridayTest[(time >= '17:00:00' & time <= '20:00:00'), ])
week34dataTest <- week34timeTest[c(6,9)]
print(week34dataTest)

# Week 35
week35FridayTest <- with(TestingData, TestingData[(date >= '2009/08/14' & date < '2009/08/15'), ])
week35timeTest <- with(week35FridayTest, week35FridayTest[(time >= '17:00:00' & time <= '20:00:00'), ])
week35dataTest <- week35timeTest[c(6,9)]
print(week35dataTest)

# Week 36
week36FridayTest <- with(TestingData, TestingData[(date >= '2009/08/21' & date < '2009/08/22'), ])
week36timeTest <- with(week36FridayTest, week36FridayTest[(time >= '17:00:00' & time <= '20:00:00'), ])
week36dataTest <- week36timeTest[c(6,9)]
print(week36dataTest)

# Week 37
week37FridayTest <- with(TestingData, TestingData[(date >= '2009/08/28' & date < '2009/08/29'), ])
week37timeTest <- with(week37FridayTest, week37FridayTest[(time >= '17:00:00' & time <= '20:00:00'), ])
week37dataTest <- week37timeTest[c(6,9)]
print(week37dataTest)

# Week 38
week38FridayTest <- with(TestingData, TestingData[(date >= '2009/09/04' & date < '2009/09/05'), ])
week38timeTest <- with(week38FridayTest, week38FridayTest[(time >= '17:00:00' & time <= '20:00:00'), ])
week38dataTest <- week38timeTest[c(6,9)]
print(week38dataTest)

# Week 39
week39FridayTest <- with(TestingData, TestingData[(date >= '2009/09/11' & date < '2009/09/12'), ])
week39timeTest <- with(week39FridayTest, week39FridayTest[(time >= '17:00:00' & time <= '20:00:00'), ])
week39dataTest <- week39timeTest[c(6,9)]
print(week39dataTest)

# Week 40
week40FridayTest <- with(TestingData, TestingData[(date >= '2009/09/18' & date < '2009/09/19'), ])
week40timeTest <- with(week40FridayTest, week40FridayTest[(time >= '17:00:00' & time <= '20:00:00'), ])
week40dataTest <- week40timeTest[c(6,9)]
print(week40dataTest)

# Week 41
week41FridayTest <- with(TestingData, TestingData[(date >= '2009/09/25' & date < '2009/09/26'), ])
week41timeTest <- with(week41FridayTest, week41FridayTest[(time >= '17:00:00' & time <= '20:00:00'), ])
week41dataTest <- week41timeTest[c(6,9)]
print(week41dataTest)

# Week 42 [Has missing data points]
week42FridayTest <- with(TestingData, TestingData[(date >= '2009/10/02' & date < '2009/10/03'), ])
week42timeTest <- with(week42FridayTest, week42FridayTest[(time >= '17:00:00' & time <= '20:00:00'), ])
week42dataTest <- week42timeTest[c(6,9)]
print(week42dataTest)

# Week 43
week43FridayTest <- with(TestingData, TestingData[(date >= '2009/10/09' & date < '2009/10/10'), ])
week43timeTest <- with(week43FridayTest, week43FridayTest[(time >= '17:00:00' & time <= '20:00:00'), ])
week43dataTest <- week43timeTest[c(6,9)]
print(week43dataTest)

# Week 44
week44FridayTest <- with(TestingData, TestingData[(date >= '2009/10/16' & date < '2009/10/17'), ])
week44timeTest <- with(week44FridayTest, week44FridayTest[(time >= '17:00:00' & time <= '20:00:00'), ])
week44dataTest <- week44timeTest[c(6,9)]
print(week44dataTest)

# Week 45
week45FridayTest <- with(TestingData, TestingData[(date >= '2009/10/23' & date < '2009/10/24'), ])
week45timeTest <- with(week45FridayTest, week45FridayTest[(time >= '17:00:00' & time <= '20:00:00'), ])
week45dataTest <- week45timeTest[c(6,9)]
print(week45dataTest)

# Week 46
week46FridayTest <- with(TestingData, TestingData[(date >= '2009/10/30' & date < '2009/10/31'), ])
week46timeTest <- with(week46FridayTest, week46FridayTest[(time >= '17:00:00' & time <= '20:00:00'), ])
week46dataTest <- week46timeTest[c(6,9)]
print(week46dataTest)

# Week 47
week47FridayTest <- with(TestingData, TestingData[(date >= '2009/11/06' & date < '2009/11/07'), ])
week47timeTest <- with(week47FridayTest, week47FridayTest[(time >= '17:00:00' & time <= '20:00:00'), ])
week47dataTest <- week47timeTest[c(6,9)]
print(week47dataTest)

# Week 48
week48FridayTest <- with(TestingData, TestingData[(date >= '2009/11/13' & date < '2009/11/14'), ])
week48timeTest <- with(week48FridayTest, week48FridayTest[(time >= '17:00:00' & time <= '20:00:00'), ])
week48dataTest <- week48timeTest[c(6,9)]
print(week48dataTest)

# Week 49
week49FridayTest <- with(TestingData, TestingData[(date >= '2009/11/20' & date < '2009/11/21'), ])
week49timeTest <- with(week49FridayTest, week49FridayTest[(time >= '17:00:00' & time <= '20:00:00'), ])
week49dataTest <- week49timeTest[c(6,9)]
print(week49dataTest)

# Week 50
week50FridayTest <- with(TestingData, TestingData[(date >= '2009/11/27' & date < '2009/11/28'), ])
week50timeTest <- with(week50FridayTest, week50FridayTest[(time >= '17:00:00' & time <= '20:00:00'), ])
week50dataTest <- week50timeTest[c(6,9)]
print(week50dataTest)

# Week 51
week51FridayTest <- with(TestingData, TestingData[(date >= '2009/12/04' & date < '2009/12/05'), ])
week51timeTest <- with(week51FridayTest, week51FridayTest[(time >= '17:00:00' & time <= '20:00:00'), ])
week51dataTest <- week51timeTest[c(6,9)]
print(week51dataTest)

# Week 52
week52FridayTest <- with(TestingData, TestingData[(date >= '2009/12/11' & date < '2009/12/12'), ])
week52timeTest <- with(week52FridayTest, week52FridayTest[(time >= '17:00:00' & time <= '20:00:00'), ])
week52dataTest <- week52timeTest[c(6,9)]
print(week52dataTest)

concatenatedTestWeeks = rbind(week1dataTest,week2dataTest,week3dataTest,week4dataTest,week5dataTest,week6dataTest,
                      week7dataTest,week8dataTest,week9dataTest,week10dataTest,week11dataTest,week12dataTest,
                      week13dataTest,week14dataTest,week15dataTest,week16dataTest,week17dataTest,week18dataTest,
                      week19dataTest,week20dataTest,week21dataTest,week22dataTest,week23dataTest,week24dataTest,
                      week25dataTest,week26dataTest,week27dataTest,week28dataTest,week29dataTest,week30dataTest,
                      week31dataTest,week32dataTest,week33dataTest,week34dataTest,week35dataTest,week36dataTest,
                      week37dataTest,week38dataTest,week39dataTest,week40dataTest,week41dataTest,week42dataTest,
                      week43dataTest,week44dataTest,week45dataTest,week46dataTest,week47dataTest,week48dataTest,
                      week49dataTest,week50dataTest,week51dataTest,week52dataTest)


scaledData <- scale(concatenatedTrainingWeeks, center = TRUE, scale = TRUE)
trainDataScaled <- data.frame(scaledData)

scaledTestData <- scale(concatenatedTestWeeks, center = TRUE, scale = TRUE)
testDataScaled <- data.frame(scaledTestData)

###############################################
#             To Train the Data               #
###############################################

library("depmixS4")
data()
set.seed(1)
mod1 <- depmix(response = list(Global_intensity~1, Sub_metering_3~1),
               data = trainDataScaled, nstates = 4,
               family = list(gaussian(), gaussian()),
               ntimes = 20453)

fm1 <- fit(mod1)
BIC1 <- BIC(fm1)
loglik1 <- logLik(fm1)
print(fm1)
summary(fm1)

###############################################

mod2 <- depmix(response = list(Global_intensity~1, Sub_metering_3~1),
               data = trainDataScaled, nstates = 8,
               family = list(gaussian(), gaussian()),
               ntimes = 20453)
fm2 <- fit(mod2)
BIC2 <- BIC(fm2)
loglik2 <- logLik(fm2)
print(fm2)
summary(fm2)

###############################################

mod3 <- depmix(response = list(Global_intensity~1, Sub_metering_3~1),
               data = trainDataScaled, nstates = 12,
               family = list(gaussian(), gaussian()),
               ntimes = 20453)

fm3 <- fit(mod3)
BIC3 <- BIC(fm3)
loglik3 <- logLik(fm3)
print(fm3)
summary(fm3)

###############################################

mod4 <- depmix(response = list(Global_intensity~1, Sub_metering_3~1),
               data = trainDataScaled, nstates = 16,
               family = list(gaussian(), gaussian()),
               ntimes = 20453)
fm4 <- fit(mod4)
BIC4 <- BIC(fm4)
loglik4 <- logLik(fm4)
print(fm4)
summary(fm4)

###############################################

mod5 <- depmix(response = list(Global_intensity~1, Sub_metering_3~1),
               data = trainDataScaled, nstates = 20,
               family = list(gaussian(), gaussian()),
               ntimes = 20453)
fm5 <- fit(mod5)
BIC5 <- BIC(fm5)
loglik5 <- logLik(fm5)
print(fm5)
summary(fm5)

###############################################

mod6 <- depmix(response = list(Global_intensity~1, Sub_metering_3~1),
               data = trainDataScaled, nstates = 24,
               family = list(gaussian(), gaussian()),
               ntimes = 20453)
fm6 <- fit(mod6)
BIC6 <- BIC(fm6)
loglik6 <- logLik(fm6)
print(fm6)
summary(fm6)



###############################################
#               Testing Data                  #
###############################################
mod1Test <- depmix(response = list(Global_intensity~1, Sub_metering_3~1),
               data = testDataScaled, nstates = 4,
               family = list(gaussian(), gaussian()),
               ntimes = 9050)
mod1Test <- setpars(mod1Test, getpars(fm1))
fb1Test <- forwardbackward(mod1Test, return.all = FALSE)

###############################################

mod2Test <- depmix(response = list(Global_intensity~1, Sub_metering_3~1),
                   data = testDataScaled, nstates = 8,
                   family = list(gaussian(), gaussian()),
                   ntimes = 9050)
mod2Test <- setpars(mod2Test, getpars(fm2))
fb2Test <- forwardbackward(mod2Test, return.all = FALSE)

###############################################

mod3Test <- depmix(response = list(Global_intensity~1, Sub_metering_3~1),
                   data = testDataScaled, nstates = 12,
                   family = list(gaussian(), gaussian()),
                   ntimes = 9050)
mod3Test <- setpars(mod3Test, getpars(fm3))
fb3Test <- forwardbackward(mod3Test, return.all = FALSE)

###############################################

mod4Test <- depmix(response = list(Global_intensity~1, Sub_metering_3~1),
                   data = testDataScaled, nstates = 16,
                   family = list(gaussian(), gaussian()),
                   ntimes = 9050)
mod4Test <- setpars(mod4Test, getpars(fm4))
fb4Test <- forwardbackward(mod4Test, return.all = FALSE)

###############################################

mod5Test <- depmix(response = list(Global_intensity~1, Sub_metering_3~1),
                   data = testDataScaled, nstates = 20,
                   family = list(gaussian(), gaussian()),
                   ntimes = 9050)
mod5Test <- setpars(mod5Test, getpars(fm5))
fb5Test <- forwardbackward(mod5Test, return.all = FALSE)

###############################################

mod6Test <- depmix(response = list(Global_intensity~1, Sub_metering_3~1),
                   data = testDataScaled, nstates = 24,
                   family = list(gaussian(), gaussian()),
                   ntimes = 9050)
mod6Test <- setpars(mod6Test, getpars(fm6))
fb6Test <- forwardbackward(mod6Test, return.all = FALSE)


###############################################
#         Log-likelihood Calculations         #
###############################################

loglikTraining <- c((loglik1/103), (loglik2/103), (loglik3/103),
                    (loglik4/103), (loglik5/103), (loglik6/103))

loglikTest <- c((fb1Test$logLike/52), (fb2Test$logLike/52), (fb3Test$logLike/52),
                (fb4Test$logLike/52), (fb5Test$logLike/52), (fb6Test$logLike/52))

testTable <- data.frame(1:6, loglikTest)
colnames(testTable) <- c("State_Number", "Loglik_Value")
trainTable <- data.frame(1:6, loglikTraining)
colnames(trainTable) <- c("State_Number", "Loglik_Value")

###############################################
# chosen nstates = 12, i.e. fm3
mod_ntimes <- depmix(response = list(Global_intensity~1, Sub_metering_3~1),
                     data = trainDataScaled, nstates = 12,
                     family = list(gaussian(), gaussian()),
                     ntimes = 20453)
fm_ntimes <- fit(mod_ntimes)
print(fm_ntimes)
summary(fm_ntimes)

###############################################
#                   Plots                     #
###############################################

BIC_Values <- c(BIC1,BIC2,BIC3,BIC4,BIC5,BIC6)
Loglikelihood_Values <- c(loglik1,loglik2,loglik3,loglik4,loglik5,loglik6)
plot(1:6,BIC_Values,ty="b", col="red")
plot(1:6,Loglikelihood_Values, ty="b", col="blue")
# Red is for BIC values and Blue is for log-likelihood values

plot(1:6,loglikTest,ty="b", col="red")
plot(1:6,loglikTraining, ty="b", col="blue")

# library(ggplot2)
# library(gridExtra)
# dfTest<-as.data.frame(t(testTable))
# dfTrain<-as.data.frame(t(trainTable))
# g1 <- ggplot() + 
#   geom_line(data=dfTest, aes(x="State_Number", y="Loglik_Value"), color='green')
# g2 <- ggplot() +
#   geom_line(data=dfTrain, aes(x="State_Number", y="Loglik_Value"), color='red')
# gridExtra::grid.arrange(g1, g2)

###############################################
#         Anomaly 1 Detection Data            #
###############################################

pathA1 <- getwd()
setwd(pathA1)
fileAnomalies1 <- read.table("./DataWithAnomalies1.txt", header = TRUE, sep = ",")

fileAnomalies1$date <- as.POSIXlt(fileAnomalies1$Date, format='%d/%m/%Y')

fileAnomalies1$time <- chron(times = fileAnomalies1$Time)

anomalies <- with(fileAnomalies1, fileAnomalies1[(date >= '2009/12/1' & date < '2010/11/27'), ])

# Week 1 
week1FridayA1 <- with(anomalies, anomalies[(date >= '2009/12/4' & date < '2009/12/5'), ])
week1timeFridayA1 <- with(week1FridayA1, week1FridayA1[(time >= '17:00:00' & time <= '20:00:00'), ])
week1dataFridayA1 <- week1timeFridayA1[c(6,9)]
print(week1dataFridayA1)

# Week 2 
week2FridayA1 <- with(anomalies, anomalies[(date >= '2009/12/11' & date < '2009/12/12'), ])
week2timeFridayA1 <- with(week2FridayA1, week2FridayA1[(time >= '17:00:00' & time <= '20:00:00'), ])
week2dataFridayA1 <- week2timeFridayA1[c(6,9)]
print(week2dataFridayA1)

# Week 3
week3FridayA1 <- with(anomalies, anomalies[(date >= '2009/12/18' & date < '2009/12/19'), ])
week3timeFridayA1 <- with(week3FridayA1, week3FridayA1[(time >= '17:00:00' & time <= '20:00:00'), ])
week3dataFridayA1 <- week3timeFridayA1[c(6,9)]
print(week3dataFridayA1)

# Week 4 
week4FridayA1 <- with(anomalies, anomalies[(date >= '2009/12/25' & date < '2009/12/26'), ])
week4timeFridayA1 <- with(week4FridayA1, week4FridayA1[(time >= '17:00:00' & time <= '20:00:00'), ])
week4dataFridayA1 <- week4timeFridayA1[c(6,9)]
print(week4dataFridayA1)

# Week 5 
week5FridayA1 <- with(anomalies, anomalies[(date >= '2010/01/01' & date < '2010/01/02'), ])
week5timeFridayA1 <- with(week5FridayA1, week5FridayA1[(time >= '17:00:00' & time <= '20:00:00'), ])
week5dataFridayA1 <- week5timeFridayA1[c(6,9)]
print(week5dataFridayA1)

# Week 6
week6FridayA1 <- with(anomalies, anomalies[(date >= '2010/01/08' & date < '2010/01/09'), ])
week6timeFridayA1 <- with(week6FridayA1, week6FridayA1[(time >= '17:00:00' & time <= '20:00:00'), ])
week6dataFridayA1 <- week6timeFridayA1[c(6,9)]
print(week6dataFridayA1)

# Week 7 
week7FridayA1 <- with(anomalies, anomalies[(date >= '2010/01/15' & date < '2010/01/16'), ])
week7timeFridayA1 <- with(week7FridayA1, week7FridayA1[(time >= '17:00:00' & time <= '20:00:00'), ])
week7dataFridayA1 <- week7timeFridayA1[c(6,9)]
print(week7dataFridayA1)

# Week 8 
week8FridayA1 <- with(anomalies, anomalies[(date >= '2010/01/22' & date < '2010/01/23'), ])
week8timeFridayA1 <- with(week8FridayA1, week8FridayA1[(time >= '17:00:00' & time <= '20:00:00'), ])
week8dataFridayA1 <- week8timeFridayA1[c(6,9)]
print(week8dataFridayA1)

# Week 9
week9FridayA1 <- with(anomalies, anomalies[(date >= '2010/01/29' & date < '2010/01/30'), ])
week9timeFridayA1 <- with(week9FridayA1, week9FridayA1[(time >= '17:00:00' & time <= '20:00:00'), ])
week9dataFridayA1 <- week9timeFridayA1[c(6,9)]
print(week9dataFridayA1)

# Week 10
week10FridayA1 <- with(anomalies, anomalies[(date >= '2010/02/05' & date < '2010/02/06'), ])
week10timeFridayA1 <- with(week10FridayA1, week10FridayA1[(time >= '17:00:00' & time <= '20:00:00'), ])
week10dataFridayA1 <- week10timeFridayA1[c(6,9)]
print(week10dataFridayA1)

# Week 11
week11FridayA1 <- with(anomalies, anomalies[(date >= '2010/02/12' & date < '2010/02/13'), ])
week11timeFridayA1 <- with(week11FridayA1, week11FridayA1[(time >= '17:00:00' & time <= '20:00:00'), ])
week11dataFridayA1 <- week11timeFridayA1[c(6,9)]
print(week11dataFridayA1)

# Week 12 
week12FridayA1 <- with(anomalies, anomalies[(date >= '2010/02/19' & date < '2010/02/20'), ])
week12timeFridayA1 <- with(week12FridayA1, week12FridayA1[(time >= '17:00:00' & time <= '20:00:00'), ])
week12dataFridayA1 <- week12timeFridayA1[c(6,9)]
print(week12dataFridayA1)

# Week 13
week13FridayA1 <- with(anomalies, anomalies[(date >= '2010/02/26' & date < '2010/02/27'), ])
week13timeFridayA1 <- with(week13FridayA1, week13FridayA1[(time >= '17:00:00' & time <= '20:00:00'), ])
week13dataFridayA1 <- week13timeFridayA1[c(6,9)]
print(week13dataFridayA1)

# Week 14
week14FridayA1 <- with(anomalies, anomalies[(date >= '2010/03/05' & date < '2010/03/06'), ])
week14timeFridayA1 <- with(week14FridayA1, week14FridayA1[(time >= '17:00:00' & time <= '20:00:00'), ])
week14dataFridayA1 <- week14timeFridayA1[c(6,9)]
print(week14dataFridayA1)

# Week 15 
week15FridayA1 <- with(anomalies, anomalies[(date >= '2010/03/12' & date < '2010/03/13'), ])
week15timeFridayA1 <- with(week15FridayA1, week15FridayA1[(time >= '17:00:00' & time <= '20:00:00'), ])
week15dataFridayA1 <- week15timeFridayA1[c(6,9)]
print(week15dataFridayA1)

# Week 16
week16FridayA1 <- with(anomalies, anomalies[(date >= '2010/03/18' & date < '2007/04/19'), ])
week16timeFridayA1 <- with(week16FridayA1, week16FridayA1[(time >= '17:00:00' & time <= '20:00:00'), ])
week16dataFridayA1 <- week16timeFridayA1[c(6,9)]
print(week16dataFridayA1)

# Week 17
week17FridayA1 <- with(anomalies, anomalies[(date >= '2010/03/26' & date < '2010/03/27'), ])
week17timeFridayA1 <- with(week17FridayA1, week17FridayA1[(time >= '17:00:00' & time <= '20:00:00'), ])
week17dataFridayA1 <- week17timeFridayA1[c(6,9)]
print(week17dataFridayA1)

# Week 18 
week18FridayA1 <- with(anomalies, anomalies[(date >= '2010/04/02' & date < '2010/04/03'), ])
week18timeFridayA1 <- with(week18FridayA1, week18FridayA1[(time >= '17:00:00' & time <= '20:00:00'), ])
week18dataFridayA1 <- week18timeFridayA1[c(6,9)]
print(week18dataFridayA1)

# Week 19
week19FridayA1 <- with(anomalies, anomalies[(date >= '2010/04/09' & date < '2010/04/10'), ])
week19timeFridayA1 <- with(week19FridayA1, week19FridayA1[(time >= '17:00:00' & time <= '20:00:00'), ])
week19dataFridayA1 <- week19timeFridayA1[c(6,9)]
print(week19dataFridayA1)

# Week 20
week20FridayA1 <- with(anomalies, anomalies[(date >= '2010/04/16' & date < '2010/04/17'), ])
week20timeFridayA1 <- with(week20FridayA1, week20FridayA1[(time >= '17:00:00' & time <= '20:00:00'), ])
week20dataFridayA1 <- week20timeFridayA1[c(6,9)]
print(week20dataFridayA1)

# Week 21 
week21FridayA1 <- with(anomalies, anomalies[(date >= '2010/04/23' & date < '2010/04/24'), ])
week21timeFridayA1 <- with(week21FridayA1, week21FridayA1[(time >= '17:00:00' & time <= '20:00:00'), ])
week21dataFridayA1 <- week21timeFridayA1[c(6,9)]
print(week21dataFridayA1)

# Week 22 
week22FridayA1 <- with(anomalies, anomalies[(date >= '2010/04/30' & date < '2010/05/01'), ])
week22timeFridayA1 <- with(week22FridayA1, week22FridayA1[(time >= '17:00:00' & time <= '20:00:00'), ])
week22dataFridayA1 <- week22timeFridayA1[c(6,9)]
print(week22dataFridayA1)

# Week 23 
week23FridayA1 <- with(anomalies, anomalies[(date >= '2010/05/07' & date < '2010/05/08'), ])
week23timeFridayA1 <- with(week23FridayA1, week23FridayA1[(time >= '17:00:00' & time <= '20:00:00'), ])
week23dataFridayA1 <- week23timeFridayA1[c(6,9)]
print(week23dataFridayA1)

# Week 24
week24FridayA1 <- with(anomalies, anomalies[(date >= '2010/05/14' & date < '2010/05/15'), ])
week24timeFridayA1 <- with(week24FridayA1, week24FridayA1[(time >= '17:00:00' & time <= '20:00:00'), ])
week24dataFridayA1 <- week24timeFridayA1[c(6,9)]
print(week24dataFridayA1)

# Week 25 
week25FridayA1 <- with(anomalies, anomalies[(date >= '2010/05/21' & date < '2010/05/22'), ])
week25timeFridayA1 <- with(week25FridayA1, week25FridayA1[(time >= '17:00:00' & time <= '20:00:00'), ])
week25dataFridayA1 <- week25timeFridayA1[c(6,9)]
print(week25dataFridayA1)

# Week 26
week26FridayA1 <- with(anomalies, anomalies[(date >= '2010/05/28' & date < '2010/05/29'), ])
week26timeFridayA1 <- with(week26FridayA1, week26FridayA1[(time >= '17:00:00' & time <= '20:00:00'), ])
week26dataFridayA1 <- week26timeFridayA1[c(6,9)]
print(week26dataFridayA1)

# Week 27 
week27FridayA1 <- with(anomalies, anomalies[(date >= '2010/06/04' & date < '2010/06/05'), ])
week27timeFridayA1 <- with(week27FridayA1, week27FridayA1[(time >= '17:00:00' & time <= '20:00:00'), ])
week27dataFridayA1 <- week27timeFridayA1[c(6,9)]
print(week27dataFridayA1)

# Week 28 
week28FridayA1 <- with(anomalies, anomalies[(date >= '2010/06/11' & date < '2010/06/12'), ])
week28timeFridayA1 <- with(week28FridayA1, week28FridayA1[(time >= '17:00:00' & time <= '20:00:00'), ])
week28dataFridayA1 <- week28timeFridayA1[c(6,9)]
print(week28dataFridayA1)

# Week 29
week29FridayA1 <- with(anomalies, anomalies[(date >= '2010/06/18' & date < '2010/06/19'), ])
week29timeFridayA1 <- with(week29FridayA1, week29FridayA1[(time >= '17:00:00' & time <= '20:00:00'), ])
week29dataFridayA1 <- week29timeFridayA1[c(6,9)]
print(week29dataFridayA1)

# Week 30
week30FridayA1 <- with(anomalies, anomalies[(date >= '2010/06/25' & date < '2010/06/26'), ])
week30timeFridayA1 <- with(week30FridayA1, week30FridayA1[(time >= '17:00:00' & time <= '20:00:00'), ])
week30dataFridayA1 <- week30timeFridayA1[c(6,9)]
print(week30dataFridayA1)

# Week 31
week31FridayA1 <- with(anomalies, anomalies[(date >= '2010/07/02' & date < '2010/07/03'), ])
week31timeFridayA1 <- with(week31FridayA1, week31FridayA1[(time >= '17:00:00' & time <= '20:00:00'), ])
week31dataFridayA1 <- week31timeFridayA1[c(6,9)]
print(week31dataFridayA1)

# Week 32 
week32FridayA1 <- with(anomalies, anomalies[(date >= '2010/07/09' & date < '2010/07/10'), ])
week32timeFridayA1 <- with(week32FridayA1, week32FridayA1[(time >= '17:00:00' & time <= '20:00:00'), ])
week32dataFridayA1 <- week32timeFridayA1[c(6,9)]
print(week32dataFridayA1)

# Week 33
week33FridayA1 <- with(anomalies, anomalies[(date >= '2010/07/16' & date < '2010/07/17'), ])
week33timeFridayA1 <- with(week33FridayA1, week33FridayA1[(time >= '17:00:00' & time <= '20:00:00'), ])
week33dataFridayA1 <- week33timeFridayA1[c(6,9)]
print(week33dataFridayA1)

# Week 34
week34FridayA1 <- with(anomalies, anomalies[(date >= '2010/07/23' & date < '2010/07/24'), ])
week34timeFridayA1 <- with(week34FridayA1, week34FridayA1[(time >= '17:00:00' & time <= '20:00:00'), ])
week34dataFridayA1 <- week34timeFridayA1[c(6,9)]
print(week34dataFridayA1)

# Week 35 
week35FridayA1 <- with(anomalies, anomalies[(date >= '2010/07/30' & date < '2010/07/31'), ])
week35timeFridayA1 <- with(week35FridayA1, week35FridayA1[(time >= '17:00:00' & time <= '20:00:00'), ])
week35dataFridayA1 <- week35timeFridayA1[c(6,9)]
print(week35dataFridayA1)

# Week 36 
week36FridayA1 <- with(anomalies, anomalies[(date >= '2010/08/06' & date < '2010/08/07'), ])
week36timeFridayA1 <- with(week36FridayA1, week36FridayA1[(time >= '17:00:00' & time <= '20:00:00'), ])
week36dataFridayA1 <- week36timeFridayA1[c(6,9)]
print(week36dataFridayA1)

# Week 37 
week37FridayA1 <- with(anomalies, anomalies[(date >= '2010/08/13' & date < '2010/08/14'), ])
week37timeFridayA1 <- with(week37FridayA1, week37FridayA1[(time >= '17:00:00' & time <= '20:00:00'), ])
week37dataFridayA1 <- week37timeFridayA1[c(6,9)]
print(week37dataFridayA1)

# Week 38 
week38FridayA1 <- with(anomalies, anomalies[(date >= '2010/08/20' & date < '2010/08/21'), ])
week38timeFridayA1 <- with(week38FridayA1, week38FridayA1[(time >= '17:00:00' & time <= '20:00:00'), ])
week38dataFridayA1 <- week38timeFridayA1[c(6,9)]
print(week38dataFridayA1)

# Week 39 
week39FridayA1 <- with(anomalies, anomalies[(date >= '2010/08/27' & date < '2010/08/28'), ])
week39timeFridayA1 <- with(week39FridayA1, week39FridayA1[(time >= '17:00:00' & time <= '20:00:00'), ])
week39dataFridayA1 <- week39timeFridayA1[c(6,9)]
print(week39dataFridayA1)

# Week 40
week40FridayA1 <- with(anomalies, anomalies[(date >= '2010/09/03' & date < '2010/09/04'), ])
week40timeFridayA1 <- with(week40FridayA1, week40FridayA1[(time >= '17:00:00' & time <= '20:00:00'), ])
week40dataFridayA1 <- week40timeFridayA1[c(6,9)]
print(week40dataFridayA1)

# Week 41
week41FridayA1 <- with(anomalies, anomalies[(date >= '2010/09/10' & date < '2010/09/11'), ])
week41timeFridayA1 <- with(week41FridayA1, week41FridayA1[(time >= '17:00:00' & time <= '20:00:00'), ])
week41dataFridayA1 <- week41timeFridayA1[c(6,9)]
print(week41dataFridayA1)

# Week 42
week42FridayA1 <- with(anomalies, anomalies[(date >= '2010/09/17' & date < '2010/09/18'), ])
week42timeFridayA1 <- with(week42FridayA1, week42FridayA1[(time >= '17:00:00' & time <= '20:00:00'), ])
week42dataFridayA1 <- week42timeFridayA1[c(6,9)]
print(week42dataFridayA1)

# Week 43
week43FridayA1 <- with(anomalies, anomalies[(date >= '2010/09/24' & date < '2010/09/25'), ])
week43timeFridayA1 <- with(week43FridayA1, week43FridayA1[(time >= '17:00:00' & time <= '20:00:00'), ])
week43dataFridayA1 <- week43timeFridayA1[c(6,9)]
print(week43dataFridayA1)

# Week 44
week44FridayA1 <- with(anomalies, anomalies[(date >= '2010/10/01' & date < '2010/10/02'), ])
week44timeFridayA1 <- with(week44FridayA1, week44FridayA1[(time >= '17:00:00' & time <= '20:00:00'), ])
week44dataFridayA1 <- week44timeFridayA1[c(6,9)]
print(week44dataFridayA1)

# Week 45 
week45FridayA1 <- with(anomalies, anomalies[(date >= '2010/10/08' & date < '2010/10/09'), ])
week45timeFridayA1 <- with(week45FridayA1, week45FridayA1[(time >= '17:00:00' & time <= '20:00:00'), ])
week45dataFridayA1 <- week45timeFridayA1[c(6,9)]
print(week45dataFridayA1)

# Week 46 
week46FridayA1 <- with(anomalies, anomalies[(date >= '2010/10/15' & date < '2010/10/16'), ])
week46timeFridayA1 <- with(week46FridayA1, week46FridayA1[(time >= '17:00:00' & time <= '20:00:00'), ])
week46dataFridayA1 <- week46timeFridayA1[c(6,9)]
print(week46dataFridayA1)

# Week 47
week47FridayA1 <- with(anomalies, anomalies[(date >= '2010/10/22' & date < '2010/10/23'), ])
week47timeFridayA1 <- with(week47FridayA1, week47FridayA1[(time >= '17:00:00' & time <= '20:00:00'), ])
week47dataFridayA1 <- week47timeFridayA1[c(6,9)]
print(week47dataFridayA1)

# Week 48 
week48FridayA1 <- with(anomalies, anomalies[(date >= '2010/10/29' & date < '2010/10/30'), ])
week48timeFridayA1 <- with(week48FridayA1, week48FridayA1[(time >= '17:00:00' & time <= '20:00:00'), ])
week48dataFridayA1 <- week48timeFridayA1[c(6,9)]
print(week48dataFridayA1)

# Week 49 
week49FridayA1 <- with(anomalies, anomalies[(date >= '2010/11/05' & date < '2010/11/06'), ])
week49timeFridayA1 <- with(week49FridayA1, week49FridayA1[(time >= '17:00:00' & time <= '20:00:00'), ])
week49dataFridayA1 <- week49timeFridayA1[c(6,9)]
print(week49dataFridayA1)

# Week 50 
week50FridayA1 <- with(anomalies, anomalies[(date >= '2010/11/12' & date < '2010/11/13'), ])
week50timeFridayA1 <- with(week50FridayA1, week50FridayA1[(time >= '17:00:00' & time <= '20:00:00'), ])
week50dataFridayA1 <- week50timeFridayA1[c(6,9)]
print(week50dataFridayA1)

# Week 51
week51FridayA1 <- with(anomalies, anomalies[(date >= '2010/11/19' & date < '2010/11/20'), ])
week51timeFridayA1 <- with(week51FridayA1, week51FridayA1[(time >= '17:00:00' & time <= '20:00:00'), ])
week51dataFridayA1 <- week51timeFridayA1[c(6,9)]
print(week51dataFridayA1)

# Week 52
week52FridayA1 <- with(anomalies, anomalies[(date >= '2010/11/26' & date < '2010/11/27'), ])
week52timeFridayA1 <- with(week52FridayA1, week52FridayA1[(time >= '17:00:00' & time <= '20:00:00'), ])
week52dataFridayA1 <- week52timeFridayA1[c(6,9)]
print(week52dataFridayA1)

concatenatedAnomaly1Weeks = rbind(week1dataFridayA1,week2dataFridayA1,week3dataFridayA1,week4dataFridayA1,week5dataFridayA1,week6dataFridayA1,
                              week7dataFridayA1,week8dataFridayA1,week9dataFridayA1,week10dataFridayA1,week11dataFridayA1,week12dataFridayA1,
                              week13dataFridayA1,week14dataFridayA1,week15dataFridayA1,week16dataFridayA1,week17dataFridayA1,week18dataFridayA1,
                              week19dataFridayA1,week20dataFridayA1,week21dataFridayA1,week22dataFridayA1,week23dataFridayA1,week24dataFridayA1,
                              week25dataFridayA1,week26dataFridayA1,week27dataFridayA1,week28dataFridayA1,week29dataFridayA1,week30dataFridayA1,
                              week31dataFridayA1,week32dataFridayA1,week33dataFridayA1,week34dataFridayA1,week35dataFridayA1,week36dataFridayA1,
                              week37dataFridayA1,week38dataFridayA1,week39dataFridayA1,week40dataFridayA1,week41dataFridayA1,week42dataFridayA1,
                              week43dataFridayA1,week44dataFridayA1,week45dataFridayA1,week46dataFridayA1,week47dataFridayA1,week48dataFridayA1,
                              week49dataFridayA1,week50dataFridayA1,week51dataFridayA1,week52dataFridayA1)

###############################################
#         Anomaly 2 Detection Data            #
###############################################
pathA2 <- getwd()
setwd(pathA2)

fileAnomalies2 <- read.table("./DataWithAnomalies2.txt", header = TRUE, sep = ",")

fileAnomalies2$date <- as.POSIXlt(fileAnomalies2$Date, format='%d/%m/%Y')

fileAnomalies2$time <- chron(times = fileAnomalies2$Time)

anomalies2 <- with(fileAnomalies2, fileAnomalies2[(date >= '2009/12/1' & date < '2010/11/27'), ])

# Week 1 
week1FridayA2 <- with(anomalies2, anomalies2[(date >= '2009/12/4' & date < '2009/12/5'), ])
week1timeFridayA2 <- with(week1FridayA2, week1FridayA2[(time >= '17:00:00' & time <= '20:00:00'), ])
week1dataFridayA2 <- week1timeFridayA2[c(6,9)]
print(week1dataFridayA2)

# Week 2 
week2FridayA2 <- with(anomalies2, anomalies2[(date >= '2009/12/11' & date < '2009/12/12'), ])
week2timeFridayA2 <- with(week2FridayA2, week2FridayA2[(time >= '17:00:00' & time <= '20:00:00'), ])
week2dataFridayA2 <- week2timeFridayA2[c(6,9)]
print(week2dataFridayA2)

# Week 3
week3FridayA2 <- with(anomalies2, anomalies2[(date >= '2009/12/18' & date < '2009/12/19'), ])
week3timeFridayA2 <- with(week3FridayA2, week3FridayA2[(time >= '17:00:00' & time <= '20:00:00'), ])
week3dataFridayA2 <- week3timeFridayA2[c(6,9)]
print(week3dataFridayA2)

# Week 4 
week4FridayA2 <- with(anomalies2, anomalies2[(date >= '2009/12/25' & date < '2009/12/26'), ])
week4timeFridayA2 <- with(week4FridayA2, week4FridayA2[(time >= '17:00:00' & time <= '20:00:00'), ])
week4dataFridayA2 <- week4timeFridayA2[c(6,9)]
print(week4dataFridayA2)

# Week 5 
week5FridayA2 <- with(anomalies2, anomalies2[(date >= '2010/01/01' & date < '2010/01/02'), ])
week5timeFridayA2 <- with(week5FridayA2, week5FridayA2[(time >= '17:00:00' & time <= '20:00:00'), ])
week5dataFridayA2 <- week5timeFridayA2[c(6,9)]
print(week5dataFridayA2)

# Week 6
week6FridayA2 <- with(anomalies2, anomalies2[(date >= '2010/01/08' & date < '2010/01/09'), ])
week6timeFridayA2 <- with(week6FridayA2, week6FridayA2[(time >= '17:00:00' & time <= '20:00:00'), ])
week6dataFridayA2 <- week6timeFridayA2[c(6,9)]
print(week6dataFridayA2)

# Week 7 
week7FridayA2 <- with(anomalies2, anomalies2[(date >= '2010/01/15' & date < '2010/01/16'), ])
week7timeFridayA2 <- with(week7FridayA2, week7FridayA2[(time >= '17:00:00' & time <= '20:00:00'), ])
week7dataFridayA2 <- week7timeFridayA2[c(6,9)]
print(week7dataFridayA2)

# Week 8 
week8FridayA2 <- with(anomalies2, anomalies2[(date >= '2010/01/22' & date < '2010/01/23'), ])
week8timeFridayA2 <- with(week8FridayA2, week8FridayA2[(time >= '17:00:00' & time <= '20:00:00'), ])
week8dataFridayA2 <- week8timeFridayA2[c(6,9)]
print(week8dataFridayA2)

# Week 9
week9FridayA2 <- with(anomalies2, anomalies2[(date >= '2010/01/29' & date < '2010/01/30'), ])
week9timeFridayA2 <- with(week9FridayA2, week9FridayA2[(time >= '17:00:00' & time <= '20:00:00'), ])
week9dataFridayA2 <- week9timeFridayA2[c(6,9)]
print(week9dataFridayA2)

# Week 10
week10FridayA2 <- with(anomalies2, anomalies2[(date >= '2010/02/05' & date < '2010/02/06'), ])
week10timeFridayA2 <- with(week10FridayA2, week10FridayA2[(time >= '17:00:00' & time <= '20:00:00'), ])
week10dataFridayA2 <- week10timeFridayA2[c(6,9)]
print(week10dataFridayA2)

# Week 11
week11FridayA2 <- with(anomalies2, anomalies2[(date >= '2010/02/12' & date < '2010/02/13'), ])
week11timeFridayA2 <- with(week11FridayA2, week11FridayA2[(time >= '17:00:00' & time <= '20:00:00'), ])
week11dataFridayA2 <- week11timeFridayA2[c(6,9)]
print(week11dataFridayA2)

# Week 12 
week12FridayA2 <- with(anomalies2, anomalies2[(date >= '2010/02/19' & date < '2010/02/20'), ])
week12timeFridayA2 <- with(week12FridayA2, week12FridayA2[(time >= '17:00:00' & time <= '20:00:00'), ])
week12dataFridayA2 <- week12timeFridayA2[c(6,9)]
print(week12dataFridayA2)

# Week 13
week13FridayA2 <- with(anomalies2, anomalies2[(date >= '2010/02/26' & date < '2010/02/27'), ])
week13timeFridayA2 <- with(week13FridayA2, week13FridayA2[(time >= '17:00:00' & time <= '20:00:00'), ])
week13dataFridayA2 <- week13timeFridayA2[c(6,9)]
print(week13dataFridayA2)

# Week 14
week14FridayA2 <- with(anomalies2, anomalies2[(date >= '2010/03/05' & date < '2010/03/06'), ])
week14timeFridayA2 <- with(week14FridayA2, week14FridayA2[(time >= '17:00:00' & time <= '20:00:00'), ])
week14dataFridayA2 <- week14timeFridayA2[c(6,9)]
print(week14dataFridayA2)

# Week 15 
week15FridayA2 <- with(anomalies2, anomalies2[(date >= '2010/03/12' & date < '2010/03/13'), ])
week15timeFridayA2 <- with(week15FridayA2, week15FridayA2[(time >= '17:00:00' & time <= '20:00:00'), ])
week15dataFridayA2 <- week15timeFridayA2[c(6,9)]
print(week15dataFridayA2)

# Week 16
week16FridayA2 <- with(anomalies2, anomalies2[(date >= '2010/03/18' & date < '2007/04/19'), ])
week16timeFridayA2 <- with(week16FridayA2, week16FridayA2[(time >= '17:00:00' & time <= '20:00:00'), ])
week16dataFridayA2 <- week16timeFridayA2[c(6,9)]
print(week16dataFridayA2)

# Week 17
week17FridayA2 <- with(anomalies2, anomalies2[(date >= '2010/03/26' & date < '2010/03/27'), ])
week17timeFridayA2 <- with(week17FridayA2, week17FridayA2[(time >= '17:00:00' & time <= '20:00:00'), ])
week17dataFridayA2 <- week17timeFridayA2[c(6,9)]
print(week17dataFridayA2)

# Week 18 
week18FridayA2 <- with(anomalies2, anomalies2[(date >= '2010/04/02' & date < '2010/04/03'), ])
week18timeFridayA2 <- with(week18FridayA2, week18FridayA2[(time >= '17:00:00' & time <= '20:00:00'), ])
week18dataFridayA2 <- week18timeFridayA2[c(6,9)]
print(week18dataFridayA2)

# Week 19
week19FridayA2 <- with(anomalies2, anomalies2[(date >= '2010/04/09' & date < '2010/04/10'), ])
week19timeFridayA2 <- with(week19FridayA2, week19FridayA2[(time >= '17:00:00' & time <= '20:00:00'), ])
week19dataFridayA2 <- week19timeFridayA2[c(6,9)]
print(week19dataFridayA2)

# Week 20
week20FridayA2 <- with(anomalies2, anomalies2[(date >= '2010/04/16' & date < '2010/04/17'), ])
week20timeFridayA2 <- with(week20FridayA2, week20FridayA2[(time >= '17:00:00' & time <= '20:00:00'), ])
week20dataFridayA2 <- week20timeFridayA2[c(6,9)]
print(week20dataFridayA2)

# Week 21 
week21FridayA2 <- with(anomalies2, anomalies2[(date >= '2010/04/23' & date < '2010/04/24'), ])
week21timeFridayA2 <- with(week21FridayA2, week21FridayA2[(time >= '17:00:00' & time <= '20:00:00'), ])
week21dataFridayA2 <- week21timeFridayA2[c(6,9)]
print(week21dataFridayA2)

# Week 22 
week22FridayA2 <- with(anomalies2, anomalies2[(date >= '2010/04/30' & date < '2010/05/01'), ])
week22timeFridayA2 <- with(week22FridayA2, week22FridayA2[(time >= '17:00:00' & time <= '20:00:00'), ])
week22dataFridayA2 <- week22timeFridayA2[c(6,9)]
print(week22dataFridayA2)

# Week 23 
week23FridayA2 <- with(anomalies2, anomalies2[(date >= '2010/05/07' & date < '2010/05/08'), ])
week23timeFridayA2 <- with(week23FridayA2, week23FridayA2[(time >= '17:00:00' & time <= '20:00:00'), ])
week23dataFridayA2 <- week23timeFridayA2[c(6,9)]
print(week23dataFridayA2)

# Week 24
week24FridayA2 <- with(anomalies2, anomalies2[(date >= '2010/05/14' & date < '2010/05/15'), ])
week24timeFridayA2 <- with(week24FridayA2, week24FridayA2[(time >= '17:00:00' & time <= '20:00:00'), ])
week24dataFridayA2 <- week24timeFridayA2[c(6,9)]
print(week24dataFridayA2)

# Week 25 
week25FridayA2 <- with(anomalies2, anomalies2[(date >= '2010/05/21' & date < '2010/05/22'), ])
week25timeFridayA2 <- with(week25FridayA2, week25FridayA2[(time >= '17:00:00' & time <= '20:00:00'), ])
week25dataFridayA2 <- week25timeFridayA2[c(6,9)]
print(week25dataFridayA2)

# Week 26
week26FridayA2 <- with(anomalies2, anomalies2[(date >= '2010/05/28' & date < '2010/05/29'), ])
week26timeFridayA2 <- with(week26FridayA2, week26FridayA2[(time >= '17:00:00' & time <= '20:00:00'), ])
week26dataFridayA2 <- week26timeFridayA2[c(6,9)]
print(week26dataFridayA2)

# Week 27 
week27FridayA2 <- with(anomalies2, anomalies2[(date >= '2010/06/04' & date < '2010/06/05'), ])
week27timeFridayA2 <- with(week27FridayA2, week27FridayA2[(time >= '17:00:00' & time <= '20:00:00'), ])
week27dataFridayA2 <- week27timeFridayA2[c(6,9)]
print(week27dataFridayA2)

# Week 28 
week28FridayA2 <- with(anomalies2, anomalies2[(date >= '2010/06/11' & date < '2010/06/12'), ])
week28timeFridayA2 <- with(week28FridayA2, week28FridayA2[(time >= '17:00:00' & time <= '20:00:00'), ])
week28dataFridayA2 <- week28timeFridayA2[c(6,9)]
print(week28dataFridayA2)

# Week 29
week29FridayA2 <- with(anomalies2, anomalies2[(date >= '2010/06/18' & date < '2010/06/19'), ])
week29timeFridayA2 <- with(week29FridayA2, week29FridayA2[(time >= '17:00:00' & time <= '20:00:00'), ])
week29dataFridayA2 <- week29timeFridayA2[c(6,9)]
print(week29dataFridayA2)

# Week 30
week30FridayA2 <- with(anomalies2, anomalies2[(date >= '2010/06/25' & date < '2010/06/26'), ])
week30timeFridayA2 <- with(week30FridayA2, week30FridayA2[(time >= '17:00:00' & time <= '20:00:00'), ])
week30dataFridayA2 <- week30timeFridayA2[c(6,9)]
print(week30dataFridayA2)

# Week 31
week31FridayA2 <- with(anomalies2, anomalies2[(date >= '2010/07/02' & date < '2010/07/03'), ])
week31timeFridayA2 <- with(week31FridayA2, week31FridayA2[(time >= '17:00:00' & time <= '20:00:00'), ])
week31dataFridayA2 <- week31timeFridayA2[c(6,9)]
print(week31dataFridayA2)

# Week 32 
week32FridayA2 <- with(anomalies2, anomalies2[(date >= '2010/07/09' & date < '2010/07/10'), ])
week32timeFridayA2 <- with(week32FridayA2, week32FridayA2[(time >= '17:00:00' & time <= '20:00:00'), ])
week32dataFridayA2 <- week32timeFridayA2[c(6,9)]
print(week32dataFridayA2)

# Week 33
week33FridayA2 <- with(anomalies2, anomalies2[(date >= '2010/07/16' & date < '2010/07/17'), ])
week33timeFridayA2 <- with(week33FridayA2, week33FridayA2[(time >= '17:00:00' & time <= '20:00:00'), ])
week33dataFridayA2 <- week33timeFridayA2[c(6,9)]
print(week33dataFridayA2)

# Week 34
week34FridayA2 <- with(anomalies2, anomalies2[(date >= '2010/07/23' & date < '2010/07/24'), ])
week34timeFridayA2 <- with(week34FridayA2, week34FridayA2[(time >= '17:00:00' & time <= '20:00:00'), ])
week34dataFridayA2 <- week34timeFridayA2[c(6,9)]
print(week34dataFridayA2)

# Week 35 
week35FridayA2 <- with(anomalies2, anomalies2[(date >= '2010/07/30' & date < '2010/07/31'), ])
week35timeFridayA2 <- with(week35FridayA2, week35FridayA2[(time >= '17:00:00' & time <= '20:00:00'), ])
week35dataFridayA2 <- week35timeFridayA2[c(6,9)]
print(week35dataFridayA2)

# Week 36 
week36FridayA2 <- with(anomalies2, anomalies2[(date >= '2010/08/06' & date < '2010/08/07'), ])
week36timeFridayA2 <- with(week36FridayA2, week36FridayA2[(time >= '17:00:00' & time <= '20:00:00'), ])
week36dataFridayA2 <- week36timeFridayA2[c(6,9)]
print(week36dataFridayA2)

# Week 37 
week37FridayA2 <- with(anomalies2, anomalies2[(date >= '2010/08/13' & date < '2010/08/14'), ])
week37timeFridayA2 <- with(week37FridayA2, week37FridayA2[(time >= '17:00:00' & time <= '20:00:00'), ])
week37dataFridayA2 <- week37timeFridayA2[c(6,9)]
print(week37dataFridayA2)

# Week 38 
week38FridayA2 <- with(anomalies2, anomalies2[(date >= '2010/08/20' & date < '2010/08/21'), ])
week38timeFridayA2 <- with(week38FridayA2, week38FridayA2[(time >= '17:00:00' & time <= '20:00:00'), ])
week38dataFridayA2 <- week38timeFridayA2[c(6,9)]
print(week38dataFridayA2)

# Week 39 
week39FridayA2 <- with(anomalies2, anomalies2[(date >= '2010/08/27' & date < '2010/08/28'), ])
week39timeFridayA2 <- with(week39FridayA2, week39FridayA2[(time >= '17:00:00' & time <= '20:00:00'), ])
week39dataFridayA2 <- week39timeFridayA2[c(6,9)]
print(week39dataFridayA2)

# Week 40
week40FridayA2 <- with(anomalies2, anomalies2[(date >= '2010/09/03' & date < '2010/09/04'), ])
week40timeFridayA2 <- with(week40FridayA2, week40FridayA2[(time >= '17:00:00' & time <= '20:00:00'), ])
week40dataFridayA2 <- week40timeFridayA2[c(6,9)]
print(week40dataFridayA2)

# Week 41
week41FridayA2 <- with(anomalies2, anomalies2[(date >= '2010/09/10' & date < '2010/09/11'), ])
week41timeFridayA2 <- with(week41FridayA2, week41FridayA2[(time >= '17:00:00' & time <= '20:00:00'), ])
week41dataFridayA2 <- week41timeFridayA2[c(6,9)]
print(week41dataFridayA2)

# Week 42
week42FridayA2 <- with(anomalies2, anomalies2[(date >= '2010/09/17' & date < '2010/09/18'), ])
week42timeFridayA2 <- with(week42FridayA2, week42FridayA2[(time >= '17:00:00' & time <= '20:00:00'), ])
week42dataFridayA2 <- week42timeFridayA2[c(6,9)]
print(week42dataFridayA2)

# Week 43
week43FridayA2 <- with(anomalies2, anomalies2[(date >= '2010/09/24' & date < '2010/09/25'), ])
week43timeFridayA2 <- with(week43FridayA2, week43FridayA2[(time >= '17:00:00' & time <= '20:00:00'), ])
week43dataFridayA2 <- week43timeFridayA2[c(6,9)]
print(week43dataFridayA2)

# Week 44
week44FridayA2 <- with(anomalies2, anomalies2[(date >= '2010/10/01' & date < '2010/10/02'), ])
week44timeFridayA2 <- with(week44FridayA2, week44FridayA2[(time >= '17:00:00' & time <= '20:00:00'), ])
week44dataFridayA2 <- week44timeFridayA2[c(6,9)]
print(week44dataFridayA2)

# Week 45 
week45FridayA2 <- with(anomalies2, anomalies2[(date >= '2010/10/08' & date < '2010/10/09'), ])
week45timeFridayA2 <- with(week45FridayA2, week45FridayA2[(time >= '17:00:00' & time <= '20:00:00'), ])
week45dataFridayA2 <- week45timeFridayA2[c(6,9)]
print(week45dataFridayA2)

# Week 46 
week46FridayA2 <- with(anomalies2, anomalies2[(date >= '2010/10/15' & date < '2010/10/16'), ])
week46timeFridayA2 <- with(week46FridayA2, week46FridayA2[(time >= '17:00:00' & time <= '20:00:00'), ])
week46dataFridayA2 <- week46timeFridayA2[c(6,9)]
print(week46dataFridayA2)

# Week 47
week47FridayA2 <- with(anomalies2, anomalies2[(date >= '2010/10/22' & date < '2010/10/23'), ])
week47timeFridayA2 <- with(week47FridayA2, week47FridayA2[(time >= '17:00:00' & time <= '20:00:00'), ])
week47dataFridayA2 <- week47timeFridayA2[c(6,9)]
print(week47dataFridayA2)

# Week 48 
week48FridayA2 <- with(anomalies2, anomalies2[(date >= '2010/10/29' & date < '2010/10/30'), ])
week48timeFridayA2 <- with(week48FridayA2, week48FridayA2[(time >= '17:00:00' & time <= '20:00:00'), ])
week48dataFridayA2 <- week48timeFridayA2[c(6,9)]
print(week48dataFridayA2)

# Week 49 
week49FridayA2 <- with(anomalies2, anomalies2[(date >= '2010/11/05' & date < '2010/11/06'), ])
week49timeFridayA2 <- with(week49FridayA2, week49FridayA2[(time >= '17:00:00' & time <= '20:00:00'), ])
week49dataFridayA2 <- week49timeFridayA2[c(6,9)]
print(week49dataFridayA2)

# Week 50 
week50FridayA2 <- with(anomalies2, anomalies2[(date >= '2010/11/12' & date < '2010/11/13'), ])
week50timeFridayA2 <- with(week50FridayA2, week50FridayA2[(time >= '17:00:00' & time <= '20:00:00'), ])
week50dataFridayA2 <- week50timeFridayA2[c(6,9)]
print(week50dataFridayA2)

# Week 51
week51FridayA2 <- with(anomalies2, anomalies2[(date >= '2010/11/19' & date < '2010/11/20'), ])
week51timeFridayA2 <- with(week51FridayA2, week51FridayA2[(time >= '17:00:00' & time <= '20:00:00'), ])
week51dataFridayA2 <- week51timeFridayA2[c(6,9)]
print(week51dataFridayA2)

# Week 52
week52FridayA2 <- with(anomalies2, anomalies2[(date >= '2010/11/26' & date < '2010/11/27'), ])
week52timeFridayA2 <- with(week52FridayA2, week52FridayA2[(time >= '17:00:00' & time <= '20:00:00'), ])
week52dataFridayA2 <- week52timeFridayA2[c(6,9)]
print(week52dataFridayA2)

concatenatedAnomaly2Weeks = rbind(week1dataFridayA2,week2dataFridayA2,week3dataFridayA2,week4dataFridayA2,week5dataFridayA2,week6dataFridayA2,
                              week7dataFridayA2,week8dataFridayA2,week9dataFridayA2,week10dataFridayA2,week11dataFridayA2,week12dataFridayA2,
                              week13dataFridayA2,week14dataFridayA2,week15dataFridayA2,week16dataFridayA2,week17dataFridayA2,week18dataFridayA2,
                              week19dataFridayA2,week20dataFridayA2,week21dataFridayA2,week22dataFridayA2,week23dataFridayA2,week24dataFridayA2,
                              week25dataFridayA2,week26dataFridayA2,week27dataFridayA2,week28dataFridayA2,week29dataFridayA2,week30dataFridayA2,
                              week31dataFridayA2,week32dataFridayA2,week33dataFridayA2,week34dataFridayA2,week35dataFridayA2,week36dataFridayA2,
                              week37dataFridayA2,week38dataFridayA2,week39dataFridayA2,week40dataFridayA2,week41dataFridayA2,week42dataFridayA2,
                              week43dataFridayA2,week44dataFridayA2,week45dataFridayA2,week46dataFridayA2,week47dataFridayA2,week48dataFridayA2,
                              week49dataFridayA2,week50dataFridayA2,week51dataFridayA2,week52dataFridayA2)

###############################################
#         Anomaly 3 Detection Data            #
###############################################
pathA3 <- getwd()
setwd(pathA3)

fileAnomalies3 <- read.table("./DataWithAnomalies3.txt", header = TRUE, sep = ",")

fileAnomalies3$date <- as.POSIXlt(fileAnomalies3$Date, format='%d/%m/%Y')

fileAnomalies3$time <- chron(times = fileAnomalies3$Time)

anomalies3 <- with(fileAnomalies3, fileAnomalies3[(date >= '2009/12/1' & date < '2010/11/27'), ])

# Week 1 
week1FridayA3 <- with(anomalies3, anomalies3[(date >= '2009/12/4' & date < '2009/12/5'), ])
week1timeFridayA3 <- with(week1FridayA3, week1FridayA3[(time >= '17:00:00' & time <= '20:00:00'), ])
week1dataFridayA3 <- week1timeFridayA3[c(6,9)]
print(week1dataFridayA3)

# Week 2 
week2FridayA3 <- with(anomalies3, anomalies3[(date >= '2009/12/11' & date < '2009/12/12'), ])
week2timeFridayA3 <- with(week2FridayA3, week2FridayA3[(time >= '17:00:00' & time <= '20:00:00'), ])
week2dataFridayA3 <- week2timeFridayA3[c(6,9)]
print(week2dataFridayA3)

# Week 3
week3FridayA3 <- with(anomalies3, anomalies3[(date >= '2009/12/18' & date < '2009/12/19'), ])
week3timeFridayA3 <- with(week3FridayA3, week3FridayA3[(time >= '17:00:00' & time <= '20:00:00'), ])
week3dataFridayA3 <- week3timeFridayA3[c(6,9)]
print(week3dataFridayA3)

# Week 4 
week4FridayA3 <- with(anomalies3, anomalies3[(date >= '2009/12/25' & date < '2009/12/26'), ])
week4timeFridayA3 <- with(week4FridayA3, week4FridayA3[(time >= '17:00:00' & time <= '20:00:00'), ])
week4dataFridayA3 <- week4timeFridayA3[c(6,9)]
print(week4dataFridayA3)

# Week 5 
week5FridayA3 <- with(anomalies3, anomalies3[(date >= '2010/01/01' & date < '2010/01/02'), ])
week5timeFridayA3 <- with(week5FridayA3, week5FridayA3[(time >= '17:00:00' & time <= '20:00:00'), ])
week5dataFridayA3 <- week5timeFridayA3[c(6,9)]
print(week5dataFridayA3)

# Week 6
week6FridayA3 <- with(anomalies3, anomalies3[(date >= '2010/01/08' & date < '2010/01/09'), ])
week6timeFridayA3 <- with(week6FridayA3, week6FridayA3[(time >= '17:00:00' & time <= '20:00:00'), ])
week6dataFridayA3 <- week6timeFridayA3[c(6,9)]
print(week6dataFridayA3)

# Week 7 
week7FridayA3 <- with(anomalies3, anomalies3[(date >= '2010/01/15' & date < '2010/01/16'), ])
week7timeFridayA3 <- with(week7FridayA3, week7FridayA3[(time >= '17:00:00' & time <= '20:00:00'), ])
week7dataFridayA3 <- week7timeFridayA3[c(6,9)]
print(week7dataFridayA3)

# Week 8 
week8FridayA3 <- with(anomalies3, anomalies3[(date >= '2010/01/22' & date < '2010/01/23'), ])
week8timeFridayA3 <- with(week8FridayA3, week8FridayA3[(time >= '17:00:00' & time <= '20:00:00'), ])
week8dataFridayA3 <- week8timeFridayA3[c(6,9)]
print(week8dataFridayA3)

# Week 9
week9FridayA3 <- with(anomalies3, anomalies3[(date >= '2010/01/29' & date < '2010/01/30'), ])
week9timeFridayA3 <- with(week9FridayA3, week9FridayA3[(time >= '17:00:00' & time <= '20:00:00'), ])
week9dataFridayA3 <- week9timeFridayA3[c(6,9)]
print(week9dataFridayA3)

# Week 10
week10FridayA3 <- with(anomalies3, anomalies3[(date >= '2010/02/05' & date < '2010/02/06'), ])
week10timeFridayA3 <- with(week10FridayA3, week10FridayA3[(time >= '17:00:00' & time <= '20:00:00'), ])
week10dataFridayA3 <- week10timeFridayA3[c(6,9)]
print(week10dataFridayA3)

# Week 11
week11FridayA3 <- with(anomalies3, anomalies3[(date >= '2010/02/12' & date < '2010/02/13'), ])
week11timeFridayA3 <- with(week11FridayA3, week11FridayA3[(time >= '17:00:00' & time <= '20:00:00'), ])
week11dataFridayA3 <- week11timeFridayA3[c(6,9)]
print(week11dataFridayA3)

# Week 12 
week12FridayA3 <- with(anomalies3, anomalies3[(date >= '2010/02/19' & date < '2010/02/20'), ])
week12timeFridayA3 <- with(week12FridayA3, week12FridayA3[(time >= '17:00:00' & time <= '20:00:00'), ])
week12dataFridayA3 <- week12timeFridayA3[c(6,9)]
print(week12dataFridayA3)

# Week 13
week13FridayA3 <- with(anomalies3, anomalies3[(date >= '2010/02/26' & date < '2010/02/27'), ])
week13timeFridayA3 <- with(week13FridayA3, week13FridayA3[(time >= '17:00:00' & time <= '20:00:00'), ])
week13dataFridayA3 <- week13timeFridayA3[c(6,9)]
print(week13dataFridayA3)

# Week 14
week14FridayA3 <- with(anomalies3, anomalies3[(date >= '2010/03/05' & date < '2010/03/06'), ])
week14timeFridayA3 <- with(week14FridayA3, week14FridayA3[(time >= '17:00:00' & time <= '20:00:00'), ])
week14dataFridayA3 <- week14timeFridayA3[c(6,9)]
print(week14dataFridayA3)

# Week 15 
week15FridayA3 <- with(anomalies3, anomalies3[(date >= '2010/03/12' & date < '2010/03/13'), ])
week15timeFridayA3 <- with(week15FridayA3, week15FridayA3[(time >= '17:00:00' & time <= '20:00:00'), ])
week15dataFridayA3 <- week15timeFridayA3[c(6,9)]
print(week15dataFridayA3)

# Week 16
week16FridayA3 <- with(anomalies3, anomalies3[(date >= '2010/03/18' & date < '2007/04/19'), ])
week16timeFridayA3 <- with(week16FridayA3, week16FridayA3[(time >= '17:00:00' & time <= '20:00:00'), ])
week16dataFridayA3 <- week16timeFridayA3[c(6,9)]
print(week16dataFridayA3)

# Week 17
week17FridayA3 <- with(anomalies3, anomalies3[(date >= '2010/03/26' & date < '2010/03/27'), ])
week17timeFridayA3 <- with(week17FridayA3, week17FridayA3[(time >= '17:00:00' & time <= '20:00:00'), ])
week17dataFridayA3 <- week17timeFridayA3[c(6,9)]
print(week17dataFridayA3)

# Week 18 
week18FridayA3 <- with(anomalies3, anomalies3[(date >= '2010/04/02' & date < '2010/04/03'), ])
week18timeFridayA3 <- with(week18FridayA3, week18FridayA3[(time >= '17:00:00' & time <= '20:00:00'), ])
week18dataFridayA3 <- week18timeFridayA3[c(6,9)]
print(week18dataFridayA3)

# Week 19
week19FridayA3 <- with(anomalies3, anomalies3[(date >= '2010/04/09' & date < '2010/04/10'), ])
week19timeFridayA3 <- with(week19FridayA3, week19FridayA3[(time >= '17:00:00' & time <= '20:00:00'), ])
week19dataFridayA3 <- week19timeFridayA3[c(6,9)]
print(week19dataFridayA3)

# Week 20
week20FridayA3 <- with(anomalies3, anomalies3[(date >= '2010/04/16' & date < '2010/04/17'), ])
week20timeFridayA3 <- with(week20FridayA3, week20FridayA3[(time >= '17:00:00' & time <= '20:00:00'), ])
week20dataFridayA3 <- week20timeFridayA3[c(6,9)]
print(week20dataFridayA3)

# Week 21 
week21FridayA3 <- with(anomalies3, anomalies3[(date >= '2010/04/23' & date < '2010/04/24'), ])
week21timeFridayA3 <- with(week21FridayA3, week21FridayA3[(time >= '17:00:00' & time <= '20:00:00'), ])
week21dataFridayA3 <- week21timeFridayA3[c(6,9)]
print(week21dataFridayA3)

# Week 22 
week22FridayA3 <- with(anomalies3, anomalies3[(date >= '2010/04/30' & date < '2010/05/01'), ])
week22timeFridayA3 <- with(week22FridayA3, week22FridayA3[(time >= '17:00:00' & time <= '20:00:00'), ])
week22dataFridayA3 <- week22timeFridayA3[c(6,9)]
print(week22dataFridayA3)

# Week 23 
week23FridayA3 <- with(anomalies3, anomalies3[(date >= '2010/05/07' & date < '2010/05/08'), ])
week23timeFridayA3 <- with(week23FridayA3, week23FridayA3[(time >= '17:00:00' & time <= '20:00:00'), ])
week23dataFridayA3 <- week23timeFridayA3[c(6,9)]
print(week23dataFridayA3)

# Week 24
week24FridayA3 <- with(anomalies3, anomalies3[(date >= '2010/05/14' & date < '2010/05/15'), ])
week24timeFridayA3 <- with(week24FridayA3, week24FridayA3[(time >= '17:00:00' & time <= '20:00:00'), ])
week24dataFridayA3 <- week24timeFridayA3[c(6,9)]
print(week24dataFridayA3)

# Week 25 
week25FridayA3 <- with(anomalies3, anomalies3[(date >= '2010/05/21' & date < '2010/05/22'), ])
week25timeFridayA3 <- with(week25FridayA3, week25FridayA3[(time >= '17:00:00' & time <= '20:00:00'), ])
week25dataFridayA3 <- week25timeFridayA3[c(6,9)]
print(week25dataFridayA3)

# Week 26
week26FridayA3 <- with(anomalies3, anomalies3[(date >= '2010/05/28' & date < '2010/05/29'), ])
week26timeFridayA3 <- with(week26FridayA3, week26FridayA3[(time >= '17:00:00' & time <= '20:00:00'), ])
week26dataFridayA3 <- week26timeFridayA3[c(6,9)]
print(week26dataFridayA3)

# Week 27 
week27FridayA3 <- with(anomalies3, anomalies3[(date >= '2010/06/04' & date < '2010/06/05'), ])
week27timeFridayA3 <- with(week27FridayA3, week27FridayA3[(time >= '17:00:00' & time <= '20:00:00'), ])
week27dataFridayA3 <- week27timeFridayA3[c(6,9)]
print(week27dataFridayA3)

# Week 28 
week28FridayA3 <- with(anomalies3, anomalies3[(date >= '2010/06/11' & date < '2010/06/12'), ])
week28timeFridayA3 <- with(week28FridayA3, week28FridayA3[(time >= '17:00:00' & time <= '20:00:00'), ])
week28dataFridayA3 <- week28timeFridayA3[c(6,9)]
print(week28dataFridayA3)

# Week 29
week29FridayA3 <- with(anomalies3, anomalies3[(date >= '2010/06/18' & date < '2010/06/19'), ])
week29timeFridayA3 <- with(week29FridayA3, week29FridayA3[(time >= '17:00:00' & time <= '20:00:00'), ])
week29dataFridayA3 <- week29timeFridayA3[c(6,9)]
print(week29dataFridayA3)

# Week 30
week30FridayA3 <- with(anomalies3, anomalies3[(date >= '2010/06/25' & date < '2010/06/26'), ])
week30timeFridayA3 <- with(week30FridayA3, week30FridayA3[(time >= '17:00:00' & time <= '20:00:00'), ])
week30dataFridayA3 <- week30timeFridayA3[c(6,9)]
print(week30dataFridayA3)

# Week 31
week31FridayA3 <- with(anomalies3, anomalies3[(date >= '2010/07/02' & date < '2010/07/03'), ])
week31timeFridayA3 <- with(week31FridayA3, week31FridayA3[(time >= '17:00:00' & time <= '20:00:00'), ])
week31dataFridayA3 <- week31timeFridayA3[c(6,9)]
print(week31dataFridayA3)

# Week 32 
week32FridayA3 <- with(anomalies3, anomalies3[(date >= '2010/07/09' & date < '2010/07/10'), ])
week32timeFridayA3 <- with(week32FridayA3, week32FridayA3[(time >= '17:00:00' & time <= '20:00:00'), ])
week32dataFridayA3 <- week32timeFridayA3[c(6,9)]
print(week32dataFridayA3)

# Week 33
week33FridayA3 <- with(anomalies3, anomalies3[(date >= '2010/07/16' & date < '2010/07/17'), ])
week33timeFridayA3 <- with(week33FridayA3, week33FridayA3[(time >= '17:00:00' & time <= '20:00:00'), ])
week33dataFridayA3 <- week33timeFridayA3[c(6,9)]
print(week33dataFridayA3)

# Week 34
week34FridayA3 <- with(anomalies3, anomalies3[(date >= '2010/07/23' & date < '2010/07/24'), ])
week34timeFridayA3 <- with(week34FridayA3, week34FridayA3[(time >= '17:00:00' & time <= '20:00:00'), ])
week34dataFridayA3 <- week34timeFridayA3[c(6,9)]
print(week34dataFridayA3)

# Week 35 
week35FridayA3 <- with(anomalies3, anomalies3[(date >= '2010/07/30' & date < '2010/07/31'), ])
week35timeFridayA3 <- with(week35FridayA3, week35FridayA3[(time >= '17:00:00' & time <= '20:00:00'), ])
week35dataFridayA3 <- week35timeFridayA3[c(6,9)]
print(week35dataFridayA3)

# Week 36 
week36FridayA3 <- with(anomalies3, anomalies3[(date >= '2010/08/06' & date < '2010/08/07'), ])
week36timeFridayA3 <- with(week36FridayA3, week36FridayA3[(time >= '17:00:00' & time <= '20:00:00'), ])
week36dataFridayA3 <- week36timeFridayA3[c(6,9)]
print(week36dataFridayA3)

# Week 37 
week37FridayA3 <- with(anomalies3, anomalies3[(date >= '2010/08/13' & date < '2010/08/14'), ])
week37timeFridayA3 <- with(week37FridayA3, week37FridayA3[(time >= '17:00:00' & time <= '20:00:00'), ])
week37dataFridayA3 <- week37timeFridayA3[c(6,9)]
print(week37dataFridayA3)

# Week 38 
week38FridayA3 <- with(anomalies3, anomalies3[(date >= '2010/08/20' & date < '2010/08/21'), ])
week38timeFridayA3 <- with(week38FridayA3, week38FridayA3[(time >= '17:00:00' & time <= '20:00:00'), ])
week38dataFridayA3 <- week38timeFridayA3[c(6,9)]
print(week38dataFridayA3)

# Week 39 
week39FridayA3 <- with(anomalies3, anomalies3[(date >= '2010/08/27' & date < '2010/08/28'), ])
week39timeFridayA3 <- with(week39FridayA3, week39FridayA3[(time >= '17:00:00' & time <= '20:00:00'), ])
week39dataFridayA3 <- week39timeFridayA3[c(6,9)]
print(week39dataFridayA3)

# Week 40
week40FridayA3 <- with(anomalies3, anomalies3[(date >= '2010/09/03' & date < '2010/09/04'), ])
week40timeFridayA3 <- with(week40FridayA3, week40FridayA3[(time >= '17:00:00' & time <= '20:00:00'), ])
week40dataFridayA3 <- week40timeFridayA3[c(6,9)]
print(week40dataFridayA3)

# Week 41
week41FridayA3 <- with(anomalies3, anomalies3[(date >= '2010/09/10' & date < '2010/09/11'), ])
week41timeFridayA3 <- with(week41FridayA3, week41FridayA3[(time >= '17:00:00' & time <= '20:00:00'), ])
week41dataFridayA3 <- week41timeFridayA3[c(6,9)]
print(week41dataFridayA3)

# Week 42
week42FridayA3 <- with(anomalies3, anomalies3[(date >= '2010/09/17' & date < '2010/09/18'), ])
week42timeFridayA3 <- with(week42FridayA3, week42FridayA3[(time >= '17:00:00' & time <= '20:00:00'), ])
week42dataFridayA3 <- week42timeFridayA3[c(6,9)]
print(week42dataFridayA3)

# Week 43
week43FridayA3 <- with(anomalies3, anomalies3[(date >= '2010/09/24' & date < '2010/09/25'), ])
week43timeFridayA3 <- with(week43FridayA3, week43FridayA3[(time >= '17:00:00' & time <= '20:00:00'), ])
week43dataFridayA3 <- week43timeFridayA3[c(6,9)]
print(week43dataFridayA3)

# Week 44
week44FridayA3 <- with(anomalies3, anomalies3[(date >= '2010/10/01' & date < '2010/10/02'), ])
week44timeFridayA3 <- with(week44FridayA3, week44FridayA3[(time >= '17:00:00' & time <= '20:00:00'), ])
week44dataFridayA3 <- week44timeFridayA3[c(6,9)]
print(week44dataFridayA3)

# Week 45 
week45FridayA3 <- with(anomalies3, anomalies3[(date >= '2010/10/08' & date < '2010/10/09'), ])
week45timeFridayA3 <- with(week45FridayA3, week45FridayA3[(time >= '17:00:00' & time <= '20:00:00'), ])
week45dataFridayA3 <- week45timeFridayA3[c(6,9)]
print(week45dataFridayA3)

# Week 46 
week46FridayA3 <- with(anomalies3, anomalies3[(date >= '2010/10/15' & date < '2010/10/16'), ])
week46timeFridayA3 <- with(week46FridayA3, week46FridayA3[(time >= '17:00:00' & time <= '20:00:00'), ])
week46dataFridayA3 <- week46timeFridayA3[c(6,9)]
print(week46dataFridayA3)

# Week 47
week47FridayA3 <- with(anomalies3, anomalies3[(date >= '2010/10/22' & date < '2010/10/23'), ])
week47timeFridayA3 <- with(week47FridayA3, week47FridayA3[(time >= '17:00:00' & time <= '20:00:00'), ])
week47dataFridayA3 <- week47timeFridayA3[c(6,9)]
print(week47dataFridayA3)

# Week 48 
week48FridayA3 <- with(anomalies3, anomalies3[(date >= '2010/10/29' & date < '2010/10/30'), ])
week48timeFridayA3 <- with(week48FridayA3, week48FridayA3[(time >= '17:00:00' & time <= '20:00:00'), ])
week48dataFridayA3 <- week48timeFridayA3[c(6,9)]
print(week48dataFridayA3)

# Week 49 
week49FridayA3 <- with(anomalies3, anomalies3[(date >= '2010/11/05' & date < '2010/11/06'), ])
week49timeFridayA3 <- with(week49FridayA3, week49FridayA3[(time >= '17:00:00' & time <= '20:00:00'), ])
week49dataFridayA3 <- week49timeFridayA3[c(6,9)]
print(week49dataFridayA3)

# Week 50 
week50FridayA3 <- with(anomalies3, anomalies3[(date >= '2010/11/12' & date < '2010/11/13'), ])
week50timeFridayA3 <- with(week50FridayA3, week50FridayA3[(time >= '17:00:00' & time <= '20:00:00'), ])
week50dataFridayA3 <- week50timeFridayA3[c(6,9)]
print(week50dataFridayA3)

# Week 51
week51FridayA3 <- with(anomalies3, anomalies3[(date >= '2010/11/19' & date < '2010/11/20'), ])
week51timeFridayA3 <- with(week51FridayA3, week51FridayA3[(time >= '17:00:00' & time <= '20:00:00'), ])
week51dataFridayA3 <- week51timeFridayA3[c(6,9)]
print(week51dataFridayA3)

# Week 52
week52FridayA3 <- with(anomalies3, anomalies3[(date >= '2010/11/26' & date < '2010/11/27'), ])
week52timeFridayA3 <- with(week52FridayA3, week52FridayA3[(time >= '17:00:00' & time <= '20:00:00'), ])
week52dataFridayA3 <- week52timeFridayA3[c(6,9)]
print(week52dataFridayA3)

concatenatedAnomaly3Weeks = rbind(week1dataFridayA3,week2dataFridayA3,week3dataFridayA3,week4dataFridayA3,week5dataFridayA3,week6dataFridayA3,
                              week7dataFridayA3,week8dataFridayA3,week9dataFridayA3,week10dataFridayA3,week11dataFridayA3,week12dataFridayA3,
                              week13dataFridayA3,week14dataFridayA3,week15dataFridayA3,week16dataFridayA3,week17dataFridayA3,week18dataFridayA3,
                              week19dataFridayA3,week20dataFridayA3,week21dataFridayA3,week22dataFridayA3,week23dataFridayA3,week24dataFridayA3,
                              week25dataFridayA3,week26dataFridayA3,week27dataFridayA3,week28dataFridayA3,week29dataFridayA3,week30dataFridayA3,
                              week31dataFridayA3,week32dataFridayA3,week33dataFridayA3,week34dataFridayA3,week35dataFridayA3,week36dataFridayA3,
                              week37dataFridayA3,week38dataFridayA3,week39dataFridayA3,week40dataFridayA3,week41dataFridayA3,week42dataFridayA3,
                              week43dataFridayA3,week44dataFridayA3,week45dataFridayA3,week46dataFridayA3,week47dataFridayA3,week48dataFridayA3,
                              week49dataFridayA3,week50dataFridayA3,week51dataFridayA3,week52dataFridayA3)

scaledAnomaly1Data <- scale(concatenatedAnomaly1Weeks, center = TRUE, scale = TRUE)
anomaly1DataScaled <- data.frame(scaledAnomaly1Data)


scaledAnomaly2Data <- scale(concatenatedAnomaly2Weeks, center = TRUE, scale = TRUE)
anomaly2DataScaled <- data.frame(scaledAnomaly2Data)


scaledAnomaly3Data <- scale(concatenatedAnomaly3Weeks, center = TRUE, scale = TRUE)
anomaly3DataScaled <- data.frame(scaledAnomaly3Data)


###############################################

modA1 <- depmix(response = list(Global_intensity~1, Sub_metering_3~1),
                    data = anomaly1DataScaled, nstates = 12,
                    family = list(gaussian(), gaussian()),
                    ntimes = 9231)
modA1 <- setpars(modA1, getpars(fm3))
fbA1 <- forwardbackward(modA1, return.all = FALSE)
print(fbA1$logLike)

###############################################

modA2 <- depmix(response = list(Global_intensity~1, Sub_metering_3~1),
                    data = anomaly2DataScaled, nstates = 12,
                    family = list(gaussian(), gaussian()),
                    ntimes = 9231)
modA2 <- setpars(modA2, getpars(fm3))
fbA2 <- forwardbackward(modA2, return.all = FALSE)
print(fbA2$logLike)

###############################################

modA3 <- depmix(response = list(Global_intensity~1, Sub_metering_3~1),
                    data = anomaly3DataScaled, nstates = 12,
                    family = list(gaussian(), gaussian()),
                    ntimes = 9231)
modA3 <- setpars(modA3, getpars(fm3))
fbA3 <- forwardbackward(modA3, return.all = FALSE)
print(fbA3$logLike)

###############################################
###############################################

modA1state5 <- depmix(response = list(Global_intensity~1, Sub_metering_3~1),
                data = anomaly1DataScaled, nstates = 20,
                family = list(gaussian(), gaussian()),
                ntimes = 9231)
modA1state5 <- setpars(modA1state5, getpars(fm5))
fbA1s5 <- forwardbackward(modA1state5, return.all = FALSE)
print(fbA1s5$logLike)

###############################################

modA2state5 <- depmix(response = list(Global_intensity~1, Sub_metering_3~1),
                      data = anomaly2DataScaled, nstates = 20,
                      family = list(gaussian(), gaussian()),
                      ntimes = 9231)
modA2state5 <- setpars(modA2state5, getpars(fm5))
fbA2s5 <- forwardbackward(modA2state5, return.all = FALSE)
print(fbA2s5$logLike)

###############################################

modA3state5 <- depmix(response = list(Global_intensity~1, Sub_metering_3~1),
                      data = anomaly3DataScaled, nstates = 20,
                      family = list(gaussian(), gaussian()),
                      ntimes = 9231)
modA3state5 <- setpars(modA3state5, getpars(fm5))
fbA3s5 <- forwardbackward(modA3state5, return.all = FALSE)
print(fbA3s5$logLike)

###############################################
###############################################

modA1state6 <- depmix(response = list(Global_intensity~1, Sub_metering_3~1),
                      data = anomaly1DataScaled, nstates = 24,
                      family = list(gaussian(), gaussian()),
                      ntimes = 9231)
modA1state6 <- setpars(modA1state6, getpars(fm6))
fbA1s6 <- forwardbackward(modA1state6, return.all = FALSE)
print(fbA1s6$logLike)

###############################################

modA2state6 <- depmix(response = list(Global_intensity~1, Sub_metering_3~1),
                      data = anomaly2DataScaled, nstates = 24,
                      family = list(gaussian(), gaussian()),
                      ntimes = 9231)
modA2state6 <- setpars(modA2state6, getpars(fm6))
fbA2s6 <- forwardbackward(modA2state6, return.all = FALSE)
print(fbA2s6$logLike)

###############################################

modA3state6 <- depmix(response = list(Global_intensity~1, Sub_metering_3~1),
                      data = anomaly3DataScaled, nstates = 24,
                      family = list(gaussian(), gaussian()),
                      ntimes = 9231)
modA3state6 <- setpars(modA3state6, getpars(fm6))
fbA3s6 <- forwardbackward(modA3state6, return.all = FALSE)
print(fbA3s6$logLike)


# ###############################################
# install.packages("devtools")
# devtools::install_github("twitter/AnomalyDetection")
# library(AnomalyDetection)
# # library(zoo)
# # z <- zoo(anomalies$Global_intensity)
# # na.approx(z)
# AnomalyDetectionVec(na.omit(anomaly1DataScaled$Global_intensity), max_anoms=0.02, 
#                     period=1440, direction='both', plot=TRUE)

logLikeStateswise <- c(fbA1$logLike, fbA2$logLike, fbA3$logLike, 
                   fbA1s5$logLike, fbA2s5$logLike, fbA3s5$logLike,
                   fbA1s6$logLike,fbA2s6$logLike, fbA3s6$logLike)

logLikeStates <- c(fbA1$logLike,fbA1s5$logLike,fbA1s6$logLike,
                   fbA2$logLike,fbA2s5$logLike,fbA2s6$logLike,
                   fbA3$logLike, fbA3s5$logLike, fbA3s6$logLike)

plot(1:9,logLikeStates, col="blue", ty ="line")
plot(1:9,logLikeStateswise, col="red", ty ="line")
