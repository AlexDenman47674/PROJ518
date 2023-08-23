#Dependencies need to be installed before analysis can begin
#RDieharder contains the test suite that will be used and RJson allows for the reading of JSON files
install.packages("RDieHarder")
install.packages("rjson")
install.packages("dgof")
install.packages("Rtools")
install.packages("randtoolbox")
install.packages("randtests")
install.packages("EnvStats")
install.packages("CryptRndTest")

library(rjson)
library(plyr)
library(dplyr)
library(ggplot2)
library(dgof)
library(randtoolbox)
library(randtests)
library(EnvStats)
library(CryptRndTest)

#The working directory is set to allow access to stored JSON files
setwd("D:/Github/PROJ518/C#_Rand_Function/RandFunctionOutput")

#Retrieving the Dice Sim data from the C# output
CDiceSim1Values <- fromJSON(file = "C#DiceSim1.json")
CDiceSim2Values <- fromJSON(file = "C#DiceSim2.json")

print(CDiceSim1Values)
print(CDiceSim2Values)

#Retrieving the Dice Sim data from the Python output
setwd("D:/Github/PROJ518/Python_Rand_Function/Python_Output")
PDiceSim1Values <- fromJSON(file = "PythonDiceOutput1.json")
PDiceSim2Values <- fromJSON(file = "PythonDiceOutput2.json")
PDiceSim3Values <- fromJSON(file = "PythonDiceOutput3.json")

print(PDiceSim1Values)
print(PDiceSim2Values)
print(PDiceSim3Values)

#Retrieving the Dice Sim data from the JavaScript output
setwd("D:/Github/PROJ518/Javascript_Rand/Javascript_Rand_Output")
JSDiceSim1Values <- fromJSON(file = "JavaScriptDiceSim.json")
print(JSDiceSim1Values)

#Retrieving the Dice Sim data from Random Dot Org
setwd("D:/Github/PROJ518/RandDotOrg/RandDotOrgOutput")
RndDiceSim1Values <- fromJSON(file = "RandDotOrgDiceSim.json")
print(RndDiceSim1Values)

#Retrieving the Real Dice Roll data
setwd("D:/Github/PROJ518/Dice_Rolls")
RealDiceValues <- fromJSON(file = "DiceRolls.json")
print(RealDiceValues)

#Chi-Squared Test
#Due to the nature of the data provided for coin flips and dice rolls, the chi-squared test can be used to help determine randomness
#In these tests all three pseudorandom algorithms from C#, Python and JavaScript will be compared against the physical dice roll and coin flip data
#This test will be run as seen in D Knuth's 'The Art of Computer Programming Volume 2: Seminumerical Algorithms'

#1a) Dice Rolls
#The first step is to determine the expected number (np) of each possible value (in this case faces 1 to 6)
#The probability that any face is rolled on a standard die such as the ones used for testing is always 1/6
#Therefore the expected number of times each face should occur after 500 rolls is: 500 * 0.166666...

np <- 500 * (1/6)
print(np)

#Each face should therefore occur roughly 83 times after 500 rolls
#The expected number can then be compared against the observed number (Yn)

#Counting the frequency of each face
CDice1 <- as.data.frame(table(CDiceSim1Values))
print(CDice1)

CDice2 <- as.data.frame(table(CDiceSim2Values))
print(CDice2)

PDice1 <- as.data.frame(table(PDiceSim1Values))
print(PDice1)

PDice2 <- as.data.frame(table(PDiceSim2Values))
print(PDice2)

PDice3 <- as.data.frame(table(PDiceSim3Values))
print(PDice3)

JSDice <- as.data.frame(table(JSDiceSim1Values))
print(JSDice)

RndDice <- as.data.frame(table(RndDiceSim1Values))
print(RndDice)

RealDice <- as.data.frame(table(RealDiceValues))
print(RealDice)

#Performing the Chi Squared equation
#The Chi Squared equation: V = ((Yn - np)^2 / np) + ((Yn+1 - np)^2 / np)  + ...

V_CDice1 <- ((CDice1[1, 2] - np)^2 / np) + ((CDice1[2, 2] - np)^2 / np) + ((CDice1[3, 2] - np)^2 / np) + ((CDice1[4, 2] - np)^2 / np) + ((CDice1[5, 2] - np)^2 / np) + ((CDice1[6, 2] - np)^2 / np)
print(V_CDice1)
V_CDice2 <- ((CDice2[1, 2] - np)^2 / np) + ((CDice2[2, 2] - np)^2 / np) + ((CDice2[3, 2] - np)^2 / np) + ((CDice2[4, 2] - np)^2 / np) + ((CDice2[5, 2] - np)^2 / np) + ((CDice2[6, 2] - np)^2 / np)
print(V_CDice2)

#For the C# generations the first method had a V = 1.192 while the second method had a V = 4.024

V_PDice1 <- ((PDice1[1, 2] - np)^2 / np) + ((PDice1[2, 2] - np)^2 / np) + ((PDice1[3, 2] - np)^2 / np) + ((PDice1[4, 2] - np)^2 / np) + ((PDice1[5, 2] - np)^2 / np) + ((PDice1[6, 2] - np)^2 / np)
print(V_PDice1)
V_PDice2 <- ((PDice2[1, 2] - np)^2 / np) + ((PDice2[2, 2] - np)^2 / np) + ((PDice2[3, 2] - np)^2 / np) + ((PDice2[4, 2] - np)^2 / np) + ((PDice2[5, 2] - np)^2 / np) + ((PDice2[6, 2] - np)^2 / np)
print(V_PDice2)
V_PDice3 <- ((PDice3[1, 2] - np)^2 / np) + ((PDice3[2, 2] - np)^2 / np) + ((PDice3[3, 2] - np)^2 / np) + ((PDice3[4, 2] - np)^2 / np) + ((PDice3[5, 2] - np)^2 / np) + ((PDice3[6, 2] - np)^2 / np)
print(V_PDice3)

#For the Python generations the first method had a v = 3.064 while the second method had a v = 2.536 and the third method had a v = 3.88

V_JSDice <- ((JSDice[1, 2] - np)^2 / np) + ((JSDice[2, 2] - np)^2 / np) + ((JSDice[3, 2] - np)^2 / np) + ((JSDice[4, 2] - np)^2 / np) + ((JSDice[5, 2] - np)^2 / np) + ((JSDice[6, 2] - np)^2 / np)
print(V_JSDice)

#For the JavaScript generations v = 37.12

V_RndDice <- ((RndDice[1, 2] - np)^2 / np) + ((RndDice[2, 2] - np)^2 / np) + ((RndDice[3, 2] - np)^2 / np) + ((RndDice[4, 2] - np)^2 / np) + ((RndDice[5, 2] - np)^2 / np) + ((RndDice[6, 2] - np)^2 / np)
print(V_RndDice)

#For the Rand Dot Org generations v = 4.936

V_RealDice <- ((RealDice[1, 2] - np)^2 / np) + ((RealDice[2, 2] - np)^2 / np) + ((RealDice[3, 2] - np)^2 / np) + ((RealDice[4, 2] - np)^2 / np) + ((RealDice[5, 2] - np)^2 / np) + ((RealDice[6, 2] - np)^2 / np)
print(V_RealDice)

#For the real dice rolls v = 6.28


#1b) Coin Flips
#Retrieving the Coin Sim data from the C# output
setwd("D:/Github/PROJ518/C#_Rand_Function/RandFunctionOutput")
CCoinSim1Values <- fromJSON(file = "C#CoinSim1.json")
CCoinSim2Values <- fromJSON(file = "C#CoinSim2.json")

print(CCoinSim1Values)
print(CCoinSim2Values)

#Retrieving the Coin Sim data from the Python output
setwd("D:/Github/PROJ518/Python_Rand_Function/Python_Output")
PCoinSim1Values <- fromJSON(file = "PythonCoinSim1.json")
PCoinSim2Values <- fromJSON(file = "PythonCoinSim2.json")
PCoinSim3Values <- fromJSON(file = "PythonCoinSim3.json")

print(PCoinSim1Values)
print(PCoinSim2Values)
print(PCoinSim3Values)

#Retrieving the Coin Sim data from the JavaScript output
setwd("D:/Github/PROJ518/Javascript_Rand/Javascript_Rand_Output")
JSCoinSim1Values <- fromJSON(file = "JavaScriptCoinSim.json")
print(JSCoinSim1Values)

#Retrieving the Coin Sim data from Random Dot Org
setwd("D:/Github/PROJ518/RandDotOrg/RandDotOrgOutput")
RndCoinSim1Values <- fromJSON(file = "RandDotOrgCoinSim.json")
print(RndCoinSim1Values)

#Retrieving the Real Coin Flip data
setwd("D:/Github/PROJ518/Coin_Flips")
RealCoinValues <- fromJSON(file = "Coin_Flips.json")
print(RealCoinValues)

#Performing the Chi Squared equation
#As a coin is being simulated instead of dice, the np must be adjusted to the new set of possible outcomes 0 (heads) or 1 (tails)
np <- 500 * (1/2)
print(np)
#Each side should ideally appear 250 times each

#Counting the frequency of each side
CCoin1 <- as.data.frame(table(CCoinSim1Values))
print(CCoin1)

CCoin2 <- as.data.frame(table(CCoinSim2Values))
print(CCoin2)

PCoin1 <- as.data.frame(table(PCoinSim1Values))
print(PCoin1)

PCoin2 <- as.data.frame(table(PCoinSim2Values))
print(PCoin2)

PCoin3 <- as.data.frame(table(PCoinSim3Values))
print(PCoin3)

JSCoin <- as.data.frame(table(JSCoinSim1Values))
print(JSCoin)

RndCoin <- as.data.frame(table(RndCoinSim1Values))
print(RndCoin)

RealCoin <- as.data.frame(table(RealCoinValues))
print(RealCoin)

V_CCoin1 <- ((CCoin1[1, 2] - np)^2 / np) + ((CCoin1[2, 2] - np)^2 / np)
print(V_CCoin1)
V_CCoin2 <- ((CCoin2[1, 2] - np)^2 / np) + ((CCoin2[2, 2] - np)^2 / np)
print(V_CCoin2)

#For the C# generations the first method had a V = 1.352 while the second method had a V = 0.8

V_PCoin1 <- ((PCoin1[1, 2] - np)^2 / np) + ((PCoin1[2, 2] - np)^2 / np)
print(V_PCoin1)
V_PCoin2 <- ((PCoin2[1, 2] - np)^2 / np) + ((PCoin2[2, 2] - np)^2 / np)
print(V_PCoin2)
V_PCoin3 <- ((PCoin3[1, 2] - np)^2 / np) + ((PCoin3[2, 2] - np)^2 / np)
print(V_PCoin3)

#For the Python generations the first method had a v = 0.968 while the second method had a v = 0.008 and the third method had a v = 0.008

V_JSCoin <- ((JSCoin[1, 2] - np)^2 / np) + ((JSCoin[2, 2] - np)^2 / np)
print(V_JSCoin)

#For the JavaScript generations v = 0.128

V_RndCoin <- ((RndCoin[1, 2] - np)^2 / np) + ((RndCoin[2, 2] - np)^2 / np)
print(V_RndCoin)

#For the Rand Dot Org generations v = 0.512

V_RealCoin <- ((RealCoin[1, 2] - np)^2 / np) + ((RealCoin[2, 2] - np)^2 / np)
print(V_RealCoin)

#For the real dice rolls v = 0.128

#2a) Visualisation of Dice Data
CDice1Freq <- CDice1[,2]
CDice2Freq <- CDice2[,2]
PDice1Freq <- PDice1[,2]
PDice2Freq <- PDice2[,2]
PDice3Freq <- PDice3[,2]
JSDiceFreq <- JSDice[,2]
RndDiceFreq <- RndDice[,2]
RealDiceFreq <- RealDice[,2]
DiceFreq <- c(CDice1Freq,CDice2Freq, PDice1Freq, PDice2Freq, PDice3Freq, JSDiceFreq, RndDiceFreq, RealDiceFreq)
print(DiceFreq)
DiceOutcomes <- c(1,2,3,4,5,6,1,2,3,4,5,6,1,2,3,4,5,6,1,2,3,4,5,6,1,2,3,4,5,6,1,2,3,4,5,6,1,2,3,4,5,6,1,2,3,4,5,6)
DiceGroup <- c("C1","C1","C1","C1","C1","C1","C2","C2","C2","C2","C2","C2","P1","P1","P1","P1","P1","P1","P2","P2","P2","P2","P2","P2",
               "P3","P3","P3","P3","P3","P3","JS","JS","JS","JS","JS","JS","Rnd","Rnd","Rnd","Rnd","Rnd","Rnd","Real","Real","Real","Real","Real","Real")
Dice_DF <- data.frame(DiceOutcomes, DiceGroup, DiceFreq)
print(Dice_DF)

DiceGroup_DF <- Dice_DF %>% group_by(DiceGroup)
print(DiceGroup_DF)

#Lined Scatterplot of Dice data
ggplot(DiceGroup_DF, aes(x = DiceOutcomes, y = DiceFreq, colour = DiceGroup)) + 
  geom_point(size = 3) + geom_smooth(linetype="dashed") + labs(x = "Possible Dice Outcomes (1-6)", y = "Frequency", color = "Data Sources", title = "A Scatterplot Showing the Frequency of Dice Outcomes")

#2b) Visualisation of Coin Data
CCoin1Freq <- CCoin1[,2]
CCoin2Freq <- CCoin2[,2]
PCoin1Freq <- PCoin1[,2]
PCoin2Freq <- PCoin2[,2]
PCoin3Freq <- PCoin3[,2]
JSCoinFreq <- JSCoin[,2]
RndCoinFreq <- RndCoin[,2]
RealCoinFreq <- RealCoin[,2]
CoinFreq <- c(CCoin1Freq, CCoin2Freq, PCoin1Freq, PCoin2Freq, PCoin3Freq, JSCoinFreq, RndCoinFreq, RealCoinFreq)
print(CoinFreq)
CoinOutcomes <- c(0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1)
CoinGroup <- c("C1","C1","C2","C2","P1","P1","P2","P2","P3",
               "P3","JS","JS","Rnd","Rnd","Real","Real")
Coin_DF <- data.frame(CoinOutcomes, CoinGroup, CoinFreq)
print(Coin_DF)

CoinGroup_DF <- Coin_DF %>% group_by(CoinGroup)
print(CoinGroup_DF)

#Bar Graph of Coin Data
ggplot(CoinGroup_DF, aes(x = CoinGroup, y = CoinFreq, fill = factor(CoinOutcomes))) + 
  geom_bar(stat = "identity", width=0.5, position="dodge") +
  scale_fill_discrete(name="Possible Coin Outcomes",breaks=c(0, 1),labels=c("Heads", "Tails")) + labs(x = "Data Sources", y = "Frequency", title = "A Barchart Showing the Frequency of Coin Outcomes")

#3a) Kolmogorov-Smirnov Test of C# rand data
#The Kolmogorov-Smirnov Test takes  a collection of random data between 0 and 1 then calculates the equidistribution of the data
#A balanced generator will provide an equal distribution of all possible values between 0 and 1 as equal weighting is used
#As not all data generated was orginally between 0 and 1, a division of 100 was used on some data sets to ensure they align with the requirements of the KS test
#Due to the generators providing results between 0 and 100 this will not effect the accuracy or weighting of the generators

#Retrieving the C# rand data from the C# output
setwd("D:/Github/PROJ518/C#_Rand_Function/RandFunctionOutput")
CRand1Values <- fromJSON(file = "RandVer1.json")
CRand2Values <- fromJSON(file = "RandVer2.json")
CRand3Values <- fromJSON(file = "RandVer3.json")
print(CRand1Values)
print(CRand2Values)
print(CRand3Values)

#1 sample tests
ks.test(CRand1Values/100, "pnorm")
ks.test(CRand2Values/100, "pnorm")
ks.test(CRand3Values/100, "pnorm")

#2 sample tests
ks.test(CRand1Values/100, CRand2Values/100)
ks.test(CRand1Values/100, CRand3Values/100)
ks.test(CRand2Values/100, CRand3Values/100)

#Visualisation
plot(ecdf(CRand1Values/100), main="Distribution of C# Rand Data",
     xlim = range(c(CRand1Values/100, CRand2Values/100)),
     col = "blue")
plot(ecdf(CRand2Values/100),
     add = TRUE,
     lty = "dashed",
     col = "red")
plot(ecdf(CRand3Values/100),
     add = TRUE,
     lty = "dashed",
     col = "green")
legend(0, 1, legend=c("C# Rand", "C# Seeded Rand","C# Cryptographic Rand"),
       col=c("red", "blue", "green"), lty=1:2, cex=0.8)

#3b) Kolmogorov-Smirnov Test of Lehmer rand data
setwd("D:/Github/PROJ518/Lehmer_Generator/Lehmer_Generator_Output")
LehmerInt1Values <- fromJSON(file = "IntegerVer1Output.json")
LehmerInt2Values <- fromJSON(file = "IntegerVer2Output.json")
LehmerReal1Values <- fromJSON(file = "RealVer1Output.json")
LehmerReal2Values <- fromJSON(file = "RealVer2Output.json")
print(LehmerInt1Values)
print(LehmerInt2Values)
print(LehmerReal1Values)
print(LehmerReal2Values)

#1 sample tests
ks.test(LehmerInt1Values, "pnorm")
ks.test(LehmerInt2Values, "pnorm")
ks.test(LehmerReal1Values, "pnorm")
ks.test(LehmerReal2Values, "pnorm")

#2 sample tests
ks.test(LehmerInt1Values, LehmerInt2Values)
ks.test(LehmerReal1Values, LehmerReal2Values)

#Visualisation
plot(ecdf(LehmerReal1Values), main="Distribution of Lehmer Generator Data",
     xlim = range(c(LehmerReal1Values, LehmerReal2Values)),
     lty = "dashed",
     col = "blue")
plot(ecdf(LehmerReal2Values),
     add = TRUE,
     lty = "dashed",
     col = "red")

plot(ecdf(LehmerInt1Values),
     add = TRUE,
     xlim = range(c(LehmerInt1Values, LehmerInt2Values)),
     lty = "dashed",
     col = "green")
plot(ecdf(LehmerInt2Values),
     add = TRUE,
     lty = "dashed",
     col = "purple")
legend(0, 1, legend=c("Lehmer Real 1", "Lehmer Real 2","Lehmer Int 1","Lehmer Int 2"),
       col=c("red", "blue", "green", "purple"), lty=1:2, cex=0.8)

#3c) Kolmogorov-Smirnov Test of Python rand data
setwd("D:/Github/PROJ518/Python_Rand_Function/Python_Output")
PRand1Values <- fromJSON(file = "PythonOutput1.json")
PRand2Values <- fromJSON(file = "PythonOutput2.json")
PRand3Values <- fromJSON(file = "PythonOutput3.json")
PRand4Values <- fromJSON(file = "PythonOutput4.json")
PRand5Values <- fromJSON(file = "PythonOutput5.json")

#1 sample tests
ks.test(PRand1Values/100, "pnorm")
ks.test(PRand2Values, "pnorm")
ks.test(PRand3Values, "pnorm")
ks.test(PRand4Values/100, "pnorm")
ks.test(PRand5Values/100, "pnorm")

#2 sample tests
ks.test(PRand2Values, PRand3Values)
ks.test(PRand1Values/100, PRand4Values/100)
ks.test(PRand1Values/100, PRand5Values/100)
ks.test(PRand4Values/100, PRand5Values/100)

#Visualisation
plot(ecdf(PRand1Values/100), main="Distribution of Python Rand Data",
     xlim = range(c(PRand1Values/100, PRand2Values)),
     lty = "dashed",
     col = "blue")
plot(ecdf(PRand2Values),
     add = TRUE,
     lty = "dashed",
     col = "red")

plot(ecdf(PRand3Values),
     add = TRUE,
     lty = "dashed",
     col = "green")
plot(ecdf(PRand4Values/100),
     add = TRUE,
     lty = "dashed",
     col = "purple")
plot(ecdf(PRand5Values/100),
     add = TRUE,
     lty = "dashed",
     col = "orange")
legend(0, 1, legend=c("Python Randint", "Python Random","Python Seeded Random","Numpy Randint","Numpy Seeded Randint"),
       col=c("red", "blue", "green", "purple","orange"), lty=1:2, cex=0.8)

#3d) Kolmogorov-Smirnov Test of Javascript rand data
setwd("D:/Github/PROJ518/Javascript_Rand/Javascript_Rand_Output")
JSRandValues <- fromJSON(file = "JavaScriptOutput.json")

#1 sample test
ks.test(JSRandValues/100, "pnorm")

#Visualisation
plot(ecdf(JSRandValues/100), main="Distribution of JavaScript Rand Data",
     lty = "dashed",
     col = "blue")

#3e) Kolmogorov-Smirnov Test of Middle Square Method data
setwd("D:/Github/PROJ518/Middle_Square_Method/Middle_Square_Output")
MSRandValues <- fromJSON(file = "MiddleSquare.json")

#1 sample test
ks.test(MSRandValues/100, "pnorm")

#Visualisation
plot(ecdf(MSRandValues/100), main="Distribution of Middle Square Method Data",
     lty = "dashed",
     col = "blue")

#3f) Kolmogorov-Smirnov Test of Rand.Org Data
setwd("D:/Github/PROJ518/RandDotOrg/RandDotOrgOutput")
RndRandValues <- fromJSON(file = "RandDotOrgOutput1.json")

#1 sample test
ks.test(RndRandValues/100, "pnorm")

#Visualisation
plot(ecdf(RndRandValues/100), main="Distribution of Rand.Org Rand Data",
     lty = "dashed",
     col = "blue")

#3g) Kolmogorov-Smirnov Test of White Noise Data
setwd("D:/Github/PROJ518/White_Noise/White_Noise_Output")
ParkRandValues <-fromJSON(file = "ParkDataCut.json")
RoundaboutRandValues <-fromJSON(file = "RoundaboutDataCut.json")
SeaRandValues <-fromJSON(file = "SeaDataCut.json")

#1 sample tests
ks.test(ParkRandValues/100, "pnorm")
ks.test(RoundaboutRandValues/100, "pnorm")
ks.test(SeaRandValues/100, "pnorm")

#2 sample tests
ks.test(ParkRandValues/100, RoundaboutRandValues/100)
ks.test(ParkRandValues/100, SeaRandValues/100)
ks.test(RoundaboutRandValues/100, SeaRandValues/100)

#Visualisation
plot(ecdf(ParkRandValues/100), main="Distribution of White Noise Data",
     xlim = range(c(ParkRandValues/100, RoundaboutRandValues/100)),
     lty = "dashed",
     col = "blue")
plot(ecdf(RoundaboutRandValues/100),
     add = TRUE,
     lty = "dashed",
     col = "red")

plot(ecdf(SeaRandValues/100),
     add = TRUE,
     lty = "dashed",
     col = "green")
legend(-13, 1, legend=c("Park White Noise", "Roundabout White Noise","Sea White Noise"),
       col=c("red", "blue", "green"), lty=1:2, cex=0.8)

#3h) Visualisation of Kolmogorov-Smirnov Tests
CRand1Fn <- ecdf(CRand1Values)
CRand1Dist <- CRand1Fn(CRand1Values)
CRand2Fn <- ecdf(CRand2Values)
CRand2Dist <- CRand2Fn(CRand2Values)
CRand3Fn <- ecdf(CRand3Values)
CRand3Dist <- CRand3Fn(CRand3Values)

PRand1Fn <- ecdf(PRand1Values)
PRand1Dist <- PRand1Fn(PRand1Values)
PRand2Fn <- ecdf(PRand2Values)
PRand2Dist <- PRand2Fn(PRand2Values)
PRand3Fn <- ecdf(PRand3Values)
PRand3Dist <- PRand3Fn(PRand3Values)
PRand4Fn <- ecdf(PRand4Values)
PRand4Dist <- PRand4Fn(PRand4Values)
PRand5Fn <- ecdf(PRand5Values)
PRand5Dist <- PRand5Fn(PRand5Values)

JSRandFn <- ecdf(JSRandValues)
JSRandDist <- JSRandFn(JSRandValues)

RndRandFn <- ecdf(RndRandValues)
RndRandDist <- RndRandFn(RndRandValues)

LehmerInt1Fn <- ecdf(LehmerInt1Values)
LehmerInt1Dist <- LehmerInt1Fn(LehmerInt1Values)
LehmerInt2Fn <- ecdf(LehmerInt2Values)
LehmerInt2Dist <- LehmerInt2Fn(LehmerInt2Values)
LehmerReal1Fn <- ecdf(LehmerReal1Values)
LehmerReal1Dist <- LehmerReal1Fn(LehmerReal1Values)
LehmerReal2Fn <- ecdf(LehmerReal2Values)
LehmerReal2Dist <- LehmerReal2Fn(LehmerReal2Values)

MSRandFn <- ecdf(MSRandValues)
MSRandDist <- MSRandFn(MSRandValues)

ParkRandFn <- ecdf(ParkRandValues)
ParkRandDist <- ParkRandFn(ParkRandValues)
RoundaboutRandFn <- ecdf(RoundaboutRandValues)
RoundaboutRandDist <- RoundaboutRandFn(RoundaboutRandValues)
SeaRandFn <- ecdf(SeaRandValues)
SeaRandDist <- SeaRandFn(SeaRandValues)

RandDist <- c(CRand1Dist,CRand2Dist,CRand3Dist,PRand1Dist,PRand2Dist,PRand3Dist,PRand4Dist,PRand5Dist,JSRandDist,RndRandDist,LehmerInt1Dist,LehmerInt2Dist,
              LehmerReal1Dist,LehmerReal2Dist,MSRandDist,ParkRandDist,RoundaboutRandDist,SeaRandDist)

RandOutput <- c(CRand1Values/100,CRand2Values/100,CRand3Values/100,PRand1Values/100,PRand2Values,PRand3Values,PRand4Values/100,PRand5Values/100,
                JSRandDist/100,RndRandDist/100,LehmerInt1Dist,LehmerInt2Dist,
                LehmerReal1Dist,LehmerReal2Dist,MSRandDist/100,ParkRandDist/100,RoundaboutRandDist/100,SeaRandDist/100)

RandGroup <- c()
for (x in 1:500) {
  RandGroup <- append(RandGroup,'C# Rand')
}
for (x in 1:500) {
  RandGroup <- append(RandGroup,'C# Seeded Rand')
}
for (x in 1:500) {
  RandGroup <- append(RandGroup,'C# Cryptographic Rand')
}
for (x in 1:500) {
  RandGroup <- append(RandGroup,'Python Randint')
}
for (x in 1:500) {
  RandGroup <- append(RandGroup,'Python Random')
}
for (x in 1:500) {
  RandGroup <- append(RandGroup,'Python Seeded Random')
}
for (x in 1:500) {
  RandGroup <- append(RandGroup,'Numpy Randint')
}
for (x in 1:500) {
  RandGroup <- append(RandGroup,'Numpy Seeded Randint')
}
for (x in 1:500) {
  RandGroup <- append(RandGroup,'JavaScript Rand')
}
for (x in 1:500) {
  RandGroup <- append(RandGroup,'Rand.Org Data')
}
for (x in 1:500) {
  RandGroup <- append(RandGroup,'Lehmer Int 1')
}
for (x in 1:500) {
  RandGroup <- append(RandGroup,'Lehmer Int 2')
}
for (x in 1:500) {
  RandGroup <- append(RandGroup,'Lehmer Real 1')
}
for (x in 1:500) {
  RandGroup <- append(RandGroup,'Lehmer Real 2')
}
for (x in 1:500) {
  RandGroup <- append(RandGroup,'Middle Square Data')
}
for (x in 1:500) {
  RandGroup <- append(RandGroup,'Park White Noise')
}
for (x in 1:500) {
  RandGroup <- append(RandGroup,'Roundabout White Noise')
}
for (x in 1:500) {
  RandGroup <- append(RandGroup,'Sea White Noise')
}

Rand_DF <- data.frame(RandDist, RandGroup, RandOutput)
RandGroup_DF <- Rand_DF %>% group_by(RandGroup)

ggplot(RandGroup_DF, aes(x = RandOutput, y = RandDist, colour = RandGroup)) + 
  geom_point() + scale_x_continuous(limits=c(0, 1))+ labs(x = "Random Numbers Generated", y = "Distribution", color = "Data Sources", title = "A Scatterplot Showing Distribution of Collected Rand Data")

#4a) Serial Test of C# Rand Data
#The Serial Test is used to test the equidistribution of pairs of values
#While the KS Test focused on the distribution of the dataset as a whole, the Serial Test uses value d to compare sets of values
#As with the KS Test, the Serial Test requires data between 0 and 1, so division is used on some datasets to ensure they conform to the test requirements
print(serial.test(CRand1Values/100, d=2))
#CRand1 scored a chi-squared stat of 1.2 and a p-value of 0.76
#The observed counts achieved were 62, 69, 62, 57
print(serial.test(CRand2Values/100, d=2))
#CRand2 scored a chi-squared stat of 1.5 and a p-value of 0.69
#The observed counts achieved were 55, 68, 62, 65
print(serial.test(CRand3Values/100, d=2))
#CRand3 scored a chi-squared stat of 139 and a p-value of 7.1e-30
#The observed counts achieved were 11, 11, 20, 23

#4b) Serial Test of Python Rand Data
print(serial.test(PRand1Values/100, d=2))
#PRand1 scored a chi-squared stat of 5.4 and a p-value of 0.14
#The observed counts achieved were 55, 54, 77, 62
print(serial.test(PRand2Values, d=2))
#PRand2 scored a chi-squared stat of 3.9 and a p-value of 0.27
#The observed counts achieved were 57, 58, 76, 59
print(serial.test(PRand3Values, d=2))
#PRand3 scored a chi-squared stat of 9 and a p-value of 0.03
#The observed counts achieved were 60, 48, 61, 81
print(serial.test(PRand4Values/100, d=2))
#PRand4 scored a chi-squared stat of 2.8 and a p-value of 0.43
#The observed counts achieved were 55, 62, 73, 60
print(serial.test(PRand5Values/100, d=2))
#PRand5 scored a chi-squared stat of 1.4 and a p-value of 0.7
#The observed counts achieved were 63, 68, 55, 64

#4c) Serial Test of JavaScript Rand Data
print(serial.test(JSRandValues/100, d=2))
#JSRand scored a chi-squared stat of 5 and a p-value of 0.17
#The observed counts achieved were 70, 47, 61, 66

#4d) Serial Test of Rand.Org Data
print(serial.test(RndRandValues/100, d=2))
#RndRand scored a chi-squared stat of 3.4 and a p-value of 0.33
#The observed counts achieved were 57, 67, 71, 53

#4e) Serial Test of Lehmer Generator Rand Data
print(serial.test(LehmerInt1Values, d=2))
#LehmerInt1 scored a chi-sqaured stat of 87 and a p-value of 0.1e-19
#The observed counts achieved were 46, 35, 18, 13
print(serial.test(LehmerInt2Values, d=2))
#LehmerInt2 scored a chi-squared stat of 223 and a p-value of 4.9e-48
#The observed counts achieved were 3, 3, 5, 3
print(serial.test(LehmerReal1Values, d=2))
#LehmerReal1 scored a chi-sqaured stat of 3.9 and a p-value of 0.28
#The observed counts achieved were 62, 75, 59, 54
print(serial.test(LehmerReal2Values, d=2))
#LehmerReal2 scored a chi-squared stat of 0.34 and a p-value of 0.95
#The observed counts achieved were 60, 63, 61, 66

#4f) Serial Test of Middle Square Generator Data
print(serial.test(MSRandValues/100, d=2))
#MSRand scored a chi-squared stat of 726 and a p-value of 4.2e-157
#The observed counts achieved were 247, 0, 3, 0

#4g) Serial Test of White Noise Data
print(serial.test(ParkRandValues/100, d=2))
#ParkRand scored a chi-squared stat of 250 and a p-value of 6.5e-54
#The observed counts achieved were 0, 0, 0, 0
print(serial.test(RoundaboutRandValues/100, d=2))
#RoundaboutRand scored a chi-squared stat of 230 and a p-value of 1.1e-49
#The observed counts achieved were 3, 4, 2, 1
print(serial.test(SeaRandValues/100, d=2))
#SeaRand scored a chi-squared stat of 237 and a p-value of 5.2e-51
#The observed counts achieved were 0, 6, 0, 1

#4h) Visualisation
#Some p-values presented were considered to be anomalous either due to the nature or faults of the generators in question
#Therefore when visualising the data these generators were removed to allow for a more balanced overview
SerialPValue <- c(0.76,0.69,0.14,0.27,0.03,0.43,0.7,0.17,0.33,0.28,0.95)
SerialPGroup <- c("C# Rand","C# Seeded Rand","Python Randint","Python Random","Python Seeded Random","Numpy Randint","Numpy Seeded Randint",
                  "JavaScript Rand","Rand.Org Data","Lehmer Real 1","Lehmer Real 2")
SerialP_DF <- data.frame(SerialPValue, SerialPGroup)
SerialPGroup_DF <- SerialP_DF %>% group_by(SerialPGroup)

ggplot(SerialPGroup_DF, aes(x = reorder(SerialPGroup, -SerialPValue), y = SerialPValue, fill = SerialPGroup)) + 
  geom_bar(stat = "identity", width=0.5, position="dodge") + theme(axis.text.x = element_text(angle=90, vjust=.5, hjust=1),legend.position = "none") + 
  labs(x = "Data Sources", y = "P-Values", title = "A Bar Chart Showing P-Values of Collected Serial Test Data") + geom_text(aes(label = SerialPValue, vjust = 1.2))

#5a) Gap Test of C# Rand Data
#The Gap Test is designed to show the length of gaps between recurring values in the data
#The tests used count the observed frequency of gaps ranging in length from 1 to 10 which is then compared against the theoretical frequency of these gaps
print(gap.test(CRand1Values/100))
#CRand1 had a chi-squared stat of 11 and a p-value of 0.3
#The observed frequencies were 75, 37, 10, 7, 4, 2, 3, 0, 0, 0
print(gap.test(CRand2Values/100))
#CRand2 had a chi-squared stat of 14 and a p-value of 0.12
#The observed frequencies were 49, 39, 16, 12, 1, 0, 1, 0, 1, 0
print(gap.test(CRand3Values/100))
#CRand3 had a chi-sqaured stat of 32 and a p-value of 0.00023
#The observed frequencies were 58, 18, 3, 0, 0, 0, 0, 0, 0, 0

#5b) Gap Test of Python Rand Data
print(gap.test(PRand1Values/100))
#PRand1 had a chi-squared stat of 5.3 and a p-value of 0.8
#The observed frequencies were 61, 35, 15, 7, 7, 1, 0, 0, 0, 0
print(gap.test(PRand2Values))
#PRand2 had a chi-squared stat of 7.7 and a p-value of 0.56
#The observed frequencies were 67, 29, 21, 5, 2, 1, 1, 1, 1, 0
print(gap.test(PRand3Values))
#PRand3 had a chi-squared stat of 6.1 and a p-value of 0.73
#The observed frequencies were 55, 39, 16, 8, 2, 1, 0, 0, 0, 0
print(gap.test(PRand4Values/100))
#PRand4 had a chi-squared stat of 7.3 and a p-value of 0.6
#The observed frequencies were 64, 29, 19, 6, 2, 2, 1, 2, 0, 0
print(gap.test(PRand5Values/100))
#PRand5 had a chi-squared stat of 12 and a p-value of 0.22
#The observed frequencies were 67, 40, 8, 6, 4, 2, 1, 2, 0, 0

#5c) Gap Test of JavaScript Rand Data
print(gap.test(JSRandValues/100))
#JSRand had a chi-squared stat of 6.7 and a p-value of 0.67
#The observed frequencies were 49, 38, 17, 10, 3, 1, 1, 1, 0, 0

#5d) Gap Test of Rand.Org Data
print(gap.test(RndRandValues/100))
#Rand.Org had a chi-squared stat of 17 and a p-value of 0.042
#The observed frequencies were 51, 26, 19, 11, 1, 4, 2, 0, 0, 1

#5e) Gap Test of Lehmer Generator Rand Data
print(gap.test(LehmerInt1Values))
#LehmerInt1 had a chi-squared stat of 27 and a p-value of 0.0012
#The observed frequencies were 75, 18, 4, 3, 0, 0, 0, 0, 0, 0
print(gap.test(LehmerInt2Values))
#LehmerInt2 had a chi-squared stat of 79 and a p-value of 2.6e-13
#The observed frequencies were 13, 9, 2, 2, 0, 0, 0, 0, 0, 0
print(gap.test(LehmerReal1Values))
#LehmerReal1 had a chi-squared stat of 18 and a p-value of 0.036
#The observed frequencies were 60, 26, 16, 10, 4, 0, 2, 3, 0, 0
print(gap.test(LehmerReal2Values))
#LehmerReal2 had a chi-squared stat of 4.8 and a p-value of 0.85
#The observed frequencies were 58, 33, 20, 9, 1, 2, 1, 0, 0, 0

#5f) Gap Test of Middle Square Data
print(gap.test(MSRandValues/100))
#MSRand had a chi-squared stat of 1.6e+147 and a p-value of 0

#5g) Gap Test of White Noise Data
print(gap.test(ParkRandValues/100))
#ParkRand had a chi-squared stat of 120 and a p-value of 1.3e-21
#The observed frequencies were 0, 1, 1, 0, 0, 0, 1, 0, 0, 0
print(gap.test(RoundaboutRandValues/100))
#RoundaboutRand had a chi-squared stat of 109 and a p-value of 2e-19
#The observed frequencies were 5, 0, 1, 0, 2, 0, 1, 0, 0, 0
print(gap.test(SeaRandValues/100))
#SeaRand had a chi-squared stat of 121 and a p-value of 8.5e-22
#The observed frequencies were 1, 0, 1, 0, 0, 0, 0, 0, 0, 0

#5h) Visualisation
GapPValue <- c(0.3,0.12,0.00023,0.8,0.56,0.73,0.6,0.22,0.67,0.042,0.0012,0.036,0.85)
GapPGroup <- c("C# Rand","C# Seeded Rand","C# Cryptographic Rand","Python Randint","Python Random","Python Seeded Random","Numpy Randint","Numpy Seeded Randint",
                  "JavaScript Rand","Rand.Org Data","Lehmer Int 1","Lehmer Real 1","Lehmer Real 2")

GapP_DF <- data.frame(GapPValue, GapPGroup)
GapPGroup_DF <- GapP_DF %>% group_by(GapPGroup)

ggplot(GapPGroup_DF, aes(x = reorder(GapPGroup, -GapPValue), y = GapPValue, fill = GapPGroup)) + 
  geom_bar(stat = "identity", width=0.5, position="dodge") + theme(axis.text.x = element_text(angle=90, vjust=.5, hjust=1),legend.position = "none") + 
  labs(x = "Data Sources", y = "P-Values", title = "A Bar Chart Showing P-Values of Collected Gap Test Data") + geom_text(aes(label = GapPValue, vjust = 1.2))

#6a) Card Draw Distribution Visualisation
#Retrieving the C# card draw data from the C# output
setwd("D:/Github/PROJ518/C#_Rand_Function/RandFunctionOutput")
CCard1Values <- fromJSON(file = "C#CardSim1.json")
CCard2Values <- fromJSON(file = "C#CardSim2.json")

setwd("D:/Github/PROJ518/Python_Rand_Function/Python_Output")
PCard1Values <- fromJSON(file = "PythonCardSim1.json")
PCard2Values <- fromJSON(file = "PythonCardSim2.json")
PCard3Values <- fromJSON(file = "PythonCardSim3.json")

setwd("D:/Github/PROJ518/Card_Draws")
RealCard1Values <- fromJSON(file = "CardDraw1(Alex).json")
RealCard2Values <- fromJSON(file = "CardDraw2(Ollie).json")
RealCard3Values <- fromJSON(file = "CardDraw3(James).json")

#In string form, the values given aren't suitable for visualisation
#First the data can be sorted into the four suits (spades, hearts, clubs, diamonds)
CardSuitGroup <- c()
for (x in CCard1Values) {
  if (grepl("of Spades", x)) {
    CardSuitGroup <- append(CardSuitGroup, "Spade")
  }else if (grepl("of Hearts", x)){
    CardSuitGroup <- append(CardSuitGroup, "Heart")
  }else if (grepl("of Clubs", x)){
    CardSuitGroup <- append(CardSuitGroup, "Club")
  }else if (grepl("of Diamonds", x)){
    CardSuitGroup <- append(CardSuitGroup, "Diamond")
  }
}
for (x in CCard2Values) {
  if (grepl("of Spades", x)) {
    CardSuitGroup <- append(CardSuitGroup, "Spade")
  }else if (grepl("of Hearts", x)){
    CardSuitGroup <- append(CardSuitGroup, "Heart")
  }else if (grepl("of Clubs", x)){
    CardSuitGroup <- append(CardSuitGroup, "Club")
  }else if (grepl("of Diamonds", x)){
    CardSuitGroup <- append(CardSuitGroup, "Diamond")
  }
}
for (x in PCard1Values) {
  if (grepl("of Spades", x)) {
    CardSuitGroup <- append(CardSuitGroup, "Spade")
  }else if (grepl("of Hearts", x)){
    CardSuitGroup <- append(CardSuitGroup, "Heart")
  }else if (grepl("of Clubs", x)){
    CardSuitGroup <- append(CardSuitGroup, "Club")
  }else if (grepl("of Diamonds", x)){
    CardSuitGroup <- append(CardSuitGroup, "Diamond")
  }
}
for (x in PCard2Values) {
  if (grepl("of Spades", x)) {
    CardSuitGroup <- append(CardSuitGroup, "Spade")
  }else if (grepl("of Hearts", x)){
    CardSuitGroup <- append(CardSuitGroup, "Heart")
  }else if (grepl("of Clubs", x)){
    CardSuitGroup <- append(CardSuitGroup, "Club")
  }else if (grepl("of Diamonds", x)){
    CardSuitGroup <- append(CardSuitGroup, "Diamond")
  }
}
for (x in PCard3Values) {
  if (grepl("of Spades", x)) {
    CardSuitGroup <- append(CardSuitGroup, "Spade")
  }else if (grepl("of Hearts", x)){
    CardSuitGroup <- append(CardSuitGroup, "Heart")
  }else if (grepl("of Clubs", x)){
    CardSuitGroup <- append(CardSuitGroup, "Club")
  }else if (grepl("of Diamonds", x)){
    CardSuitGroup <- append(CardSuitGroup, "Diamond")
  }
}
for (x in RealCard1Values) {
  if (grepl("of Spades", x)) {
    CardSuitGroup <- append(CardSuitGroup, "Spade")
  }else if (grepl("of Hearts", x)){
    CardSuitGroup <- append(CardSuitGroup, "Heart")
  }else if (grepl("of Clubs", x)){
    CardSuitGroup <- append(CardSuitGroup, "Club")
  }else if (grepl("of Diamonds", x)){
    CardSuitGroup <- append(CardSuitGroup, "Diamond")
  }
}
for (x in RealCard2Values) {
  if (grepl("of Spades", x)) {
    CardSuitGroup <- append(CardSuitGroup, "Spade")
  }else if (grepl("of Hearts", x)){
    CardSuitGroup <- append(CardSuitGroup, "Heart")
  }else if (grepl("of Clubs", x)){
    CardSuitGroup <- append(CardSuitGroup, "Club")
  }else if (grepl("of Diamonds", x)){
    CardSuitGroup <- append(CardSuitGroup, "Diamond")
  }
}
for (x in RealCard3Values) {
  if (grepl("of Spades", x)) {
    CardSuitGroup <- append(CardSuitGroup, "Spade")
  }else if (grepl("of Hearts", x)){
    CardSuitGroup <- append(CardSuitGroup, "Heart")
  }else if (grepl("of Clubs", x)){
    CardSuitGroup <- append(CardSuitGroup, "Club")
  }else if (grepl("of Diamonds", x)){
    CardSuitGroup <- append(CardSuitGroup, "Diamond")
  }
}

CardShuffleGroup <- c()
CardPosition <- c()
for (x in 1:52) {
  CardShuffleGroup <- append(CardShuffleGroup,'C# Random')
  CardPosition <- append(CardPosition, x)
}
for (x in 1:52) {
  CardShuffleGroup <- append(CardShuffleGroup,'C# Seeded Random')
  CardPosition <- append(CardPosition, x)
}
for (x in 1:52) {
  CardShuffleGroup <- append(CardShuffleGroup,'Python Randint')
  CardPosition <- append(CardPosition, x)
}
for (x in 1:52) {
  CardShuffleGroup <- append(CardShuffleGroup,'Numpy Random')
  CardPosition <- append(CardPosition, x)
}
for (x in 1:52) {
  CardShuffleGroup <- append(CardShuffleGroup,'Numpy Seeded Random')
  CardPosition <- append(CardPosition, x)
}
for (x in 1:52) {
  CardShuffleGroup <- append(CardShuffleGroup,'Real Card Shuffle 1')
  CardPosition <- append(CardPosition, x)
}
for (x in 1:52) {
  CardShuffleGroup <- append(CardShuffleGroup,'Real Card Shuffle 2')
  CardPosition <- append(CardPosition, x)
}
for (x in 1:52) {
  CardShuffleGroup <- append(CardShuffleGroup,'Real Card Shuffle 3')
  CardPosition <- append(CardPosition, x)
}

CardSuit_DF <- data.frame(CardPosition, CardSuitGroup, CardShuffleGroup)
print(CardSuit_DF)

ggplot(CardSuit_DF, aes(x = CardShuffleGroup, y = CardPosition, colour = CardSuitGroup)) + 
  geom_point(size = 3) + labs(x = "Data Sources", y = "Card Position", 
                                                               color = "Suits", title = "A Scatterplot Showing the Distribution of Card Suits in Different Shuffle Methods")
#6b) The Poker Test of Card Draw Data
#The Poker Test is designed to evaluate the patterns in n groups of 5 successive values, replicating possible poker hands
#The decks used for this test consist only of a traditional 52 cards, divided evenly into the four suits
#The hands to be examined consist of either one pair, two pairs, three of a kind, full house (3 of a kind and a pair), four of a kind or a flush/five of a kind
#While in an actual game of poker other hands can be made that relate to the card value not just the suit for the sake of this test only the suit will be taken into account
#In addition as this test compares successive groups of 5 cards and each deck contains 52 cards, cards 51 and 52 will be noted and excluded from testing
#The suit of these cards will be taken into consideration and the probability of the respective suits will be adjusted accordingly
print(CCard1Values)
CCard1PT <- CCard1Values[-52]
CCard1PT <- CCard1PT[-51]
#CCard1 removed 6 of Clubs and King of Diamonds
print(CCard2Values)
CCard2PT <- CCard2Values[-52]
CCard2PT <- CCard2PT[-51]
#CCard2 removed 3 of Hearts and King of Diamonds

print(PCard1Values)
PCard1PT <- PCard1Values[-52]
PCard1PT <- PCard1PT[-51]
#PCard1 removed 10 of Hearts and 3 of Clubs
print(PCard2Values)
PCard2PT <- PCard2Values[-52]
PCard2PT <- PCard2PT[-51]
#PCard2 removed 4 of Clubs and King of Diamonds
print(PCard3Values)
PCard3PT <- PCard3Values[-52]
PCard3PT <- PCard3PT[-51]
#PCard3 removed 10 of Spades and King of Diamonds

print(RealCard1Values)
RealCard1PT <- RealCard1Values[-52]
RealCard1PT <- RealCard1PT[-51]
#RealCard1 removed King of Hearts and Queen of Hearts
print(RealCard2Values)
RealCard2PT <- RealCard2Values[-52]
RealCard2PT <- RealCard2PT[-51]
#RealCard2 removed 8 of Diamonds and 7 of Diamonds
print(RealCard3Values)
RealCard3PT <- RealCard3Values[-52]
RealCard3PT <- RealCard3PT[-51]
#RealCard3 removed 9 of Clubs and 3 of Hearts

#To better automate this process a function will be used that allows for the choice of deck to be input
PokerTest_function <- function(InputDeck) {
  for (x in 1:10){
    #The function will first empty the hand, then draw the first five cards from the input deck
    TempHand <- c()
    SpadeCount <- 0
    HeartCount <- 0
    ClubCount <- 0
    DiamondCount <- 0
    TempHand <- append(TempHand, InputDeck[1])
    TempHand <- append(TempHand, InputDeck[2])
    TempHand <- append(TempHand, InputDeck[3])
    TempHand <- append(TempHand, InputDeck[4])
    TempHand <- append(TempHand, InputDeck[5])
    #The function will then identify the suit of each card
    for (i in TempHand){
      if (grepl("of Spades", i)) {
        SpadeCount <- SpadeCount + 1
      }else if (grepl("of Hearts", i)){
        HeartCount <- HeartCount + 1
      }else if (grepl("of Clubs", i)){
        ClubCount <- ClubCount + 1
      }else if (grepl("of Diamonds", i)){
        DiamondCount <- DiamondCount + 1
      }
    }
    #The function will then print the results
    print(paste("Group", x))
    print(paste("Number of Spades:", SpadeCount))
    print(paste("Number of Hearts:", HeartCount))
    print(paste("Number of Clubs:", ClubCount))
    print(paste("Number of Diamonds:", DiamondCount))
    #Finally the function will remove the first five cards from the deck
    InputDeck <- InputDeck[-1]
    InputDeck <- InputDeck[-1]
    InputDeck <- InputDeck[-1]
    InputDeck <- InputDeck[-1]
    InputDeck <- InputDeck[-1]
  }
}
PokerTest_function(CCard1PT)
#The results of the PokerTest for CCard1:
#Pair(Diamonds), Two Pair(Spades,Clubs), 3 of a kind(Clubs), Two Pair(Hearts,Clubs), 3 of a kind(Spades)
#Two Pair(Spades,Hearts), Two Pair(Hearts,Diamonds), Two Pair(Spades,Clubs), Full House(Diamonds,Hearts), Two Pair(Hearts, Diamonds)
PokerTest_function(CCard2PT)
#The results of the PokerTest for CCard2:
#Pair(Spades), 3 of a kind(Hearts), 3 of a kind(Diamonds), 3 of a kind(Clubs), Two Pair(Clubs,Diamonds)
#Two Pair(Hearts,Clubs), Two Pair(Clubs,Diamonds), Two Pair(Spades,Hearts), Pair(Spades), Three of a kind(Spades)

PokerTest_function(PCard1PT)
#The results of the PokerTest for PCard1:
#3 of a kind(Clubs), Full House(Hearts,Spades), Two Pair(Spades,Diamonds), 3 of a kind(Clubs), Pair(Hearts)
#Pair(Hearts), Pair(Hearts), Two Pair(Spades,Diamonds), Full House(Spades,Clubs), 4 of a kind(Diamonds)
PokerTest_function(PCard2PT)
#The results of the PokerTest for PCard2:
#Two Pair(Spades,Diamonds), 3 of a kind(Hearts), 4 of a kind(Spades), 3 of a kind(Hearts), 3 of a kind(Spades)
#Two Pair(Hearts, Diamonds), 3 of a kind(Diamonds), Two Pair(Hearts,Clubs), Full House(Diamonds,Clubs), 3 of a kind(Clubs)
PokerTest_function(PCard3PT)
#The results of the PokerTest for PCard3:
#Two Pair(Clubs,Diamonds), Pair(Spades), Two Pair(Hearts,Diamonds), Two Pair(Spades,Hearts), Two Pair(Spades,Clubs)
#Two Pair(Spades,Diamonds), Full House(Diamonds,Clubs), Pair(Hearts), 4 of a kind(Clubs), 4 of a kind(Hearts)

PokerTest_function(RealCard1PT)
#The results of the PokerTest for RealCard1:
#4 of a kind(Hearts), Full House(Diamonds,Clubs), 3 of a kind(Diamonds), 5 of a kind(Clubs), Full House(Clubs,Diamonds)
#4 of a kind(Hearts), 4 of a kind(Spades), 4 of a kind(Spades), 3 of a kind(Spades), Pair(Diamonds)
PokerTest_function(RealCard2PT)
#The results of the PokerTest for RealCard2:
#Two Pair(Hearts,Diamonds), Two Pair(Spades, Hearts), Pair(Clubs), 4 of a kind(Diamonds), 3 of a kind(Spades)
#3 of a kind(Hearts), Full House(Diamonds,Spades), Pair(Hearts), Full House(Clubs,Spades), 3 of a kind(Clubs)
PokerTest_function(RealCard3PT)
#The results of the PokerTest for RealCard3:
#Full House(Clubs,Spades), 3 of a kind(Diamonds), 5 of a kind(Diamonds), 3 of a kind(Hearts), Two Pair(Spades,Clubs)
#3 of a kind(Clubs), 4 of a kind(Hearts), 3 of a kind(Spades), 3 of a kind(Diamonds), Two Pair(Spades,Clubs)

#Totals for CCard1: 1 Pair, 6 Two Pairs, 2 3 of a kind, 1 Full House, 0 4 of a kind, 0 five of a kind
#Totals for CCard2: 2 Pair, 4 Two Pairs, 4 3 of a kind, 0 Full House, 0 4 of a kind, 0 five of a kind
#Totals for PCard1: 3 Pair, 2 Two Pairs, 2 3 of a kind, 2 Full House, 1 4 of a kind, 0 five of a kind
#Totals for PCard2: 0 Pair, 3 Two Pairs, 5 3 of a kind, 1 Full House, 1 4 of a kind, 0 five of a kind
#Totals for PCard3: 1 Pair, 5 Two Pairs, 0 3 of a kind, 1 Full House, 2 4 of a kind, 0 five of a kind
#Totals for RealCard1: 1 Pair, 0 Two Pairs, 2 3 of a kind, 2 Full House, 4 4 of a kind, 1 five of a kind
#Totals for RealCard2: 2 Pair, 2 Two Pairs, 3 3 of a kind, 2 Full House, 1 4 of a kind, 0 five of a kind
#Totals for RealCard3: 0 Pair, 2 Two Pairs, 5 3 of a kind, 1 Full House, 1 4 of a kind, 1 five of a kind

#6c) Visualisation
CardOutcomes <-c(1,6,2,1,0,0,
                 2,4,4,0,0,0,
                 3,2,2,2,1,0,
                 0,3,5,1,1,0,
                 1,5,0,1,2,0,
                 1,0,2,2,4,1,
                 2,2,3,2,1,0,
                 0,2,5,1,1,1)
HandOutcomes <-c("Pair","Two Pair","Three of a Kind","Full House","Four of a Kind","Five of a Kind",
                 "Pair","Two Pair","Three of a Kind","Full House","Four of a Kind","Five of a Kind",
                 "Pair","Two Pair","Three of a Kind","Full House","Four of a Kind","Five of a Kind",
                 "Pair","Two Pair","Three of a Kind","Full House","Four of a Kind","Five of a Kind",
                 "Pair","Two Pair","Three of a Kind","Full House","Four of a Kind","Five of a Kind",
                 "Pair","Two Pair","Three of a Kind","Full House","Four of a Kind","Five of a Kind",
                 "Pair","Two Pair","Three of a Kind","Full House","Four of a Kind","Five of a Kind",
                 "Pair","Two Pair","Three of a Kind","Full House","Four of a Kind","Five of a Kind")
CardSources <-c("C# Random","C# Random","C# Random","C# Random","C# Random","C# Random",
                "C# Seeded Random","C# Seeded Random","C# Seeded Random","C# Seeded Random","C# Seeded Random","C# Seeded Random",
                "Python Randint","Python Randint","Python Randint","Python Randint","Python Randint","Python Randint",
                "Numpy Random","Numpy Random","Numpy Random","Numpy Random","Numpy Random","Numpy Random",
                "Numpy Seeded Random","Numpy Seeded Random","Numpy Seeded Random","Numpy Seeded Random","Numpy Seeded Random","Numpy Seeded Random",
                "Real Card Shuffle 1","Real Card Shuffle 1","Real Card Shuffle 1","Real Card Shuffle 1","Real Card Shuffle 1","Real Card Shuffle 1",
                "Real Card Shuffle 2","Real Card Shuffle 2","Real Card Shuffle 2","Real Card Shuffle 2","Real Card Shuffle 2","Real Card Shuffle 2",
                "Real Card Shuffle 3","Real Card Shuffle 3","Real Card Shuffle 3","Real Card Shuffle 3","Real Card Shuffle 3","Real Card Shuffle 3")

PokerTest_DF <- data.frame(CardOutcomes, HandOutcomes, CardSources)
print(PokerTest_DF)

ggplot(PokerTest_DF, aes(x = CardSources, y = CardOutcomes, fill = factor(HandOutcomes, levels = c("Pair", "Two Pair", "Three of a Kind", "Full House", "Four of a Kind", "Five of a Kind")))) + 
  geom_bar(stat = "identity", width=0.8, position="dodge") +
  scale_fill_discrete(name="Possible Card Outcomes",breaks=c("Pair", "Two Pair", "Three of a Kind", "Full House", "Four of a Kind", "Five of a Kind"),
                      labels=c("Pair", "Two Pair","Three of a Kind","Full House", "Four of a Kind","Five of a Kind")) + 
  theme(axis.text.x = element_text(angle=90, vjust=.5, hjust=1)) +
  labs(x = "Data Sources", y = "Frequency", title = "A Barchart Showing the Frequency of Poker Hand Outcomes")

#6d) Poker Hand Distribution Simulation
#The Poker Test performed above focused on comparing sequential groups of hands
#In order to compare to a more accurate real world environment, this simulation aims to determine the probability of hand draws based on standard poker rules
#This includes the dealing of cards to multiple players and a greater range of outcomes relating to the cards held in each players hand
#To do this a function will be used that allows for the selection of a shuffled deck
PokerDealer_Fn <- function(InputDeck){
  #Once a deck is selected the 'Dealer' will distribute cards between 6 players
  #As with the rules of poker, the 'Dealer' will deal 1 card to each 'Player' in turn and repeat until each player has a five card hand
  #A 6 player game was chosen as this allows for 30 of the 52 cards in the deck to see play and 5-6 players make for the ideal game of poker
  Player1Hand <- c()
  Player2Hand <- c()
  Player3Hand <- c()
  Player4Hand <- c()
  Player5Hand <- c()
  Player6Hand <- c()
  
  for (x in 1:5){
    Player1Hand <- append(Player1Hand, InputDeck[1])
    Player2Hand <- append(Player2Hand, InputDeck[2])
    Player3Hand <- append(Player3Hand, InputDeck[3])
    Player4Hand <- append(Player4Hand, InputDeck[4])
    Player5Hand <- append(Player5Hand, InputDeck[5])
    Player6Hand <- append(Player6Hand, InputDeck[6])
    
    for (i in 1:6){
      InputDeck <- InputDeck[-1]
    }
  }
  
  print("Player Hands:")
  print("Player 1:")
  print(Player1Hand)
  print("Player 2:")
  print(Player2Hand)
  print("Player 3:")
  print(Player3Hand)
  print("Player 4:")
  print(Player4Hand)
  print("Player 5:")
  print(Player5Hand)
  print("Player 6:")
  print(Player6Hand)
  
}

PokerDealer_Fn(CCard1Values)
#Player 1 has a Pair
#Player 2 has a High Card
#Player 3 has a Pair
#Player 4 has a Pair
#Player 5 has a High Card
#Player 6 has a Pair
PokerDealer_Fn(CCard2Values)
#Player 1 has Three of a Kind
#Player 2 has High Card
#Player 3 has High Card
#Player 4 has a Pair
#Player 5 has High Card
#Player 6 has a Pair

PokerDealer_Fn(PCard1Values)
#Player 1 has High Card
#Player 2 has a Pair
#Player 3 has High Card
#Player 4 has a Pair
#Player 5 has High Card
#Player 6 has a Pair
PokerDealer_Fn(PCard2Values)
#Player 1 has High Card
#Player 2 has High Card
#Player 3 has High Card
#Player 4 has High Card
#Player 5 has a Pair
#Player 6 has High Card
PokerDealer_Fn(PCard3Values)
#Player 1 has a Pair
#Player 2 has a Pair
#Player 3 has a Pair
#Player 4 has a Pair
#Player 5 has High Card
#Player 6 has High Card

PokerDealer_Fn(RealCard1Values)
#Player 1 has a Pair
#Player 2 has High Card
#Player 3 has High Card
#Player 4 has a Pair
#Player 5 has Two Pair
#Player 6 has a Pair
PokerDealer_Fn(RealCard2Values)
#Player 1 has a Pair
#Player 2 has Two Pair
#Player 3 has a Pair
#Player 4 has a Pair
#Player 5 has High Card
#Player 6 has High Card
PokerDealer_Fn(RealCard3Values)
#Player 1 has High Card
#Player 2 has High Card
#Player 3 has a Pair
#Player 4 has a Pair
#Player 5 has High Card
#Player 6 has High Card

#Visualisation
CardDealValues <- c(2,4,0,0,3,2,0,1,3,3,0,0,5,1,0,0,2,4,0,0,2,3,1,0,2,3,1,0,4,2,0,0)
CardDealOutcomes <- c("High Card","Pair","Two Pair","Three of a Kind",
                      "High Card","Pair","Two Pair","Three of a Kind",
                      "High Card","Pair","Two Pair","Three of a Kind",
                      "High Card","Pair","Two Pair","Three of a Kind",
                      "High Card","Pair","Two Pair","Three of a Kind",
                      "High Card","Pair","Two Pair","Three of a Kind",
                      "High Card","Pair","Two Pair","Three of a Kind",
                      "High Card","Pair","Two Pair","Three of a Kind")
CardDealSources <- c("C# Random","C# Random","C# Random","C# Random",
                     "C# Seeded Random","C# Seeded Random","C# Seeded Random","C# Seeded Random",
                     "Python Randint","Python Randint","Python Randint","Python Randint",
                     "Numpy Random","Numpy Random","Numpy Random","Numpy Random",
                     "Numpy Seeded Random","Numpy Seeded Random","Numpy Seeded Random","Numpy Seeded Random",
                     "Real Card Shuffle 1","Real Card Shuffle 1","Real Card Shuffle 1","Real Card Shuffle 1",
                     "Real Card Shuffle 2","Real Card Shuffle 2","Real Card Shuffle 2","Real Card Shuffle 2",
                     "Real Card Shuffle 3","Real Card Shuffle 3","Real Card Shuffle 3","Real Card Shuffle 3")

PokerDeal_DF <- data.frame(CardDealValues, CardDealOutcomes, CardDealSources)
print(PokerDeal_DF)

ggplot(PokerDeal_DF, aes(x = CardDealSources, y = CardDealValues, fill = factor(CardDealOutcomes, levels = c("High Card","Pair", "Two Pair", "Three of a Kind")))) + 
  geom_bar(stat = "identity", width=0.8, position="dodge") +
  scale_fill_discrete(name="Possible Card Outcomes",breaks=c("High Card","Pair", "Two Pair", "Three of a Kind"),
                      labels=c("High Card","Pair", "Two Pair","Three of a Kind")) + 
  theme(axis.text.x = element_text(angle=90, vjust=.5, hjust=1)) +
  labs(x = "Data Sources", y = "Frequency", title = "A Barchart Showing the Frequency of Poker Hand Outcomes after non-sequential distribution")

#6e) 'Texas Hold Em' Style Card Distribution
#Similarly to the above simulation, this simulation aims to evaluate Poker hand outcomes when using a non-sequential distribution method
#In Texas Hold Em each player is given a two card hand and five cards form a communal river
#The flop (the starting 3 communal cards) are dealt after each players hand, then the remaining two cards (the turn and finally the river) are dealt after rounds of betting
#As this is only a distribution simulation and no other cards can be dealt, all 5 communal cards will be dealt at once
#Hands are decided using a combination of the player's hand and the river although hands can still only be five cards
TexasDealer_Fn <- function(InputDeck){
  Player1Hand <- c()
  Player2Hand <- c()
  Player3Hand <- c()
  Player4Hand <- c()
  Player5Hand <- c()
  Player6Hand <- c()
  TheRiver <- c()
  
  for (x in 1:2){
    Player1Hand <- append(Player1Hand, InputDeck[1])
    Player2Hand <- append(Player2Hand, InputDeck[2])
    Player3Hand <- append(Player3Hand, InputDeck[3])
    Player4Hand <- append(Player4Hand, InputDeck[4])
    Player5Hand <- append(Player5Hand, InputDeck[5])
    Player6Hand <- append(Player6Hand, InputDeck[6])
    
    for (i in 1:6){
      InputDeck <- InputDeck[-1]
    }
  }
  
  for (x in 1:5){
    TheRiver <- append(TheRiver, InputDeck[1])
    InputDeck <- InputDeck[-1]
  }
  
  print("Player Hands:")
  print("Player 1:")
  print(Player1Hand)
  print("Player 2:")
  print(Player2Hand)
  print("Player 3:")
  print(Player3Hand)
  print("Player 4:")
  print(Player4Hand)
  print("Player 5:")
  print(Player5Hand)
  print("Player 6:")
  print(Player6Hand)
  print("The River")
  print(TheRiver)
}
TexasDealer_Fn(CCard1Values)
#Player 1 has a Pair
#Player 2 has a Pair
#Player 3 has Two Pair
#Player 4 has a Pair
#Player 5 has Three of a Kind
#Player 6 has a Pair
TexasDealer_Fn(CCard2Values)
#Player 1 has a Pair
#Player 2 has High Card
#Player 3 has High Card
#Player 4 has Two Pair
#Player 5 has High Card
#Player 6 has a Pair

TexasDealer_Fn(PCard1Values)
#Player 1 has a Pair
#Player 2 has a Pair
#Player 3 has a Pair
#Player 4 has Two Pair
#Player 5 has Three of a Kind
#Player 6 has Two Pair
TexasDealer_Fn(PCard2Values)
#Player 1 has High Card
#Player 2 has High Card
#Player 3 has Two Pair
#Player 4 has a Pair
#Player 5 has a Flush
#Player 6 has a Pair
TexasDealer_Fn(PCard3Values)
#Player 1 has a Pair
#Player 2 has a Pair
#Player 3 has High Card
#Player 4 has a Pair
#Player 5 has High Card
#Player 6 has a Pair

TexasDealer_Fn(RealCard1Values)
#Player 1 has Two Pair
#Player 2 has High Card
#Player 3 has High Card
#Player 4 has High Card
#Player 5 has a Pair
#Player 6 has High Card
TexasDealer_Fn(RealCard2Values)
#Player 1 has Two Pair
#Player 2 has High Card
#Player 3 has a Pair
#Player 4 has High Card
#Player 5 has a Pair
#Player 6 has a Pair
TexasDealer_Fn(RealCard3Values)
#Player 1 has a Pair
#Player 2 has a flush
#Player 3 has a flush
#Player 4 has a flush
#Player 5 has a flush
#Player 6 has a flush

#Visualisation
TexasDealValues <- c(0,4,1,1,0,3,2,1,0,0,0,4,2,1,0,2,2,1,0,1,2,4,0,0,0,4,1,1,0,0,2,3,1,0,0,0,1,0,0,5)
TexasDealOutcomes <- c("High Card","Pair","Two Pair","Three of a Kind","Flush",
                      "High Card","Pair","Two Pair","Three of a Kind","Flush",
                      "High Card","Pair","Two Pair","Three of a Kind","Flush",
                      "High Card","Pair","Two Pair","Three of a Kind","Flush",
                      "High Card","Pair","Two Pair","Three of a Kind","Flush",
                      "High Card","Pair","Two Pair","Three of a Kind","Flush",
                      "High Card","Pair","Two Pair","Three of a Kind","Flush",
                      "High Card","Pair","Two Pair","Three of a Kind","Flush")
TexasDealSources <- c("C# Random","C# Random","C# Random","C# Random","C# Random",
                     "C# Seeded Random","C# Seeded Random","C# Seeded Random","C# Seeded Random","C# Seeded Random",
                     "Python Randint","Python Randint","Python Randint","Python Randint", "Python Randint",
                     "Numpy Random","Numpy Random","Numpy Random","Numpy Random","Numpy Random",
                     "Numpy Seeded Random","Numpy Seeded Random","Numpy Seeded Random","Numpy Seeded Random","Numpy Seeded Random",
                     "Real Card Shuffle 1","Real Card Shuffle 1","Real Card Shuffle 1","Real Card Shuffle 1","Real Card Shuffle 1",
                     "Real Card Shuffle 2","Real Card Shuffle 2","Real Card Shuffle 2","Real Card Shuffle 2","Real Card Shuffle 2",
                     "Real Card Shuffle 3","Real Card Shuffle 3","Real Card Shuffle 3","Real Card Shuffle 3","Real Card Shuffle 3")
TexasDeal_DF <- data.frame(TexasDealValues, TexasDealOutcomes, TexasDealSources)
print(TexasDeal_DF)

ggplot(TexasDeal_DF, aes(x = TexasDealSources, y = TexasDealValues, fill = factor(TexasDealOutcomes, levels = c("High Card","Pair", "Two Pair", "Three of a Kind","Flush")))) + 
  geom_bar(stat = "identity", width=0.8, position="dodge") +
  scale_fill_discrete(name="Possible Card Outcomes",breaks=c("High Card","Pair", "Two Pair", "Three of a Kind","Flush"),
                      labels=c("High Card","Pair", "Two Pair","Three of a Kind","Flush")) + 
  theme(axis.text.x = element_text(angle=90, vjust=.5, hjust=1)) +
  labs(x = "Data Sources", y = "Frequency", title = "A Barchart Showing the Frequency of Poker Hand Outcomes after alternative non-sequential distribution")

#7a) Run Test of Rand Data
#The Runs Test examines sequences of random numbers for 'Runs Up' (Where the sequence increases) and 'Runs Down' (where the sequence decreases)
#The P-Value generated is compared against a 0.05 significance level, if greater then the sequence can be considered random, if less then the sequence can be considered non-random
#As this test can be used on any data set, no alterations need to be made to the data sequences used
print(runs.test(CRand1Values))
#CRand1 had a test statistic of 1.7118 and a p-value of 0.08693
print(runs.test(CRand2Values))
#CRand2 had a test statistic of -1.2151 and a p-value of 0.2243
print(runs.test(CRand3Values))
#CRand3 had a test statistic of -0.98848 and a p-value of 0.3229

print(runs.test(PRand1Values))
#PRand1 had a test statistic of 0.36778 and a p-value of 0.713
print(runs.test(PRand2Values))
#PRand2 had a test statistic of 0.44766 and a p-value of 0.6544
print(runs.test(PRand3Values))
#PRand3 had a test statistic of -0.62673 and a p-value of 0.5308
print(runs.test(PRand4Values))
#PRand4 had a test statistic of 0.00036468 and a p-value of 0.9997
print(runs.test(PRand5Values))
#PRand5 had a test statistic of 0.89532 and a p-value of 0.3706

print(runs.test(JSRandValues))
#JSRand had a test statistic of -1.0326 and a p-value of 0.3018

print(runs.test(RndRandValues))
#RndRand had a test statistic of -1.9385 and a p-value of 0.05256

print(runs.test(LehmerInt1Values))
#LehmerInt1 had a test statistic of 0.2686 and a p-value of 0.7882
print(runs.test(LehmerInt2Values))
#LehmerInt2 had a test statistic of 1.9697 and a p-value of 0.04887
print(runs.test(LehmerReal1Values))
#LehmerReal1 had a test statistic of -0.35813 and a p-value of 0.7202
print(runs.test(LehmerReal2Values))
#LehmerReal2 had a test statistic of -0.44766 and a p-value of 0.6544

print(runs.test(MSRandValues))
#MSRand had a test statistic of 8.8831 and a p-value of 2.2e-16

print(runs.test(ParkRandValues))
#ParkRand had a test statistic of -20.951 and a p-value of 2.2e-16
print(runs.test(RoundaboutRandValues))
#RoundaboutRand had a test statistic of -20.454 and a p-value of 2.2e-16
print(runs.test(SeaRandValues))
#SeaRand had a test statistic of -21.531 and a p-value of 2.2e-16

#7b) Visualisation
RunsPValue <- c(0.08693, 0.2243, 0.3229, 0.713, 0.6544, 0.5308, 0.9997, 0.3706, 0.3018, 0.05256, 0.7882, 0.04887, 0.7202, 0.6544)
RunsPGroup <- c("C# Rand","C# Seeded Rand","C# Cryptographic Rand","Python Randint","Python Random","Python Seeded Random","Numpy Randint","Numpy Seeded Randint",
               "JavaScript Rand","Rand.Org Data","Lehmer Int 1","Lehmer Int 2","Lehmer Real 1","Lehmer Real 2")
RunsP_DF <- data.frame(RunsPValue, RunsPGroup)

ggplot(RunsP_DF, aes(x = reorder(RunsPGroup, -RunsPValue), y = RunsPValue, fill = RunsPGroup)) + 
  geom_bar(stat = "identity", width=0.5, position="dodge") + theme(axis.text.x = element_text(angle=90, vjust=.5, hjust=1),legend.position = "none") + 
  labs(x = "Data Sources", y = "P-Values", title = "A Bar Chart Showing P-Values of Collected Runs Test Data") + geom_text(aes(label = RunsPValue, vjust = -0.5))

#8a) Serial Correlation Test of Rand Data
#The Serial Correlation Test evaluates the serial correlation coefficient of a data set which is a measure of dependancy
#Test Statistic values closer to 0 show that the data is relatively independent of each other
serialCorrelationTest(CRand1Values)
#CRand1 has a Test Statistic of 1.6 and a p-value of 0.11
serialCorrelationTest(CRand2Values)
#Crand2 has a Test Statistic of -0.84 and a p-value of 0.4
serialCorrelationTest(CRand3Values)
#CRand3 has a Test Statistic of -0.96 and a p-value of 0.34

serialCorrelationTest(PRand1Values)
#PRand1 has a Test Statistic of 0.68 and a p-value of 0.5
serialCorrelationTest(PRand2Values)
#PRand2 has a Test Statistic of -0.082 and a p-value of 0.93
serialCorrelationTest(PRand3Values)
#PRand3 has a Test Statistic of -0.43 and a p-value of 0.67
serialCorrelationTest(PRand4Values)
#PRand4 has a Test Statistic of 1.2 and a p-value of 0.24
serialCorrelationTest(PRand5Values)
#PRand5 has a Test Statistic of 1.8 and a p-value of 0.079

serialCorrelationTest(JSRandValues)
#JSRand has a Test Statistic of -0.7 and a p-value of 0.49

serialCorrelationTest(RndRandValues)
#RndRand has a Test Statistic of -2.9 and a p-value of 0.0039

serialCorrelationTest(LehmerInt1Values)
#LehmerInt1 has a Test Statistic of 0.6 and a p-value of 0.55
serialCorrelationTest(LehmerInt2Values)
#LehmerInt2 has a Test Statistic of -0.039 and a p-value of 0.97
serialCorrelationTest(LehmerReal1Values)
#LehmerReal1 has a Test Statistic of -0.14 and a p-value of 0.89
serialCorrelationTest(LehmerReal2Values)
#LehmerReal2 has a Test Statistic of 0.29 and a p-value of 0.78

serialCorrelationTest(MSRandValues)
#MSRand has a Test Statistic of 2.2 and a p-value of 0.025

serialCorrelationTest(ParkRandValues)
#ParkRand has a Test Statistic of -22 and a p-value of 0
serialCorrelationTest(RoundaboutRandValues)
#RoundaboutRand has a Test Statistic of -22 and a p-value of 0
serialCorrelationTest(SeaRandValues)
#SeaRand has a Test Statistic of -22 and a p-value of 0

#8b) Visualisation
SerialTValue <- c(1.6,-0.84,-0.96,0.68,-0.082,-0.43,1.2,1.8,-0.7,-2.9,0.6,-0.039,-0.14,0.29,2.2)
SerialTPValue <- c(0.11,0.4,0.34,0.5,0.93,0.67,0.24,0.079,0.49,0.0039,0.55,0.97,0.89,0.78,0.025)
SerialTGroup <- c("C# Rand","C# Seeded Rand","C# Cryptographic Rand","Python Randint","Python Random","Python Seeded Random","Numpy Randint","Numpy Seeded Randint",
                "JavaScript Rand","Rand.Org Data","Lehmer Int 1","Lehmer Int 2","Lehmer Real 1","Lehmer Real 2", "Middle Square Method")
SerialT_DF <- data.frame(SerialTValue, SerialTPValue, SerialTGroup)

ggplot(SerialT_DF, aes(x = SerialTValue, y = SerialTPValue, colour = SerialTGroup, label=SerialTGroup)) + 
  geom_point(size = 3) +geom_text(hjust=0.4, vjust=1.3) + labs(x = "Test Statistic", y = "P-Value", 
                              title = "A Scatterplot Showing the Test Statistics and P-Values from Serial Correlation Testing") + theme(legend.position = "none")

#9a) Birthday Spacings Test of Rand Data
#The Birthday Spacings Test works by choosing m birthdays from a year composed of n days then looking at the number of repetitions of spacing values 
#The test evaluates the distribution of a data set and whether it fits a normal distribution
#The Birthday Spacings Test provides results for KS distribution, Anderson-Darling distribution and Chi-Square distribution
#The output to be represented is the Anderson-Darling Test Statistic and Result
#A Result of 1 is considers the sequence to be a normal distribution, while a result of 0 is considered non-normal distribution
#As the sequences being used are designed to replicate randomness, non-normal distribution is expected here
B <- 16
m <- 32
n <- 2^B
lambda <- (m^3)/(4*n)
alpha <- 0.05
print(birthday.spacings(CRand1Values,m,n,alpha,lambda,num.class = 100))
#CRand1 had a Test Statistic of 15.172 and an AD result of 0
print(birthday.spacings(CRand2Values,m,n,alpha,lambda,num.class = 100))
#CRand2 had a Test Statistic of 15.013 and an AD result of 0
print(birthday.spacings(CRand3Values,m,n,alpha,lambda,num.class = 100))
#CRand3 had a Test Statistic of 16.221 and an AD result of 0

print(birthday.spacings(PRand1Values,m,n,alpha,lambda,num.class = 100))
#PRand1 had a Test Statistic of 14.739 and an AD result of 0
print(birthday.spacings(PRand2Values,m,n,alpha,lambda,num.class = 100))
#PRand2 had a Test Statistic of 2 and an AD result of 1
print(birthday.spacings(PRand3Values,m,n,alpha,lambda,num.class = 100))
#PRand3 had a Test Statistic of 2 and an AD result of 1
print(birthday.spacings(PRand4Values,m,n,alpha,lambda,num.class = 100))
#PRand4 had a Test Statistic of 15.504 and an AD result of 0
print(birthday.spacings(PRand5Values,m,n,alpha,lambda,num.class = 100))
#PRand5 had a Test Statistic of 15.221 and an AD result of 0

print(birthday.spacings(JSRandValues,m,n,alpha,lambda,num.class = 100))
#JSRand had a Test Statistic of 14.583 and an AD result of 0

print(birthday.spacings(RndRandValues,m,n,alpha,lambda,num.class = 100))
#RndRand had a Test Statistic of 14.837 and an AD result of 0

print(birthday.spacings(LehmerInt1Values,m,n,alpha,lambda,num.class = 100))
#LehmerInt1 had a Test Statistic of 2 and an AD result of 1
print(birthday.spacings(LehmerInt2Values,m,n,alpha,lambda,num.class = 100))
#LehmerInt2 had a Test Statistic of 2 and an AD result of 1
print(birthday.spacings(LehmerReal1Values,m,n,alpha,lambda,num.class = 100))
#LehmerReal1 had a Test Statistic of 2 and an AD result of 1
print(birthday.spacings(LehmerReal2Values,m,n,alpha,lambda,num.class = 100))
#LehmerReal2 had a Test Statistic of 2 and an AD result of 1

print(birthday.spacings(MSRandValues,m,n,alpha,lambda,num.class = 100))
#MSRand had a Test Statistic of 12.816 and an AD result of 0

print(birthday.spacings(ParkRandValues,m,n,alpha,lambda,num.class = 100))
#ParkRand had a Test Statistic of 16.813 and an AD result of 0
print(birthday.spacings(RoundaboutRandValues,m,n,alpha,lambda,num.class = 100))
#RoundaboutRand had a Test Statistic of 16.73 and an AD result of 0
print(birthday.spacings(SeaRandValues,m,n,alpha,lambda,num.class = 100))
#SeaRand had a Test Statistic of 16.858 and an AD result of 0

#9b) Visualisation
BirthdayStatValue <- c(15.172,15.013,16.221,14.739,2,2,15.504,15.221,14.583,14.837,2,2,2,2,12.816,16.813,16.73,16.858)
BirthdayResultValue <- c(0,0,0,0,1,1,0,0,0,0,1,1,1,1,0,0,0,0)
BirthdayGroup <- c("C# Rand",
                  "C# Seeded Rand",
                  "C# Cryptographic Rand",
                  "Python Randint",
                  "Python Random",
                  "Python Seeded Random",
                  "Numpy Randint",
                  "Numpy Seeded Randint",
                  "JavaScript Rand",
                  "Rand.Org Data",
                  "Lehmer Int 1",
                  "Lehmer Int 2",
                  "Lehmer Real 1",
                  "Lehmer Real 2", 
                  "Middle Square Method",
                  "Park White Noise",
                  "Roundabout White Noise",
                  "Sea White Noise")
Birthday_DF <- data.frame(BirthdayStatValue, BirthdayResultValue, BirthdayGroup)

ggplot(Birthday_DF, aes(x = BirthdayResultValue, y = BirthdayStatValue, fill = factor(BirthdayGroup, levels = c("C# Rand", "C# Seeded Rand", "C# Cryptographic Rand", "Python Randint", "Python Random", "Python Seeded Random",
                                                                                                                "Numpy Randint","Numpy Seeded Randint","JavaScript Rand","Rand.Org Data","Lehmer Int 1","Lehmer Int 2",
                                                                                                                "Lehmer Real 1","Lehmer Real 2","Middle Square Method","Park White Noise","Roundabout White Noise","Sea White Noise")))) + 
  geom_bar(stat = "identity", width=0.8, position="dodge") +
  scale_fill_discrete(name="Data Sources",breaks=c("C# Rand", "C# Seeded Rand", "C# Cryptographic Rand", "Python Randint", "Python Random", "Python Seeded Random",
                                                   "Numpy Randint","Numpy Seeded Randint","JavaScript Rand","Rand.Org Data","Lehmer Int 1","Lehmer Int 2",
                                                   "Lehmer Real 1","Lehmer Real 2","Middle Square Method","Park White Noise","Roundabout White Noise","Sea White Noise"),
                      labels=c("C# Rand", "C# Seeded Rand", "C# Cryptographic Rand", "Python Randint", "Python Random", "Python Seeded Random",
                               "Numpy Randint","Numpy Seeded Randint","JavaScript Rand","Rand.Org Data","Lehmer Int 1","Lehmer Int 2",
                               "Lehmer Real 1","Lehmer Real 2","Middle Square Method","Park White Noise","Roundabout White Noise","Sea White Noise")) + 
  labs(x = "Anderson-Darling Test Result", y = "Anderson-Darling Test Statistic", title = "A Barchart Showing the Results of the Birthday Spacings Test")

#10) Visualisation of P-Values
RandDataPValues <- c()
RandDataPValues <- append(RandDataPValues, SerialPValue)
RandDataPValues <- append(RandDataPValues, GapPValue)
RandDataPValues <- append(RandDataPValues, RunsPValue)
RandDataPValues <- append(RandDataPValues, SerialTPValue)

RandDataPGroups <- c()
RandDataPGroups <- append(RandDataPGroups, SerialPGroup)
RandDataPGroups <- append(RandDataPGroups, GapPGroup)
RandDataPGroups <- append(RandDataPGroups, RunsPGroup)
RandDataPGroups <- append(RandDataPGroups, SerialTGroup)

RandDataPTests <- c("Serial Test","Serial Test","Serial Test","Serial Test","Serial Test","Serial Test","Serial Test","Serial Test","Serial Test","Serial Test","Serial Test",
                    "Gap Test","Gap Test","Gap Test","Gap Test","Gap Test","Gap Test","Gap Test","Gap Test","Gap Test","Gap Test","Gap Test","Gap Test","Gap Test",
                    "Runs Test","Runs Test","Runs Test","Runs Test","Runs Test","Runs Test","Runs Test","Runs Test","Runs Test","Runs Test","Runs Test","Runs Test","Runs Test","Runs Test",
                    "Serial Correlation Test","Serial Correlation Test","Serial Correlation Test","Serial Correlation Test","Serial Correlation Test","Serial Correlation Test","Serial Correlation Test",
                    "Serial Correlation Test","Serial Correlation Test","Serial Correlation Test","Serial Correlation Test","Serial Correlation Test","Serial Correlation Test","Serial Correlation Test", "Serial Correlation Test")

RandDataP_DF <- data.frame(RandDataPValues, RandDataPGroups, RandDataPTests)

ggplot(RandDataP_DF, aes(x = RandDataPTests, y = RandDataPValues, fill = factor(RandDataPGroups,levels = c("C# Rand", "C# Seeded Rand", "C# Cryptographic Rand", "Python Randint", "Python Random", "Python Seeded Random",
                                                                                                           "Numpy Randint","Numpy Seeded Randint","JavaScript Rand","Rand.Org Data","Lehmer Int 1","Lehmer Int 2",
                                                                                                           "Lehmer Real 1","Lehmer Real 2","Middle Square Method","Park White Noise","Roundabout White Noise","Sea White Noise"), ordered = TRUE))) + 
  geom_bar(stat = "identity", width=0.8, position="dodge") +
  scale_fill_discrete(name="Data Sources",breaks=c("C# Rand", "C# Seeded Rand", "C# Cryptographic Rand", "Python Randint", "Python Random", "Python Seeded Random",
                                                   "Numpy Randint","Numpy Seeded Randint","JavaScript Rand","Rand.Org Data","Lehmer Int 1","Lehmer Int 2",
                                                   "Lehmer Real 1","Lehmer Real 2","Middle Square Method","Park White Noise","Roundabout White Noise","Sea White Noise"),
                      labels=c("C# Rand", "C# Seeded Rand", "C# Cryptographic Rand", "Python Randint", "Python Random", "Python Seeded Random",
                               "Numpy Randint","Numpy Seeded Randint","JavaScript Rand","Rand.Org Data","Lehmer Int 1","Lehmer Int 2",
                               "Lehmer Real 1","Lehmer Real 2","Middle Square Method","Park White Noise","Roundabout White Noise","Sea White Noise")) + 
  labs(x = "Statistical Tests Performed", y = "P-Values", title = "A Barchart Showing all collected P-Values of the Rand Data Sources")
