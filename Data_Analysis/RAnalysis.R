#Dependencies need to be installed before analysis can begin
#RDieharder contains the test suite that will be used and RJson allows for the reading of JSON files
install.packages("RDieHarder")
install.packages("rjson")
install.packages("dgof")
install.packages("Rtools")
install.packages("randtoolbox")

library(rjson)
library(plyr)
library(dplyr)
library(ggplot2)
library(dgof)
library(randtoolbox)

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
#Due to the nature of the data provided for coin flips, dice rolls and card draws, the chi-squared test can be used to help determine randomness
#In these tests all three pseudorandom algorithms from C#, Python and JavaScript will be compared against the physical dice roll data
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
#LehmerReal2 scored a chi-sqaured stat of 0.34 and a p-value of 0.95
#The observed counts achieved were 60, 63, 61, 66

#4f) Serial Test of Middle Square Generator Data
print(serial.test(MSRandValues/100, d=2))

#4g) Serial Test of White Noise Data
print(serial.test(ParkRandValues/100, d=2))
print(serial.test(RoundaboutRandValues/100, d=2))
print(serial.test(SeaRandValues/100, d=2))
