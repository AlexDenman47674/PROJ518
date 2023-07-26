#Dependencies need to be installed before analysis can begin
#RDieharder contains the test suite that will be used and RJson allows for the reading of JSON files
install.packages("RDieHarder")
install.packages("rjson")

library(RDieHarder)
library(rjson)
library(dplyr)

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

V_RealDice <- ((RealDice[1, 2] - np)^2 / np) + ((RealDice[2, 2] - np)^2 / np) + ((RealDice[3, 2] - np)^2 / np) + ((RealDice[4, 2] - np)^2 / np) + ((RealDice[5, 2] - np)^2 / np) + ((RealDice[6, 2] - np)^2 / np)
print(V_RealDice)

#For the real dice rolls v = 6.28