#Dependencies need to be installed before analysis can begin
#RDieharder contains the test suite that will be used and RJson allows for the reading of JSON files
install.packages("RDieHarder")
install.packages("rjson")

library(RDieHarder)
library(rjson)

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
