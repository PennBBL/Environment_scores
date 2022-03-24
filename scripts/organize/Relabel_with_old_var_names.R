# Load in necessary data and packages 
library(tidyverse)
CensusVariableMap <- read_csv("~/Projects/Geocoding/Data/CensusVariableMap.csv")
LocalAddress <- read_csv("~/Projects/Geocoding/Data/LocalAddressDumpCensus.csv")
PermAddress <- read_csv("~/Projects/Geocoding/Data/PermAddressDumpCensus.csv")

CensusVariableMap <- CensusVariableMap %>% 
  distinct(Old.var,.keep_all = T) %>% 
  select(Old.var,New.Var) %>% 
  rename(New.var = New.Var)

LocalAddress_OldNames <- LocalAddress %>% 
  pivot_longer(cols = B01001_001:C24010_001,names_to = "Variable",values_to = "Value") %>% 
  left_join(CensusVariableMap,by = c("Variable" = "New.var")) %>% 
  select(-Variable) %>% 
  pivot_wider(names_from = Old.var,values_from = Value) 

PermAddress_OldNames <- PermAddress %>% 
  pivot_longer(cols = B01001_001:C24010_001,names_to = "Variable",values_to = "Value") %>% 
  left_join(CensusVariableMap,by = c("Variable" = "New.var")) %>% 
  select(-Variable) %>% 
  pivot_wider(names_from = Old.var,values_from = Value) 

write.csv(LocalAddress_OldNames,file = "/Users/hillmann/Projects/Geocoding/Data/LocalAddressDumpCensus_OldVarNames.csv")
write.csv(PermAddress_OldNames,file = "/Users/hillmann/Projects/Geocoding/Data/PermAddressDumpCensus_OldVarNames.csv")

  