library(tidyverse)
censusPDB <- read_csv("Downloads/pdb2021bgv3_us.csv")
PermAddress <- read_csv("Projects/Geocoding/Data/PermAddressDumpCensus_OldVarNames.csv", 
                        col_types = cols(...1 = col_skip(), ...2 = col_skip()))
LocalAddress <- read_csv("~/Projects/Geocoding/Data/LocalAddressDumpCensus_OldVarNames.csv")

censusTrim <- censusPDB %>% 
  select(State,County,Tract,Block_group,LAND_AREA) %>% 
  mutate(block_id = paste0(State,County,Tract,Block_group))

PermAddress <- PermAddress %>% 
  mutate(block_id = paste0(state,county,tract,block)) %>% 
  mutate(block_id = str_replace_all(block_id,pattern = "...$","")) %>% 
  left_join(censusTrim[,c("block_id","LAND_AREA")]) 

LocalAddress <- LocalAddress %>% 
  mutate(block_id = paste0(state,county,tract,block)) %>% 
  mutate(block_id = str_replace_all(block_id,pattern = "...$","")) %>% 
  left_join(censusTrim[,c("block_id","LAND_AREA")])

write.csv(LocalAddress,file = "~/Projects/Geocoding/Data/LocalAddressDumpCensus_OldVarNames_withLandArea.csv")
write.csv(PermAddress,file = "~/Projects/Geocoding/Data/PermAddressDumpCensus_OldVarNames_withLandArea.csv")
  