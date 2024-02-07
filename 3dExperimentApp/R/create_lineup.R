## ---------------------------
##
## Script name: create_lineup.R
##
## Purpose of script:
##
## Author: Tyler Wiederich
##
## Date Created: 2024-01-31
##
## ---------------------------
##
## Notes:
##   
##
## ---------------------------


create_lineup <- function(subKit, num.kits = 21, seed = NULL){
  require(dplyr)
  set.seed(seed)
  online <- FALSE
  
  #Randomize kit number if online 
  if(subKit == 'Online'){
    online <- TRUE
    subKit <- sample(1:num.kits, size = 1)
  }
  
  #Options for given kit
  lineup.options <- printedKitInfo %>% 
    select(-fileID) %>%
    filter(kit == subKit) %>% 
    left_join(fileKey, by = "trueRatio") %>% 
    filter(fileID < 15) %>% 
    select(kit, trueRatio, fileID) %>% 
    group_by(trueRatio)
  
  #Randomly select from fileID, giving type 1 or type 3
  lineup.2dDigital <- lineup.options %>% 
    slice_sample(n = 1) %>% 
    mutate(plot = "2dDigital")
  lineup.3dStatic <- lineup.options %>% 
    slice_sample(n = 1) %>%
    mutate(plot = "3dStatic")
  lineup.3dDigital <- lineup.options %>% 
    slice_sample(n = 1) %>%
    mutate(plot = "3dDigital")
  
  #All options that don't need kit
  lineup.online <- bind_rows(lineup.2dDigital, lineup.3dStatic, lineup.3dDigital) 
  
  #Combine online and physical lineups if not online
  if(online){
    lineup <- mutate(lineup.online, kitMarking = NA)
  } else{
    lineup.physical <- printedKitInfo %>% 
      # select(fileID) %>%
      filter(kit == subKit)
    lineup <- bind_rows(lineup.online, lineup.physical)
  }
  
  #Randomize lineup order
  lineup.randomized <- lineup %>% 
    ungroup() %>% 
    slice_sample(n = nrow(lineup)) %>% 
    mutate(trial_order = 1:n())
  
  return(lineup.randomized)
}
