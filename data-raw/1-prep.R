library(tidyverse)
source('R/readin.R')


### Cropland, harvested, irrigated
stateLand <- read_downloaded_files('data-raw//downloaded_files//stateLandAreas')
stateLand <- lapply(stateLand, function(x) dplyr::mutate(x, Value = as.character(Value)))
# stateLand[[3]] <- mutate(stateLand[[3]], Value=as.character(Value))
stateLand <-  bind_rows(stateLand)
stateLand <- munge_land_download(stateLand) %>%
  select(-County)
usethis::use_data(stateLand, overwrite = TRUE)

countyLand <- read_downloaded_files('data-raw//downloaded_files//countyLandAreas')
countyLand <- bind_rows(countyLand)
countyLand <- munge_land_download(countyLand)
usethis::use_data(countyLand, overwrite=TRUE)



### This section doesn't work. I think it is superseded by the "update.R" script.

# ### Field crops
# stateFieldCrop <- read_downloaded_files('data-raw//downloaded_files//stateCropAreas') %>%
#   bind_rows() %>% munge_crop_download() # %>% select(-County)
#
# countyFieldCrop <- read_downloaded_files('data-raw//downloaded_files//countyCropAreas') %>%
#   bind_rows() %>% munge_crop_download()
#
# countyFieldCropAcres <- filter(countyCrop, Domain=='ACRES HARVESTED') %>%
#   select(-Domain, -`Domain Category`) %>%
#   rename(Acres=Value)
#
# countyFieldCropFarms <- filter(countyCrop, Domain=='OPERATIONS WITH AREA HARVESTED') %>%
#   select(-Domain) %>%
#   rename(Farms=Value)
#
# stateFieldCropAcres <- filter(stateCrop, Domain=='ACRES HARVESTED') %>%
#   # select(-Domain, -`Domain Category`) %>%
#   rename(Acres=Value)
#
# stateFieldCropFarms <- filter(stateCrop, Domain=='OPERATIONS WITH AREA HARVESTED') %>%
#   select(-Domain) %>%
#   rename(Farms=Value)
#
#
# usethis::use_data(countyArea, overwrite=TRUE)
# usethis::use_data(countyFarms, overwrite=TRUE)






##### This is older and likely irrelevant.

# scLand <- filter(stateLand, State=='SOUTH CAROLINA') %>%
#   select(-State, -`CV (%)`)
#
# scLand %>%
#   select(-High, -Low) %>%
#   spread(LandType, Value) %>%
#   scwateruse::reporTable()

# state5 <- stateAcres %>%
#   filter(State=='SOUTH CAROLINA') %>%
#   filter(!str_detect(CropType, fixed('CROP'))) %>%
#   filter((Crop == 'CORN' & CropType == 'GRAIN') |
#            (Crop == 'COTTON' & CropType == 'UPLAND') |
#            (Crop == 'WHEAT' & CropType == 'WINTER') |
#            (Crop %in% c('PEANUTS', 'SOYBEANS'))) %>%
#            # (Crop %in% c('COTTON', 'WHEAT', 'PEANUTS', 'SOYBEANS'))) %>%
#   filter(`Domain Category`=='NOT SPECIFIED')
#
# ggplot(state5, aes(x=Year, y=Acres/1000, linetype=type)) +
#   geom_ribbon(aes(ymin=Low/1000, ymax=High/1000), alpha=.3) +
#   geom_line() +
#   facet_wrap("Crop") +
#   scale_y_continuous(name='Acres (thousands)',
#                    labels=scales::comma_format())
#
#
# state5 %>%
#   group_by(Year, type) %>%
#   summarise(Acres = sum(Acres, na.rm=T)) %>%
#   spread(type, Acres) %>%
#   scwateruse::reporTable()
