library(tidyverse)
library(acresCOA)

### I'm not sure this part is necessary

# countyIrrigation_old <-  countyIrrigation
# cropIrrigation_old <-  cropIrrigation
#
# ## Cropland, harvested, irrigated
# ### state-wide
# stateLand_old <- stateLand
#
# stateLand <- read_downloaded_files('data-raw//downloaded_files//stateLandAreas')
# stateLand[[3]] <- mutate(stateLand[[3]], Value=as.character(Value))
# stateLand[[4]] <- mutate(stateLand[[4]], Value=as.character(Value))
# stateLand <-  bind_rows(stateLand)
# stateLand <- munge_land_download(stateLand) %>%
#   select(-County)
# stateLand <- unique(stateLand)
#
# usethis::use_data(stateLand, overwrite = TRUE)
#
# ### county-wide
# countyLand_old <-  countyLand
#
# countyLand <- read_downloaded_files('data-raw//downloaded_files//countyLandAreas')
# countyLand <- bind_rows(countyLand)
# countyLand <- munge_land_download(countyLand)
# countyLand <- unique(countyLand)
#
# usethis::use_data(countyLand, overwrite=TRUE)




## Field crops

### State-wide
stateFieldCrop <- read_downloaded_files('data-raw//downloaded_files//stateFieldCropAreas') %>%
  bind_rows()

#### Some of the wrong data got in there...
stateFieldCrop %>%
  group_by(Year) %>%
  summarise(Commodity = length(unique(Commodity)),
            DataItem = length(unique(`Data Item`)),
            Domain = length(unique(Domain)),
            DomainCat = length(unique(`Domain Category`)))

stateFieldCrop %>%
  group_by(Year) %>%
  summarise(Domain = paste0(unique(Domain), collapse='; '))

#### This seems to work
x <- stateFieldCrop %>%
  dplyr::filter(Domain == 'TOTAL') %>%
  dplyr::select(-`Domain Category`, -Domain)

x %>%
  group_by(Year) %>%
  summarise(Commodity = length(unique(Commodity)),
            DataItem = length(unique(`Data Item`)))

y <- x %>% munge_crop_download()
unique(y$Domain)
unique(y$type)
unique(y$County)

stateFieldCrop <- y %>% select(-County) %>% unique()
usethis::use_data(stateFieldCrop, overwrite=TRUE)


## County-wide
countyFieldCrop <- read_downloaded_files('data-raw//downloaded_files//countyFieldCropAreas') %>%
  bind_rows()

#### see if the wrong data got in to this too...
countyFieldCrop %>%
  group_by(Year) %>%
  summarise(Commodity = length(unique(Commodity)),
            DataItem = length(unique(`Data Item`)),
            Domain = length(unique(Domain)),
            DomainCat = length(unique(`Domain Category`)))

countyFieldCrop %>%
  group_by(Year) %>%
  summarise(Domain = paste0(unique(Domain), collapse='; '))

countyFieldCrop <- countyFieldCrop %>%
  dplyr::filter(Domain == 'TOTAL') %>%
  munge_crop_download()

unique(countyFieldCrop$Domain)
unique(countyFieldCrop$`Domain Category`)

countyFieldCrop <- countyFieldCrop %>%
  select(-`Domain Category`) %>%
  unique()

usethis::use_data(countyFieldCrop, overwrite=TRUE)

### old code to split the acres from number of operations
# countyFieldCropAcres <- filter(countyCrop, Domain=='ACRES HARVESTED') %>%
#   select(-Domain, -`Domain Category`) %>%
#   rename(Acres=Value)
#
# countyFieldCropFarms <- filter(countyCrop, Domain=='OPERATIONS WITH AREA HARVESTED') %>%
#   select(-Domain) %>%
#   rename(Farms=Value)

# stateFieldCropAcres <- filter(stateCrop, Domain=='ACRES HARVESTED') %>%
#   # select(-Domain, -`Domain Category`) %>%
#   rename(Acres=Value)
#
# stateFieldCropFarms <- filter(stateCrop, Domain=='OPERATIONS WITH AREA HARVESTED') %>%
#   select(-Domain) %>%
#   rename(Farms=Value)


# usethis::use_data(countyArea, overwrite=TRUE)
# usethis::use_data(countyFarms, overwrite=TRUE)


# Other Crops
## By state (only for SC)
stateOtherCrop <- read_downloaded_files('data-raw//downloaded_files//stateOtherCropAreas') %>%
  bind_rows() %>%
  filter(Domain == "TOTAL") %>%
  munge_crop_download() %>%
  select(-County, -`Domain Category`)

x <- stateOtherCrop %>%
  filter(Year==2017 &
           # Domain == 'ACRES GROWN' &
           type == 'Irrigated')

scwateruse::reporTable(x)
### get rid of the part of crop stuff... ?


## By county (only for SC)
countyOtherCrop <- read_downloaded_files('data-raw//downloaded_files//countyOtherCropAreas') %>%
  bind_rows() %>%
  filter(Domain == "TOTAL") %>%
  munge_crop_download() %>%
  select(-`Domain Category`)


## Program: CENSUS
## Group: IRRIGATION
## Geographic Level: STATE
## 46,256 records. nov 6, 2020
x2 <- read_downloaded_files(
  'data-raw//downloaded_files//stateIrrigationStats') %>%
  bind_rows()

x2 %>%
  dplyr::mutate(
    Commodity=as_factor(Commodity),
    `Data Item` = as_factor(`Data Item`),
    Domain = as_factor(Domain),
    `Domain Category` = as_factor(`Domain Category`)) %>%
  .[,c("Commodity", "Data Item", "Domain", "Domain Category")] %>%
  unique() %>%
  scutils::reporTable()

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
