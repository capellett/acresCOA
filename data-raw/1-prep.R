library(readxl)
library(tidyverse)

#' Read spreadsheets downloaded as described in the vignette.
read_downloaded_files <- function(directory) {
  lapply(
    list.files(directory),
    function(i) {
      readr::read_csv(
        file.path(directory, i),
        guess_max=10000) %>%
        dplyr::select(Year, State, County, Commodity, `Data Item`,
                      Domain, `Domain Category`, Value, `CV (%)`)
    } )
}

munge_land_download <- function(x) {
  x2 <- str_split(x$`Data Item`, fixed(" - "), n=2, simplify=TRUE)
  x$LandType <- x2[,1]
  x$Domain <- x2[,2]
  x$Value <- as.numeric(str_remove(x$Value, fixed(',')))
  x$`CV (%)` <- as.numeric(x$`CV (%)`)
  x$High <- (1+(x$`CV (%)`/100))* x$Value
  x$Low <- (1-(x$`CV (%)`/100))* x$Value
  x[,c('Commodity', 'Domain Category', 'Data Item')] <- NULL
  x
}
stateLand <- read_downloaded_files('data-raw//downloaded_files//stateLandAreas')
stateLand[[3]] <- mutate(stateLand[[3]], Value=as.character(Value))
stateLand <-  bind_rows(stateLand)
stateLand <- munge_land_download(stateLand) %>%
  select(-County)

scLand <- filter(stateLand, State=='SOUTH CAROLINA') %>%
  select(-State, -`CV (%)`)

scLand %>%
  select(-High, -Low) %>%
  spread(LandType, Value) %>%
  scwateruse::reporTable()

munge_crop_download <- function(x) {
  x2 <- str_split(x$`Data Item`, fixed(" - "), n=2, simplify=TRUE)
  x$CropType <- x2[,1]
  x$Domain <- x2[,2]
  x$type <- if_else(
    str_detect(x$CropType, fixed('IRRIGATED')),
    'Irrigated', 'All')
  x$CropType <- str_remove(x$CropType, fixed(', IRRIGATED'))
  x$CommodityComma <- paste0(x$Commodity, ', ')
  x$CropType <- str_remove(x$CropType, fixed(x$CommodityComma))
  x$CropType <- str_remove(x$CropType, fixed(x$Commodity))
  x[,c('Data Item', 'CommodityComma')] <- NULL
  x$Value <- as.numeric(str_remove(x$Value, fixed(',')))
  x$`CV (%)` <- as.numeric(x$`CV (%)`)
  x$High <- (1+(x$`CV (%)`/100))* x$Value
  x$Low <- (1-(x$`CV (%)`/100))* x$Value
  x$Crop <- x$Commodity
  x$Commodity <- NULL
  x
}

stateCrop <- read_downloaded_files('data-raw//downloaded_files//stateCropAreas') %>%
  bind_rows() %>% munge_crop_download() %>% select(-County)

countyCrop <- read_downloaded_files('data-raw//downloaded_files//countyCropAreas') %>%
  bind_rows() %>% munge_crop_download()

countyAcres <- filter(countyCrop, Domain=='ACRES HARVESTED') %>%
  select(-Domain, -`Domain Category`) %>%
  rename(Acres=Value)

countyFarms <- filter(countyCrop, Domain=='OPERATIONS WITH AREA HARVESTED') %>%
  select(-Domain) %>%
  rename(Farms=Value)

# rm(county)

stateAcres <- filter(stateCrop, Domain=='ACRES HARVESTED') %>%
  # select(-Domain, -`Domain Category`) %>%
  rename(Acres=Value)

stateFarms <- filter(stateCrop, Domain=='OPERATIONS WITH AREA HARVESTED') %>%
  select(-Domain) %>%
  rename(Farms=Value)

# rm(state)

state5 <- stateAcres %>%
  filter(State=='SOUTH CAROLINA') %>%
  filter(!str_detect(CropType, fixed('CROP'))) %>%
  filter((Crop == 'CORN' & CropType == 'GRAIN') |
           (Crop == 'COTTON' & CropType == 'UPLAND') |
           (Crop == 'WHEAT' & CropType == 'WINTER') |
           (Crop %in% c('PEANUTS', 'SOYBEANS'))) %>%
           # (Crop %in% c('COTTON', 'WHEAT', 'PEANUTS', 'SOYBEANS'))) %>%
  filter(`Domain Category`=='NOT SPECIFIED')

ggplot(state5, aes(x=Year, y=Acres/1000, linetype=type)) +
  geom_ribbon(aes(ymin=Low/1000, ymax=High/1000), alpha=.3) +
  geom_line() +
  facet_wrap("Crop") +
  scale_y_continuous(name='Acres (thousands)',
                   labels=scales::comma_format())


state5 %>%
  group_by(Year, type) %>%
  summarise(Acres = sum(Acres, na.rm=T)) %>%
  spread(type, Acres) %>%
  scwateruse::reporTable()

x <- countyAcres %>%
  filter(State=='SOUTH CAROLINA') %>%
  filter((Crop == 'CORN' & CropType == 'GRAIN') |
           (Crop %in% c('COTTON', 'PEANUTS', 'SOYBEANS', 'WHEAT'))) %>%
  group_by(Year, type) %>%
  summarise(Acres=sum(Acres, na.rm=T))

## corn for grain, cotton, peanuts, soybeans, wheat


#usethis::use_data(state, overwrite=TRUE)
usethis::use_data(countyArea, overwrite=TRUE)
usethis::use_data(countyFarms, overwrite=TRUE)



s2 <- str_split(state$`Data Item`, fixed(" - "), n=2, simplify=TRUE)
unique(s2[,2])




x <- filter(state, State=='SOUTH CAROLINA') %>%
  filter(Domain=='TOTAL') %>%
  Value=as.numeric(Value) %>%
  group_by()


############## Old Script #################
## The Census of Agriculture
coa10 <- read_excel("data-raw//CensusOfAg_Table10.xls", na="(D)")
coa10[coa10 == '-'] <- NA

## Call NAs 0, this isn't correct.
## Should distribute the statewide totals among the NA counties.
# coa10[is.na(coa10)] <- 0
coa10[,4:51] <-
  as.numeric(unlist(coa10[,4:51]))

statewide <- coa10 %>%
  select(1:5) %>%
  filter(Classification == "Harvested cropland" & Unit=="Acres") %>%
  select(Category, Year, Acres=`South Carolina`)

# statewide %>%
#   mutate(Year=as.factor(Year)) %>%
#   spread(key='Category', value='Acres') %>%
#   ggplot(aes(x=Year)) +
#   geom_col(aes(y=All/1000), color='black', fill='tan') +
#   geom_col(aes(y=Irrigated/1000), color='black', fill = 'skyblue') +
#   geom_text(aes(y=((Irrigated/1000)+100), label=prettyNum(round(Irrigated/1000),big.mark=",")),
#             color='blue', size=4, hjust='center', vjust='top') +
#   scale_y_continuous(labels=comma) +
#   ylab("Acres (thousands)") +
#   ggtitle("Harvested Cropland in South Carolina",
#           "Irrigated portion in blue.") +
#   theme_bw() +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#         panel.background = element_blank(), axis.line = element_line(colour = "black"))

countyIrrigation <- coa10 %>%
  # select(-`South Carolina`) %>%
  gather(County, Value, 5:51) %>%
  filter(Classification != "Land") %>%
  filter(Classification == "Farms" | Unit != "Farms") %>%
  select(-Unit) %>%
  spread(Classification, Value)

  # spread(Unit, Value) %>%
  # filter(Classification == "Harvested cropland") %>%
  # select(-Classification, -Farms) %>%
  # mutate(# Year = as.factor(Year),
  #        # Year = factor(Year, levels = rev(levels(Year))),
  #        County = as.factor(County),
  #       # County = factor(County, levels = rev(levels(County)))) %>%
  # spread(Category, Acres) # %>%
  # # mutate(PercentIrrigatedHarvestedCropland = Irrigated * 100 / All)

usethis::use_data(countyIrrigation)

##################################

FieldCropsByCounty <- read_excel("data-raw//CensusOfAg_FieldCropsByCounty.xlsx", na='(D)')

cropIrrigation <-
  FieldCropsByCounty %>%
  mutate( `CV (%)`=round(as.numeric(`CV (%)`), 1),
         `Data Item`=str_sub(`Data Item`, end=-19)) %>%
  mutate(Irrigated = if_else(str_detect(`Data Item`, 'IRRIGATED'),
                             'IrrigatedAcres', 'AllAcres'),
         `Data Item`=str_replace(`Data Item`, ', IRRIGATED, ', ', ')) %>%
  mutate(`Data Item`=str_replace(`Data Item`, ', IRRIGATED', '')) %>%
  mutate(`Data Item`=str_replace(`Data Item`, paste0(Commodity, ', '), '')) %>%
  mutate(`Data Item`=str_replace(`Data Item`, Commodity, ''))

  # group_by(Year, County, Commodity, `Data Item`, Irrigated) %>%
  # summarise(Acres = sum(Value, na.rm=TRUE)) %>%

  # spread(Commodity, Acres, fill=0) %>%
  # mutate(`HAY & HAYLAGE TOTALS` = HAY + HAYLAGE + `HAY & HAYLAGE`) %>%
  # gather(Commodity, Acres, BARLEY:`HAY & HAYLAGE TOTALS`) %>%
  # spread(Year, Acres, fill=0) %>%
  # gather(Year, Acres, `1997`:`2012`) %>%
  # spread(Irrigated, Acres, fill=0) %>%
  # ungroup() %>%
  # filter(!(Commodity %in% c(
  #   "HAY", "HAYLAGE", "HAY & HAYLAGE",
  #   "GRASSES", "LEGUMES", "GRASSES & LEGUMES, OTHER"))) %>%
  # mutate( # PercentIrrigated = IrrigatedAcres/AllAcres,
  #        County = as.factor(County), Commodity = as.factor(Commodity))


### drop commodities included in other totals.

usethis::use_data(cropIrrigation)


######### FRIS

## 425 total responses in SC, 54,301 acres irrigated, 27,423 acrefeet applied
## TODO: readin 1, 4, 5, 6, 12, 17, 18, 22, 40-44
## TODO: define a FRIS readin function from the CSVs?

FRISdata <- "Census of Agriculture//FRIS_SC.xlsx"

FRIS25 <- read_excel(
  FRISdata, sheet='Table25_edit', skip=3, na='-',
  col_types=c('text', 'text', 'numeric', 'numeric', 'numeric'))

FRIS26 <- read_excel(
  FRISdata, sheet='Table26_edit', skip=4, na='-',
  col_types=c('text', 'text', 'numeric','numeric'),
  col_names=c('Area', 'Source', 'Farms', 'AcresIrrigated'))

FRIS27 <- read_excel(
  FRISdata, sheet='Table27_edit', skip=6, na='(D)') %>%
  dplyr::mutate(Farms = as.numeric(replace(Farms, Farms=="-", '0')),
                IrrAcres.PrevCensusYr = as.numeric(replace(
                  Acres.Irrigated.Previous.Census.Year,
                  Acres.Irrigated.Previous.Census.Year=='-', '0')))

FRIS35 <- read_excel(
  FRISdata, sheet='Table35_edit', skip=6, na='-',
  col_types=c('text','text','numeric','text',
              'numeric','numeric','numeric','numeric'),
  col_names=c('AreaType','AreaName','Year','Crop','IrrAcres',
              'IrrYield','NonIrrAcres','NonIrrYield'))

FRIS36 <- bind_rows(
  read_excel("Census of Agriculture//fris08_t28.xlsx", sheet=5, na='(D)'),
  read_excel("Census of Agriculture//fris13_t36.xlsx", sheet=5, na='(D)')) %>%
  mutate_at(vars(Farms:`Average acre-feet applied per acre`),
            function(x) as.numeric(if_else(x=='-',"0",x) ) ) %>%
  unique()

# dnr_save(FRIS25, input=TRUE)
# dnr_save(FRIS26, input=TRUE)
# dnr_save(FRIS27, input=TRUE)
# dnr_save(FRIS35, input=TRUE)
# dnr_save(FRIS36, input=TRUE)

# rm(FRISdataFolder, FRISdata, FRIS25, FRIS26, FRIS27, FRIS35, FRIS36)

# setwd('Planning_Input//Agriculture')
write_excel_csv(FRIS25, "Barriers to investment.csv")
write_excel_csv(FRIS26, "Irrigation by source.csv")
write_excel_csv(FRIS27, "Change in irrigation.csv")
write_excel_csv(FRIS35, "Yield by irrigation.csv")
write_excel_csv(FRIS36, "Irrigation by method.csv")
write_excel_csv(IrrigatedAcresByCounty, "IrrigatedAcresByCounty.csv")
write_excel_csv(IrrigatedCropsByCounty, "IrrigatedCropsByCounty.csv")
