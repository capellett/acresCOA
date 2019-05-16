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
