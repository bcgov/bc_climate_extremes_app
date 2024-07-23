## R Shiny app An app to visualize monthly and annual temperature and precipitation anomalies in BC and its sub regions ( eco-regions and watersheds) along with their trends.
## author: Aseem Raj Sharma aseem.sharma@gov.bc.ca
# Copyright 2023 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

# Required -------------------
library('shiny')
library('shinydashboard')
library('shinyWidgets')
library("shinythemes")
library("shinyjs")
library('shinyalert')
library('markdown')
library('rmarkdown')

library('sf')
library('terra')
library('leaflet')
library('tidyterra')

library('tidyverse')
library('magrittr')
library('plotly')

library('lubridate')
library("DT")
library('zoo')
library('zyp')
library('spatialEco')

library('viridisLite')
library('cptcity')
library('RColorBrewer')
library('colorspace')


# Load and process input data -------
## Paths --
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
shp_fls_pth <- './shapefiles/'
extrm_indx_dt_pth <-  './extrm_indices_wna_mswx_database/'

# Credit  -----
plt_wtrmrk <-
  "Created by Aseem Sharma (aseem.sharma@gov.bc.ca), BC Ministry of Forests. Data credit: MSWX/GloH2O."
plt_wtrmrk

## Shape files --------------
# Domain
xmi = -140
xmx = -108
ymi = 39
ymx = 61

# List of shape files
list.files(path = shp_fls_pth,
           pattern = ".shp",
           full.names = T) -> shp_fls_lst
shp_fls_lst

# BC
bc_shp <-
  st_read(shp_fls_lst[str_detect(shp_fls_lst, "bc_shapefile") == T])
# plot(st_geometry(bc_shp))

# BC eco-provinces
bc_ecoprv_shp <-
  st_read(shp_fls_lst[str_detect(shp_fls_lst, "bc_ecoprovince") == T])
# plot(st_geometry(bc_ecoprv_shp))

# BC eco-regions
bc_ecorgns_shp <-
  st_read(shp_fls_lst[str_detect(shp_fls_lst, "bc_ecoregions") == T])
# plot(st_geometry(bc_ecorgns_shp))

# BC eco-sections
bc_ecosec_shp <-
  st_read(shp_fls_lst[str_detect(shp_fls_lst, "bc_ecosections") == T])
# plot(st_geometry(bc_ecosec_shp))

# BC watersheds
bc_wtrshd_shp <-
  st_read(shp_fls_lst[str_detect(shp_fls_lst, "bc_watersheds") == T])
# plot(st_geometry(bc_wtrshd_shp))

# BC municipalities
bc_muni_shp <-
  st_read(shp_fls_lst[str_detect(shp_fls_lst, "bc_municipalities") == T])
# plot(st_geometry(bc_muni_shp))

# Western North America
na_shp <-
  st_read(shp_fls_lst[str_detect(shp_fls_lst, "north_america") == T])
sf_use_s2(FALSE)

wna_shp <-
  st_crop(
    na_shp,
    xmin = xmi,
    ymin = ymi,
    xmax = xmx,
    ymax = ymx
  )
# plot(st_geometry(wna_shp))
# wna_shp <- sf::st_cast(wna_shp, "MULTIPOLYGON")

## Extreme indices chhoice format ----
months_nam <-
  c(
    "annual",
    "Jan",
    "Feb",
    "Mar",
    "Apr",
    "May",
    "Jun",
    "Jul",
    "Aug",
    "Sep",
    "Oct",
    "Nov",
    "Dec"
  )
months_nam

extrm_indx_mtdt <- read_csv(paste0(extrm_indx_dt_pth,'climate_extreme_indices_database_metadata.csv'))
extrm_indx_mtdt

# Add categories to the indices
indx <- unique(extrm_indx_mtdt$indx)
indx

# Temperature and precipitation variables in the  indices

extrm_indx_mtdt %<>%
  mutate(tmp_prc=ifelse(indx == 'fd' | indx == 'tnlt2' | indx ==  'tnltm2'|
                       indx == 'tnltm20' | indx == 'id' | indx ==  'su' |
                       indx == 'tr' | indx == 'gsl' | indx ==  'tx2tn2' |
                       indx == 'txx' | indx == 'tnx' | indx ==  'tnn'|
                       indx == 'wsdi' | indx =='wsdi5' | indx == 'csdi' |
                       indx ==  'tn10p' |indx == 'tn90p' | indx == 'csdi5' |
                       indx == 'tmlt10' | indx ==  'tmlt5' |indx == 'tmge10' |
                       indx == 'tmge5' | indx ==  'tx10p'|indx == 'txm'|
                       indx == 'tx90p' |indx == 'tx95t' |  indx == 'txb3tnb3'| indx == 'tx3tn3' | indx ==  'txge30' |
                       indx == 'cddcold18'| indx == 'dtr' | indx == 'tmm' |indx == 'tnm'|
                       indx == 'txn' | indx == 'txge35' |indx == 'txgt50p' |
                       indx == 'gddgrow10' | indx == 'gddgrow10'| indx =='hddheat18'|
                       indx == 'hw'| indx == "cwa_ecf" |indx ==   "cwd_ecf" |indx ==   "cwf_ecf"
                     |indx == "cwm_ecf"|indx ==   "cwn_ecf" |indx ==  "hwa_ehf"|indx ==   "hwa_tn90"
                     |indx =="hwa_tx90" |indx == "hwd_ehf" |indx == "hwd_tn90" |indx ==  "hwd_tx90"
                     |indx == "hwf_ehf" |indx ==  "hwf_tn90" |indx == "hwf_tx90" |indx == "hwm_ehf"
                     |indx == "hwm_tn90" |indx == "hwm_tx90" |indx == "hwn_ehf" |indx == "hwn_tn90"
                     |indx == "hwn_tx90",'temp',
                     ifelse(indx == 'r20mm'|indx == 'cdd'|  indx == 'r10mm'| indx ==  'r30mm' |
                              indx == 'r95p' | indx == 'r95ptot' | indx ==  'r99p' |
                              indx == 'r99ptot' | indx == 'rx1day' | indx ==  'rx5day'  | indx ==  'rx7day' |
                              indx == 'cwd' | indx == 'prcptot' | indx ==  'rx5day' |
                              indx == 'sdii'| indx == 'spei'| indx =='spi'|
                              indx == 'spei3'| indx =='spi3'| indx == 'spei6'| indx =='spi6'|
                              indx == 'spei12'| indx =='spi12'| indx == 'spei24'| indx =='spi24'
                            ,'prcp', NA)))

# Heat-cold precipitation and drought  categories
extrm_indx_mtdt%<>%
  mutate(category=ifelse(indx == 'fd' | indx == 'id'|indx == 'gsl'|
                           indx == 'csdi' | indx == "csdi5"|indx == 'xx'|
                           indx == "tmlt10" | indx == "tmlt5" |indx ==  "tn10p"|
                           indx == "tnlt2" | indx == "tnltm2"|indx == "tnltm20"|
                           indx == "tnn"  | indx == "txn"|indx == 'xx'|
                           indx == "tx10p" | indx == "txb3tnb3"|indx == "tnm",'cold',

                         ifelse(indx == 'cddcold18' | indx == "gddgrow10" |
                                  indx == "hw" |indx == "tx95t"|indx =='hddheat18'|
                                  indx == "su" | indx == "tmge10"|indx == "tmge5" |
                                  indx == "tn90p" | indx == "tnx"| indx == "tmm"|
                                  indx == "tr" | indx == "tx90p"|indx == "txx" | indx == 'tx3tn3' |
                                  indx == "tx95t" | indx == "txge30"|indx == "txge35"|
                                  indx == "txgt50p" | indx == "txm"|indx == "wsdi"|indx == "wsdi5"
                                |indx == "dtr", 'heat',

                                ifelse(indx == "cwa_ecf" | indx ==  "cwd_ecf" | indx ==   "cwf_ecf" |
                                         indx ==   "cwm_ecf" | indx ==   "cwn_ecf" , 'coldwave',

                                       ifelse(indx ==  "hwa_ehf" | indx ==  "hwa_tn90" | indx ==  "hwa_tx90"
                                              | indx ==  "hwd_ehf"  | indx == "hwd_tn90" | indx ==  "hwd_tx90"
                                              | indx ==   "hwf_ehf" | indx ==    "hwf_tn90" | indx ==  "hwf_tx90"
                                              | indx ==  "hwm_ehf" | indx ==   "hwm_tn90" | indx ==  "hwm_tx90"
                                              | indx ==  "hwn_ehf"| indx == "hwn_tn90" | indx ==  "hwn_tx90", 'heatwave',

                                              ifelse(indx ==  "spi" | indx ==  "spei" |
                                                       indx ==  "spi3" | indx ==  "spei3" |
                                                       indx ==  "spi6" | indx ==  "spei6" |
                                                       indx ==  "spi12" | indx ==  "spei12" |
                                                       indx ==  "spi24" | indx ==  "spei24" , 'drought',

                                                     ifelse(indx == "cwd" | indx == "cdd" | indx == "prcptot"|indx ==  "r10mm"|
                                                              indx == "r20mm" | indx == "r30mm"|indx == "r99ptot" |
                                                              indx == "r95p" | indx == "r95ptot"|indx == "r99p"|
                                                              indx == "rx1day" | indx == "rx7day"| indx == "rx5day"|
                                                              indx == "sdii" |indx == "rx5day", 'prcp',
                                                            'other')))))))
extrm_indx_mtdt


# Crete a index choice list to choose in the app

indx_def <- unique(extrm_indx_mtdt$indx_long_name)
indx_def
# index choixes list

indx_choice_list <- list(
  "Heat indices",
  "Cooling degree days (cddcold18)"="cddcold18",
  "Daily temperature range (dtr)"="dtr",
  "Heating degree days (hddheat18)"="hddheat18",
  "Summer days (su)"="su",
  "Average temperature of at least 10c (tmge10)"="tmge10",
  "Average temperature of at least 5c (tmge5)"="tmge5",
  "Average temperature below 10c (tmlt10)"="tmlt10",
  "Mean daily mean temperature (tmm)"="tmm",
  "Amount of warm nights (tn90p)"="tn90p",
  "Tropical nights (tr)"="tr",
  "Amount of cool days (tx10p)"="tx10p",
  "Amount of hot days (tx90p)"="tx90p",
  "User-defined consecutive number of cold days and nights (txb3tnb3)"="txb3tnb3",
  "User-defined consecutive number of hot days and nights (tx3tn3)"="tx3tn3",
  "Maximum temperature of at least 30c (txge30)"="txge30",
  "Maximum temperature of at least 35c (txge35)"="txge35",
  "Fraction of days with temperatures above the median (txgt50p)"="txgt50p",
  "Mean daily maximum temperature (txm)"="txm",
  "Warmest daily maximum temperature (txx)"="txx",
  "Warm spell duration indicator (wsdi)"="wsdi",
  "User-defined Warm spell duration indicator (wsdi5)"="wsdi5",
  "Precipitation indices",
  "Consecutive wet days (cwd)"="cwd",
  "Annual total wet-day precipitation (prcptot)"="prcptot",
  "Number of heavy rain days (r10mm)"="r10mm",
  "Number of very heavy rain days (r20mm)"="r20mm",
  "Total annual pr from heavy rain days (r95p)"="r95p",
  "Contribution from very wet days (r95ptot)"="r95ptot",
  "Total annual precipitation from very heavy rain days (r99p)"="r99p",
  "Contribution from extremely wet days (r99ptot)"="r99ptot",
  "Number of customised rain days (r30mm)"="r30mm",
  "Max 1-day precipitation (rx1day)"="rx1day",
  "Max 5-day precipitation (rx5day)"="rx5day",
  "User-defined consecutive days precipitation amount (rx7day)"="rx7day",
  "Daily precipitation intensity (sdii)"="sdii",
  "Drought indices",
  "Consecutive dry days (cdd)"="cdd",
  "Standardised precipitation evapotranspiration index- 12 months (spei12)"="spei12",
  "Standardised precipitation evapotranspiration index- 3 months (spei3)"="spei3",
  "Standardised precipitation evapotranspiration index - 6 months (spei6)"="spei6",
  "Standardised precipitation index- 12 months (spi12)"="spi12",
  "Standardised precipitation index- 3 months (spi3)"="spi3",
  "Standardised precipitation index- 6 months (spi6)"="spi6",
  "Heatwave indices",
  "Heatwave amplitude by the excess heat factor (hwa_ehf)"="hwa_ehf",
  "Heatwave amplitude by the 90th percentile of tn (hwa_tn90)"="hwa_tn90",
  "Heatwave amplitude by the 90th percentile of tx (hwa_tx90)"="hwa_tx90",
  "Heatwave duration by the excess heat factor (hwd_ehf)"="hwd_ehf",
  "Heatwave duration by the 90th percentile of tn (hwd_tn90)"="hwd_tn90",
  "Heatwave duration by the 90th percentile of tx (hwd_tx90)"="hwd_tx90",
  "Heatwave frequency by the excess heat factor (hwf_ehf)"="hwf_ehf",
  "Heatwave frequency by the 90th percentile of tn (hwf_tn90)"="hwf_tn90",
  "Heatwave frequency by the 90th percentile of tx (hwf_tx90)"="hwf_tx90",
  "Heatwave magnitude by the excess heat factor (hwm_ehf)"="hwm_ehf",
  "Heatwave magnitude by the 90th percentile of tn (hwm_tn90)"="hwm_tn90",
  "Heatwave magnitude by the 90th percentile of tx (hwm_tx90)"="hwm_tx90",
  "Heatwave number by the excess heat factor (hwn_ehf)"="hwn_ehf",
  "Heatwave number by the 90th percentile of tn (hwn_tn90)"="hwn_tn90",
  "Heatwave number by the 90th percentile of tx (hwn_tx90)"="hwn_tx90",
  "Cold indices",
  "Cold spell duration indicator (csdi)"="csdi",
  "User-defined csdi (csdi5)"="csdi5",
  "Frost days (fd)"="fd",
  "Growing degree days (gddgrow10)"="gddgrow10",
  "Growing season length (gsl)"="gsl",
  "Ice days (id)"="id",
  "Average temperature below 5c (tmlt5)"="tmlt5",
  "Amount of cold nights (tn10p)"="tn10p",
  "Minimum temperature below 2c (tnlt2)"="tnlt2",
  "Minimum temperature below -2c (tnltm2)"="tnltm2",
  "Minimum temperature below -20c (tnltm20)"="tnltm20",
  "Mean daily minimum temperature (tnm)"="tnm",
  "Coldest daily minimum temperature (tnn)"="tnn",
  "Warmest daily minimum temperature (tnx)"="tnx",
  "Coldest daily maximum temperature (txn)"="txn",
  "Coldwave indices",
  "Coldwave amplitude by the excess cold factor (cwa_ecf)"="cwa_ecf",
  "Coldwave duration by the excess cold factor (cwd_ecf)"="cwd_ecf",
  "Coldwave frequency by the excess cold factor  (cwf_ecf)"="cwf_ecf",
  "Coldwave magnitude by the excess cold factor (cwm_ecf)"="cwm_ecf",
  "Coldwave number by the excess cold factor (cwn_ecf)"="cwn_ecf"
)

# Index items design list

indx_item_design_list <-
  list(style = c(
    "font-size:18px;font-weight: bold;font-family: Arial; text-align: center; color:white; background: forestgreen;",
    "font-size:13px;font-weight: plain;font-family: Arial; text-align: left ; color:white;",
    "font-size:13px;font-weight: plain; font-family: Arial;text-align: left ; color:white;",
    "font-size:13px;font-weight: plain;font-family: Arial; text-align: left ; color:white;",
    "font-size:13px;font-weight: plain;font-family: Arial; text-align: left ; color:white;",
    "font-size:13px;font-weight: plain;font-family: Arial; text-align: left ; color:white;",
    "font-size:13px;font-weight: plain;font-family: Arial; text-align: left ; color:white;",
    "font-size:13px;font-weight: plain;font-family: Arial; text-align: left ; color:white;",
    "font-size:13px;font-weight: plain;font-family: Arial; text-align: left ; color:white;",
    "font-size:13px;font-weight: plain;font-family: Arial; text-align: left ; color:white;",
    "font-size:13px;font-weight: plain;font-family: Arial; text-align: left ; color:white;",
    "font-size:13px;font-weight: plain;font-family: Arial; text-align: left ; color:white;",
    "font-size:13px;font-weight: plain;font-family: Arial; text-align: left ; color:white;",
    "font-size:13px;font-weight: plain;font-family: Arial; text-align: left ; color:white;",
    "font-size:13px;font-weight: plain;font-family: Arial; text-align: left ; color:white;",
    "font-size:13px;font-weight: plain;font-family: Arial; text-align: left ; color:white;",
    "font-size:13px;font-weight: plain;font-family: Arial; text-align: left ; color:white;",
    "font-size:13px;font-weight: plain;font-family: Arial; text-align: left ; color:white;",
    "font-size:13px;font-weight: plain;font-family: Arial; text-align: left ; color:white;",
    "font-size:13px;font-weight: plain;font-family: Arial; text-align: left ; color:white;",
    "font-size:13px;font-weight: plain;font-family: Arial; text-align: left ; color:white;",
    "font-size:13px;font-weight: plain;font-family: Arial; text-align: left ; color:white;",
    "font-size:18px;font-weight: bold;font-family: Arial; text-align: center; color:white; background: forestgreen;",
    "font-size:13px;font-weight: plain;font-family: Arial; text-align: left ; color:white;",
    "font-size:13px;font-weight: plain;font-family: Arial; text-align: left ; color:white;",
    "font-size:13px;font-weight: plain;font-family: Arial; text-align: left ; color:white;",
    "font-size:13px;font-weight: plain;font-family: Arial; text-align: left ; color:white;",
    "font-size:13px;font-weight: plain;font-family: Arial; text-align: left ; color:white;",
    "font-size:13px;font-weight: plain;font-family: Arial; text-align: left ; color:white;",
    "font-size:13px;font-weight: plain;font-family: Arial; text-align: left ; color:white;",
    "font-size:13px;font-weight: plain; font-family: Arial;text-align: left ; color:white;",
    "font-size:13px;font-weight: plain;font-family: Arial; text-align: left ; color:white;",
    "font-size:13px;font-weight: plain;font-family: Arial; text-align: left ; color:white;",
    "font-size:13px;font-weight: plain;font-family: Arial; text-align: left ; color:white;",
    "font-size:13px;font-weight: plain;font-family: Arial; text-align: left ; color:white;",
    "font-size:13px;font-weight: plain;font-family: Arial; text-align: left ; color:white;",
    "font-size:18px;font-weight: bold;font-family: Arial; text-align: center; color:white; background: forestgreen;",
    "font-size:13px;font-weight: plain;font-family: Arial; text-align: left ; color:white;",
    "font-size:13px;font-weight: plain;font-family: Arial; text-align: left ; color:white;",
    "font-size:13px;font-weight: plain;font-family: Arial; text-align: left ; color:white;",
    "font-size:13px;font-weight: plain;font-family: Arial; text-align: left ; color:white;",
    "font-size:13px;font-weight: plain;font-family: Arial; text-align: left ; color:white;",
    "font-size:13px;font-weight: plain;font-family: Arial; text-align: left ; color:white;",
    "font-size:13px;font-weight: plain;font-family: Arial; text-align: left ; color:white;",
    "font-size:18px;font-weight: bold;font-family: Arial; text-align: center; color:white; background: forestgreen;",
    "font-size:13px;font-weight: plain;font-family: Arial; text-align: left ; color:white;",
    "font-size:13px;font-weight: plain;font-family: Arial; text-align: left ; color:white;",
    "font-size:13px;font-weight: plain;font-family: Arial; text-align: left ; color:white;",
    "font-size:13px;font-weight: plain;font-family: Arial; text-align: left ; color:white;",
    "font-size:13px;font-weight: plain;font-family: Arial; text-align: left ; color:white;",
    "font-size:13px;font-weight: plain;font-family: Arial; text-align: left ; color:white;",
    "font-size:13px;font-weight: plain;font-family: Arial; text-align: left ; color:white;",
    "font-size:13px;font-weight: plain;font-family: Arial; text-align: left ; color:white;",
    "font-size:13px;font-weight: plain;font-family: Arial; text-align: left ; color:white;",
    "font-size:13px;font-weight: plain;font-family: Arial; text-align: left ; color:white;",
    "font-size:13px;font-weight: plain;font-family: Arial; text-align: left ; color:white;",
    "font-size:13px;font-weight: plain;font-family: Arial; text-align: left ; color:white;",
    "font-size:13px;font-weight: plain;font-family: Arial; text-align: left ; color:white;",
    "font-size:13px;font-weight: plain;font-family: Arial; text-align: left ; color:white;",
    "font-size:13px;font-weight: plain;font-family: Arial; text-align: left ; color:white;",
    "font-size:18px;font-weight: bold;font-family: Arial; text-align: center; color:white; background: forestgreen;",
    "font-size:13px;font-weight: plain;font-family: Arial; text-align: left ; color:white;",
    "font-size:13px;font-weight: plain;font-family: Arial; text-align: left ; color:white;",
    "font-size:13px;font-weight: plain;font-family: Arial; text-align: left ; color:white;",
    "font-size:13px;font-weight: plain;font-family: Arial; text-align: left ; color:white;",
    "font-size:13px;font-weight: plain;font-family: Arial; text-align: left ; color:white;",
    "font-size:13px;font-weight: plain; font-family: Arial;text-align: left ; color:white;",
    "font-size:13px;font-weight: plain;font-family: Arial; text-align: left ; color:white;",
    "font-size:13px;font-weight: plain;font-family: Arial; text-align: left ; color:white;",
    "font-size:13px;font-weight: plain;font-family: Arial; text-align: left ; color:white;",
    "font-size:13px;font-weight: plain;font-family: Arial; text-align: left ; color:white;",
    "font-size:13px;font-weight: plain;font-family: Arial; text-align: left ; color:white;",
    "font-size:13px;font-weight: plain;font-family: Arial; text-align: left ; color:white;",
    "font-size:13px;font-weight: plain;font-family: Arial; text-align: left ; color:white;",
    "font-size:13px;font-weight: plain;font-family: Arial; text-align: left ; color:white;",
    "font-size:13px;font-weight: plain;font-family: Arial; text-align: left ; color:white;",
    "font-size:18px;font-weight: bold;font-family: Arial; text-align: center; color:white; background: forestgreen;",
    "font-size:13px;font-weight: plain;font-family: Arial; text-align: left ; color:white;",
    "font-size:13px;font-weight: plain;font-family: Arial; text-align: left ; color:white;",
    "font-size:13px;font-weight: plain;font-family: Arial; text-align: left ; color:white;",
    "font-size:13px;font-weight: plain;font-family: Arial; text-align: left ; color:white;",
    "font-size:13px;font-weight: plain;font-family: Arial; text-align: left ; color:white;",
    "font-size:13px;font-weight: plain;font-family: Arial; text-align: left ; color:white;"
    ))

indx <- unique(extrm_indx_mtdt$indx)
indx

min_year <- 1979
max_year <- 2023

years <- seq(min_year, max_year, 1)
yr_choices <- sort(years, decreasing = T)

mons <- c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec','ann')
dt_type <- c( 'ano', 'clm') # Anomaly or climatology

## Extreme indices data files  -----
list.files(path = extrm_indx_dt_pth,
           pattern = "*.nc",
           full.names = T) -> extrm_indx_mtdt_dt_fls
extrm_indx_mtdt_dt_fls
head( extrm_indx_mtdt_dt_fls)

extrm_indx_mtdt_dt_fl <- tibble(dt_pth = extrm_indx_mtdt_dt_fls)

extrm_indx_mtdt_dt_fl %<>%
  mutate(fl_nam = basename(dt_pth)) %<>%
  mutate(indx = str_extract(fl_nam, paste(indx[order(-nchar(indx))], collapse = "|")),
         mon = str_extract(extrm_indx_mtdt_dt_fls,
                           paste(mons, collapse = "|")),
         dt_type = str_extract(extrm_indx_mtdt_dt_fls,
                           paste(dt_type, collapse = "|")))
extrm_indx_mtdt_dt_fl

# Merge data path with meta data file
extrm_indx_mtdt_dt_fl <- full_join(extrm_indx_mtdt_dt_fl,extrm_indx_mtdt)
extrm_indx_mtdt_dt_fl

#  UI ----
ui <- fluidPage(
  navbarPage(
    id = "bc_extrm",
    title = "BC Climate Extremes",
    theme = "bcgov.css",

    ## Intro page ----
    tabPanel(
      title = "Introduction",
      value = "intro",
      column(
        width = 12,
        wellPanel(
          HTML(
            "<h3><b>BC climate extremes app</b>: Visualizing changing climate extremes in British Columbia (BC) </h2>"
          )),
          includeMarkdown("intro_bc_climate_extremes_app.Rmd"),
        column(
          width = 12,
          HTML(
            "<h4><b>Citation</b></h4>
                            <h5> <u>Please cite the contents of this app as:</u>
                            <br>
                            Sharma, A.R. 2023. BC climate extremes app: Visualizing monthly and annual climate extremes in British Columbia (BC).</a>
                            British Columbia Ministry of Forests.
                  <a href='https://bcgov-env.shinyapps.io/bc_climate_anomaly/'
            target='_blank'>https://bcgov-env.shinyapps.io/bc_climate_anomaly/</a> </h5>"
          )
        ),
        column(
          width = 12,
          HTML(
            "<h5> <u>App created by:</u>
             <br>
             <b>Aseem R. Sharma, PhD</b><br>
              Research Climatologist<br>
              FFEC, FCCSB, OCF, BC Ministry of Forests<br>
              <a href= 'mailto: Aseem.Sharma@gov.bc.ca'>Aseem.Sharma@gov.bc.ca</a> <br>
              <br>
              <h4><b>Code</b></h4>
              <h5> The code and data of this app are available through GitHub at <a href='https://github.com/aseemsharma1/bc_climate_extremes.git'> https://github.com/aseemsharma1/bc_climate_extremes.git.</a></h5>"
          )
        ),
        column(width = 12,
               textOutput("deploymentDate"),),
        ###### footer ----
        column(
          width = 12,
          style = "background-color:#003366; border-top:2px solid #fcba19;",
          column(
            width = 12,
            style = "background-color:#003366; border-top:2px solid #fcba19;",
            tags$footer(
              class = "footer",
              tags$div(
                class = "container",
                style = "display:flex; justify-content:center; flex-direction:column; text-align:center; height:46px;",
                tags$ul(
                  style = "display:flex; flex-direction:row; flex-wrap:wrap; margin:0; list-style:none; align-items:center; height:100%;",
                  tags$li(
                    a(href = "https://www2.gov.bc.ca/gov/content/home", "Home", style = "font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")
                  ),
                  tags$li(
                    a(href = "https://www2.gov.bc.ca/gov/content/home/disclaimer", "Disclaimer", style =
                        "font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")
                  ),
                  tags$li(
                    a(href = "https://www2.gov.bc.ca/gov/content/home/privacy", "Privacy", style =
                        "font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")
                  ),
                  tags$li(
                    a(href = "https://www2.gov.bc.ca/gov/content/home/accessibility", "Accessibility", style =
                        "font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")
                  ),
                  tags$li(
                    a(href = "https://www2.gov.bc.ca/gov/content/home/copyright", "Copyright", style =
                        "font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")
                  ),
                  tags$li(
                    a(href = "https://www2.gov.bc.ca/StaticWebResources/static/gov3/html/contact-us.html", "Contact", style =
                        "font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")
                  )
                )
              )
            )
          )
        )
      )
    ),

    ## App page ----
    tabPanel(
      title = "Climate Extremes App",
      sidebarLayout(
        sidebarPanel(
          shinyjs::useShinyjs(),
          id = "selection-panel",
          # style = "position:fixed; width:24%; max-height: 100vh;",
          width = 3,
          ##### filters -----
          helpText(HTML("<h4><b> Filter/Selections</b> </h4>",)),
          fluidRow(
            useShinyjs(),
            pickerInput(
              "major_area",
              "Select region ",
              choices = c("Western North America",
                          "BC", "Ecoprovinces","Ecoregions","Ecosections", "Watersheds", "Municipalities")
            ),
            hidden(
              pickerInput(
                "ecoprov_area",
                "Ecoprovinces",
                choices = c("all_ecoprovinces", c(bc_ecoprv_shp$name)),
                multiple = F
              )
            ),
            hidden(
              pickerInput(
                "ecorgn_area",
                "Ecoregions",
                choices = c("all_ecoregions", c(bc_ecorgns_shp$CRGNNM)),
                multiple = F
              )
            ),
            hidden(
              pickerInput(
                "ecosec_area",
                "Ecosections",
                choices = c("all_ecosections", c(bc_ecosec_shp$ECOSEC_NM)),
                multiple = F
              )
            ),
            hidden(
              selectInput(
                "wtrshd_area",
                "Watersheds",
                choices = c("all_watersheds", c(bc_wtrshd_shp$MJR_WTRSHM)),
                multiple = F
              )
            ),
            hidden(
              pickerInput(
                "muni_area",
                "Municipalities",
                choices = c("all_municipalities", c(bc_muni_shp$ABRVN)),
                multiple = F
              )
            ),
          HTML("(Western North America, BC, Eco-provinces/regions/sections, Watersheds, Municipalities)"),
          ),
          br(),
          fluidRow(offset = 3,
                   # div(style = "height:70px;width:100%;background-color: #999999;border-style: dashed;border-color: #000000",),
                   uiOutput("indx_picker")),
          br(),
          fluidRow(title = "Month",
                   uiOutput("month_picker")),
          helpText(HTML("Extreme indices may not always have monthly or annual data available.
                        If no data exists, an error with <i> filename is empty </i>  occur.",)),
          br(),
          fluidRow(
            helpText(HTML("<h5><b> Choose range of years or specific year(s)</b> </h5>",)),
            actionButton("rng_years_choose", "Range of years"),
            actionButton("ab_years_choose", "Specific year(s)"),
           sliderInput(
              "year_range",
              "year range",
              min_year,
              max_year
              ,
              value = c((max_year - 5), (max_year)),
              sep = ""
            ),
            chooseSliderSkin(skin = "Shiny"),
            tags$style(
              HTML(
                ".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: purple}"
              )
            ),
            hidden(selectInput("year_specific",
                        "year(s)",
                        choices = yr_choices,
                        multiple = T,
                        selected = max_year)),
            # Reset selection
            br(),
            actionButton("reset_input", "Reset"),
            # actionButton("execute_app", "Execute")
          ),
          fluidRow(column(
            HTML("<h4><b>Location Map</b> </h4>"),
            title = "Map Location",
            width = 12,
            leafletOutput("loc_map", height = "22vh")
          )),
          br(),
        ),
        mainPanel(
          tags$head(tags$style(HTML(
            '.box {margin: 25px;}'
          ))),
          width = 9,
          ##### Anomaly map plot ------
                   column(width = 12,
                 wellPanel(
                   HTML("<h4><b> Climate extreme index definition</b> </h4>"),
                   uiOutput("indx_def_text")
                 )),
          column(width = 12,
                 wellPanel(
                   HTML("<h4><b> Climate extreme index anomaly map</b> </h4>")
                 )),
          fluidRow(column(
            width = 12,
            box(
              width = 12,
              plotOutput("ano_map", height = "70vh"),
              downloadButton(outputId = "download_ano_plt",
                             label = "Download plot"),
              downloadButton(outputId = "download_ano_data",
                             label = "Download raster data"),
            )
          )),
          ###### climate normal map and anomalies overview ----
          fluidRow(
            box(
              width = 6,
              align="left",
              wellPanel(HTML(
                "<h5><b>Climate extreme indices climatology (1981-2010)</b> </h5>"
              )),
              uiOutput("clm_nor_title", height = "30vh"),
              plotOutput("clm_nor_map", width = "100%", height = "30vh"),
              downloadButton(outputId = "download_clm_nor_plt",
                             label = "Download plot"),
              downloadButton(outputId = "download_clm_nor_data",
                             label = "Download raster data"),
            ),
            box(
              width = 6,
              status = "primary",
              wellPanel(HTML("<h5><b>Anomalies Overview</b> </h5>")),
              uiOutput("anomaly_overview", height = "30vh"),
              br(),
              DTOutput('ano_ovr_tbl')
            )
          ),
          ##### trend plots ----
          column(width = 12,
                 wellPanel(
                   HTML("<h4><b>Time-series and trend </b> </h4>")
                 )),
          fluidRow(column(
            width = 12,
            offset = 0.5,
            tabBox(
              width = 12,
              tabPanel(
                status = 'primary',
                title = "Time-series plot",
                plotlyOutput("spatial_trn_plt", height = "60vh"),
                downloadButton(outputId = "download_avtrn_plt",
                               label = "Download plot"),
                downloadButton(outputId = "download_ano_ts_data",
                               label = "Download anomaly time series data"),
              ),
              # tabPanel(
              #   status = 'primary',
              #   title = "Timeseries line plot",
              #   echarts4rOutput("echarts_trn_plt", height = "50vh")
              # )
            )
          )),
        ),
      ),
      ##### footer -----
      HTML("<br>",
           "<br>"),
      column(
        width = 12,
        style = "background-color:#003366; border-top:2px solid #fcba19;position:relative;",
        tags$footer(
          class = "footer",
          tags$div(
            class = "container",
            style = "display:flex; justify-content:center; flex-direction:column; text-align:center; height:46px;",
            tags$ul(
              style = "display:flex; flex-direction:row; flex-wrap:wrap; margin:0; list-style:none; align-items:center; height:100%;",
              tags$li(
                a(href = "https://www2.gov.bc.ca/gov/content/home", "Home", style = "font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")
              ),
              tags$li(
                a(href = "https://www2.gov.bc.ca/gov/content/home/disclaimer", "Disclaimer", style =
                    "font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")
              ),
              tags$li(
                a(href = "https://www2.gov.bc.ca/gov/content/home/privacy", "Privacy", style =
                    "font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")
              ),
              tags$li(
                a(href = "https://www2.gov.bc.ca/gov/content/home/accessibility", "Accessibility", style =
                    "font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")
              ),
              tags$li(
                a(href = "https://www2.gov.bc.ca/gov/content/home/copyright", "Copyright", style =
                    "font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")
              ),
              tags$li(
                a(href = "https://www2.gov.bc.ca/StaticWebResources/static/gov3/html/contact-us.html", "Contact", style =
                    "font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")
              )
            )
          )
        )
      )
    ),

    ## Feedback and links ----
    tabPanel(
      title = "Feedback & Links",
      value = "feed_link",
      column(width = 12,
             wellPanel(HTML(
               "<h3><b>Feedback</h3>"
             )), fluidRow(
               box(
                 width = 12,
                 status = 'primary',
                 # title = "Note",
                 uiOutput("feedback_text"),
               )
             )),
      column(
        width = 12,
        wellPanel(HTML("<h4><b>Links to other app </h4>")),
        HTML(
          "<h5><b>Here are the links to other apps developed in FFEC.</b></h5>

          <a href= 'https://bcgov-env.shinyapps.io/bc_climate_anomaly/'> bc_climate_anomaly app </a>
           <br>
          <a href= 'https://bcgov-env.shinyapps.io/cmip6-BC/'> CMIP6-BC </a>
                               <br>
          <br>"
        )
      ),
      ###### footer ----
      column(
        width = 12,
        style = "background-color:#003366; border-top:2px solid #fcba19;",
        tags$footer(
          class = "footer",
          tags$div(
            class = "container",
            style = "display:flex; justify-content:center; flex-direction:column; text-align:center; height:46px;",
            tags$ul(
              style = "display:flex; flex-direction:row; flex-wrap:wrap; margin:0; list-style:none; align-items:center; height:100%;",
              tags$li(
                a(href = "https://www2.gov.bc.ca/gov/content/home", "Home", style = "font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")
              ),
              tags$li(
                a(href = "https://www2.gov.bc.ca/gov/content/home/disclaimer", "Disclaimer", style =
                    "font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")
              ),
              tags$li(
                a(href = "https://www2.gov.bc.ca/gov/content/home/privacy", "Privacy", style =
                    "font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")
              ),
              tags$li(
                a(href = "https://www2.gov.bc.ca/gov/content/home/accessibility", "Accessibility", style =
                    "font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")
              ),
              tags$li(
                a(href = "https://www2.gov.bc.ca/gov/content/home/copyright", "Copyright", style =
                    "font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")
              ),
              tags$li(
                a(href = "https://www2.gov.bc.ca/StaticWebResources/static/gov3/html/contact-us.html", "Contact", style =
                    "font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")
              )
            )
          )
        )
      )
    )
  )
)


# Define server and reactive contents from data

# Server ----
server <- function(session, input, output) {
  options(warn = -1)

  # Maps and plots tab ----
  # Filters ------
  # # Filter : Area

  observeEvent(input$major_area, {
    if (input$major_area == "Ecoprovinces") {
      showElement("ecoprov_area")
      hideElement("wtrshd_area")
      hideElement("ecorgn_area")
      hideElement("ecosec_area")
      hideElement("muni_area")
    } else if (input$major_area == "Ecoregions"){
      showElement("ecorgn_area")
      hideElement("ecoprov_area")
      hideElement("wtrshd_area")
      hideElement("ecosec_area")
      hideElement("muni_area")
    } else if (input$major_area == "Ecosections"){
      showElement("ecosec_area")
      hideElement("ecorgn_area")
      hideElement("ecoprov_area")
      hideElement("wtrshd_area")
      hideElement("muni_area")
    } else if (input$major_area == "Watersheds") {
      showElement("wtrshd_area")
      hideElement("ecosec_area")
      hideElement("ecorgn_area")
      hideElement("ecoprov_area")
      hideElement("muni_area")
    } else if (input$major_area == "Municipalities") {
      showElement("muni_area")
      hideElement("ecosec_area")
      hideElement("ecorgn_area")
      hideElement("ecoprov_area")
      hideElement("wtrshd_area")
    } else {
      hideElement("muni_area")
      hideElement("ecosec_area")
      hideElement("ecorgn_area")
      hideElement("ecoprov_area")
      hideElement("wtrshd_area")
    }
  })

  # Area shape file interactive

  get_shapefile <- reactive({
    if (input$major_area == "BC") {
      sel_area_shpfl <- bc_shp
    } else if (input$major_area == "Western North America") {
      sel_area_shpfl <- wna_shp
    } else if (input$major_area == "Ecoprovinces") {
      bc_ecoprv_shp %>%
        filter(name == input$ecoprov_area) -> sel_area_shpfl
      if (input$ecoprov_area == "all_ecoprovinces")
        sel_area_shpfl <- bc_shp
    } else if (input$major_area == "Ecoregions") {
      bc_ecorgns_shp %>%
        filter(CRGNNM == input$ecorgn_area) -> sel_area_shpfl
      if (input$ecorgn_area == "all_ecoregions")
        sel_area_shpfl <- bc_shp
    }  else if (input$major_area == "Ecosections") {
      bc_ecosec_shp %>%
        filter(ECOSEC_NM == input$ecosec_area) -> sel_area_shpfl
      if (input$ecosec_area == "all_ecosections")
        sel_area_shpfl <- bc_shp
    } else if (input$major_area == "Watersheds") {
      bc_wtrshd_shp %>%
        filter(MJR_WTRSHM == input$wtrshd_area) -> sel_area_shpfl
      if (input$wtrshd_area == "all_watersheds")
        sel_area_shpfl <- bc_shp
    } else if(input$major_area == "Municipalities"){
      bc_muni_shp %>%
        filter(ABRVN == input$muni_area) -> sel_area_shpfl
      if (input$muni_area == "all_municipalities")
        sel_area_shpfl <- bc_shp
    }
    sel_area_shpfl
  })

  # Get regions name
  get_region <- reactive({
    if (input$major_area == "BC") {
      region = "BC"
    } else if (input$major_area == "Western North America") {
      region = "Western North America"
    } else if (input$major_area == "Ecoprovinces") {
      region = input$ecoprov_area
    } else if (input$major_area == "Ecoregions") {
      region = input$ecorgn_area
    } else if (input$major_area == "Ecosections") {
      region = input$ecosec_area
    } else if (input$major_area == "Watersheds") {
      region =  input$wtrshd_area
    }else if (input$major_area == "Municipalities") {
      region =  input$muni_area
    }
  })


  # Filter : parameter

  output$indx_picker <- renderUI({
    indx_choices <- indx
    indx_choices <- indx_choice_list

    pickerInput(
      "indx_picker",
      "Select Climate Extremes Index ",
      choices = indx_choices,
      choicesOpt = indx_item_design_list,
      selected = 'su'
    )
  })

  # Filter : month
  output$month_picker <- renderUI({
    mon_choices <- months_nam
    mon_choices <- list(
      "Annual" = 'ann',
      "January" = 'Jan',
      "February" = 'Feb',
      "March" = 'Mar',
      "April" = 'Apr',
      "May" = 'May',
      "June" = 'Jun',
      "July" = 'Jul',
      "August" = 'Aug',
      "September" = 'Sep',
      "October" = 'Oct',
      "November" = 'Nov',
      "December" = 'Dec'
    )
    pickerInput(
      "month_picker",
      "Select monthly or annual period"
      ,
      choices = mon_choices,
      selected = "ann"
    )
  })

  #interactive years choices

  whichInput <- reactiveValues(type = "range")


  observeEvent(input$rng_years_choose, {

    showElement("year_range")
    hideElement("year_specific")
    whichInput$type <- "range"

  })

  observeEvent(input$ab_years_choose, {
    showElement("year_specific")
    hideElement("year_range")
    whichInput$type <- "specific"

  })


  # get values for naming etc ------
  get_years <- reactive({
    if (whichInput$type == "specific") {
      sel_yrs <- input$year_specific
    } else{
      sel_yrs <- seq(input$year_range[1], input$year_range[2], 1)
    }
  })

  # Get unit interactively
  # Units
  get_unit <- reactive({
    req(input$indx_picker)

    unt1 <-   extrm_indx_mtdt%>%
      filter(indx == input$indx_picker)
    unt1
    unt <- unt1$Units
    unt
  })

  # Get parameter name full
  get_par_full <-  reactive({
    req(input$indx_picker)

    exindx_full1 <-   extrm_indx_mtdt%>%
      filter(indx == input$indx_picker)
    exindx_full1
    exindx_full <- paste0(exindx_full1$indx_long_name, ' (',exindx_full1$indx,')')
    exindx_full
     })

# Get months name full
  get_mon_full <-  reactive({
    req(input$month_picker)
    if (input$month_picker == 'ann') {
      mon_full = "Annual"
    } else if (input$month_picker == 'spring') {
      mon_full = "spring"
    } else if (input$month_picker == 'summer') {
      mon_full = "summer"
    } else if (input$month_picker == 'fall') {
      mon_full = "fall"
    } else if (input$month_picker == 'winter') {
      mon_full = "winter"
    } else if (input$month_picker == 'Jan') {
      mon_full = "January"
    } else if (input$month_picker == 'Feb') {
      mon_full = "February"
    } else if (input$month_picker == 'Mar') {
      mon_full = "March"
    } else if (input$month_picker == 'Apr') {
      mon_full = "April"
    } else if (input$month_picker == 'May') {
      mon_full = "May"
    } else if (input$month_picker == 'Jun') {
      mon_full = "June"
    } else if (input$month_picker == 'Jul') {
      mon_full = "July"
    } else if (input$month_picker == 'Aug') {
      mon_full = "August"
    } else if (input$month_picker == 'Sep') {
      mon_full = "September"
    } else if (input$month_picker == 'Oct') {
      mon_full = "October"
    } else if (input$month_picker == 'Nov') {
      mon_full = "November"
    } else if (input$month_picker == 'Dec') {
      mon_full = "December"
    }
    mon_full
  })

   # Indices definition text ----

  reactive_extrm_indx_def <- reactive({
    req(input$indx_picker)
    req(input$month_picker)
    req(input$year_range)

    extrm_indx_mtdt %>%
      dplyr::filter(indx == input$indx_picker) -> extrm_indx_def

    if(extrm_indx_def$Timescale == "Ann"){
      timeperiod <- 'annual'
    } else if(extrm_indx_def$Timescale == "Mon"){
      timeperiod <- 'monthly'
    } else {
      timeperiod <- 'annual and monthly'
    }

    extrm_indx_def_txt <-
      HTML(paste0("<b>",extrm_indx_def$indx_long_name,' (',extrm_indx_def$indx_short_name,') : ', "</b>",
                  'The extreme index ',get_par_full(), ' is described as ', extrm_indx_def$description,
             '. It is available at ', timeperiod, ' timescale with unit ', extrm_indx_def$Units,'.'))
  })

 output$indx_def_text <- renderUI({

   reactive_extrm_indx_def()
  })


  # Spatial anomaly data : reactive to selection
  reactive_extrm_indx_dt_fl <- reactive({
    req(input$indx_picker)
    req(input$month_picker)
    req(input$year_range)

    extrm_indx_mtdt_dt_fl %>%
      dplyr::filter(mon == input$month_picker &
               indx == input$indx_picker) -> extrm_indx_mtdt_dt_fl_mon

    #Anomaly data path
    extrm_indx_mtdt_dt_fl_mon_ano <- extrm_indx_mtdt_dt_fl_mon%>%
      dplyr::filter(dt_type =='ano')

    # extrm_indx_mtdt_dt_fl_mon_clm <- extrm_indx_mtdt_dt_fl_mon%>%
    #   dplyr::filter(dt_type =='clm')

    # extrm_indx_mtdt_dt_fl %>%
    #   dplyr::filter(mon == 'ann' &
    #            indx == 'txx') -> extrm_indx_mtdt_dt_fl_mon

    ano_dt_sel_rast <- rast(extrm_indx_mtdt_dt_fl_mon_ano$dt_pth)
    ano_dt_sel_rast
    # plot(ano_dt_sel_rast)

    # clm_dt_sel_rast <- rast(extrm_indx_mtdt_dt_fl_mon_clm$dt_pth)
    # clm_dt_sel_rast
    # plot(clm_dt_sel_rast)

    # Clip by shapefile of the selected area
    #browser()
    sel_area_shpfl <- get_shapefile()
    #browser()

# For sample run ----
#
# monn <- 'Mar'
# exindx <- 'spei12'
# sel_yrs <- seq(1979,2023,1)
# sel_yrs
# sel_area_shpfl <- bc_shp
# sel_area_shpfl
# bc_wtrshd_shp %>%
#   filter(MJR_WTRSHM == 'Quesnel River') -> sel_area_shpfl
# plot(st_geometry(sel_area_shpfl))
# region = "QR"
# extrm_indx_mtdt_dt_fl %>%
#   dplyr::filter(mon == monn & indx == exindx ) -> extrm_indx_mtdt_dt_fl_mon
# extrm_indx_mtdt_dt_fl_mon
#
# #Anomaly data path
# extrm_indx_mtdt_dt_fl_mon_ano <- extrm_indx_mtdt_dt_fl_mon%>%
#   filter(dt_type =='ano')
# extrm_indx_mtdt_dt_fl_mon_clm <- extrm_indx_mtdt_dt_fl_mon%>%
#   filter(dt_type =='clm')
# ano_dt_sel_rast <- rast(extrm_indx_mtdt_dt_fl_mon_ano$dt_pth)
# ano_dt_sel_rast
# terra::plot(ano_dt_sel_rast,30:nlyr(ano_dt_sel_rast))

####

# The spatial plot -------------------
    ano_dt_sel_rast <-
      terra::crop(ano_dt_sel_rast, sel_area_shpfl, mask = T)
    ano_dt_sel_rast
    # plot(ano_dt_sel_rast)
    names(ano_dt_sel_rast)
    terra::time(ano_dt_sel_rast) <-   as.integer(time(ano_dt_sel_rast))
    time(ano_dt_sel_rast)
   # Filter for selected year (s)
     sel_yrs <- get_years()

    if (length(sel_yrs) > 30) {
      sel_yrs <- sel_yrs[1:30]
      shinyalert(
        html = T,
        text = tagList(h3(
          "Too many years selected, maximum 30 allowed."
        )),
        showCancelButton = T
      )
    }

     yr_df <- tibble(paryr = time(ano_dt_sel_rast))
     yr_df %<>%
       mutate(yr = as.numeric(str_extract(paryr, "[0-9]+")))
     names(ano_dt_sel_rast) <- yr_df$yr
     # sel_yrs <- seq(1979,1985,1)
     ano_dt_fil_rast <-  subset(ano_dt_sel_rast, which(names(ano_dt_sel_rast) %in% sel_yrs))
     ano_dt_fil_rast
     # plot(ano_dt_fil_rast)
  })

  # Create reactive anomaly plot for display and save ------
  sp_ano_plt_rct <- reactive({
    req(input$indx_picker)
    req(input$month_picker)
    req(input$year_range)

    # other requirements
    monn = input$month_picker
    exindx = input$indx_picker

    # exindx = 'txx'

    # Clip by shapefile of the selected area
    sel_area_shpfl <- get_shapefile()

    reactive_extrm_indx_dt_fl() -> ano_dt_rast

    #Sample
    # ano_dt_rast <-ano_dt_fil_rast

    # # Import climatology and calculate % anomaly for precipitation
    # names(ano_dt_rast)
    #
    # clm_dt_sel_rast <- rast(extrm_indx_mtdt_dt_fl_mon_clm$dt_pth)
    # # plot(clm_dt_sel_rast)
    # names(clm_dt_sel_rast) <- extrm_indx_mtdt_dt_fl_mon_clm$mon
    #
    # # Select for input month
    # clm_dt_sel_rast_mon <-
    #   subset(clm_dt_sel_rast, which(names(clm_dt_sel_rast) %in% monn))
    # clm_dt_sel_rast <- clm_dt_sel_rast_mon
    # rm(clm_dt_sel_rast_mon)

    # # Crop to shpfile
    # clm_dt_rast <-
    #   terra::crop(clm_dt_sel_rast, sel_area_shpfl, mask = T)
    # # plot(clm_dt_rast)
    # # plot(ano_dt_rast)

    ano_rng_lmt <- terra::minmax(ano_dt_rast, compute = T)
    minval <- (-1) * (max(abs(ano_rng_lmt), na.rm = T))
    maxval <- (1) * (max(abs(ano_rng_lmt), na.rm = T))

    # Breaks and labels
    brk_neg <-
      ceiling(c(seq(minval, 0, length.out = 4)))
    brk_pos <-
      floor(c(seq(0, maxval, length.out = 4)))[-1]

    #create breaks with "00"

    if (nchar(abs(brk_neg[[1]])) == 4) {
      brk_negn <- plyr::round_any(brk_neg, 100, f = ceiling)
    } else if (nchar(abs(brk_neg[[1]])) == 3) {
      brk_negn <- plyr::round_any(brk_neg, 10, f = ceiling)
    } else if (nchar(abs(brk_neg[[1]])) == 2) {
      brk_negn <- plyr::round_any(brk_neg, 1, f = ceiling)
    } else if (nchar(abs(brk_neg[[1]])) == 1) {
      brk_negn <- plyr::round_any(brk_neg, 1, f = ceiling)
    }
    brk_negn

    if (nchar(abs(brk_neg[[1]])) == 4) {
      brk_posp <- plyr::round_any(brk_pos, 100, f = floor)
    } else if (nchar(abs(brk_neg[[1]])) == 3) {
      brk_posp <- plyr::round_any(brk_pos, 10, f = floor)
    } else if (nchar(abs(brk_pos[[1]])) == 2) {
      brk_posp <- plyr::round_any(brk_pos, 1, f = floor)
    } else if (nchar(abs(brk_pos[[1]])) == 1) {
      brk_posp <- plyr::round_any(brk_pos, 1, f = floor)
    }
    brk_posp

    brks_seq <- c(brk_negn, brk_posp)
    labels_val <- c(
      paste0("<", brks_seq[[1]]),
      brks_seq[[2]],
      brks_seq[[3]],
      brks_seq[[4]],
      brks_seq[[5]],
      brks_seq[[6]],
      paste0(">", brks_seq[[7]])
    )
    labels_val

    # Plot using terra rast

    # Climate plot title ( use log for prcp)
      par_title <-  paste0(get_region(), " ",
                           get_par_full(), " anomaly (", get_unit(),")",
                           ": ",
                           get_mon_full())
      par_title

    xlim <- c(-140,-113.0)
    ylim <- c(45,61)

   spatial_ano_plt <-  ggplot() +
      geom_spatraster(data = ano_dt_rast) +
      scale_fill_gradientn(
        name = paste0(indx, " anomaly ", get_unit()),
        colours = cpt(pal = "ncl_BlWhRe",
                      n = 100,
                      rev = F),
        na.value = "transparent",
        limits = c(minval, maxval),
        breaks = brks_seq
      ) +
      facet_wrap(. ~ lyr) +
      geom_sf(
        data = sel_area_shpfl,
        colour = "black",
        size = 1,
        fill = NA,
        alpha = 0.8
      ) +
     # coord_sf(xlim = xlim, ylim = ylim)+
      scale_x_continuous(
        name =  "Longitude (°W) ",
        breaks = seq(xmi - 5, xmx + 5, 10),
        labels = abs,
        expand = c(0.01, 0.01)
      ) +
      scale_y_continuous(
        name = "Latitude (°N) ",
        # breaks = seq((ymi - 1), (ymx + 1), 6),
        # labels = abs,
        expand = c(0.01, 0.01)
      ) +
      theme(
        panel.spacing = unit(0.1, "lines"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(
          color = "gray60",
          linewidth = 0.02,
          linetype = "dashed"
        ),
        axis.line = element_line(colour = "gray70", linewidth = 0.08),
        axis.ticks.length = unit(-0.20, "cm"),
        element_line(colour = "black", linewidth =  1),
        axis.title.y = element_text(
          angle = 90,
          face = "plain",
          size = 15,
          colour = "Black",
          margin = unit(c(-1, -1, -1, -1), "mm")
        ),
        axis.title.x = element_text(
          angle = 0,
          face = "plain",
          size = 15,
          colour = "Black",
          margin = unit(c(-1, -1, -1, -1), "mm")
        ),
        axis.text.x = element_text(
          angle = 0,
          hjust = 0.5,
          vjust = 0.5,
          colour = "black",
          size = 14,
          margin = margin(
            t = 2,
            r = 2,
            b = 2,
            l = 2
          )
        ),
        axis.text.y = element_text(
          angle = 90,
          hjust = 0.5,
          vjust = 0.5,
          colour = "black",
          size = 14,
          margin = margin(
            t = 2,
            r = 2,
            b = 2,
            l = 2
          )
        ),
        plot.title = element_text(
          angle = 0,
          face = "bold",
          size = 13,
          colour = "Black"
        ),
        legend.position = 'right',
        legend.direction = "vertical",
        legend.margin = margin(0, 0, 0, 0),
        legend.box.margin = margin(-5, -5, -5, -5),
        legend.title = element_text(size = 15),
        legend.text = element_text(margin = margin(t = -5), size = 16),
        strip.text.x = element_text(size = 12, angle = 0),
        strip.text.y = element_text(size = 12, face = "bold"),
        axis.text = element_text(margin = -5),
        strip.background = element_rect(color = "black", fill = "gray90"),
        strip.text = element_text(
          face = "bold",
          size = 18,
          colour = 'black'
        )
      ) +
      guides(
        fill = guide_colorbar(
          barwidth = 1.7,
          barheight = 20,
          label.vjust = 0.5,
          label.hjust = 0.0,
          title.vjust = 0.5,
          title.hjust = 0.5,
          title = NULL,
          # title.position = NULL,
          ticks.colour = 'black',
          # ticks.linewidth = 1,
          frame.colour = 'black',
          # frame.linewidth = 1,
          # draw.ulim = FALSE,
          # draw.llim = TRUE,
        )
      ) +
      theme(
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()
      )

    spatial_ano_plt <- spatial_ano_plt +
      labs(tag = plt_wtrmrk, title = par_title,subtitle = "Baseline: 1981-2010") +
      theme(
        plot.tag.position = "bottom",
        plot.tag = element_text(
          color = 'gray50',
          hjust = 1,
          vjust = 0,
          size = 8
        )
      )
    spatial_ano_plt
    })

  # Anomaly map plot display ----
  output$ano_map <- renderPlot({
    sp_ano_plt_rct()
  })

  # Location map plot ----
  output$loc_map <- renderLeaflet({
    req(input$ecoprov_area,input$ecorgn_area,input$ecosec_area,
        input$muni_area, input$wtrshd_area, input$major_area)

    # Shape file for location
    sel_area_shpfl <- get_shapefile()

    if (input$wtrshd_area == "all_watersheds" &&
        input$major_area == "Watersheds") {
      sel_area_shpfl <- bc_wtrshd_shp['MJR_WTRSHM']

      leaflet(sel_area_shpfl) %>%
        addTiles() %>%
        addPolygons(
          layerId = ~ MJR_WTRSHM,
          popup = ~ MJR_WTRSHM,
          color = "Red",
          weight = 1,
          opacity = 1,
          fill = TRUE,
          fillOpacity = 0
        )

    } else if (input$ecoprov_area == "all_ecoprovinces" &&
               input$major_area == "Ecoprovinces") {
      sel_area_shpfl <- bc_ecoprv_shp['name']

      leaflet(sel_area_shpfl) %>%
        addTiles() %>%
        addPolygons(
          layerId = ~ name,
          popup = ~ name,
          color = "Red",
          weight = 1,
          opacity = 1,
          fill = TRUE,
          fillOpacity = 0
        )
    } else if (input$ecorgn_area == "all_ecoregions" &&
               input$major_area == "Ecoregions") {
      sel_area_shpfl <- bc_ecorgns_shp['CRGNNM']

      leaflet(sel_area_shpfl) %>%
        addTiles() %>%
        addPolygons(
          layerId = ~ CRGNNM,
          popup = ~ CRGNNM,
          color = "Red",
          weight = 1,
          opacity = 1,
          fill = TRUE,
          fillOpacity = 0
        )
    } else if (input$ecosec_area == "all_ecosections" &&
              input$major_area == "Ecosections") {
      sel_area_shpfl <- bc_ecosec_shp['ECOSEC_NM']

      leaflet(sel_area_shpfl) %>%
        addTiles() %>%
        addPolygons(
          layerId = ~ ECOSEC_NM,
          popup = ~ ECOSEC_NM,
          color = "Red",
          weight = 1,
          opacity = 1,
          fill = TRUE,
          fillOpacity = 0
        )
    }
    else if (input$muni_area == "all_municipalities" &&
               input$major_area == "Municipalities") {
      sel_area_shpfl <- bc_muni_shp['ABRVN']

      leaflet(sel_area_shpfl) %>%
        addTiles() %>%
        addPolygons(
          layerId = ~ ABRVN,
          popup = ~ ABRVN,
          color = "Red",
          weight = 1,
          opacity = 1,
          fill = TRUE,
          fillOpacity = 0
        )
    } else{
      leaflet(sel_area_shpfl) %>%
        addTiles() %>%
        addPolygons(
          color = "Red",
          weight = 1,
          opacity = 1,
          fill = TRUE,
          fillOpacity = 0
        )
    }

  })

  observeEvent(input$loc_map_shape_click, {
    nm <- input$loc_map_shape_click$id
    print(nm)
    if (input$major_area == "Ecoprovinces") {
      updatePickerInput(session, "ecoprov_area", selected = nm)
    } else if (input$major_area == "Ecoregions") {
      updateSelectInput(session, "ecorgn_area", selected = nm)
    } else if (input$major_area == "Ecosections") {
      updateSelectInput(session, "ecosec_area", selected = nm)
    } else if (input$major_area == "Watersheds") {
      updateSelectInput(session, "wtrshd_area", selected = nm)
    } else if (input$major_area == "Municipalities") {
      updateSelectInput(session, "muni_area", selected = nm)
    }
  })

  # Climate normal plot ----
   ## Climate normal map title ----
  clm_plot_title_info <- reactive({
    req(input$indx_picker)
    req(input$month_picker)

    # other requirements
    monn = input$month_picker
    exindx = input$indx_picker

    sel_area_shpfl <- get_shapefile()

    clm_nor_title_txt <-  paste0(get_region(), " ",
                                   get_par_full(), " (average of  1981-2010) ", "(", get_unit(),")" ,
                                   " : ",
                                   get_mon_full())
    clm_nor_title_txt
  })

  output$clm_nor_title <- renderText({
    clm_plot_title_info()
  })

  # Climate normal data plot : for normal plot
  reactive_clm_dt_plt <- reactive({
    req(input$major_area)
    req(input$indx_picker)
    req(input$month_picker)
    req(input$year_range)

    # other requirements
    monn = input$month_picker
    exindx = input$indx_picker

    sel_area_shpfl <- get_shapefile()

    extrm_indx_mtdt_dt_fl %>%
      dplyr::filter(mon == input$month_picker &
                      indx == input$indx_picker) -> extrm_indx_mtdt_dt_fl_mon

    #Climatology data path
    extrm_indx_mtdt_dt_fl_mon_clm <- extrm_indx_mtdt_dt_fl_mon%>%
      dplyr::filter(dt_type =='clm')

    clm_dt_sel_rast <- rast(extrm_indx_mtdt_dt_fl_mon_clm$dt_pth)
    # plot(clm_dt_sel_rast)

    # clm_dt_fl %>%
    #   filter(par == "tmean") -> clm_dt_fl_par
    # clm_dt_sel_rast <- rast(clm_dt_fl_par$dt_pth)
    # clm_dt_sel_rast
    # sel_area_shpfl <- bc_shp

    names(clm_dt_sel_rast) <- monn
    clm_dt_sel_rast

    # Clip by shape file of the selected area

    clm_dt_sel_rast <-
      terra::crop(clm_dt_sel_rast, sel_area_shpfl, mask = T)
    # plot(clm_dt_sel_rast)

    mn_clm_val <-
      round(global(clm_dt_sel_rast, 'mean', na.rm = T), digits = 2)
    mi_clm_val <-
      round(global(clm_dt_sel_rast, 'min', na.rm = T), digits = 2)
    mx_clm_val <-
      round(global(clm_dt_sel_rast, 'max', na.rm = T), digits = 2)

    # Plot using terra rast

    spatial_clm_plt <-  ggplot() +
      geom_spatraster(data = clm_dt_sel_rast) +
      scale_fill_continuous(
        type = "viridis",
        name = " ",
        option = "inferno",
        direction = -1,
        na.value = "transparent"
      ) +
      geom_sf(
        data = sel_area_shpfl,
        colour = "black",
        size = 1,
        fill = NA,
        alpha = 0.8
      ) +
      # scale_x_continuous(
      #   name =  "Longitude (°W) ",
      #   breaks = seq(xmi - 5, xmx + 5, 10),
      #   labels = abs,
      #   expand = c(0.01, 0.01)
      # ) +
      # scale_y_continuous(
      #   name = "Latitude (°N) ",
      #   # breaks = seq(ymi - 1, ymx + 1, 1),
      #   labels = abs,
      #   expand = c(0.01, 0.01)
      # ) +
      theme(
        panel.spacing = unit(0.1, "lines"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(
          color = "gray60",
          linewidth = 0.02,
          linetype = "dashed"
        ),
        axis.line = element_line(colour = "gray70", linewidth = 0.08),
        axis.ticks.length = unit(-0.20, "cm"),
        element_line(colour = "black", linewidth =  1),
        axis.title.y = element_text(
          angle = 90,
          face = "plain",
          size = 15,
          colour = "Black",
          margin = unit(c(-1, -1, -1, -1), "mm")
        ),
        axis.title.x = element_text(
          angle = 0,
          face = "plain",
          size = 15,
          colour = "Black",
          margin = unit(c(-1, -1, -1, -1), "mm")
        ),
        axis.text.x = element_text(
          angle = 0,
          hjust = 0.5,
          vjust = 0.5,
          colour = "black",
          size = 14,
          margin = margin(
            t = 2,
            r = 2,
            b = 2,
            l = 2
          )
        ),
        axis.text.y = element_text(
          angle = 90,
          hjust = 0.5,
          vjust = 0.5,
          colour = "black",
          size = 14,
          margin = margin(
            t = 2,
            r = 2,
            b = 2,
            l = 2
          )
        ),
        plot.title = element_text(
          angle = 0,
          face = "bold",
          size = 15,
          colour = "Black"
        ),
        legend.position = 'right',
        legend.direction = "vertical",
        legend.margin = margin(0, 0, 0, 0),
        legend.box.margin = margin(-5, -5, -5, -5),
        legend.title = element_text(size = 15),
        legend.text = element_text(margin = margin(t = -5), size = 16),
        strip.text.x = element_text(size = 12, angle = 0),
        strip.text.y = element_text(size = 12, face = "bold"),
        axis.text = element_text(margin = -5),
        strip.background = element_rect(color = "black", fill = "gray90"),
        strip.text = element_text(
          face = "bold",
          size = 18,
          colour = 'black'
        )
      ) +
      guides(
        fill = guide_colorbar(
          barwidth = 1.0,
          barheight = 10,
          label.vjust = 0.5,
          label.hjust = 0.0,
          title.vjust = 0.5,
          title.hjust = 0.5,
          title = NULL,
          # title.position = NULL,
          ticks.colour = 'black',
          # ticks.linewidth = 1,
          frame.colour = 'black',
          # frame.linewidth = 1,
          # draw.ulim = FALSE,
          # draw.llim = TRUE,
        )
      ) +
      theme(
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()
      )
    spatial_clm_plt


    # Climate plot title ( use log for prcp)
    # if (exindx == "prcp") {
    #   par_title <-  paste0(get_region(), " mean ",
    #                        get_par_full(),"\n ","(", get_unit(),")" ," (log-scale)",
    #                        " : ",
    #                        get_mon_full())
    # } else{
    #   par_title <-  paste0(get_region(), " ",
    #                        get_par_full(), " ", "(", get_unit(),")" ,
    #                        " : ",
    #                        get_mon_full())
    # }
    spatial_clm_plt <- spatial_clm_plt +
      # labs(tag = plt_wtrmrk) +
      # theme(
      #   plot.tag.position = "bottom",
      #   plot.tag = element_text(
      #     color = 'gray50',
      #     hjust = 1,
      #     size = 8
      #   )
      # ) +
      labs(
        # title = par_title,
        subtitle = paste0(
          'Mean = ',
          mn_clm_val[[1]]," ",
          "(", get_unit(),")" ,
          "  ",
          "Range = ",
          "[",
          mi_clm_val[[1]],
          " - ",
          mx_clm_val[[1]],
          "]"
        )
      ) +
      theme(
        plot.title = element_text(size = 12, face = 'plain'),
        plot.subtitle = element_text(size = 10)
      )
    spatial_clm_plt
  })

  output$clm_nor_map <- renderPlot({
    reactive_clm_dt_plt()
  })
  # Climate normal data : for download
  reactive_clm_dt_fl <- reactive({
    req(input$major_area)
    req(input$indx_picker)
    req(input$month_picker)
    req(input$year_range)

    req(input$major_area)
    req(input$indx_picker)
    req(input$month_picker)
    req(input$year_range)

    # other requirements
    monn = input$month_picker
    exindx = input$indx_picker

    sel_area_shpfl <- get_shapefile()

    extrm_indx_mtdt_dt_fl %>%
      dplyr::filter(mon == input$month_picker &
                      indx == input$indx_picker) -> extrm_indx_mtdt_dt_fl_mon

    #Climatology data path
    extrm_indx_mtdt_dt_fl_mon_clm <- extrm_indx_mtdt_dt_fl_mon%>%
      dplyr::filter(dt_type =='clm')

    clm_dt_sel_rast <- rast(extrm_indx_mtdt_dt_fl_mon_clm$dt_pth)
    # plot(clm_dt_sel_rast)

    # clm_dt_fl %>%
    #   filter(par == "tmean") -> clm_dt_fl_par
    # clm_dt_sel_rast <- rast(clm_dt_fl_par$dt_pth)
    # clm_dt_sel_rast
    # sel_area_shpfl <- bc_shp

    names(clm_dt_sel_rast) <- monn
    clm_dt_sel_rast

    # Clip by shape file of the selected area

    clm_dt_sel_rast <-
      terra::crop(clm_dt_sel_rast, sel_area_shpfl, mask = T)
    # plot(clm_dt_sel_rast)
    # clm_dt_fl %>%
    #   filter(par == "tmean") -> clm_dt_fl_par
    # clm_dt_sel_rast <- rast(clm_dt_fl_par$dt_pth)
    # clm_dt_sel_rast
    # sel_area_shpfl <- bc_shp

    names(clm_dt_sel_rast) <- monn
    clm_dt_sel_rast

  })

  # Anomaly overview table caption text ----
  ano_overview_info <- reactive({
    req(input$indx_picker)
    req(input$month_picker)
    req(input$year_range)

    sel_area_shpfl <- get_shapefile()

    # other requirements
    monn = input$month_picker
    exindx = input$indx_picker

    # Years
    # Filter for selected year (s)
    if (input$ab_years_choose %% 2 == 0) {
      sel_yrs <- seq(input$year_range[1], input$year_range[2], 1)
    } else{
      sel_yrs <- input$year_specific
    }

    minyr <- min(sel_yrs)
    maxyr <- max(sel_yrs)

    ano_overview <-
      paste0(
        "The spatially averaged anomaly overview of ",
        get_region(),
        "'s ",
        get_mon_full(),
        " ",
        get_par_full(),
        " ",
        minyr,
        "—",
        maxyr,
        "."
      )
    ano_overview
  })

  output$anomaly_overview <- renderText({
    ano_overview_info()
  })

  # Anomaly overview  table ----
  output$ano_ovr_tbl <- renderDT({
    reactive_extrm_indx_dt_fl() -> ano_dt_rast
    sel_area_shpfl <- get_shapefile()

    # Background requirements
    monn = input$month_picker
    exindx = input$indx_picker

    #For percentage of precipitation anomaly
    names(ano_dt_rast)

    mn_ano <-
      terra::global(ano_dt_rast, fun = "mean", na.rm = T)
    mn_ano
    mn_ano <- round(mean(mn_ano$mean, na.rm = T), 2)
    mi_ano <- terra::global(ano_dt_rast, fun = "min", na.rm = T)
    mi_ano <- round(min(mi_ano$min, na.rm = T), 2)
    mx_ano <- terra::global(ano_dt_rast, fun = "max", na.rm = T)
    mx_ano <- round(max(mx_ano$max, na.rm = T), 2)

      mi_ano_val = paste0(mi_ano, ' ',get_unit())
      mn_ano_val = paste0(mn_ano, ' ',get_unit())
      mx_ano_val = paste0(mx_ano, ' ',get_unit())

    # Create a table
    ano_ovr_dt <-
      data.frame(
        "Anomaly" = c("Minimum", "Mean", "Maximum"),
        "Value" = c(mi_ano_val, mn_ano_val, mx_ano_val)
      )
    ano_ovr_dt
    datatable(
      ano_ovr_dt,
      rownames = FALSE,
      options = list(
        lengthChange = FALSE,
        info = FALSE,
        paging = FALSE,
        searching = FALSE
      )
    )

  })

  # Spatially averaged anomaly trend plot -----
  # clip the anomalies to selected shape file area and calculate spatially average values and trend
  # Reactive data
  reactive_extrm_indx_dt_fl_sp <- reactive({
    req(input$month_picker)
    req(input$indx_picker)
    req(input$major_area)

    extrm_indx_mtdt_dt_fl %>%
      dplyr::filter(mon == input$month_picker &
                      indx == input$indx_picker) -> extrm_indx_mtdt_dt_fl_mon

    # Anomaly data path
    extrm_indx_mtdt_dt_fl_mon_ano <- extrm_indx_mtdt_dt_fl_mon%>%
      dplyr::filter(dt_type =='ano')

    ano_dt_sel_rast <- rast(extrm_indx_mtdt_dt_fl_mon_ano$dt_pth)
    ano_dt_sel_rast
    # plot(ano_dt_sel_rast)

    # Clip by shapefile of the selected area
    sel_area_shpfl <- get_shapefile()

    ano_dt_sel_rast <-
      terra::crop(ano_dt_sel_rast, sel_area_shpfl, mask = T)
    ano_dt_sel_rast
    # plot(ano_dt_sel_rast)
    names(ano_dt_sel_rast)
    terra::time(ano_dt_sel_rast) <-   as.integer(time(ano_dt_sel_rast))
    time(ano_dt_sel_rast)
    names(ano_dt_sel_rast) <-  as.integer(time(ano_dt_sel_rast))

    # ano_dt_sel_rast <- ano_dt_fil_rast
    # plot(ano_dt_sel_rast)

      # other requirements
    monn = input$month_picker
    exindx = input$indx_picker

    ano_dt_shp_rast <- ano_dt_sel_rast
    # Shapefile spatial average anomalies by year
    ano_shp_av_dt <-
      tibble(rownames_to_column(global(
        ano_dt_shp_rast, fun = "mean", na.rm = T
      ), "yr")) %>%
      dplyr::select(yr, ano = mean)
    ano_shp_av_dt %<>%
      drop_na()
    ano_shp_av_dt$yr <-
      as.numeric(str_extract(ano_shp_av_dt$yr, "[0-9]+"))
    ano_shp_av_dt$indx <- exindx
    ano_shp_av_dt$mon <- monn
    ano_shp_av_dt$region <- get_region()
    ano_shp_av_dt
  })

  # ggplot2/plotly trend plot  ----
  spatial_av_trnd_plt_rct <- reactive({
    reactive_extrm_indx_dt_fl_sp() -> ano_shp_dt
    ano_shp_dt

    # ano_shp_dt <-  ano_shp_av_dt

    # Background requirements for plots
    exindx <- unique(ano_shp_dt$indx)
    monn <- unique(ano_shp_dt$mon)
    region <- unique(ano_shp_dt$region)

    # Trend on average anomaly 1950 - now
    ano_shp_dt %<>%
      filter(yr > 1978) %<>%
      mutate(# trnd =zyp.trend.vector(ano)[["trend"]],
        # incpt =zyp.trend.vector(ano)[["intercept"]],
        #sig = zyp.trend.vector(ano)[["sig"]])
        sig = round(MannKendall(ano)[[2]], digits = 2))
    ano_shp_dt

    ano_mk_trnd <-
      zyp.sen(ano ~ yr, ano_shp_dt)##Give the trend###
    ano_mk_trnd$coefficients
    ano_shp_dt$trn <-  ano_mk_trnd$coeff[[2]]
    ano_shp_dt$incpt <-  ano_mk_trnd$coeff[[1]]

    xs = c(min(ano_shp_dt$yr), max(ano_shp_dt$yr))
    trn_slp = c(unique(ano_shp_dt$incpt), unique(ano_shp_dt$trn))
    ys = cbind(1, xs) %*% trn_slp
    ano_shp_dt$trn_lab = paste(
      "italic(trend)==",
      round(ano_shp_dt$trn, 2),"~yr^{-1}~','~italic(p)==",
      round(ano_shp_dt$sig, 2)
    )

    # anomaly plot
    ymin <- (-1) * (max(abs(ano_shp_dt$ano)))
    ymax <- (1) * (max(abs(ano_shp_dt$ano)))
    minyr <- min(ano_shp_dt$yr)
    maxyr <- max(ano_shp_dt$yr)

    if(ymax < 1){
      ybrk_neg <-
        round(c(seq((-1) * (max(
          abs(ano_shp_dt$ano)
        )), 0, length.out = 2)), digits=2)
      ybrk_neg
      ybrk_pos <-
        round(c(seq(0, (1) * (max(
          abs(ano_shp_dt$ano)
        )), length.out = 2))[-1], digits=2)
      ybrk_pos
    } else {
    ybrk_neg <-
      ceiling(c(seq((-1) * (max(
        abs(ano_shp_dt$ano)
      )), 0, length.out = 4)))
    ybrk_neg
    ybrk_pos <-
      floor(c(seq(0, (1) * (max(
        abs(ano_shp_dt$ano)
      )), length.out = 4)))[-1]
    ybrk_pos
}
    #create breaks with "00"

    if (nchar(abs(ybrk_neg[[1]])) == 4) {
      ybrk_negn <- plyr::round_any(ybrk_neg, 100, f = ceiling)
    } else if (nchar(abs(ybrk_neg[[1]])) == 3) {
      ybrk_negn <- plyr::round_any(ybrk_neg, 10, f = ceiling)
    } else if (nchar(abs(ybrk_neg[[1]])) == 2) {
      ybrk_negn <- plyr::round_any(ybrk_neg, 1, f = ceiling)
    } else if (nchar(abs(ybrk_neg[[1]])) == 1) {
      ybrk_negn <- plyr::round_any(ybrk_neg, 1, f = ceiling)
    }
    ybrk_negn

    if (nchar(abs(ybrk_neg[[1]])) == 4) {
      ybrk_posp <- plyr::round_any(ybrk_pos, 100, f = floor)
    } else if (nchar(abs(ybrk_neg[[1]])) == 3) {
      ybrk_posp <- plyr::round_any(ybrk_pos, 10, f = floor)
    } else if (nchar(abs(ybrk_pos[[1]])) == 2) {
      ybrk_posp <- plyr::round_any(ybrk_pos, 1, f = floor)
    } else if (nchar(abs(ybrk_pos[[1]])) == 1) {
      ybrk_posp <- plyr::round_any(ybrk_pos, 1, f = floor)
    }
    ybrk_posp

    if(ymax < 1){
      ybrks_seq <- c(ybrk_neg, ybrk_pos)
    }else {
      ybrks_seq <- c(ybrk_negn, ybrk_posp)
      }
    ybrks_seq

    # Positive and negative anomalies and 3 years moving average to create bar plot
    ano_shp_dt %<>%
      mutate(pos_neg = if_else(ano <= 0, "neg", "pos")) %>%
      mutate(ano_mv = rollmean(ano, 3, fill = list(NA, NULL, NA)))
    ano_shp_dt
    tail(ano_shp_dt)

    par_title <-  paste0(get_region(), " ",
                           get_par_full(), " ", "anomaly"," (", get_unit(),")",
                           " : ",
                           get_mon_full())

   y_axis_lab <- paste0(exindx, " average anomaly ", "(", get_unit(), ")")

    ano_shp_trn_plt <-
      ggplot(data = ano_shp_dt, aes(x = yr, y = ano)) +
      annotate(
        geom = 'text',
        label = plt_wtrmrk,
        x = Inf,
        y = -Inf,
        hjust = 1,
        vjust = -0.5,
        color = 'gray80',
        size = 3.0
      ) +
      geom_bar(
        stat = "identity",
        aes(fill = ano),
        width = 0.7,
        show.legend = FALSE
      ) +
      geom_hline(
        yintercept = 0,
        color = "gray10",
        linewidth = 0.5
      ) +
      scale_fill_gradientn(
        name = paste0(exindx, " anomaly ", get_unit()),
        colours = cpt(pal = "ncl_BlWhRe",
                      n = 100,
                      rev = F),
        limits = c(ymin, ymax),
        breaks = ybrks_seq
      ) +
      geom_line(
        aes(y = ano_mv, color = "3-yrs moving mean"),
        linewidth = 1.1,
        alpha = 0.7,
        na.rm = T
      ) +
      # geom_point(color = "blue", size = 2) +
      geom_segment(
        aes(
          x = xs[[1]],
          xend = xs[[2]],
          y = ys[[1]],
          yend = ys[[2]],
          color = "trend"
        ),
        linetype = "dashed",
        linewidth = 0.9
      ) +
      geom_label(
        aes(x = xs[[1]] + 5),
        y = ymax - 0.05,
        fill = NA,
        color = 'deepskyblue2',
        label = ano_shp_dt$trn_lab[[1]],
        size = 4.0,
        parse = TRUE
      ) +
      scale_x_continuous(
        name = " ",
        breaks = seq(1980, maxyr, 2),
        expand = c(0.02, 0.02)
      ) +
      scale_y_continuous(name = y_axis_lab,
                         limits = c(ymin, ymax),
                         breaks = ybrks_seq) +
      labs(title = par_title, subtitle = "Baseline: 1981-2010") +
      scale_color_manual(
        " ",
        values = c(
          "3-yrs moving mean" = "green",
          "trend" = "deepskyblue2"
        ),
        labels =  c(
          "3-yrs moving mean" = "3-yrs moving mean",
          "trend" = "Trend"
        )
      ) +
      theme_bw() +
      theme(
        # panel.spacing=unit(0.1,"lines"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(
          color = "gray75",
          linewidth = 0.05,
          linetype = "dashed"
        ),
        axis.line = element_line(colour = "black", linewidth = 1),
        axis.ticks.length = unit(-0.20, "cm"),
        element_line(colour = "black", linewidth =  1),
        axis.title.y = element_text(
          angle = 90,
          face = "plain",
          size = 13,
          colour = "Black",
          margin = unit(c(1, 1, 1, 1), "mm")
        ),
        axis.title.x = element_text(
          angle = 0,
          face = "plain",
          size = 13,
          colour = "Black",
          margin = unit(c(1, 1, 1, 1), "mm")
        ),
        axis.text.x = element_text(
          angle = 0,
          hjust = 0.5,
          vjust = 0.5,
          colour = "black",
          size = 12,
          margin = margin(
            t = 2,
            r = 2,
            b = 2,
            l = 2
          )
        ),
        axis.text.y = element_text(
          angle = 90,
          hjust = 0.5,
          vjust = 0.5,
          colour = "black",
          size = 12,
          margin = margin(
            t = 2,
            r = 2,
            b = 2,
            l = 2
          )
        ),
        plot.title = element_text(
          angle = 0,
          face = "bold",
          size = 13,
          colour = "Black"
        ),
        legend.position = c(0.90, 0.94),
        legend.direction = "vertical",
        legend.background = element_rect(fill = NA, color = NA),
        legend.margin = margin(0, 0, 0, 0),
        legend.box.margin = margin(0, 0, 0, 0),
        legend.title = element_text(size = 13),
        legend.text = element_text(margin = margin(t = -5), size = 12),
        strip.text.x = element_text(size = 12, angle = 0),
        strip.text.y = element_text(size = 12, face = "bold"),
        axis.text = element_text(margin = -5),
        strip.background = element_rect(fill = "black"),
        strip.text = element_text(colour = 'Black')
      )
    ano_shp_trn_plt

    ano_shp_trn_plt<- ano_shp_trn_plt +
      theme(axis.title.y = element_blank())
    ano_shp_trn_plt
  })

  ## Plotly display ------
  output$spatial_trn_plt <- renderPlotly({
    spatial_av_trnd_plt_rct() -> ano_shp_trn_plt
    reactive_extrm_indx_dt_fl_sp() -> ano_shp_dt
    # Background requirements for plots
    exindx  <- unique(ano_shp_dt$indx)
    monn <- unique(ano_shp_dt$mon)
    region <- unique(ano_shp_dt$region)


    # Trend on average anomaly 1979 - now
    ano_shp_dt %<>%
      mutate(# trnd =zyp.trend.vector(ano)[["trend"]],
        # incpt =zyp.trend.vector(ano)[["intercept"]],
        #sig = zyp.trend.vector(ano)[["sig"]])
        sig = round(MannKendall(ano)[[2]], digits = 2))
    ano_shp_dt

    ano_mk_trnd <-
      zyp.sen(ano ~ yr, ano_shp_dt)##Give the trend###
    ano_mk_trnd$coefficients
    ano_shp_dt$trn <-  ano_mk_trnd$coeff[[2]]
    ano_shp_dt$incpt <-  ano_mk_trnd$coeff[[1]]

    ano_shp_dt$trn_lab = paste(
      "italic(trend)==",
      round(ano_shp_dt$trn, 2),
      "~','~italic(p)==",
      round(ano_shp_dt$sig, 2)
    )

  par_title <-  paste0(get_region(), " ",
                           get_par_full(), " ", "anomaly"," (", get_unit(),")",
                           ": ",
                           get_mon_full())

    trn_lab <-
      paste0('trend = ',
             round(ano_shp_dt$trn[[1]], 2),'yr<sup>-1</sup>','<span>&#44;</span> ',
             ' <i>p<i>=',
             round(ano_shp_dt$sig[[1]], 2) )
    trn_lab

    # Convert to plotly
    ano_shp_trn_plty<-  ggplotly(ano_shp_trn_plt) %>%
    layout(legend = list(orientation = "h",
                           xanchor = "center",
                           x = 0.6,
                           y = 1.0))%>%
      layout(margin = list(l = 0, r = 0, b = 10, t = 80),
             title = list( x = 0.001 ,
                           y = 0.92,
                           text = paste0(par_title,
                                         '<br>',
                                         '<sup>',
                                         'Baseline: 1981-2010', '</sup>')))%>%
      layout(
        annotations = list(
          list(
            x = 1 ,
            y = 0.0,
            text = plt_wtrmrk,
            showarrow = F,
            xref = 'paper',
            yref = 'paper',
            xanchor='right', yanchor='auto', xshift=0, yshift=0,
            font=list(size=9, color='#e5e5e5')
          )
        ))%>%
      layout(
        annotations = list(
          list(
            x = 0.30 ,
            y = 0.97,
            text = trn_lab,
            showarrow = F,
            xref = 'paper',
            yref = 'paper',
            xanchor='right', yanchor='auto', xshift=0, yshift=0,
            font=list(size=15, color="#00bfff")
          )
        ))%>%
      layout(xaxis = list(showgrid = FALSE),
             yaxis = list(showgrid = FALSE))
    ano_shp_trn_plty
  })

  # Download data save plots ---------------

   ## Spatial anomaly map and raster data download ----
  file_nam_info <- reactive({
    req(input$indx_picker)
    req(input$month_picker)
    req(input$year_range)


    sel_area_shpfl <- get_shapefile()
    # plot(st_geometry(sel_area_shpfl))

    # other requirements
    monn = input$month_picker
    exindx = input$indx_picker

    fl_nam <-
      paste0(get_region(),
             "_",
             exindx,"_anomaly",
             "_",
             monn,
             "_",
             input$year_range[1],
             "_",
             input$year_range[2])
    fl_nam
  })

  # Spatial anomaly data download as raster (tif )
  output$download_ano_data <- downloadHandler(
    filename = function(file) {
      paste0(file_nam_info(), "_data.tif")
    },
    content = function(file) {
      writeRaster(reactive_extrm_indx_dt_fl(),
                  file,
                  filetype = "GTiff",
                  overwrite = TRUE)
    }
  )

  # Spatial anomaly map save
  output$download_ano_plt <- downloadHandler(
    filename = function(file) {
      paste0(file_nam_info(), "_plot.png")
    },
    content = function(file) {
      ggsave(
        file,
        plot = sp_ano_plt_rct(),
        width = 11,
        height = 10,
        units = "in",
        dpi = 300,
        scale = 1.0,
        limitsize = F,
        device = "png"
      )
    }
  )

  ## Anomaly time series and plot download ----
  file_nam_info_sp_av <- reactive({
    req(input$indx_picker)
    req(input$month_picker)
    req(input$year_range)

    sel_area_shpfl <- get_shapefile()

    # other requirements
    monn = input$month_picker
    exindx = input$indx_picker

    # Year range
    if (monn != "annual") {
      mx_yr = max_year
    } else {
      mx_yr = max_year - 1
    }

    fl_nam <-
      paste0(get_region(),
             "_",
             exindx,"_anomaly_timeseries",
             "_",
             monn,
             "_",
             min_year,
             "_",
             mx_yr)
    fl_nam
  })

  # Spatial anomaly time series data download (.csv)
  output$download_ano_ts_data <- downloadHandler(
    filename = function(file) {
      paste0(file_nam_info_sp_av(),
             "_data.csv")
    },
    content = function(file) {
      write_csv(reactive_extrm_indx_dt_fl_sp(),
                file, append = FALSE)
    }
  )

  #Spatially averaged trend plot save
  output$download_avtrn_plt <- downloadHandler(
    filename = function(file) {
      paste0(file_nam_info_sp_av(), "_trend_plot.png")
    },
    content = function(file) {
      ggsave(
        file,
        plot = spatial_av_trnd_plt_rct(),
        width = 13,
        height = 6,
        units = "in",
        dpi = 400,
        scale = 1,
        limitsize = F,
        device = "png"
      )
    }
  )
  ## Climate normal data and plot download  ----
  file_nam_info_clm_nor <- reactive({
    req(input$indx_picker)
    req(input$month_picker)
    req(input$year_range)

    # other requirements
    monn = input$month_picker
    exindx = input$indx_picker


    sel_area_shpfl <- get_shapefile()


    fl_nam <-
      paste0(get_region(),
             "_",
             exindx,"_climate_normal_1981_2010",
             "_",
             get_mon_full())
    fl_nam
  })

  dwnlnd_clm_plt <- reactive({
    req(input$indx_picker)
    req(input$month_picker)
    req(input$year_range)
    # other requirements
    monn = input$month_picker
    exindx = input$indx_picker

    sel_area_shpfl <- get_shapefile()

      # Climate plot title ( use log for prcp)
        par_title <-  paste0(get_region(), " ",
                            get_par_full(), " ", "(", get_unit(),") climatology (1981-2010)" ,
                             " : ",
                             get_mon_full())

    dwn_clm_plt <- reactive_clm_dt_plt() +
    labs(tag = plt_wtrmrk,title = par_title) +
    theme(
      plot.tag.position = 'bottom',
      plot.tag = element_text(
        color = 'gray50',
        hjust = 1,
        size = 8
      )
    ) +
    theme(
      plot.title = element_text(size = 12, face = 'plain'),
      plot.subtitle = element_text(size = 10)
    )
    dwn_clm_plt
  })

  # Climatological normal plot save
  output$download_clm_nor_plt <- downloadHandler(
    filename = function(file) {
      paste0(file_nam_info_clm_nor(), "_plot.png")
    },
    content = function(file) {
      ggsave(
        file,
        plot =  dwnlnd_clm_plt(),
        width = 10,
        height = 9,
        units = "in",
        dpi = 300,
        scale = 0.9,
        limitsize = F,
        device = "png"
      )
    }
  )

  # Download climate normal data in tiff
  output$download_clm_nor_data <- downloadHandler(
    filename = function(file) {
      paste0(file_nam_info_clm_nor(), "_data.tif")
    },
    content = function(file) {
      writeRaster(reactive_clm_dt_fl(),
                  file,
                  filetype = "GTiff",
                  overwrite = TRUE)
    }
  )


  # Reset  selection /filters -----
  observeEvent(input$reset_input, {
    shinyjs::reset("selection-panel")
  })

  # observeEvent(input$reset_input, {
  #   shinyjs::reset("sel_yrs")
  # })

  # Feedback text -------
  output$feedback_text <- renderText({
    HTML(
      "<p>We used <a href='https://www.gloh2o.org/mswx/'>
      Multi-Source Weather (MSWX) daily data</a> to calculate the extreme climate indices,their anomalies and climatology.
      Anomalies are calculated as the measure of departure from the climatological averages spanning from 1981 to 2010.
      Should you have any inquiries or wish to provide feedback, please do not hesitate to use
      <a href=https://forms.office.com/r/wN0QYAvSTZ'> this feedback form </a> or write to Aseem Sharma @ <a href= 'mailto: Aseem.Sharma@gov.bc.ca'>Aseem.Sharma@gov.bc.ca</a> . </p>"
    )

  })

  # Reports ----

  # MAY 2024
  # HTML in the shiny www folder
  output$doc_html_mon_summ_may2024 <- renderUI({
    a(
      "bc_monthly_climate_summary_May_2024",
      target = "_blank",
      style = "font-size:20px;",
      href = "bc_monthly_climate_summary_May_2024.html",
      img(
        src = "html_logo.png",
        height = "2%",
        width = "2%",
        align = "center"
      )
    )
  })

  # April 2024
  # HTML in the shiny www folder
  output$doc_html_mon_summ_apr2024 <- renderUI({
    a(
      "bc_monthly_climate_summary_April_2024",
      target = "_blank",
      style = "font-size:20px;",
      href = "bc_monthly_climate_summary_April_2024.html",
      img(
        src = "html_logo.png",
        height = "2%",
        width = "2%",
        align = "center"
      )
    )
  })

  # March 2024
  # HTML in the shiny www folder
  output$doc_html_mon_summ_mar2024 <- renderUI({
    a(
      "bc_monthly_climate_summary_March_2024",
      target = "_blank",
      style = "font-size:20px;",
      href = "bc_monthly_climate_summary_March_2024.html",
      img(
        src = "html_logo.png",
        height = "2%",
        width = "2%",
        align = "center"
      )
    )
  })

  # February 2024
  # HTML in the shiny www folder
  output$doc_html_mon_summ_feb2024 <- renderUI({
    a(
      "bc_monthly_climate_summary_February_2024",
      target = "_blank",
      style = "font-size:20px;",
      href = "bc_monthly_climate_summary_February_2024.html",
      img(
        src = "html_logo.png",
        height = "2%",
        width = "2%",
        align = "center"
      )
    )
  })


  # January 2024
  # HTML in the shiny www folder
  output$doc_html_mon_summ_jan2024 <- renderUI({
    a(
      "bc_monthly_climate_summary_January_2024",
      target = "_blank",
      style = "font-size:20px;",
      href = "bc_monthly_climate_summary_January_2024.html",
      img(
        src = "html_logo.png",
        height = "2%",
        width = "2%",
        align = "center"
      )
    )
  })

  # December
  # HTML in the shiny www folder
  output$doc_html_mon_summ_dec2023 <- renderUI({
    a(
      "bc_monthly_climate_summary_December_2023",
      target = "_blank",
      style = "font-size:20px;",
      href = "bc_monthly_climate_summary_December_2023.html",
      img(
        src = "html_logo.png",
        height = "2%",
        width = "2%",
        align = "center"
      )
    )
  })

  # November
  # HTML in the shiny www folder
  output$doc_html_mon_summ_nov2023 <- renderUI({
    a(
      "bc_monthly_climate_summary_November_2023",
      target = "_blank",
      style = "font-size:20px;",
      href = "bc_monthly_climate_summary_November_2023.html",
      img(
        src = "html_logo.png",
        height = "2%",
        width = "2%",
        align = "center"
      )
    )
  })

  # October
  # HTML in the shiny www folder
  output$doc_html_mon_summ_oct2023 <- renderUI({
    a(
      "bc_monthly_climate_summary_October_2023",
      target = "_blank",
      style = "font-size:20px;",
      href = "bc_monthly_climate_summary_October_2023.html",
      img(
        src = "html_logo.png",
        height = "2%",
        width = "2%",
        align = "center"
      )
    )
  })

  # September
  # HTML in the shiny www folder
  output$doc_html_mon_summ_sep2023 <- renderUI({
    a(
      "bc_monthly_climate_summary_September_2023",
      target = "_blank",
      style = "font-size:20px;",
      href = "bc_monthly_climate_summary_September_2023.html",
      img(
        src = "html_logo.png",
        height = "2%",
        width = "2%",
        align = "center"
      )
    )
  })

  # Annual all summary
  # HTML in the shiny www folder
  output$doc_html_longterm_rep <- renderUI({
    a("bc_temperature_precipitation_anomaly_1980_2022",
      target = "_blank",
      style = "font-size:20px;",
      href = "bc_longterm_temp_prcp_anomaly_report_1980_2022_html.html",
      img(
        src = "html_logo.png",
        height = "2%",
        width = "2%",
        align = "center"
      )
    )
  })

  # # pdf in the shiny www folder
  #   output$doc_pdf <- renderUI({
  #     a(
  #       "BC climate anomaly report:",
  #       pdf_file_name,
  #       target = "_blank",
  #       style = "font-size:20px;",
  #       href = pdf_file_name,
  #       img(
  #         src = "pdf_logo.png",
  #         height = "3%",
  #         width = "3%",
  #         align = "center"
  #       )
  #     )
  #   })

  # App deployment date ----
  output$deploymentDate <- renderText({
    paste0("This app was last updated on ",
           readLines("deployment_history.txt"), '.'
           )
  })

}

# Run the application
shinyApp(ui = ui, server = server)
