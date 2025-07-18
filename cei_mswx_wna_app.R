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
library('shinycssloaders')
library('plotly')
library('gt')

library('markdown')
library('rmarkdown')

library('terra')
library('tidyterra')
library('leaflet')

library('tidyverse')
library('magrittr')
library('lubridate')

library('zoo')
library('zyp')
library('colorspace')
library('cptcity')

# Load and process input data -----------------------------
## Paths -------------------
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
shp_fls_pth <- './shapefiles/'
cei_ano_dt_pth <-  './mswx_wna_cei_ano_clm_trn_dt/'
cei_eval_rep_pth <- './cei_eval_comp_report/'

#add resource path to read evaluation html directly without moving to wwww
shiny::addResourcePath('report', 'cei_eval_comp_report')


# Credit  -----
plt_wtrmrk <-
  "@Aseem R. Sharma, BC Ministry of Forests. Data credit: ERA5land/C3S/ECMWF."
plt_wtrmrk

# Shape files -------------------------------------------------
# Domain
xmi = -140
xmx = -108
ymi = 39
ymx = 60

# List of shape files
list.files(path = shp_fls_pth,
           pattern = "\\.(shp|gpkg)$",
           full.names = TRUE,
           ignore.case = TRUE) -> shp_fls_lst
shp_fls_lst

# Western North America
na_shp <-   vect(shp_fls_lst[str_detect(shp_fls_lst, "north_america") == T])
# plot(na_shp)
wna_shp <- crop(na_shp, ext(xmi,xmx,ymi,ymx))
# plot(wna_shp)

# BC
bc_shp <-vect(shp_fls_lst[str_detect(shp_fls_lst, "bc_shapefile") == T])
# plot(bc_shp)

# BC eco-province
bc_ecoprv_shp <- vect(shp_fls_lst[str_detect(shp_fls_lst, "bc_ecoprovince") == T])
# plot(bc_ecoprv_shp)
# text(bc_ecoprv_shp, "code", cex = 0.8, col = "black")

# Remove coastal ecoprovince
bc_ecoprv_shp %<>%
  filter(code != 'NEP')
# plot(bc_ecoprv_shp)
# text(bc_ecoprv_shp, "code", cex = 0.8, col = "black")

# BC eco-regions
bc_ecorgn_shp <-vect(shp_fls_lst[str_detect(shp_fls_lst, "bc_ecoregions") == T])
# plot(bc_ecorgns_shp)

# BC eco-sections
bc_ecosec_shp <- vect(shp_fls_lst[str_detect(shp_fls_lst, "bc_ecosections") == T])
# plot(bc_ecosec_shp)
# bc_ecosec_shp$ECOSEC_NM
bc_ecosec_shp <- project(bc_ecosec_shp, "EPSG:4326")

# FLP tables ( Forest landscape planning)
bc_flp_shp <- vect(shp_fls_lst[str_detect(shp_fls_lst, "flp") == T])
# plot(bc_flp_shp)

bc_flp_shp %<>%
  mutate(flp_unit_nam = paste0('FLP- ', ORG_UNIT))

# BC watersheds
bc_wtrshd_shp <- vect(shp_fls_lst[str_detect(shp_fls_lst, "bc_watersheds") == T])
# plot(bc_wtrshd_shp)

# BC FWA watersheds ( Freshwater atlas watersheds)
bc_fwa_shp <- vect(shp_fls_lst[str_detect(shp_fls_lst, "fwa_watersheds") == T])
# plot(bc_wtrshd_shp)
bc_fwa_shp <- project(bc_fwa_shp, "EPSG:4326")

# BC municipalities
bc_muni_shp <-  vect(shp_fls_lst[str_detect(shp_fls_lst, "bc_municipalities") == T])
# plot(bc_muni_shp)
bc_muni_shp <- project(bc_muni_shp, "EPSG:4326")

# CEI indices metadata and category --------------------------------------

cei_mtdt <-  read_csv(paste0(cei_ano_dt_pth, 'cei_indvl_metadata_all.csv'))
cei_mtdt

# Add categories and definitions to the indices
# Categories Heat-related, Cold-related, precipitation  related

indxxs <- unique(cei_mtdt$indx)
indxxs

# Indices into categories
heat_indices <- c(
  "gddgrow10","cddcold18", "su", "tmge10", "tmge5", "tmm", "tn90p", "tnx", "tr",
  "tx90p", "txge30", "txge35", "txgt50p", "txm", "txx","tx3tn3","tx95t",    "wsdi", "wsdi5"
)
cold_indices <- c(
  "csdi", "csdi5", "fd", "hddheat18", "id", "tmlt10", "tmlt5", "tn10p", "tnlt2", "tnltm2",
  "tnltm20", "tnm", "tnn", "tx10p", "txn","txb3tnb3"
)
precipitation_indices <- c(
  "cwd", "prcptot", "r10mm", "r20mm", "r30mm", "r95p", "r95ptot", "r99p",
  "r99ptot", "rx1day", "rx5day", "rx7day", "sdii"
)
drought_indices <- c(
  "cdd",  "spei_3mon", "spei_6mon", "spei_12mon", "spi_3mon",
  "spi_6mon", "spi_12mon"
)
heatwave_indices <- c(
  "hwm_tx90", "hwm_tn90", "hwm_ehf", "hwa_tx90", "hwa_tn90", "hwa_ehf",
  "hwn_tx90", "hwn_tn90", "hwn_ehf", "hwd_tx90", "hwd_tn90", "hwd_ehf",
  "hwf_tx90", "hwf_tn90", "hwf_ehf"
)
coldwave_indices <- c(
  "cwm_ecf", "cwa_ecf", "cwn_ecf", "cwd_ecf", "cwf_ecf"
)
temporal_indicator_indices <- c(
  "day_of_rx1day", "day_of_rx5day", "day_of_rx7day", "day_of_tnn",
  "day_of_tnx", "day_of_txn", "day_of_txx","dtr", "gsl"
)

# Use dplyr::mutate and dplyr::case_when to add the category column
cei_mtdt_cat <- cei_mtdt %>%
  dplyr::mutate(
    category = dplyr::case_when(
      indx %in% heat_indices ~ "Heat",
      indx %in% cold_indices ~ "Cold",
      indx %in% precipitation_indices ~ "Precipitation",
      indx %in% drought_indices ~ "Drought",
      indx %in% heatwave_indices ~ "Heatwave",
      indx %in% coldwave_indices ~ "Coldwave",
      indx %in% temporal_indicator_indices ~ "Temporal_Indicator",
      TRUE ~ "Other"
    )
  ) %>%
  mutate(long_nam_dis = paste0(long_name, ' (',indx, ')',' (',unt, ')'))
cei_mtdt_cat

unique(cei_mtdt_cat$category)

# CEI anomalies  and Climate Data files -------------------------------------
list.files(path = cei_ano_dt_pth,
           pattern = ".nc",
           full.names = T) -> cei_ano_dt_fls
cei_ano_dt_fls

cei_ano_dt_fl <- tibble(dt_pth = cei_ano_dt_fls)

cei_ano_dt_fl %<>%
  mutate(fl_nam = basename(dt_pth)) %<>%
  mutate(indx = str_extract(fl_nam, ".*(?=_cei)"),
         prd = str_extract(fl_nam, "ANN|MON|DAY"),
         ann_mon =case_when(
           str_detect(fl_nam, "_ANN_") ~ "ann",
           str_detect(fl_nam, "_MON_\\w{3}_") ~ str_extract(fl_nam, "_MON_\\w{3}_") %>%
             str_remove_all("_MON_|_"),
           TRUE ~ NA_character_
         ),
         dt_type = str_extract(fl_nam,'ano|clm|trend'),
         str_yr = str_match(fl_nam, "_(\\d{4})_(\\d{4})\\.nc$")[, 2],
         end_yr   = str_match(fl_nam, "_(\\d{4})_(\\d{4})\\.nc$")[, 3]) %<>%
  dplyr::select(-fl_nam)
cei_ano_dt_fl

# Merge CEI data file path with meta data file
cei_ano_dt_mtdt <- full_join(cei_ano_dt_fl,cei_mtdt_cat)
cei_ano_dt_mtdt %<>%
  drop_na()
cei_ano_dt_mtdt

unique(cei_ano_dt_mtdt$ann_mon)

# Rename and reorder categories
cei_ano_dt_mtdt %<>%
  mutate(
    cat_display = case_when(
      category == "Heat" ~ "Heat-related",
      category == "Cold" ~ "Cold-related",
      category == "Precipitation" ~ "Precipitation-related",
      category == "Temporal_Indicator" ~ "Temporal_indicator",
      TRUE ~ category
    ),
    cat_display = factor(cat_display, levels = c(
      "Heat-related", "Cold-related", "Precipitation-related",
      "Drought", "Heatwave", "Coldwave",
      "Temporal_indicator"
    ))
  )
cei_ano_dt_mtdt

# Quarto CEI evaluation report (Quarto HTML)-------------------------------
quarto::quarto_render(paste0(cei_eval_rep_pth,"2_bc_ceiapp_eval_comp.qmd"), output_format = "html")

# Filtering parameters -----------------
cei_ano_dt_mtdt

### Years and months --------------------------------
months_nam <-
  c(
    "ann","Jan","Feb","Mar","Apr","May","Jun","Jul",
    "Aug","Sep","Oct","Nov","Dec"
  )
months_nam
min_year <- 1979
max_year <- 2024

update_month <- "June"
update_year <- "2025"

years <- seq(min_year, max_year, 1)
yr_choices <- sort(years, decreasing = T)

### CEI variables and category -------------------------
# Unique, category  named  and ordered vector for choices
cei_cat_choices <- cei_ano_dt_mtdt %>%
  distinct(category, cat_display) %>%
  arrange(factor(cat_display, levels = levels(cei_ano_dt_mtdt$cat_display))) %>%
  pull(cat_display) %>%
  unique()

names(cei_cat_choices) <- cei_cat_choices
cei_cat_choices

#  UI -----------------------------------------
ui <- fluidPage(
  navbarPage(
    id = "bc_cei",
    title = "BC Climate Extremes",
    theme = "bcgov.css",
    selected = "cei_app",

    ## Intro page --------------------------
    tabPanel(
      title = "Introduction",
      value = "intro",
      column(
        width = 12,
        wellPanel(
          HTML(
            "<h3><b>BC climate extremes app</b>: Data and visualization of changing climate extreme indices (CEI) in British Columbia (BC) </h2>"
          )),
        includeMarkdown("intro_bc_cei_app.Rmd"),
        gt_output("cei_metadt_tbl"),
        column(
          width = 12,
          HTML(
            "<h4><b>Citation</b></h4>
                            <h5> <u>Please cite the contents of this app as:</u>
                            <br>
                            Sharma, A.R. 2024. BC climate extremes app: Visualizing climate extremes indices in British Columbia (BC).</a>
                            British Columbia Ministry of Forests.
                  <a href='https://bcgov-env.shinyapps.io/bc_climate_extremes_app/'
            target='_blank'>https://bcgov-env.shinyapps.io/bc_climate_extremes_app/</a> </h5>"
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
              <h5> The code and data of this app are available through GitHub at <a href='https://github.com/bcgov/bc_climate_extremes_app.git'> https://github.com/bcgov/bc_climate_extremes_app.git.</a></h5>"
          )
        ),
      column(
          width = 12,
          HTML(
            "<h5> <b>Disclaimer</b><h5>
              <h8>  This app have been prepared using <a href='https://www.gloh2o.org/mswx/'> MSWX </a> data,
              as available at the time of preparation.
              Please note that the original data may be subject to updates or revisions.
              Any modifications to the original data may result in adjustments to the findings presented in this report.</h8>"
          )
        ),
        column(width = 12,
               textOutput("deploymentDate"),),

        ###### footer ----------------------------
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

    ## CEI methods and validation ---------------------
    tabPanel(
      title = "Methods and validation",
      # value = 'meth_val',
      # withMathJax(includeMarkdown("cei_methods_validation.Rmd")),
      tags$iframe(
        src = "report/2_bc_ceiapp_eval_comp.html",
        width = "100%",
        height = "2000px",
        frameborder = "0"
      ),

      ###### footer ---------------------------
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
    ),

    ## CEI App page ----------------------------
    tabPanel(
      title = "Climate Extremes App",
      value = 'cei_app',
      sidebarLayout(
        sidebarPanel(
          shinyjs::useShinyjs(),
          id = "selection-panel",
          # style = "position:fixed; width:24%; max-height: 100vh;",
          width = 3,

          ##### Filters/selectors ------------------
          # Custom CSS
          tags$head(tags$style(HTML("
    select.form-control {
      transition: background-color 0.3s ease;
    }
    select.form-control:focus {
      background-color: #d4edda !important;
    }
    .selectize-dropdown .option {
      border-bottom: 1px solid #ccc;
      padding: 6px 10px;
    }
  "))),
      helpText(HTML("<h4><b> Filter/Selections</b> </h4>",)),
      helpText(HTML('
  <style>
    .flash-text {
      color: red;
      animation: flash 1s infinite;
    }
    @keyframes flash {
      0%   { opacity: 1; }
      50%  { opacity: 0; }
      100% { opacity: 1; }
    }
  </style>
  <p>After selection, click <b><i class="flash-text">Run Analysis</i></b> to get outputs.</p>
')),

      # helpText(HTML("<p> After selection click <i> <b> Run Analysis</i></b> to get outputs. </p>",)),
      fluidRow(
        useShinyjs(),
        # Area selection
          pickerInput(
              "major_area",
              "Select region ",
              choices = c("Western North America",
                          "BC", "Ecoprovinces", "Ecoregions", 'Ecosections', "Major watersheds",
                          'FWA watersheds', 'FLP boundaries', 'Municipalities'),
              selected = 'BC'
            ),
            hidden(
              pickerInput(
                "ecoprov_area",
                "Ecoprovinces",
                choices = c("Ecoprovinces (select one)", c(bc_ecoprv_shp$name)),
                multiple = F
              )
            ),
            hidden(
              pickerInput(
                "ecorgn_area",
                "Ecoregions",
                choices = c("Ecoregions (select one)", c(bc_ecorgn_shp$CRGNNM)),
                multiple = F
              )
            ),
            hidden(
              pickerInput(
                "ecosec_area",
                "Ecosections",
                choices = c("Ecosections (select one)", c(bc_ecosec_shp$ECOSEC_NM)),
                multiple = F
              )
            ),
            hidden(
              selectInput(
                "wtrshd_area",
                "Watershed",
                choices = c("Major watersheds (select one)", c(bc_wtrshd_shp$MJR_WTRSHM)),
                multiple = F
              )
            ),
            hidden(
              selectInput(
                "fwa_area",
                "FWA watersheds",
                choices = c("FWA watersheds (select one)", c(bc_fwa_shp$WATERSHE_2)),
                multiple = F
              )
            ),
            hidden(
              selectInput(
                "flp_area",
                "FLP boundaries",
                choices = c("FLP boundaries (select one)", c(bc_flp_shp$flp_unit_nam)),
                multiple = F
              )
            ),
            hidden(
              pickerInput(
                "muni_area",
                "Municipalities",
                choices = c("Municipalities (select one)", c(bc_muni_shp$ABRVN)),
                multiple = F
              )
            ),
            HTML("(Western North America, BC, Eco-provinces/regions/sections,
                 Major Watersheds, FWA watersheds, FLP boundaries, Municipalities)"),
          ),
          br(),
      # CEI variables selection
          fluidRow(
                   selectizeInput(
                     "category", "Choose Category:",
                     choices = cei_cat_choices,
                     options = list(
                       render = I("{
            option: function(item, escape) {
              return '<div style=\"color:brown; font-weight:bold;\">' + escape(item.label) + '</div>';
            },
            item: function(item, escape) {
              return '<div style=\"color:brown; font-weight:bold;\">' + escape(item.label) + '</div>';
            }
          }"))
                   ),
        HTML("(Heat, Cold, Precipitation, Drought, Heatwave, Coldwave, Temopral indicator)"),
                   uiOutput("cei_picker")
          ),
          # fluidRow(
          #   selectInput(
          #     "category", "Choose Category:",
          #     choices = cei_cat_choices,
          #   ),
          #   # HTML("<p style='margin-top:0px; color:gray; (Heat, Cold, Precipitation, Drought,
          #   #      Heatwave, Coldwave, Temopral indicator) </p>"),
          #   uiOutput("cei_picker")
          # ),
          br(),
      # Month year
          fluidRow(title = "Month",
                   uiOutput("month_picker")),
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

            # Run analysis and Reset selection
            br(),
            actionButton("run_ana_button", tags$b(tags$span(style = "color: red;", "Run analysis"))),
            actionButton("reset_input", "Reset"),
            br()
          ),
          fluidRow(column(
            HTML("<h4><b>Location Map</b> </h4>"),
            title = "Map Location",
            width = 12,
            withSpinner(leafletOutput("loc_map", height = "22vh"),type = 6)
          )),
          br(),
          br(),
          fluidRow(column(width = 12, wellPanel(
            style = "background-color: white;",
            HTML(
              '<h4> For BC Climate Anomaly App refer to <a href="https://bcgov-env.shinyapps.io/bc_climate_anomaly/" target="_blank"><b>bc_climate_anomaly_app</b></a></h4>'
            )
            ,
          ))),
        ),
      #### App main panel ---------------
        mainPanel(
          tags$head(tags$style(HTML(
            '.box {margin: 25px;}'
          ))),
          width = 9,
          ##### CEI definition/Description ----------------------
          column(width = 12,
                 wellPanel(
                   HTML("<h4><b>  Climate extreme index (CEI) definition</b> </h4>"),
                   uiOutput("cei_def_text")
                 )),

          ##### Linear trends and spatial anomaly map plots and summary ---------------------
          column(width = 12,
                 wellPanel(
                   HTML("<h4><b> CEI anomaly time series, linear trends and spatial maps</b> </h4>")
                 )),
          fluidRow(column(
            width = 12,
            offset = 0.1,
            tabBox(
              width = 12,
              tabPanel(
                width = 12,
                status = 'primary',
                title = "Time-series plot",
                withSpinner(plotlyOutput("lnr_trn_plt", height = "60vh"),type =6),
                downloadButton(outputId = "download_lnr_trn_plt",
                               label = "Download plot"),
                downloadButton(outputId = "download_ano_ts_data",
                               label = "Download anomaly time series data"),
              ),
              tabPanel(
                width = 12,
                status = 'primary',
                title = "CEI spatial anomaly maps",
                withSpinner(plotOutput("cei_sptl_ano_map", height = "70vh"),type =6),
                downloadButton(outputId = "download_sptl_ano_plt",
                               label = "Download plot"),
                downloadButton(outputId = "download_sptl_ano_data",
                               label = "Download raster data"),
              ),
            )
          )),
        ###### climate normal map and  spatial trends maps (1950s 1980s) --------------------------
          fluidRow(
            box(
              width = 6,
              align="left",
              wellPanel(HTML(
                "<h5><b>CEI climatology (normal) (1981-2010)</b> </h5>"
              )),
              uiOutput("cei_clm_nor_title", height = "30vh"),
              # withSpinner(plotOutput("cei_clm_nor_map", width = "100%", height = "30vh"),type =6),
              withSpinner(plotlyOutput("cei_clm_nor_map",height = "700px"),type =6),
              downloadButton(outputId = "download_cei_clm_nor_plt",
                             label = "Download plot"),
              downloadButton(outputId = "download_cei_clm_nor_data",
                             label = "Download raster data"),
            ),
            box(
              width = 6,
              align="left",
              wellPanel(HTML(
                "<h5><b> Spatial trends (1979-2024) </b> </h5>"
              )),
              uiOutput("cei_sp_trn_title", height = "30vh"),
              # withSpinner(plotOutput("cei_sp_trn_map", width = "100%", height = "30vh"),type =6),
              withSpinner( plotlyOutput("cei_sp_trn_map", height = "700px"),type =6),
              downloadButton(outputId = "download_cei_sp_trn_plt",
                             label = "Download plot"),
              downloadButton(outputId = "download_cei_sp_trn_data",
                             label = "Download raster data"),
            ),
            # box(
            #   width = 4,
            #   align="left",
            #   wellPanel(HTML(
            #     "<h5><b> Spatial trends since 1980 </b> </h5>"
            #   )),
            #   uiOutput("clm_trn80_title", height = "30vh"),
            #   withSpinner(plotOutput("clm_trn80_map", width = "100%", height = "30vh"),type =6),
            #   downloadButton(outputId = "download_clm_trn80_plt",
            #                  label = "Download plot"),
            #   downloadButton(outputId = "download_clm_trn80_data",
            #                  label = "Download raster data"),
            # )
          ),

          ##### App disclaimer -----------------------

          column(width = 12,
                 HTML("<h5><b> Disclaimer:</b> </h5> <h6> This analysis utilizes <a href='https://www.gloh2o.org/mswx/'> MSWX </a> data.
                Any modifications to the dataset or discrepancies in the results due to data
                changes should be carefully considered by users. </h6>")
          ),
        ),
      ),

      ##### footer ---------------------------
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

    ## Feedback and links --------------------------
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
          <a href= 'https://bcgov-env.shinyapps.io/cmip6-BC/'> CMIP6-BC </a>
                               <br>
         <a href= 'https://bcgov-env.shinyapps.io/bc_climate_anomaly/'> BC_climate_anomaly_app </a>
                               <br>
          <br>"
        )
      ),
      ###### footer -----------------------
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


# Server ----------------------------------
server <- function(session, input, output) {
  options(warn = -1)

  # Filters, selections, namings  -----------------------
  ## Filter : Area ----------------------------
  observeEvent(input$major_area, {
    if (input$major_area == "Ecoprovinces") {
      showElement("ecoprov_area")
      hideElement("wtrshd_area")
      hideElement("fwa_area")
      hideElement("flp_area")
      hideElement("ecorgn_area")
      hideElement("ecosec_area")
      hideElement("muni_area")
    } else if (input$major_area == "Ecoregions"){
      hideElement("ecoprov_area")
      hideElement("wtrshd_area")
      hideElement("fwa_area")
      hideElement("flp_area")
      showElement("ecorgn_area")
      hideElement("ecosec_area")
      hideElement("muni_area")
    } else if (input$major_area == "Ecosections"){
      hideElement("ecoprov_area")
      hideElement("wtrshd_area")
      hideElement("fwa_area")
      hideElement("flp_area")
      hideElement("ecorgn_area")
      showElement("ecosec_area")
      hideElement("muni_area")
    } else if (input$major_area == "Major watersheds") {
      hideElement("ecoprov_area")
      showElement("wtrshd_area")
      hideElement("fwa_area")
      hideElement("flp_area")
      hideElement("ecorgn_area")
      hideElement("ecosec_area")
      hideElement("muni_area")
    } else if (input$major_area == "FWA watersheds") {
      hideElement("ecoprov_area")
      hideElement("wtrshd_area")
      showElement("fwa_area")
      hideElement("flp_area")
      hideElement("ecorgn_area")
      hideElement("ecosec_area")
      hideElement("muni_area")
    } else if (input$major_area == "Municipalities") {
      hideElement("ecoprov_area")
      hideElement("wtrshd_area")
      hideElement("fwa_area")
      hideElement("flp_area")
      hideElement("ecorgn_area")
      hideElement("ecosec_area")
      showElement("muni_area")
    }  else if (input$major_area == "FLP boundaries") {
      hideElement("ecoprov_area")
      hideElement("wtrshd_area")
      hideElement("fwa_area")
      showElement("flp_area")
      hideElement("ecorgn_area")
      hideElement("ecosec_area")
      hideElement("muni_area")
    } else {
      hideElement("muni_area")
      hideElement("ecosec_area")
      hideElement("ecorgn_area")
      hideElement("ecoprov_area")
      hideElement("wtrshd_area")
      hideElement("fwa_area")
      hideElement("flp_area")
    }
  })
  ### Area (interactive shapefiles )
  get_shapefile <- reactive({
    if (input$major_area == "Western North America") {
      sel_area_shpfl <- wna_shp

    } else if (input$major_area == "BC") {
      sel_area_shpfl <- bc_shp

    } else if (input$major_area == "Ecoprovinces") {
      if (input$ecoprov_area == "Ecoprovinces (select one)") {
        sel_area_shpfl <- bc_shp
      } else {
        sel_area_shpfl <- bc_ecoprv_shp %>%
          filter(name == input$ecoprov_area)
      }

    } else if (input$major_area == "Ecoregions") {
      if (input$ecorgn_area == "Ecoregions (select one)") {
        sel_area_shpfl <- bc_shp
      } else {
        sel_area_shpfl <- bc_ecorgn_shp %>%
          filter(CRGNNM == input$ecorgn_area)
      }

    } else if (input$major_area == "Ecosections") {
      if (input$ecosec_area == "Ecosections (select one)") {
        sel_area_shpfl <- bc_shp
      } else {
        sel_area_shpfl <- bc_ecosec_shp %>%
          filter(ECOSEC_NM == input$ecosec_area)
      }

    } else if (input$major_area == "Major watersheds") {
      if (input$wtrshd_area == "Major watersheds (select one)") {
        sel_area_shpfl <- bc_shp
      } else {
        sel_area_shpfl <- bc_wtrshd_shp %>%
          filter(MJR_WTRSHM == input$wtrshd_area)
      }

    } else if (input$major_area == "FWA watersheds") {
      if (input$fwa_area == "FWA watersheds (select one)") {
        sel_area_shpfl <- bc_shp
      } else {
        sel_area_shpfl <- bc_fwa_shp %>%
          filter(WATERSHE_2 == input$fwa_area)
      }

    } else if (input$major_area == "FLP boundaries") {
      if (input$flp_area == "FLP boundaries (select one)") {
        sel_area_shpfl <- bc_shp
      } else {
        sel_area_shpfl <- bc_flp_shp %>%
          filter(flp_unit_nam == input$flp_area)
      }
    } else if (input$major_area == "Municipalities") {
      if (input$muni_area == "Municipalities (select one)") {
        sel_area_shpfl <- bc_shp
      } else {
        sel_area_shpfl <- bc_muni_shp %>%
          filter(ABRVN == input$muni_area)
      }
    }

    sel_area_shpfl
  })

  # Region name (interactive)
  get_region <- reactive({
    region <- NULL

    if (input$major_area == "BC") {
      region <- "BC"
    } else if (input$major_area == "Western North America") {
      region <- "Western North America"
    } else if (input$major_area == "Ecoprovinces") {
      region <- input$ecoprov_area
    } else if (input$major_area == "Ecoregions") {
      region <- input$ecorgn_area
    } else if (input$major_area == "Ecosections") {
      region <- input$ecosec_area
    } else if (input$major_area == "Municipalities") {
      region <- input$muni_area
    } else if (input$major_area == "Major watersheds") {
      region <- input$wtrshd_area
    } else if (input$major_area == "FWA watersheds") {
      region <- input$fwa_area
    } else if (input$major_area == "FLP boundaries") {
      region <- input$flp_area
    }

    region
  })

  ## Filter : CEI variables with categories ---------------------------------

  # Dynamically render the index dropdown *after* category is selected
  output$cei_picker <- renderUI({
    req(input$category)

   cei_filtrd <- cei_ano_dt_mtdt %>%
      filter(cat_display == input$category) %>%
      distinct(indx, long_nam_dis)

   selectInput("cei_picker", "Choose Index:",
               choices = setNames(cei_filtrd$indx, cei_filtrd$long_nam_dis))
  })

  ## Filter : time  --------------------------------------
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
      "Select month or annual"
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

  ## Get values (variables name and unit )-------------------------
  ### Years
  get_years <- reactive({
    if (whichInput$type == "specific") {
      sel_yrs <- input$year_specific
    } else{
      sel_yrs <- seq(input$year_range[1], input$year_range[2], 1)
    }
  })

  # Months/Seasons full name ( interactive)
  get_mon_full <- reactive({
    req(input$month_picker)

    month_lookup <- c(
      ann = "Annual",
      Jan = "January", Feb = "February", Mar = "March", Apr = "April",
      May = "May", Jun = "June", Jul = "July", Aug = "August",
      Sep = "September", Oct = "October", Nov = "November", Dec = "December"
    )

    mon_full <- month_lookup[[input$month_picker]]

    if (is.null(mon_full)) mon_full <- "Unknown"
    mon_full
  })

  # CEI full name and and units
  # Get parameter name full
  get_indx_full <-  reactive({
    req(input$cei_picker)
    cei_indx_full <-  cei_ano_dt_mtdt %>%
      filter(indx == input$cei_picker)
    cei_indx_full
    # cei_indx_full <-  cei_ano_dt_mtdt %>%
    #   filter(indx == 'fd')
    # cei_indx_full
    cei_indx_full <- paste0(unique(cei_indx_full$indx_name),'(', unique(cei_indx_full$indx), ')' )
    cei_indx_full
 })

  # Get unit interactively
  get_unit <- reactive({
    req(input$cei_picker)

    unt <-  cei_ano_dt_mtdt %>%
      filter(indx == input$cei_picker)
    unt
    unt <- unique(unt$unt)
    unt
  })

  # Reset  selection /filters -----
  observeEvent(input$reset_input, {
    shinyjs::reset("selection-panel")

  })

  # Location map plot -------------------------------------------
  output$loc_map <- renderLeaflet({
    req(
      input$ecoprov_area,
      input$wtrshd_area,
      input$major_area,
      input$fwa_area,
      input$flp_area
    )

    # Default shape
    sel_area_shpfl <- get_shapefile()
    lyr_id <- NULL

    # Select appropriate shapefile based on inputs
    if (input$major_area == "Major watersheds" &&
        input$wtrshd_area == "Major watersheds (select one)") {
      sel_area_shpfl <- bc_wtrshd_shp['MJR_WTRSHM']
      lyr_id <- "MJR_WTRSHM"

    } else if (input$major_area == "Ecoprovinces" &&
               input$ecoprov_area == "Ecoprovinces (select one)") {
      sel_area_shpfl <- bc_ecoprv_shp['name']
      lyr_id <- "name"

    } else if (input$major_area == "Ecoregions" &&
               input$ecorgn_area == "Ecoregions (select one)") {
      sel_area_shpfl <- bc_ecorgn_shp['CRGNNM']
      lyr_id <- "CRGNNM"

    } else if (input$major_area == "Ecosections" &&
               input$ecosec_area == "Ecosections (select one)") {
      sel_area_shpfl <- bc_ecosec_shp['ECOSEC_NM']
      lyr_id <- "ECOSEC_NM"

    } else if (input$major_area == "Municipalities" &&
               input$muni_area == "Municipalities (select one)") {
      sel_area_shpfl <- bc_muni_shp['ABRVN']
      lyr_id <- "ABRVN"

    } else if (input$major_area == "FLP boundaries" &&
               input$flp_area == "FLP boundaries (select one)") {
      sel_area_shpfl <- bc_flp_shp['flp_unit_nam']
      lyr_id <- "flp_unit_nam"
    }

    # Render leaflet map
    leaflet(sel_area_shpfl) %>%
      addTiles() %>%
      addPolygons(
        layerId = if (!is.null(lyr_id)) as.formula(paste0("~", lyr_id)) else NULL,
        popup = if (!is.null(lyr_id)) as.formula(paste0("~", lyr_id)) else NULL,
        color = "Red",
        weight = 1,
        opacity = 1,
        fill = TRUE,
        fillOpacity = 0
      )
  })

  observeEvent(input$loc_map_shape_click, {
    nm <- input$loc_map_shape_click$id
    print(nm)

    switch(input$major_area,
           "Ecoprovinces"   = updatePickerInput(session, "ecoprov_area", selected = nm),
           "Ecoregions"     = updateSelectInput(session, "ecorgn_area", selected = nm),
           "Ecosections"    = updateSelectInput(session, "ecosec_area", selected = nm),
           "Major watersheds" = updateSelectInput(session, "wtrshd_area", selected = nm),
           "FLP boundaries" = updateSelectInput(session, "flp_area", selected = nm),
           "Municipalities" = updateSelectInput(session, "muni_area", selected = nm)
           # "FWA watersheds" = updateSelectInput(session, "fwa_area", selected = nm)
    )
  })

  # CEI metadata table ---------------------------------
  cei_ano_dt_mtdt
  cei_ano_dt_mtdt %>%
    distinct(indx, .keep_all = T) %>%
    mutate(`Index Name` = paste0(long_name, ' (', indx, ')')) %>%
    dplyr::select(Category = cat_display,`Index Name`,
                  Definition  = definition,Description =description, Unit = unt, Timescale = timescale)  %>%
    arrange(Category,`Index Name`)%>%
    gt(groupname_col = "Category")%>%
    tab_header(title = md("**Table: List of the CEIs with their definitions.**"),
    )%>%
    opt_align_table_header(align = "left")%>%
    tab_source_note(
      source_note =md( "Source: *Climpact R software 'https://climpact-sci.org/'*")
    )%>%
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_row_groups()
    )%>%
    tab_options(column_labels.font.weight = "bold")%>%
    tab_style(
      style = cell_borders(
        sides = c("t", "b"),
        color = "black",
        weight = px(3)
      ),
      locations = list(cells_column_labels(), cells_stubhead())
    )%>%
    tab_style(
      style = cell_borders(
        sides = c("top", "bottom"),
        color = "powderblue",
        weight = px(2.5),
        style = "solid"
      ),
      locations = cells_body()
    ) -> cei_mtdt_gt_tbl
  cei_mtdt_gt_tbl

  output$cei_metadt_tbl <- render_gt(expr = cei_mtdt_gt_tbl)


  # CEI definition text:: reactive ----------------------------------------
  # Definitions, Unit and description of the selected CEI

    # # Check for selected cei
   #  observe({
   #   print(paste("Selected CEI:", input$cei_picker))
   # })

   cei_def_rct <- reactive({
     req(input$cei_picker)

     cei_ano_dt_mtdt %>%
       filter(indx == input$cei_picker) %>%
       filter(dt_type == 'ano') %>%
       distinct(indx, .keep_all = TRUE) -> cei_tbl_def
     cei_tbl_def

    cei_def_txt <-
       HTML(paste0(
         "<b>", cei_tbl_def$long_nam_dis, ":</b> ",
         "The climate extreme index (CEI) ", get_indx_full(),
         " <em>(Definition: ", cei_tbl_def$definition, ")</em> is described as ",
         cei_tbl_def$description, " with unit <em>", cei_tbl_def$unt, "</em>."
       ))
     cei_def_txt

   })

   output$cei_def_text <- renderUI({
       cei_def_rct()
   })

   # For sample run ----
  # cei_ano_dt_mtdt
  # mon <- 'ann'
  # indxx <-  'hwa_tx90'
  # monn <- 'ann'
  # indx <-  'hwa_tx90'
  # sel_area_shpfl <- bc_shp
  #  cei_ano_dt_mtdt %>%
  #    dplyr::filter(ann_mon == mon &
  #                    indx == indxx ) -> cei_ano_dt_mtdt_mon
  #  cei_ano_dt_mtdt_mon
  #  cei_fltr_ano_dt <- mask(rast(cei_ano_dt_mtdt_mon$dt_pth[1]), bc_shp)
  #  cei_fltr_ano_dt
  #  names(cei_fltr_ano_dt) <- time(cei_fltr_ano_dt)
  #  cei_fltr_clm_dt <- mask(rast(cei_ano_dt_mtdt_mon$dt_pth[2]), bc_shp)
  #  cei_fltr_clm_dt
  #  names(cei_fltr_clm_dt) <- paste0(
  #    unique(cei_ano_dt_mtdt_mon$indx), '_clm',
  #    unique(cei_ano_dt_mtdt_mon$ann_mon))
  # cei_fltr_trn_dt <- mask(rast(cei_ano_dt_mtdt_mon$dt_pth[3]), bc_shp)
  # cei_fltr_trn_dt
  # plot(cei_fltr_trn_dt)
  # monn <- unique(cei_ano_dt_mtdt_mon$ann_mon)
  # indx <- unique(cei_ano_dt_mtdt_mon$indx)

   # sel_yrs <- seq(1990,2010,1)
   # sel_yrs
   # sel_area_shpfl <- bc_shp
   # sel_area_shpfl
   # region = "BC"

   # Monthly or annual CEI info selection display --------------
   cei_ann_mon_dt_rct <- eventReactive(input$run_ana_button, {
     req(input$cei_picker, input$month_picker, input$year_range)

     cei_ano_dt_mtdt_mon <- cei_ano_dt_mtdt %>%
       filter(indx == input$cei_picker, ann_mon == input$month_picker)

     if (nrow(cei_ano_dt_mtdt_mon) == 0) {
       msg <- if (input$month_picker == "ann") {
         "This index does not have annual data—please select a month."
       } else {
         "This index has only annual values—please select 'Annual'."
       }
       shinyalert(html = TRUE, text = tagList(h3(msg)), showCancelButton = TRUE,
                  callbackJS = "function(x){ Shiny.setInputValue('reset_trigger', Math.random()); }")
       return(NULL)
     }

     sel_area_shpfl <- get_shapefile()

     # Anomaly
     cei_ano_dt_mtdt_mon %>%
       filter(dt_type == 'ano') -> cei_ano_dt_mtdt_mon_ano

     cei_fltr_ano_dt_rst <- rast(cei_ano_dt_mtdt_mon_ano$dt_pth)

     cei_fltr_ano_msk_rst <-
       terra::crop(cei_fltr_ano_dt_rst, sel_area_shpfl, mask = TRUE)

     names(cei_fltr_ano_msk_rst)
     terra::time(cei_fltr_ano_msk_rst)
     names(cei_fltr_ano_msk_rst) <- terra::time(cei_fltr_ano_msk_rst)

     # Climatology
     cei_ano_dt_mtdt_mon %>%
       filter(dt_type == 'clm') -> cei_ano_dt_mtdt_mon_clm

     cei_fltr_clm_dt_rst <- rast(cei_ano_dt_mtdt_mon_clm$dt_pth)

     cei_fltr_clm_msk_rst <-
       terra::crop(cei_fltr_clm_dt_rst, sel_area_shpfl, mask = TRUE)

     names(cei_fltr_clm_msk_rst) <- paste0(
       cei_ano_dt_mtdt_mon_clm$indx, '_',
       cei_ano_dt_mtdt_mon_clm$ann_mon, '_',
       cei_ano_dt_mtdt_mon_clm$dt_type
     )

     # spatial trend
     cei_ano_dt_mtdt_mon %>%
       filter(dt_type == 'trend') -> cei_ano_dt_mtdt_mon_trn

     cei_fltr_trn_dt_rst <- rast(cei_ano_dt_mtdt_mon_trn$dt_pth)

     cei_fltr_trn_dt_rst <- terra::crop(cei_fltr_trn_dt_rst, sel_area_shpfl, mask = TRUE)


     # Final return list
    result_lst <-  return(list(
       cei_fltr_ano_dt = cei_fltr_ano_msk_rst,
       cei_fltr_clm_dt = cei_fltr_clm_msk_rst,
       cei_sel_mtdt_fl = cei_ano_dt_mtdt_mon,
       cei_fltr_trn_dt = cei_fltr_trn_dt_rst
     ))

   return(result_lst)

   })


 # Time-series and linear trend -------------------------
  time_series_trnd_rct <- eventReactive(input$run_ana_button,{
     req(input$month_picker)
     req(input$cei_picker)
     req(input$major_area)

     withProgress(message = 'Calculating linear trends', value = 0, {
       incProgress(0.02, detail = "Filtering data...")

    ## Time series data generate -----------
     # Filtered reactive data
      cei_ann_mon_dt_rct()[[1]] -> cei_fltr_ano_dt
      # plot(cei_fltr_ano_dt,2)

      cei_ann_mon_dt_rct()[[2]] -> cei_fltr_clm_dt
      # plot(cei_fltr_ano_dt,2)

      cei_ann_mon_dt_rct()[[3]] -> cei_fltr_mtdt

      yr_df <- tibble(paryr = names(cei_fltr_ano_dt))
      yr_df %<>%
         mutate(yr = as.numeric(str_extract(paryr, "[0-9]+")))
       names(cei_fltr_ano_dt) <- yr_df$yr

       # Shapefile spatial average anomalies by year
       cei_ano_ts_av_dt <-
         tibble(rownames_to_column(global(
           cei_fltr_ano_dt, fun = "mean", na.rm = T
         ), "yr")) %>%
         dplyr::select(yr, ano = mean)

       cei_ano_ts_av_dt$ano <- round(cei_ano_ts_av_dt$ano, digits=4)

       cei_ano_ts_av_dt %<>%
         drop_na()
       cei_ano_ts_av_dt$yr <-
         as.numeric(str_extract(cei_ano_ts_av_dt$yr, "[0-9]+"))

       cei_ano_ts_av_dt$indx <- unique(cei_fltr_mtdt$indx)
       cei_ano_ts_av_dt$indx_name <- unique(cei_fltr_mtdt$indx_name)

       cei_ano_ts_av_dt$mon <- unique(cei_fltr_mtdt$ann_mon)
       cei_ano_ts_av_dt$region <- get_region()
       cei_ano_ts_av_dt

       # To download time series
       cei_ano_ts_av_dt %>%
         dplyr::select(indx, indx_name , yr,mon, ano,region) -> cei_av_ano_ts

       ## Trend calculation and plot ------------

       # Background requirements for plots
       indx <- unique(cei_ano_ts_av_dt$indx)
       monn <- unique(cei_ano_ts_av_dt$mon)
       region <- unique(cei_ano_ts_av_dt$region)

       # Trend on average anomaly 1979 - now
       cei_ano_ts_av_dt %>%
         filter(yr >= min_year) %>%
         mutate(# trnd =zyp.trend.vector(ano)[["trend"]],
           # incpt =zyp.trend.vector(ano)[["intercept"]],
           #sig = zyp.trend.vector(ano)[["sig"]])
           sig = round(MannKendall(ano)[[2]], digits = 4)) -> cei_ano_ts_av_dt
       cei_ano_ts_av_dt

       ano_mk_trnd <- zyp.sen(ano ~ yr, cei_ano_ts_av_dt)
       ano_mk_trnd$coefficients
       cei_ano_ts_av_dt$trn <-  ano_mk_trnd$coeff[[2]]
       cei_ano_ts_av_dt$incpt <-  ano_mk_trnd$coeff[[1]]

       xs = c(min(cei_ano_ts_av_dt$yr), max(cei_ano_ts_av_dt$yr))
       trn_slp = c(unique(cei_ano_ts_av_dt$incpt), unique(cei_ano_ts_av_dt$trn))
       ys = cbind(1, xs) %*% trn_slp
       cei_ano_ts_av_dt$trn_lab = paste(
         "italic(~trend( 1979 - present))==",
         round(cei_ano_ts_av_dt$trn, 2),"~yr^{-1}~','~italic(p)==",
         round(cei_ano_ts_av_dt$sig, 2)
       )

       incProgress(0.02, detail = "Plotting linear trend ...")

       # anomaly plot
       ymin <- (-1) * (max(abs(cei_ano_ts_av_dt$ano)))
       ymax <- (1) * (max(abs(cei_ano_ts_av_dt$ano)))
       minyr <- min(cei_ano_ts_av_dt$yr)
       maxyr <- max(cei_ano_ts_av_dt$yr)

       if(ymax < 1){
         ybrk_neg <-
           round(c(seq((-1) * (max(
             abs(cei_ano_ts_av_dt$ano)
           )), 0, length.out = 2)), digits=2)
         ybrk_neg
         ybrk_pos <-
           round(c(seq(0, (1) * (max(
             abs(cei_ano_ts_av_dt$ano)
           )), length.out = 2))[-1], digits=2)
         ybrk_pos
       } else {
         ybrk_neg <-
           ceiling(c(seq((-1) * (max(
             abs(cei_ano_ts_av_dt$ano)
           )), 0, length.out = 4)))
         ybrk_neg
         ybrk_pos <-
           floor(c(seq(0, (1) * (max(
             abs(cei_ano_ts_av_dt$ano)
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
       cei_ano_ts_av_dt %<>%
         mutate(pos_neg = if_else(ano <= 0, "neg", "pos")) %>%
         mutate(ano_mv = rollmean(ano, 3, fill = list(NA, NULL, NA)))
       cei_ano_ts_av_dt
       tail(cei_ano_ts_av_dt)

       cei_trn_plt_title <-  paste0(get_region(), " ",
                              get_indx_full(), " ", "anomaly"," (", get_unit(),")",
                              " : ",
                              get_mon_full())

       y_axis_lab <- paste0(indx, " average anomaly ", "(", get_unit(), ")")

       cei_ano_tst_trn_plt <-
         ggplot(data = cei_ano_ts_av_dt, aes(x = yr, y = ano)) +
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
           name = paste0(indx, " anomaly ", get_unit()),
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
         geom_label(
           aes(x = xs[[1]] + 20),
           color = 'black',
           y = ymax - 0.05,
           fill = NA,
           label = cei_ano_ts_av_dt$trn_lab[[1]],
           size = 4.0, parse=T
         ) +
         geom_segment(
           aes(
             x = xs[[1]],
             xend = xs[[2]],
             y = ys[[1]],
             yend = ys[[2]],
             color = "1979-trend"
           ),
           linetype = "dashed",
           linewidth = 0.9
         ) +
         scale_x_continuous(
           name = " ",
           breaks = seq(min_year, max_year, 5),
           expand = c(0.02, 0.02)
         ) +
         scale_y_continuous(name = y_axis_lab,
                            limits = c(ymin, ymax),
                            breaks = ybrks_seq) +
         labs(title = cei_trn_plt_title, subtitle = "Baseline: 1981-2010") +
         scale_color_manual(
           " ",
           values = c(
             "3-yrs moving mean" = "green",
             "1979-trend" = "black"
           ),
           labels =  c(
             "3-yrs moving mean" = "3-yrs moving mean",
             "1979-trend" = "1979-trend"
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
       cei_ano_tst_trn_plt

      cei_ano_tst_trn_plt <- cei_ano_tst_trn_plt +
         theme(axis.title.y = element_blank())
      cei_ano_tst_trn_plt

       # plotly display

       trn1979_lab <-
         paste0('1979-trend = ',
                round(cei_ano_ts_av_dt$trn[[1]], 2),'yr<sup>-1</sup>','<span>&#44;</span> ',
                ' <i>p<i>=',
                round(cei_ano_ts_av_dt$sig[[1]], 2) )
       trn1979_lab

       #Convert to plotly
       cei_ano_ts_trn_plty <-  ggplotly(cei_ano_tst_trn_plt) %>%
         layout(legend = list(orientation = "v",
                              xanchor = "right",
                              x = 0.99,
                              y = 1.0))%>%
         layout(margin = list(l = 0, r = 0, b = 10, t = 80),
                title = list( x = 0.001 ,
                              y = 0.92,
                              text = paste0(cei_trn_plt_title,
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
               x = 0.40 ,
               y = 0.97,
               text = trn1979_lab,
               showarrow = F,
               xref = 'paper',
               yref = 'paper',
               xanchor='right', yanchor='auto', xshift=0, yshift=0,
               font=list(size=15, color="black")
             )
           ))%>%
        layout(xaxis = list(showgrid = FALSE),
                yaxis = list(showgrid = FALSE))
       cei_ano_ts_trn_plty

       ### File name for download -----
       # Year range
       if (monn != "annual") {
         mx_yr = max_year
       } else {
         mx_yr = max_year - 1
       }

       fl_nam <-
         paste0(get_region(),
                "_",
                indx,"_anomaly_timeseries",
                "_",
                monn,
                "_",
                min_year,
                "_",
                mx_yr)
       fl_nam
       incProgress(0.05, detail = "Finalizing linear trend ...")

       # Final return list
       return(list(lnr_trn_ptly_plt =  cei_ano_ts_trn_plty,
                   fl_nam_dwnld = fl_nam,
                   lnr_trn_plt_dwnld =   cei_ano_tst_trn_plt,
                   ts_data_csv = cei_av_ano_ts
       ))
     })
})

   ## display linear trend  ---------------
   output$lnr_trn_plt <- renderPlotly({
     time_series_trnd_rct()[[1]]})

   ## Download linear trend plot and time series data --------
   # Download plot

   output$download_lnr_trn_plt <- downloadHandler(
     filename = function(file) {
       paste0(time_series_trnd_rct()[[2]], "_trend_plot.png")
     },
     content = function(file) {
       ggsave(
         file,
         plot = time_series_trnd_rct()[[3]],
         width = 13,
         height = 6,
         units = "in",
         dpi = 300,
         scale = 0.9,
         limitsize = F,
         device = "png"
       )
     }
   )

   # Download time series (.csv)
   output$download_ano_ts_data <- downloadHandler(
     filename = function(file) {
       paste0(time_series_trnd_rct()[[2]],
              "_data.csv")
     },
     content = function(file) {
       write_csv(time_series_trnd_rct()[[4]],
                 file, append = FALSE)
     }
   )

   # Spatial anomaly data and plot:  Reactive ----------------------------------------------------------------

   cei_spatial_ano_dt_plt_rct <- eventReactive(input$run_ana_button, {
     req(input$cei_picker)
     req(input$month_picker)
     req(input$year_range)

     sel_area_shpfl <- get_shapefile()

     ## Filtered spatial anomaly data  ----------

     # Filtered reactive data
     cei_ann_mon_dt_rct()[[1]] -> cei_fltr_ano_dt
     # plot(cei_fltr_ano_dt,2)

     # cei_ann_mon_dt_rct()[[2]] -> cei_fltr_clm_dt
     # # plot(cei_fltr_ano_dt,2)

     cei_ann_mon_dt_rct()[[3]] -> cei_fltr_mtdt

    monn <- unique(cei_fltr_mtdt$ann_mon)
    indx <- unique(cei_fltr_mtdt$indx)

    withProgress(message = 'Extracting and plotting anomaly data...', value = 0, {

      incProgress(0.05, detail = "Initializing...")

     yr_df <- tibble(paryr = names(cei_fltr_ano_dt))
     yr_df %<>%
       mutate(yr = as.numeric(str_extract(paryr, "[0-9]+")))
     names(cei_fltr_ano_dt) <- yr_df$yr

     # Filter for selected year (s)
     sel_yrs <- get_years()

     if (length(sel_yrs) > 50) {
       sel_yrs <- sel_yrs[1:50]
       shinyalert(html = T,
                  text = tagList(h3(
                    "Too many years selected, maximum 50 allowed."
                  )),
                  showCancelButton = T)
     }
     incProgress(0.10, detail = "Filtering data for selected years...")
     ano_dt_rast  <-  subset(cei_fltr_ano_dt, which(names(cei_fltr_ano_dt) %in% sel_yrs))
     ano_dt_rast

     ## Spatial anomaly overview summary  --------
     incProgress(0.10, detail = "Calculating anomaly statistics...")
     # Year range
     minyr <- min(sel_yrs)
     maxyr <- max(sel_yrs)

     mn_ano <-
       terra::global(ano_dt_rast, fun = "mean", na.rm = T)
     mn_ano
     mn_ano <- round(mean(mn_ano$mean, na.rm = T), 2)
     mi_ano <- terra::global(ano_dt_rast, fun = "min", na.rm = T)
     mi_ano <- round(min(mi_ano$min, na.rm = T), 2)
     mx_ano <- terra::global(ano_dt_rast, fun = "max", na.rm = T)
     mx_ano <- round(max(mx_ano$max, na.rm = T), 2)

     # Combine for a display table
       mi_ano_val = paste0(mi_ano)
       mn_ano_val = paste0(mn_ano, 'get_unit()')
       mx_ano_val = paste0(mx_ano)

     # Create a table
     ano_ovr_dt <-
       data.frame(
         "Anomaly" = c("Minimum", "Mean", "Maximum"),
         "Value" = c(mi_ano_val, mn_ano_val, mx_ano_val)
       )
     ano_ovr_dt

     ## Spatial anomaly plot ----------
     ano_rng_lmt <- terra::minmax(ano_dt_rast, compute = T)
     minval <- (-1) * (max(abs(ano_rng_lmt), na.rm = T))
     maxval <- (1) * (max(abs(ano_rng_lmt), na.rm = T))

     # Breaks and labels
     incProgress(0.10, detail = "Building color breaks and labels...")
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
     indx_plt_title <-  paste0(get_region(), " ",
                            get_indx_full(), " anomaly (", get_unit(),")",
                            ": ",
                            get_mon_full())
     # indx_plt_title  <- 'indx_plt_title '

     xlim <- c(-140,-113.0)
     ylim <- c(45,61)

     ### plot to display ----
     incProgress(0.5, detail = "Creating spatial anomaly plot...")

    cei_spatial_ano_plt <-  ggplot() +
       geom_spatraster(data = ano_dt_rast) +
       scale_fill_gradientn(
         name = paste0(indx, " anomaly ", 'get_unit()'),
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
# cei_spatial_ano_plt

 cei_spatial_ano_plt <- cei_spatial_ano_plt +
       labs(
         tag = plt_wtrmrk,
         title = indx_plt_title,
         subtitle = paste0(
           "Baseline: 1981-2010. ",
           '[',"get_region()",  ' anomaly over ',
           minyr,
           '-',
           maxyr,
           ': Mean = ',
           ano_ovr_dt[2, 2],
           ' ,',
           ' Range = ',ano_ovr_dt[1, 2], ' - ',
           ano_ovr_dt[3, 2],
           ']'
         )
       ) +
       theme(
         plot.tag.position = "bottom",
         plot.tag = element_text(
           color = 'gray50',
           hjust = 1,
           vjust = 0,
           size = 8
         )
       )
     cei_spatial_ano_plt


     ### File name for download ------------
     fl_nam <-
       paste0(get_region(),
              "_",
              indx,"_anomaly",
              "_",
              monn,
              "_",
              input$year_range[1],
              "_",
              input$year_range[2])
     fl_nam

     ## final reactive output list  -------------------
     incProgress(0.10, detail = "Finalizing results...")
     return(list(
       cei_sptl_ano_data =  ano_dt_rast,
       cei_sptl_ano_plt = cei_spatial_ano_plt,
       download_fl_nam = fl_nam
     ))
})
   })

   ### Spatial anomaly map display ---------------------
   output$cei_sptl_ano_map <- renderPlot({
     cei_spatial_ano_dt_plt_rct()[[2]]
   })

   ### Spatial anomaly map and data download ------------------
   # Spatial anomaly plot download/save
   output$download_sptl_ano_plt <- downloadHandler(
     filename = function(file) {
       paste0(cei_spatial_ano_dt_plt_rct()[[3]], "_plot.png")
     },
     content = function(file) {
       ggsave(
         file,
         plot = cei_spatial_ano_dt_plt_rct()[[2]],
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

   # Spatial anomaly data download as raster (tif )
   output$download_sptl_ano_data <- downloadHandler(
     filename = function(file) {
       paste0(cei_spatial_ano_dt_plt_rct()[[3]], "_data.tif")
     },
     content = function(file) {
       writeRaster(cei_spatial_ano_dt_plt_rct()[[1]],
                   file,
                   filetype = "GTiff",
                   overwrite = TRUE)
     }
   )

  # CEI climatology (normal) plot ----------------------------------------------------------------------------
   cei_clm_nor_plt_rct <- eventReactive(input$run_ana_button,{
     req(input$cei_picker)
     req(input$month_picker)
     ## Filtered spatial anomaly data  ----------

     sel_area_shpfl <- get_shapefile()

     # Filtered reactive data
     # cei_ann_mon_dt_rct()[[1]] -> cei_fltr_ano_dt
     # plot(cei_fltr_ano_dt,2)

     cei_ann_mon_dt_rct()[[2]] -> cei_fltr_clm_dt
     # plot(cei_fltr_ano_dt,2)

     cei_ann_mon_dt_rct()[[3]] -> cei_fltr_mtdt

     monn <- unique(cei_fltr_mtdt$ann_mon)
     indx <- unique(cei_fltr_mtdt$indx)


     ## Climate normal plot title -----
     cei_clm_nor_title_txt <-  paste0(get_region(), " ",
                                      get_indx_full(), " (average of  1981-2010) ", "(", get_unit(),")" ,
                                    " : ",
                                    get_mon_full())
     cei_clm_nor_title_txt


     ## Climate normal plot for display -------
     cei_fltr_clm_dt
     names(cei_fltr_clm_dt) <- monn

     # Calculate mean and range of normal values
     mn_clm_val <-
       round(global(cei_fltr_clm_dt, 'mean', na.rm = T), digits = 2)
     mi_clm_val <-
       round(global(cei_fltr_clm_dt, 'min', na.rm = T), digits = 2)
     mx_clm_val <-
       round(global(cei_fltr_clm_dt, 'max', na.rm = T), digits = 2)

     # Convert to points and plot
     cei_fltr_clm_dt
     cei_fltr_clm_dt_df <- as_tibble (cei_fltr_clm_dt, xy = TRUE, na.rm = TRUE)
     names(cei_fltr_clm_dt_df) <- c( 'x','y', 'indx_clm')

    cei_spatial_clm_plt <-  ggplot() +
      geom_tile(data = cei_fltr_clm_dt_df, aes(x=x,y=y,fill=indx_clm),alpha=1)+
       # geom_spatraster(data = cei_fltr_clm_dt) +
       scale_fill_continuous(
         type = "viridis",
         name = " ",
         option = "viridis",
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
       scale_x_continuous(
         name =  "Longitude (°W) ",
         # breaks = seq(xmi - 5, xmx + 5, 10),
         labels = abs,
         expand = c(0.01, 0.01)
       ) +
       scale_y_continuous(
         name = "Latitude (°N) ",
         # breaks = seq(ymi - 1, ymx + 1, 6),
         labels = abs,
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
     cei_spatial_clm_plt

     cei_spatial_clm_plt <- cei_spatial_clm_plt +
       labs(tag = plt_wtrmrk) +
       theme(
         plot.tag.position = "bottom",
         plot.tag = element_text(
           color = 'gray50',
           hjust = 1,
           size = 6
         )
       ) +
       labs(
         # title = cei_plt_title,
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
     cei_spatial_clm_plt

     ## Climate normal data and plot download ---------
     fl_nam <-
       paste0(get_region(),
              "_",
              get_indx_full(),"_climatology(normal)_1981_2010",
              "_",
              get_mon_full())
     fl_nam

     # Plot with title for download

     # Climate plot title ( use log for prcp)
     cei_plt_title <-  paste0(get_region(), " ",
                            get_indx_full(), " ", "(", get_unit(),")" ,
                            " : ",
                            get_mon_full(), " (average 1981-2010)")

     cei_spatial_clm_plt_dnwld <-  cei_spatial_clm_plt+
       labs( title = cei_plt_title)
     cei_spatial_clm_plt_dnwld

     ### Final reactive output list --------------

     return(list(
       cei_clm_nor_title_txt = cei_clm_nor_title_txt,
       cei_clm_nor_plt = cei_spatial_clm_plt,
       cei_clm_nor_plt_dnwld = cei_spatial_clm_plt_dnwld,
       cei_clm_nor_data =  cei_fltr_clm_dt,
       download_fl_nam = fl_nam
     ))

   })

   ## Climate normal plot display & download ------------

   # Plot title
   output$cei_clm_nor_title <- renderText({
     cei_clm_nor_plt_rct()[[1]]
   })

   # plot display
   # output$cei_clm_nor_map <- renderPlot({
   #   cei_clm_nor_plt_rct()[[2]]
   # })

   output$cei_clm_nor_map <- renderPlotly({
     ggplotly(cei_clm_nor_plt_rct()[[2]], tooltip = c("x", "y", "indx_clm")) %>%
       layout(
         autosize = TRUE,
         dragmode = "zoom",
         hovermode = "closest"
       )
   })


   # climate normal plot download/save
   output$download_cei_clm_nor_plt <- downloadHandler(
     filename = function(file) {
       paste0(cei_clm_nor_plt_rct()[[5]], "_plot.png")
     },
     content = function(file) {
       ggsave(
         file,
         plot =  cei_clm_nor_plt_rct()[[3]],
         width = 11,
         height = 9,
         units = "in",
         dpi = 300,
         scale = 0.9,
         limitsize = F,
         device = "png"
       )
     }
   )

   # Climate normal data save in tiff
   output$download_cei_clm_nor_data <- downloadHandler(
     filename = function(file) {
       paste0(cei_clm_nor_plt_rct()[[5]], "_data.tif")
     },
     content = function(file) {
       writeRaster(cei_clm_nor_plt_rct()[[4]],
                   file,
                   filetype = "GTiff",
                   overwrite = TRUE)
     }
   )

   # CEI Spatial trend (1979-2024) plot ----------------------------------------------------------------------------
   cei_sp_trn_plt_rct <- eventReactive(input$run_ana_button,{
     req(input$cei_picker)
     req(input$month_picker)
     ## Filtered spatial trend data----------

     sel_area_shpfl <- get_shapefile()

     # Filtered reactive data
     # cei_ann_mon_dt_rct()[[1]] -> cei_fltr_ano_dt
     # plot(cei_fltr_ano_dt,2)

     cei_ann_mon_dt_rct()[[4]] -> cei_fltr_trn_dt
     # plot(cei_fltr_ano_dt,2)

     cei_ann_mon_dt_rct()[[3]] -> cei_fltr_mtdt

     monn <- unique(cei_fltr_mtdt$ann_mon)
     indx <- unique(cei_fltr_mtdt$indx)


     ## Climate normal plot title -----
     cei_sp_trn_title_txt <-  paste0(get_region(), " ",
                                     get_indx_full(), " (trend (1979 -2024)) ", "(", get_unit(),")" ,
                                     " : ",
                                     get_mon_full())
     cei_sp_trn_title_txt


     ## Spatial trend plot  for display -------
     cei_fltr_trn_dt

     mn_trn_val <-
       round(global (cei_fltr_trn_dt[[1]], 'mean', na.rm = T), digits = 3)
     mi_trn_val <-
       round(global (cei_fltr_trn_dt[[1]], 'min', na.rm = T), digits = 3)
     mx_trn_val <-
       round(global (cei_fltr_trn_dt[[1]], 'max', na.rm = T), digits = 3)

     # Convert to point data
     cei_ano_sp_mk_trn_sig_dt <- as_tibble (cei_fltr_trn_dt, xy = TRUE, na.rm = TRUE) %>%
       mutate(trend_mag = round(trend_mag, 3))

     trn_unt = "get_unit()"

     #### Plot trend map
     # incProgress(0.1, detail = "Plotting spatial trend (1950-now)...")

     cei_ano_dt_sig_trn <- cei_ano_sp_mk_trn_sig_dt %>%
       dplyr::filter(pval <= 0.1)
     cei_ano_dt_sig_trn

     mxtrn <- max(abs(cei_ano_sp_mk_trn_sig_dt$trend_mag), na.rm = T)
     mxtrn

     cei_ano_dt_sp_trn_sig_plt <- ggplot() +
       geom_tile(data = cei_ano_sp_mk_trn_sig_dt, aes(x=x,y=y,fill=trend_mag),alpha=1)+
       scale_fill_continuous_diverging(palette="Blue-Red",n_interp=21,
                                       limits=c(-mxtrn,mxtrn),
                                       # breaks=seq(-1.2, 1.2,0.3),
                                       # labels=seq(-0.8, 0.8,0.2),
                                       # name=expression(paste0(parr," trend ", unt, " yr \U2212 \U00B9")))+
                                       name=bquote(~"trend"~yr^{-1}))+
       geom_point(data=cei_ano_dt_sig_trn,aes(x=x,y=y),color="Black",fill="Gray10", alpha=0.4,size=0.3, shape =3)+
       geom_sf(
         data = sel_area_shpfl,
         colour = "black",
         size = 1,
         fill = NA,
         alpha = 0.8
       ) +
       scale_x_continuous(
         name =  "Longitude (°W) ",
         # breaks = seq(xmi - 5, xmx + 5, 10),
         labels = abs,
         expand = c(0.01, 0.01)
       ) +
       scale_y_continuous(
         name = "Latitude (°N) ",
         # breaks = seq(ymi - 1, ymx + 1, 6),
         labels = abs,
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
     cei_ano_dt_sp_trn_sig_plt

     cei_ano_dt_sp_trn_sig_plt <-  cei_ano_dt_sp_trn_sig_plt +
       labs(tag = plt_wtrmrk) +
       theme(
         plot.tag.position = "bottom",
         plot.tag = element_text(
           color = 'gray50',
           hjust = 1,
           size = 6
         )
       ) +
       labs(
         # title = par_title,
         subtitle = paste0(
           'Mean = ',
           mn_trn_val[[1]]," ",
           "(", trn_unt," yr", "\u207B", "\u00B9)" ,
           "  ",
           "Range = ",
           "[",
           mi_trn_val[[1]],
           " - ",
           mx_trn_val[[1]],"]. "
         )
       ) +
       theme(
         plot.title = element_text(size = 12, face = 'plain'),
         plot.subtitle = element_text(size = 10)
       )
     cei_ano_dt_sp_trn_sig_plt

     ## Spatial Trend data and plot download ---------
     fl_nam <-
       paste0(get_region(),
              "_",
              get_indx_full(),"_spatial_trend_1979_2024",
              "_",
              get_mon_full())
     fl_nam

     # Plot with title for download

     # Climate plot title ( use log for prcp)
     cei_plt_title <-  paste0(get_region(), " ",
                              get_indx_full(), " ", "(", get_unit(),")" ,
                              " : ",
                              get_mon_full(), " (spatial trend 1979-2024)")

     cei_spatial_trn_plt_dnwld <-   cei_ano_dt_sp_trn_sig_plt+
       labs( title = cei_plt_title)
     cei_spatial_trn_plt_dnwld

     ### Final reactive output list --------------

     return(list(
       cei_sp_trn_title_txt = cei_sp_trn_title_txt,
       cei_sp_trn_plt = cei_ano_dt_sp_trn_sig_plt,
       cei_sp_trn_plt_dnwld =  cei_spatial_trn_plt_dnwld ,
       cei_sp_trn_data =  cei_fltr_trn_dt,
       download_fl_nam = fl_nam
     ))

   })

   ## Spatial trend  display & download ------------

   # Plot title
   output$cei_sp_trn_title <- renderText({
     cei_sp_trn_plt_rct()[[1]]
   })

   # # plot display
   # output$cei_sp_trn_map <- renderPlot({
   #   cei_sp_trn_plt_rct()[[2]]
   # })

   output$cei_sp_trn_map <- renderPlotly({
     ggplotly(cei_sp_trn_plt_rct()[[2]], tooltip = c("x", "y", "trend_mag")) %>%
       layout(
         autosize = TRUE,
         dragmode = "zoom",
         hovermode = "closest"
       )
   })


   # Spatial trend  download/save
   output$download_cei_sp_trn_plt <- downloadHandler(
     filename = function(file) {
       paste0(cei_sp_trn_plt_rct()[[5]], "_plot.png")
     },
     content = function(file) {
       ggsave(
         file,
         plot =  cei_sp_trn_plt_rct()[[3]],
         width = 11,
         height = 9,
         units = "in",
         dpi = 300,
         scale = 0.9,
         limitsize = F,
         device = "png"
       )
     }
   )

   # Spatial trend  data in tiff
   output$download_cei_sp_trn_data <- downloadHandler(
     filename = function(file) {
       paste0(cei_sp_trn_plt_rct()[[5]], "_data.tif")
     },
     content = function(file) {
       writeRaster(cei_sp_trn_plt_rct()[[4]],
                   file,
                   filetype = "GTiff",
                   overwrite = TRUE)
     }
   )


  # Feedback text -------
  output$feedback_text <- renderText({
    HTML("<p>We used <a href='https://www.gloh2o.org/mswx/' target='_blank'>
MSWX daily data </a> to calculate the anomalies and climatology.
Anomalies are calculated as the measure of departure from the climatological averages spanning from 1981 to 2010.
Should you have any inquiries or wish to provide feedback, please do not hesitate to use
<a href='https://forms.office.com/r/wN0QYAvSTZ' target='_blank'>this feedback form</a> or write to
<a href='mailto:Aseem.Sharma@gov.bc.ca'><b>Aseem Sharma</b></a>.</p>")

  })

  # App deployment date ----
  output$deploymentDate <- renderText({
    paste0("This app was last updated on ",
           readLines("deployment_history.txt"), '.'
    )
  })

}

# Run the application
shinyApp(ui = ui, server = server)
