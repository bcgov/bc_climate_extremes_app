# This script calculates the spatial trend and their significance for every CEI to the lareger
# Domain ( here western North America) and save in the folder.
# This spatial trend  data is used for rendering the spatial trends in the bc_extremes_app

# Required -------------------

library('terra')
library('tidyterra')

library('tidyverse')
library('magrittr')
library('lubridate')
library('furrr')

library('zoo')
library('zyp')
library('colorspace')
library('cptcity')

library(tictoc)

cei_ano_dt_pth <-  './mswx_wna_cei_ano_clm_dt/'
spatial_trn_pth <- cei_ano_dt_pth

# CEI indices metadata and category --------------------------------------

cei_mtdt <-  read_csv(paste0(cei_ano_dt_pth, 'cei_indvl_metadata_all.csv'))
cei_mtdt

# CEI anomalies data -------------------------------------
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
         dt_type = str_extract(fl_nam,'ano|clm'),
         str_yr = str_match(fl_nam, "_(\\d{4})_(\\d{4})\\.nc$")[, 2],
         end_yr   = str_match(fl_nam, "_(\\d{4})_(\\d{4})\\.nc$")[, 3]) %<>%
  dplyr::select(-fl_nam) %>%
  filter(dt_type =='ano')
cei_ano_dt_fl

# Merge CEI data file path with meta data file
cei_ano_dt_mtdt <- full_join(cei_ano_dt_fl,cei_mtdt)
cei_ano_dt_mtdt %<>%
  drop_na()
cei_ano_dt_mtdt

unique(cei_ano_dt_mtdt$ann_mon)
unique(cei_ano_dt_mtdt$indx)

# Spatial anomaly trend  calculation ----------------------------------------------------------------------------------------------------------

spatial_ano_trnd_cal_fun <- function(indx_code, mon_label) {
  tryCatch({
    # Filter metadata for given index and month
    meta_filtered <- cei_ano_dt_mtdt %>%
      filter(indx == indx_code, ann_mon == mon_label)

    if (nrow(meta_filtered) == 0) {
      message(glue::glue("No metadata found for index '{indx_code}' and month '{mon_label}'. Skipping."))
      return(NULL)
    }

    # Prepare metadata
    short_name <- meta_filtered$indx[1]
    long_name  <- paste0(meta_filtered$long_name[1], "_trend_mag_pval")
    month_str  <- meta_filtered$ann_mon[1]
    prdd <- meta_filtered$prd[1]

    # Load anomaly raster
    rst <- terra::rast(meta_filtered$dt_pth)
    names(rst) <- terra::time(rst)

    # Extract year info
    yr_df <- tibble(paryr = names(rst)) %>%
      mutate(yr = as.numeric(str_extract(paryr, "[0-9]+")))
    yr_df
    names(rst) <- yr_df$yr

    start_year <- min(yr_df$yr, na.rm = TRUE)
    end_year   <- max(yr_df$yr, na.rm = TRUE)

    # Mann-Kendall trend function
    mk_trend_fun <- function(y) {
      se <- zyp::zyp.trend.vector(y, x = 1:length(y), conf.intervals = FALSE)
      trend <- if ("trend" %in% names(se)) se[["trend"]] else NA
      pval  <- if ("sig"   %in% names(se)) se[["sig"]] else NA
      c(trend, pval)
    }

    # Attempt trend calculation
    trend_stack <- tryCatch({
      terra::app(rst, mk_trend_fun)
    }, error = function(e) {
      message(glue::glue("Trend calculation failed for index '{indx_code}', month '{mon_label}': {e$message}"))
      # Create empty NA rasters with same dimensions
      na_rast <- terra::rast(rst[[1]])
      na_rast[] <- NA
      c_trend <- na_rast
      c_pval <- na_rast
      terra::rast(list(c_trend, c_pval))
    })

    names(trend_stack) <- c("trend_mag", "pval")

    # Save to NetCDF
    output_file <- file.path(
      spatial_trn_pth,
      glue::glue("{short_name}_cei_sp_trend_{prdd}_{month_str}_mswx_wna_{start_year}_{end_year}.nc")
    )

    terra::writeCDF(
      trend_stack,
      filename = output_file,
      longname = long_name,
      unit = " ",
      overwrite = TRUE,
      split = TRUE,
      compression = 9
    )

    message(glue::glue("Saved: {output_file}"))
  }, error = function(e) {
    message(glue::glue("Error processing index '{indx_code}', month '{mon_label}': {e$message}"))
  })
}

# Setup parallel plan
plan(multisession, workers = 15)

# Create combinations of index and month
combinations <- cei_ano_dt_mtdt %>%
  distinct(indx, ann_mon)

# Run the function in parallel with error handling inside
future_walk2(
  combinations$indx,
  combinations$ann_mon,
  ~ spatial_ano_trnd_cal_fun(.x, .y)
)


# One time replacement calcualtions -------------------------------------------------------------------------------------------------------------

get_output_file_path <- function(short_name, prdd, month_str, start_year, end_year) {
  file.path(
    spatial_trn_pth,
    glue::glue("{short_name}_cei_sp_trend_{prdd}_{month_str}_mswx_wna_{start_year}_{end_year}.nc")
  )
}

# Identify combinations and filter out already processed ones
combinations <- cei_ano_dt_mtdt %>%
  distinct(indx, ann_mon) %>%
  rowwise() %>%
  mutate(
    meta_row = list(
      cei_ano_dt_mtdt %>%
        filter(indx == indx, ann_mon == ann_mon) %>%
        slice(1)
    ),
    output_file = {
      short_name <- meta_row$indx
      prdd       <- meta_row$prd
      month_str  <- meta_row$ann_mon

      # Load raster to determine years
      dt_pth <- meta_row$dt_pth
      if (!file.exists(dt_pth)) {
        NA_character_
      } else {
        years <- tryCatch({
          rst <- terra::rast(dt_pth)
          names(rst) <- time(rst)
          years <- as.numeric(stringr::str_extract(names(rst), "\\d+"))
          get_output_file_path(short_name, prdd, month_str, min(years, na.rm = TRUE), max(years, na.rm = TRUE))
        }, error = function(e) NA_character_)
      }
    }
  ) %>%
  ungroup() %>%
  filter(!is.na(output_file), !file.exists(output_file))
combinations

future_walk2(
  combinations$indx,
  combinations$ann_mon,
  ~ spatial_ano_trnd_cal_fun(.x, .y)
)


# Original ---------------------------
spatial_ano_trnd_cal_fun <- function(cei_mtdt_mon_indx) {
  # mon <- 'ann'
  # indxx <- 'fd'
  # cei_ano_dt_mtdt %>%
  #   filter(ann_mon == mon & indx == indx) -> cei_ano_dt_mtdt_i


  cei_nam_short <- paste0(cei_mtdt_mon_indx$indx)
  cei_nam_long <- paste0(cei_mtdt_mon_indx$long_name, '_trend_mag_pval')
  monn <- paste0(cei_mtdt_mon_indx$ann_mon)


  cei_ano_dt_rst_i <- rast(cei_mtdt_mon_indx$dt_pth)
  names(cei_ano_dt_rst_i) <- time(cei_ano_dt_rst_i)
  # Extract year info
  yr_df <- tibble(paryr = names(cei_ano_dt_rst_i)) %>%
    mutate(yr = as.numeric(str_extract(paryr, "[0-9]+")))
  yr_df
  names(cei_ano_dt_rst_i) <- yr_df$yr
  mx_yr_trn <- max(yr_df$yr)
  mi_yr_trn <- min(yr_df$yr)

  ## Calculate trend: 1979-now
  mk_trn_mag_sig_fun <- function(y) {
    se = zyp.trend.vector(y, x = 1:length(y), conf.intervals = FALSE)
    c(se[['trend']], se[['sig']])
  }

  ano_trn_mag_sig <- app(cei_ano_dt_rst_i, mk_trn_mag_sig_fun)
  names(ano_trn_mag_sig) <- c("trend_mag", "pval")
  # plot(ano_trn_mag_sig)

  # Write NetCDF file
  writeCDF(
    ano_trn_mag_sig,
    filename = paste0(spatial_trn_pth, cei_nam_short, '_', monn, '_spatial_trend_',
                      mi_yr_trn,'_', mx_yr_trn,'.nc'),
    # varname = "trend_magnitude_pval",
    longname = cei_nam_long,
    unit = ' ',
    overwrite = TRUE,
    split=TRUE,
    compression = 9
  )
}

for (i in 1:length(unique(cei_ano_dt_mtdt$indx))) {
  indx_i <- unique(cei_ano_dt_mtdt$indx)[[i]]
  cei_ano_dt_mtdt %>%
    filter(indx == indx_i) -> cei_ano_dt_mtdt_i
  cei_ano_dt_mtdt_i

  mons <- unique(cei_ano_dt_mtdt_i$ann_mon)

  for (j in 1:length(mons)) {
    mons_j <- mons[[j]]
    cei_ano_dt_mtdt_i %>%
      filter(ann_mon == mons_j) -> cei_ano_dt_mtdt_i_j
    cei_ano_dt_mtdt_i_j

    spatial_ano_trnd_cal_fun(cei_ano_dt_mtdt_i_j)

  }

}

tt <- rast('C:/Users/asharma1/OneDrive - Government of BC/bc_climate_extremes_app/cei_spatial_trend/txx_ann_spatial_trend_1979_2024.nc')
tt
varnames(tt)
plot(tt)
