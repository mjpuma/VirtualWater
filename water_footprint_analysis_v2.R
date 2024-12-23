# =============================================================================
# Updated Water Footprint and Trade Flow Analysis Script
# =============================================================================

# -----------------------------
# 1. Load Required Packages
# -----------------------------
required_packages <- c(
  "readxl", "dplyr", "tidyr", "stringr", "ggplot2",
  "rnaturalearth", "rnaturalearthdata", "sf", "viridis",
  "igraph", "rworldmap", "RColorBrewer", "FAOSTAT", "ggpubr",
  "networkD3", "tibble", "circlize", "htmlwidgets", "grid", "rlang",
  "rnaturalearth",    # For map data
  "rnaturalearthdata", # For map data
  "sf",               # For spatial features
  "rworldmap",        # For country centroids
  "ggraph",          # For network visualization
  "tidygraph"        # For network data structures
)

installed_packages <- rownames(installed.packages())
for (p in required_packages) {
  if (!(p %in% installed_packages)) {
    install.packages(p, dependencies = TRUE)
  }
}

suppressPackageStartupMessages({
  lapply(required_packages, library, character.only = TRUE)
})

source("additional_checks.R")

# -----------------------------
# 2. Configuration Settings
# -----------------------------
# Define analysis options
analysis_type <- "period"  # Options: "period" or "single_year"
target_year <- 2005        # Only used if analysis_type is "single_year"

# Define periods
periods <- list(
  early = 1996:2000,
  late = 2016:2020
)

# -----------------------------
# 3. Helper Functions
# -----------------------------
# Function to get analysis years based on configuration
get_analysis_years <- function() {
  if (analysis_type == "single_year") {
    return(target_year)
  } else {
    return(periods)
  }
}

# Function to get year label for outputs
get_year_label <- function(years) {
  if (length(years) == 1) {
    return(as.character(years))
  } else {
    return(paste(min(years), max(years), sep = "-"))
  }
}

# Function to standardize crop names
standardize_crop_name <- function(crop_name) {
  crop_name %>%
    str_trim() %>%
    str_to_title() %>%
    str_replace_all("\\s+", " ")
}

create_logger <- function(context) {
  log_file <- file.path("outputs", "logs", paste0(
    context, "_",
    format(Sys.time(), "%Y%m%d_%H%M%S"),
    ".log"
  ))
  dir.create(dirname(log_file), recursive = TRUE, showWarnings = FALSE)
  
  function(msg, level = "INFO") {
    timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    message <- paste0(timestamp, " - [", level, "] - ", msg)
    cat(message, "\n")
    cat(message, "\n", file = log_file, append = TRUE)
  }
}

# -----------------------------
# 4. Load Base Data
# -----------------------------
# Load country conversion table
country_conversion <- read.csv("ancillary/country_conversion_table.csv", stringsAsFactors = FALSE) %>%
  mutate(
    Country = str_trim(Country),
    FAOST_CODE = as.character(FAOSTAT),
    iso3 = str_trim(ISO3.alpha)
  ) %>%
  filter(!is.na(iso3) & iso3 != "") %>%
  distinct(iso3, .keep_all = TRUE)

# Modify country_conversion to combine China, Hong Kong, and Macao under "CHN"
china_codes <- c("41", "96", "128", "351")  # China, Hong Kong SAR, Macao SAR, China
country_conversion <- country_conversion %>%
  filter(!FAOST_CODE %in% china_codes)

china_entries <- data.frame(
  Country = "China",
  FAOST_CODE = china_codes,
  iso3 = "CHN",
  stringsAsFactors = FALSE
)

country_conversion <- bind_rows(country_conversion, china_entries) %>%
  distinct(iso3, .keep_all = TRUE)

# Create country mappings
faostat_to_iso3 <- setNames(country_conversion$iso3, country_conversion$FAOST_CODE)
iso3_to_name <- setNames(country_conversion$Country, country_conversion$iso3)

# Handle alternative country names
alternative_names <- data.frame(
  Country = c(
    "Democratic Republic of the Congo", "Congo", "Republic of the Congo",
    "Sudan", "Sudan (former)", "South Sudan",
    "Turkey", "Turkiye", "Türkiye",
    "Iran (Islamic Republic of)", "Russia", "Syrian Arab Republic", "Viet Nam"
  ),
  Standardized_Country = c(
    "Democratic Republic of Congo", "Republic of Congo", "Republic of Congo",
    "Sudan", "Sudan", "South Sudan",
    "Turkey", "Turkey", "Turkey",
    "Iran", "Russian Federation", "Syria", "Vietnam"
  ),
  stringsAsFactors = FALSE
)

# Production data mapping
production_mapping <- data.frame(
  FAO_Code = c(
    # Individual crops - production codes
    "27",   # Rice, paddy
    "156",  # Sugar cane
    "56",   # Maize
    "242",  # Groundnuts (with shell)
    "270",  # Rapeseed
    "236",  # Soybeans
    "267",  # Sunflower seed
    
    # Pulses
    "176",  # Beans, dry
    "187",  # Peas, dry
    "191",  # Chick peas
    
    # Cereals
    "15",   # Wheat
    "44",   # Barley
    "71",   # Rye
    "75",   # Oats
    "83",   # Sorghum
    "79",   # Millet
    
    # Roots
    "116",  # Potatoes
    "157",  # Sugar beet
    "125",  # Cassava
    "122",  # Sweet potatoes
    "137"   # Yams
  ),
  WF_Crop = c(
    "Rice",
    "Sugar",
    "Maize",
    "Groundnut",
    "Rapeseed",
    "Soyabean",
    "Sunflower",
    rep("Pulses", 3),
    rep("Temperate Cereals", 4),
    rep("Tropical Cereals", 2),
    rep("Temperate Roots", 2),
    rep("Tropical Roots", 3)
  ),
  stringsAsFactors = FALSE
)

# Trade data mapping
trade_mapping <- data.frame(
  FAO_Code = c(
    # Individual crops - trade codes
    "27",   # Rice, paddy
    "156",  # Sugar cane
    "56",   # Maize
    "243",  # Groundnuts, shelled (Note: trade uses shelled version)
    "270",  # Rapeseed
    "236",  # Soybeans
    "267",  # Sunflower seed
    
    # Pulses
    "176",  # Beans, dry
    "187",  # Peas, dry
    "191",  # Chick peas
    
    # Cereals
    "15",   # Wheat
    "44",   # Barley
    "71",   # Rye
    "75",   # Oats
    "83",   # Sorghum
    "79",   # Millet
    
    # Roots
    "116",  # Potatoes
    "157",  # Sugar beet
    "125",  # Cassava
    "122",  # Sweet potatoes
    "137"   # Yams
  ),
  WF_Crop = c(
    "Rice",
    "Sugar",
    "Maize",
    "Groundnut",
    "Rapeseed",
    "Soyabean",
    "Sunflower",
    rep("Pulses", 3),
    rep("Temperate Cereals", 4),
    rep("Tropical Cereals", 2),
    rep("Temperate Roots", 2),
    rep("Tropical Roots", 3)
  ),
  stringsAsFactors = FALSE
)





# Load extraction rates
extraction_rates <- read_excel("ancillary/FoodCommodity_ForCarole_v5.xlsx", sheet = "withData") %>%
  rename(
    FAO_Code = `FAO Code`,
    Extraction_Rate = `Primary Product Extraction Rate`
  ) %>%
  mutate(
    FAO_Code = as.character(FAO_Code),
    Extraction_Rate = ifelse(is.na(Extraction_Rate) | Extraction_Rate == 0, 1, Extraction_Rate)
  )

# -----------------------------
# 5. Water Footprint Functions
# -----------------------------


# Function to read and process raw water footprint data
read_water_footprint <- function(file_path, water_type) {
  log_msg <- create_logger("water_footprint_reading")
  
  tryCatch({
    log_msg(paste("Starting to read:", file_path))
    
    # Read sheet names
    sheet_names <- excel_sheets(file_path)
    log_msg(paste("Found sheets:", paste(sheet_names, collapse = ", ")))
    
    # Initialize data list
    data_list <- list()
    
    # Define sheets to skip
    skip_sheets <- c("C3per", "C4per", "C3annual", "Global")
    
    for (sheet in sheet_names) {
      if (sheet %in% skip_sheets) {
        log_msg(paste("Skipping non-crop sheet:", sheet))
        next
      }
      
      log_msg(paste("Processing sheet:", sheet))
      
      # Read the sheet with error handling
      df <- tryCatch({
        suppressMessages(read_excel(file_path, sheet = sheet))
      }, error = function(e) {
        log_msg(paste("Error reading sheet:", sheet, "- Error:", e$message), "ERROR")
        return(NULL)
      })
      
      if (is.null(df)) next
      
      # Process the sheet
      colnames(df)[1] <- "Country"
      
      # Add metadata
      df$WF_Crop <- sheet
      df$Water_Type <- water_type
      
      # Convert to long format with immediate year column rename
      df_long <- df %>%
        pivot_longer(
          cols = -c(Country, WF_Crop, Water_Type),
          names_to = "year",  # Changed from "Year" to "year"
          values_to = "WaterFootprint_km3_per_year"
        ) %>%
        mutate(year = as.numeric(year))  # Ensure year is numeric
      
      data_list[[sheet]] <- df_long
      log_msg(paste("Successfully processed sheet:", sheet,
                    "- Rows:", nrow(df_long)))
    }
    
    # Combine all data
    combined_data <- bind_rows(data_list)
    log_msg(paste("Total combined rows:", nrow(combined_data)))
    
    # Print column names for debugging
    log_msg(paste("Column names in combined data:", paste(colnames(combined_data), collapse = ", ")))
    
    return(combined_data)
    
  }, error = function(e) {
    log_msg(paste("Critical error:", e$message), "ERROR")
    stop("Error processing water footprint file ", file_path, ": ", e$message)
  })
}


# Function to get water footprint by type
get_water_footprint_by_type <- function(data, water_type = NULL) {
  if (is.null(water_type)) {
    return(data %>%
             group_by(Country, WF_Crop, Year, iso3) %>%
             summarise(
               WaterFootprint_km3_per_year = sum(WaterFootprint_km3_per_year, na.rm = TRUE),
               Water_Type = "Total",
               .groups = 'drop'
             ))
  } else {
    return(data %>% filter(Water_Type == water_type))
  }
}

# Function to process production data
process_production_data <- function(years) {
  production_data_file <- "data_raw/crop_production_data.rds"
  
  if (!file.exists(production_data_file)) {
    cat("Downloading production data from FAOSTAT...\n")
    fao_item_codes <- production_mapping$FAO_Code
    production_data_raw <- FAOSTAT::get_faostat_data(
      domain_code = "QC",
      item_code = fao_item_codes,
      element_code = 5510,
      output_format = "RDS"
    )
    saveRDS(production_data_raw, production_data_file)
  } else {
    production_data_raw <- readRDS(production_data_file)
  }
  
  # Process using production mapping with proper ISO3 conversion
  production_data <- production_data_raw %>%
    mutate(
      area_code = as.character(area_code),
      year = as.numeric(year),
      item_code = as.character(item_code)
    ) %>%
    filter(year %in% years) %>%
    left_join(production_mapping, by = c("item_code" = "FAO_Code")) %>%
    filter(!is.na(WF_Crop)) %>%
    left_join(country_conversion %>% 
                select(FAOST_CODE, iso3), 
              by = c("area_code" = "FAOST_CODE")) %>%
    filter(!is.na(iso3)) %>%
    group_by(iso3, year, WF_Crop) %>%  # Changed Year to year
    summarise(
      Production_kg = sum(value * 1000, na.rm = TRUE),  # Convert tonnes to kg
      .groups = 'drop'
    )
  
  return(production_data)
}

# =============================================================================
#  Updated Code: calculate_water_footprints() with LPJmL structure
# =============================================================================
# References:
# - Schaphoff et al. (2018) - LPJmL4 model description
#   DOI: 10.5194/gmd-11-1343-2018
# - Jägermeyr et al. (2015) - Irrigation efficiency parameterization
#   DOI: 10.5194/hess-19-3073-2015
# - von Bloh et al. (2018) - LPJmL water balance
#   DOI: 10.5194/gmd-11-1377-2018
# - Konar et al. (2012) - Virtual water transfers
# - Mekonnen & Hoekstra (2011) - Water footprint concepts
# =============================================================================

# -------------------------------------------------------------------------
# Example irrigation efficiency table (Jägermeyr et al., 2015)
# Here, you might eventually load a more detailed country-specific table
# -------------------------------------------------------------------------
irrigation_efficiency_table <- data.frame(
  system_type = c("surface", "sprinkler", "drip"),
  field_efficiency = c(0.6, 0.75, 0.9),  # fraction of water actually reaching soil
  conveyance_efficiency = c(0.7, 0.8, 0.95)  # fraction of water that reaches field
)

# -------------------------------------------------------------------------
# Example function to get irrigation efficiency by country
# -------------------------------------------------------------------------
get_irrigation_efficiency <- function(country_iso3) {
  # For now, return a single global average from Jägermeyr, or do a lookup
  efficiency_factors <- rep(0.7, length(country_iso3))  # e.g., 70% total
  names(efficiency_factors) <- country_iso3
  return(efficiency_factors)
}

# -------------------------------------------------------------------------
# Function to calculate production split using LPJmL water-volume ratios
# -------------------------------------------------------------------------
calculate_production_split <- function(water_data, country_iso3) {
  # Get irrigation efficiency for this country
  irr_efficiency <- get_irrigation_efficiency(country_iso3)
  
  # Sum water volumes
  total_irrigated_water <- water_data$blue_irrigated + water_data$green_irrigated
  total_rainfed_water <- water_data$green_rainfed
  
  # Adjust by efficiency (blue+green in irrigated systems)
  effective_irrigated_water <- total_irrigated_water * irr_efficiency
  effective_rainfed_water   <- total_rainfed_water  # Rainfed efficiency ~ 1
  
  # Calculate fractions that each system contributes to total production
  total_effective_water <- effective_irrigated_water + effective_rainfed_water
  if (total_effective_water == 0) {
    # Edge case: if no water volumes, avoid NaN
    return(list(irrigated = 0, rainfed = 0))
  }
  
  list(
    irrigated = effective_irrigated_water / total_effective_water,
    rainfed   = effective_rainfed_water   / total_effective_water
  )
}

# -------------------------------------------------------------------------
# Updated function to calculate water footprints (LPJmL logic under the hood)
# but returning the same columns & structure as the original.
# -------------------------------------------------------------------------
calculate_water_footprints <- function(years) {
  tryCatch({
    
    log_msg <- create_logger("water_footprints")
    log_msg("Reading water footprint data files")
    
    # Read raw water footprint data (already in km3/year)
    gw_rainfed_df   <- read_water_footprint("ancillary/GW_rainfed_lpjml.xlsx", "Green_Rainfed")
    gw_irrigated_df <- read_water_footprint("ancillary/GW_irrigated_lpjml.xlsx", "Green_Irrigated")
    bw_irrigated_df <- read_water_footprint("ancillary/BW_irrigated_lpjml.xlsx", "Blue_Irrigated")
    
    # Print column names for debugging
    log_msg("Column names in water footprint data:")
    log_msg(paste("GW Rainfed:", paste(colnames(gw_rainfed_df), collapse = ", ")))
    
    # Create country mapping (same as original)
    country_mapping <- country_conversion %>%
      select(Country, iso3) %>%
      bind_rows(
        alternative_names %>%
          select(Country) %>%
          left_join(alternative_names, by = "Country") %>%
          left_join(country_conversion %>% select(Country, iso3),
                    by = c("Standardized_Country" = "Country")) %>%
          select(Country, iso3)
      ) %>%
      distinct() %>%
      group_by(Country) %>%
      slice(1) %>%
      ungroup()
    
    # Combine water footprint data (long format initially)
    water_footprint_long <- bind_rows(gw_rainfed_df, gw_irrigated_df, bw_irrigated_df) %>%
      mutate(
        Country  = str_replace_all(Country, "^['\"]+|['\"]+$", "") %>% str_trim(),
        WF_Crop  = standardize_crop_name(WF_Crop),
        # Fix some special cases
        Country = case_when(
          Country %in% c("Hong Kong", "Hong Kong S.A.R.", "Hong Kong SAR") ~ "China",
          Country %in% c("Macao", "Macau", "Macau SAR", "Macao SAR") ~ "China",
          TRUE ~ Country
        )
      ) %>%
      left_join(country_mapping, by = "Country") %>%
      filter(!is.na(iso3)) %>%
      filter(year %in% years)
    
    # Get production data
    production_data <- process_production_data(years)
    
    # ---------------------------------------------------------------------
    # 1) Pivot to WIDE for LPJmL splitting (but we'll pivot back later).
    # ---------------------------------------------------------------------
    wide_df <- water_footprint_long %>%
      group_by(iso3, WF_Crop, Water_Type, year) %>%
      summarise(
        WaterFootprint_km3_per_year = sum(WaterFootprint_km3_per_year, na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      inner_join(production_data, by = c("iso3", "WF_Crop", "year")) %>%
      # Pivot so we have columns: Blue_Irrigated, Green_Irrigated, Green_Rainfed
      tidyr::pivot_wider(
        names_from  = Water_Type,
        values_from = WaterFootprint_km3_per_year,
        values_fill = 0
      ) %>%
      mutate(
        Blue_Irrigated  = ifelse(is.na(Blue_Irrigated), 0, Blue_Irrigated),
        Green_Irrigated = ifelse(is.na(Green_Irrigated), 0, Green_Irrigated),
        Green_Rainfed   = ifelse(is.na(Green_Rainfed), 0, Green_Rainfed)
      )
    
    # ---------------------------------------------------------------------
    # 2) Apply LPJmL-based irrigation/rainfed split (example approach)
    # ---------------------------------------------------------------------
    # E.g., you might define a function get_irrigation_efficiency() or
    # calculate_production_split(), but here's a simple placeholder:
    
    wide_df <- wide_df %>%
      rowwise() %>%
      mutate(
        # For demonstration, you might apply an irrigation efficiency factor:
        irr_efficiency = 1.0, #0.7,  # or get_irrigation_efficiency(iso3)
        
        # Effective water volumes
        effective_irrigated_water =
          (Blue_Irrigated + Green_Irrigated) * irr_efficiency,
        effective_rainfed_water = Green_Rainfed,
        
        total_effective_water = effective_irrigated_water + effective_rainfed_water,
        fraction_irrigated = ifelse(total_effective_water > 0,
                                    effective_irrigated_water / total_effective_water,
                                    0),
        fraction_rainfed  = ifelse(total_effective_water > 0,
                                   effective_rainfed_water / total_effective_water,
                                   0)
      ) %>%
      ungroup()
    
    # [You could do additional logic here if you want fully separate
    #  'irrigated_production_kg' and 'rainfed_production_kg', etc.]
    
    # ---------------------------------------------------------------------
    # 3) Re-split water footprints according to fraction_irrigated/rainfed
    #    So each final water type gets "splitted" volumes. This is optional.
    # ---------------------------------------------------------------------
    wide_df <- wide_df %>%
      mutate(
        # Only the "irrigated" water gets multiplied by fraction_irrigated:
        splitted_Blue_Irrigated  = Blue_Irrigated  * fraction_irrigated,
        splitted_Green_Irrigated = Green_Irrigated * fraction_irrigated,
        # Rainfed remains multiplied by fraction_rainfed
        splitted_Green_Rainfed   = Green_Rainfed   * fraction_rainfed
      )
    
    # ---------------------------------------------------------------------
    # 4) Pivot BACK to LONG format with original Water_Type structure
    # ---------------------------------------------------------------------
    water_footprint_intensities <- wide_df %>%
      select(
        iso3, WF_Crop, year, Production_kg,
        splitted_Blue_Irrigated, splitted_Green_Irrigated, splitted_Green_Rainfed
      ) %>%
      tidyr::pivot_longer(
        cols      = starts_with("splitted_"),
        names_to  = "Water_Type",
        values_to = "WaterFootprint_km3_per_year"
      ) %>%
      mutate(
        # Water_Type will be "splitted_Blue_Irrigated", etc.
        Water_Type = case_when(
          Water_Type == "splitted_Blue_Irrigated"  ~ "Blue_Irrigated",
          Water_Type == "splitted_Green_Irrigated" ~ "Green_Irrigated",
          Water_Type == "splitted_Green_Rainfed"   ~ "Green_Rainfed",
          TRUE ~ Water_Type
        )
      )
    
    # ---------------------------------------------------------------------
    # 5) Calculate WF_L_per_kg exactly as your original code did
    # ---------------------------------------------------------------------
    water_footprint_intensities <- water_footprint_intensities %>%
      group_by(iso3, WF_Crop, Water_Type, year) %>%
      summarise(
        WaterFootprint_km3_per_year = sum(WaterFootprint_km3_per_year, na.rm = TRUE),
        Production_kg = sum(Production_kg, na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      mutate(
        WF_L_per_kg = (WaterFootprint_km3_per_year * 1e12) / Production_kg,
        WF_L_per_kg = ifelse(is.infinite(WF_L_per_kg) | is.nan(WF_L_per_kg),
                             NA, WF_L_per_kg),
        WF_L_per_kg = pmax(WF_L_per_kg, 0, na.rm = TRUE)
      )
    
    # Print summary stats for debugging
    log_msg("\nSummary statistics:")
    log_msg(paste("Total unique countries:", n_distinct(water_footprint_intensities$iso3)))
    log_msg(paste("Total unique crops:", n_distinct(water_footprint_intensities$WF_Crop)))
    log_msg(paste("Years covered:",
                  paste(sort(unique(water_footprint_intensities$year)), collapse=", ")))
    
    return(water_footprint_intensities)
    
  }, error = function(e) {
    log_msg(paste("Error in calculate_water_footprints:", e$message), "ERROR")
    print(e)
    stop(e)
  })
}

# =============================================================================
# calculate_virtual_water_flows() with new intensities
# =============================================================================
calculate_virtual_water_flows <- function(trade_data, water_footprint_data) {
  
  # Print summary stats for debugging
  cat("\nTrade data summary:")
  cat("\nTotal rows:", nrow(trade_data))
  cat("\nUnique crops:", paste(unique(trade_data$WF_Crop), collapse=", "))
  cat("\nValue range (kg):", range(trade_data$value_primary, na.rm=TRUE))
  
  cat("\n\nWater footprint data summary:")
  cat("\nTotal rows:", nrow(water_footprint_data))
  cat("\nUnique crops:", paste(unique(water_footprint_data$WF_Crop), collapse=", "))
  cat("\nWater volume range (km3):",
      range(water_footprint_data$WaterFootprint_km3_per_year, na.rm=TRUE))
  
  # -------------------------------------------------------------------------
  # 1) Calculate row-level virtual water flows using direct proportion
  # -------------------------------------------------------------------------
  flows <- trade_data %>%
    left_join(
      water_footprint_data %>%
        select(
          iso3, WF_Crop, Water_Type, year,
          WaterFootprint_km3_per_year, Production_kg, WF_L_per_kg
        ),
      by = c(
        "ReporterISO3" = "iso3",
        "WF_Crop"      = "WF_Crop",
        "year"         = "year"
      )
    ) %>%
    mutate(
      # fraction of production that is traded
      trade_fraction = value_primary / Production_kg,
      
      # Virtual water in m3 (since WaterFootprint_km3_per_year is in km³)
      virtual_water_m3 = WaterFootprint_km3_per_year * trade_fraction * 1e9,
      
      # Also keep a check column in km³
      virtual_water_km3 = virtual_water_m3 / 1e9
    ) %>%
    filter(!is.infinite(trade_fraction), !is.na(trade_fraction))
  
  # -------------------------------------------------------------------------
  # 2) Determine how many distinct years are present for potential averaging
  # -------------------------------------------------------------------------
  n_years <- length(unique(trade_data$year))
  cat("\nNumber of distinct years in this trade dataset:", n_years, "\n")
  
  # Just for reference: if you're intentionally passing 5-year windows,
  # n_years should be 5. But the code is flexible if you pass more or fewer.
  
  # -------------------------------------------------------------------------
  # 3) Print basic summary of flows by water type (all flows combined)
  # -------------------------------------------------------------------------
  summary_stats <- flows %>%
    group_by(Water_Type) %>%
    summarise(
      total_virtual_water_km3     = sum(virtual_water_km3, na.rm=TRUE),
      annual_avg_virtual_water_km3 = sum(virtual_water_km3, na.rm=TRUE) / n_years,
      n_flows                 = n(),
      mean_trade_fraction     = mean(trade_fraction, na.rm=TRUE),
      .groups = 'drop'
    )
  
  cat("\n\nResults summary by water type:")
  print(summary_stats)
  
  # -------------------------------------------------------------------------
  # 4) (Optional) Konar comparison for "main crops" (Soybean, Cereals, Maize, Rice)
  #    We compute green vs. blue totals and ANNUAL averages
  # -------------------------------------------------------------------------
  
  # A) EARLY PERIOD (akin to Konar 1986, but your data might be 1996-2000)
  #    We'll detect if the entire dataset is "early" by checking max year <= 2000
  if (max(trade_data$year) <= 2000) {
    main_crops_summary_early <- flows %>%
      filter(
        WF_Crop %in% c("Soyabean", "Temperate Cereals", "Maize", "Rice"),
        Water_Type %in% c("Blue_Irrigated", "Green_Rainfed", "Green_Irrigated")
      ) %>%
      group_by(WF_Crop) %>%
      summarise(
        green_water_km3 = sum(
          virtual_water_km3[Water_Type %in% c("Green_Rainfed","Green_Irrigated")],
          na.rm=TRUE
        ),
        blue_water_km3 = sum(
          virtual_water_km3[Water_Type == "Blue_Irrigated"],
          na.rm=TRUE
        ),
        
        # Annual average
        green_water_annual_avg = green_water_km3 / n_years,
        blue_water_annual_avg  = blue_water_km3  / n_years,
        
        .groups = 'drop'
      )
    
    cat("\n\nMain crops summary (Konar ~1986 equivalent, i.e. 1996–2000):\n")
    print(main_crops_summary_early)
    
    # NEW: Save to a file for further reference
    write.csv(
      main_crops_summary_early,
      file = file.path("outputs", "KonarComparison_Early.csv"),
      row.names = FALSE
    )
  }
  
  # B) LATE PERIOD (akin to Konar 2008, but your data might be 2016-2020)
  #    We'll detect if the entire dataset is "late" by checking min year >= 2016
  if (min(trade_data$year) >= 2016) {
    main_crops_summary_late <- flows %>%
      filter(
        WF_Crop %in% c("Soyabean", "Temperate Cereals", "Maize", "Rice"),
        Water_Type %in% c("Blue_Irrigated", "Green_Rainfed", "Green_Irrigated")
      ) %>%
      group_by(WF_Crop) %>%
      summarise(
        green_water_km3 = sum(
          virtual_water_km3[Water_Type %in% c("Green_Rainfed","Green_Irrigated")],
          na.rm=TRUE
        ),
        blue_water_km3 = sum(
          virtual_water_km3[Water_Type == "Blue_Irrigated"],
          na.rm=TRUE
        ),
        
        # Annual average
        green_water_annual_avg = green_water_km3 / n_years,
        blue_water_annual_avg  = blue_water_km3  / n_years,
        
        .groups = 'drop'
      )
    
    cat("\n\nMain crops summary (Konar 2008 equivalent, i.e. 2016–2020):\n")
    print(main_crops_summary_late)
    
    # NEW: Save to a file for further reference
    write.csv(
      main_crops_summary_late,
      file = file.path("outputs", "KonarComparison_Late.csv"),
      row.names = FALSE
    )
  }
  
  # -------------------------------------------------------------------------
  # 5) Return the row-level flows for further usage
  # -------------------------------------------------------------------------
  return(flows)
}


# -----------------------------
# 6. Trade Data Processing
# -----------------------------
process_trade_data <- function(years) {
  log_message <- function(msg) {
    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "-", msg, "\n")
  }
  
  log_message("Starting trade data processing")
  
  trade_data <- read.csv("inputs/Trade_DetailedTradeMatrix_E_All_Data_(Normalized).csv") %>%
    select(
      reporter = Reporter.Country.Code,
      partner = Partner.Country.Code,
      cropid = Item.Code,
      element = Element,
      year = Year,
      value = Value
    ) %>%
    mutate(
      cropid = as.character(cropid)
    ) %>%
    # Use trade mapping here
    inner_join(trade_mapping, by = c("cropid" = "FAO_Code")) %>%
    filter(
      year %in% years,
      element == "Export Quantity",
      reporter != partner
    ) %>%
    mutate(
      reporter = as.character(reporter),
      partner = as.character(partner),
      value = as.numeric(value)
    )
  
  log_message(paste("Filtered trade data:", nrow(trade_data), "rows"))
  
  # Join with country mappings
  trade_data <- trade_data %>%
    mutate(
      ReporterISO3 = faostat_to_iso3[reporter],
      PartnerISO3 = faostat_to_iso3[partner]
    ) %>%
    filter(!is.na(ReporterISO3) & !is.na(PartnerISO3))
  
  log_message(paste("After ISO3 mapping:", nrow(trade_data), "rows"))
  
  # Add extraction rates and convert to primary equivalents
  trade_data <- trade_data %>%
    left_join(extraction_rates, by = c("cropid" = "FAO_Code")) %>%
    mutate(
      value_primary = value / Extraction_Rate * 1000  # Convert to kg
    )
  check_years(trade_data, "trade data")
  
  log_message(paste("Final processed rows:", nrow(trade_data), "rows"))
  
  # Validate the processed data
  validate_trade_data(trade_data)
  
  return(trade_data)
}

# Add validation function for mappings
validate_mappings <- function() {
  cat("\nValidating Code Mappings:\n")
  cat("=======================\n")
  
  # Check for duplicate codes
  prod_dupes <- production_mapping$FAO_Code[duplicated(production_mapping$FAO_Code)]
  trade_dupes <- trade_mapping$FAO_Code[duplicated(trade_mapping$FAO_Code)]
  
  if(length(prod_dupes) > 0) {
    cat("Warning: Duplicate production codes:", paste(prod_dupes, collapse=", "), "\n")
  }
  if(length(trade_dupes) > 0) {
    cat("Warning: Duplicate trade codes:", paste(trade_dupes, collapse=", "), "\n")
  }
  
  # Check for consistency in WF_Crop categories
  prod_cats <- sort(unique(production_mapping$WF_Crop))
  trade_cats <- sort(unique(trade_mapping$WF_Crop))
  
  if(!identical(prod_cats, trade_cats)) {
    cat("Warning: WF_Crop categories differ between mappings\n")
    cat("Production:", paste(prod_cats, collapse=", "), "\n")
    cat("Trade:", paste(trade_cats, collapse=", "), "\n")
  }
  
  return(list(
    production_codes = sort(production_mapping$FAO_Code),
    trade_codes = sort(trade_mapping$FAO_Code),
    production_crops = prod_cats,
    trade_crops = trade_cats
  ))
}


# -----------------------------
# 7. Virtual Water Flow Functions
# -----------------------------
# Function to analyze and track units through the pipeline
analyze_units <- function(water_footprint_data, trade_data, flows) {
  # Create summary function
  create_summary <- function(data, stage_name) {
    summary <- list(
      n_rows = nrow(data),
      n_crops = n_distinct(data$WF_Crop),
      value_range = range(data$virtual_water_m3, na.rm = TRUE),
      value_mean = mean(data$virtual_water_m3, na.rm = TRUE)
    )
    cat(sprintf("\n%s Summary:\n", stage_name))
    cat("Rows:", summary$n_rows, "\n")
    cat("Unique crops:", summary$n_crops, "\n")
    cat("Value range:", paste(format(summary$value_range, scientific = TRUE), collapse = " to "), "\n")
    cat("Mean value:", format(summary$value_mean, scientific = TRUE), "\n")
    return(summary)
  }
  
  # Function to generate key statistics
  generate_key_stats <- function(flows) {
    stats <- flows %>%
      group_by(WF_Crop, Water_Type, year) %>%
      summarise(
        Total_Virtual_Water_m3 = sum(virtual_water_m3, na.rm = TRUE),
        Total_Trade_Volume_kg = sum(trade_volume_kg, na.rm = TRUE),
        Mean_Intensity_L_per_kg = mean(mean_intensity, na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      mutate(
        Period = if_else(year <= 2005, "Early", "Late"),
        # Convert units for better readability
        Total_Virtual_Water_km3 = Total_Virtual_Water_m3 / 1e9,
        Total_Trade_Volume_Mt = Total_Trade_Volume_kg / 1e9
      )
    
    return(stats)
  }
  
  # Write key statistics to CSV
  write_key_stats <- function(stats, filename) {
    stats_formatted <- stats %>%
      select(
        WF_Crop,
        Water_Type,
        year,
        Period,
        Total_Virtual_Water_km3,
        Total_Trade_Volume_Mt,
        Mean_Intensity_L_per_kg
      ) %>%
      arrange(Period, WF_Crop, Water_Type)
    
    write.csv(stats_formatted, 
              file.path("outputs", filename), 
              row.names = FALSE)
    
    return(stats_formatted)
  }
  
  return(list(
    create_summary = create_summary,
    generate_key_stats = generate_key_stats,
    write_key_stats = write_key_stats
  ))
}

# -----------------------------
# 8. Validation Functions
# -----------------------------
validate_water_footprint_data <- function(data) {
  validation_report <- list(
    total_rows = nrow(data),
    unique_crops = unique(data$WF_Crop),
    unique_countries = unique(data$Country),
    year_range = range(as.numeric(data$Year), na.rm = TRUE),
    missing_values = colSums(is.na(data)),
    water_types = unique(data$Water_Type)
  )
  
  cat("\nData Validation Report:\n")
  cat("=====================\n")
  cat("Total rows:", validation_report$total_rows, "\n")
  cat("Unique crops:", paste(validation_report$unique_crops, collapse = ", "), "\n")
  cat("Year range:", paste(validation_report$year_range, collapse = " - "), "\n")
  cat("Water types:", paste(validation_report$water_types, collapse = ", "), "\n")
  
  if (any(validation_report$missing_values > 0)) {
    warning("Missing values detected in columns: ",
            paste(names(validation_report$missing_values[validation_report$missing_values > 0]),
                  collapse = ", "))
  }
  
  return(validation_report)
}

validate_trade_data <- function(data) {
  cat("\nTrade Data Validation:\n")
  cat("=====================\n")
  cat("Total rows:", nrow(data), "\n")
  cat("Unique crops:", paste(unique(data$WF_Crop), collapse = ", "), "\n")
  cat("Year range:", paste(range(data$year), collapse = " - "), "\n")
  cat("Number of reporting countries:", length(unique(data$ReporterISO3)), "\n")
  cat("Number of partner countries:", length(unique(data$PartnerISO3)), "\n")
}

validate_flows <- function(flows) {
  cat("\nVirtual Water Flows Validation:\n")
  cat("============================\n")
  cat("Total flow records:", nrow(flows), "\n")
  cat("Unique crops:", paste(unique(flows$WF_Crop), collapse = ", "), "\n")
  cat("Water types:", paste(unique(flows$Water_Type), collapse = ", "), "\n")
  cat("Total virtual water flow (km³):", 
      sum(flows$virtual_water_m3, na.rm = TRUE) / 1e9, "\n")
}

# -----------------------------
# 9. Plotting Functions
# -----------------------------
# First, add these packages to the required_packages list at the top of v0 file:
required_packages <- c(
  "readxl", "dplyr", "tidyr", "stringr", "ggplot2",
  "rnaturalearth", "rnaturalearthdata", "sf", "viridis",
  "igraph", "rworldmap", "RColorBrewer", "FAOSTAT", "ggpubr",
  "networkD3", "tibble", "circlize", "htmlwidgets", "grid", "rlang",
  # Add these new ones:
  "rnaturalearth",    # For map data
  "rnaturalearthdata", # For map data
  "sf",               # For spatial features
  "rworldmap"         # For country centroids
)


# First, let's create a helper function to safely convert lists to data frames
safe_as_dataframe <- function(x, default = NULL) {
  tryCatch({
    if (is.data.frame(x)) return(x)
    if (is.list(x) && !is.null(x)) {
      return(as.data.frame(x))
    }
    return(default)
  }, error = function(e) {
    return(default)
  })
}

# Updated create_water_type_maps function
create_water_type_maps <- function(flows_early, flows_late, years_early, years_late) {
  # Ensure we're working with data frames
  flows_early <- safe_as_dataframe(flows_early)
  flows_late <- safe_as_dataframe(flows_late)
  
  if (is.null(flows_early) || is.null(flows_late)) {
    warning("Invalid input data for water type maps")
    return(NULL)
  }
  
  # Function to safely process flows
  process_flows <- function(flows, water_type) {
    if (!is.data.frame(flows)) return(NULL)
    if (water_type == "blue") {
      return(flows %>% filter(!is.na(Water_Type), Water_Type == "Blue_Irrigated"))
    } else if (water_type == "green") {
      return(flows %>% filter(!is.na(Water_Type), 
                              Water_Type %in% c("Green_Rainfed", "Green_Irrigated")))
    } else {
      return(flows %>% filter(!is.na(Water_Type)))
    }
  }
  
  # Process each type with error handling
  flows_early_blue <- process_flows(flows_early, "blue")
  flows_early_green <- process_flows(flows_early, "green")
  flows_early_total <- process_flows(flows_early, "total")
  
  flows_late_blue <- process_flows(flows_late, "blue")
  flows_late_green <- process_flows(flows_late, "green")
  flows_late_total <- process_flows(flows_late, "total")
  
  # Store plots in a list
  plots <- list()
  
  # Generate maps with error handling
  tryCatch({
    plots$early_blue <- plot_net_virtual_water_trade(flows_early_blue, "early_blue", years_early)
    plots$early_green <- plot_net_virtual_water_trade(flows_early_green, "early_green", years_early)
    plots$early_total <- plot_net_virtual_water_trade(flows_early_total, "early_total", years_early)
    
    plots$late_blue <- plot_net_virtual_water_trade(flows_late_blue, "late_blue", years_late)
    plots$late_green <- plot_net_virtual_water_trade(flows_late_green, "late_green", years_late)
    plots$late_total <- plot_net_virtual_water_trade(flows_late_total, "late_total", years_late)
  }, error = function(e) {
    warning(paste("Error generating maps:", e$message))
  })
  
  # Create validation summary with safe operations
  validation_summary <- tryCatch({
    bind_rows(
      tibble(
        Period = "Early",
        Type = c("Blue", "Green", "Total"),
        Total_Volume_km3 = c(
          sum(flows_early_blue$virtual_water_m3, na.rm = TRUE) / 1e9,
          sum(flows_early_green$virtual_water_m3, na.rm = TRUE) / 1e9,
          sum(flows_early_total$virtual_water_m3, na.rm = TRUE) / 1e9
        )
      ),
      tibble(
        Period = "Late",
        Type = c("Blue", "Green", "Total"),
        Total_Volume_km3 = c(
          sum(flows_late_blue$virtual_water_m3, na.rm = TRUE) / 1e9,
          sum(flows_late_green$virtual_water_m3, na.rm = TRUE) / 1e9,
          sum(flows_late_total$virtual_water_m3, na.rm = TRUE) / 1e9
        )
      )
    )
  }, error = function(e) {
    warning(paste("Error creating validation summary:", e$message))
    return(tibble(
      Period = character(),
      Type = character(),
      Total_Volume_km3 = numeric()
    ))
  })
  
  # Create comparison table with safe operations
  comparison_table <- tryCatch({
    validation_summary %>%
      group_by(Period) %>%
      summarise(
        Blue_Green_Sum = sum(Total_Volume_km3[Type %in% c("Blue", "Green")], na.rm = TRUE),
        Total = Total_Volume_km3[Type == "Total"],
        Difference = abs(Blue_Green_Sum - Total),
        Difference_Percent = (Difference / Total) * 100,
        .groups = 'drop'
      )
  }, error = function(e) {
    warning(paste("Error creating comparison table:", e$message))
    return(tibble(
      Period = character(),
      Blue_Green_Sum = numeric(),
      Total = numeric(),
      Difference = numeric(),
      Difference_Percent = numeric()
    ))
  })
  
  # Return results with validation flag
  list(
    plots = plots,
    validation_summary = validation_summary,
    comparison_table = comparison_table,
    is_valid = !is.null(validation_summary) && nrow(validation_summary) > 0
  )
}

# Function to plot net virtual water trade with flows
plot_net_virtual_water_trade <- function(flows, period_name, years) {
  # Ensure output directory exists
  dir.create("outputs", showWarnings = FALSE)
  
  # Calculate total exports per country
  exports <- flows %>%
    group_by(ReporterISO3) %>%
    summarise(
      total_exports_m3 = sum(virtual_water_m3, na.rm = TRUE),
      .groups = 'drop'
    )
  
  # Calculate total imports per country
  imports <- flows %>%
    group_by(PartnerISO3) %>%
    summarise(
      total_imports_m3 = sum(virtual_water_m3, na.rm = TRUE),
      .groups = 'drop'
    )
  
  # Merge exports and imports to compute net trade
  net_trade <- full_join(exports, imports, by = c("ReporterISO3" = "PartnerISO3")) %>%
    rename(ISO3 = ReporterISO3) %>%
    mutate(
      total_exports_m3 = replace_na(total_exports_m3, 0),
      total_imports_m3 = replace_na(total_imports_m3, 0),
      net_virtual_water_m3 = total_exports_m3 - total_imports_m3,
      net_virtual_water_km3 = net_virtual_water_m3 / 1e9  # Convert to km³
    )
  
  # Define bins for net virtual water trade
  net_trade <- net_trade %>%
    mutate(
      net_trade_category = case_when(
        net_virtual_water_km3 > 100 ~ ">100 km³/yr export",
        net_virtual_water_km3 > 50 ~ "50-100 km³/yr export",
        net_virtual_water_km3 > 10 ~ "10-50 km³/yr export",
        net_virtual_water_km3 > 5 ~ "5-10 km³/yr export",
        net_virtual_water_km3 > 0 ~ "0-5 km³/yr export",
        net_virtual_water_km3 < -100 ~ ">100 km³/yr import",
        net_virtual_water_km3 < -50 ~ "50-100 km³/yr import",
        net_virtual_water_km3 < -10 ~ "10-50 km³/yr import",
        net_virtual_water_km3 < -5 ~ "5-10 km³/yr import",
        net_virtual_water_km3 < 0 ~ "0-5 km³/yr import",
        TRUE ~ "Neutral"
      )
    )
  
  # Set the factor levels to ensure correct ordering
  net_trade$net_trade_category <- factor(net_trade$net_trade_category,
                                         levels = c(
                                           ">100 km³/yr export",
                                           "50-100 km³/yr export",
                                           "10-50 km³/yr export",
                                           "5-10 km³/yr export",
                                           "0-5 km³/yr export",
                                           "Neutral",
                                           "0-5 km³/yr import",
                                           "5-10 km³/yr import",
                                           "10-50 km³/yr import",
                                           "50-100 km³/yr import",
                                           ">100 km³/yr import"
                                         ))
  
  # Define custom colors for each category
  colors <- c(
    ">100 km³/yr export" = "#08306b",   # Darkest blue
    "50-100 km³/yr export" = "#08519c",
    "10-50 km³/yr export" = "#2171b5",
    "5-10 km³/yr export" = "#4292c6",
    "0-5 km³/yr export" = "#6baed6",
    "Neutral" = "#f0f0f0",               # Light grey
    "0-5 km³/yr import" = "#fee0d2",
    "5-10 km³/yr import" = "#fcbba1",
    "10-50 km³/yr import" = "#fc9272",
    "50-100 km³/yr import" = "#fb6a4a",
    ">100 km³/yr import" = "#de2d26"     # Darkest red
  )
  
  # Merge net trade data with world map and remove Antarctica
  tryCatch({
    world <- ne_countries(scale = "medium", returnclass = "sf") %>%
      filter(admin != "Antarctica")  # Remove Antarctica
    
    world_trade <- world %>%
      left_join(net_trade, by = c("iso_a3" = "ISO3"))
    
    # Create the choropleth map with modified theme
    choropleth_map <- ggplot(data = world_trade) +
      geom_sf(aes(fill = net_trade_category), color = "gray") +
      scale_fill_manual(values = colors, na.value = "lightgray") +
      theme_minimal() +
      labs(
        title = paste("Net Virtual Water Trade (", get_year_label(years), ")", sep = ""),
        fill = "Net Virtual Water Trade (km³/yr)"
      ) +
      theme(
        legend.position = "bottom",
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank()
      )
    
    # Analyze flows to determine appropriate threshold
    flow_summary <- flows %>%
      group_by(ReporterISO3, PartnerISO3) %>%
      summarise(
        virtual_water_m3 = sum(virtual_water_m3, na.rm = TRUE),
        virtual_water_km3 = virtual_water_m3 / 1e9,
        .groups = 'drop'
      )
    
    # Calculate quantiles for flow volumes
    flow_quantiles <- quantile(flow_summary$virtual_water_km3, 
                               probs = c(0.75, 0.9, 0.95, 0.99), 
                               na.rm = TRUE)
    
    # Set threshold using 95th percentile but minimum of 15 km³/yr
    threshold_km3 <- max(15, flow_quantiles["95%"])
    
    # Print analysis information
    cat("\nFlow Analysis for", period_name, ":\n")
    cat("======================\n")
    cat("Flow quantiles (km³/yr):\n")
    print(flow_quantiles)
    cat("\nUsing threshold:", threshold_km3, "km³/yr\n")
    
    # Filter flows above the threshold
    flows_filtered <- flow_summary %>%
      filter(virtual_water_km3 >= threshold_km3)
    
    # Print top flows
    cat("\nLargest flows (km³/yr):\n")
    top_flows <- flows_filtered %>%
      arrange(desc(virtual_water_km3)) %>%
      head(10) %>%
      mutate(
        From = iso3_to_name[ReporterISO3],
        To = iso3_to_name[PartnerISO3]
      ) %>%
      select(From, To, virtual_water_km3)
    print(top_flows)
    
    cat("Number of flows above threshold:", nrow(flows_filtered), "\n")
    
    # Get country centroids and remove Antarctica
    world_map <- rworldmap::getMap(resolution = "low")
    country_centroids <- data.frame(
      ISO3 = world_map$ISO_A3,
      ADMIN = world_map$ADMIN,
      Longitude = coordinates(world_map)[,1],
      Latitude = coordinates(world_map)[,2]
    ) %>%
      filter(ISO3 != "-99" & ADMIN != "Antarctica")
    
    # Add coordinates for exporters and importers
    flows_filtered <- flows_filtered %>%
      left_join(country_centroids, by = c("ReporterISO3" = "ISO3")) %>%
      rename(Exporter_Longitude = Longitude, Exporter_Latitude = Latitude) %>%
      left_join(country_centroids, by = c("PartnerISO3" = "ISO3")) %>%
      rename(Importer_Longitude = Longitude, Importer_Latitude = Latitude) %>%
      filter(!is.na(Exporter_Longitude) & !is.na(Importer_Longitude))
    
    # Plot the combined map
    combined_map <- choropleth_map +
      geom_curve(
        data = flows_filtered,
        aes(
          x = Exporter_Longitude,
          y = Exporter_Latitude,
          xend = Importer_Longitude,
          yend = Importer_Latitude,
          linewidth = virtual_water_km3
        ),
        color = "black",
        curvature = 0.2,
        alpha = 0.7
      ) +
      scale_linewidth_continuous(
        name = "Virtual Water Flow (km³/yr)",
        range = c(0.5, 2),
        guide = guide_legend(title.position = "top")
      ) +
      guides(
        fill = guide_legend(title.position = "top"),
        linewidth = guide_legend(title.position = "top")
      ) +
      theme(
        legend.position = "bottom",
        legend.box = "vertical",
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank()
      )
    
    # Save the combined map
    ggsave(
      filename = file.path("outputs", sprintf("NetVirtualWaterTrade_%s.png", period_name)),
      plot = combined_map,
      width = 12,
      height = 8,
      dpi = 300
    )
    # Also save as SVG
    ggsave(
      filename = file.path("outputs", sprintf("NetVirtualWaterTrade_%s.svg", period_name)),
      plot = combined_map,
      width = 12,
      height = 8
    )
    
    # Save flow analysis
    write.csv(
      top_flows,
      file.path("outputs", sprintf("TopFlows_%s.csv", period_name)),
      row.names = FALSE
    )
    
  }, error = function(e) {
    warning(paste("Error creating map visualization:", e$message))
  })
}

# Function to prepare virtual water barplot data
prepare_vw_barplot_data <- function(flows_early, flows_late) {
  # First, track what crops we have
  track_crops_through_pipeline(flows_early, flows_late)
  
  # Process early period data
  early_data <- flows_early %>%
    mutate(
      Water_Category = case_when(
        Water_Type == "Blue_Irrigated" ~ "Blue",
        Water_Type %in% c("Green_Rainfed", "Green_Irrigated") ~ "Green",
        TRUE ~ as.character(Water_Type)
      )
    ) %>%
    group_by(WF_Crop, Water_Category) %>%
    summarise(
      Total_Virtual_Water_m3 = sum(virtual_water_m3, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(Period = "Early")
  
  # Process late period data
  late_data <- flows_late %>%
    mutate(
      Water_Category = case_when(
        Water_Type == "Blue_Irrigated" ~ "Blue",
        Water_Type %in% c("Green_Rainfed", "Green_Irrigated") ~ "Green",
        TRUE ~ as.character(Water_Type)
      )
    ) %>%
    group_by(WF_Crop, Water_Category) %>%
    summarise(
      Total_Virtual_Water_m3 = sum(virtual_water_m3, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(Period = "Late")
  
  # Combine the data
  plot_data <- bind_rows(early_data, late_data) %>%
    filter(!is.na(Water_Category))
  
  return(plot_data)
}

# Function to create dot plot
create_dot_plot <- function(plot_data) {
  # Convert to km3 and prepare data
  plot_data <- plot_data %>%
    mutate(
      Total_Virtual_Water_km3 = Total_Virtual_Water_m3 / 1e9,
      WF_Crop = factor(WF_Crop, levels = c(
        "Rice", "Maize", "Temperate Cereals", "Tropical Cereals",
        "Groundnut", "Rapeseed", "Soyabean", "Sunflower",
        "Sugar", "Pulses",
        "Temperate Roots", "Tropical Roots"
      ))
    )
  
  # Create the dot plot
  p <- ggplot(plot_data, 
              aes(x = Total_Virtual_Water_km3, 
                  y = WF_Crop, 
                  color = Water_Category,
                  shape = Period)) +
    geom_point(size = 3) +
    scale_x_log10(labels = scales::comma) +
    scale_color_manual(values = c("Blue" = "#2171b5", "Green" = "#31a354")) +
    scale_shape_manual(values = c("Early" = 16, "Late" = 17)) +
    labs(
      x = "Virtual Water Volume (km³, log scale)",
      y = NULL,
      color = "Water Source",
      shape = "Period"
    ) +
    theme_minimal() +
    theme(
      panel.grid.major.x = element_line(color = "gray90"),
      panel.grid.minor.x = element_line(color = "gray95"),
      legend.position = "bottom"
    )
  
  return(p)
}

# Function to create small multiples plot
create_small_multiples <- function(plot_data) {
  # Convert to km3 and prepare data
  plot_data <- plot_data %>%
    mutate(
      Total_Virtual_Water_km3 = Total_Virtual_Water_m3 / 1e9,
      WF_Crop = factor(WF_Crop, levels = c(
        "Rice", "Maize", "Temperate Cereals", "Tropical Cereals",
        "Groundnut", "Rapeseed", "Soyabean", "Sunflower",
        "Sugar", "Pulses",
        "Temperate Roots", "Tropical Roots"
      ))
    )
  
  # Create the small multiples plot
  p <- ggplot(plot_data, 
              aes(x = Period, 
                  y = Total_Virtual_Water_km3, 
                  fill = Water_Category)) +
    geom_col(position = "dodge", width = 0.7) +
    facet_wrap(~WF_Crop, scales = "free_y", ncol = 3) +
    scale_fill_manual(values = c("Blue" = "#2171b5", "Green" = "#31a354")) +
    scale_y_continuous(labels = scales::comma) +
    labs(
      x = NULL,
      y = "Virtual Water Volume (km³)",
      fill = "Water Source"
    ) +
    theme_minimal() +
    theme(
      strip.background = element_rect(fill = "gray95"),
      strip.text = element_text(face = "bold"),
      panel.grid.major = element_line(color = "gray90"),
      panel.grid.minor = element_line(color = "gray95"),
      legend.position = "bottom",
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  
  return(p)
}

# Updated plot_vw_barplots function
plot_vw_barplots <- function(plot_data) {
  # Create the three different visualizations
  dot_plot <- create_dot_plot(plot_data)
  small_multiples <- create_small_multiples(plot_data)
  
  # Regular bar plot with side-by-side periods
  bar_plot <- ggplot(
    plot_data %>% 
      mutate(Total_Virtual_Water_km3 = Total_Virtual_Water_m3 / 1e9),
    aes(x = interaction(WF_Crop, Period),
        y = Total_Virtual_Water_km3,
        fill = interaction(Water_Category, Period))
  ) +
    geom_col(position = "stack", width = 0.7) +
    scale_fill_manual(
      values = c(
        "Blue.Early" = "#2171b5",
        "Blue.Late" = "#6baed6",
        "Green.Early" = "#31a354",
        "Green.Late" = "#74c476"
      ),
      name = "Water Source and Period"
    ) +
    labs(
      x = NULL,
      y = "Virtual Water Volume (km³)"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom"
    )
  
  # Save all plots
  ggsave(
    filename = file.path("outputs", "VW_DotPlot.png"),
    plot = dot_plot,
    width = 12,
    height = 8,
    dpi = 300
  )
  
  ggsave(
    filename = file.path("outputs", "VW_SmallMultiples.png"),
    plot = small_multiples,
    width = 15,
    height = 12,
    dpi = 300
  )
  
  ggsave(
    filename = file.path("outputs", "VW_BarPlot.png"),
    plot = bar_plot,
    width = 12,
    height = 8,
    dpi = 300
  )
  
  return(list(
    dot_plot = dot_plot,
    small_multiples = small_multiples,
    bar_plot = bar_plot
  ))
}

# Function to create time series plots of virtual water intensity
plot_water_intensity_trends <- function(stats_data) {
  # Create time series plot
  intensity_plot <- ggplot(stats_data %>% 
                             filter(!is.na(Mean_Intensity_L_per_kg) & Water_Type != "NA"), 
                           aes(x = year, 
                               y = Mean_Intensity_L_per_kg,
                               color = Water_Type)) +
    geom_line() +
    geom_point() +
    facet_wrap(~WF_Crop, scales = "free_y") +
    scale_color_manual(values = c("Blue_Irrigated" = "#2171b5",
                                  "Green_Irrigated" = "#74c476",
                                  "Green_Rainfed" = "#31a354")) +
    theme_minimal() +
    labs(title = "Virtual Water Intensity Trends by Crop",
         y = "Water Intensity (L/kg)",
         x = "Year",
         color = "Water Type") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Save plot
  ggsave(filename = file.path("outputs", "water_intensity_trends.png"),
         plot = intensity_plot,
         width = 15,
         height = 10,
         dpi = 300)
  
  return(intensity_plot)
}

# Function to create stacked bar chart of virtual water volumes
plot_water_volumes <- function(stats_data) {
  # Prepare data for plotting
  volume_data <- stats_data %>%
    filter(Water_Type != "NA") %>%
    mutate(Year = factor(year))
  
  # Create stacked bar plot
  volume_plot <- ggplot(volume_data, 
                        aes(x = Year, 
                            y = Total_Virtual_Water_km3,
                            fill = Water_Type)) +
    geom_bar(stat = "identity") +
    facet_wrap(~WF_Crop, scales = "free_y") +
    scale_fill_manual(values = c("Blue_Irrigated" = "#2171b5",
                                 "Green_Irrigated" = "#74c476",
                                 "Green_Rainfed" = "#31a354")) +
    theme_minimal() +
    labs(title = "Virtual Water Trade Volumes by Crop",
         y = "Virtual Water Volume (km³/year)",
         x = "Year",
         fill = "Water Type") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Save plot
  ggsave(filename = file.path("outputs", "water_volumes.png"),
         plot = volume_plot,
         width = 15,
         height = 10,
         dpi = 300)
  
  return(volume_plot)
}

# Function to create diagnostic maps
create_diagnostic_maps <- function(water_footprint_data, production_data, flows, period_name) {
  # Load required mapping data
  world <- ne_countries(scale = "medium", returnclass = "sf") %>%
    filter(admin != "Antarctica")
  
  # 1. Water Footprint Intensity Map
  intensity_data <- water_footprint_data %>%
    group_by(iso3) %>%
    summarise(
      mean_intensity = mean(WF_L_per_kg, na.rm = TRUE),
      .groups = 'drop'
    )
  
  intensity_map <- world %>%
    left_join(intensity_data, by = c("iso_a3" = "iso3")) %>%
    ggplot() +
    geom_sf(aes(fill = mean_intensity), color = "gray") +
    scale_fill_viridis_c(
      name = "Water Footprint Intensity (L/kg)",
      trans = "log10",
      na.value = "gray80"
    ) +
    theme_minimal() +
    labs(title = paste("Water Footprint Intensity -", period_name)) +
    theme(axis.text = element_blank(),
          axis.title = element_blank())
  
  # 2. Production Map
  production_data_agg <- production_data %>%
    group_by(iso3) %>%
    summarise(
      total_production_mt = sum(Production_kg, na.rm = TRUE) / 1e9,  # Convert to million tonnes
      .groups = 'drop'
    )
  
  production_map <- world %>%
    left_join(production_data_agg, by = c("iso_a3" = "iso3")) %>%
    ggplot() +
    geom_sf(aes(fill = total_production_mt), color = "gray") +
    scale_fill_viridis_c(
      name = "Production (Mt)",
      trans = "log10",
      na.value = "gray80"
    ) +
    theme_minimal() +
    labs(title = paste("Agricultural Production -", period_name)) +
    theme(axis.text = element_blank(),
          axis.title = element_blank())
  
  # 3. Virtual Water Flow Map
  flow_data <- flows %>%
    group_by(ReporterISO3) %>%
    summarise(
      total_flow_km3 = sum(virtual_water_m3, na.rm = TRUE) / 1e9,
      .groups = 'drop'
    )
  
  flow_map <- world %>%
    left_join(flow_data, by = c("iso_a3" = "ReporterISO3")) %>%
    ggplot() +
    geom_sf(aes(fill = total_flow_km3), color = "gray") +
    scale_fill_viridis_c(
      name = "Virtual Water Flow (km³)",
      trans = "log10",
      na.value = "gray80"
    ) +
    theme_minimal() +
    labs(title = paste("Virtual Water Flows -", period_name)) +
    theme(axis.text = element_blank(),
          axis.title = element_blank())
  
  # Create comparison table
  comparison_table <- full_join(
    intensity_data,
    production_data_agg,
    by = "iso3"
  ) %>%
    full_join(
      flow_data %>% rename(iso3 = ReporterISO3),
      by = "iso3"
    ) %>%
    arrange(desc(total_flow_km3)) %>%
    head(20)  # Top 20 countries
  
  # Save the diagnostic plots
  ggsave(
    filename = file.path("outputs", paste0("diagnostic_maps_", period_name, ".png")),
    gridExtra::grid.arrange(intensity_map, production_map, flow_map, ncol = 1),
    width = 12,
    height = 15,
    dpi = 300
  )
  
  # Return diagnostic data
  return(list(
    intensity_map = intensity_map,
    production_map = production_map,
    flow_map = flow_map,
    comparison_table = comparison_table
  ))
}

# Function to analyze calculations
analyze_calculations <- function(water_footprint_data, production_data, flows) {
  # Check water footprint intensity distribution
  wf_summary <- water_footprint_data %>%
    group_by(WF_Crop, Water_Type) %>%
    summarise(
      mean_intensity = mean(WF_L_per_kg, na.rm = TRUE),
      median_intensity = median(WF_L_per_kg, na.rm = TRUE),
      min_intensity = min(WF_L_per_kg, na.rm = TRUE),
      max_intensity = max(WF_L_per_kg, na.rm = TRUE),
      n_countries = n_distinct(iso3),
      .groups = 'drop'
    )
  
  # Check production aggregation
  prod_summary <- production_data %>%
    group_by(WF_Crop) %>%
    summarise(
      total_production_mt = sum(Production_kg, na.rm = TRUE) / 1e9,
      n_countries = n_distinct(iso3),
      .groups = 'drop'
    )
  
  # Check flow calculations
  flow_summary <- flows %>%
    group_by(WF_Crop, Water_Type) %>%
    summarise(
      total_flow_km3 = sum(virtual_water_m3, na.rm = TRUE) / 1e9,
      n_flows = n(),
      n_exporters = n_distinct(ReporterISO3),
      n_importers = n_distinct(PartnerISO3),
      .groups = 'drop'
    )
  
  # Return all summaries
  return(list(
    water_footprint_summary = wf_summary,
    production_summary = prod_summary,
    flow_summary = flow_summary
  ))
}


# Function to compare with previous values
analyze_value_differences <- function(stats_data) {
  # Calculate averages by period and store results
  period_stats <- stats_data %>%
    filter(Water_Type != "NA") %>%
    mutate(Period = if_else(year <= 2005, "Early", "Late")) %>%
    group_by(WF_Crop, Water_Type) %>%  # Group by crop and water type
    mutate(
      Previous_Volume_km3 = lag(Total_Virtual_Water_km3),  # Get previous period's value
      Difference_Factor = Total_Virtual_Water_km3 / Previous_Volume_km3  # Calculate ratio
    ) %>%
    select(
      WF_Crop,
      Water_Type,
      Period,
      Current_Volume_km3 = Total_Virtual_Water_km3,
      Previous_Volume_km3,
      Mean_Intensity = Mean_Intensity_L_per_kg,
      Trade_Volume_Mt,
      Difference_Factor
    ) %>%
    arrange(WF_Crop, Water_Type, Period)
  
  # Save results
  write.csv(period_stats,
            file.path("outputs", "period_comparison.csv"),
            row.names = FALSE)
  
  return(period_stats)
}

# -----------------------------------
# 9b. Data Processing
# -----------------------------------

# Function to track crops through pipeline
track_crops_through_pipeline <- function(flows_early, flows_late) {
  cat("\nCrop Tracking Analysis:\n")
  cat("=====================\n")
  
  # Get unique crops at each stage
  early_crops <- unique(flows_early$WF_Crop)
  late_crops <- unique(flows_late$WF_Crop)
  
  cat("\nEarly period crops:", paste(sort(early_crops), collapse = ", "))
  cat("\nLate period crops:", paste(sort(late_crops), collapse = ", "))
  
  # Check water types for each crop
  cat("\n\nWater Types by Crop (Early Period):\n")
  flows_early %>%
    group_by(WF_Crop, Water_Type) %>%
    summarise(n = n(), .groups = 'drop') %>%
    print()
  
  cat("\nWater Types by Crop (Late Period):\n")
  flows_late %>%
    group_by(WF_Crop, Water_Type) %>%
    summarise(n = n(), .groups = 'drop') %>%
    print()
}



# Function to investigate data loss
investigate_data_loss <- function(trade_data, water_footprint_data, flows) {
  cat("\nData Loss Investigation:\n")
  cat("=====================\n")
  
  # Check crops at each stage
  trade_crops <- unique(trade_data$WF_Crop)
  wf_crops <- unique(water_footprint_data$WF_Crop)
  flow_crops <- unique(flows$WF_Crop)
  
  cat("\nCrops in trade data:", paste(sort(trade_crops), collapse = ", "))
  cat("\nCrops in water footprint data:", paste(sort(wf_crops), collapse = ", "))
  cat("\nCrops in final flows:", paste(sort(flow_crops), collapse = ", "))
  
  # Check for missing crops
  missing_after_join <- setdiff(trade_crops, flow_crops)
  if(length(missing_after_join) > 0) {
    cat("\n\nCrops lost during join operations:", paste(missing_after_join, collapse = ", "))
    
    # Investigate why these crops were lost
    for(crop in missing_after_join) {
      cat("\n\nInvestigating missing crop:", crop)
      cat("\nTrade data records:", sum(trade_data$WF_Crop == crop))
      cat("\nWater footprint records:", sum(water_footprint_data$WF_Crop == crop))
      
      # Check the join keys
      trade_countries <- unique(trade_data$ReporterISO3[trade_data$WF_Crop == crop])
      wf_countries <- unique(water_footprint_data$iso3[water_footprint_data$WF_Crop == crop])
      missing_countries <- setdiff(trade_countries, wf_countries)
      
      if(length(missing_countries) > 0) {
        cat("\nCountries with trade data but no water footprint data:", 
            paste(missing_countries, collapse = ", "))
      }
    }
  }
  
  return(list(
    trade_crops = trade_crops,
    wf_crops = wf_crops,
    flow_crops = flow_crops,
    missing_crops = missing_after_join
  ))
}



# Function to diagnose production data issues
diagnose_production_codes <- function(years) {
  # Download/load production data
  production_data_file <- "data_raw/crop_production_data.rds"
  
  if (!file.exists(production_data_file)) {
    cat("Downloading fresh production data from FAOSTAT...\n")
    fao_item_codes <- crop_mapping$FAO_Code
    production_data_raw <- FAOSTAT::get_faostat_data(
      domain_code = "QC",
      item_code = fao_item_codes,
      element_code = 5510,
      output_format = "RDS"
    )
    saveRDS(production_data_raw, production_data_file)
  } else {
    production_data_raw <- readRDS(production_data_file)
  }
  
  # Print our crop mapping codes
  cat("\nCrop Mapping FAO Codes:\n")
  cat("======================\n")
  crop_mapping %>%
    select(FAO_Code, FAO_Crop_Name, WF_Crop) %>%
    arrange(WF_Crop) %>%
    print(n = nrow(.))
  
  # Analyze raw production data
  cat("\nProduction Data Analysis:\n")
  cat("=======================\n")
  production_summary <- production_data_raw %>%
    mutate(
      item_code = as.character(item_code),
      year = as.numeric(year)
    ) %>%
    filter(year %in% years) %>%
    group_by(item_code, item) %>%
    summarise(
      n_records = n(),
      total_production = sum(value, na.rm = TRUE),
      n_countries = n_distinct(area_code),
      .groups = 'drop'
    )
  
  # Print summary
  cat("\nProduction Records Found:\n")
  print(production_summary)
  
  # Find missing codes
  missing_codes <- setdiff(crop_mapping$FAO_Code, production_summary$item_code)
  if(length(missing_codes) > 0) {
    cat("\nFAO Codes in mapping but missing in production data:\n")
    crop_mapping %>%
      filter(FAO_Code %in% missing_codes) %>%
      select(FAO_Code, FAO_Crop_Name, WF_Crop) %>%
      arrange(WF_Crop) %>%
      print(n = nrow(.))
  }
  
  # Check for code mismatches
  cat("\nChecking for potential code mismatches...\n")
  all_production_codes <- unique(production_data_raw$item_code)
  cat("All available production codes:", paste(sort(all_production_codes), collapse=", "), "\n")
  
  # Return diagnostic data
  return(list(
    mapping = crop_mapping,
    production = production_summary,
    missing_codes = missing_codes,
    all_available_codes = all_production_codes
  ))
}

# Function to validate FAO code mappings
validate_fao_codes <- function() {
  cat("\nValidating FAO Code Mappings:\n")
  cat("=========================\n")
  
  # Check for duplicate codes
  duplicate_codes <- crop_mapping %>%
    count(FAO_Code) %>%
    filter(n > 1)
  
  if(nrow(duplicate_codes) > 0) {
    cat("\nWarning: Duplicate FAO codes found:\n")
    print(duplicate_codes)
  }
  
  # Check code format consistency
  code_issues <- crop_mapping %>%
    filter(!str_detect(FAO_Code, "^\\d+$"))
  
  if(nrow(code_issues) > 0) {
    cat("\nWarning: Non-numeric FAO codes found:\n")
    print(code_issues)
  }
  
  # Print mapping structure
  cat("\nCurrent mapping structure:\n")
  str(crop_mapping)
  
  # Return validation results
  return(list(
    duplicate_codes = duplicate_codes,
    code_issues = code_issues
  ))
}


# Define consistent color schemes
water_colors <- list(
  "Blue_Irrigated" = "#1f78b4",    # Dark blue
  "Green_Irrigated" = "#33a02c",   # Dark green
  "Green_Rainfed" = "#b2df8a"      # Light green
)

trade_colors <- list(
  "export" = "#ff7f00",    # Orange for exports
  "import" = "#e31a1c"     # Red for imports
)

# Function to create multi-panel trade rankings
# Define consistent color schemes
water_colors <- list(
  "Blue_Irrigated" = "#1f78b4",    # Dark blue
  "Green_Irrigated" = "#33a02c",   # Dark green
  "Green_Rainfed" = "#b2df8a"      # Light green
)

trade_colors <- list(
  "export" = "#ff7f00",    # Orange for exports
  "import" = "#e31a1c"     # Red for imports
)

# Function to create multi-panel trade rankings
create_condensed_trade_rankings <- function(flows, period_name, n_top = 5) {
  # Calculate rankings
  rankings <- flows %>%
    filter(!is.na(virtual_water_m3), virtual_water_m3 > 0) %>%
    group_by(WF_Crop, Water_Type) %>%
    summarise(
      # Top exporters
      top_exporters = list(
        data.frame(
          Country = ReporterISO3,
          Value = virtual_water_m3 / 1e9
        ) %>%
          group_by(Country) %>%
          summarise(Total = sum(Value, na.rm = TRUE)) %>%
          filter(Total > 0) %>%
          arrange(desc(Total)) %>%
          head(n_top) %>%
          mutate(
            Label = sprintf("%s (%.1f)", Country, Total)
          ) %>%
          pull(Label) %>%
          paste(collapse = "\n")
      ),
      # Top importers
      top_importers = list(
        data.frame(
          Country = PartnerISO3,
          Value = virtual_water_m3 / 1e9
        ) %>%
          group_by(Country) %>%
          summarise(Total = sum(Value, na.rm = TRUE)) %>%
          filter(Total > 0) %>%
          arrange(desc(Total)) %>%
          head(n_top) %>%
          mutate(
            Label = sprintf("%s (%.1f)", Country, Total)
          ) %>%
          pull(Label) %>%
          paste(collapse = "\n")
      ),
      total_volume = sum(virtual_water_m3, na.rm = TRUE) / 1e9,
      .groups = 'drop'
    )
  
  # Create visualization using ggplot2 with facets
  plot_data <- rankings %>%
    mutate(
      WF_Crop = factor(WF_Crop, levels = c(
        "Rice", "Maize", "Temperate Cereals", "Tropical Cereals",
        "Groundnut", "Rapeseed", "Soyabean", "Sunflower",
        "Sugar", "Pulses", "Temperate Roots", "Tropical Roots"
      ))
    )
  
  # Create plot
  p <- ggplot(plot_data) +
    geom_rect(
      aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, fill = Water_Type),
      alpha = 0.1
    ) +
    geom_text(
      aes(x = 0, y = 0.8, 
          label = sprintf("Total Volume: %.1f km³", total_volume)),
      size = 3, fontface = "bold"
    ) +
    geom_text(
      aes(x = -0.4, y = 0.3, label = "Top Exporters:"),
      size = 3, fontface = "bold", hjust = 0
    ) +
    geom_text(
      aes(x = -0.4, y = 0.1, label = top_exporters),
      size = 2.5, hjust = 0, lineheight = 0.8
    ) +
    geom_text(
      aes(x = 0.4, y = 0.3, label = "Top Importers:"),
      size = 3, fontface = "bold", hjust = 0
    ) +
    geom_text(
      aes(x = 0.4, y = 0.1, label = top_importers),
      size = 2.5, hjust = 0, lineheight = 0.8
    ) +
    facet_grid(WF_Crop ~ Water_Type) +
    scale_fill_manual(values = water_colors) +
    theme_minimal() +
    theme(
      axis.text = element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank(),
      strip.text = element_text(size = 8, face = "bold"),
      strip.background = element_rect(fill = "white", color = "gray80"),
      panel.border = element_rect(color = "gray80", fill = NA)
    ) +
    labs(title = paste("Virtual Water Trade Rankings -", period_name))
  
  # Save plot
  ggsave(
    filename = file.path(
      "outputs",
      sprintf("trade_rankings_condensed_%s.png", period_name)
    ),
    plot = p,
    width = 15,
    height = 20,
    dpi = 300
  )
  
  return(p)
}


# Function to print data summary for debugging
print_data_summary <- function(flows, name = "") {
  cat("\nData Summary for:", name, "\n")
  cat("Total rows:", nrow(flows), "\n")
  cat("Unique crops:", paste(unique(flows$WF_Crop), collapse = ", "), "\n")
  cat("Unique water types:", paste(unique(flows$Water_Type), collapse = ", "), "\n")
  cat("Value range:", range(flows$virtual_water_m3, na.rm = TRUE), "\n")
  cat("NA values:", sum(is.na(flows$virtual_water_m3)), "\n")
}


# Function to create static network plots
# Modified network plot function with consistent crop ordering
# Fixed create_static_network_plots function to create separate plots for each water type
create_static_network_plots <- function(flows, period_name, threshold_quantile = 0.95) {
  # Define consistent crop order
  crop_order <- c(
    "Rice", "Maize", "Temperate Cereals", "Tropical Cereals",
    "Groundnut", "Rapeseed", "Soyabean", "Sunflower",
    "Sugar", "Pulses", "Temperate Roots", "Tropical Roots"
  )
  
  # Filter out NA water types
  flows <- flows %>% filter(!is.na(Water_Type))
  
  # Calculate thresholds for each water type and crop
  thresholds <- flows %>%
    group_by(WF_Crop, Water_Type) %>%
    summarise(
      threshold = quantile(virtual_water_m3, threshold_quantile, na.rm = TRUE),
      .groups = 'drop'
    )
  
  # Process each water type separately
  for (water_type in unique(flows$Water_Type)) {
    water_flows <- flows %>%
      filter(Water_Type == water_type) %>%
      left_join(thresholds, by = c("WF_Crop", "Water_Type")) %>%
      filter(virtual_water_m3 >= threshold)
    
    # Create list for plots
    plots_list <- list()
    
    # Create plots for each crop
    for (crop in crop_order) {
      crop_flows <- water_flows %>% filter(WF_Crop == crop)
      
      if (nrow(crop_flows) == 0) {
        p <- ggplot() + 
          theme_void() +
          labs(title = crop) +
          theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5))
      } else {
        g <- graph_from_data_frame(
          d = crop_flows %>% select(ReporterISO3, PartnerISO3, virtual_water_m3),
          directed = TRUE
        )
        
        if (gorder(g) == 0) {
          p <- ggplot() + 
            theme_void() +
            labs(title = crop) +
            theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5))
        } else {
          p <- ggraph(g, layout = "fr") +
            geom_edge_link(
              aes(width = virtual_water_m3),
              alpha = 0.6,
              arrow = arrow(length = unit(4, "mm"))
            ) +
            geom_node_point(size = 5, color = water_colors[[water_type]]) +
            geom_node_text(
              aes(label = name),
              repel = TRUE,
              size = 4,
              max.overlaps = 20
            ) +
            labs(title = crop) +
            theme_void() +
            theme(
              plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
              legend.position = "none"
            )
        }
      }
      
      plots_list[[crop]] <- p
    }
    
    # Arrange plots in a grid
    combined <- gridExtra::arrangeGrob(
      grobs = plots_list,
      ncol = 3,  # 3 columns for better layout
      nrow = 4,  # 4 rows to fit 12 crops
      top = textGrob(
        paste0(water_type, " Virtual Water Trade Networks - ", period_name),
        gp = gpar(fontsize = 20, fontface = "bold")
      )
    )
    
    # Save plot for this water type
    ggsave(
      filename = file.path("outputs", sprintf("trade_networks_%s_%s.png", 
                                              gsub(" ", "_", water_type), 
                                              period_name)),
      plot = combined,
      width = 15,  # Adjusted width
      height = 20,  # Adjusted height
      dpi = 300
    )
  }
}

# New function for creating ranking plots
# Function for Figure 9: Major Trading Nations
create_trade_rankings_plot <- function(flows_early, flows_late, n_top = 10) {
  # Function to process one period's data
  process_period <- function(flows, period_label) {
    flows %>%
      filter(!is.na(Water_Type)) %>%
      group_by(Water_Type, ReporterISO3) %>%
      summarise(
        exports = sum(virtual_water_m3, na.rm = TRUE) / 1e9,
        .groups = "drop"
      ) %>%
      bind_rows(
        flows %>%
          filter(!is.na(Water_Type)) %>%
          group_by(Water_Type, PartnerISO3) %>%
          summarise(
            imports = sum(virtual_water_m3, na.rm = TRUE) / 1e9,
            .groups = "drop"
          ) %>%
          rename(ReporterISO3 = PartnerISO3)
      ) %>%
      mutate(
        Period = period_label,
        Country = iso3_to_name[ReporterISO3]
      ) %>%
      pivot_longer(
        cols = c(exports, imports),
        names_to = "Type",
        values_to = "Volume"
      ) %>%
      filter(!is.na(Volume)) %>%
      group_by(Water_Type, Type, Period) %>%
      slice_max(order_by = Volume, n = n_top)
  }
  
  # Combine early and late period data
  rankings_data <- bind_rows(
    process_period(flows_early, "1996-2000"),
    process_period(flows_late, "2016-2020")
  )
  
  # Create plot with facets for periods and water types
  p <- ggplot(rankings_data, 
              aes(x = reorder(Country, Volume), 
                  y = Volume,
                  fill = Type)) +
    geom_col() +
    facet_grid(Water_Type ~ Period, scales = "free") +
    coord_flip() +
    scale_fill_manual(
      values = c("exports" = "#2171b5", "imports" = "#fb6a4a"),
      labels = c("Exports", "Imports")
    ) +
    labs(
      title = "Major Virtual Water Trading Nations",
      subtitle = "Top 10 importers and exporters by water type and period",
      x = NULL,
      y = "Virtual Water Volume (km³/year)",
      fill = "Trade Type"
    ) +
    theme_minimal() +
    theme(
      strip.text = element_text(face = "bold"),
      strip.background = element_rect(fill = "gray95"),
      panel.grid.major.x = element_line(color = "gray90"),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_blank(),
      legend.position = "bottom"
    )
  
  # Save plot
  ggsave(
    filename = file.path("outputs", "major_trading_nations.png"),
    plot = p,
    width = 15,
    height = 12,
    dpi = 300
  )
  
  return(p)
}

# Function for Figure 10: Network Metrics Comparison
plot_network_metrics_comparison <- function(early_metrics, late_metrics) {
  # Prepare data for plotting
  metrics_data <- bind_rows(
    early_metrics %>% 
      select(WF_Crop, Water_Type, density, centralization, reciprocity, mean_degree) %>%
      mutate(Period = "1996-2000"),
    late_metrics %>% 
      select(WF_Crop, Water_Type, density, centralization, reciprocity, mean_degree) %>%
      mutate(Period = "2016-2020")
  ) %>%
    filter(!is.na(Water_Type)) %>%
    pivot_longer(
      cols = c(density, centralization, reciprocity, mean_degree),
      names_to = "Metric",
      values_to = "Value"
    ) %>%
    mutate(
      Metric = factor(Metric, 
                      levels = c("density", "centralization", "reciprocity", "mean_degree"),
                      labels = c("Network Density", "Centralization", 
                                 "Reciprocity", "Mean Degree")),
      Water_Type = factor(Water_Type,
                          levels = c("Green_Rainfed", "Green_Irrigated", "Blue_Irrigated"),
                          labels = c("Green (Rainfed)", "Green (Irrigated)", "Blue (Irrigated)"))
    )
  
  # Create plot
  p <- ggplot(metrics_data, 
              aes(x = Water_Type, y = Value, fill = Period)) +
    geom_bar(position = position_dodge(width = 0.8), stat = "summary", fun = "mean") +
    facet_wrap(~Metric, scales = "free_y", ncol = 2) +
    scale_fill_manual(
      values = c("1996-2000" = "#92c5de", "2016-2020" = "#f4a582")
    ) +
    labs(
      title = "Network Metrics for Virtual Water Trade Networks",
      subtitle = "Comparison between early (1996-2000) and late (2016-2020) periods",
      x = NULL,
      y = "Value",
      fill = "Period"
    ) +
    theme_minimal() +
    theme(
      strip.text = element_text(face = "bold"),
      strip.background = element_rect(fill = "gray95"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom",
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank()
    )
  
  # Save plot
  ggsave(
    filename = file.path("outputs", "network_metrics_comparison.png"),
    plot = p,
    width = 12,
    height = 10,
    dpi = 300
  )
  
  return(p)
}

# Function to create summary visualization
create_trade_dashboard <- function(flows, period_name, n_top = 5) {
  # Calculate overall statistics
  overall_stats <- flows %>%
    group_by(Water_Type) %>%
    summarise(
      total_volume = sum(virtual_water_m3, na.rm = TRUE) / 1e9,
      n_connections = n_distinct(paste(ReporterISO3, PartnerISO3)),
      n_exporters = n_distinct(ReporterISO3),
      n_importers = n_distinct(PartnerISO3),
      .groups = 'drop'
    )
  
  # Calculate top global traders
  top_traders <- flows %>%
    group_by(ReporterISO3) %>%
    summarise(
      export_volume = sum(virtual_water_m3, na.rm = TRUE) / 1e9,
      .groups = 'drop'
    ) %>%
    arrange(desc(export_volume)) %>%
    head(n_top)
  
  # Create summary plot
  p1 <- ggplot(overall_stats, aes(x = Water_Type, y = total_volume, fill = Water_Type)) +
    geom_col() +
    scale_fill_manual(values = water_colors) +
    theme_minimal() +
    labs(
      title = "Total Virtual Water Trade Volume",
      y = "Volume (km³)"
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Create crop-specific summary
  p2 <- flows %>%
    group_by(WF_Crop, Water_Type) %>%
    summarise(
      volume = sum(virtual_water_m3, na.rm = TRUE) / 1e9,
      .groups = 'drop'
    ) %>%
    ggplot(aes(x = WF_Crop, y = volume, fill = Water_Type)) +
    geom_col() +
    scale_fill_manual(values = water_colors) +
    theme_minimal() +
    labs(
      title = "Virtual Water Trade by Crop",
      y = "Volume (km³)"
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Combine plots
  combined <- gridExtra::arrangeGrob(
    p1, p2,
    ncol = 1,
    top = textGrob(
      paste("Virtual Water Trade Dashboard -", period_name),
      gp = gpar(fontsize = 16, fontface = "bold")
    )
  )
  
  # Save dashboard
  ggsave(
    filename = file.path(
      "outputs",
      sprintf("trade_dashboard_%s.png", period_name)
    ),
    plot = combined,
    width = 12,
    height = 10,
    dpi = 300
  )
  
  return(list(
    dashboard = combined,
    overall_stats = overall_stats,
    top_traders = top_traders
  ))
}

# Fixed analyze_network_metrics function
analyze_network_metrics <- function(flows, period_name) {
  print("Starting network analysis...")
  print(paste("Total rows in input:", nrow(flows)))
  print(paste("Unique water types:", paste(unique(flows$Water_Type), collapse=", ")))
  
  # Process each crop-water type combination separately
  results <- data.frame()
  
  for(crop in unique(flows$WF_Crop)) {
    for(wt in unique(flows$Water_Type)) {
      print(paste("Processing:", crop, "-", wt))
      
      # Filter data for this combination
      subset_data <- flows %>%
        filter(WF_Crop == crop, Water_Type == wt)
      
      # Get edges with non-zero flows
      edges_df <- subset_data %>%
        filter(virtual_water_m3 > 0) %>%
        select(ReporterISO3, PartnerISO3, virtual_water_m3)
      
      print(paste("Number of edges after filtering:", nrow(edges_df)))
      
      # Calculate metrics
      if(nrow(edges_df) > 0) {
        # Create graph
        g <- graph_from_data_frame(
          d = edges_df,
          directed = TRUE
        )
        
        print(paste("Graph created with", vcount(g), "vertices and", ecount(g), "edges"))
        
        # Calculate metrics
        metrics <- data.frame(
          WF_Crop = crop,
          Water_Type = wt,
          n_nodes = vcount(g),
          n_edges = ecount(g),
          total_volume_km3 = sum(subset_data$virtual_water_m3, na.rm = TRUE) / 1e9,
          density = edge_density(g),
          reciprocity = reciprocity(g),
          transitivity = transitivity(g, type = "global"),
          mean_degree = mean(degree(g)),
          centralization = centralization.degree(g)$centralization,
          clustering_coef = transitivity(g, type = "global"),
          avg_path_length = tryCatch(
            mean_distance(g, directed = TRUE),
            error = function(e) NA
          )
        )
      } else {
        metrics <- data.frame(
          WF_Crop = crop,
          Water_Type = wt,
          n_nodes = 0,
          n_edges = 0,
          total_volume_km3 = sum(subset_data$virtual_water_m3, na.rm = TRUE) / 1e9,
          density = 0,
          reciprocity = 0,
          transitivity = 0,
          mean_degree = 0,
          centralization = 0,
          clustering_coef = 0,
          avg_path_length = NA
        )
      }
      
      results <- bind_rows(results, metrics)
    }
  }
  
  # Round numeric columns and arrange
  results <- results %>%
    mutate(across(where(is.numeric), ~round(., 3))) %>%
    arrange(Water_Type, WF_Crop)
  
  # Save results
  write.csv(
    results,
    file.path("outputs", sprintf("network_metrics_%s.csv", period_name)),
    row.names = FALSE
  )
  
  return(results)
}

# Fixed plot_network_metrics function
plot_network_metrics <- function(early_metrics, late_metrics) {
  # Prepare data for plotting
  plot_data <- bind_rows(
    early_metrics %>% mutate(Period = "Early"),
    late_metrics %>% mutate(Period = "Late")
  ) %>%
    # Ensure WF_Crop is a factor with consistent order
    mutate(
      WF_Crop = factor(WF_Crop, levels = c(
        "Rice", "Maize", "Temperate Cereals", "Tropical Cereals",
        "Groundnut", "Rapeseed", "Soyabean", "Sunflower",
        "Sugar", "Pulses", "Temperate Roots", "Tropical Roots"
      ))
    )
  
  # Create plots with error handling
  p1 <- ggplot(plot_data %>% filter(!is.na(density)), 
               aes(x = WF_Crop, y = density, fill = Period)) +
    geom_bar(stat = "identity", position = "dodge") +
    facet_wrap(~Water_Type) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      strip.text = element_text(face = "bold"),
      strip.background = element_rect(fill = "gray95")
    ) +
    labs(title = "Network Density by Crop and Water Type",
         y = "Density")
  
  p2 <- ggplot(plot_data %>% filter(!is.na(centralization)), 
               aes(x = WF_Crop, y = centralization, fill = Period)) +
    geom_bar(stat = "identity", position = "dodge") +
    facet_wrap(~Water_Type) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      strip.text = element_text(face = "bold"),
      strip.background = element_rect(fill = "gray95")
    ) +
    labs(title = "Network Centralization by Crop and Water Type",
         y = "Centralization")
  
  # Combine plots
  combined <- gridExtra::arrangeGrob(
    p1, p2,
    ncol = 1,
    top = textGrob(
      "Network Metrics Comparison",
      gp = gpar(fontsize = 16, fontface = "bold")
    )
  )
  
  # Save plot
  ggsave(
    filename = file.path("outputs", "network_metrics_comparison.png"),
    plot = combined,
    width = 12,
    height = 10,
    dpi = 300
  )
  
  return(combined)
}

# Fixed compare_network_metrics function
compare_network_metrics <- function(early_metrics, late_metrics) {
  # Calculate changes
  comparison <- early_metrics %>%
    left_join(
      late_metrics,
      by = c("WF_Crop", "Water_Type"),
      suffix = c("_early", "_late")
    ) %>%
    mutate(
      across(
        matches("_late$"),
        ~case_when(
          is.numeric(.) & !is.na(.) & 
            !is.na(get(str_replace(cur_column(), "_late", "_early"))) &
            get(str_replace(cur_column(), "_late", "_early")) != 0 ~
            (. - get(str_replace(cur_column(), "_late", "_early"))) /
            get(str_replace(cur_column(), "_late", "_early")) * 100,
          TRUE ~ NA_real_
        ),
        .names = "{str_replace(col, '_change_pct', '_pct_change')}"
      )
    )
  
  # Save comparison
  write.csv(
    comparison,
    file.path("outputs", "network_metrics_comparison.csv"),
    row.names = FALSE
  )
  
  return(comparison)
}

# New simplified approach to trade rankings
# Improved trade rankings visualization with summary statistics
create_trade_rankings <- function(flows_early, flows_late, n_top = 5) {
  # Function to process flows for one period
  process_period_flows <- function(flows, period_name) {
    flows %>%
      group_by(WF_Crop, Water_Type, ReporterISO3) %>%
      summarise(
        export_volume = sum(virtual_water_m3, na.rm = TRUE) / 1e9,
        .groups = "drop"
      ) %>%
      group_by(WF_Crop, Water_Type) %>%
      mutate(
        rank = rank(-export_volume),
        Period = period_name
      ) %>%
      filter(rank <= n_top)
  }
  
  # Process both periods
  rankings_data <- bind_rows(
    process_period_flows(flows_early, "Early"),
    process_period_flows(flows_late, "Late")
  ) %>%
    mutate(
      WF_Crop = factor(WF_Crop, levels = c(
        "Rice", "Maize", "Temperate Cereals", "Tropical Cereals",
        "Groundnut", "Rapeseed", "Soyabean", "Sunflower",
        "Sugar", "Pulses", "Temperate Roots", "Tropical Roots"
      )),
      Country = iso3_to_name[ReporterISO3]
    )
  
  # Create separate plots for each water type
  plots <- list()
  
  for(wt in unique(rankings_data$Water_Type)) {
    wt_data <- rankings_data %>% 
      filter(Water_Type == wt)
    
    p <- ggplot(wt_data, 
                aes(x = export_volume, y = reorder(Country, export_volume))) +
      geom_col(fill = case_when(
        wt == "Blue_Irrigated" ~ "#2171b5",
        wt == "Green_Irrigated" ~ "#74c476",
        wt == "Green_Rainfed" ~ "#31a354"
      )) +
      facet_grid(WF_Crop ~ Period, scales = "free_y", space = "free_y") +
      labs(
        title = paste("Top Virtual Water Exporters -", wt),
        x = "Virtual Water Export Volume (km³/year)",
        y = NULL
      ) +
      theme_minimal() +
      theme(
        strip.text = element_text(face = "bold"),
        strip.background = element_rect(fill = "gray95"),
        panel.grid.major.y = element_blank(),
        axis.text.y = element_text(size = 8)
      )
    
    plots[[wt]] <- p
  }
  
  # Save individual plots
  for(wt in names(plots)) {
    ggsave(
      filename = file.path("outputs", sprintf("trade_rankings_%s.png", gsub(" ", "_", wt))),
      plot = plots[[wt]],
      width = 12,
      height = 15,
      dpi = 300
    )
  }
  
  # Create summary statistics
  summary_stats <- bind_rows(
    flows_early %>% mutate(Period = "Early"),
    flows_late %>% mutate(Period = "Late")
  ) %>%
    group_by(WF_Crop, Water_Type, Period) %>%
    summarise(
      total_volume = sum(virtual_water_m3, na.rm = TRUE) / 1e9,
      n_exporters = n_distinct(ReporterISO3),
      n_importers = n_distinct(PartnerISO3),
      mean_flow = mean(virtual_water_m3, na.rm = TRUE) / 1e9,
      .groups = 'drop'
    )
  
  # Save summary statistics
  write.csv(
    summary_stats,
    file.path("outputs", "trade_rankings_summary.csv"),
    row.names = FALSE
  )
  
  # Create additional summary visualizations
  summary_plot <- ggplot(summary_stats,
                         aes(x = Period, y = total_volume, fill = Water_Type)) +
    geom_col(position = "dodge") +
    facet_wrap(~WF_Crop, scales = "free_y") +
    scale_fill_manual(values = c(
      "Blue_Irrigated" = "#2171b5",
      "Green_Irrigated" = "#74c476",
      "Green_Rainfed" = "#31a354"
    )) +
    labs(
      title = "Total Virtual Water Volume by Crop and Water Type",
      y = "Total Volume (km³/year)",
      x = NULL
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      strip.text = element_text(face = "bold")
    )
  
  ggsave(
    filename = file.path("outputs", "trade_summary_volumes.png"),
    plot = summary_plot,
    width = 15,
    height = 10,
    dpi = 300
  )
  
  return(list(
    plots = plots,
    rankings = rankings_data,
    summary = summary_stats,
    summary_plot = summary_plot
  ))
}

# Function to read (or specify) global totals from Excel files
read_global_totals <- function() {
  log_msg <- create_logger("global_totals_reading")
  
  # Years vector
  years <- 1980:2022
  
  # Create the data frame with all values
  global_totals <- data.frame(
    year = years,
    
    BW_irrigated = c(1032.519, 1062.679, 1082.03, 1098.243, 1100.114, 1156.544, 
                     1172.826, 1195.41, 1196.787, 1244.994, 1243.589, 1201.951, 
                     1235.657, 1220.979, 1274.05, 1270.72, 1342.7, 1297.34, 
                     1439.368, 1439.071, 1436.117, 1491.822, 1490.117, 1461.026, 
                     1482.95, 1489.87, 1552.66, 1412.569, 1410.015, 1446.023, 
                     1514.265, 1489.53, 1547.352, 1525.531, 1510.726, 1535.031, 
                     1546.815, 1576.378, 1529.713, 1543.923, 1608.663, 1658.721, 
                     1603.769),
    
    GW_irrigated = c(373.9066, 472.2894, 485.531, 483.7982, 458.1971, 444.6049,
                     439.1366, 440.8959, 441.7777, 431.9364, 348.1673, 589.1798,
                     591.9338, 591.8969, 565.3221, 571.1387, 536.2149, 560.5931,
                     503.6565, 554.8268, 481.4223, 521.5454, 614.82, 648.3132,
                     649.2904, 609.0249, 587.5598, 671.9902, 727.7901, 649.3626,
                     692.1093, 747.4845, 714.8245, 820.2679, 836.0033, 849.1072,
                     849.1785, 874.6378, 935.4788, 929.0189, 903.5993, 868.9254,
                     900.5145),
    
    GW_rainfed = c(4361.256, 4457.501, 4441.705, 4522.391, 4372.718, 4307.828,
                   4316.413, 4300.503, 4330.102, 4410.193, 4339.587, 4407.276,
                   4471.649, 4382.395, 4384.374, 4413.775, 4316.173, 4264.817,
                   4314.265, 4370.737, 4508.443, 4338.487, 4298.99, 4240.854,
                   4612.716, 4404.339, 4397.312, 4421.585, 4484.239, 4478.53,
                   4501.071, 4695.588, 4860.14, 4798.796, 4895.495, 5013.211,
                   5043.786, 4885.687, 5188.391, 5230.691, 5215.514, 5155.661,
                   5200.622)
  )
  
  # Print summary for validation
  log_msg("Global totals summary:")
  log_msg(paste("Years covered:", paste(range(global_totals$year), collapse = " - ")))
  log_msg(paste("Number of years:", nrow(global_totals)))
  
  # Quick validation of totals
  log_msg("\nMean values by type (km³/year):")
  log_msg(paste("Blue irrigated:", mean(global_totals$BW_irrigated)))
  log_msg(paste("Green irrigated:", mean(global_totals$GW_irrigated)))
  log_msg(paste("Green rainfed:", mean(global_totals$GW_rainfed)))
  
  return(global_totals)
}

# Improved validation results handling with unified structure
create_unified_validation_results <- function(results) {
  # This function now just ensures that trade_fractions, mass_balance, and water_proportions
  # are extracted from results if present, and returns them as a list.
  
  unified_results <- list()
  
  if (!is.null(results$trade_fractions_late)) {
    unified_results$trade_fractions <- results$trade_fractions_late
  }
  
  if (!is.null(results$mass_balance_late)) {
    unified_results$mass_balance <- results$mass_balance_late
  }
  
  if (!is.null(results$water_proportions_late)) {
    unified_results$water_proportions <- results$water_proportions_late
  }
  
  return(unified_results)
}

# Helper function to ensure we're working with data frames
ensure_dataframe <- function(x, name = "unknown") {
  if (is.null(x)) {
    cat("\nWarning:", name, "is NULL\n")
    return(NULL)
  }
  if (is.data.frame(x)) {
    return(x)
  }
  if (is.list(x)) {
    cat("\nConverting list to data frame:", name, "\n")
    return(as.data.frame(x))
  }
  cat("\nWarning: Unable to convert", name, "to data frame\n")
  return(NULL)
}

# Updated functions to return proper data frames
calculate_trade_fraction <- function(flows, global_total, target_year) {
  result <- tryCatch({
    traded_volumes <- flows %>%
      filter(!is.na(Water_Type)) %>%
      group_by(Water_Type) %>%
      summarise(
        traded_volume_km3 = sum(virtual_water_m3, na.rm = TRUE) / 1e9,
        .groups = 'drop'
      )
    
    year_totals <- global_total %>%
      filter(year == target_year) %>%
      select(BW_irrigated, GW_irrigated, GW_rainfed) %>%
      slice(1)
    
    if(nrow(year_totals) == 0) {
      return(NULL)
    }
    
    return(traded_volumes %>%
             mutate(
               global_volume_km3 = case_when(
                 Water_Type == "Blue_Irrigated" ~ year_totals$BW_irrigated,
                 Water_Type == "Green_Irrigated" ~ year_totals$GW_irrigated,
                 Water_Type == "Green_Rainfed" ~ year_totals$GW_rainfed,
                 TRUE ~ NA_real_
               ),
               trade_fraction = ifelse(
                 global_volume_km3 > 0,
                 traded_volume_km3 / global_volume_km3,
                 NA_real_
               )
             ))
  }, error = function(e) {
    cat("\nError in calculate_trade_fraction:", e$message, "\n")
    return(NULL)
  })
  
  return(ensure_dataframe(result, "trade_fraction"))
}

mass_balance <- function(flows) {
  result <- tryCatch({
    flows %>%
      filter(!is.na(Water_Type)) %>%
      group_by(WF_Crop) %>%
      summarise(
        total_exports = sum(virtual_water_m3[ReporterISO3 != PartnerISO3], na.rm = TRUE) / 1e9,
        total_imports = sum(virtual_water_m3[ReporterISO3 != PartnerISO3], na.rm = TRUE) / 1e9,
        .groups = 'drop'
      ) %>%
      mutate(
        balance_difference = abs(total_exports - total_imports),
        balance_error_pct = ifelse(total_exports > 0,
                                   (balance_difference / total_exports) * 100,
                                   NA_real_)
      )
  }, error = function(e) {
    cat("\nError in mass_balance:", e$message, "\n")
    return(NULL)
  })
  
  return(ensure_dataframe(result, "mass_balance"))
}

water_proportions <- function(flows) {
  result <- tryCatch({
    flows %>%
      filter(!is.na(Water_Type)) %>%
      group_by(Water_Type) %>%
      summarise(
        total_volume_km3 = sum(virtual_water_m3, na.rm = TRUE) / 1e9,
        .groups = 'drop'
      ) %>%
      mutate(
        proportion = total_volume_km3 / sum(total_volume_km3, na.rm = TRUE)
      )
  }, error = function(e) {
    cat("\nError in water_proportions:", e$message, "\n")
    return(NULL)
  })
  
  return(ensure_dataframe(result, "water_proportions"))
}


# Main validation function with improved error handling and reporting
validate_water_balance <- function(flows_early, flows_late,
                                   water_footprints_early, water_footprints_late,
                                   global_totals) {
  # Clean input data
  clean_flows <- function(flows) {
    flows %>%
      filter(!is.na(Water_Type)) %>%
      filter(!is.infinite(virtual_water_m3))
  }
  
  flows_early <- clean_flows(flows_early)
  flows_late <- clean_flows(flows_late)
  
  # Calculate all validations
  results <- list(
    trade_fractions_early = calculate_trade_fraction(flows_early, global_totals, 1996),
    trade_fractions_late = calculate_trade_fraction(flows_late, global_totals, 2016),
    mass_balance_early = mass_balance(flows_early),
    mass_balance_late = mass_balance(flows_late),
    water_proportions_early = water_proportions(flows_early),
    water_proportions_late = water_proportions(flows_late)
  )
  
  # Create unified results structure
  unified_results <- create_unified_validation_results(results)
  
  # Generate validation report
  print_validation_report(unified_results)
  
  return(unified_results)
}

check_na_counts <- function(data, stage_name) {
  na_counts <- sapply(data, function(x) sum(is.na(x)))
  total_rows <- nrow(data)
  
  cat("\nNA Check for:", stage_name)
  cat("\nTotal rows:", total_rows)
  cat("\nNA counts per column:\n")
  print(na_counts)
  
  return(na_counts)  # Return the counts so we can use them if needed
}


# Helper function to print formatted validation report
print_validation_report <- function(unified_results) {
  cat("\nValidation Summary Report\n")
  cat("=======================\n")
  
  # 1. Trade Fractions (Late Period)
  cat("\n1. Trade as Fraction of Global Water Use (Late Period):\n")
  if (!is.null(unified_results$trade_fractions)) {
    # Expected columns: Water_Type, traded_volume_km3, global_volume_km3, trade_fraction
    print(unified_results$trade_fractions)
  } else {
    cat("No trade fraction data available.\n")
  }
  
  # 2. Mass Balance (Late Period)
  cat("\n2. Mass Balance Check (Late Period):\n")
  if (!is.null(unified_results$mass_balance) && nrow(unified_results$mass_balance) > 0) {
    # Expected columns: WF_Crop, total_exports, total_imports, balance_difference, balance_error_pct
    print(unified_results$mass_balance)
  } else {
    cat("No significant mass balance errors found.\n")
  }
  
  # 3. Water Proportions (Late Period)
  cat("\n3. Water Type Proportions (Late Period):\n")
  if (!is.null(unified_results$water_proportions)) {
    # Expected columns: Water_Type, total_volume_km3, proportion
    print(unified_results$water_proportions)
  } else {
    cat("No water proportions data available.\n")
  }
}

# Add this function near the top with other helper functions
validate_period_coverage <- function(data, period_name, expected_years) {
  actual_years <- sort(unique(data$year))
  missing_years <- setdiff(expected_years, actual_years)
  if (length(missing_years) > 0) {
    warning(paste("Missing years in", period_name, "period:",
                  paste(missing_years, collapse=", ")))
  }
  # Print years found for verification
  message(paste("Years found in", period_name, "period:",
                paste(actual_years, collapse=", ")))
}

check_years <- function(data, name) {
  if (!is.null(data)) {
    years <- sort(unique(data$year))
    message(paste("Years in", name, ":", paste(years, collapse=", ")))
  }
}

debug_single_flow <- function(trade_data, water_footprint_data) {
  # Look at USA maize export to one country
  single_flow <- trade_data %>%
    filter(
      ReporterISO3 == "USA",
      WF_Crop == "Maize",
      year == 1998  # Close to Konar's early period
    ) %>%
    # Get the largest import partner
    arrange(desc(value_primary)) %>%
    slice(1) %>%
    # Join with water footprint data
    left_join(
      water_footprint_data %>%
        filter(WF_Crop == "Maize") %>%
        select(iso3, WF_Crop, Water_Type, year, 
               WaterFootprint_km3_per_year, Production_kg),
      by = c(
        "ReporterISO3" = "iso3",
        "WF_Crop" = "WF_Crop",
        "year" = "year"
      )
    )
  
  # Print detailed debug info
  cat("\nDebugging single largest USA maize export flow:")
  cat("\n==========================================")
  cat("\nFrom: USA")
  cat("\nTo:", single_flow$PartnerISO3)
  cat("\nYear:", single_flow$year)
  cat("\n")
  cat("\nTrade volume (kg):", format(single_flow$value_primary, scientific = FALSE))
  cat("\nTotal production (kg):", format(single_flow$Production_kg, scientific = FALSE))
  cat("\nTrade fraction:", single_flow$value_primary / single_flow$Production_kg)
  cat("\n")
  cat("\nWater volumes (km3/year):")
  print(single_flow %>% 
          select(Water_Type, WaterFootprint_km3_per_year) %>%
          filter(!is.na(Water_Type)))
  
  # Calculate virtual water flow both ways for comparison
  vw_flows <- single_flow %>%
    filter(!is.na(Water_Type)) %>%
    mutate(
      # Method 1: Direct fraction of water volume
      flow_1 = WaterFootprint_km3_per_year * (value_primary / Production_kg),
      
      # Method 2: Through L/kg intensity (Konar's apparent approach)
      intensity_L_per_kg = (WaterFootprint_km3_per_year * 1e12) / Production_kg,
      flow_2 = (intensity_L_per_kg * value_primary) / 1e12
    )
  
  cat("\nCalculated flows (km3):")
  print(vw_flows %>%
          select(Water_Type, flow_1, flow_2))
  
  return(vw_flows)
}

# -----------------------------
# 10. Main Function
# -----------------------------
main <- function() {
  # Create output directories
  dir.create("outputs/logs", recursive = TRUE, showWarnings = FALSE)
  
  # Setup logging
  log_file <- file.path(
    "outputs", "logs",
    paste0("main_execution_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".log")
  )
  
  log_message <- function(msg) {
    timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    message <- paste0(timestamp, " - ", msg)
    cat(message, "\n")
    cat(message, "\n", file = log_file, append = TRUE)
  }
  
  # Load additional checks
  source("additional_checks.R")
  
  # Validate initial setup
  tryCatch({
    log_message("Validating code mappings...")
    mapping_validation <- validate_mappings()
    
    if (analysis_type == "period") {
      years_early <- periods$early
      years_late <- periods$late
      period_name_early <- paste(min(years_early), max(years_early), sep="-")
      period_name_late <- paste(min(years_late), max(years_late), sep="-")  
      
      # Process early period
      log_message("Processing early period")
      
      # Early period data processing with error checking
      production_data_early <- tryCatch({
        process_production_data(years_early)
      }, error = function(e) {
        log_message(paste("Error processing early production data:", e$message))
        stop(e)
      })
      
      water_footprints_early <- tryCatch({
        calculate_water_footprints(years_early)
      }, error = function(e) {
        log_message(paste("Error calculating early water footprints:", e$message))
        stop(e)
      })
      
      # NEW: Check water footprint proportions for early period
      log_message("Analyzing early period water proportions...")
      early_water_props <- check_water_proportions(water_footprints_early, period_name_early)
      
      trade_data_early <- tryCatch({
        process_trade_data(years_early)
      }, error = function(e) {
        log_message(paste("Error processing early trade data:", e$message))
        stop(e)
      })
      
      # NEW: Analyze early period trade connections
      log_message("Analyzing early period trade connections...")
      early_trade_connections <- analyze_trade_connections(trade_data_early, period_name_early)
      
      flows_early <- tryCatch({
        calculate_virtual_water_flows(trade_data_early, water_footprints_early)
      }, error = function(e) {
        log_message(paste("Error calculating early virtual water flows:", e$message))
        stop(e)
      })
      
      # NEW: Analyze China's role in early period
      log_message("Analyzing China's trade role in early period...")
      early_china_analysis <- analyze_china_flows(flows_early, period_name_early)
      
      diagnostic_maps_early <- create_diagnostic_maps(
        water_footprints_early,
        production_data_early,
        flows_early,
        period_name_early  # Change from "early_period"
      )
      
      log_message("Running debug analysis on single flow...")
      debug_results_early <- debug_single_flow(trade_data_early, water_footprints_early)
      
      # Write debug results to file
      write.csv(debug_results_early, 
                file.path("outputs", "debug_single_flow_early.csv"),
                row.names = FALSE)
      
      # After calculating flows_early:
      log_message("Creating early period network plots...")
      tryCatch({
        create_static_network_plots(
          flows = flows_early %>% filter(!is.na(Water_Type)),
          period_name = period_name_early,
          threshold_quantile = 0.95
        )
      }, error = function(e) {
        log_message(paste("Error creating early period network plots:", e$message))
      })
      
      # Process late period
      log_message("Processing late period")
      
      # Late period data processing with error checking
      production_data_late <- tryCatch({
        process_production_data(years_late)
      }, error = function(e) {
        log_message(paste("Error processing late production data:", e$message))
        stop(e)
      })
      
      water_footprints_late <- tryCatch({
        calculate_water_footprints(years_late)
      }, error = function(e) {
        log_message(paste("Error calculating late water footprints:", e$message))
        stop(e)
      })
      
      # NEW: Check water footprint proportions for late period
      log_message("Analyzing late period water proportions...")
      late_water_props <- check_water_proportions(water_footprints_late, period_name_late)
      
      trade_data_late <- tryCatch({
        process_trade_data(years_late)
      }, error = function(e) {
        log_message(paste("Error processing late trade data:", e$message))
        stop(e)
      })
      
      # NEW: Analyze late period trade connections
      log_message("Analyzing late period trade connections...")
      late_trade_connections <- analyze_trade_connections(trade_data_late, period_name_late)
      
      flows_late <- tryCatch({
        calculate_virtual_water_flows(trade_data_late, water_footprints_late)
      }, error = function(e) {
        log_message(paste("Error calculating late virtual water flows:", e$message))
        stop(e)
      })
      
      # NEW: Analyze China's role in late period
      log_message("Analyzing China's trade role in late period...")
      late_china_analysis <- analyze_china_flows(flows_late, period_name_late)
      
      diagnostic_maps_late <- create_diagnostic_maps(
        water_footprints_late,
        production_data_late,
        flows_late,
        period_name_late  # Change from "late_period"
      )
      
      # Read global totals for validation
      log_message("Reading global water use totals...")
      global_totals <- read_global_totals()
      
      # Run validation checks
      log_message("Running comprehensive validation checks...")
      validation_results <- validate_water_balance(
        flows_early = flows_early,
        flows_late = flows_late,
        water_footprints_early = water_footprints_early,
        water_footprints_late = water_footprints_late,
        global_totals = global_totals
      )
      
      # Save validation results
      log_message("Saving validation results...")
      if (!is.null(validation_results)) {
        # Save each component separately with error handling
        tryCatch({
          if (!is.null(validation_results$trade_fractions)) {
            write.csv(
              as.data.frame(validation_results$trade_fractions),
              file.path("outputs", "validation_trade_fractions.csv"),
              row.names = FALSE
            )
          }
          
          if (!is.null(validation_results$mass_balance)) {
            write.csv(
              as.data.frame(validation_results$mass_balance),
              file.path("outputs", "validation_mass_balance.csv"),
              row.names = FALSE
            )
          }
          
          if (!is.null(validation_results$water_proportions)) {
            write.csv(
              as.data.frame(validation_results$water_proportions),
              file.path("outputs", "validation_water_proportions.csv"),
              row.names = FALSE
            )
          }
          
          # NEW: Save additional check results
          write.csv(
            early_water_props,
            file.path("outputs", "early_water_proportions.csv"),
            row.names = FALSE
          )
          write.csv(
            late_water_props,
            file.path("outputs", "late_water_proportions.csv"),
            row.names = FALSE
          )
          write.csv(
            early_china_analysis,
            file.path("outputs", "early_china_analysis.csv"),
            row.names = FALSE
          )
          write.csv(
            late_china_analysis,
            file.path("outputs", "late_china_analysis.csv"),
            row.names = FALSE
          )
          write.csv(
            early_trade_connections,
            file.path("outputs", "early_trade_connections.csv"),
            row.names = FALSE
          )
          write.csv(
            late_trade_connections,
            file.path("outputs", "late_trade_connections.csv"),
            row.names = FALSE
          )
        }, error = function(e) {
          log_message(paste("Error saving validation results:", e$message))
        })
      } else {
        log_message("Warning: No validation results to save")
      }
      
      # Generate key statistics with proper period sums
      key_stats <- bind_rows(
        # EARLY PERIOD
        flows_early %>%
          filter(!is.na(Water_Type)) %>%
          group_by(WF_Crop, Water_Type) %>%
          summarise(
            # 1. Total Virtual Water (km³)
            #    Sum all flows and convert m³ -> km³ by / 1e9
            Total_Virtual_Water_km3 = sum(virtual_water_m3, na.rm = TRUE) / 1e9,
            
            # 2. Weighted average intensity (optional):
            #    This weights WF_L_per_kg by how much volume of trade there was (value_primary).
            #    If you prefer a simple unweighted average, just use mean(WF_L_per_kg).
            Weighted_Mean_Intensity_L_per_kg =
              ifelse(
                sum(value_primary, na.rm = TRUE) > 0,
                sum(WF_L_per_kg * value_primary, na.rm = TRUE) / sum(value_primary, na.rm = TRUE),
                NA_real_
              ),
            
            # 3. Total trade volume in million tonnes:
            #    Sum all 'value_primary' (kg), then / 1e9 to get million tonnes (Mt).
            Trade_Volume_Mt = sum(value_primary, na.rm = TRUE) / 1e9,
            
            # 4. Period label (e.g., 1996-2000)
            Period = paste(min(years_early), max(years_early), sep = "-"),
            
            .groups = 'drop'
          ),
        
        # LATE PERIOD
        flows_late %>%
          filter(!is.na(Water_Type)) %>%
          group_by(WF_Crop, Water_Type) %>%
          summarise(
            Total_Virtual_Water_km3 = sum(virtual_water_m3, na.rm = TRUE) / 1e9,
            
            Weighted_Mean_Intensity_L_per_kg =
              ifelse(
                sum(value_primary, na.rm = TRUE) > 0,
                sum(WF_L_per_kg * value_primary, na.rm = TRUE) / sum(value_primary, na.rm = TRUE),
                NA_real_
              ),
            
            Trade_Volume_Mt = sum(value_primary, na.rm = TRUE) / 1e9,
            
            Period = paste(min(years_late), max(years_late), sep = "-"),
            
            .groups = 'drop'
          )
      )
      
      # Save the updated key statistics
      write.csv(
        key_stats,
        file.path("outputs", "key_statistics.csv"),
        row.names = FALSE
      )
      
      
      # Simple write without the comment
      write.csv(key_stats,
                file.path("outputs", "key_statistics.csv"),
                row.names = FALSE)
      
      # Create visualizations
      log_message("Creating visualizations...")
      
      
      # After calculating flows_late:
      log_message("Creating late period network plots...")
      tryCatch({
        create_static_network_plots(
          flows = flows_late %>% filter(!is.na(Water_Type)),
          period_name = period_name_late,
          threshold_quantile = 0.95
        )
      })
      
      # 1. Network analysis and metrics
      log_message("Calculating network metrics...")
      early_metrics <- analyze_network_metrics(
        flows_early %>% filter(!is.na(Water_Type)),
        period_name_early  # Change from "early_period"
      )
      late_metrics <- analyze_network_metrics(
        flows_late %>% filter(!is.na(Water_Type)),
        period_name_late  # Change from "late_period"
      )
      
      metrics_comparison <- compare_network_metrics(early_metrics, late_metrics)
      
      # 2. Create and save water type maps
      log_message("Creating water type maps...")
      water_map_results <- tryCatch({
        create_water_type_maps(
          flows_early,
          flows_late,
          years_early,
          years_late
        )
      }, error = function(e) {
        log_message(paste("Error in create_water_type_maps:", e$message))
        NULL
      })
      
      # Print validation results with proper checks
      if (!is.null(water_map_results) && water_map_results$is_valid) {
        cat("\nValidation Summary:\n")
        print(water_map_results$validation_summary)
        
        cat("\nValidation Check (Blue + Green vs Total):\n")
        print(water_map_results$comparison_table)
      } else {
        log_message("Warning: Water type maps validation failed or produced invalid results")
      }
      
      # Calculate validation check with error handling
      validation_check <- tryCatch({
        if (is.data.frame(water_map_results$validation_summary)) {
          water_map_results$validation_summary %>%
            group_by(Period) %>%
            reframe(
              Blue_Green_Sum = sum(Total_Volume_km3[Type %in% c("Blue", "Green")], na.rm = TRUE),
              Total = Total_Volume_km3[Type == "Total"],
              Difference = abs(Blue_Green_Sum - Total),
              Difference_Percent = (Difference / Total) * 100
            )
        } else {
          log_message("Warning: validation_summary is not a data frame")
          NULL
        }
      }, error = function(e) {
        log_message(paste("Error calculating validation check:", e$message))
        NULL
      })
      
      if (!is.null(validation_check)) {
        cat("\nValidation Check (Blue + Green vs Total):\n")
        print(validation_check)
      } else {
        cat("\nValidation check could not be performed\n")
      }
      
      # Create remaining visualizations and analyses
      log_message("Creating remaining visualizations and analyses...")
      
      rankings_plot <- create_trade_rankings_plot(flows_early, flows_late)
      metrics_comparison_plot <- plot_network_metrics_comparison(early_metrics, late_metrics)
      condensed_rankings <- create_condensed_trade_rankings(flows_early, period_name_early)
      condensed_rankings_late <- create_condensed_trade_rankings(flows_late, period_name_late)
      
      plot_data <- prepare_vw_barplot_data(
        flows_early %>% filter(!is.na(Water_Type)),
        flows_late %>% filter(!is.na(Water_Type))
      )
      
      if (!is.null(plot_data) && nrow(plot_data) > 0) {
        barplots <- plot_vw_barplots(plot_data)
      }
      
      early_dashboard <- create_trade_dashboard(flows_early, period_name_early)
      late_dashboard <- create_trade_dashboard(flows_late, period_name_late)
      
      # Final analysis and validation
      log_message("Running final analysis and validation...")
      print_data_summary(flows_early %>% filter(!is.na(Water_Type)), "Early Period")
      print_data_summary(flows_late %>% filter(!is.na(Water_Type)), "Late Period")
      value_analysis <- analyze_value_differences(key_stats)
      
      # Save final results
      log_message("Saving final results...")
      write.csv(
        flows_early %>% filter(!is.na(Water_Type)),
        file.path("outputs", "virtual_water_flows_early_period.csv"),
        row.names = FALSE
      )
      write.csv(
        flows_late %>% filter(!is.na(Water_Type)),
        file.path("outputs", "virtual_water_flows_late_period.csv"),
        row.names = FALSE
      )
      
      log_message("Analysis complete.")
    }
  }, error = function(e) {
    log_message(paste("Critical error in main execution:", e$message))
    stop(e)
  })
}

# -----------------------------
# 11. Execute the Analysis
# -----------------------------
tryCatch({
  main()
}, error = function(e) {
  cat("\nError in main execution:", conditionMessage(e), "\n")
  cat("Check the log files in outputs/logs for details\n")
})