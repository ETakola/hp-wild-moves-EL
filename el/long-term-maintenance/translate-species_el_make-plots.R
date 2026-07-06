# Retrieve vernacular species names in any language from online sources ----
# Author: Dr. Elina Takola
# Date: 2026-07-06 (06 July 2026)
# Description: This is a script that finds common names of species in various languages, from each species' scientific (latin) name using WikiData.
# The last chunk is optional and contains code to create an interactive visualization and a static PNG of the species networks. 

# Import species names, packages & build functions to get Greek common names ----

library(httr)        # Sends web requests, used for Wikidata
library(ragg)        # Graphics device package, kept from your original workflow
library(dplyr)       # Data wrangling
library(scales)      # Plotting/scaling tools, kept from your original workflow
library(plotly)      # Interactive plotting, kept from your original workflow
library(stringr)     # String cleaning and matching
library(htmlwidgets) # HTML widget tools, kept from your original workflow
library(jsonlite)    # Reads JSON returned by Wikidata
library(htmltools)   # HTML tools, kept from your original workflow
library(readr)       # Writes UTF-8 CSV files
library(rfishbase)   # Queries FishBase and SeaLifeBase
library(rvest)       # Scrapes the HCG / Αλιεία webpage
library(purrr)       # Provides map_dfr()
library(tidyr)       # Tidy data helpers
library(tibble)      # Creates tibbles
library(worrms)      # Queries WoRMS / Aphia
library(ritis)       # Queries ITIS

set.seed(161)        # Sets seed for reproducibility if random steps are used later


# Language settings - You can change this into any language ----

wikidata_language <- "el"                         # Wikidata language code for Greek
fishbase_language <- "Greek"                      # FishBase / SeaLifeBase language name for Greek
worms_language_name <- "Greek"                    # WoRMS language name for Greek
worms_language_codes <- c("el", "ell", "gre")     # Possible Greek language codes in WoRMS
itis_language_name <- "Greek"                     # ITIS language name for Greek

# Final output column names ----
# Don't forget to change those based on the selected language!

final_name_col <- "vernacularNameEl"              # Final Greek common name
final_source_col <- "source_El"                   # Source of the final Greek common name

# Temporary internal column names ----
# These columns are used only while the script runs.
# They are removed at the end. You don't have to rename them if you don't want to. 

col_wikidata <- "tmp_wikidata_Gr"                 # Temporary Wikidata Greek-name column
col_fishbase <- "tmp_fishbase_Gr"                 # Temporary FishBase Greek-name column
col_sealifebase <- "tmp_sealifebase_Gr"           # Temporary SeaLifeBase Greek-name column
col_worms <- "tmp_worms_Gr"                       # Temporary WoRMS Greek-name column
col_itis <- "tmp_itis_Gr"                         # Temporary ITIS Greek-name column
col_hcg <- "tmp_hcg_Gr"                           # Temporary HCG Greek-name column

# Import data ----

sp <- read.csv(file.choose(), header = TRUE, sep = ",", stringsAsFactors = FALSE) # Choose the input file ("species.csv")
str(sp)                                           # Check structure


# Save original columns ----
# This keeps your original data columns and lets us remove temporary lookup columns later.

original_cols <- setdiff(                         # Store original columns, excluding final columns if already present
  names(sp),
  c(final_name_col, final_source_col)
)


# Check required column ----

if (!"species" %in% names(sp)) {                  # Check that scientific names are present
  stop("Your data frame must contain a column named 'species'.")
}


# Helper: detect missing or blank names ----

is_missing_name <- function(x) {                  # Function to identify missing names
  x <- as.character(x)                            # Convert input to text
  is.na(x) | stringr::str_trim(x) == ""           # TRUE if NA or blank
}


# Helper: keep only the first valid name ----
# This prevents multiple names being combined with semicolons.

collapse_names <- function(x) {                   # Function to choose one name
  x <- as.character(x)                            # Convert input to text
  x <- stringr::str_squish(x)                     # Remove extra spaces
  x <- x[!is_missing_name(x)]                     # Remove missing or blank values
  x <- unique(x)                                  # Keep unique names only
  
  if (length(x) == 0) {                           # If no valid names exist
    NA_character_                                 # Return NA
  } else {                                        # If at least one valid name exists
    x[1]                                          # Return only the first valid name
  }
}


# 1) Wikidata Greek common names ----

get_wikidata_vernacular_names <- function(sp,
                                          language = wikidata_language,
                                          output_col = col_wikidata) {
  
  species_values <- paste0(                       # Create SPARQL VALUES list
    '"',                                          # Opening quote
    gsub('"', '\\"', sp$species),                 # Escape double quotes in names
    '"',                                          # Closing quote
    collapse = " "                                # Separate names with spaces
  )
  
  query <- paste0(                                # Build Wikidata SPARQL query
    '
    SELECT ?species ?vernacularName WHERE {
      VALUES ?species { ', species_values, ' }
      
      ?item wdt:P225 ?species .
      ?item rdfs:label ?vernacularName .
      
      FILTER(LANG(?vernacularName) = "', language, '")
    }
    '
  )
  
  res <- httr::POST(                              # Send query to Wikidata
    url = "https://query.wikidata.org/sparql",    # Wikidata SPARQL endpoint
    body = list(                                  # Request body
      query = query,                              # SPARQL query
      format = "json"                             # JSON output
    ),
    encode = "form",                              # Form-encoded request
    httr::user_agent("R Wikidata species name lookup") # User agent
  )
  
  httr::stop_for_status(res)                      # Stop if request failed
  
  dat <- jsonlite::fromJSON(                      # Parse JSON response
    httr::content(res, as = "text", encoding = "UTF-8"),
    flatten = TRUE
  )
  
  bindings <- dat$results$bindings               # Extract result rows
  
  if (length(bindings) == 0 || nrow(bindings) == 0) { # If Wikidata returned nothing
    wikidata_names <- tibble(                     # Create empty lookup table
      species = character(),
      source_name = character()
    )
  } else {                                        # If Wikidata returned names
    wikidata_names <- tibble(                     # Create lookup table
      species = bindings$species.value,           # Scientific name
      source_name = bindings$vernacularName.value # Greek label
    ) %>%
      mutate(
        species = stringr::str_squish(species),   # Clean scientific name
        source_name = stringr::str_squish(source_name), # Clean Greek name
        source_name = if_else(                    # Remove names identical to scientific names
          source_name == species,
          NA_character_,
          source_name
        )
      ) %>%
      group_by(species) %>%                       # Group by species
      summarise(
        source_name = collapse_names(source_name), # Keep first valid Greek name
        .groups = "drop"
      )
  }
  
  sp %>%                                          # Return data with Wikidata temporary column
    select(-any_of(output_col)) %>%               # Remove old temporary column if present
    left_join(wikidata_names, by = "species") %>% # Join Wikidata names
    mutate("{output_col}" := source_name) %>%     # Save names into temporary column
    select(-source_name)                          # Remove helper column
}


# 2) FishBase / SeaLifeBase Greek common names ----

get_rfishbase_common_names <- function(sp,
                                       species_col = "species",
                                       output_col,
                                       language = fishbase_language,
                                       server = "fishbase",
                                       source_label = "FishBase") {
  
  species_vec <- sp[[species_col]] %>%            # Extract species names
    unique() %>%                                  # Keep unique species names
    na.omit() %>%                                 # Remove NA
    stringr::str_squish()                         # Clean spaces
  
  fb <- purrr::map_dfr(species_vec, function(x) { # Query species one by one
    message(source_label, ": ", x)                # Print progress
    
    tryCatch({                                    # Keep running even if one query fails
      
      dat <- rfishbase::common_names(             # Query FishBase or SeaLifeBase
        species_list = x,                         # Scientific name
        Language = language,                      # Greek
        server = server                           # fishbase or sealifebase
      )
      
      if (is.null(dat) || nrow(dat) == 0) {       # If no names were found
        return(tibble(
          species = x,
          source_name = NA_character_
        ))
      }
      
      dat %>%
        mutate(
          species = x,                            # Use original input species for joining
          source_name = ComName                   # Common name from source
        ) %>%
        select(species, source_name)              # Keep only necessary columns
      
    }, error = function(e) {                      # If query fails
      tibble(
        species = x,
        source_name = NA_character_               # Return NA instead of stopping
      )
    })
  })
  
  fb_lookup <- fb %>%                             # Build lookup table
    group_by(species) %>%                         # Group by species
    summarise(
      source_name = collapse_names(source_name),  # Keep first valid Greek name
      .groups = "drop"
    )
  
  sp %>%                                          # Return data with temporary column
    select(-any_of(output_col)) %>%               # Remove old temporary column if present
    left_join(fb_lookup, by = setNames("species", species_col)) %>% # Join names
    mutate("{output_col}" := source_name) %>%     # Save names into temporary column
    select(-source_name)                          # Remove helper column
}


# 3) WoRMS Greek common names ----

get_worms_common_names <- function(sp,
                                   species_col = "species",
                                   output_col = col_worms,
                                   language_name = worms_language_name,
                                   language_codes = worms_language_codes) {
  
  species_vec <- sp[[species_col]] %>%            # Extract species names
    unique() %>%                                  # Keep unique species names
    na.omit() %>%                                 # Remove NA
    stringr::str_squish()                         # Clean spaces
  
  worms <- purrr::map_dfr(species_vec, function(x) { # Query species one by one
    message("WoRMS: ", x)                         # Print progress
    
    tryCatch({                                    # Keep running if one query fails
      
      aphia_id <- worrms::wm_name2id(             # Find WoRMS AphiaID
        name = x,                                 # Scientific name
        marine_only = FALSE                       # Search marine and non-marine
      )
      
      if (is.null(aphia_id) || length(aphia_id) == 0 || is.na(aphia_id[1])) {
        return(tibble(
          species = x,
          source_name = NA_character_             # Return NA if no AphiaID
        ))
      }
      
      dat <- worrms::wm_common_id(aphia_id[1])    # Get common names for AphiaID
      
      if (is.null(dat) || nrow(dat) == 0) {       # If no common names returned
        return(tibble(
          species = x,
          source_name = NA_character_
        ))
      }
      
      dat <- as_tibble(dat)                       # Convert to tibble
      
      if (!"vernacular" %in% names(dat)) {        # Check vernacular-name column exists
        return(tibble(
          species = x,
          source_name = NA_character_
        ))
      }
      
      if (!"language" %in% names(dat)) {          # Add language column if missing
        dat$language <- NA_character_
      }
      
      if (!"language_code" %in% names(dat)) {     # Add language-code column if missing
        dat$language_code <- NA_character_
      }
      
      dat <- dat %>%                              # Filter to Greek names only
        mutate(
          language = as.character(language),      # Ensure language is text
          language_code = as.character(language_code) # Ensure code is text
        ) %>%
        filter(
          stringr::str_to_lower(language) == stringr::str_to_lower(language_name) |
            stringr::str_to_lower(language_code) %in% stringr::str_to_lower(language_codes)
        )
      
      if (nrow(dat) == 0) {                       # If no Greek names remain
        return(tibble(
          species = x,
          source_name = NA_character_
        ))
      }
      
      tibble(
        species = x,                              # Original input species
        source_name = dat$vernacular              # Greek WoRMS name
      )
      
    }, error = function(e) {                      # If query fails
      tibble(
        species = x,
        source_name = NA_character_               # Return NA instead of stopping
      )
    })
  })
  
  worms_lookup <- worms %>%                       # Build lookup table
    group_by(species) %>%                         # Group by species
    summarise(
      source_name = collapse_names(source_name),  # Keep first valid Greek name
      .groups = "drop"
    )
  
  sp %>%                                          # Return data with temporary column
    select(-any_of(output_col)) %>%               # Remove old temporary column if present
    left_join(worms_lookup, by = setNames("species", species_col)) %>% # Join names
    mutate("{output_col}" := source_name) %>%     # Save names into temporary column
    select(-source_name)                          # Remove helper column
}


# 4) ITIS Greek common names ----

get_itis_common_names <- function(sp,
                                  species_col = "species",
                                  output_col = col_itis,
                                  language_name = itis_language_name) {
  
  species_vec <- sp[[species_col]] %>%            # Extract species names
    unique() %>%                                  # Keep unique names
    na.omit() %>%                                 # Remove NA
    stringr::str_squish()                         # Clean spaces
  
  itis <- purrr::map_dfr(species_vec, function(x) { # Query species one by one
    message("ITIS: ", x)                          # Print progress
    
    tryCatch({                                    # Keep running if one query fails
      
      sci <- ritis::search_scientific(x)          # Search ITIS by scientific name
      
      if (is.null(sci) || nrow(sci) == 0) {       # If no match found
        return(tibble(
          species = x,
          source_name = NA_character_
        ))
      }
      
      sci <- as_tibble(sci)                       # Convert ITIS result to tibble
      
      if (!"tsn" %in% names(sci)) {               # Check TSN column exists
        return(tibble(
          species = x,
          source_name = NA_character_
        ))
      }
      
      tsn <- sci$tsn[1]                           # Use the first TSN match
      
      if (is.null(tsn) || length(tsn) == 0 || is.na(tsn)) { # Check TSN is valid
        return(tibble(
          species = x,
          source_name = NA_character_
        ))
      }
      
      dat <- ritis::common_names(tsn)             # Get common names for the TSN
      
      if (is.null(dat) || nrow(dat) == 0) {       # If no common names returned
        return(tibble(
          species = x,
          source_name = NA_character_
        ))
      }
      
      dat <- as_tibble(dat)                       # Convert common names to tibble
      
      common_col <- intersect(                    # Find the common-name column
        c("commonName", "common_name", "vernacular", "vernacularName"),
        names(dat)
      )[1]
      
      if (is.na(common_col)) {                    # If no common-name column found
        return(tibble(
          species = x,
          source_name = NA_character_
        ))
      }
      
      if (!"language" %in% names(dat)) {          # Add language column if missing
        dat$language <- NA_character_
      }
      
      dat <- dat %>%                              # Filter ITIS common names to Greek
        mutate(language = as.character(language)) %>%
        filter(stringr::str_to_lower(language) == stringr::str_to_lower(language_name))
      
      if (nrow(dat) == 0) {                       # If no Greek names remain
        return(tibble(
          species = x,
          source_name = NA_character_
        ))
      }
      
      tibble(
        species = x,                              # Original input species
        source_name = dat[[common_col]]           # Greek ITIS common name
      )
      
    }, error = function(e) {                      # If query fails
      tibble(
        species = x,
        source_name = NA_character_               # Return NA instead of stopping
      )
    })
  })
  
  itis_lookup <- itis %>%                         # Build lookup table
    group_by(species) %>%                         # Group by species
    summarise(
      source_name = collapse_names(source_name),  # Keep first valid Greek name
      .groups = "drop"
    )
  
  sp %>%                                          # Return data with temporary column
    select(-any_of(output_col)) %>%               # Remove old temporary column if present
    left_join(itis_lookup, by = setNames("species", species_col)) %>% # Join names
    mutate("{output_col}" := source_name) %>%     # Save names into temporary column
    select(-source_name)                          # Remove helper column
}


# 5) HCG / Αλιεία Greek commercial names ----

get_hcg_alieia_greek_names <- function(sp,
                                       species_col = "species",
                                       output_col = col_hcg,
                                       url = "https://alieia.hcg.gr/fishes/PSARIA/PSARIA_KAT.php") {
  
  page <- rvest::read_html(                       # Read HCG / Αλιεία webpage
    url,                                          # URL of Greek fish catalogue
    encoding = "UTF-8"                            # Use UTF-8 for Greek text
  )
  
  txt <- page %>%                                 # Extract webpage text
    rvest::html_text2() %>%                       # Convert HTML to readable text
    stringr::str_split("\n") %>%                  # Split text into lines
    unlist() %>%                                  # Convert list to vector
    stringr::str_squish()                         # Clean spaces
  
  txt <- txt[txt != ""]                           # Remove empty lines
  
  hcg_lookup <- tibble(line = txt) %>%            # Put page lines into table
    mutate(
      fao_code = stringr::str_match(line, "^([A-Z]{3})\\s+")[, 2], # Extract FAO code
      line_no_code = stringr::str_remove(line, "^[A-Z]{3}\\s+"),   # Remove FAO code
      
      scientific_name = stringr::str_match(       # Extract scientific name at line end
        line_no_code,
        "([A-Z][a-z]+\\s+(?:[a-z]+|spp\\.?))$"
      )[, 2],
      
      greek_name = stringr::str_squish(           # Extract Greek commercial name
        stringr::str_remove(
          line_no_code,
          "\\s+[A-Z][a-z]+\\s+(?:[a-z]+|spp\\.?)$"
        )
      )
    ) %>%
    filter(
      !is.na(scientific_name),                    # Keep rows with scientific names
      !is.na(greek_name),                         # Keep rows with Greek names
      greek_name != scientific_name,              # Remove invalid matches
      greek_name != ""                            # Remove blanks
    ) %>%
    transmute(
      species = stringr::str_squish(scientific_name), # Scientific name
      source_name = greek_name                    # Greek HCG name
    ) %>%
    distinct()                                    # Remove duplicate rows
  
  hcg_lookup <- hcg_lookup %>%                    # Build lookup table
    group_by(species) %>%                         # Group by species
    summarise(
      source_name = collapse_names(source_name),  # Keep first valid Greek name
      .groups = "drop"
    )
  
  sp %>%                                          # Return data with temporary column
    select(-any_of(output_col)) %>%               # Remove old temporary column if present
    left_join(hcg_lookup, by = setNames("species", species_col)) %>% # Join names
    mutate("{output_col}" := source_name) %>%     # Save names into temporary column
    select(-source_name)                          # Remove helper column
}


# 6) Run all Greek-name lookups ----

sp <- get_wikidata_vernacular_names(              # Add Wikidata Greek names
  sp = sp,
  language = wikidata_language,
  output_col = col_wikidata
)

sp <- get_rfishbase_common_names(                 # Add FishBase Greek names
  sp = sp,
  species_col = "species",
  output_col = col_fishbase,
  language = fishbase_language,
  server = "fishbase",
  source_label = "FishBase"
)

sp <- get_rfishbase_common_names(                 # Add SeaLifeBase Greek names
  sp = sp,
  species_col = "species",
  output_col = col_sealifebase,
  language = fishbase_language,
  server = "sealifebase",
  source_label = "SeaLifeBase"
)

sp <- get_worms_common_names(                     # Add WoRMS Greek names
  sp = sp,
  species_col = "species",
  output_col = col_worms,
  language_name = worms_language_name,
  language_codes = worms_language_codes
)

sp <- get_itis_common_names(                      # Add ITIS Greek names
  sp = sp,
  species_col = "species",
  output_col = col_itis,
  language_name = itis_language_name
)

sp <- get_hcg_alieia_greek_names(                 # Add HCG / Αλιεία Greek names
  sp = sp,
  species_col = "species",
  output_col = col_hcg
)


# 7) Create final Greek name and final source only ----
# Priority order:
# Wikidata -> FishBase -> SeaLifeBase -> WoRMS -> ITIS -> HCG

sp <- sp %>%
  mutate(
    "{final_name_col}" := case_when(              # Create final Greek-name column
      !is_missing_name(.data[[col_wikidata]]) ~ .data[[col_wikidata]],             # Use Wikidata first
      !is_missing_name(.data[[col_fishbase]]) ~ .data[[col_fishbase]],             # Then FishBase
      !is_missing_name(.data[[col_sealifebase]]) ~ .data[[col_sealifebase]],       # Then SeaLifeBase
      !is_missing_name(.data[[col_worms]]) ~ .data[[col_worms]],                   # Then WoRMS
      !is_missing_name(.data[[col_itis]]) ~ .data[[col_itis]],                     # Then ITIS
      !is_missing_name(.data[[col_hcg]]) ~ .data[[col_hcg]],                       # Then HCG
      TRUE ~ NA_character_                                                         # Otherwise NA
    ),
    
    "{final_source_col}" := case_when(            # Create source column
      !is_missing_name(.data[[col_wikidata]]) ~ "Wikidata",             # Source is Wikidata
      !is_missing_name(.data[[col_fishbase]]) ~ "FishBase",             # Source is FishBase
      !is_missing_name(.data[[col_sealifebase]]) ~ "SeaLifeBase",       # Source is SeaLifeBase
      !is_missing_name(.data[[col_worms]]) ~ "WoRMS",                   # Source is WoRMS
      !is_missing_name(.data[[col_itis]]) ~ "ITIS",                     # Source is ITIS
      !is_missing_name(.data[[col_hcg]]) ~ "HCG_Alieia",                # Source is HCG / Αλιεία
      TRUE ~ NA_character_                                              # Otherwise no source
    )
  )


# 8) Remove temporary source-specific columns ----
# Final data will contain original columns plus:
# vernacularNameEl
# source_El

sp <- sp %>%
  select(
    any_of(original_cols),                         # Keep original input columns
    all_of(final_name_col),                        # Keep final Greek name
    all_of(final_source_col)                       # Keep final source
  )


# 9) Inspect results ----

sp %>%
  count(source_El, sort = TRUE)                    # Count how many names came from each source


# 10) Save CSV ----

readr::write_excel_csv(                            # Save final result as CSV
  sp,                                              # Data frame to save
  "species_with_Greek_vernacular_names_final_only.csv", # Output filename
  na = ""                                          # Save NA as blank cells
)


# Optional: visualize interactive species network with sp data frame and static png ----
# Assumes your data frame is already called sp

# Helpers
is_missing <- function(x) {
  is.na(x) | str_trim(x) == ""
}

hex_to_rgba <- function(hex, alpha = 1) {
  rgb <- grDevices::col2rgb(hex)
  sprintf(
    "rgba(%s,%s,%s,%.3f)",
    rgb[1, 1], rgb[2, 1], rgb[3, 1], alpha
  )
}

# Simple force-based label repulsion
repel_label_positions <- function(df,
                                  iterations = 500,
                                  step = 0.025,
                                  pull = 0.015,
                                  point_padding = 0.35) {
  
  n <- nrow(df)
  
  lx <- df$label_x
  ly <- df$label_y
  ax <- df$x
  ay <- df$y
  
  # approximate label "radius" using text length
  label_radius <- pmin(1.15, pmax(0.35, nchar(df$display_name) * 0.035))
  
  for (iter in seq_len(iterations)) {
    
    # label-label repulsion
    dx <- outer(lx, lx, "-")
    dy <- outer(ly, ly, "-")
    dist <- sqrt(dx^2 + dy^2) + 1e-6
    
    diag(dist) <- Inf
    
    min_dist <- outer(label_radius, label_radius, "+")
    overlap <- pmax(0, min_dist - dist)
    
    fx <- rowSums((dx / dist) * overlap, na.rm = TRUE) * step
    fy <- rowSums((dy / dist) * overlap, na.rm = TRUE) * step
    
    # label-point repulsion, so labels avoid sitting on stars
    px <- outer(lx, df$x, "-")
    py <- outer(ly, df$y, "-")
    pdist <- sqrt(px^2 + py^2) + 1e-6
    
    point_overlap <- pmax(0, point_padding - pdist)
    
    fx_point <- rowSums((px / pdist) * point_overlap, na.rm = TRUE) * step
    fy_point <- rowSums((py / pdist) * point_overlap, na.rm = TRUE) * step
    
    # gentle pull back toward each species point
    fx_pull <- (ax - lx) * pull
    fy_pull <- (ay - ly) * pull
    
    lx <- lx + fx + fx_point + fx_pull
    ly <- ly + fy + fy_point + fy_pull
  }
  
  df$label_x <- lx
  df$label_y <- ly
  
  df
}


# Clean data
sp_clean <- sp %>%
  mutate(
    occurrenceCount = as.numeric(occurrenceCount),
    occurrenceCount = if_else(is.na(occurrenceCount), 0, occurrenceCount),
    speciesGroup = if_else(is_missing(speciesGroup), "Unknown group", speciesGroup),
    display_name = case_when(
      !is_missing(vernacularNameEn) & vernacularNameEn != species ~ vernacularNameEn,
      TRUE ~ species
    ),
    occ_log = log10(occurrenceCount + 1)
  )


# Colours
group_names <- sort(unique(sp_clean$speciesGroup))

group_cols <- setNames(
  hcl.colors(length(group_names), palette = "Dark 3"),
  group_names
)


# Group centres
group_centres <- sp_clean %>%
  group_by(speciesGroup) %>%
  summarise(
    total_occurrence = sum(occurrenceCount, na.rm = TRUE),
    n_species = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(total_occurrence)) %>%
  mutate(
    group_id = row_number(),
    n_groups = n(),
    angle = seq(0, 2 * pi, length.out = n_groups + 1)[1:n_groups],
    centre_radius = 7,
    centre_x = centre_radius * cos(angle),
    centre_y = centre_radius * sin(angle)
  )


# Species positions

constellation_df <- sp_clean %>%
  left_join(group_centres, by = "speciesGroup") %>%
  group_by(speciesGroup) %>%
  arrange(desc(occurrenceCount), .by_group = TRUE) %>%
  mutate(
    rank_in_group = row_number(),
    
    # abundant species closer to the group centre
    local_radius = rescale(rank_in_group, to = c(0.45, 3.1)) +
      runif(n(), -0.35, 0.35),
    
    local_angle = runif(n(), 0, 2 * pi),
    
    x = centre_x + local_radius * cos(local_angle),
    y = centre_y + local_radius * sin(local_angle),
    
    # initial label position: slightly outside each species point
    outward_x = if_else(
      sqrt((x - centre_x)^2 + (y - centre_y)^2) == 0,
      cos(local_angle),
      (x - centre_x) / sqrt((x - centre_x)^2 + (y - centre_y)^2)
    ),
    outward_y = if_else(
      sqrt((x - centre_x)^2 + (y - centre_y)^2) == 0,
      sin(local_angle),
      (y - centre_y) / sqrt((x - centre_x)^2 + (y - centre_y)^2)
    ),
    
    label_x = x + 0.45 * outward_x,
    label_y = y + 0.45 * outward_y
  ) %>%
  ungroup() %>%
  arrange(desc(occurrenceCount)) %>%
  mutate(
    label_rank = row_number(),
    point_size = rescale(sqrt(occurrenceCount + 1), to = c(5, 28)),
    hover_text = paste0(
      "<b>", htmlEscape(display_name), "</b>",
      "<br><i>", htmlEscape(species), "</i>",
      "<br>Species group: ", htmlEscape(speciesGroup),
      "<br>Occurrences: ", comma(occurrenceCount),
      "<br>Species key: ", speciesKey
    )
  )

# repel all species labels from one another and from stars
constellation_df <- repel_label_positions(constellation_df)


# Cluster label positions

cluster_labels <- constellation_df %>%
  group_by(speciesGroup, centre_x, centre_y, angle) %>%
  summarise(
    cluster_extent = max(
      sqrt((x - centre_x)^2 + (y - centre_y)^2),
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  mutate(
    label_offset = cluster_extent + 1.35,
    label_x = centre_x + label_offset * cos(angle),
    label_y = centre_y + label_offset * sin(angle),
    label_xanchor = case_when(
      cos(angle) > 0.25 ~ "left",
      cos(angle) < -0.25 ~ "right",
      TRUE ~ "center"
    ),
    label_yanchor = case_when(
      sin(angle) > 0.25 ~ "bottom",
      sin(angle) < -0.25 ~ "top",
      TRUE ~ "middle"
    )
  )


# Annotations/labels
initial_labels <- 18

species_annotations <- lapply(seq_len(nrow(constellation_df)), function(i) {
  
  d <- constellation_df[i, ]
  
  list(
    x = d$label_x,
    y = d$label_y,
    xref = "x",
    yref = "y",
    text = htmlEscape(d$display_name),
    showarrow = FALSE,
    visible = d$label_rank <= initial_labels,
    font = list(
      size = 11,
      color = group_cols[[d$speciesGroup]]
    ),
    bgcolor = "rgba(11,16,32,0.72)",
    bordercolor = "rgba(255,255,255,0)",
    borderpad = 2,
    opacity = 0.98
  )
})

cluster_annotations <- lapply(seq_len(nrow(cluster_labels)), function(i) {
  
  d <- cluster_labels[i, ]
  
  list(
    x = d$label_x,
    y = d$label_y,
    xref = "x",
    yref = "y",
    text = paste0("<b>", htmlEscape(d$speciesGroup), "</b>"),
    showarrow = FALSE,
    visible = TRUE,
    xanchor = d$label_xanchor,
    yanchor = d$label_yanchor,
    font = list(
      size = 14,
      color = "white"
    ),
    bgcolor = group_cols[[d$speciesGroup]],
    bordercolor = "rgba(255,255,255,0)",
    borderpad = 5,
    opacity = 0.98
  )
})

all_annotations <- c(species_annotations, cluster_annotations)


# Plotly interactive plot

p <- plot_ly()

# connection lines, one trace per group
for (g in group_names) {
  
  d <- constellation_df %>%
    filter(speciesGroup == g)
  
  seg_x <- as.vector(rbind(d$centre_x, d$x, NA))
  seg_y <- as.vector(rbind(d$centre_y, d$y, NA))
  
  p <- p %>%
    add_trace(
      x = seg_x,
      y = seg_y,
      type = "scatter",
      mode = "lines",
      line = list(
        color = hex_to_rgba(group_cols[[g]], 0.20),
        width = 0.8
      ),
      hoverinfo = "none",
      showlegend = FALSE
    )
}

# species stars
for (g in group_names) {
  
  d <- constellation_df %>%
    filter(speciesGroup == g)
  
  p <- p %>%
    add_trace(
      data = d,
      x = ~x,
      y = ~y,
      type = "scatter",
      mode = "markers",
      name = g,
      text = ~hover_text,
      hoverinfo = "text",
      marker = list(
        color = group_cols[[g]],
        size = d$point_size,
        opacity = 0.86,
        line = list(
          color = "rgba(255,255,255,0.35)",
          width = 0.5
        )
      )
    )
}

# cluster centre stars
p <- p %>%
  add_trace(
    data = group_centres,
    x = ~centre_x,
    y = ~centre_y,
    type = "scatter",
    mode = "markers",
    hoverinfo = "none",
    showlegend = FALSE,
    marker = list(
      color = unname(group_cols[group_centres$speciesGroup]),
      size = 16,
      opacity = 0.95,
      line = list(
        color = "white",
        width = 1
      )
    )
  )

all_x <- c(constellation_df$x, constellation_df$label_x, cluster_labels$label_x)
all_y <- c(constellation_df$y, constellation_df$label_y, cluster_labels$label_y)

x_range <- range(all_x, na.rm = TRUE) + c(-1.2, 1.2)
y_range <- range(all_y, na.rm = TRUE) + c(-1.2, 1.2)

p <- p %>%
  layout(
    title = list(
      text = paste0(
        "<b>Constellations of species in Wild Moves</b>",
        "<br><sup>",
        "Each cluster is a species group. Larger stars and stars closer to the cluster centre represent species with more occurrences.",
        "</sup>"
      ),
      font = list(color = "white", size = 21),
      x = 0.5
    ),
    annotations = all_annotations,
    xaxis = list(
      visible = FALSE,
      zeroline = FALSE,
      showgrid = FALSE,
      range = x_range,
      scaleanchor = "y"
    ),
    yaxis = list(
      visible = FALSE,
      zeroline = FALSE,
      showgrid = FALSE,
      range = y_range
    ),
    plot_bgcolor = "#0B1020",
    paper_bgcolor = "#0B1020",
    legend = list(
      orientation = "h",
      x = 0.5,
      xanchor = "center",
      y = -0.05,
      font = list(color = "white")
    ),
    margin = list(l = 20, r = 20, t = 80, b = 40)
  ) %>%
  config(
    displayModeBar = TRUE,
    scrollZoom = TRUE
  )


# Reveal more labels when zooming in
label_meta <- constellation_df %>%
  transmute(
    rank = label_rank,
    x = x,
    y = y
  )

js_payload <- list(
  labels = label_meta,
  nSpeciesAnnotations = nrow(constellation_df),
  xRange = x_range,
  yRange = y_range
)

p <- htmlwidgets::onRender(
  p,
  sprintf(
    "
    function(el, x) {
      var gd = document.getElementById(el.id);
      var payload = %s;
      var labels = payload.labels;
      var nSpecies = payload.nSpeciesAnnotations;
      var fullX = payload.xRange;
      var fullY = payload.yRange;
      var updating = false;

      function currentRange(axisName, fallback) {
        var axis = gd._fullLayout[axisName];
        if (axis && axis.range) {
          return axis.range;
        }
        return fallback;
      }

      function maxRankForZoom(areaRatio) {
        if (areaRatio < 0.035) return 1000000;  // very zoomed in: all labels
        if (areaRatio < 0.080) return 220;
        if (areaRatio < 0.160) return 150;
        if (areaRatio < 0.300) return 90;
        if (areaRatio < 0.550) return 45;
        return 18;                              // zoomed out: only top species
      }

      function updateLabels() {
        if (updating) return;
        updating = true;

        var xr = currentRange('xaxis', fullX);
        var yr = currentRange('yaxis', fullY);

        var fullArea = Math.abs((fullX[1] - fullX[0]) * (fullY[1] - fullY[0]));
        var viewArea = Math.abs((xr[1] - xr[0]) * (yr[1] - yr[0]));
        var areaRatio = viewArea / fullArea;

        var maxRank = maxRankForZoom(areaRatio);
        var update = {};

        for (var i = 0; i < labels.length; i++) {
          var d = labels[i];

          var inView =
            d.x >= Math.min(xr[0], xr[1]) &&
            d.x <= Math.max(xr[0], xr[1]) &&
            d.y >= Math.min(yr[0], yr[1]) &&
            d.y <= Math.max(yr[0], yr[1]);

          update['annotations[' + i + '].visible'] =
            inView && d.rank <= maxRank;
        }

        // cluster labels are always visible
        var totalAnnotations = gd.layout.annotations.length;
        for (var j = nSpecies; j < totalAnnotations; j++) {
          update['annotations[' + j + '].visible'] = true;
        }

        Plotly.relayout(gd, update).then(function() {
          updating = false;
        });
      }

      gd.on('plotly_relayout', updateLabels);
      setTimeout(updateLabels, 400);
    }
    ",
    jsonlite::toJSON(js_payload, auto_unbox = TRUE)
  )
)
p

# save as HTML
htmlwidgets::saveWidget(p, file = "wildmoves_constellation_interactive.html", selfcontained = TRUE)

# and a static PNG 
# Change this if you want more or fewer labels
label_fraction <- 0.20

# Re-repel label positions for the static PNG
constellation_png <- repel_label_positions(
  constellation_df,
  iterations = 2000,
  step = 0.03,
  pull = 0.012,
  point_padding = 0.60
)

# Label only the most abundant 20% of species within each group
species_labels_png <- constellation_png %>%
  group_by(speciesGroup) %>%
  arrange(desc(occurrenceCount), .by_group = TRUE) %>%
  mutate(
    label_rank_in_group = row_number(),
    n_in_group = n(),
    n_to_label = pmax(1L, ceiling(label_fraction * n_in_group))
  ) %>%
  filter(label_rank_in_group <= n_to_label) %>%
  ungroup()

# Recompute cluster label positions
cluster_png <- constellation_png %>%
  group_by(speciesGroup, centre_x, centre_y, angle) %>%
  summarise(
    cluster_extent = max(
      sqrt((x - centre_x)^2 + (y - centre_y)^2),
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  mutate(
    label_offset = cluster_extent + 1.45,
    label_x = centre_x + label_offset * cos(angle),
    label_y = centre_y + label_offset * sin(angle)
  )

# Dynamic plot boundaries based on the actual plotted points and visible labels
all_x_png <- c(
  constellation_png$x,
  group_centres$centre_x,
  species_labels_png$label_x,
  cluster_png$label_x
)

all_y_png <- c(
  constellation_png$y,
  group_centres$centre_y,
  species_labels_png$label_y,
  cluster_png$label_y
)

x_span <- diff(range(all_x_png, na.rm = TRUE))
y_span <- diff(range(all_y_png, na.rm = TRUE))

x_pad <- max(1.8, 0.14 * x_span)
y_pad <- max(1.8, 0.14 * y_span)

x_range_png <- range(all_x_png, na.rm = TRUE) + c(-x_pad, x_pad)
y_range_png <- range(all_y_png, na.rm = TRUE) + c(-y_pad, y_pad)

# Dynamic PNG size based on the plotted coordinate range
plot_width_units <- diff(x_range_png)
plot_height_units <- diff(y_range_png)

px_per_unit <- 120

main_width_px <- round(plot_width_units * px_per_unit)
main_height_px <- round(plot_height_units * px_per_unit)

# Keep output within practical bounds
main_width_px <- min(max(main_width_px, 2400), 5200)
main_height_px <- min(max(main_height_px, 1800), 4200)

# Extra height for title/subtitle and legend
legend_rows <- ceiling(length(group_names) / 5)
extra_height_px <- 360 + legend_rows * 120

png_width_px <- main_width_px
png_height_px <- main_height_px + extra_height_px

# Helper to draw label boxes
draw_label <- function(x, y, label, text_col, fill_col, cex = 0.75, font = 1) {
  w <- strwidth(label, cex = cex, font = font) * 1.20
  h <- strheight(label, cex = cex, font = font) * 1.70
  
  rect(
    x - w / 2, y - h / 2,
    x + w / 2, y + h / 2,
    col = fill_col,
    border = NA
  )
  
  text(
    x,
    y,
    label,
    col = text_col,
    cex = cex,
    font = font
  )
}

# Save PNG
ragg::agg_png(
  filename = "wildmoves_constellation_static_top20pct_labels.png",
  width = png_width_px,
  height = png_height_px,
  units = "px",
  res = 220,
  background = "#0B1020"
)

# Separate main plot and legend so the legend is not cut off
layout(
  matrix(c(1, 2), ncol = 1),
  heights = c(main_height_px, extra_height_px)
)

# Main plot
par(
  bg = "#0B1020",
  mar = c(1.2, 1.2, 5.2, 1.2),
  xpd = NA
)

plot(
  NA,
  xlim = x_range_png,
  ylim = y_range_png,
  asp = 1,
  axes = FALSE,
  xlab = "",
  ylab = "",
  main = ""
)

title(
  main = "Constellations of species in Wild Moves",
  col.main = "white",
  cex.main = 1.9,
  font.main = 2,
  line = 3.2
)

mtext(
  "Each cluster is a species group. Larger stars and stars closer to the cluster centre represent species with more occurrences.",
  side = 3,
  line = 1.4,
  col = adjustcolor("white", alpha.f = 0.82),
  cex = 0.9
)

# Connection lines
for (g in group_names) {
  d <- constellation_png %>% filter(speciesGroup == g)
  
  segments(
    x0 = d$centre_x,
    y0 = d$centre_y,
    x1 = d$x,
    y1 = d$y,
    col = adjustcolor(group_cols[[g]], alpha.f = 0.22),
    lwd = 1
  )
}

# Species points
for (g in group_names) {
  d <- constellation_png %>% filter(speciesGroup == g)
  
  points(
    d$x,
    d$y,
    pch = 21,
    bg = adjustcolor(group_cols[[g]], alpha.f = 0.86),
    col = adjustcolor("white", alpha.f = 0.35),
    lwd = 0.8,
    cex = scales::rescale(d$point_size, to = c(0.9, 2.4))
  )
}

# Cluster centre points
points(
  group_centres$centre_x,
  group_centres$centre_y,
  pch = 21,
  bg = group_cols[group_centres$speciesGroup],
  col = "white",
  lwd = 1.2,
  cex = 2.1
)

# Species labels: top 20% most abundant within each group
for (i in seq_len(nrow(species_labels_png))) {
  d <- species_labels_png[i, ]
  
  draw_label(
    x = d$label_x,
    y = d$label_y,
    label = d$display_name,
    text_col = group_cols[[d$speciesGroup]],
    fill_col = adjustcolor("#0B1020", alpha.f = 0.72),
    cex = 0.72,
    font = 1
  )
}

# Cluster labels
for (i in seq_len(nrow(cluster_png))) {
  d <- cluster_png[i, ]
  
  draw_label(
    x = d$label_x,
    y = d$label_y,
    label = d$speciesGroup,
    text_col = "white",
    fill_col = group_cols[[d$speciesGroup]],
    cex = 1.05,
    font = 2
  )
}

# Legend panel
par(
  bg = "#0B1020",
  mar = c(0, 1, 0, 1),
  xpd = NA
)

plot.new()

legend(
  "center",
  legend = group_names,
  ncol = min(5, length(group_names)),
  bty = "n",
  text.col = "white",
  col = adjustcolor("white", alpha.f = 0.65),
  pt.bg = group_cols[group_names],
  pch = 21,
  pt.cex = 1.8,
  cex = 0.9
)

dev.off()

# Save script ----
writeLines(rstudioapi::getActiveDocumentContext()$contents, file.path(getwd(), "wikidata_GR_specnames_plot.R"), useBytes = TRUE)

# Session Info ----
#sessionInfo()
#R version 4.5.1 (2025-06-13 ucrt)
#Platform: x86_64-w64-mingw32/x64
#Running under: Windows 11 x64 (build 26200)
#Matrix products: default
#LAPACK version 3.12.1
#locale:
#[1] LC_COLLATE=English_United States.utf8 
#[2] LC_CTYPE=English_United States.utf8   
#[3] LC_MONETARY=English_United States.utf8
#[4] LC_NUMERIC=C                          
#[5] LC_TIME=English_United States.utf8    
#time zone: Europe/Berlin
#tzcode source: internal
#attached base packages:
#  [1] stats     graphics  grDevices utils     datasets  methods  base     
#loaded via a namespace (and not attached):
#  [1] compiler_4.5.1    cli_3.6.5         tools_4.5.1       rstudioapi_0.17.1    
