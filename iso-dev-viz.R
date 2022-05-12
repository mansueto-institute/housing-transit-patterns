

# https://drive.google.com/drive/folders/1R4lj1Ik-vNYFnVdYrMl0qHJ1y1wziUoP
# https://gist.github.com/marcdloeb/0f4f5994cd0e0be1aab302e090e81381


library(tidyverse)
library(sf)
library(lwgeom)
library(tidycensus)
library(scales)
library(viridis)
library(DT)
library(shiny)
library(ggplot2)
library(readxl)
library(patchwork)


# Read the .Renviron file (only necessary fi you ran census_api_key()
readRenviron("~/.Renviron")

# Explore ACS application ----------------------------------------------------

# Function to launch a mini Shiny app to look up Census variables
explore_acs_vars <- function () { 
  ui <- basicPage(h2("ACS Variable Search"), 
                  tags$style('#display {height:100px; white-space: pre-wrap;}'),
                  verbatimTextOutput('display', placeholder = TRUE),
                  mainPanel(DT::dataTableOutput(outputId = "acs_table", width = '800px'))
  )
  server <- function(input, output, session) {
    output$acs_table= DT::renderDataTable({ 
      acs5_vars <- acs5_vars 
    }, filter = "top", selection = 'multiple', options = list(columnDefs = list( list(className = "nowrap",width = '100px', targets = c(1,2))), pageLength = 20), server = TRUE) 
    selected_index <- reactive({
      acs5_vars %>% slice(input$acs_table_rows_selected) %>% pull(name)
    })
    output$display = renderPrint({
      s = unique(input$acs_table_rows_selected)
      if (length(s)) {cat(paste0("'",selected_index(),"'",collapse = ","))}
    })
  }
  shinyApp(ui, server)
}

# Census Variables
acs5_vars <- load_variables(year = 2020, dataset = c('acs5'), cache = FALSE) %>% 
  separate(col = 'concept',  into = c('concept_main','concept_part'), sep = c(' BY '), remove = FALSE,extra = "merge") %>%
  mutate(concept_part = case_when(is.na(concept_part) ~ 'TOTAL', TRUE ~ as.character(concept_part)))

# explore_acs_vars()

# Load vectors ---------------------------------------------------------------

tract <- get_acs(year = 2020, geography = "tract", 
                 survey = 'acs5', variables = 'B01003_001',
                 state = '17', county = '031', geometry = TRUE) %>%
  select(GEOID) %>%
  st_transform(crs = st_crs(4326)) # Transform to WGS84

bgroup <- get_acs(year = 2020, geography = "block group", 
                  survey = 'acs5', variables = 'B01003_001',
                  state = '17', county = '031', geometry = TRUE) %>%
  select(GEOID) %>%
  st_transform(crs = st_crs(4326)) # Transform to WGS84

# https://data.cityofchicago.org/Facilities-Geographic-Boundaries/Boundaries-Community-Areas-current-/cauq-8yn6
community_areas_url <- 'https://data.cityofchicago.org/api/geospatial/cauq-8yn6?method=export&format=GeoJSON'
community_areas <- sf::st_read(community_areas_url) %>% st_as_sf() %>% select(community)

chi_outline <- st_union(community_areas)

chi_bbox = st_bbox(community_areas)
chi_bbox_crop <- st_bbox(c(xmin = -87.862226, 
                           xmax = chi_bbox[[3]], 
                           ymax = chi_bbox[[4]], 
                           ymin = chi_bbox[[2]]), crs = st_crs(4326))


# Load isochrones ---------------------------------------------------------

#iso_5 <- st_read('/Users/nm/Desktop/saved_geojson/iso_5.geojson') %>% 
#  st_buffer(x = ., dist = units::set_units(100,m)) 
iso_10 <- st_read('/Users/nm/Desktop/saved_geojson/iso_10.geojson') %>%
  st_buffer(x = ., dist = units::set_units(200,m)) 
#iso_15 <- st_read('/Users/nm/Desktop/saved_geojson/iso_15.geojson')%>%
#  st_buffer(x = ., dist = units::set_units(100,m)) 
iso_20 <- st_read('/Users/nm/Desktop/saved_geojson/iso_20.geojson')%>%
  st_buffer(x = ., dist = units::set_units(100,m)) 
#iso_25 <- st_read('/Users/nm/Desktop/saved_geojson/iso_25.geojson')

isos <- rbind(iso_10, iso_20) #iso_5,   iso_15, iso_25
plot(isos)


# Join together -----------------------------------------------------------

bgroup2 <- bgroup %>%
  st_join(., isos %>% select(range, time), largest = TRUE)
tract2 <- tract %>%
  st_join(., isos %>% select(range, time), largest = TRUE)

plot(bgroup2 %>% select(time))

tract2 <- tract2 %>%
  st_intersection(., chi_outline) %>%
  st_crop(tract2, y = chi_bbox_crop) %>%
  mutate(lon = map_dbl(geometry, ~st_point_on_surface(.x)[[1]]),
         lat = map_dbl(geometry, ~st_point_on_surface(.x)[[2]])) 
  
bgroup2 <- bgroup2 %>%
  st_intersection(., chi_outline) %>%
  st_crop(., y = chi_bbox_crop) %>%
  mutate(lon = map_dbl(geometry, ~st_point_on_surface(.x)[[1]]),
         lat = map_dbl(geometry, ~st_point_on_surface(.x)[[2]])) 

bgroup2 <- bgroup2 %>%
  mutate(time_bucket = case_when(time == 5 ~ '0-10',
                                 time == 10 ~ '0-10',
                                 time == 15 ~ '10-20',
                                 time == 20 ~ '10-20',
                                 time == 25 ~ '21+',
                                 TRUE ~ as.character('21+')))
bgroup2$time_bucket <- factor(x = bgroup2$time_bucket, levels = c('0-10', '10-20', '21+'))

tract2 <- tract2 %>%
  mutate(time_bucket = case_when(time == 5 ~ '0-10',
                                 time == 10 ~ '0-10',
                                 time == 15 ~ '10-20',
                                 time == 20 ~ '10-20',
                                 time == 25 ~ '21+',
                                 TRUE ~ as.character('21+')))
tract2$time_bucket <- factor(x = tract2$time_bucket, levels = c('0-10', '10-20', '21+'))

bgroup2 <- bgroup2 %>% st_make_valid() %>%
  st_join(., community_areas %>% st_make_valid(), largest = TRUE)

tract2 <- tract2 %>% st_make_valid() %>%
  st_join(., community_areas %>% st_make_valid(), largest = TRUE)


# Total data --------------------------------------------------------------

tract_data_totals <- get_acs(year = 2020, geography = "tract", 
                              survey = 'acs5', variables = c('B01003_001', 'B08015_001', 'B25071_001', 'B25001_001', 'B25077_001', 'B19013_001', 'B19113_001', 'B19301_001', 'B25106_001', 'B25106_002', 'B25106_024'), 
                             summary_var = 'B25001_001',
                              state = '17', county = '031', geometry = FALSE) %>% 
  filter(GEOID %in% unique(tract2$GEOID)) %>%
  mutate(variable_label = case_when(
    variable == 'B01003_001' ~ 'Total population', 
    variable == 'B25001_001' ~ 'Total housing units',
    variable == 'B25071_001' ~ 'Median gross rent as a percentage of household income',
    variable == 'B08015_001' ~ 'Aggregate number of vehicles (car, truck, or van) used in commuting',
    variable == 'B25077_001' ~ 'Median housing value (dollars)',
    variable == 'B19013_001' ~ 'Median household income in the past 12 months (in 2020 inflation-adjusted dollars)', 
    variable == 'B19113_001' ~ 'Median family income in the past 12 months (in 2020 inflation-adjusted dollars)', 
    variable == 'B19301_001' ~ 'Per capita income in the past 12 months (in 2020 inflation-adjusted dollars)', 
    variable == 'B25106_001' ~ 'Housing cost as a percentage of household income in the past 12 months', 
    variable == 'B25106_002' ~ 'Owner-occupied housing cost as a percentage of household income in the past 12 months', 
    variable == 'B25106_024' ~ 'Renter-occupied housing cost as a percentage of household income in the past 12 months')) 

bgroup_data_totals <- get_acs(year = 2020, geography = "block group", 
                              survey = 'acs5',variables = c('B01003_001', 'B08015_001', 'B25071_001', 'B25001_001', 'B25077_001', 'B19013_001', 'B19113_001', 'B19301_001', 'B25106_001', 'B25106_002', 'B25106_024'), 
                              summary_var = 'B01003_001',
                              state = '17', county = '031', geometry = FALSE) %>% 
  filter(GEOID %in% unique(bgroup2$GEOID)) %>%
  mutate(variable_label = case_when(
    variable == 'B01003_001' ~ 'Total population', 
    variable == 'B25001_001' ~ 'Total housing units',
    variable == 'B25071_001' ~ 'Median gross rent as a percentage of household income',
    variable == 'B08015_001' ~ 'Aggregate number of vehicles (car, truck, or van) used in commuting',
    variable == 'B25077_001' ~ 'Median housing value (dollars)',
    variable == 'B19013_001' ~ 'Median household income in the past 12 months (in 2020 inflation-adjusted dollars)', 
    variable == 'B19113_001' ~ 'Median family income in the past 12 months (in 2020 inflation-adjusted dollars)', 
    variable == 'B19301_001' ~ 'Per capita income in the past 12 months (in 2020 inflation-adjusted dollars)', 
    variable == 'B25106_001' ~ 'Housing cost as a percentage of household income in the past 12 months', 
    variable == 'B25106_002' ~ 'Owner-occupied housing cost as a percentage of household income in the past 12 months', 
    variable == 'B25106_024' ~ 'Renter-occupied housing cost as a percentage of household income in the past 12 months')) 



# Block group categorical -------------------------------------------------

bgroup_data_race <- get_acs(year = 2020, geography = "block group", 
                            survey = 'acs5', variables = c('B03002_003', 'B08015_001', 'B03002_004','B03002_005','B03002_006','B03002_007','B03002_008','B03002_009','B03002_012'),
                            summary_var = 'B03002_001',
                            state = '17', county = '031', geometry = FALSE) %>%
  filter(GEOID %in% unique(bgroup2$GEOID)) %>%
  mutate(variable_label = case_when(
    variable == 'B03002_003' ~ 'White', # 'White alone'
    variable == 'B03002_004' ~ 'Black', # 'Black or African American alone'
    variable == 'B03002_005' ~ 'Other', # 'American Indian and Alaska Native alone'
    variable == 'B03002_006' ~ 'Asian', # 'Asian alone'
    variable == 'B03002_007' ~ 'Other', # 'Native Hawaiian and Other Pacific Islander alone'
    variable == 'B03002_008' ~ 'Other', # 'Some other race alone'
    variable == 'B03002_009' ~ 'Other', # 'Two or more races'
    variable == 'B03002_012' ~ 'Latino')) %>% # 'Hispanic or Latino'
  group_by(GEOID, variable_label, summary_est) %>% 
  summarize_at(vars(estimate), list(sum), na.rm = TRUE) %>%
  ungroup() %>%
  mutate(share = estimate/summary_est)
    
bgroup_data_income <- get_acs(year = 2020, geography = "block group", 
                              survey = 'acs5', variables = c('B19001_002', 'B19001_003', 'B19001_004', 'B19001_005', 'B19001_006', 'B19001_007', 'B19001_008', 'B19001_009', 'B19001_010', 'B19001_011', 'B19001_012', 'B19001_013', 'B19001_014', 'B19001_015', 'B19001_016', 'B19001_017'),
                              summary_var = 'B19001_001',
                              state = '17', county = '031', geometry = FALSE) %>% 
  filter(GEOID %in% unique(bgroup2$GEOID)) %>%
  mutate(variable_label = case_when(    
    variable == 'B19001_002' ~ '$0-29k', # 'Less than $10,000', 
    variable == 'B19001_003' ~ '$0-29k', # '$10,000 to $14,999', 
    variable == 'B19001_004' ~ '$0-29k', # '$15,000 to $19,999', 
    variable == 'B19001_005' ~ '$0-29k', # '$20,000 to $24,999', 
    variable == 'B19001_006' ~ '$0-29k', # '$25,000 to $29,999', 
    variable == 'B19001_007' ~ '$30-59k', # '$30,000 to $34,999', 
    variable == 'B19001_008' ~ '$30-59k', # '$35,000 to $39,999', 
    variable == 'B19001_009' ~ '$30-59k', # '$40,000 to $44,999', 
    variable == 'B19001_010' ~ '$30-59k', # '$45,000 to $49,999', 
    variable == 'B19001_011' ~ '$30-59k', # '$50,000 to $59,999', 
    variable == 'B19001_012' ~ '$60-99k', # '$60,000 to $74,999', 
    variable == 'B19001_013' ~ '$60-99k', # '$75,000 to $99,999', 
    variable == 'B19001_014' ~ '$100-149k', # '$100,000 to $124,999', 
    variable == 'B19001_015' ~ '$100-149k', # '$125,000 to $149,999', 
    variable == 'B19001_016' ~ '$150k+',# '$150,000 to $199,999', 
    variable == 'B19001_017' ~ '$150k+')) %>% # '$200,000 or more', 
  group_by(GEOID, variable_label, summary_est) %>% 
  summarize_at(vars(estimate), list(sum), na.rm = TRUE) %>%
  ungroup() %>%
  mutate(share = estimate/summary_est)
    
bgroup_data_units <- get_acs(year = 2020, geography = "block group", 
                             survey = 'acs5', variables = c('B25024_002', 'B25024_003', 'B25024_004', 'B25024_005', 'B25024_006', 'B25024_007', 'B25024_008', 'B25024_009', 'B25024_010', 'B25024_011'),
                             summary_var = 'B25024_001',
                             state = '17', county = '031', geometry = FALSE) %>% 
  filter(GEOID %in% unique(bgroup2$GEOID)) %>%
  mutate(variable_label = case_when(
    variable == 'B25024_002' ~ '1, detached', 
    variable == 'B25024_003' ~ 'Townhouse', #'1, attached', 
    variable == 'B25024_004' ~ '2 units', #'2', 
    variable == 'B25024_005' ~ '3-4 units', #'3 or 4', 
    variable == 'B25024_006' ~ '5-9 units', #'5 to 9', 
    variable == 'B25024_007' ~ '10-19 units', #'10 to 19', 
    variable == 'B25024_008' ~ '20+ units', #'20 to 49', 
    variable == 'B25024_009' ~ '20+ units', #'50 or more', 
    variable == 'B25024_010' ~ 'Other units', #'Mobile home', 
    variable == 'B25024_011' ~ 'Other units')) %>% #'Boat, RV, van, etc.', 
  group_by(GEOID, variable_label, summary_est) %>% 
  summarize_at(vars(estimate), list(sum), na.rm = TRUE) %>%
  ungroup() %>%
  mutate(share = estimate/summary_est)

bgroup_data_tenure <- get_acs(year = 2020, geography = "block group", 
                              survey = 'acs5', variables = c('B25032_002', 'B25032_003', 'B25032_013'),
                              summary_var = 'B25032_001',
                              state = '17', county = '031', geometry = FALSE) %>% 
  filter(GEOID %in% unique(bgroup2$GEOID)) %>%
  mutate(variable_label = case_when(
    variable == 'B25032_002' ~ 'Owner-occupied housing units', 
    variable == 'B25032_003' ~ 'Owner-occupied single-detached units', 
    variable == 'B25032_013' ~ 'Renter-occupied housing units')) %>%
  group_by(GEOID, variable_label, summary_est) %>% 
  summarize_at(vars(estimate), list(sum), na.rm = TRUE) %>%
  ungroup() %>%
  mutate(share = estimate/summary_est)

bgroup_data_hval <- get_acs(year = 2020, geography = "block group", 
                                 survey = 'acs5', variables = c('B25075_002','B25075_003','B25075_004','B25075_005',
                                                                'B25075_006','B25075_007','B25075_008','B25075_009',
                                                                'B25075_010','B25075_011','B25075_012','B25075_013',
                                                                'B25075_014','B25075_015','B25075_016','B25075_017',
                                                                'B25075_018','B25075_019','B25075_020','B25075_021',
                                                                'B25075_022','B25075_023','B25075_024','B25075_025',
                                                                'B25075_026','B25075_027'),       
                                 summary_var = 'B25075_001',
                                 state = '17', county = '031', geometry = FALSE) %>% 
  filter(GEOID %in% unique(bgroup2$GEOID)) %>%
  mutate(variable_label = case_when(
    variable == 'B25075_002' ~ '$0-99K', # 'Less than $10,000'
    variable == 'B25075_003' ~ '$0-99K', # '$10,000 to $14,999'
    variable == 'B25075_004' ~ '$0-99K', # '$15,000 to $19,999'
    variable == 'B25075_005' ~ '$0-99K', # '$20,000 to $24,999'
    variable == 'B25075_006' ~ '$0-99K', # '$25,000 to $29,999'
    variable == 'B25075_007' ~ '$0-99K', # '$30,000 to $34,999'
    variable == 'B25075_008' ~ '$0-99K', # '$35,000 to $39,999'
    variable == 'B25075_009' ~ '$0-99K', # '$40,000 to $49,999'
    variable == 'B25075_010' ~ '$0-99K', # '$50,000 to $59,999'
    variable == 'B25075_011' ~ '$0-99K', # '$60,000 to $69,999'
    variable == 'B25075_012' ~ '$0-99K', # '$70,000 to $79,999'
    variable == 'B25075_013' ~ '$0-99K', # '$80,000 to $89,999'
    variable == 'B25075_014' ~ '$0-99K', # '$90,000 to $99,999'
    variable == 'B25075_015' ~ '$100-199K', # '$100,000 to $124,999'
    variable == 'B25075_016' ~ '$100-199K', # '$125,000 to $149,999'
    variable == 'B25075_017' ~ '$100-199K', # '$150,000 to $174,999'
    variable == 'B25075_018' ~ '$100-199K', # '$175,000 to $199,999'
    variable == 'B25075_019' ~ '$200-299K', # '$200,000 to $249,999'
    variable == 'B25075_020' ~ '$200-299K', # '$250,000 to $299,999'
    variable == 'B25075_021' ~ '$300-399K', # '$300,000 to $399,999'
    variable == 'B25075_022' ~ '$400-499K', # '$400,000 to $499,999'
    variable == 'B25075_023' ~ '$500-749K', # '$500,000 to $749,999'
    variable == 'B25075_024' ~ '$750-999K', # '$750,000 to $999,999'
    variable == 'B25075_025' ~ '$1M+', # '$1,000,000 to $1,499,999'
    variable == 'B25075_026' ~ '$1M+', # '$1,500,000 to $1,999,999'
    variable == 'B25075_027' ~ '$1M+')) %>% # '$2,000,000 or more'
  group_by(GEOID, variable_label, summary_est) %>% 
  summarize_at(vars(estimate), list(sum), na.rm = TRUE) %>%
  ungroup() %>%
  mutate(share = estimate/summary_est)

bgroup_data_yearbuilt <- get_acs(year = 2020, geography = "block group", 
                                 survey = 'acs5', variables = c('B25034_002','B25034_003','B25034_004','B25034_005','B25034_006','B25034_007','B25034_008','B25034_009','B25034_010','B25034_011'),
                                 summary_var = 'B25034_001',
                                 state = '17', county = '031', geometry = FALSE) %>% 
  filter(GEOID %in% unique(bgroup2$GEOID)) %>%
  mutate(variable_label = case_when(
    variable == 'B25034_002' ~ 'Built 2014 or later',
    variable == 'B25034_003' ~ 'Built 2010-13',
    variable == 'B25034_004' ~ 'Built 2000-09',
    variable == 'B25034_005' ~ 'Built 1990-99',
    variable == 'B25034_006' ~ 'Built 1980-89',
    variable == 'B25034_007' ~ 'Built 1970-79',
    variable == 'B25034_008' ~ 'Built 1960-69',
    variable == 'B25034_009' ~ 'Built 1950-59',
    variable == 'B25034_010' ~ 'Built 1940-49',
    variable == 'B25034_011' ~ 'Built 1939 or earlier')) %>%
  group_by(GEOID, variable_label, summary_est) %>% 
  summarize_at(vars(estimate), list(sum), na.rm = TRUE) %>%
  ungroup() %>%
  mutate(share = estimate/summary_est)

# Tract categorical -------------------------------------------------

tract_data_transport <- get_acs(year = 2020, geography = "tract", 
                              survey = 'acs5', variables = c('B08006_003','B08006_004','B08006_008','B08006_014','B08006_015','B08006_016','B08006_017'),
                              summary_var = 'B08006_001',
                              state = '17', county = '031', geometry = FALSE) %>% 
  filter(GEOID %in% unique(tract2$GEOID)) %>%
  mutate(variable_label = case_when(
    variable == 'B08006_003' ~ 'Drove alone',
    variable == 'B08006_004' ~ 'Carpooled',
    variable == 'B08006_008' ~ 'Public transportation',
    variable == 'B08006_014' ~ 'Bicycle',
    variable == 'B08006_015' ~ 'Walked',
    variable == 'B08006_016' ~ 'Taxicab, motorcycle, or other',
    variable == 'B08006_017' ~ 'Worked from home')) %>%
  group_by(GEOID, variable_label, summary_est) %>% 
  summarize_at(vars(estimate), list(sum), na.rm = TRUE) %>%
  ungroup() %>%
  mutate(share = estimate/summary_est)

tract_data_afford <- get_acs(year = 2020, geography = "tract", 
                             survey = 'acs5', variables = c('B25070_002', 'B25070_003', 'B25070_004', 'B25070_005', 'B25070_006', 'B25070_007', 'B25070_008', 'B25070_009', 'B25070_010'),
                             summary_var = 'B25070_001',
                             state = '17', county = '031', geometry = FALSE) %>% 
  filter(GEOID %in% unique(tract2$GEOID)) %>%
  mutate(variable_label = case_when(
    variable == 'B25070_002' ~ '< 10%',
    variable == 'B25070_003' ~ '10-14%',
    variable == 'B25070_004' ~ '15-19%',
    variable == 'B25070_005' ~ '20-24%',
    variable == 'B25070_006' ~ '25-29%',
    variable == 'B25070_007' ~ '30-34%',
    variable == 'B25070_008' ~ '35-39%',
    variable == 'B25070_009' ~ '40-49%',
    variable == 'B25070_010' ~ '> 50%')) %>%
  group_by(GEOID, variable_label, summary_est) %>% 
  summarize_at(vars(estimate), list(sum), na.rm = TRUE) %>%
  ungroup() %>%
  mutate(share = estimate/summary_est)

tract_data_travel <- get_acs(year = 2020, geography = "tract", 
                              survey = 'acs5', variables = c('B08012_002','B08012_003','B08012_004','B08012_005','B08012_006','B08012_007','B08012_008','B08012_009','B08012_010','B08012_011','B08012_013'),   
                              summary_var = 'B08012_001',
                              state = '17', county = '031', geometry = FALSE) %>% 
  filter(GEOID %in% unique(tract2$GEOID)) %>%
  mutate(variable_label = case_when(
    variable == 'B08012_002' ~ '0-14 minutes', # 'Less than 5 minutes',
    variable == 'B08012_003' ~ '0-14 minutes', # '5 to 9 minutes',
    variable == 'B08012_004' ~ '0-14 minutes', # '10 to 14 minutes',
    variable == 'B08012_005' ~ '15-29 minutes', # '15 to 19 minutes',
    variable == 'B08012_006' ~ '15-29 minutes', # '20 to 24 minutes',
    variable == 'B08012_007' ~ '15-29 minutes', # '25 to 29 minutes',
    variable == 'B08012_008' ~ '30-44 minutes', # '30 to 34 minutes',
    variable == 'B08012_009' ~ '30-44 minutes', # '35 to 39 minutes',
    variable == 'B08012_010' ~ '30-44 minutes', # '40 to 44 minutes',
    variable == 'B08012_011' ~ '45-59 minutes', # '45 to 59 minutes',
    variable == 'B08012_012' ~ '60+ minutes', # '60 to 89 minutes',
    variable == 'B08012_013' ~ '60+ minutes')) %>% # '90 or more minutes',
  group_by(GEOID, variable_label, summary_est) %>% 
  summarize_at(vars(estimate), list(sum), na.rm = TRUE) %>%
  ungroup() %>%
  mutate(share = estimate/summary_est)


# Prep for viz ------------------------------------------------------------

bgroup_data_race <- bgroup_data_race %>% mutate(group = 'Race')
bgroup_data_income <- bgroup_data_income %>% mutate(group = 'Income')
bgroup_data_units <- bgroup_data_units %>% mutate(group = 'Units')
bgroup_data_tenure <- bgroup_data_tenure  %>% mutate(group = 'Tenure')
bgroup_data_hval <- bgroup_data_hval %>% mutate(group = 'Housing value')
bgroup_data_yearbuilt <- bgroup_data_yearbuilt  %>% mutate(group = 'Year built')

bgroup_data_shares <- rbind(bgroup_data_race, bgroup_data_income, bgroup_data_units, bgroup_data_tenure, bgroup_data_hval, bgroup_data_yearbuilt )                   
bgroup2_nogeo <- bgroup2 %>% st_drop_geometry() %>% select(GEOID, time_bucket, community)
bgroup_data_shares <- bgroup_data_shares %>% 
  left_join(., bgroup2_nogeo, by = c('GEOID' = 'GEOID'))
bgroup_data_shares <- bgroup_data_shares %>%
  filter(variable_label != 'Owner-occupied single-detached units') %>%
  group_by(time_bucket, group, variable_label) %>%
  summarize_at(vars(estimate), list(sum), na.rm = TRUE) %>%
  ungroup() %>%
  group_by(time_bucket, group) %>%
  mutate(estimate_sum = sum(estimate)) %>%
  ungroup() %>%
  mutate(share = estimate / estimate_sum)
bgroup_data_shares$time_bucket <- factor(bgroup_data_shares$time_bucket, levels = c('0-10', '10-20', '21+')) 

tract_data_travel <- tract_data_travel  %>% mutate(group = 'Travel time')
tract_data_transport <- tract_data_transport  %>% mutate(group = 'Transport')
tract_data_afford <- tract_data_afford  %>% mutate(group = 'Afford')

tract_data_shares <- rbind(tract_data_transport, tract_data_travel, tract_data_afford )
tract2_nogeo <- tract2 %>% st_drop_geometry() %>% select(GEOID, time_bucket, community)
tract_data_shares <- tract_data_shares %>% 
  left_join(., tract2_nogeo, by = c('GEOID' = 'GEOID'))
tract_data_shares <- tract_data_shares %>%
  group_by(time_bucket, group, variable_label) %>%
  summarize_at(vars(estimate), list(sum), na.rm = TRUE) %>%
  ungroup() %>%
  group_by(time_bucket, group) %>%
  mutate(estimate_sum = sum(estimate)) %>%
  ungroup() %>%
  mutate(share = estimate / estimate_sum)
tract_data_shares$time_bucket <- factor(tract_data_shares$time_bucket, levels = c('0-10', '10-20', '21+')) 


#write_csv(tract_data_shares , '/Users/nm/Desktop/tract_data.csv')
#write_csv(bgroup_data_shares  , '/Users/nm/Desktop/blockgroup_data.csv')



bgroup_race <- bgroup_data_shares %>% filter(group == 'Race') 
race_order = c('White', 'Black', 'Latino', 'Asian', 'Other')
bgroup_race$variable_label <- factor(bgroup_race$variable_label, levels = race_order)
bgroup_race <- bgroup_race %>% 
  arrange(time_bucket, factor(variable_label, levels = rev(race_order))) %>%
  #arrange(time_bucket, factor(variable_label, levels = (race_order))) %>%
  group_by(time_bucket) %>%
  mutate(pos_id_share = (cumsum(share) - 0.5*share)) %>% ungroup() 

bgroup_income <- bgroup_data_shares %>% filter(group ==  'Income') 
income_order = c('$0-29k', '$30-59k',  '$60-99k',  '$100-149k', '$150k+')
bgroup_income$variable_label <- factor(bgroup_income$variable_label, levels = income_order)
bgroup_income <- bgroup_income %>%
  arrange(time_bucket, factor(variable_label, levels = rev(income_order))) %>%
  group_by(time_bucket) %>%
  mutate(pos_id_share = (cumsum(share) - 0.5*share)) %>% ungroup() 

bgroup_units <- bgroup_data_shares %>% filter(group == 'Units') 
units_order =  c('1, detached', 'Townhouse','2 units','3-4 units','5-9 units','10-19 units','20+ units','Other units')
bgroup_units$variable_label <- factor(bgroup_units $variable_label, levels = units_order )
bgroup_units <- bgroup_units %>%
  arrange(time_bucket, factor(variable_label, levels = rev(units_order))) %>%
  group_by(time_bucket) %>%
  mutate(pos_id_share = (cumsum(share) - 0.5*share)) %>% ungroup() 

bgroup_tenure <- bgroup_data_shares %>% filter(group == 'Tenure') 
tenure_order =  c('Owner-occupied housing units','Renter-occupied housing units')
bgroup_tenure$variable_label <- factor(bgroup_tenure$variable_label, levels = tenure_order )
bgroup_tenure <- bgroup_tenure %>%
  arrange(time_bucket, factor(variable_label, levels = rev(tenure_order))) %>%
  group_by(time_bucket) %>%
  mutate(pos_id_share = (cumsum(share) - 0.5*share)) %>% ungroup() 

bgroup_hval <- bgroup_data_shares %>% filter(group == 'Housing value') 
hval_order = c('$0-99K', '$100-199K', '$200-299K', '$300-399K', '$400-499K', '$500-749K', '$750-999K', '$1M+')
bgroup_hval$variable_label <- factor(bgroup_hval$variable_label, levels = hval_order)
bgroup_hval <- bgroup_hval %>% 
  arrange(time_bucket, factor(variable_label, levels = rev(hval_order))) %>%
  group_by(time_bucket) %>%
  mutate(pos_id_share = (cumsum(share) - 0.5*share)) %>% ungroup() 

bgroup_yearbuilt <- bgroup_data_shares %>% filter(group == 'Year built') 
yearbuilt_order =  c('Built 2014 or later', 'Built 2010-13', 'Built 2000-09', 'Built 1990-99', 'Built 1980-89', 'Built 1970-79', 'Built 1960-69', 'Built 1950-59', 'Built 1940-49', 'Built 1939 or earlier')
bgroup_yearbuilt$variable_label <- factor(bgroup_yearbuilt$variable_label, levels = yearbuilt_order )
bgroup_yearbuilt <- bgroup_yearbuilt %>%
  arrange(time_bucket, factor(variable_label, levels = rev(yearbuilt_order))) %>%
  group_by(time_bucket) %>%
  mutate(pos_id_share = (cumsum(share) - 0.5*share)) %>% ungroup() 


tract_travel<- tract_data_shares %>% filter(group == 'Travel time') 
travel_order =  c('0-14 minutes', '15-29 minutes', '30-44 minutes', '45-59 minutes', '60+ minutes')
tract_travel$variable_label <- factor(tract_travel$variable_label, levels = travel_order )
tract_travel <- tract_travel %>%
  arrange(time_bucket, factor(variable_label, levels = rev(travel_order))) %>%
  group_by(time_bucket) %>%
  mutate(pos_id_share = (cumsum(share) - 0.5*share)) %>% ungroup() 

tract_transport <- tract_data_shares %>% filter(group == 'Transport') 
transport_order =  c('Drove alone', 'Carpooled', 'Public transportation', 'Bicycle', 'Walked', 'Taxicab, motorcycle, or other', 'Worked from home')
tract_transport$variable_label <- factor(tract_transport$variable_label, levels = transport_order )
tract_transport <- tract_transport %>%
  arrange(time_bucket, factor(variable_label, levels = rev(transport_order))) %>%
  group_by(time_bucket) %>%
  mutate(pos_id_share = (cumsum(share) - 0.5*share)) %>% ungroup() 

tract_afford <- tract_data_shares %>% filter(group == 'Afford') 
afford_order =  c('< 10%', '10-14%', '15-19%', '20-24%', '25-29%', '30-34%', '35-39%', '40-49%', '> 50%')
tract_afford$variable_label <- factor(tract_afford$variable_label, levels = afford_order )
tract_afford <- tract_afford %>%
  arrange(time_bucket, factor(variable_label, levels = rev(afford_order))) %>%
  group_by(time_bucket) %>%
  mutate(pos_id_share = (cumsum(share) - 0.5*share)) %>% ungroup() 


# Plot bar charts ---------------------------------------------------------

(p_bar_race <- ggplot(bgroup_race) +
  geom_bar(aes(x = time_bucket, y = share, fill = (variable_label)), 
           color = 'white', stat="identity") + coord_flip() +
  scale_fill_manual(values = (c("#009EFA",'#F77552',"#49DEA4","#ffc425",'#845EC2','#FF6F91','#00D2FC','#008F7A','#00C0A3'))) + 
  theme_bw() +
  scale_y_continuous(labels = scales::percent, expand = c(.035, 0)) +
  geom_text(aes(label=ifelse(share >= 0.12, paste0(round(share*100,0),"%\n",comma(estimate, scale = .001, accuracy =1, suffix = 'K')),""), 
                y = pos_id_share, x = time_bucket), color = 'white', fontface = "bold", size = 5) +
    labs(subtitle = 'Race / ethnicity of population') + ylab('Percent') + xlab('Walking distance to L Stop\n(minutes)') +
  theme(legend.title = element_blank(), 
        legend.position = 'bottom',
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 11),
        text = element_text(color = "#0a0a0a" )))

(p_bar_income <- ggplot(bgroup_income) +
    geom_bar(aes(x = time_bucket, y = share, fill = (variable_label)), 
             color = 'white', stat="identity") + coord_flip() +
    scale_fill_manual(values = c("#009EFA",'#F77552',"#49DEA4","#ffc425",'#845EC2','#FF6F91','#00D2FC','#008F7A','#00C0A3')) + 
    theme_bw() +
    scale_y_continuous(labels = scales::percent, expand = c(.035, 0)) +
    geom_text(aes(label=ifelse(share >= 0.12, paste0(round(share*100,0),"%\n",comma(estimate, scale = .001, accuracy =1, suffix = 'K')),""), 
                  y = pos_id_share, x = time_bucket), color = 'white', fontface = "bold", size = 5) +
    labs(subtitle = 'Household income in the past 12 months') + ylab('Percent') + xlab('Walking distance to L Stop\n(minutes)') +
    theme(legend.title = element_blank(), 
          legend.position = 'bottom',
          axis.text = element_text(size = 12),
          legend.text = element_text(size = 11),
          text = element_text(color = "#0a0a0a" )))

(p_bar_units <- ggplot(bgroup_units) +
    geom_bar(aes(x = time_bucket, y = share, fill = (variable_label)), 
             color = 'white', stat="identity") + coord_flip() +
    scale_fill_manual(values = c("#009EFA",'#F77552',"#49DEA4","#ffc425",'#845EC2','#FF6F91','#00D2FC','#008F7A','#00C0A3')) + 
    theme_bw() +
    scale_y_continuous(labels = scales::percent, expand = c(.035, 0)) +
    geom_text(aes(label=ifelse(share >= 0.12, paste0(round(share*100,0),"%\n",comma(estimate, scale = .001, accuracy =1, suffix = 'K')),""), 
                  y = pos_id_share, x = time_bucket), color = 'white', fontface = "bold", size = 5) +
    labs(subtitle = 'Housing unit type') + ylab('Percent') + xlab('Walking distance to L Stop\n(minutes)') +
    theme(legend.title = element_blank(), 
          legend.position = 'bottom',
          axis.text = element_text(size = 12),
          legend.text = element_text(size = 11),
          text = element_text(color = "#0a0a0a" )))

(p_bar_tenure  <- ggplot(bgroup_tenure ) +
    geom_bar(aes(x = time_bucket, y = share, fill = (variable_label)), 
             color = 'white', stat="identity") + coord_flip() +
    scale_fill_manual(values = c("#009EFA",'#F77552',"#49DEA4","#ffc425",'#845EC2','#FF6F91','#00D2FC','#008F7A','#00C0A3')) + 
    theme_bw() +
    scale_y_continuous(labels = scales::percent, expand = c(.035, 0)) +
    geom_text(aes(label=ifelse(share >= 0.12, paste0(round(share*100,0),"%\n",comma(estimate, scale = .001, accuracy =1, suffix = 'K')),""), 
                  y = pos_id_share, x = time_bucket), color = 'white', fontface = "bold", size = 5) +
    labs(subtitle = 'Housing unit occupancy') + ylab('Percent') + xlab('Walking distance to L Stop\n(minutes)') +
    theme(legend.title = element_blank(), 
          legend.position = 'bottom',
          axis.text = element_text(size = 12),
          legend.text = element_text(size = 11),
          text = element_text(color = "#0a0a0a" )))

(p_bar_transport  <- ggplot(tract_transport) +
    geom_bar(aes(x = time_bucket, y = share, fill = (variable_label)), 
             color = 'white', stat="identity") + coord_flip() +
    scale_fill_manual(values = c("#009EFA",'#F77552',"#49DEA4","#ffc425",'#845EC2','#FF6F91','#00D2FC','#008F7A','#00C0A3')) + 
    theme_bw() +
    scale_y_continuous(labels = scales::percent, expand = c(.035, 0)) +
    geom_text(aes(label=ifelse(share >= 0.12, paste0(round(share*100,0),"%\n",comma(estimate, scale = .001, accuracy =1, suffix = 'K')),""), 
                  y = pos_id_share, x = time_bucket), color = 'white', fontface = "bold", size = 5) +
    labs(subtitle = 'Means of transportation of working population aged 16 and over') + ylab('Percent') + xlab('Walking distance to L Stop\n(minutes)') +
    theme(legend.title = element_blank(), 
          legend.position = 'bottom',
          axis.text = element_text(size = 12),
          legend.text = element_text(size = 11),
          text = element_text(color = "#0a0a0a" )))

colorhexes <- colorRampPalette(c("#0194D3","#D1D3D4","#f5f5f5","#ffc425","#49DEA4"))(length(yearbuilt_order))
(p_bar_yearbuilt <- ggplot(bgroup_yearbuilt) +
    geom_bar(aes(x = time_bucket, y = share, fill = (variable_label)), 
             color = 'white', stat="identity") + coord_flip() +
    scale_fill_manual(values = colorhexes) + 
    theme_bw() +
    scale_y_continuous(labels = scales::percent, expand = c(.035, 0)) +
    geom_text(aes(label=ifelse(share >= 0.12, paste0(round(share*100,0),"%\n",comma(estimate, scale = .001, accuracy =1, suffix = 'K')),""), 
                  y = pos_id_share, x = time_bucket), color = 'white', fontface = "bold", size = 5) +
    labs(subtitle = 'Year structure was built') + ylab('Percent') + xlab('Walking distance to L Stop\n(minutes)') +
    theme(legend.title = element_blank(), 
          legend.position = 'bottom',
          axis.text = element_text(size = 12),
          legend.text = element_text(size = 11),
          text = element_text(color = "#0a0a0a" )))

(p_bar_hval <- ggplot(bgroup_hval) +
    geom_bar(aes(x = time_bucket, y = share, fill = (variable_label)), 
             color = 'white', stat="identity") + coord_flip() +
    scale_fill_manual(values = c("#009EFA",'#F77552',"#49DEA4","#ffc425",'#845EC2','#FF6F91','#00D2FC','#008F7A','#00C0A3')) + 
    theme_bw() +
    scale_y_continuous(labels = scales::percent, expand = c(.035, 0)) +
    geom_text(aes(label=ifelse(share >= 0.12, paste0(round(share*100,0),"%\n",comma(estimate, scale = .001, accuracy =1, suffix = 'K')),""), 
                  y = pos_id_share, x = time_bucket), color = 'white', fontface = "bold", size = 5) +
    labs(subtitle = 'Housing value') + ylab('Percent') + xlab('Walking distance to L Stop\n(minutes)') +
    theme(legend.title = element_blank(), 
          legend.position = 'bottom',
          axis.text = element_text(size = 12),
          legend.text = element_text(size = 11),
          text = element_text(color = "#0a0a0a" )))

(p_bar_afford <- ggplot(tract_afford) +
    geom_bar(aes(x = time_bucket, y = share, fill = (variable_label)), 
             color = 'white', stat="identity") + coord_flip() +
    scale_fill_manual(values = c("#009EFA",'#F77552',"#49DEA4","#ffc425",'#845EC2','#FF6F91','#00D2FC','#008F7A','#00C0A3')) + 
    theme_bw() +
    scale_y_continuous(labels = scales::percent, expand = c(.035, 0)) +
    geom_text(aes(label=ifelse(share >= 0.12, paste0(round(share*100,0),"%\n",comma(estimate, scale = .001, accuracy =1, suffix = 'K')),""), 
                  y = pos_id_share, x = time_bucket), color = 'white', fontface = "bold", size = 5) +
    labs(subtitle = 'Fraction of income spent on rent') + ylab('Percent') + xlab('Walking distance to L Stop\n(minutes)') +
    theme(legend.title = element_blank(), 
          legend.position = 'bottom',
          axis.text = element_text(size = 12),
          legend.text = element_text(size = 11),
          text = element_text(color = "#0a0a0a" )))

(p_bar_travel <- ggplot(tract_travel) +
    geom_bar(aes(x = time_bucket, y = share, fill = (variable_label)), 
             color = 'white', stat="identity") + coord_flip() +
    scale_fill_manual(values = c("#009EFA",'#F77552',"#49DEA4","#ffc425",'#845EC2','#FF6F91','#00D2FC','#008F7A','#00C0A3')) + 
    theme_bw() +
    scale_y_continuous(labels = scales::percent, expand = c(.035, 0)) +
    geom_text(aes(label=ifelse(share >= 0.12, paste0(round(share*100,0),"%\n",comma(estimate, scale = .001, accuracy =1, suffix = 'K')),""), 
                  y = pos_id_share, x = time_bucket), color = 'white', fontface = "bold", size = 5) +
    labs(subtitle = 'Travel time to work') + ylab('Percent') + xlab('Walking distance to L Stop\n(minutes)') +
    theme(legend.title = element_blank(), 
          legend.position = 'bottom',
          axis.text = element_text(size = 12),
          legend.text = element_text(size = 11),
          text = element_text(color = "#0a0a0a" )))


ggsave(plot = p_bar_race, filename = '/Users/nm/Desktop/viz/race.png', dpi = 300, height = 4, width = 9)
ggsave(plot = p_bar_income, filename = '/Users/nm/Desktop/viz/income.png', dpi = 300, height = 4, width = 9)
ggsave(plot = p_bar_units, filename = '/Users/nm/Desktop/viz/units.png', dpi = 300, height = 4, width = 9)
ggsave(plot = p_bar_tenure, filename = '/Users/nm/Desktop/viz/tenure.png', dpi = 300, height = 4, width = 9)
ggsave(plot = p_bar_transport, filename = '/Users/nm/Desktop/viz/transport.png', dpi = 300, height = 4, width = 9)
ggsave(plot = p_bar_travel, filename = '/Users/nm/Desktop/viz/travel.png', dpi = 300, height = 4, width = 9)
ggsave(plot = p_bar_yearbuilt, filename = '/Users/nm/Desktop/viz/yearbuilt.png', dpi = 300, height = 4, width = 9)
ggsave(plot = p_bar_hval, filename = '/Users/nm/Desktop/viz/hval.png', dpi = 300, height = 4, width = 9)
ggsave(plot = p_bar_afford, filename = '/Users/nm/Desktop/viz/housing_afford.png', dpi = 300, height = 4, width = 9)

#save.image(file='/Users/nm/Desktop/envhousing.RData')

# Plot maps ---------------------------------------------------------------

tract_vehicle_density <- tract_data_totals %>% 
  filter(variable %in% c('B08015_001')) %>%
  mutate(vehicle_density = summary_est/estimate) 
tract_vehicle_density <- tract2 %>% 
  left_join(., tract_vehicle_density, by = c('GEOID' = 'GEOID'))
(m_vehicle <- ggplot() +
  geom_sf(data = tract_vehicle_density, aes(color = vehicle_density, fill = vehicle_density)) +
  scale_fill_distiller(palette = 'Spectral', limits = c(0,5), labels = c('0',"1","2","3","4","5+"), oob = scales::squish) +
  scale_color_distiller(palette = 'Spectral', limits = c(0,5), labels = c('0',"1","2","3","4","5+"), oob = scales::squish) +
  geom_sf(data = isos %>% filter(time == 10), color = alpha("#0194D3", .5), alpha = 0, size = .3) + # aes(color = time), 
  labs(subtitle = "Vehicles per household\n") +
  theme_void() + theme(
  plot.caption = element_text(size = 10, hjust = .5, vjust = 5),
  legend.position = c(.5, 1),
  legend.direction = "horizontal",
  legend.key.width=unit(30,"pt"),
  legend.key.height=unit(5,"pt"),
  plot.margin=unit(c(t=15,r=0,b=0,l=0), "pt"),
  plot.subtitle = element_text(size = 11, face="bold", vjust = 5, hjust = .5),
  legend.title = element_blank(),
  text = element_text(color = "#333333"))) 


tract_affordability <- tract2 %>% 
  left_join(., tract_data_totals %>% 
              filter(variable %in% c('B25071_001')), by = c('GEOID' = 'GEOID'))
(m_affordability <- ggplot() +
  geom_sf(data = tract_affordability, aes(color = estimate/100, fill = estimate/100)) +
  scale_fill_distiller(palette = 'Spectral' ,labels = percent_format(1L)) +
  scale_color_distiller(palette = 'Spectral',labels = percent_format(1L)) +
  geom_sf(data = isos %>% filter(time == 10), color = alpha("#0194D3", .5), alpha = 0, size = .3) + # aes(color = time), 
  labs(subtitle = "Rent as a share of household income\n") +
  theme_void() + theme(
    plot.caption = element_text(size = 10, hjust = .5, vjust = 5),
    legend.position = c(.5, 1),
    legend.direction = "horizontal",
    legend.key.width=unit(30,"pt"),
    legend.key.height=unit(5,"pt"),
    plot.margin=unit(c(t=15,r=0,b=0,l=0), "pt"),
    plot.subtitle = element_text(size = 11, face="bold", vjust = 5, hjust = .5),
    legend.title = element_blank(),
    text = element_text(color = "#333333")) )

tract_income <- tract2 %>% 
  left_join(., tract_data_totals %>% 
              filter(variable %in% c('B19013_001')), by = c('GEOID' = 'GEOID'))
(m_income <- ggplot() +
  geom_sf(data = tract_income, aes(color = estimate/1000, fill = estimate/1000)) +
  scale_fill_distiller(palette = 'Spectral', labels = dollar_format(1L, suffix = 'K')) +
  scale_color_distiller(palette = 'Spectral', labels = dollar_format(1L, suffix = 'K')) +
  geom_sf(data = isos %>% filter(time == 10), color = alpha("#0194D3", .5), alpha = 0, size = .3) + # aes(color = time), 
  labs(subtitle = "Median household income\n") +
  theme_void() + theme(
    plot.caption = element_text(size = 10, hjust = .5, vjust = 5),
    legend.position = c(.5, 1),
    legend.direction = "horizontal",
    legend.key.width=unit(30,"pt"),
    legend.key.height=unit(5,"pt"),
    plot.margin=unit(c(t=15,r=0,b=0,l=0), "pt"),
    plot.subtitle = element_text(size = 11, face="bold", vjust = 5, hjust = .5),
    legend.title = element_blank(),
    text = element_text(color = "#333333")) )

tract_housingvalue <- tract2 %>% 
  left_join(., tract_data_totals %>% 
              filter(variable %in% c('B25077_001')), by = c('GEOID' = 'GEOID'))
(m_housingvalue <- ggplot() +
  geom_sf(data = tract_housingvalue, aes(color = estimate/1000, fill = estimate/1000)) +
  scale_fill_distiller(palette = 'Spectral', oob = scales::squish, limits = c(0,1000), 
                       labels = c('$0',"$250K","$500K","$750K","$1M+"), breaks = c(0,250,500,750,1000))  +
  scale_color_distiller(palette = 'Spectral', oob = scales::squish, limits = c(0,1000), 
                        labels = c('$0',"$250K","$500K","$750K","$1M+"), breaks = c(0,250,500,750,1000))  +
  geom_sf(data = isos %>% filter(time == 10), color = alpha("#0194D3", .5), alpha = 0, size = .3) + # aes(color = time), 
  labs(subtitle = 'Median housing value\n') +
  theme_void() + theme(
    plot.caption = element_text(size = 10, hjust = .5, vjust = 5),
    legend.position = c(.5, 1),
    legend.direction = "horizontal",
    legend.key.width=unit(30,"pt"),
    legend.key.height=unit(5,"pt"),
    plot.margin=unit(c(t=15,r=0,b=0,l=0), "pt"),
    plot.subtitle = element_text(size = 11, face="bold", vjust = 5, hjust = .5),
    legend.title = element_blank(),
    text = element_text(color = "#333333")) )

maps <- m_housingvalue + m_income + m_affordability + m_vehicle + plot_layout(nrow = 1)
ggsave(plot = maps , filename = '/Users/nm/Desktop/viz/maps.png', dpi = 400, height = 5, width = 12)



# Plot isos ---------------------------------------------------------------

(m_bg_isos <- ggplot() + 
  geom_sf(data = bgroup2, aes(color = time_bucket, fill = time_bucket)) +
  labs(subtitle = 'Block group') +
  theme_void() + 
  theme(plot.subtitle = element_text(face = 'bold', hjust= .5),
          legend.title = element_blank(),
        legend.position = 'none'))

(m_t_isos <- ggplot() + 
  geom_sf(data = tract2, aes(color = time_bucket, fill = time_bucket)) +
  labs(subtitle = 'Tract') +
  theme_void() +
  theme(plot.subtitle = element_text(face = 'bold', hjust = .5)#,
        #legend.title = element_blank()
        ))

m_isos <- m_bg_isos + m_t_isos

ggsave(plot = m_isos, filename = '/Users/nm/Desktop/viz/maps_isos.png', dpi = 300, height = 5, width = 6)



