# app.R - R Shiny Dashboard App for Beef Cattle GHG Emission Calculation in Indonesia (IPCC Tier 2) developed by Slamet Widodo

library(shiny)
library(shinydashboard) # For dashboard layout
library(rhandsontable) # For interactive input tables
library(DT) # For display-only DataTables (results)
library(plotly) # For interactive plots
library(dplyr) # For data manipulation (e.g., piping, mutate, summarize)
library(tidyr) # For data manipulation (e.g., replace_na, pivot_longer, pivot_wider)
library(purrr) # For purrr::reduce used in data merging

# --- 0. Global Constants (IPCC 2019 Refinements / AR5) ---
GWP_CH4 <- 28 # IPCC AR5 (2019 Refinements Annex 3.A.2)
GWP_N2O <- 265 # IPCC AR5 (2019 Refinements Annex 3.A.2)
ENERGY_CH4 <- 55.65 # MJ/kg CH4, energy content of methane (IPCC constant)
GE_CONTENT_FEED <- 18.45 # MJ/kg DM, approximate gross energy content of typical feed (IPCC constant)

# Fixed IPCC Parameters for Bo (Consistent with IPCC 2019 Refinements, Vol 4)
bo_values_beef <- data.frame(
  Animal_Category = c("Beef Cattle_Weaning", "Beef Cattle_Yearling", "Beef Cattle_Adult Male", "Beef Cattle_Adult Female", "Imported Cattle"),
  Bo = rep(0.13, 5) # All Bo values are 0.13
)

# --- UPDATED MCF_warm_climate with new 9 granular MMS categories ---
MCF_warm_climate <- data.frame(
  MMS = c("Lagoon", "Liquid/Slurry", "Solid_Storage", "Dry_Lot", "Pasture/Range/Paddock", "Daily_Spread", "Composting", "Burned_for_fuel", "Biogas"),
  MCF_general = c(80, 73, 5.0, 2.0, 0.47, 1.0, 2.5, 10, 0),
  MCF_imported_cattle = c(80, 73, 1.5, 2.0, 0.47, 1.0, 2.5, 10, 0)
)

# --- UPDATED ef3_n2o_manure with new 9 granular MMS categories ---
ef3_n2o_manure <- data.frame(
  MMS = c("Lagoon", "Liquid/Slurry", "Solid_Storage", "Dry_Lot", "Pasture/Range/Paddock", "Daily_Spread", "Composting", "Burned_for_fuel", "Biogas"),
  EF3 = c(0, 0, 0.01, 0.02, 0.006, 0, 0.01, 0, 0)
)

# --- NEW CONSTANTS for detailed GE_Intake calculation (from user's spreadsheet formulas) ---
Cfi_coeff_pregnant <- 0.386
Cfi_coeff_non_pregnant <- 0.322
Cfi_constant_weaning_yearling <- 0.322
Cfi_constant_adult_male_imported <- 0.3711 # High-precision adjustment

Ca_const <- 0.17
Cp_const <- 0.146 # High-precision adjustment to match spreadsheet for pregnancy
C_const <- 0.8
REG_const1 <- 22.02
NEl_coeff1 <- 1.47
NEl_coeff2 <- 0.4

# --- New constant for DMD to DE conversion (CSIRO 2007) ---
CSIRO_GE <- 18.4 # MJ/kg DM, specifically for the M/D to DE% conversion.
# Note: GE_CONTENT_FEED (18.45) is the IPCC standard.
# The formula uses GE_CONTENT_FEED for the final DE% conversion.

# --- NEW: Urinary Energy as a fraction of GE ---
UE_FRACTION_GE <- 0.04


# --- REORDERED: Define Beef Cattle Categories and Production Systems ---
production_systems_ordered <- c("Extensive", "Semi-Intensive", "Intensive")
sub_categories_ordered <- c("Beef Cattle_Weaning", "Beef Cattle_Yearling", "Beef Cattle_Adult Male", "Beef Cattle_Adult Female")
imported_category_intensive <- "Imported Cattle"

all_beef_combinations_final <- bind_rows(
  expand.grid(Production_System = "Extensive", Animal_Category = sub_categories_ordered, stringsAsFactors = FALSE),
  expand.grid(Production_System = "Semi-Intensive", Animal_Category = sub_categories_ordered, stringsAsFactors = FALSE),
  expand.grid(Production_System = "Intensive", Animal_Category = sub_categories_ordered, stringsAsFactors = FALSE),
  data.frame(Production_System = "Intensive", Animal_Category = imported_category_intensive, stringsAsFactors = FALSE)
)

all_beef_combinations_final$Production_System <- factor(all_beef_combinations_final$Production_System, levels = production_systems_ordered)
all_beef_combinations_final$Animal_Category <- factor(all_beef_combinations_final$Animal_Category, levels = c(sub_categories_ordered, imported_category_intensive))

all_beef_combinations_final <- all_beef_combinations_final %>%
  arrange(Production_System, Animal_Category)

master_animal_category_levels <- c(sub_categories_ordered, imported_category_intensive)

# --- Ensure bo_values_beef$Animal_Category is also a factor with consistent levels ---
bo_values_beef$Animal_Category <- factor(bo_values_beef$Animal_Category, levels = master_animal_category_levels)


# --- 2. REORDERED Initial Data (HARDCODED) ---

initial_population_data <- all_beef_combinations_final
initial_population_data$Population_Head <- c(
  773082, 1051651, 405686, 1763305,    # Extensive
  991480, 1348746, 520293, 2261444,  # Semi-Intensive
  1642586, 2234469, 754196, 3695729, 318606 # Intensive
)

initial_enteric_data <- all_beef_combinations_final
initial_enteric_data$LW <- c(95.72, 154.33, 237.23, 230.82, 120.49, 167.73, 241.23, 247.95, 120.65, 171.83, 248.41, 249.35, 331.54)
initial_enteric_data$Mature_Weight_kg <- c(154.33, 234.02, 223.69, 225.91, 148.89, 244.59, 230.74, 239.64, 171.83, 248.88, 372.15, 249.35, 477.00)
initial_enteric_data$ADG <- c(0.161, 0.280, 0.000, 0.000, 0.078, 0.211, 0.000, 0.000, 0.140, 0.211, 0.339, 0.000, 1.212)
initial_enteric_data$Milk_kg_day <- c(0, 0, 0, 0.28, 0, 0, 0, 0.46, 0, 0, 0, 0.47, 0)
# UPDATED: Hardcoded DMD_percent values based on reverse calculation from original DE%
# NOTE: These values are constant per production system category. To see variation in EF_enteric_CH4_Tier2
# these, or other GEI input parameters, would need to vary more specifically per Animal_Category.
initial_enteric_data$DMD_percent <- c(
  53.98, 53.98, 53.98, 53.98, # Extensive
  66.27, 66.27, 66.27, 66.27, # Semi-Intensive
  57.25, 57.25, 57.25, 57.25, # Intensive (Domestic)
  79.96 # Intensive (Imported Cattle)
)
initial_enteric_data$CP_percent_diet <- c(9.07, 9.07, 9.07, 9.07, 8.62, 8.62, 8.62, 8.62, 7.3, 7.3, 7.3, 7.3, 14)
initial_enteric_data$Ash_percent_diet <- c(9, 9, 9, 9, 16, 16, 16, 16, 13, 13, 13, 13, 10)
initial_enteric_data$Fat_Content_Milk_percent <- c(0, 0, 0, 5.45, 0, 0, 0, 4, 0, 0, 0, 4, 0)
initial_enteric_data$Work_Hours_day <- c(rep(0, 13))
# Prop_Cows_Pregnant_percent: Only applicable for Adult Female, 0 for others
initial_enteric_data$Prop_Cows_Pregnant_percent <- c(
  0, 0, 0, 60.76, # Weaning, Yearling, Adult Male, Adult Female (Extensive)
  0, 0, 0, 60.76, # Weaning, Yearling, Adult Male, Adult Female (Semi-Intensive)
  0, 0, 0, 60.76, # Weaning, Yearling, Adult Male, Adult Female (Intensive)
  0 # Imported Cattle (Intensive)
)
initial_enteric_data$Ym_input <- c(6.5, 6.5, 6.5, 6.5, 6.5, 6.5, 6.5, 6.5, 6.5, 6.5, 6.5, 6.5, 4.0)

# Define the 9 MMS columns (globally for use in UI and server)
mms_cols <- c("Lagoon", "Liquid/Slurry", "Solid_Storage", "Dry_Lot", "Pasture/Range/Paddock", "Daily_Spread", "Composting", "Burned_for_fuel", "Biogas")

initial_mms_data <- all_beef_combinations_final
# Initialize all MMS columns to 0 for all rows
for(col in mms_cols) { initial_mms_data[[col]] <- 0 }

# Hardcode the specific values based on your spreadsheet, ensuring all 9 columns are covered
# and rows sum to 100 for each animal/system combination.
# IMPORTANT: This block assumes your percentages are meant to be 0-100, not 0-1.
# This data MUST match the order of all_beef_combinations_final (Extensive, Semi-Intensive, Intensive, then categories within)
initial_mms_data$Lagoon <- c(0,0,0,0, 0,0,0,0, 0,0,30,0,0)
initial_mms_data$`Liquid/Slurry` <- c(0,0,0,0, 0,0,0,0, 0,0,0,0,10)
initial_mms_data$Solid_Storage <- c(0,0,0,0, 50,50,50,50, 65,65,55,65,80)
initial_mms_data$Dry_Lot <- c(0,0,0,0, 0,0,0,0, 0,0,0,0,0)
initial_mms_data$`Pasture/Range/Paddock` <- c(100,100,100,100, 50,50,50,50, 0,0,0,0,0)
initial_mms_data$Daily_Spread <- c(0,0,0,0, 0,0,0,0, 10,10,10,10,0)
initial_mms_data$Composting <- c(0,0,0,0, 0,0,0,0, 20,20,5,20,0)
initial_mms_data$Burned_for_fuel <- c(0,0,0,0, 0,0,0,0, 0,0,0,0,10)
initial_mms_data$Biogas <- c(0,0,0,0, 0,0,0,0, 5,5,0,5,0)


# --- Calculation Helper Functions (Defined globally for clarity) ---
# Function now expects DE_percent as the calculated value (Digestible Energy % of GE)
calculate_dmi <- function(GE_Intake_MJ_day, DE_percent_for_IPCC) {
  DE_prop <- DE_percent_for_IPCC / 100 # Use the calculated DE%
  if (GE_CONTENT_FEED == 0 || DE_prop == 0) return(0)
  return(GE_Intake_MJ_day / (GE_CONTENT_FEED * DE_prop))
}

# --- Define the UI ---
ui <- dashboardPage(
  dashboardHeader(title = "Beef Cattle GHG Emissions (Indonesia)"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction", tabName = "intro", icon = icon("info-circle")),
      menuItem("Input Data", icon = icon("edit"),
               menuSubItem("1. Population", tabName = "population_tab"),
               menuSubItem("2. Animal Parameters", tabName = "animal_params_tab"),
               menuSubItem("3. Manure Management", tabName = "manure_mgt_tab")
      ),
      menuItem("Results", tabName = "results_tab", icon = icon("chart-bar")),
      menuItem("Visualizations", tabName = "visualizations_tab", icon = icon("chart-pie"))
    )
  ),
  
  dashboardBody(
    tabItems(
      # Introduction Tab
      tabItem(tabName = "intro",
              h2("Beef Cattle GHG Emissions in Indonesia"),
              fluidRow(
                box(
                  title = "About This Model", status = "primary", solidHeader = TRUE, width = 12,
                  fluidRow(
                    column(width = 12, align = "center",
                           # Logos for BRIN and CSA
                           tags$img(src = "image_393121.png", height = "60px", style = "margin-right: 20px;"), # BRIN Logo
                           tags$img(src = "image_392d62.png", height = "60px;") # CSA Logo
                    )
                  ),
                  br(), # Add a break after logos
                  p("This Shiny Dashboard application estimates greenhouse gas (GHG) emissions from beef cattle in Indonesia using an IPCC Tier 2 methodology."),
                  p("Developed by the Research Center for Animal Husbandry-BRIN with support from the New Zealand Agricultural Greenhouse Gas Research Centre (NZAGRC) through the research project 'Improving National GHG Inventories For Livestock Using The IPCC Tier 2 Method - Phase 2.'"),
                  h4("Key Features:"),
                  tags$ul(
                    tags$li("Disaggregation by beef cattle sub-categories (Weaning, Yearling, Adult Male, Adult Female, and Imported Cattle)."),
                    tags$li("Differentiated production systems (Extensive, Semi-Intensive, Intensive)."),
                    tags$li("Calculation of Enteric Fermentation (CH4) and Manure Management (CH4, N2O) emissions."),
                    tags$li("Interactive data input tables for easy modification of parameters."),
                    tags$li("Dynamic calculation and display of emission factors and total emissions.")
                  ),
                  h4("Methodology Basis:"),
                  p("The calculations are based on the ",
                    tags$a(href="https://www.ipcc-nggip.iges.or.jp/public/2006gl/", "2019 Refinements to the 2006 IPCC Guidelines for National Greenhouse Gas Inventories, Volume 4: Agriculture, Forestry and Other Land Use.", target="_blank"),
                    " Tier 2 equations. All initial data for domestic and imported cattle is hardcoded within this script."),
                  p("The provided initial data is illustrative and based on typical Indonesian conditions and general IPCC guidance. Users should replace it with their specific, country-level data within the interactive tables."),
                  h4("Important Notes:"),
                  tags$ul(
                    tags$li("Global Warming Potentials (GWPs) used are IPCC AR5 values (CH4=28, N2O=265).")
                  )
                )
              )
      ),
      
      # 1. Population Input Tab
      tabItem(tabName = "population_tab",
              h2("1. Beef Cattle Population Input"),
              fluidRow(
                box(
                  title = "Enter Population (Head)", status = "info", solidHeader = TRUE, width = 12,
                  rHandsontableOutput("population_input_table"),
                  helpText("Enter the number of head for each beef cattle category and production system."),
                  helpText(HTML("<em>Note: This table automatically saves your changes.</em>"))
                )
              )
      ),
      
      # 2. Animal Parameters Tab
      tabItem(tabName = "animal_params_tab",
              h2("2. Animal Physiological and Feed Parameters"),
              fluidRow(
                box(
                  title = "Enter Parameters", status = "info", solidHeader = TRUE, width = 12,
                  rHandsontableOutput("enteric_params_input_table"),
                  # UPDATED helpText to reflect DMD input
                  helpText("LW (kg): Average Live Weight; Mature_Weight_kg (kg); ADG (kg/day): Average Daily Gain; Milk_kg_day (kg/d): Milk Production; DMD_percent (%): Dry Matter Digestibility of diet (used to calculate DE%); CP_percent_diet (%): Crude Protein of diet; Ash_percent_diet (%): Ash content of diet; Fat_Content_Milk_percent (%); Work_Hours_day (hours); Prop_Cows_Pregnant_percent (%); Ym: Methane Conversion Factor."),
                  helpText(HTML("<em>Note: This table automatically saves your changes.</em>"))
                )
              )
      ),
      
      # 3. Manure Management Tab
      tabItem(tabName = "manure_mgt_tab",
              h2("3. Manure Management System Distribution & Nitrogen Excretion"),
              fluidRow(
                box(
                  title = "Manure Management System Distribution (%)", status = "info", solidHeader = TRUE, width = 12,
                  rHandsontableOutput("mms_distribution_input_table"),
                  helpText("Enter the percentage (0-100) of manure managed in each system."), # Simplified help text
                ),
                box(
                  title = "Calculated Nitrogen Excretion (kg N/head/year)", status = "info", solidHeader = TRUE, width = 12,
                  dataTableOutput("calculated_nex_output"),
                  helpText("Nex is calculated based on Dry Matter Intake (DMI), Crude Protein (CP) of the diet, and a fixed N Retention Fraction (0.2)."),
                  helpText(HTML("<em>Note: This is a calculated output, not an input table.</em>"))
                )
              )
      ),
      
      # Results Tab
      tabItem(tabName = "results_tab",
              h2("Calculated Emission Factors and Total Emissions"),
              fluidRow(box(title = "Calculated Emission Factors (per head per year)", status = "success", solidHeader = TRUE, width = 12, dataTableOutput("emission_factors_output"))),
              fluidRow(box(title = "Overall Total Beef Cattle Emissions (Gg CO2e)", status = "success", solidHeader = TRUE, width = 12, dataTableOutput("overall_summary_output"))),
              fluidRow(box(title = "Detailed Emissions by Category and System (Gg CO2e)", status = "success", solidHeader = TRUE, width = 12, dataTableOutput("detailed_emissions_output"))),
              fluidRow(box(title = "Summary of Emissions by Production System (Gg CO2e)", status = "success", solidHeader = TRUE, width = 12, dataTableOutput("summary_by_system_output")))
      ),
      
      # Visualizations Tab
      tabItem(tabName = "visualizations_tab",
              h2("Emission Visualizations"),
              fluidRow(box(title = "Graphs", status = "primary", solidHeader = TRUE, width = 12, selectInput('visualization_type_dropdown', 'Select Visualization Type:', c('Total Emissions by Production System' = 'system_total', 'Overall Emission Breakdown by Gas Source' = 'gas_breakdown')), plotlyOutput('graph_content')))
      )
    )
  )
)


# --- Define the server logic ---
server <- function(input, output, session) {
  
  # --- Reactive Values for storing CURRENT state of the tables (after user edits) ---
  # These are initialized with the global initial_data
  rv_population_data <- reactiveVal(initial_population_data)
  rv_enteric_data <- reactiveVal(initial_enteric_data)
  rv_mms_data <- reactiveVal(initial_mms_data)
  
  # --- Observers to update reactiveVals when rhandsontable input changes ---
  # These capture the user edits and store them in the reactiveVals
  observeEvent(input$population_input_table, {
    print("DEBUG: observeEvent - population_input_table triggered.")
    df_current <- hot_to_r(input$population_input_table)
    # Only update if df_current is not NULL and has rows, and is different from stored data
    if (!is.null(df_current) && !is.data.frame(df_current) && !is.matrix(df_current)) { # hot_to_r can return weird stuff
      warning("DEBUG: hot_to_r for population_input_table returned non-dataframe/matrix. Skipping update.")
      return()
    }
    if (!is.null(df_current) && nrow(df_current) > 0 && !identical(df_current, rv_population_data())) {
      print("DEBUG: Updating rv_population_data.")
      rv_population_data(df_current)
    }
  }, ignoreNULL = FALSE) # important for initial load
  
  observeEvent(input$enteric_params_input_table, {
    print("DEBUG: observeEvent - enteric_params_input_table triggered.")
    df_current <- hot_to_r(input$enteric_params_input_table)
    if (!is.null(df_current) && !is.data.frame(df_current) && !is.matrix(df_current)) {
      warning("DEBUG: hot_to_r for enteric_params_input_table returned non-dataframe/matrix. Skipping update.")
      return()
    }
    if (!is.null(df_current) && nrow(df_current) > 0 && !identical(df_current, rv_enteric_data())) {
      print("DEBUG: Updating rv_enteric_data.")
      rv_enteric_data(df_current)
    }
  }, ignoreNULL = FALSE)
  
  observeEvent(input$mms_distribution_input_table, {
    print("DEBUG: observeEvent - mms_distribution_input_table triggered.")
    df_current <- hot_to_r(input$mms_distribution_input_table)
    if (!is.null(df_current) && !is.data.frame(df_current) && !is.matrix(df_current)) {
      warning("DEBUG: hot_to_r for mms_distribution_input_table returned non-dataframe/matrix. Skipping update.")
      return()
    }
    if (!is.null(df_current) && nrow(df_current) > 0 && !identical(df_current, rv_mms_data())) {
      print("DEBUG: Updating rv_mms_data.")
      rv_mms_data(df_current)
    }
  }, ignoreNULL = FALSE)
  
  # --- Render functions for input tables (read from reactiveVals for initial data & updates) ---
  # The crucial part here is to feed the initial data *if* input$table_id is NULL
  # Otherwise, use the data from input$table_id to reflect user edits immediately.
  
  output$population_input_table <- renderRHandsontable({
    print("DEBUG: Rendering population_input_table.")
    # Use initial data if input is NULL (first load), otherwise use input data
    df_to_render <- if (is.null(input$population_input_table)) {
      print("DEBUG: Using initial_population_data for render.")
      initial_population_data # Initial data from global constant
    } else {
      print("DEBUG: Using rv_population_data() for render.")
      # Ensure it's a data frame before passing
      as.data.frame(rv_population_data()) 
    }
    
    # Ensure factors are set for display consistency
    display_df <- df_to_render %>%
      mutate(
        Animal_Category = factor(Animal_Category, levels = master_animal_category_levels),
        Production_System = factor(Production_System, levels = production_systems_ordered)
      )
    
    rhandsontable(display_df, stretchH = "all", rowHeaders = FALSE) %>%
      hot_col(col = "Population_Head", format = "0,0", type = "numeric", readOnly = FALSE) %>%
      hot_col(col = c("Animal_Category", "Production_System"), readOnly = TRUE) %>%
      hot_validate_numeric(col = "Population_Head", min = 0, allowInvalid = TRUE)
  })
  
  output$enteric_params_input_table <- renderRHandsontable({
    print("DEBUG: Rendering enteric_params_input_table.")
    df_to_render <- if (is.null(input$enteric_params_input_table)) {
      print("DEBUG: Using initial_enteric_data for render.")
      initial_enteric_data
    } else {
      print("DEBUG: Using rv_enteric_data() for render.")
      as.data.frame(rv_enteric_data())
    }
    
    display_df <- df_to_render %>%
      mutate(
        Animal_Category = factor(Animal_Category, levels = master_animal_category_levels),
        Production_System = factor(Production_System, levels = production_systems_ordered)
      )
    
    read_only_matrix <- matrix(FALSE, nrow = nrow(display_df), ncol = ncol(display_df), dimnames = list(NULL, colnames(display_df)))
    read_only_matrix[, c("Animal_Category", "Production_System")] <- TRUE
    read_only_matrix[display_df$Animal_Category != "Beef Cattle_Adult Female", "Prop_Cows_Pregnant_percent"] <- TRUE
    
    rhandsontable(display_df, stretchH = "all", rowHeaders = FALSE, readOnly = read_only_matrix) %>%
      hot_col(col = "LW", format = "0.00", type = "numeric", readOnly = FALSE) %>% hot_validate_numeric(col = "LW", min = 0, allowInvalid = TRUE) %>%
      hot_col(col = "ADG", format = "0.00", type = "numeric", readOnly = FALSE) %>% hot_validate_numeric(col = "ADG", min = 0, allowInvalid = TRUE) %>%
      hot_col(col = "Milk_kg_day", format = "0.00", type = "numeric", readOnly = FALSE) %>% hot_validate_numeric(col = "Milk_kg_day", min = 0, allowInvalid = TRUE) %>%
      hot_col(col = "DMD_percent", format = "0.00", type = "numeric", readOnly = FALSE) %>% hot_validate_numeric(col = "DMD_percent", min = 0, max = 100, allowInvalid = TRUE) %>%
      hot_col(col = "CP_percent_diet", format = "0.00", type = "numeric", readOnly = FALSE) %>% hot_validate_numeric(col = "CP_percent_diet", min = 0, max = 100, allowInvalid = TRUE) %>%
      hot_col(col = "Ash_percent_diet", format = "0.00", type = "numeric", readOnly = FALSE) %>% hot_validate_numeric(col = "Ash_percent_diet", min = 0, max = 100, allowInvalid = TRUE) %>%
      hot_col(col = "Mature_Weight_kg", format = "0.00", type = "numeric", readOnly = FALSE) %>% hot_validate_numeric(col = "Mature_Weight_kg", min = 0, allowInvalid = TRUE) %>%
      hot_col(col = "Fat_Content_Milk_percent", format = "0.00", type = "numeric", readOnly = FALSE) %>% hot_validate_numeric(col = "Fat_Content_Milk_percent", min = 0, max = 100, allowInvalid = TRUE) %>%
      hot_col(col = "Work_Hours_day", format = "0.00", type = "numeric", readOnly = FALSE) %>% hot_validate_numeric(col = "Work_Hours_day", min = 0, allowInvalid = TRUE) %>%
      hot_col(col = "Prop_Cows_Pregnant_percent", format = "0.00", type = "numeric", readOnly = FALSE) %>% hot_validate_numeric(col = "Prop_Cows_Pregnant_percent", min = 0, max = 100, allowInvalid = TRUE) %>%
      hot_col(col = "Ym_input", format = "0.00", type = "numeric", readOnly = FALSE) %>% hot_validate_numeric(col = "Ym_input", min = 0, allowInvalid = TRUE)
  })
  
  output$mms_distribution_input_table <- renderRHandsontable({
    print("DEBUG: Rendering mms_distribution_input_table.")
    df_to_render <- if (is.null(input$mms_distribution_input_table)) {
      print("DEBUG: Using initial_mms_data for render.")
      initial_mms_data
    } else {
      print("DEBUG: Using rv_mms_data() for render.")
      as.data.frame(rv_mms_data())
    }
    
    display_df <- df_to_render %>%
      mutate(
        Animal_Category = factor(Animal_Category, levels = master_animal_category_levels),
        Production_System = factor(Production_System, levels = production_systems_ordered)
      )
    
    rhandsontable(display_df, stretchH = "all", rowHeaders = FALSE) %>%
      hot_col(col = c("Animal_Category", "Production_System"), readOnly = TRUE) %>%
      hot_col(col = mms_cols, format = "0", type = "numeric", readOnly = FALSE) %>%
      hot_validate_numeric(col = mms_cols, min = 0, max = 100, allowInvalid = TRUE)
  })
  
  # 2. Reactive for Cleaned and Merged Data
  # This reactive expression performs all data cleaning, type conversions,
  # joins, and initial validations.
  
  cleaned_and_merged_data <- reactive({
    print("DEBUG: cleaned_and_merged_data reactive re-running!")
    
    # Validate that raw inputs are available (using the reactiveVals)
    validate(
      need(!is.null(rv_population_data()), "Population data is not available for merging. Please check inputs."),
      need(!is.null(rv_enteric_data()), "Animal parameters are not available for merging. Please check inputs."),
      need(!is.null(rv_mms_data()), "Manure management data is not available for merging. Please check inputs.")
    )
    
    # Get the data from reactiveVals
    df_pop_raw <- rv_population_data()
    df_enteric_raw <- rv_enteric_data()
    df_mms_raw <- rv_mms_data()
    
    # --- Step 1: Initial Cleaning and Type Conversion for each raw input ---
    # Apply cleaning rules (NA to 0, numeric conversion, capping)
    
    # Population Data Cleaning
    df_pop_clean <- df_pop_raw %>%
      mutate(
        Population_Head = as.numeric(as.character(Population_Head)),
        Population_Head = replace_na(Population_Head, 0),
        Population_Head = pmax(0, Population_Head)
      )
    
    # Enteric Data Cleaning
    numeric_enteric_cols <- c("LW", "Mature_Weight_kg", "ADG", "Milk_kg_day", "DMD_percent",
                              "CP_percent_diet", "Ash_percent_diet", "Fat_Content_Milk_percent",
                              "Work_Hours_day", "Prop_Cows_Pregnant_percent", "Ym_input")
    
    df_enteric_clean <- df_enteric_raw
    for (col_name in numeric_enteric_cols) {
      if (col_name %in% names(df_enteric_clean)) {
        df_enteric_clean[[col_name]] <- as.numeric(as.character(df_enteric_clean[[col_name]]))
        df_enteric_clean[[col_name]][is.na(df_enteric_clean[[col_name]])] <- 0
        df_enteric_clean[[col_name]] <- pmax(0, df_enteric_clean[[col_name]])
        
        if (col_name %in% c("DMD_percent", "CP_percent_diet", "Ash_percent_diet",
                            "Fat_Content_Milk_percent", "Prop_Cows_Pregnant_percent")) {
          df_enteric_clean[[col_name]] <- pmin(100, df_enteric_clean[[col_name]])
        }
      } else {
        warning(paste0("DEBUG: Missing expected column '", col_name, "' in enteric_params_input data for cleaning. Adding as 0."))
        df_enteric_clean[[col_name]] <- 0 # Add missing column with default
      }
    }
    df_enteric_clean <- df_enteric_clean %>%
      mutate(Prop_Cows_Pregnant_percent = ifelse(Animal_Category != "Beef Cattle_Adult Female", 0, Prop_Cows_Pregnant_percent))
    
    # MMS Data Cleaning
    df_mms_clean <- df_mms_raw
    for (col_name in mms_cols) {
      if (col_name %in% names(df_mms_clean)) {
        df_mms_clean[[col_name]] <- as.numeric(as.character(df_mms_clean[[col_name]]))
        df_mms_clean[[col_name]][is.na(df_mms_clean[[col_name]])] <- 0
        df_mms_clean[[col_name]] <- pmax(0, pmin(100, df_mms_clean[[col_name]]))
      } else {
        warning(paste0("DEBUG: Missing expected column '", col_name, "' in mms_distribution_input data for cleaning. Adding as 0."))
        df_mms_clean[[col_name]] <- 0 # Add missing column with default
      }
    }
    
    # --- Step 2: Prepare for Joining (Convert join keys to character) ---
    df_pop_join <- df_pop_clean %>%
      mutate(
        Animal_Category = as.character(Animal_Category),
        Production_System = as.character(Production_System)
      )
    df_enteric_join <- df_enteric_clean %>%
      mutate(
        Animal_Category = as.character(Animal_Category),
        Production_System = as.character(Production_System)
      )
    df_mms_join <- df_mms_clean %>%
      mutate(
        Animal_Category = as.character(Animal_Category),
        Production_System = as.character(Production_System)
      )
    
    bo_values_beef_for_join <- bo_values_beef %>%
      mutate(Animal_Category = as.character(Animal_Category))
    
    # --- Step 3: Perform Joins ---
    df_merged <- list(df_pop_join, df_enteric_join, df_mms_join) %>%
      purrr::reduce(left_join, by = c("Animal_Category", "Production_System")) %>%
      left_join(bo_values_beef_for_join, by = "Animal_Category")
    
    # --- Step 4: Convert Join Keys Back to Factors for Consistency ---
    # This ensures correct ordering and categorization in results
    df_merged$Animal_Category <- factor(df_merged$Animal_Category, levels = master_animal_category_levels)
    df_merged$Production_System <- factor(df_merged$Production_System, levels = production_systems_ordered)
    
    # --- Step 5: Final data validation and preparation for calculations ---
    # Handle any Inf/NaN that might have slipped through from initial data or joins (should be minimal now)
    df_merged <- df_merged %>%
      mutate(across(where(is.numeric), ~replace(., is.infinite(.) | is.nan(.), 0)))
    
    # Validate row count after merge (to catch empty joins)
    validate(
      need(nrow(df_merged) > 0, "No valid data could be merged. Please ensure 'Animal Category' and 'Production System' match across all input tables.")
    )
    
    # Calculate Total_MMS_Percent
    df_merged$Total_MMS_Percent <- rowSums(df_merged[, mms_cols], na.rm = TRUE)
    
    # Validate MMS percentages sum to 100
    validate(
      need(all(near(df_merged$Total_MMS_Percent, 100, tol = 0.1)),
           "Warning: Manure Management System percentages for some categories do not sum to 100%. Please adjust the input table to ensure they sum to exactly 100% per row.")
    )
    
    print("DEBUG: Cleaned and merged data prepared successfully.")
    return(df_merged)
  })
  
  # 3. Reactive for Main Emission Calculations
  # This reactive expression now solely depends on the *cleaned* data,
  # making its logic simpler and less prone to input-related errors.
  
  calculated_emissions_data <- reactive({
    print("DEBUG: calculated_emissions_data reactive re-running calculations!")
    df <- cleaned_and_merged_data() # Depend on the cleaned and merged data
    
    req(df) # Ensure df is not NULL from previous reactive
    
    result_df <- tryCatch({
      # --- NEW: Calculate DE_percent for IPCC formulas from DMD_percent input ---
      df$DE_percent_for_IPCC <- ((0.172 * pmax(0, df$DMD_percent) - 1.707) / 0.81) / GE_CONTENT_FEED * 100
      df$DE_percent_for_IPCC <- pmax(0, pmin(100, df$DE_percent_for_IPCC)) # Cap between 0 and 100
      
      # --- Helper Functions for Tier 2 Calculations ---
      calculate_ge_intake_local <- function(LW, ADG, Milk_kg_day, Work_Hours_day, DE_percent_for_IPCC, Mature_Weight_kg, Fat_Content_Milk_percent, Prop_Cows_Pregnant_percent, Animal_Category) {
        DE_prop <- DE_percent_for_IPCC / 100
        Prop_Cows_Pregnant_prop <- Prop_Cows_Pregnant_percent / 100
        Fat_Content_Milk_prop <- Fat_Content_Milk_percent / 100
        
        Cfi <- 0
        if (Animal_Category == "Beef Cattle_Adult Female") { Cfi <- (Cfi_coeff_pregnant * Prop_Cows_Pregnant_prop) + (Cfi_coeff_non_pregnant * (1 - Prop_Cows_Pregnant_prop))
        } else if (Animal_Category %in% c("Beef Cattle_Weaning", "Beef Cattle_Yearling")) { Cfi <- Cfi_constant_weaning_yearling
        } else if (Animal_Category %in% c("Beef Cattle_Adult Male", "Imported Cattle")) { Cfi <- Cfi_constant_adult_male_imported
        } else { Cfi <- Cfi_coeff_non_pregnant }
        
        NEm <- Cfi * (LW^0.75)
        NEa <- NEm * Ca_const
        NEl <- Milk_kg_day * (NEl_coeff1 + NEl_coeff2 * Fat_Content_Milk_prop)
        NEwork <- 0.1 * NEm * Work_Hours_day
        NEp <- Cp_const * NEm * Prop_Cows_Pregnant_prop
        
        NEg <- 0
        if (ADG > 0 && Mature_Weight_kg > 0 && (C_const * Mature_Weight_kg) > 0) {
          Term_NEg_base <- (LW / (C_const * Mature_Weight_kg))
          if (Term_NEg_base >= 0 && ADG > 0) { NEg <- REG_const1 * (Term_NEg_base^0.75 * (ADG^1.097)) }
        }
        
        REM <- 0
        if (DE_percent_for_IPCC > 0) { REM <- (1.123 - (4.092 * (10^-3) * DE_percent_for_IPCC) + (1.126 * (10^-5) * (DE_percent_for_IPCC^2)) - (25.4 / DE_percent_for_IPCC)) }
        
        REG <- 0
        if (DE_percent_for_IPCC > 0) { REG <- (1.164 - (5.16 * (10^-3) * DE_percent_for_IPCC) + (1.308 * (10^-5) * (DE_percent_for_IPCC^2)) - (37.4 / DE_percent_for_IPCC)) }
        
        Term1 <- 0
        Sum_NE_Maintenance <- NEm + NEa + NEl + NEwork + NEp
        if (REM > 0) { Term1 <- Sum_NE_Maintenance / REM }
        
        Term2 <- 0
        if (NEg > 0 && REG > 0) { Term2 <- NEg / REG }
        
        GEI <- 0
        if (DE_prop > 0) { GEI <- (Term1 + Term2) / DE_prop }
        
        return(pmax(0, GEI))
      }
      
      calculate_nex_local <- function(GE_Intake_MJ_day, CP_percent_diet) {
        if (GE_CONTENT_FEED == 0) return(0)
        CP_percent_diet <- ifelse(is.na(CP_percent_diet), 0, CP_percent_diet)
        N_intake_kg_day <- (GE_Intake_MJ_day / GE_CONTENT_FEED) * ((CP_percent_diet / 100) / 6.25)
        N_retention_fraction <- 0.2
        Nex_kg_day <- N_intake_kg_day * (1 - N_retention_fraction)
        return(pmax(0, Nex_kg_day * 365))
      }
      
      # --- Apply Calculations to the Merged Data Frame ---
      df$GE_Intake_MJ_day <- mapply(calculate_ge_intake_local, 
                                    df$LW, df$ADG, df$Milk_kg_day, df$Work_Hours_day, df$DE_percent_for_IPCC,
                                    df$Mature_Weight_kg, df$Fat_Content_Milk_percent, df$Prop_Cows_Pregnant_percent,
                                    df$Animal_Category, SIMPLIFY = TRUE)
      
      df$Ym_calculated <- df$Ym_input
      
      # Corrected calculation for EF_enteric_CH4_Tier2
      df$EF_enteric_CH4_Tier2 <- (df$GE_Intake_MJ_day * (df$Ym_calculated / 100) * 365) / ENERGY_CH4
      
      df$DMI_kg_day <- mapply(calculate_dmi, df$GE_Intake_MJ_day, df$DE_percent_for_IPCC, SIMPLIFY = TRUE)
      
      df$Nex_calculated <- mapply(calculate_nex_local, df$GE_Intake_MJ_day, df$CP_percent_diet, SIMPLIFY = TRUE)
      
      df$VS_kg_DM_year <- (df$GE_Intake_MJ_day * (1 - (df$DE_percent_for_IPCC / 100)) + (UE_FRACTION_GE * df$GE_Intake_MJ_day)) *
        ifelse(GE_CONTENT_FEED == 0, 0, ((1 - (df$Ash_percent_diet / 100)) / GE_CONTENT_FEED)) * 365
      
      Conversion_m3_to_kg_CH4 <- 0.67
      df$EF_manure_CH4_Tier2 <- 0 
      df$EF_manure_N2O_Tier2 <- 0 
      
      for (i in 1:nrow(df)) {
        vs_val <- df$VS_kg_DM_year[i]
        bo_val <- df$Bo[i]
        nex_val <- df$Nex_calculated[i]
        animal_cat_str <- as.character(df$Animal_Category[i]) 
        
        weighted_mcf_sum = 0
        weighted_ef3_sum = 0
        
        for (mms_type in mms_cols) {
          fraction <- df[[mms_type]][i] / 100
          
          mcf_val = 0
          if (mms_type == "Solid_Storage" && animal_cat_str == "Imported Cattle") { 
            mcf_val <- MCF_warm_climate$MCF_imported_cattle[as.character(MCF_warm_climate$MMS) == mms_type]
          } else { 
            mcf_val <- MCF_warm_climate$MCF_general[as.character(MCF_warm_climate$MMS) == mms_type] 
          }
          
          ef3_val <- ef3_n2o_manure$EF3[as.character(ef3_n2o_manure$MMS) == mms_type]
          
          weighted_mcf_sum <- weighted_mcf_sum + (fraction * mcf_val)
          weighted_ef3_sum <- weighted_ef3_sum + (fraction * ef3_val)
        }
        df$EF_manure_CH4_Tier2[i] <- vs_val * bo_val * (weighted_mcf_sum / 100) * Conversion_m3_to_kg_CH4
        df$EF_manure_N2O_Tier2[i] <- nex_val * weighted_ef3_sum * (44/28)
      }
      
      # Convert to Gg CO2e
      df$Enteric_CH4_CO2e_Gg <- (df$Population_Head * df$EF_enteric_CH4_Tier2 * GWP_CH4) / 1e6
      df$Manure_CH4_CO2e_Gg <- (df$Population_Head * df$EF_manure_CH4_Tier2 * GWP_CH4) / 1e6
      df$Manure_N2O_CO2e_Gg <- (df$Population_Head * df$EF_manure_N2O_Tier2 * GWP_N2O) / 1e6
      df$Total_CO2e_Gg <- df$Enteric_CH4_CO2e_Gg + df$Manure_CH4_CO2e_Gg + df$Manure_N2O_CO2e_Gg
      
      print("DEBUG: Checking for problematic values after final calculations...")
      problematic_cols <- names(df)[sapply(df, function(x) any(is.na(x) | is.infinite(x) | is.nan(x)))]
      if(length(problematic_cols) > 0) {
        warning("DEBUG: NA/NaN/Inf values found in columns after calculation: ", paste(problematic_cols, collapse = ", "))
      } else {
        print("DEBUG: No NA/NaN/Inf values found in calculated columns.")
      }
      
      # --- DEBUGGING OUTPUT: Check GE_Intake_MJ_day, Ym_calculated, and EF_enteric_CH4_Tier2 ---
      # This will print to your R console or Shiny log.
      print("--- DEBUGGING GEI, Ym, and EF Enteric CH4 (after final calc) ---")
      print(df[, c("Production_System", "Animal_Category", "LW", "ADG", "Milk_kg_day",
                   "DMD_percent", "DE_percent_for_IPCC", "CP_percent_diet",
                   "Mature_Weight_kg", "Fat_Content_Milk_percent", "Prop_Cows_Pregnant_percent",
                   "GE_Intake_MJ_day", "Ym_calculated", "EF_enteric_CH4_Tier2")])
      print("------------------------------------------")
      # --- END DEBUGGING OUTPUT ---
      
      return(df)
    }, error = function(e) {
      warning("CALCULATION ERROR in calculated_emissions_data (calculations step): ", e$message) 
      NULL 
    })
    
    validate(need(!is.null(result_df) && nrow(result_df) > 0, "A final calculation error occurred or no valid data was generated. Please check all input fields."))
    
    return(result_df)
  })
  
  # --- Render Output Tables ---
  output$calculated_nex_output <- renderDataTable({
    df <- calculated_emissions_data()
    # Robustly handle NaN/Inf values for display. This is a final safety net.
    df <- df %>% mutate(across(where(is.numeric), ~replace(., is.infinite(.) | is.nan(.), NA)))
    
    df_nex <- df[, c("Production_System", "Animal_Category", "DMI_kg_day", "CP_percent_diet", "Nex_calculated")]
    datatable(df_nex, options = list(pageLength = 13, dom = 't'), rownames = FALSE,
              colnames = c("System", "Category", "DMI (kg/d)", "CP%", "Nex (kg N/yr)")) %>%
      formatRound(c("DMI_kg_day", "CP_percent_diet", "Nex_calculated"), 2)
  })
  
  output$emission_factors_output <- renderDataTable({
    df <- calculated_emissions_data()
    df <- df %>% mutate(across(where(is.numeric), ~replace(., is.infinite(.) | is.nan(.), NA)))
    
    df_ef <- df[, c("Production_System", "Animal_Category", "EF_enteric_CH4_Tier2", "EF_manure_CH4_Tier2", "EF_manure_N2O_Tier2")]
    datatable(df_ef, options = list(pageLength = 13, dom = 't'), rownames = FALSE, 
              colnames = c("System", "Category", "Enteric CH4 (kg/head/yr)", "Manure CH4 (kg/head/yr)", "Manure N2O (kg/head/yr)")) %>%
      formatRound(c("EF_enteric_CH4_Tier2", "EF_manure_CH4_Tier2", "EF_manure_N2O_Tier2"), 2)
  })
  
  output$detailed_emissions_output <- renderDataTable({
    df <- calculated_emissions_data()
    df <- df %>% mutate(across(where(is.numeric), ~replace(., is.infinite(.) | is.nan(.), NA)))
    
    display_mms_cols_detailed <- mms_cols
    
    df_detailed <- df[, c("Production_System", "Animal_Category", "Population_Head",
                          "LW", "ADG", "Milk_kg_day", "DMD_percent", "DE_percent_for_IPCC", "CP_percent_diet", "Ash_percent_diet",
                          "Mature_Weight_kg", "Fat_Content_Milk_percent", "Work_Hours_day", "Prop_Cows_Pregnant_percent",
                          display_mms_cols_detailed, "Total_MMS_Percent",
                          "GE_Intake_MJ_day", "DMI_kg_day", "Nex_calculated", "VS_kg_DM_year",
                          "Enteric_CH4_CO2e_Gg", "Manure_CH4_CO2e_Gg", "Manure_N2O_CO2e_Gg", "Total_CO2e_Gg")]
    
    colnames_detailed_output <- c("System", "Category", "Population",
                                  "LW (kg)", "ADG (kg/d)", "Milk (kg/d)", "DMD (%)", "DE (Calculated %)", "CP (%)", "Ash (%)",
                                  "Mature Weight (kg)", "Fat Content Milk (%)", "Work Hours (day)", "Pregnant Cows (%)",
                                  "MMS1 (Lagoon)", "MMS2 (Liquid Slurry)", "MMS3 (Solid Storage)", "MMS4 (Dry Lot)", "MMS5 (Pasture)", "MMS6 (Daily Spread)", "MMS7 (Composting)", "MMS8 (Burned Fuel)", "MMS9 (Biogas)",
                                  "Total (MMS %)",
                                  "GE (MJ/day)", "DMI (kg/day)", "Nex (kg N/yr)", "VS (kg DM/yr)",
                                  "Enteric CH4 (Gg CO2e)", "Manure CH4 (Gg CO2e)", "Manure N2O (Gg CO2e)", "Total (Gg CO2e)")
    
    datatable(df_detailed, options = list(pageLength = 13, scrollX = TRUE), rownames = FALSE,
              colnames = colnames_detailed_output) %>%
      formatCurrency("Population_Head", currency="", digits=0) %>%
      formatRound(c("LW", "ADG", "Milk_kg_day", "DMD_percent", "DE_percent_for_IPCC", "CP_percent_diet", "Ash_percent_diet",
                    "Mature_Weight_kg", "Fat_Content_Milk_percent", "Work_Hours_day", "Prop_Cows_Pregnant_percent",
                    mms_cols, "Total_MMS_Percent",
                    "GE_Intake_MJ_day", "DMI_kg_day", "Nex_calculated", "VS_kg_DM_year",
                    "Enteric_CH4_CO2e_Gg", "Manure_CH4_CO2e_Gg", "Manure_N2O_CO2e_Gg", "Total_CO2e_Gg"), digits = 2)
  })
  
  output$summary_by_system_output <- renderDataTable({
    df <- calculated_emissions_data()
    df <- df %>% mutate(across(where(is.numeric), ~replace(., is.infinite(.) | is.nan(.), NA)))
    
    summary_data <- suppressWarnings(
      df %>%
        group_by(Production_System) %>%
        summarise(
          Enteric_CH4 = sum(Enteric_CH4_CO2e_Gg, na.rm = TRUE),
          Manure_CH4 = sum(Manure_CH4_CO2e_Gg, na.rm = TRUE),
          Manure_N2O = sum(Manure_N2O_CO2e_Gg, na.rm = TRUE),
          Total_CO2e = sum(Total_CO2e_Gg, na.rm = TRUE)
        ) %>%
        ungroup()
    )
    datatable(summary_data, options = list(dom = 't'), rownames = FALSE,
              colnames = c("Production System", "Enteric CH4 (Gg CO2e)", "Manure CH4 (Gg CO2e)", "Manure N2O (Gg CO2e)", "Total (Gg CO2e)")) %>%
      formatRound(c("Enteric_CH4", "Manure_CH4", "Manure_N2O", "Total_CO2e"), 2)
  })
  
  output$overall_summary_output <- renderDataTable({
    df <- calculated_emissions_data()
    validate(need(!is.null(df), "Calculation data not available."))
    df <- df %>% mutate(across(where(is.numeric), ~replace(., is.infinite(.) | is.nan(.), NA)))
    
    val <- format(round(sum(df$Total_CO2e_Gg, na.rm = TRUE), 2), big.mark = ".", decimal.mark = ",")
    df_overall <- data.frame(Metric = "Overall Total Beef Cattle Emissions (Gg CO2e)", Value = val)
    datatable(df_overall, options = list(dom = 't', columnDefs = list(list(className = 'dt-center', targets = 1))), rownames = FALSE, colnames = c("", "")) 
  })
  
  output$graph_content <- renderPlotly({
    df <- calculated_emissions_data()
    if (is.null(df)) return(NULL)
    df <- df %>% mutate(across(where(is.numeric), ~replace(., is.infinite(.) | is.nan(.), NA)))
    
    if (input$visualization_type_dropdown == 'system_total') {
      summary <- suppressWarnings( 
        df %>% group_by(Production_System) %>% summarise(Total = sum(Total_CO2e_Gg, na.rm = TRUE))
      )
      plot_ly(summary, x = ~Production_System, y = ~Total, type = 'bar') %>%
        layout(title = 'Emissions by Production System (Gg CO2e)', yaxis = list(title = 'Total CO2e (Gg)'))
    } else {
      totals <- suppressWarnings( 
        df %>% summarise(across(c(Enteric_CH4_CO2e_Gg, Manure_CH4_CO2e_Gg, Manure_N2O_CO2e_Gg), sum, na.rm = TRUE))
      )
      breakdown <- data.frame(Source = c('Enteric CH4', 'Manure CH4', 'Manure N2O'), Emissions = as.numeric(totals[1,]))
      plot_ly(breakdown, labels = ~Source, values = ~Emissions, type = 'pie', textinfo = 'label+percent') %>%
        layout(title = 'Emission Breakdown by Gas Source')
    }
  })
}

# --- Run the application ---
shinyApp(ui = ui, server = server)