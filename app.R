# College Education Analysis Dashboard
# Licensed under CC BY-NC 4.0
# Authors: Jon Cote, John Dye, Camden Egan, Jack Markert, Shae Ramberg

library(shiny)
library(readr)
library(dplyr)
library(leaflet)
library(DT)
library(tidyverse)
library(gridExtra)
library(bslib)      
library(thematic)   

# Load Data
raw_data <- read_csv("./data/college_scorecard_11-21.csv")

# Pre-calculation for Best Value Index (using raw names before rename)
raw_data <- raw_data |>
  mutate(
    # Create NET_COST based on control (Public vs Private)
    # CONTROL: 1=Public, 2=Private nonprofit
    NET_COST_CALC = case_when(
      CONTROL == 1 ~ NPT4_PUB,
      CONTROL %in% c(2) ~ NPT4_PRIV,
      TRUE ~ NA_real_
    ),
    # Create Best Value Index
    Best_Value_Index = round(MD_EARN_WNE_P10 / (NET_COST_CALC + GRAD_DEBT_MDN), 3)
  )

# Define the Master Mapping 
rename_mapping <- c(
  "Institution"                    = "INSTNM",
  "Admission Rate"                 = "ADM_RATE",
  "Graduation Rate"                = "C150_4",
  "Median Earnings After 10 Years" = "MD_EARN_WNE_P10",
  "Tuition (In-state)"             = "TUITIONFEE_IN",
  "Tuition (Out-of-state)"         = "TUITIONFEE_OUT",
  "Median Debt at Graduation"      = "GRAD_DEBT_MDN",
  "SAT Average"                    = "SAT_AVG",
  "Average Cost (In-state)"        = "NPT4_PUB",
  "Average Cost (Out-of-state)"    = "NPT4_PRIV",
  "Faculty Salary"                 = "AVGFACSAL",
  "Entry Age"                      = "AGE_ENTRY",
  
  # Demographics
  "% Asian"                        = "UGDS_ASIAN",
  "% Black"                        = "UGDS_BLACK",
  "% Hispanic"                     = "UGDS_HISP",
  "% White"                        = "UGDS_WHITE",
  "% Female"                       = "FEMALE",
  "% First Gen"                    = "FIRST_GEN",
  
  # Categorical
  "Control"                        = "Control of institution",
  "Urbanization"                   = "Degree of urbanization (Urban-centric locale)",
  "Religion"                       = "Religious affiliation",
  "HBCU"                           = "Historically Black College or University",
  "Region"                         = "Bureau of Economic Analysis (BEA) regions",
  "State"                          = "State abbreviation", 
  "City"                           = "CITY",
  "Latitude"                       = "LATITUDE",
  "Longitude"                      = "LONGITUDE",
  "URL"                            = "INSTURL",
  "Best Value Index"               = "Best_Value_Index"
)

# Apply changes 
full_data <- raw_data |> 
  select(-any_of("HBCU")) |> 
  rename(any_of(rename_mapping)) |> 
  mutate(
    # Fix percentages 
    `Admission Rate`   = `Admission Rate` * 100,
    `% Asian`          = `% Asian` * 100,
    `% Black`          = `% Black` * 100,
    `% Hispanic`       = `% Hispanic` * 100,
    `% White`          = `% White` * 100,
    `% Female`         = `% Female` * 100,
    `% First Gen`      = `% First Gen` * 100,
    `Graduation Rate`  = `Graduation Rate` * 100,
    
    # Ensure categorical consistency
    HBCU               = ifelse(HBCU == 1, "Yes", "No"),
    # Check if Hispanic Serving exists
    `Hispanic Serving` = if("Hispanic Serving" %in% names(raw_data)) ifelse(`Hispanic Serving` == 1, "Yes", "No") else "No"
  )

# Helper vectors for UI Choices
#original numeric_vars (Removed "Urbanization") for Researchers Tab
numeric_vars <- c("Admission Rate", "Graduation Rate", "Median Earnings After 10 Years", 
                  "Tuition (In-state)", "Tuition (Out-of-state)", "Median Debt at Graduation", 
                  "SAT Average", "Faculty Salary", "Entry Age", "Best Value Index",
                  "% First Gen", "% Female", 
                  "% Asian", "% Black", "% Hispanic", "% White")

# list specifically for the Students Tab that includes Urbanization
student_vars <- c(numeric_vars, "Urbanization")

state_choices    <- sort(unique(full_data$State))
region_choices   <- sort(unique(full_data$Region))
religion_choices <- sort(unique(full_data$Religion))
urban_choices    <- sort(unique(full_data$Urbanization))
school_choices   <- sort(unique(full_data$Institution))


# -------------------------------------------------------------------------
# UI
# -------------------------------------------------------------------------

# Define the Theme
my_theme <- bs_theme(
  version = 5,                
  bootswatch = "flatly",      
  primary = "#2C3E50",        
  secondary = "#18BC9C",      
  base_font = font_google("Roboto"),
  heading_font = font_google("Montserrat"),
  "card-cap-bg" = "#2C3E50"   
)

ui <- navbarPage(
  title = "College Education Analysis",
  theme = my_theme, 
  
  # --- Main Tab 1: Intro ---
  tabPanel(
    "Intro",
    fluidPage(
      tags$head(
        tags$style(HTML("
          .well {
            background-color: #ffffff;
            border: 1px solid #e3e3e3;
            border-radius: 8px;
            box-shadow: 0 4px 6px rgba(0,0,0,0.1);
            padding: 20px;
          }
          h2 { font-weight: 700; color: #2C3E50; }
          .navbar { box-shadow: 0 2px 4px rgba(0,0,0,0.1); }
        ")),
        tags$script(HTML("
        function switchToTab(tabName) {
          var tabs = $('a[data-toggle=\"tab\"]');
          tabs.each(function() {
            var $this = $(this);
            if ($this.text().trim() === tabName) {
              $this.tab('show');
            }
          });
        }
      "))
      ),
      
      br(),
      fluidRow(
        column(
          width = 8,
          h2("College Education Analysis Dashboard")
        )
      ),
      
      # Short subtitle in a well panel
      wellPanel(
        p("Explore how college cost, debt, and post-graduation earnings interact with key institutional characteristics to help students, families, and researchers make data-informed decisions.")
      ),
      
      # Main description
      p("This app helps students, families, and institutional researchers evaluate the value of four-year colleges by combining cost, debt, and post-graduation earnings with key institutional characteristics."),
      p("The dashboard uses a cleaned subset of the U.S. Department of Education's College Scorecard and related IPEDS data, limited to currently operating four-year public and private nonprofit institutions."),
      p("SAT scores are only reported for about half of the institutions in this dataset. If you turn on SAT filtering, many schools will be removed from the results, so consider using admission rate filters instead if you want to keep more colleges in view."),
      
      br(),
      fluidRow(
        column(
          width = 4,
          wellPanel(
            h4(
              tags$a(
                href = "#",
                onclick = "switchToTab('Students and Parents'); return false;",
                "Students & Families",
                style = "color: #18BC9C; font-weight: bold;"
              )
            ),
            p(
              "Quickly narrow down a list of potential schools using filters for cost, location, and student demographics, then spot-check a few favorites with highlighted tables and plots."
            )
          )
        ),
        column(
          width = 4,
          wellPanel(
            h4(
              tags$a(
                href = "#",
                onclick = "switchToTab('Researchers and Administrators'); return false;",
                "Researchers & Admins",
                style = "color: #18BC9C; font-weight: bold;"
              )
            ),
            p(
              "Interactively examine distributions, relationships, and simple regression models to understand how institutional characteristics relate to student outcomes such as graduation rates."
            )
          )
        ),
        column(
          width = 4,
          wellPanel(
            h4(
              tags$a(
                href = "#",
                onclick = "switchToTab('Data Table'); return false;",
                "Data Table",
                style = "color: #18BC9C; font-weight: bold;"
              )
            ),
            p(
              "View the full cleaned dataset in a sortable table, filter columns, and export data for use in your own analyses or reports."
            )
          )
        )
      ),
      
      hr(),
      h3("How to Use This App"),
      tags$ul(
        tags$li(
          tags$strong("Students and Parents tab: "),
          "Filter schools and explore variables of interest for prospective students and families."
        ),
        tags$li(
          tags$strong("Best Value Index sub-tab: "),
          "Focus on the combined cost-benefit measure for your filtered set of institutions."
        ),
        tags$li(
          tags$strong("Map sub-tab: "),
          "See where filtered colleges are located and inspect key metrics by hovering over points."
        ),
        tags$li(
          tags$strong("Researchers and Administrators tab: "),
          "Access single-variable, multi-variable, and model testing tools for more advanced analysis."
        ),
        tags$li(
          tags$strong("Data Table tab: "),
          "Browse, sort, and export the underlying dataset powering the visualizations."
        )
      )
    )
  ),
  
  # --- Main Tab 2: Students and Parents ---
  tabPanel("Students and Parents",
           sidebarLayout(
             sidebarPanel(
               h3("Variable(s) of Interest"),
               # UPDATED: Uses 'student_vars' which includes Urbanization
               selectInput("selected_vars", "Select Variable(s):",
                           choices=student_vars, 
                           multiple = TRUE, selected="Admission Rate"),
               
               
               h3("School Filtering"),
               #Filtering by demographics: race, gender, firstgen
               h4("Demographics"),
               checkboxInput("showRaceWhite", "Filter by % White"),
               conditionalPanel(
                 "input.showRaceWhite == true",
                 sliderInput("filterRaceWhite", "Percent White", 
                             min = 0, max = 100, value = c(0, 100), post = "%")
               ),
               checkboxInput("showRaceAsian", "Filter by % Asian"),
               conditionalPanel(
                 "input.showRaceAsian == true",
                 sliderInput("filterRaceAsian", "Percent Asian", 
                             min = 0, max = 100, value = c(0, 100), post = "%")
               ),
               checkboxInput("showRaceBlack", "Filter by % Black"),
               conditionalPanel(
                 "input.showRaceBlack == true",
                 sliderInput("filterRaceBlack", "Percent Black", 
                             min = 0, max = 100, value = c(0, 100), post = "%")
               ),
               checkboxInput("showRaceHispanic", "Filter by % Hispanic"),
               conditionalPanel(
                 "input.showRaceHispanic == true",
                 sliderInput("filterRaceHispanic", "Percent Hispanic", 
                             min = 0, max = 100, value = c(0, 100), post = "%")
               ),
               checkboxInput("showGender", "Filter by % Female"),
               conditionalPanel(
                 "input.showGender == true",
                 sliderInput("filterGender", "Percent Female", 
                             min = 0, max = 100, value = c(0, 100), post = "%")
               ),
               checkboxInput("showFirstGen", "Filter by % First Gen"),
               conditionalPanel(
                 "input.showFirstGen == true",
                 sliderInput("filterFirstGen", "Percent First Gen", 
                             min = 0, max = 100, value = c(0, 100), post = "%")
               ),
               h4("Academics"),
               checkboxInput("showSAT", "Filter by SAT Score"),
               conditionalPanel(
                 "input.showSAT == true",
                 sliderInput("filterSAT", "SAT Average Range", 
                             min = 400, max = 1600, value = c(400, 1600))
               ),
               checkboxInput("showAdmRate", "Filter by Admission Rate"),
               conditionalPanel(
                 "input.showAdmRate == true",
                 sliderInput("filterAdmRate", "Admission Rate", 
                             min = 0, max = 100, value = c(0, 100), post = "%")
               ),
               h4("Location"),
               # State
               checkboxInput("showState", "Filter by State"),
               conditionalPanel(
                 "input.showState == true",
                 selectizeInput("filterState", "State(s)", 
                                choices = state_choices, multiple = TRUE)
               ),
               # Region
               checkboxInput("showRegion", "Filter by Region"),
               conditionalPanel(
                 "input.showRegion == true",
                 selectizeInput("filterRegion", "Region(s)", 
                                choices = region_choices, multiple = TRUE)
               ),
               h4("Characteristics"),
               #Public/Private
               checkboxInput("showPublicPrivate", "Filter by School Type"),
               conditionalPanel(
                 "input.showPublicPrivate == true",
                 radioButtons("filterPublicPrivate", "School Type", 
                              choices = c("Public", "Private not-for-profit"))
               ),
               # HBCU
               checkboxInput("showHBCU", "Filter by HBCU Status"),
               conditionalPanel(
                 "input.showHBCU == true",
                 radioButtons("filterHBCU", "HBCU Status", 
                              choices = c("Yes", "No"))
               ),
               # Religion
               checkboxInput("showReligion", "Filter by Religious Affiliation"),
               conditionalPanel(
                 "input.showReligion == true",
                 selectizeInput("filterReligion", "Religious Affiliation(s)", 
                                choices = religion_choices, multiple = TRUE)
               ),
               # Degree of Urbanization
               checkboxInput("showUrban", "Filter by Urbanization"),
               conditionalPanel(
                 "input.showUrban == true",
                 selectizeInput("filterUrban", "Degree of Urbanization", 
                                choices = urban_choices, multiple = TRUE)
               ),
               #Highlight Select Schools
               h3("Highlight Specific Schools"),
               h4("Select up to 5 schools to highlight:"),
               selectizeInput(
                 inputId = "highlightSchool",
                 label = "Select School(s):",
                 choices = school_choices,
                 multiple = TRUE,
                 options=list(maxItems=5)
               )
             ), #End of sidebar Panel Student Exporatory Section
             mainPanel(
               tabsetPanel(
                 
                 # Sub-tab 2a - Variable Exploration
                 tabPanel("Variable Exploration",
                          h4("Variable Exploration"),
                          h5("Filtered Colleges with Variable(s) of Interest:"),
                          DTOutput("st_df"),
                          h5("Graphs of Variables of Interest:"),
                          plotOutput("st_plots")
                 ),
                 
                 # Sub-tab 2b
                 tabPanel("Best Value Index",
                          h3("Best Value Index Analysis"),
                          h4(textOutput("bvi_avg_text")),
                          br(),
                          h5("Filtered Data Table:"),
                          DTOutput("bvi_df"),
                          br(),
                          h5("Distribution of Best Value Index:"),
                          plotOutput("bvi_hist")
                 ),
                 
                 tabPanel("Map",
                          # The output for the leaflet map
                          leafletOutput("collegeMap", height = "600px")
                 )
               )
             )
           )
  ),
  
  # --- Main Tab 3: Researchers and Administrators ---
  tabPanel("Researchers and Administrators",
           tabsetPanel(
             
             # Sub-tab 3a: Single variable exploration
             tabPanel("Single variable exploration",
                      sidebarLayout(
                        sidebarPanel(
                          h3("Single Variable Exploration"),
                          helpText("Choose one numeric variable and optional filters, then inspect its distribution."),
                          
                          # numeric variable to explore
                          # KEPT AS ORIGINAL: Uses numeric_vars (No Urbanization)
                          selectInput(
                            inputId = "sv_var",
                            label   = "Numeric variable:",
                            choices = numeric_vars
                          ),
                          
                          # optional region filter
                          selectInput(
                            inputId = "sv_region",
                            label   = "Region:",
                            choices = c("All regions", region_choices)
                          ),
                          
                          # optional school type filter
                          selectInput(
                            inputId = "sv_school_type",
                            label   = "School type:",
                            choices = c("All types", "Public", "Private not-for-profit")
                          ),
                          
                          # optional log transform for skewed variables
                          checkboxInput("sv_log", "Apply log10 transform", FALSE)
                        ),
                        mainPanel(
                          h3(textOutput("sv_title")),
                          plotOutput("sv_hist"),
                          br(),
                          strong(textOutput("sv_mean_text"))
                        )
                      )
             ),
             
             # Sub-tab 3b: Multi-Variable Exploration
             tabPanel("Multi-Variable Exploration",
                      sidebarLayout(
                        sidebarPanel(
                          h3("Multi-Variable Exploration"),
                          helpText("Explore the relationship between two numeric variables and optionally compare groups."),
                          
                          # X variable
                          selectInput(
                            inputId = "mv_x",
                            label   = "X variable:",
                            choices = numeric_vars,
                            selected = "Admission Rate"
                          ),
                          
                          # Y variable
                          selectInput(
                            inputId = "mv_y",
                            label   = "Y variable:",
                            choices = numeric_vars,
                            selected = "Graduation Rate"
                          ),
                          
                          # optional grouping variable to compare groups
                          selectInput(
                            inputId = "mv_group",
                            label   = "Grouping variable (for tests/boxplots):",
                            choices = c(
                              "None"                                  = "none",
                              "Public vs Private"                     = "Control",
                              "Region"                                = "Region",
                              "HBCU status"                           = "HBCU"
                            ),
                            selected = "none"
                          ),
                          
                          # optional region filter
                          selectInput(
                            inputId = "mv_region",
                            label   = "Filter by region:",
                            choices = c("All regions", region_choices)
                          )
                        ),
                        mainPanel(
                          h3("Scatterplot: X vs Y"),
                          plotOutput("mv_scatter"),
                          br(),
                          h3("Group Comparison (Boxplot)"),
                          plotOutput("mv_boxplot"),
                          br(),
                          h3("Statistical Test Result"),
                          verbatimTextOutput("mv_test_result")
                        )
                      )
             ),
             
             # Sub-tab 3c: Model Testing
             tabPanel("Model Testing",
                      sidebarLayout(
                        sidebarPanel(
                          h3("Model Testing"),
                          helpText("Fit a linear regression model with graduation rate as the response."),
                          
                          # predictors chosen by the user
                          selectInput(
                            inputId  = "mt_predictors",
                            label    = "Predictor variables:",
                            multiple = TRUE,
                            choices  = numeric_vars
                          ),
                          
                          # optional filters for model subset
                          selectInput(
                            inputId = "mt_region",
                            label   = "Region:",
                            choices = c("All regions", region_choices)
                          ),
                          selectInput(
                            inputId = "mt_school_type",
                            label   = "School type:",
                            choices = c("All types", "Public", "Private not-for-profit")
                          ),
                          
                          # button so model only refits when clicked
                          actionButton("mt_fit", "Fit Regression Model", class = "btn-primary")
                        ),
                        mainPanel(
                          h3("Model Summary"),
                          verbatimTextOutput("mt_model_summary"),
                          br(),
                          h3("Coefficient Table"),
                          DTOutput("mt_coef_table")
                        )
                      )
             )
           )
  ),  # END of main tab 3
  
  # --- Main Tab 4: Data Table ---
  tabPanel(
    "Data Table",
    fluidPage(
      h2("Full College Dataset"),
      p("Use the filters in other tabs to understand the context, then explore and export the full cleaned dataset here."),
      downloadButton("download_full_data", "Download CSV", class = "btn-success"),
      br(), br(),
      DTOutput("full_table")
    )
  )
) # End of User Input Section



# --- Define the Server Logic ---
server <- function(input, output, session) {
  
  # Apply thematic styling to plots automatically
  thematic_shiny()
  
  # Server logic for 'Students and Parents' tab...
  ##Exploratory Analysis sub-tab:
  #This section creates a reactive df of the student's filters
  
  filt_st_data<-reactive({
    # We filter on full_data which now uses Nice Names.
    # We must use backticks for variable names with spaces.
    
    res <- full_data
    
    #White
    if(input$showRaceWhite == TRUE){
      min_w<-input$filterRaceWhite[1]
      max_w<-input$filterRaceWhite[2]
      res <- res |> filter(`% White`>= min_w & `% White`<= max_w)
    }
    #Asian
    if(input$showRaceAsian == TRUE){
      min_a<-input$filterRaceAsian[1]
      max_a<-input$filterRaceAsian[2]
      res <- res |> filter(`% Asian`>= min_a & `% Asian`<= max_a)
    }
    #Black
    if(input$showRaceBlack == TRUE){
      min_b<-input$filterRaceBlack[1]
      max_b<-input$filterRaceBlack[2]
      res <- res |> filter(`% Black`>= min_b & `% Black`<= max_b)
    }
    #Hispanic
    if(input$showRaceHispanic == TRUE){
      min_h<-input$filterRaceHispanic[1]
      max_h<-input$filterRaceHispanic[2]
      res <- res |> filter(`% Hispanic`>= min_h & `% Hispanic`<= max_h)
    }
    #Gender
    if(input$showGender == TRUE){
      min_f<-input$filterGender[1]
      max_f<-input$filterGender[2]
      res <- res |> filter(`% Female`>= min_f & `% Female`<= max_f)
    }
    #First Gen
    if(input$showFirstGen == TRUE){
      min_g<-input$filterFirstGen[1]
      max_g<-input$filterFirstGen[2]
      res <- res |> filter(`% First Gen`>= min_g & `% First Gen`<= max_g)
    }
    #SAT
    if(input$showSAT == TRUE){
      min_sat<-input$filterSAT[1]
      max_sat<-input$filterSAT[2]
      res <- res |> filter(`SAT Average`>= min_sat & `SAT Average`<= max_sat)
    }
    #Admission Rate
    if(input$showAdmRate == TRUE){
      min_r<-input$filterAdmRate[1]
      max_r<-input$filterAdmRate[2]
      res <- res |> filter(`Admission Rate`>= min_r & `Admission Rate`<= max_r)
    }
    #State
    if(input$showState == TRUE){
      sel_st<-input$filterState
      res <- res |> filter(State %in% sel_st)
    }
    #Region
    if(input$showRegion == TRUE){
      sel_rg<-input$filterRegion
      res <- res |> filter(Region %in% sel_rg)
    }
    #Public/Private
    if(input$showPublicPrivate == TRUE){
      sel_pub<-input$filterPublicPrivate
      res <- res |> filter(Control %in% sel_pub)
    }
    #HBCU
    if(input$showHBCU == TRUE){
      sel_hbcu<-input$filterHBCU
      res <- res |> filter(HBCU %in% sel_hbcu)
    }
    #Religious
    if(input$showReligion == TRUE){
      sel_rlg<-input$filterReligion
      res <- res |> filter(Religion %in% sel_rlg)
    }
    #Urbanization
    if(input$showUrban == TRUE){
      sel_urb<-input$filterUrban
      res <- res |> filter(Urbanization %in% sel_urb)
    }
    
    # Selection of columns to return
    selected_vars_stud <- input$selected_vars
    res <- res |> select(Institution, `Best Value Index`, Latitude, Longitude, any_of(selected_vars_stud))
    
    return(res)
  })# End of reactive df
  
  #This outputs the data table that the student selects
  output$st_df<-renderDT({
    validate(need(nrow(filt_st_data()) > 0, "You have filtered out all of the Colleges. Please widen your search."))
    # Only show selected vars in table
    selected <- input$selected_vars
    if(is.null(selected)) selected <- names(filt_st_data())[1:5]
    filt_st_data() |> select(Institution, any_of(selected))
  },
  options = list(pageLength=10)) #End of Student DF print
  
  #This outputs faceted graphs
  output$st_plots <- renderPlot({
    req(nrow(filt_st_data()) > 0)
    
    # Grab highlight data from FULL dataset (so it shows even if filtered out)
    # We must match the column structure of the filtered data
    high_df <- full_data |> 
      filter(Institution %in% input$highlightSchool) |> 
      select(Institution, any_of(input$selected_vars))
    
    # drop inst name for plotting
    plot_df <- filt_st_data() |> select(any_of(input$selected_vars))
    
    # separate num vs cat
    num_cols <- names(which(sapply(plot_df, is.numeric)))
    cat_cols <- names(which(!sapply(plot_df, is.numeric)))
    
    # Keep only selected columns
    num_cols <- intersect(num_cols, input$selected_vars)
    cat_cols <- intersect(cat_cols, input$selected_vars)
    
    # list of plots
    plot_list <- list()
    
    #numeric hist
    if(length(num_cols) > 0) {
      # Pivot the numeric columns
      df_num <- plot_df |> 
        select(all_of(num_cols)) |>
        pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value")
      
      p1 <- ggplot(df_num, aes(x = Value)) +
        geom_histogram(fill = "#2C3E50", color = "white", bins = 30) + # Used Theme Color
        facet_wrap(~Variable, scales = "free", ncol = 2) +
        theme_minimal() +
        labs(y = "Count", x = NULL) +
        # INCREASED TITLE SIZE HERE
        theme(strip.text = element_text(size = 16, face = "bold")) 
      
      # Add vertical line for highlights if they exist
      if(nrow(high_df) > 0) {
        high_num <- high_df |> 
          select(Institution, any_of(num_cols)) |>
          pivot_longer(cols = -Institution, names_to = "Variable", values_to = "Value")
        
        p1 <- p1 + geom_vline(data = high_num, 
                              aes(xintercept = Value, color = Institution), 
                              size = 1.2, show.legend = TRUE)
      }
      
      plot_list[[length(plot_list) + 1]] <- p1
    }
    
    # categorical plots
    if(length(cat_cols) > 0) {
      # Pivot  the categorical columns
      df_cat <- plot_df |> 
        select(all_of(cat_cols)) |>
        mutate(across(everything(), as.character)) |>
        pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value")
      
      p2 <- ggplot(df_cat, aes(x = Value)) +
        geom_bar(fill = "#18BC9C") + # Used Theme Accent
        facet_wrap(~Variable, scales = "free", ncol = 2) +
        coord_flip() + # Flips text to be readable
        theme_minimal() +
        labs(y = "Count", x = NULL) +
        # INCREASED TITLE SIZE HERE
        theme(strip.text = element_text(size = 16, face = "bold"))
      
      # Add horizontal line (visually) for highlights if they exist
      if(nrow(high_df) > 0) {
        high_cat <- high_df |> 
          select(Institution, all_of(cat_cols)) |>
          mutate(across(-Institution, as.character)) |>
          pivot_longer(cols = -Institution, names_to = "Variable", values_to = "Value")
        
        # Note: with coord_flip, geom_vline becomes horizontal visually
        p2 <- p2 + geom_vline(data = high_cat, 
                              aes(xintercept = Value, color = Institution), 
                              size = 1.2, show.legend = TRUE)
      }
      
      plot_list[[length(plot_list) + 1]] <- p2
    }
    # combine plots
    if(length(plot_list) > 0) {
      gridExtra::grid.arrange(grobs = plot_list, ncol = 1)
    }
  }) # End of Student Plots
  
  
  # --- BVI Logic (Independent from Variable Exploration) ---
  
  # Average BVI Display
  output$bvi_avg_text <- renderText({
    req(nrow(filt_st_data()) > 0)
    # Using backticks because of spaces in variable name
    avg_val <- mean(filt_st_data()$`Best Value Index`, na.rm = TRUE)
    paste("Average Best Value Index of Filtered Schools:", round(avg_val, 3))
  })
  
  # BVI Histogram
  output$bvi_hist <- renderPlot({
    req(nrow(filt_st_data()) > 0)
    
    # Highlight data from FULL dataset (so it shows even if filtered out)
    high_df <- full_data |> filter(Institution %in% input$highlightSchool)
    
    p <- ggplot(filt_st_data(), aes(x = `Best Value Index`)) +
      geom_histogram(fill = "#18BC9C", color = "white", bins = 30) + # Updated to teal
      theme_minimal() +
      labs(x = "Best Value Index", y = "Count", title = "Distribution of Best Value Index") +
      # INCREASED TITLE SIZE HERE
      theme(plot.title = element_text(size = 20, face = "bold"))
    
    # Add vertical lines for highlighted schools
    if(nrow(high_df) > 0) {
      p <- p + geom_vline(data = high_df, aes(xintercept = `Best Value Index`, color = Institution), 
                          size = 1.2, show.legend = TRUE)
    }
    p
  })
  
  # BVI Data Table
  output$bvi_df <- renderDT({
    validate(need(nrow(filt_st_data()) > 0, "No colleges match your criteria."))
    
    # Select relevant BVI columns and original variables for context
    filt_st_data() |> 
      select(Institution, `Best Value Index`, any_of(numeric_vars))
  }, options = list(pageLength = 10))
  
  
  
  # --- Server Logic for Map (Sub-tab 2c) ---
  
  # 3Render the Leaflet Map
  output$collegeMap <- renderLeaflet({
    # Use the reactive data (filt_st_data())
    
    # Dynamic Popup Construction
    data_map <- filt_st_data() |> filter(!is.na(Latitude), !is.na(Longitude))
    
    popup_content <- paste0("<b>", data_map$Institution, "</b>")
    
    # Loop through selected vars to add to popup
    vars_to_show <- input$selected_vars
    if (length(vars_to_show) > 0) {
      for (var in vars_to_show) {
        values <- data_map[[var]]
        if (is.numeric(values)) { values <- round(values, 2) }
        popup_content <- paste0(popup_content, "<br>", var, ": ", values)
      }
    }
    
    leaflet(data = data_map) |> 
      addTiles() |>  
      addCircleMarkers(
        lng = ~Longitude,
        lat = ~Latitude,
        radius = 5, 
        color = "navy",
        stroke = FALSE,
        fillOpacity = 0.7,
        popup = popup_content
      )
  })
  
  # Server logic for 'Researchers and Administrators' tab...
  
  
  
  
  # Research tab: Single variable exploration
  
  sv_data <- reactive({
    df <- full_data
    
    # optional region filter
    if (input$sv_region != "All regions") {
      df <- df |> dplyr::filter(Region == input$sv_region)
    }
    
    # optional school type filter
    if (input$sv_school_type != "All types") {
      df <- df |> dplyr::filter(Control == input$sv_school_type)
    }
    
    # keep only rows with non-missing selected variable
    df <- df |> dplyr::filter(!is.na(.data[[input$sv_var]]))
    
    validate(need(nrow(df) > 0, "No data available for these filter settings."))
    df
  })
  
  # title above histogram 
  output$sv_title <- renderText({
    paste("Distribution of", input$sv_var)
  })
  
  # histogram of selected variable
  output$sv_hist <- renderPlot({
    df <- sv_data()
    x  <- df[[input$sv_var]]
    pretty_name <- input$sv_var
    
    # optional log transform
    if (input$sv_log) {
      x_label <- paste0("log10(", pretty_name, ")")
      x       <- log10(x)
    } else {
      x_label <- pretty_name
    }
    
    ggplot(data.frame(x = x), aes(x)) +
      geom_histogram(bins = 30, fill = "#2C3E50", color = "white") +  # Navy
      theme_minimal() +
      labs(
        title = paste("Histogram of", pretty_name),
        x     = x_label,
        y     = "Count"
      )
  })
  
  # mean display 
  output$sv_mean_text <- renderText({
    df <- sv_data()
    x  <- df[[input$sv_var]]
    pretty_name <- input$sv_var
    
    if (input$sv_log) {
      mean_val <- mean(log10(x), na.rm = TRUE)
      paste("Mean of log10(", pretty_name, "):", round(mean_val, 3))
    } else {
      mean_val <- mean(x, na.rm = TRUE)
      paste("Mean of", pretty_name, ":", round(mean_val, 3))
    }
  })
  
  
  
  mv_data <- reactive({
    df <- full_data
    
    # optional region filter
    if (input$mv_region != "All regions") {
      df <- df |> dplyr::filter(Region == input$mv_region)
    }
    
    # require x and y not missing
    df <- df |>
      dplyr::filter(
        !is.na(.data[[input$mv_x]]),
        !is.na(.data[[input$mv_y]])
      )
    
    # if grouping variable selected, require it not missing and present
    if (input$mv_group != "none") {
      validate(need(input$mv_group %in% names(df),
                    "Selected grouping variable is not found in the data."))
      df <- df |> dplyr::filter(!is.na(.data[[input$mv_group]]))
    }
    
    validate(need(nrow(df) > 0, "No usable data for this variable combination."))
    df
  })
  
  # scatterplot
  output$mv_scatter <- renderPlot({
    df <- mv_data()
    
    if (input$mv_group == "none") {
      ggplot(df, aes(x = .data[[input$mv_x]], y = .data[[input$mv_y]])) +
        geom_point(alpha = 0.6, color = "#2C3E50") +         # Navy
        theme_minimal() +
        labs(
          title = paste(input$mv_y, "vs", input$mv_x),
          x     = input$mv_x,
          y     = input$mv_y
        )
    } else {
      ggplot(df,
             aes(x = .data[[input$mv_x]],
                 y = .data[[input$mv_y]],
                 color = as.factor(.data[[input$mv_group]]))) +
        geom_point(alpha = 0.6) +
        scale_color_manual(
          values = c("#2C3E50", "#18BC9C", "#3498DB", "#E74C3C", "#F39C12", "#9B59B6", "#1ABC9C", "#ECF0F1", "#34495E", "#95A5A6") 
        ) +
        theme_minimal() +
        labs(
          title = paste(input$mv_y, "vs", input$mv_x, "by", input$mv_group),
          x     = input$mv_x,
          y     = input$mv_y,
          color = input$mv_group
        )
    }
  })
  
  # boxplot by group 
  output$mv_boxplot <- renderPlot({
    req(input$mv_group != "none")
    df <- mv_data()
    
    ggplot(df,
           aes(x = as.factor(.data[[input$mv_group]]),
               y = .data[[input$mv_y]],
               fill = as.factor(.data[[input$mv_group]]))) +
      geom_boxplot(alpha = 0.8) +
      scale_fill_manual(
        values = c("#2C3E50", "#18BC9C", "#3498DB", "#E74C3C", "#F39C12", "#9B59B6", "#1ABC9C", "#ECF0F1", "#34495E", "#95A5A6")
      ) +
      theme_minimal() +
      labs(
        title = paste(input$mv_y, "by", input$mv_group),
        x     = input$mv_group,
        y     = input$mv_y
      )
  })
  
  # statistical tests
  # - no group: correlation between X and Y
  # - 2 groups: t-test on Y across groups
  # - >2 groups: ANOVA on Y across groups
  output$mv_test_result <- renderPrint({
    df <- mv_data()
    x  <- df[[input$mv_x]]
    y  <- df[[input$mv_y]]
    
    if (input$mv_group == "none") {
      cat("Correlation test between", input$mv_x, "and", input$mv_y, "\n\n")
      print(cor.test(x, y))
    } else {
      group_var <- as.factor(df[[input$mv_group]])
      n_levels  <- nlevels(group_var)
      
      if (n_levels == 2) {
        cat("Two-sample t-test of", input$mv_y, "by", input$mv_group, "\n\n")
        print(t.test(y ~ group_var))
      } else {
        cat("ANOVA of", input$mv_y, "by", input$mv_group, "\n\n")
        print(summary(aov(y ~ group_var)))
      }
    }
  })
  
  
  # Research tab: Model Testing
  
  mt_data <- reactive({
    df <- full_data
    
    # optional region filter
    if (input$mt_region != "All regions") {
      df <- df |> dplyr::filter(Region == input$mt_region)
    }
    
    # optional school type filter
    if (input$mt_school_type != "All types") {
      df <- df |> dplyr::filter(Control == input$mt_school_type)
    }
    
    # response + predictors
    # Outcome is "Graduation Rate" (Nice Name)
    vars <- c("Graduation Rate", input$mt_predictors)
    
    df <- df |>
      dplyr::select(dplyr::all_of(vars)) |>
      dplyr::filter(if_all(everything(), ~ !is.na(.)))
    
    validate(need(nrow(df) > 5, "Not enough complete records to fit a regression model."))
    df
  })
  
  # fit model only when button is clicked
  mt_fit <- eventReactive(input$mt_fit, {
    df <- mt_data()
    
    # build formula like "`Graduation Rate` ~ `Admission Rate` + `Tuition` + ..."
    # We must wrap names in backticks
    preds_safe <- paste(paste0("`", input$mt_predictors, "`"), collapse = " + ")
    outcome_safe <- "`Graduation Rate`"
    
    form <- as.formula(paste(outcome_safe, "~", preds_safe))
    
    lm(form, data = df)
  })
  
  # model summary with extra interpretation
  output$mt_model_summary <- renderPrint({
    req(input$mt_predictors)
    model  <- mt_fit()
    s      <- summary(model)
    r2     <- s$r.squared
    
    cat("Interpretation in context:\n")
    cat("• The model predicts Graduation Rate from your selected predictors.\n")
    cat("• R-squared ≈", round(r2 * 100, 1),
        "%, meaning the model explains about that percent of the variation in graduation rates\n",
        "  across the filtered set of institutions.\n")
    
    # quick directional summary of coefficients
    coefs <- coef(s)
    if (nrow(coefs) > 1) {
      cat("• Positive coefficients indicate that higher values of a predictor are associated\n",
          "  with higher graduation rates (holding other variables constant).\n",
          "  Negative coefficients indicate the opposite.\n\n")
    } else {
      cat("\n")
    }
    
    cat("Full regression output:\n\n")
    print(s)
  })
  
  # coefficient table
  output$mt_coef_table <- renderDT({
    req(input$mt_predictors)
    model <- mt_fit()
    tbl   <- as.data.frame(coef(summary(model)))
    tbl   <- tibble::rownames_to_column(tbl, "Term")
    datatable(tbl, options = list(pageLength = 10))
  })
  
  
  # Server logic for 'Data Table' tab...
  full_table_data <- reactive({
    full_data
  })
  
  output$full_table <- renderDT({
    full_table_data()
  },
  options = list(pageLength = 25, scrollX = TRUE))
  
  #download the file and have the date they downloaded it in the name
  output$download_full_data <- downloadHandler(
    filename = function() {
      paste0("college_dataset_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write_csv(full_table_data(), file)
    }
  )
  
}

# --- Run the Application ---

shinyApp(ui = ui, server = server)