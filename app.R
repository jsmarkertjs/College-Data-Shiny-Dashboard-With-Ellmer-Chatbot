library(shiny)
library(readr)
library(dplyr)
library(leaflet)
library(DT)
library(tidyverse)
#Load Data
full_data<-read_csv("./data/college_scorecard_11-21.csv")
#Percents to numbers
full_data |> 
  rename(pub_pri="Control of institution",
         urbanization="Degree of urbanization (Urban-centric locale)",
         religion="Religious affiliation") |> 
  mutate(ADM_RATE=ADM_RATE*100) |> 
  mutate(UGDS_ASIAN=UGDS_ASIAN*100) |> 
  mutate(UGDS_BLACK=UGDS_BLACK*100) |> 
  mutate(UGDS_HISP=UGDS_HISP*100) |> 
  mutate(UGDS_WHITE=UGDS_WHITE*100) |> 
  mutate(FEMALE=FEMALE*100) |> 
  mutate(FIRST_GEN=FIRST_GEN*100)->full_data

#Renaming Map (New Name = Old Name)
rename_map <- c(
  "Admission Rate" = "ADM_RATE",
  "Median Earnings After 10 Years" = "MD_EARN_WNE_P10",
  "Tuition (In-state)" = "TUITIONFEE_IN",
  "Tuition (Out-of-state)" = "TUITIONFEE_OUT",
  "Median Debt at Graduation" = "GRAD_DEBT_MDN",
  "Average SAT" = "SAT_AVG",
  "Average Cost (In-state)" = "NPT4_PUB",
  "Average Cost (Out-of-state)" = "NPT4_PRIV",
  "Urbanization" = "urbanization",
  "Religion" = "religion"
)

states<-sort(full_data$"State abbreviation")
regions<-full_data$"Bureau of Economic Analysis (BEA) regions"
religion_choices<-sort(unique(full_data$religion))
urbanization_choices<-sort(unique(full_data$urbanization))
school_choices<-sort(unique(full_data$INSTNM))
variable_choices<-names(rename_map)

student_variable_dataset<-full_data |> 
  rename(any_of(rename_map)) |> 
  select(names(rename_map))


#BVI
#A higher BVI indicates a more favorable cost-benefit ratio, suggesting that the college 
#offers high earning potential relative to its price tag and debt load.
full_data <- full_data |>
  mutate(
    # Create a single 'NET_COST' column based on the institution's control
    # CONTROL: 1 = Public, 2 = Private nonprofit, 3 = Private for-profit
    NET_COST = case_when(
      CONTROL == 1 ~ NPT4_PUB,
      CONTROL %in% c(2, 3) ~ NPT4_PRIV,
      TRUE ~ NA_real_ # Use NA if CONTROL is missing or unexpected
    ),
    
    # Create BVI column
    Best_Value_Index = round(MD_EARN_WNE_P10 / (NET_COST + GRAD_DEBT_MDN), 3)
  )







ui <- navbarPage(
  title = "College Education Analysis",
  
  #INSERT THEME HERE 
  
  # --- Main Tab 1: Intro ---
  tabPanel(
    "Intro",
    fluidPage(
      tags$head(
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
      
      fluidRow(
        column(
          width = 4,
          wellPanel(
            h4(
              tags$a(
                href = "#",
                onclick = "switchToTab('Students and Parents'); return false;",
                "Students & Families"
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
                "Researchers & Admins"
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
                "Data Table"
              )
            ),
            p(
              "View the full cleaned dataset in a sortable table, filter columns, and export data for use in your own analyses or reports."
            )
          )
        )
      ),
      
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
           tabsetPanel(
             
             
             
             
             # Sub-tab 2a - shae's section
             tabPanel("Variable Exploration",
                      sidebarLayout(
                        sidebarPanel(
                          h3("Variable(s) of Interest"),
                          selectInput("selected_vars", "Select Variable(s):",
                                      choices=variable_choices, 
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
                                           choices =states, multiple = TRUE)
                          ),
                          # Region
                          checkboxInput("showRegion", "Filter by Region"),
                          conditionalPanel(
                            "input.showRegion == true",
                            selectizeInput("filterRegion", "Region(s)", 
                                           choices = regions, multiple = TRUE)
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
                                           choices = urbanization_choices, multiple = TRUE)
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
                          
                          
                          h4("Variable Exploration"),
                          h5("Filtered Colleges with Variable(s) of Interest:"),
                          DTOutput("st_df"),
                          h5("Graphs of Variables of Interest:"),
                          plotOutput("st_plots")
                          
                          # Add stuff for sub-tab here
                        ), #End up Main Panel Student Exploratory Section
                      )),
             
             
             
             
             # Sub-tab 2b
             tabPanel("Best Value Index",
                      sidebarLayout(
                        sidebarPanel(
                          h3("School Filtering (BVI)"),
                          #Filtering by demographics: race, gender, firstgen
                          h4("Demographics"),
                          checkboxInput("bvi_showRaceWhite", "Filter by % White"),
                          conditionalPanel(
                            "input.bvi_showRaceWhite == true",
                            sliderInput("bvi_filterRaceWhite", "Percent White", 
                                        min = 0, max = 100, value = c(0, 100), post = "%")
                          ),
                          checkboxInput("bvi_showRaceAsian", "Filter by % Asian"),
                          conditionalPanel(
                            "input.bvi_showRaceAsian == true",
                            sliderInput("bvi_filterRaceAsian", "Percent Asian", 
                                        min = 0, max = 100, value = c(0, 100), post = "%")
                          ),
                          checkboxInput("bvi_showRaceBlack", "Filter by % Black"),
                          conditionalPanel(
                            "input.bvi_showRaceBlack == true",
                            sliderInput("bvi_filterRaceBlack", "Percent Black", 
                                        min = 0, max = 100, value = c(0, 100), post = "%")
                          ),
                          checkboxInput("bvi_showRaceHispanic", "Filter by % Hispanic"),
                          conditionalPanel(
                            "input.bvi_showRaceHispanic == true",
                            sliderInput("bvi_filterRaceHispanic", "Percent Hispanic", 
                                        min = 0, max = 100, value = c(0, 100), post = "%")
                          ),
                          checkboxInput("bvi_showGender", "Filter by % Female"),
                          conditionalPanel(
                            "input.bvi_showGender == true",
                            sliderInput("bvi_filterGender", "Percent Female", 
                                        min = 0, max = 100, value = c(0, 100), post = "%")
                          ),
                          checkboxInput("bvi_showFirstGen", "Filter by % First Gen"),
                          conditionalPanel(
                            "input.bvi_showFirstGen == true",
                            sliderInput("bvi_filterFirstGen", "Percent First Gen", 
                                        min = 0, max = 100, value = c(0, 100), post = "%")
                          ),
                          h4("Academics"),
                          checkboxInput("bvi_showSAT", "Filter by SAT Score"),
                          conditionalPanel(
                            "input.bvi_showSAT == true",
                            sliderInput("bvi_filterSAT", "SAT Average Range", 
                                        min = 400, max = 1600, value = c(400, 1600))
                          ),
                          checkboxInput("bvi_showAdmRate", "Filter by Admission Rate"),
                          conditionalPanel(
                            "input.bvi_showAdmRate == true",
                            sliderInput("bvi_filterAdmRate", "Admission Rate", 
                                        min = 0, max = 100, value = c(0, 100), post = "%")
                          ),
                          h4("Location"),
                          # State
                          checkboxInput("bvi_showState", "Filter by State"),
                          conditionalPanel(
                            "input.bvi_showState == true",
                            selectizeInput("bvi_filterState", "State(s)", 
                                           choices =states, multiple = TRUE)
                          ),
                          # Region
                          checkboxInput("bvi_showRegion", "Filter by Region"),
                          conditionalPanel(
                            "input.bvi_showRegion == true",
                            selectizeInput("bvi_filterRegion", "Region(s)", 
                                           choices = regions, multiple = TRUE)
                          ),
                          h4("Characteristics"),
                          #Public/Private
                          checkboxInput("bvi_showPublicPrivate", "Filter by School Type"),
                          conditionalPanel(
                            "input.bvi_showPublicPrivate == true",
                            radioButtons("bvi_filterPublicPrivate", "School Type", 
                                         choices = c("Public", "Private not-for-profit"))
                          ),
                          # HBCU
                          checkboxInput("bvi_showHBCU", "Filter by HBCU Status"),
                          conditionalPanel(
                            "input.bvi_showHBCU == true",
                            radioButtons("bvi_filterHBCU", "HBCU Status", 
                                         choices = c("Yes", "No"))
                          ),
                          # Religion
                          checkboxInput("bvi_showReligion", "Filter by Religious Affiliation"),
                          conditionalPanel(
                            "input.bvi_showReligion == true",
                            selectizeInput("bvi_filterReligion", "Religious Affiliation(s)", 
                                           choices = religion_choices, multiple = TRUE)
                          ),
                          # Degree of Urbanization
                          checkboxInput("bvi_showUrban", "Filter by Urbanization"),
                          conditionalPanel(
                            "input.bvi_showUrban == true",
                            selectizeInput("bvi_filterUrban", "Degree of Urbanization", 
                                           choices = urbanization_choices, multiple = TRUE)
                          ),
                          #Highlight Select Schools
                          h3("Highlight Specific Schools"),
                          h4("Select up to 5 schools to highlight:"),
                          selectizeInput(
                            inputId = "bvi_highlightSchool",
                            label = "Select School(s):",
                            choices = school_choices,
                            multiple = TRUE,
                            options=list(maxItems=5)
                          )
                        ),
                        mainPanel(
                          h3("Best Value Index Analysis"),
                          h4(textOutput("bvi_avg_text")),
                          br(),
                          h5("Filtered Data Table:"),
                          DTOutput("bvi_df"),
                          br(),
                          h5("Distribution of Best Value Index:"),
                          plotOutput("bvi_hist")
                        )
                      )
             ),
             
             tabPanel("Map",
                      sidebarLayout(
                        sidebarPanel(
                          h3("Map Filters"),
                          # Input for selecting the Region
                          selectInput("mapRegion", "Select Region:", 
                                      choices = sort(unique(full_data$`Bureau of Economic Analysis (BEA) regions`)),
                                      selected = sort(unique(full_data$`Bureau of Economic Analysis (BEA) regions`))[1])
                        ),
                        mainPanel(
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
                          selectInput(
                            inputId = "sv_var",
                            label   = "Numeric variable:",
                            choices = c(
                              "Graduation rate (150% time)"     = "C150_4",
                              "Admission Rate (%)"              = "ADM_RATE",
                              "Median Earnings After 10 Years"  = "MD_EARN_WNE_P10",
                              "Tuition (In-state)"              = "TUITIONFEE_IN",
                              "Tuition (Out-of-state)"          = "TUITIONFEE_OUT",
                              "Median Debt at Graduation"       = "GRAD_DEBT_MDN",
                              "Average SAT"                     = "SAT_AVG",
                              "Average Cost (In-state)"         = "NPT4_PUB",
                              "Average Cost (Out-of-state)"     = "NPT4_PRIV",
                              "Average Faculty Salary"          = "AVGFACSAL",
                              "Best Value Index"                = "Best_Value_Index"
                            )
                          ),
                          
                          # optional region filter
                          selectInput(
                            inputId = "sv_region",
                            label   = "Region:",
                            choices = c("All regions", sort(unique(full_data$`Bureau of Economic Analysis (BEA) regions`)))
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
                            choices = c(
                              "Admission Rate (%)"              = "ADM_RATE",
                              "Graduation rate (150% time)"     = "C150_4",
                              "Median Earnings After 10 Years"  = "MD_EARN_WNE_P10",
                              "Tuition (In-state)"              = "TUITIONFEE_IN",
                              "Tuition (Out-of-state)"          = "TUITIONFEE_OUT",
                              "Median Debt at Graduation"       = "GRAD_DEBT_MDN",
                              "Average SAT"                     = "SAT_AVG",
                              "Average Cost (In-state)"         = "NPT4_PUB",
                              "Average Cost (Out-of-state)"     = "NPT4_PRIV",
                              "Best Value Index"                = "Best_Value_Index"
                            ),
                            selected = "ADM_RATE"
                          ),
                          
                          # Y variable
                          selectInput(
                            inputId = "mv_y",
                            label   = "Y variable:",
                            choices = c(
                              "Graduation rate (150% time)"     = "C150_4",
                              "Admission Rate (%)"              = "ADM_RATE",
                              "Median Earnings After 10 Years"  = "MD_EARN_WNE_P10",
                              "Median Debt at Graduation"       = "GRAD_DEBT_MDN",
                              "Best Value Index"                = "Best_Value_Index"
                            ),
                            selected = "C150_4"
                          ),
                          
                          # optional grouping variable to compare groups
                          selectInput(
                            inputId = "mv_group",
                            label   = "Grouping variable (for tests/boxplots):",
                            choices = c(
                              "None"                            = "none",
                              "Public vs Private"               = "pub_pri",
                              "Region"                          = "Bureau of Economic Analysis (BEA) regions",
                              "HBCU status"                     = "Historically Black College or University"
                            ),
                            selected = "none"
                          ),
                          
                          # optional region filter
                          selectInput(
                            inputId = "mv_region",
                            label   = "Filter by region:",
                            choices = c("All regions", sort(unique(full_data$`Bureau of Economic Analysis (BEA) regions`)))
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
                            choices  = c(
                              "Admission Rate (%)"              = "ADM_RATE",
                              "Median Earnings After 10 Years"  = "MD_EARN_WNE_P10",
                              "Tuition (In-state)"              = "TUITIONFEE_IN",
                              "Tuition (Out-of-state)"          = "TUITIONFEE_OUT",
                              "Median Debt at Graduation"       = "GRAD_DEBT_MDN",
                              "Average SAT"                     = "SAT_AVG",
                              "Average Cost (In-state)"         = "NPT4_PUB",
                              "Average Cost (Out-of-state)"     = "NPT4_PRIV",
                              "Average Faculty Salary"          = "AVGFACSAL",
                              "Best Value Index"                = "Best_Value_Index"
                            )
                          ),
                          
                          # optional filters for model subset
                          selectInput(
                            inputId = "mt_region",
                            label   = "Region:",
                            choices = c("All regions", sort(unique(full_data$`Bureau of Economic Analysis (BEA) regions`)))
                          ),
                          selectInput(
                            inputId = "mt_school_type",
                            label   = "School type:",
                            choices = c("All types", "Public", "Private not-for-profit")
                          ),
                          
                          # button so model only refits when clicked
                          actionButton("mt_fit", "Fit Regression Model")
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
      downloadButton("download_full_data", "Download CSV"),
      br(), br(),
      DTOutput("full_table")
    )
  )
) # End of User Input Section



# --- Define the Server Logic ---
server <- function(input, output, session) {
  
  # Server logic for 'Students and Parents' tab...
  ##Exploratory Analysis sub-tab:
  #This section creates a reactive df of the student's filters
  
  filt_st_data<-reactive({
    #White
    if(input$showRaceWhite == TRUE){
      min_w<-input$filterRaceWhite[1]
      max_w<-input$filterRaceWhite[2]
      full_data |> 
        filter(PCT_WHITE>= min_w & 
                 PCT_WHITE<= max_w)->full_data
    }
    #Asian
    if(input$showRaceAsian == TRUE){
      min_a<-input$filterRaceAsian[1]
      max_a<-input$filterRaceAsian[2]
      full_data |> 
        filter(UGDS_ASIAN>= min_a & 
                 UGDS_ASIAN<= max_a)->full_data
    }
    #Black
    if(input$showRaceBlack == TRUE){
      min_b<-input$filterRaceBlack[1]
      max_b<-input$filterRaceBlack[2]
      full_data |> 
        filter(UGDS_BLACK>= min_b & 
                 UGDS_BLACK<= max_b)->full_data
    }
    #Hispanic
    if(input$showRaceHispanic == TRUE){
      min_h<-input$filterRaceHispanic[1]
      max_h<-input$filterRaceHispanic[2]
      full_data |> 
        filter(UGDS_HISP>= min_h & 
                 UGDS_HISP<= max_h)->full_data
    }
    #Gender
    if(input$showGender == TRUE){
      min_f<-input$filterGender[1]
      max_f<-input$filterGender[2]
      full_data |> 
        filter(FEMALE>= min_f & 
                 FEMALE<= max_f)->full_data
    }
    #First Gen
    if(input$showFirstGen == TRUE){
      min_g<-input$filterFirstGen[1]
      max_g<-input$filterFirstGen[2]
      full_data |> 
        filter(FIRST_GEN>= min_g & 
                 FIRST_GEN<= max_g)->full_data
    }
    #SAT
    if(input$showSAT == TRUE){
      min_sat<-input$filterSAT[1]
      max_sat<-input$filterSAT[2]
      full_data |> 
        filter(SAT_AVG>= min_sat & 
                 SAT_AVG<= max_sat)->full_data
    }
    #Admission Rate
    if(input$showAdmRate == TRUE){
      min_r<-input$filterAdmRate[1]
      max_r<-input$filterAdmRate[2]
      full_data |> 
        filter(ADM_RATE>= min_r & 
                 ADM_RATE<= max_r)->full_data
    }
    #State
    if(input$showState == TRUE){
      sel_st<-input$filterState
      full_data |> 
        filter(`State abbreviation` %in% sel_st)->full_data
    }
    #Region
    if(input$showRegion == TRUE){
      sel_rg<-input$filterRegion
      full_data |> 
        filter(`Bureau of Economic Analysis (BEA) regions` %in% sel_rg)->full_data
    }
    #Public/Private
    if(input$showPublicPrivate == TRUE){
      sel_pub<-input$filterPublicPrivate
      full_data |> 
        filter(pub_pri %in% sel_pub)->full_data
    }
    #HBCU
    if(input$showHBCU == TRUE){
      sel_hbcu<-input$filterHBCU
      full_data |> 
        filter(`Historically Black College or University` %in% sel_hbcu)->full_data
    }
    #Religious
    if(input$showReligion == TRUE){
      sel_rlg<-input$filterReligion
      full_data |> 
        filter(religion %in% sel_rlg)->full_data
    }
    #Urbanization
    if(input$showUrban == TRUE){
      sel_urb<-input$filterUrban
      full_data |> 
        filter(urbanization %in% sel_urb)->full_data
    }
    
    #RENAME COLUMNS
    full_data |> 
      rename(any_of(rename_map))->full_data
    
    selected_vars_stud<-as.character(input$selected_vars)
    full_data<-full_data |> 
      select(INSTNM, all_of(selected_vars_stud))
    
    return(full_data)
  })# End of reactive df
  
  #This outputs the data table that the student selects
  output$st_df<-renderDT({
    validate(need(nrow(filt_st_data()) > 0, "You have filtered out all of the Colleges. Please widen your search."))
    filt_st_data()
  },
  options = list(pageLength=10)) #End of Student DF print
  
  #This outputs faceted graphs
  output$st_plots <- renderPlot({
    req(nrow(filt_st_data()) > 0)
    
    # 1. Grab highlight data from FULL dataset (so it shows even if filtered out)
    # We must rename columns to match the plotting data
    high_df <- full_data |> 
      filter(INSTNM %in% input$highlightSchool) |> 
      rename(any_of(rename_map)) |> 
      select(INSTNM, any_of(input$selected_vars))
    
    # drop inst name
    plot_df <- filt_st_data() |> select(-INSTNM)
    
    # separate num vs cat
    num_cols <- names(which(sapply(plot_df, is.numeric)))
    cat_cols <- names(which(!sapply(plot_df, is.numeric)))
    
    # list of plots
    plot_list <- list()
    
    #numeric hist
    if(length(num_cols) > 0) {
      # Pivot the numeric columns
      df_num <- plot_df |> 
        select(all_of(num_cols)) |>
        pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value")
      
      p1 <- ggplot(df_num, aes(x = Value)) +
        geom_histogram(fill = "steelblue", color = "white", bins = 30) +
        facet_wrap(~Variable, scales = "free", ncol = 2) +
        theme_minimal() +
        labs(y = "Count", x = NULL)
      
      # Add vertical line for highlights if they exist
      if(nrow(high_df) > 0) {
        high_num <- high_df |> 
          select(INSTNM, all_of(num_cols)) |>
          pivot_longer(cols = -INSTNM, names_to = "Variable", values_to = "Value")
        
        p1 <- p1 + geom_vline(data = high_num, 
                              aes(xintercept = Value, color = INSTNM), 
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
        geom_bar(fill = "darkgreen") +
        facet_wrap(~Variable, scales = "free", ncol = 2) +
        coord_flip() + # Flips text to be readable
        theme_minimal() +
        labs(y = "Count", x = NULL)
      
      # Add horizontal line (visually) for highlights if they exist
      if(nrow(high_df) > 0) {
        high_cat <- high_df |> 
          select(INSTNM, all_of(cat_cols)) |>
          mutate(across(-INSTNM, as.character)) |>
          pivot_longer(cols = -INSTNM, names_to = "Variable", values_to = "Value")
        
        # Note: with coord_flip, geom_vline becomes horizontal visually
        p2 <- p2 + geom_vline(data = high_cat, 
                              aes(xintercept = Value, color = INSTNM), 
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
  
  # Reactive dataframe for BVI tab
  filt_bvi_data <- reactive({
    res <- full_data
    
    #White
    if(input$bvi_showRaceWhite == TRUE){
      min_w<-input$bvi_filterRaceWhite[1]
      max_w<-input$bvi_filterRaceWhite[2]
      res <- res |> filter(PCT_WHITE>= min_w & PCT_WHITE<= max_w)
    }
    #Asian
    if(input$bvi_showRaceAsian == TRUE){
      min_a<-input$bvi_filterRaceAsian[1]
      max_a<-input$bvi_filterRaceAsian[2]
      res <- res |> filter(UGDS_ASIAN>= min_a & UGDS_ASIAN<= max_a)
    }
    #Black
    if(input$bvi_showRaceBlack == TRUE){
      min_b<-input$bvi_filterRaceBlack[1]
      max_b<-input$bvi_filterRaceBlack[2]
      res <- res |> filter(UGDS_BLACK>= min_b & UGDS_BLACK<= max_b)
    }
    #Hispanic
    if(input$bvi_showRaceHispanic == TRUE){
      min_h<-input$bvi_filterRaceHispanic[1]
      max_h<-input$bvi_filterRaceHispanic[2]
      res <- res |> filter(UGDS_HISP>= min_h & UGDS_HISP<= max_h)
    }
    #Gender
    if(input$bvi_showGender == TRUE){
      min_f<-input$bvi_filterGender[1]
      max_f<-input$bvi_filterGender[2]
      res <- res |> filter(FEMALE>= min_f & FEMALE<= max_f)
    }
    #First Gen
    if(input$bvi_showFirstGen == TRUE){
      min_g<-input$bvi_filterFirstGen[1]
      max_g<-input$bvi_filterFirstGen[2]
      res <- res |> filter(FIRST_GEN>= min_g & FIRST_GEN<= max_g)
    }
    #SAT
    if(input$bvi_showSAT == TRUE){
      min_sat<-input$bvi_filterSAT[1]
      max_sat<-input$bvi_filterSAT[2]
      res <- res |> filter(SAT_AVG>= min_sat & SAT_AVG<= max_sat)
    }
    #Admission Rate
    if(input$bvi_showAdmRate == TRUE){
      min_r<-input$bvi_filterAdmRate[1]
      max_r<-input$bvi_filterAdmRate[2]
      res <- res |> filter(ADM_RATE>= min_r & ADM_RATE<= max_r)
    }
    #State
    if(input$bvi_showState == TRUE){
      sel_st<-input$bvi_filterState
      res <- res |> filter(`State abbreviation` %in% sel_st)
    }
    #Region
    if(input$bvi_showRegion == TRUE){
      sel_rg<-input$bvi_filterRegion
      res <- res |> filter(`Bureau of Economic Analysis (BEA) regions` %in% sel_rg)
    }
    #Public/Private
    if(input$bvi_showPublicPrivate == TRUE){
      sel_pub<-input$bvi_filterPublicPrivate
      res <- res |> filter(pub_pri %in% sel_pub)
    }
    #HBCU
    if(input$bvi_showHBCU == TRUE){
      sel_hbcu<-input$bvi_filterHBCU
      res <- res |> filter(`Historically Black College or University` %in% sel_hbcu)
    }
    #Religious
    if(input$bvi_showReligion == TRUE){
      sel_rlg<-input$bvi_filterReligion
      res <- res |> filter(religion %in% sel_rlg)
    }
    #Urbanization
    if(input$bvi_showUrban == TRUE){
      sel_urb<-input$bvi_filterUrban
      res <- res |> filter(urbanization %in% sel_urb)
    }
    
    return(res)
  })
  
  # Average BVI Display
  output$bvi_avg_text <- renderText({
    req(nrow(filt_bvi_data()) > 0)
    avg_val <- mean(filt_bvi_data()$Best_Value_Index, na.rm = TRUE)
    paste("Average Best Value Index of Filtered Schools:", round(avg_val, 3))
  })
  
  # BVI Histogram
  output$bvi_hist <- renderPlot({
    req(nrow(filt_bvi_data()) > 0)
    
    # Highlight data from FULL dataset (so it shows even if filtered out)
    high_df <- full_data |> filter(INSTNM %in% input$bvi_highlightSchool)
    
    p <- ggplot(filt_bvi_data(), aes(x = Best_Value_Index)) +
      geom_histogram(fill = "purple", color = "white", bins = 30) +
      theme_minimal() +
      labs(x = "Best Value Index", y = "Count", title = "Distribution of Best Value Index")
    
    # Add vertical lines for highlighted schools
    if(nrow(high_df) > 0) {
      p <- p + geom_vline(data = high_df, aes(xintercept = Best_Value_Index, color = INSTNM), 
                          size = 1.2, show.legend = TRUE)
    }
    p
  })
  
  # BVI Data Table
  output$bvi_df <- renderDT({
    validate(need(nrow(filt_bvi_data()) > 0, "No colleges match your criteria."))
    
    # Select relevant BVI columns and original variables for context
    filt_bvi_data() |> 
      select(INSTNM, Best_Value_Index, any_of(names(rename_map)))
  }, options = list(pageLength = 10))
  
  
  
  # --- Server Logic for Map (Sub-tab 2c) ---
  
  # 1. Create a reactive dataset for the map that filters by Region
  map_data <- reactive({
    # Filter full_data based on the selected region from the dropdown
    full_data |> 
      filter(`Bureau of Economic Analysis (BEA) regions` == input$mapRegion)
  })
  
  # 3. Render the Leaflet Map
  output$collegeMap <- renderLeaflet({
    # Use the reactive data (map_data())
    leaflet(data = map_data()) |> 
      addTiles() |>  # 
      addCircleMarkers(
        lng = ~LONGITUDE,
        lat = ~LATITUDE,
        radius = 5, 
        color = "navy",
        stroke = FALSE,
        fillOpacity = 0.7,
        popup = ~paste0(
          "<b>", INSTNM, "</b><br>",
          "Admission Rate: ", round(ADM_RATE, 1), "%<br>",
          "Average SAT: ", SAT_AVG, "<br>",
          "Best Value Index: ", Best_Value_Index
        )
      )
  })
  
  # Server logic for 'Researchers and Administrators' tab...

  
  
  
  # Research tab: Single variable exploration

  sv_choices <- c(
    "Graduation rate (150% time)"    = "C150_4",
    "Admission Rate (%)"            = "ADM_RATE",
    "Median Earnings After 10 Years"= "MD_EARN_WNE_P10",
    "Tuition (In-state)"            = "TUITIONFEE_IN",
    "Tuition (Out-of-state)"        = "TUITIONFEE_OUT",
    "Median Debt at Graduation"     = "GRAD_DEBT_MDN",
    "Average SAT"                   = "SAT_AVG",
    "Average Cost (In-state)"       = "NPT4_PUB",
    "Average Cost (Out-of-state)"   = "NPT4_PRIV",
    "Best Value Index"              = "Best_Value_Index"
  )
  
  get_sv_label <- function(code) {
    nm <- names(sv_choices)[sv_choices == code]
    if (length(nm) == 0) code else nm[1]
  }
  
  sv_data <- reactive({
    df <- full_data
    
    # optional region filter
    if (input$sv_region != "All regions") {
      df <- df |> dplyr::filter(`Bureau of Economic Analysis (BEA) regions` == input$sv_region)
    }
    
    # optional school type filter
    if (input$sv_school_type != "All types") {
      df <- df |> dplyr::filter(pub_pri == input$sv_school_type)
    }
    
    # keep only rows with non-missing selected variable
    df <- df |> dplyr::filter(!is.na(.data[[input$sv_var]]))
    
    validate(need(nrow(df) > 0, "No data available for these filter settings."))
    df
  })
  
  # title above histogram (use readable label)
  output$sv_title <- renderText({
    pretty_name <- get_sv_label(input$sv_var)
    paste("Distribution of", pretty_name)
  })
  
  # histogram of selected variable
  output$sv_hist <- renderPlot({
    df <- sv_data()
    x  <- df[[input$sv_var]]
    pretty_name <- get_sv_label(input$sv_var)
    
    # optional log transform
    if (input$sv_log) {
      x_label <- paste0("log10(", pretty_name, ")")
      x       <- log10(x)
    } else {
      x_label <- pretty_name
    }
    
    ggplot(data.frame(x = x), aes(x)) +
      geom_histogram(bins = 30, fill = "steelblue", color = "white") +  # match student tab
      theme_minimal() +
      labs(
        title = paste("Histogram of", pretty_name),
        x     = x_label,
        y     = "Count"
      )
  })
  
  # mean display (use readable label)
  output$sv_mean_text <- renderText({
    df <- sv_data()
    x  <- df[[input$sv_var]]
    pretty_name <- get_sv_label(input$sv_var)
    
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
      df <- df |> dplyr::filter(`Bureau of Economic Analysis (BEA) regions` == input$mv_region)
    }
    
    # require x and y not missing
    df <- df |>
      dplyr::filter(
        !is.na(.data[[input$mv_x]]),
        !is.na(.data[[input$mv_y]])
      )
    
    # if grouping variable selected, require it not missing AND present
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
        geom_point(alpha = 0.6, color = "steelblue") +        # match numeric student plots
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
          values = c("steelblue", "darkgreen", "purple", "navy")
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
  
  # boxplot by group (only when group selected)
  output$mv_boxplot <- renderPlot({
    req(input$mv_group != "none")
    df <- mv_data()
    
    ggplot(df,
           aes(x = as.factor(.data[[input$mv_group]]),
               y = .data[[input$mv_y]],
               fill = as.factor(.data[[input$mv_group]]))) +
      geom_boxplot(alpha = 0.8) +
      scale_fill_manual(
        values = c("steelblue", "darkgreen", "purple", "navy")
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
      df <- df |> dplyr::filter(`Bureau of Economic Analysis (BEA) regions` == input$mt_region)
    }
    
    # optional school type filter
    if (input$mt_school_type != "All types") {
      df <- df |> dplyr::filter(pub_pri == input$mt_school_type)
    }
    
    # response + predictors
    vars <- c("C150_4", input$mt_predictors)
    
    df <- df |>
      dplyr::select(dplyr::all_of(vars)) |>
      dplyr::filter(if_all(everything(), ~ !is.na(.)))
    
    validate(need(nrow(df) > 5, "Not enough complete records to fit a regression model."))
    df
  })
  
  # fit model only when button is clicked
  mt_fit <- eventReactive(input$mt_fit, {
    df <- mt_data()
    
    # build formula like "C150_4 ~ ADM_RATE + TUITIONFEE_IN + ..."
    form <- as.formula(
      paste("C150_4 ~", paste(input$mt_predictors, collapse = " + "))
    )
    
    lm(form, data = df)
  })
  
  # model summary with extra interpretation
  output$mt_model_summary <- renderPrint({
    req(input$mt_predictors)
    model  <- mt_fit()
    s      <- summary(model)
    r2     <- s$r.squared
    
    cat("Interpretation in context:\n")
    cat("• The model predicts 6-year graduation rate (C150_4) from your selected predictors.\n")
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
    full_data |>
      rename(any_of(rename_map))
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