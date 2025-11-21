
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

states<-sort(full_data$"State abbreviation")
regions<-full_data$"Bureau of Economic Analysis (BEA) regions"
religion_choices<-sort(full_data$religion)
urbanization_choices<-full_data$urbanization
school_choices<-full_data$INSTNM

student_variable_dataset<-full_data |> 
  select(ADM_RATE, GRAD_DEBT_MDN, MD_EARN_WNE_P10, SAT_AVG, TUITIONFEE_IN, 
         TUITIONFEE_OUT, NPT4_PUB, NPT4_PRIV, urbanization, religion)


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
  tabPanel("Intro",
           h2("Introduction Page"),
           p("Content for intro page.")
           #Please note SAT filtering will eliminate 1/2 the data and explain 
           #some of the variables that may be confusing 
           
  ),
  
  # --- Main Tab 2: Students and Parents ---
  tabPanel("Students and Parents",
           tabsetPanel(
             
             
             
             
             # Sub-tab 2a - shae's section
             tabPanel("Variable Exploration",
                      sidebarLayout(
                        sidebarPanel(
                          h3("Variable(s) of Interest"),
                          varSelectInput("selected_vars", "Select Variable(s):",
                                         data=student_variable_dataset, 
                                         multiple = TRUE, selected="ADM_RATE"),
                          
                          
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
                      h3("Best Value Index"),
                      p("Content here.")
                      # Add stuff for sub-tab here
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
             
             # Sub-tab 3a
             tabPanel("Single variable exploration",
                      h3("Single Variable Exploration"),
                      p("Content here.")
                      # Add stuff for sub-tab here
             ),
             
             # Sub-tab 3b
             tabPanel("Multi-Variable Exploration",
                      h3("Multi-Variable Exploration"),
                      p("Content here.")
                      # Add stuff for sub-tab here
             ),
             
             # Sub-tab 3c
             tabPanel("Model Testing",
                      h3("Model Testing"),
                      p("Content here.")
                      # Add stuff for sub-tab here
             )
           )
  ),
  
  # --- Main Tab 4: Data Table ---
  tabPanel("Data Table",
           h2("Data Table Page"),
           p("This is where the main data table will be displayed."),
           
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
  output$st_plots<-renderPlot({
    validate(need(nrow(filt_st_data()) > 1, "Graphing requires more than one College. Please widen your search. "))
    filt_data<-filt_st_data()[, -1]
    var1<-sym(colnames(filt_data)[1])
    if(ncol(filt_data) == 1){
      st_plot_final<-ggplot(filt_data, aes(x=!!var1))+
        geom_point()
    }
    st_plot_final
    
  }) # End of Student Plots
  
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
  
  # Server logic for 'Data Table' tab...
  
}

# --- Run the Application ---

shinyApp(ui = ui, server = server)
