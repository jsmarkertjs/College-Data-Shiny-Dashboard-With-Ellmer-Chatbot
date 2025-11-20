
library(shiny)
library(readr)
library(dplyr)


full_data<-read_csv("./data/college_scorecard_clean_ug.csv")
full_data |> 
  mutate(ADM_RATE=ADM_RATE*100) |> 
  mutate(UGDS_ASIAN=UGDS_ASIAN*100) |> 
  mutate(UGDS_BLACK=UGDS_BLACK*100) |> 
  mutate(UGDS_HISP=UGDS_HISP*100) |> 
  mutate(UGDS_WHITE=UGDS_WHITE*100) |> 
  mutate(FEMALE=FEMALE*100) |> 
  mutate(FIRST_GEN=FIRST_GEN*100)->full_data

ui <- navbarPage(
  title = "College Education Analysis",
 
  #INSERT THEME HERE
  
  # --- Main Tab 1: Intro ---
  tabPanel("Intro",
           h2("Introduction Page"),
           p("Content for intro page.")
           
  ),
  
  # --- Main Tab 2: Students and Parents ---
  tabPanel("Students and Parents",
           tabsetPanel(
             
             
             
             
             # Sub-tab 2a - shae's section
             tabPanel("Variable Exploration",
                      sidebarLayout(
                        sidebarPanel(
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
                          )
                        ), #End of sidebar Panel Student Exporatory Section
                        mainPanel(
                      
                      
                      h4("Variable Exploration"),
                      p("Content here."),
                      tableOutput("st_df")
                      
                      # Add stuff for sub-tab here
                        ), #End up Main Panel Student Exploratory Section
             )),
             
             
             
             
             # Sub-tab 2b
             tabPanel("Best Value Index",
                      h3("Best Value Index"),
                      p("Content here.")
                      # Add stuff for sub-tab here
             ),
             
             # Sub-tab 2c
             tabPanel("Map",
                      h3("Map"),
                      p("Content here.")
                      # Add map output here
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
# The server is where all the calculations and plot/table generation happens.
# For now, it's empty because we are just building the layout.
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
    
    return(full_data)
    })# End of reactive df
  
  output$st_df<-renderTable({
    
    head(filt_st_data()) 
  })
  
  
  # Server logic for 'Researchers and Administrators' tab...
  
  # Server logic for 'Data Table' tab...
  
}

# --- Run the Application ---

shinyApp(ui = ui, server = server)
