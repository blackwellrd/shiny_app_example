library(shiny)
library(plotly)

# Load the organisation map
df_org_map <- read.csv('www/MAPPING_NHS_GEOGRAPHIES_2223.csv')
region_list <- df_org_map %>% arrange(REGION_NAME) %>% distinct(REGION_ODS_CODE) %>% .$REGION_ODS_CODE
names(region_list) <- df_org_map %>% arrange(REGION_NAME) %>% distinct(REGION_NAME) %>% .$REGION_NAME

# Load the prevalence data
df_prev <- read.csv('www/PREVALENCE_2223.csv')

# Define UI for application that draws a histogram
ui <- fluidPage(
    # Sidebar layout 
    sidebarLayout(
        sidebarPanel(
          selectInput(inputId = 'edtRegion', 
                      label = 'Region', 
                      choices = region_list,
                      selected = 1
                      ),
          selectInput(inputId = 'edtICB', 
                      label = 'ICB', 
                      choices = '',
                      selected = 1
          ),
          width = 2
        ),

        # Show a plot of the generated distribution
        mainPanel(
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  observeEvent(input$edtRegion,
          {
            icb_list <- df_org_map %>% 
              dplyr::filter(REGION_ODS_CODE==input$edtRegion) %>% 
              arrange(ICB_NAME) %>% 
              distinct(ICB_ODS_CODE) %>% 
              .$ICB_ODS_CODE
            
            names(icb_list) <- df_org_map %>% 
              dplyr::filter(REGION_ODS_CODE==input$edtRegion) %>% 
              arrange(ICB_NAME) %>% 
              distinct(ICB_NAME) %>% 
              .$ICB_NAME
            
            updateSelectInput(inputId = 'edtICB', choices = icb_list, selected = 1)
            
          })
}

# Run the application 
shinyApp(ui = ui, server = server)
