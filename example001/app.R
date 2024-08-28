library(shiny)
library(tidyverse)
library(plotly)
library(conflicted)

# Load the files outside the main app functions
#df_org_map <- read.csv('example001/www/MAPPING_NHS_GEOGRAPHIES_2223.csv')
df_org_map <- read.csv('www/MAPPING_NHS_GEOGRAPHIES_2223.csv')

# Create the region list
df_regions <- df_org_map %>% distinct(REGION_ODS_CODE, REGION_NAME) %>% arrange(REGION_NAME)
region_list <- df_regions$REGION_ODS_CODE
names(region_list) <- df_regions$REGION_NAME

# Load the prevalence data
#df_prev <- read.csv('example001/www/PREVALENCE_2223.csv')
df_prev <- read.csv('www/PREVALENCE_2223.csv')

# Create the register list
register_list <- df_prev %>% distinct(GROUP_CODE) %>% arrange(GROUP_CODE) %>% .$GROUP_CODE

# Define UI for application that allows selection of Region then ICB 
ui <- fluidPage(
  # REMOVE COMMENT ON THE NEXT LINE TO SEE STYLING IMPACT
  # tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "dark_mode.css")),

  # Sidebar layout 
  sidebarLayout(
      sidebarPanel(
        radioButtons(inputId = 'radOrgLevel', label = 'Org Level', choices = c('Practice', 'PCN')),
        selectInput(inputId = 'edtRegion', label = 'Region', choices = region_list),
        selectInput(inputId = 'edtICB', label = 'ICB', choices = ''),
        selectInput(inputId = 'edtRegister', label = 'Register', choices = register_list),
        width = 3
      ),
      mainPanel(
        plotlyOutput(outputId = 'pltPrevalence', width = 1400, height = 800)
      )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  # Create our reactive dataset
  rvData <- reactive(
    {
      if(input$radOrgLevel=='Practice'){
        # Practice Level
        df_data <- df_prev %>% 
          left_join(df_org_map %>% select(PRACTICE_CODE, PRACTICE_NAME, ICB_ODS_CODE), by = 'PRACTICE_CODE') %>%
          dplyr::filter(ICB_ODS_CODE==input$edtICB & GROUP_CODE==input$edtRegister) %>%
          mutate(
            ORG_CODE = as.factor(PRACTICE_CODE),
            ORG_NAME = PRACTICE_NAME,
            GROUP_CODE,
            NUMERATOR = REGISTER,
            DENOMINATOR = PRACTICE_LIST_SIZE,
            PREVALENCE = NUMERATOR/DENOMINATOR,
            SIZE_DECILE = as.factor(ntile(DENOMINATOR, n = 10)),
            LABEL = sprintf('[%s] - %s<br>%s Prevalence: %.2f%% (%d / %d)', ORG_CODE, ORG_NAME, GROUP_CODE, PREVALENCE*100, NUMERATOR, DENOMINATOR)
          )
      } else {
        # PCN Level
        df_data <- df_prev %>% 
          left_join(df_org_map %>% select(PRACTICE_CODE, PCN_ODS_CODE, PCN_NAME, ICB_ODS_CODE), by = 'PRACTICE_CODE') %>%
          dplyr::filter(ICB_ODS_CODE==input$edtICB & GROUP_CODE==input$edtRegister) %>%
          group_by(PCN_ODS_CODE, PCN_NAME, GROUP_CODE) %>%
          summarise(REGISTER = sum(REGISTER, na.rm = TRUE), 
                    PRACTICE_LIST_SIZE  = sum(PRACTICE_LIST_SIZE, na.rm = TRUE),
                    .groups = 'keep') %>%
          ungroup() %>%
          mutate(
            ORG_CODE = as.factor(PCN_ODS_CODE),
            ORG_NAME = PCN_NAME,
            GROUP_CODE,
            NUMERATOR = REGISTER,
            DENOMINATOR = PRACTICE_LIST_SIZE,
            PREVALENCE = NUMERATOR/DENOMINATOR,
            SIZE_DECILE = as.factor(ntile(DENOMINATOR, n = 10)),
            LABEL = sprintf('[%s] - %s<br>%s Prevalence: %.2f%% (%d / %d)', ORG_CODE, ORG_NAME, GROUP_CODE, PREVALENCE*100, NUMERATOR, DENOMINATOR)
          )
      }
      return(df_data)
    }
  )
  
  observeEvent(
    input$edtRegion,
    {
      df_icb <- df_org_map %>% 
        dplyr::filter(REGION_ODS_CODE==input$edtRegion) %>%
        distinct(ICB_ODS_CODE, ICB_NAME) %>%
        arrange(ICB_NAME)
      icb_list <- df_icb$ICB_ODS_CODE
      names(icb_list) <- gsub('Integrated Care Board', 'ICB', df_icb$ICB_NAME)
      updateSelectInput(inputId = 'edtICB', choices = icb_list)
    }
  )

  output$pltPrevalence <- renderPlotly({
    pal <- colorRampPalette(c('#ece7f2','#023858'))(10)
    plt <- plot_ly(data = rvData()) %>%
      add_trace(type = 'bar',
                x = ~fct_reorder(ORG_CODE, desc(PREVALENCE)),
                y = ~PREVALENCE,
                color = ~SIZE_DECILE,
                colors = pal,
                hoverinfo = 'text',
                hovertext = ~LABEL
                ) %>%
      plotly::layout(
        title = sprintf('<br>%s Prevalance for %ss in %s', 
                        input$edtRegister, 
                        input$radOrgLevel, 
                        df_org_map %>% 
                          distinct(ICB_ODS_CODE, ICB_NAME) %>% 
                          dplyr::filter(ICB_ODS_CODE==input$edtICB) %>%
                          .$ICB_NAME),
        xaxis = list(title = input$radOrgLevel),
        yaxis = list(title = 'Prevalence', tickformat = '.1%'))
    })
    
  session$onSessionEnded(stopApp)
>>>>>>> ca9bdf47346b1d35fefaea297369fe707c032403
}

# Run the application 
shinyApp(ui = ui, server = server)
