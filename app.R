# app.R
pacman::p_load(shiny,
               tidyverse,
               DT,
               knitr,
               kableExtra,
               tidyllm,
               shinythemes,
               arrow,
               here,
               shinycssloaders)

# Load your embedded articles dataset
embedded_collection <- here("embedded_articles") |>
  open_dataset() |>
  collect()  |> 
  mutate(category = case_when(
  journal %in% c("American Economic Review",                          
                 "American Economic Review: Insights",  
                 "Econometrica",   
                 "Review of Economic Studies",   
                 "Journal of Political Economy (JPE)",
                 "Quarterly Journal of Economics"                    
  ) ~ "Top 5 Journals",
  
  journal %in% c("American Economic Journal: Applied Economics",      
                 "American Economic Journal: Economic Policy",        
                 "American Economic Journal: Macroeconomics",         
                 "American Economic Journal: Microeconomics",
                 "American Economic Review: P&P",
                 "Journal of Economic Literature",                   
                 "Journal of Economic Perspectives") ~ "AEJs",
  
  journal %in% c("Review of Economics and Statistics (RESTAT)",       
                 "The Economic Journal",    
                 "Economic Policy", 
                 "Journal of the European Economic Association",
                 "Journal of Financial Economics (JFE)") ~ "General Interest",
  
  journal %in% c(  "Journal of Economic Theory (JET)", 
                   "Journal of Labor Economics (JOLE)",                 
                   "Journal of Economic History",                       
                   "Journal of Environmental Economics and Management", 
                   "Journal of Health Economics",                       
                   "Journal of Human Resources",                        
                   "Journal of Public Economics",                       
                   "Journal of Urban Economics",  
                   "Research Policy",  
                   "Econometrics Journal") ~ "Top Field Journals (A)",
  
  journal %in% c("Journal of Population Economics",                   
                 "Labour Economics",                                  
                 "Journal of Economic Growth",                        
                 "Energy Economics",                                  
                 "European Economic Review",                          
                 "Journal of Applied Econometrics",                   
                 "Journal of Econometrics",                           
                 "Journal of Economic Behavior & Organization (JEBO)",
                 "Journal of Economic Geography" ) ~ "Second in Field Journals (B)",
  
  journal %in% c("CESifo Working Paper Series",                       
                 "Discussion Papers of DIW Berlin", 
                 "ZEW Discussion Papers", 
                 "SOEPpapers",                                        
                 "NBER Working Papers",
                 "IZA Discussion Papers") ~   "Working Paper Series",                             
  
  TRUE ~ "Second in Field Journals (B)"
)) |>
  left_join(read_parquet(here("journal_urls.parquet")),by = "Handle")



# Define available journals from the dataset (assuming 'series' holds journal info)
journals <- sort(unique(embedded_collection$journal))

# Define a function to calculate cosine similarity
cosine_similarity <- function(vec1, vec2) {
  sum(vec1 * vec2) / (sqrt(sum(vec1^2)) * sqrt(sum(vec2^2)))
}

# Sort articles by semantic similarity
semantic_sort <- function(.query, .embedded_collection) {
  query_embedding <- ollama_embedding(.query, .model = "mxbai-embed-large")
  
  .embedded_collection %>%
    mutate(similarity = map_dbl(embeddings, 
                                ~cosine_similarity(unlist(query_embedding$embeddings), unlist(.x)))) %>%
    arrange(desc(similarity))
}

ui <- fluidPage(
  theme = shinytheme("flatly"),
  tags$head(
    tags$link(
      rel = "stylesheet",
      href = "https://cdn.datatables.net/buttons/2.3.6/css/buttons.bootstrap.min.css"
    ),
    tags$style(HTML("
      table.dataTable td, table.dataTable th {
        vertical-align: top !important;
      }
      .btn-custom {
        background-color: #555 !important; /* Dark grey */
        color: #fff !important;
        border: 1px solid #444 !important;
        border-radius: 5px !important;   /* Rounded corners */
        padding: 6px 12px !important;
        font-size: 14px !important;
      }
      .btn-custom:hover {
        background-color: #777 !important; /* Lighter on hover */
        color: #fff !important;
        border-color: #555 !important;
      }
      .dt-button.btn-custom {
        background-color: #555 !important;
        color: #fff !important;
        border: 1px solid #444 !important;
        border-radius: 5px !important;
        padding: 6px 12px !important;
        font-size: 14px !important;
      }
      .dt-button.btn-custom:hover {
        background-color: #777 !important;
        color: #fff !important;
        border-color: #555 !important;
      }
    "))
  ),
  titlePanel("Semantic Article Search"),
  
  fluidRow(
    column(
      width = 3,
      style = "vertical-align: top;",
      wellPanel(
        h4("Enter Abstract Text"),
        textAreaInput(
          inputId = "query",
          label = NULL,  
          placeholder = "Paste your search text here...",
          rows = 8,
          width = "100%"
        ),
        selectInput(
          inputId = "min_quantile",
          label = "Minimum Similarity Quantile",
          choices = c("Top 0.5%" = "0.995", 
                      "Top 1%"   = "0.99", 
                      "Top 1.5%" = "0.985", 
                      "Top 2%"   = "0.98", 
                      "No restriction" = "none"),
          selected = "none"
        ),
        numericInput(
          inputId = "min_year",
          label = "Minimum Year:",
          value = 1995,
          min = 1995,
          max = as.integer(format(Sys.Date(), "%Y")),
          step = 1
        ),
        checkboxGroupInput(
          inputId = "journal_filter",
          label = "Select Journal Categories:",
          choices = c("Top 5 Journals", 
                      "General Interest",
                      "AEJs", 
                      "Top Field Journals (A)", 
                      "Second in Field Journals (B)",
                      "Working Paper Series"),
          selected = c("Top 5 Journals", 
                       "General Interest",
                       "AEJs", 
                       "Top Field Journals (A)", 
                       "Second in Field Journals (B)")
        ),
        actionButton("search", "Search", class = "btn-custom")
      )
    ),
    column(
      width = 9,
      style = "vertical-align: top;",
      withSpinner(DTOutput("results"), type = 6, color = "#555")
    )
  )
)

server <- function(input, output, session) {
  
  # Trigger the search when the button is clicked
  result <- eventReactive(input$search, {
    req(input$query)
    res <- semantic_sort(input$query, embedded_collection |> 
                           filter(category %in% input$journal_filter,
                                  year >= input$min_year))
    
    # Apply similarity quantile filtering if a restriction is set
    if (input$min_quantile != "none") {
      quant_value <- as.numeric(input$min_quantile)
      threshold <- quantile(res$similarity, quant_value)
      res <- res |> filter(similarity > threshold)
    }
    
    res |>
      mutate(
        pdf_link = paste0("<a href='", url, "' target='_blank'>",title,"</a>"),
        abstract = stringr::str_wrap(abstract, width = 80),
        similarity = round(similarity, 4)
      ) |>
      select(Similarity= similarity, 
             Year=year, 
             Authors = authors, 
             Journal = journal,
             Title = pdf_link, 
             Abstract = abstract)
  })
  
  output$results <- renderDT({
    req(result())
    df <- result()
    
    datatable(
      df,
      escape = FALSE,  
      extensions = c('Buttons'),
      options = list(
        dom = 'Bfrtip',  
        buttons = list(
          list(extend = 'copy', className = 'btn-custom'),
          list(extend = 'excel', className = 'btn-custom')
        ),
        pageLength = 20,
        autoWidth = TRUE,
        columnDefs = list(
          list(className = 'dt-top', targets = '_all'),
          list(width = '40%', targets = 'Abstract')
        ),
      language = list(search = "Search in results:")
      ),
      class = "table table-striped table-hover"
    )
  })
  
}

shinyApp(ui, server)
