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
journal_list <- read_csv(here::here("journals.csv")) |> 
  distinct(journal=long_name, category)

journals <- journal_list$journal

embedded_collection <- here("embedded_articles") |>
  open_dataset() |>
  collect()  |> 
  left_join(journal_list, by = "journal") |>
  left_join(read_parquet(here("journal_urls.parquet")), by = "Handle")

# Define the second semantic sort function using matrix operations
semantic_sort <- function(.query, .embedded_collection) {
  query_vec <- unlist(ollama_embedding(.query, .model = "mxbai-embed-large")$embeddings)
  query_norm <- sqrt(sum(query_vec^2))
  
  embedding_mat <- do.call(rbind, lapply(.embedded_collection$embeddings, unlist))
  
  row_norms <- sqrt(rowSums(embedding_mat^2))
  dot_products <- embedding_mat %*% query_vec
  
  similarities <- as.vector(dot_products) / (row_norms * query_norm)
  
  .embedded_collection %>%
    mutate(similarity = similarities) %>%
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
