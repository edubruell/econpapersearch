# app.R
pacman::p_load(duckdb,
               pool,
               shiny,
               tidyverse,
               DT,
               knitr,
               kableExtra,
               tidyllm,
               shinythemes,
               here,
               shinycssloaders)

# Connection pool for better concurrent access
pool <- pool::dbPool(
  drv = duckdb::duckdb(),
  dbdir = "articles.duckdb",
  max_connections = 5
)

# Duckdb semantic sort using internal array functions
semantic_sort <- function(.query, .pool, .journal_filter, .min_year) {
  # Embed the query and extract the embedding vector
  query_vec <- unlist(ollama_embedding(.query, .model = "mxbai-embed-large")$embeddings)
  
  # Determine the dimension of the embedding vector
  dim_vec <- length(query_vec)
  
  # Format the query vector as a fixed-length DOUBLE array literal
  query_vec_str <- paste0(
    "CAST(ARRAY[", 
    paste(query_vec, collapse = ", "), 
    "] AS DOUBLE[", dim_vec, "])"
  )
  
  # Build additional filter conditions
  filter_conditions <- c()
  if (!is.null(.min_year) && is.numeric(.min_year)) {
    filter_conditions <- c(filter_conditions, sprintf("year >= %d", .min_year))
  }
  if (!is.null(.journal_filter) && length(.journal_filter) > 0) {
    # Format the journal filter: assuming the column is called "journal_category"
    categories_str <- paste(shQuote(.journal_filter), collapse = ", ")
    filter_conditions <- c(filter_conditions, sprintf("category IN (%s)", categories_str))
  }
  
  # Combine filters into a WHERE clause if needed
  where_clause <- ""
  if (length(filter_conditions) > 0) {
    where_clause <- paste("WHERE", paste(filter_conditions, collapse = " AND "))
  }
  
  # Build the full SQL query string.
  # Note: stored embeddings are cast to a fixed-length DOUBLE array to match the query vector.
  sql <- sprintf("
    SELECT *,
      array_cosine_similarity(embeddings::DOUBLE[%d], %s) AS similarity
    FROM articles
    %s
    ORDER BY similarity DESC
    ", dim_vec, query_vec_str, where_clause)
  
  # Execute and return the query result
  pool::dbGetQuery(.pool, sql)
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
      withSpinner(DTOutput("results"), type = 6)
    )
  )
)

server <- function(input, output, session) {
  
  # Trigger the search when the button is clicked
  result <- eventReactive(input$search, {
    req(input$query)
    res <- semantic_sort(input$query,
                         pool,
                         input$journal_filter, 
                         input$min_year)
    
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
