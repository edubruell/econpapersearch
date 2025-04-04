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

#dbpath <- normalizePath("/srv/shiny-server/econpapersearch/articles_ollama.duckdb")
dbpath <- "articles_ollama.duckdb"

db_state <- file.info(dbpath)$mtime |> 
  as.character() |>
  str_extract("\\d{4}-\\d{2}-\\d{2}")

pool <- pool::dbPool(
  drv = duckdb::duckdb(dbpath),
  dbdir = "articles_ollama.duckdb",
  max_connections = 5
)

semantic_sort <- function(.query, .pool, .journal_filter, .min_year) {
  query_vec <- unlist(ollama_embedding(.query,.model="mxbai-embed-large")$embeddings)
  dim_vec <- length(query_vec)
  query_vec_str <- paste0(
    "CAST(ARRAY[", 
    paste(query_vec, collapse = ", "), 
    "] AS DOUBLE[", dim_vec, "])"
  )
  
  filter_conditions <- c()
  if (!is.null(.min_year) && is.numeric(.min_year)) {
    filter_conditions <- c(filter_conditions, sprintf("year >= %d", .min_year))
  }
  if (!is.null(.journal_filter) && length(.journal_filter) > 0) {
    categories_str <- paste(shQuote(.journal_filter), collapse = ", ")
    filter_conditions <- c(filter_conditions, sprintf("category IN (%s)", categories_str))
  }
  
  where_clause <- ""
  if (length(filter_conditions) > 0) {
    where_clause <- paste("WHERE", paste(filter_conditions, collapse = " AND "))
  }
  
  sql <- sprintf("
    SELECT *,
      array_cosine_similarity(embeddings::DOUBLE[%d], %s) AS similarity
    FROM articles
    %s
    ORDER BY similarity DESC
    ", dim_vec, query_vec_str, where_clause)
  
  pool::dbGetQuery(.pool, sql)
}


ui <- fluidPage(
  theme = shinytheme("flatly"),
  tags$head(
    tags$title("Semantic Paper Search"),
    tags$link(
      rel = "stylesheet",
      href = "https://cdn.datatables.net/buttons/2.3.6/css/buttons.bootstrap.min.css"
    ),
    tags$link(
      rel = "stylesheet",
      href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0/css/all.min.css"
    ),
    tags$link(rel = "icon", type = "image/x-icon", href = "favicon.ico"),
    tags$link(rel = "icon", type = "image/png", sizes = "16x16", href = "favicon-16x16.png"),
    tags$link(rel = "icon", type = "image/png", sizes = "32x32", href = "favicon-32x32.png"),
    tags$link(rel = "apple-touch-icon", sizes = "180x180", href = "apple-touch-icon.png"),
    tags$link(rel = "icon", sizes = "192x192", href = "android-chrome-192x192.png"),
    tags$link(rel = "icon", sizes = "512x512", href = "android-chrome-512x512.png"),
    tags$link(rel = "manifest", href = "site.webmanifest"),
    tags$link(
      rel = "stylesheet",
      href = "https://fonts.googleapis.com/css2?family=Josefin+Sans:wght@600;700&family=Lato:wght@300;400;600&display=swap"),
    tags$script(HTML("
      $(document).on('click', '.btn-bibtex', function() {
        var bibtex = $(this).data('bibtex-content');
        navigator.clipboard.writeText(bibtex).then(function() {
          alert('BibTeX entry copied to clipboard');
        }, function(err) {
          console.error('Copying failed: ', err);
        });
      });
    ")),
    tags$style(HTML("
      body, .btn, .form-control, .well {
        font-family: 'Lato', sans-serif !important;
      }

      h2 {
        font-family: 'Josefin Sans', sans-serif !important;
        font-size: 32px !important; 
        font-weight: 600 !important;
        color: #004f80 !important;
        letter-spacing: 1px;
      }

      table.dataTable td, table.dataTable th {
        vertical-align: top !important;
      }

      .btn-custom {
        background-color: #555 !important;
        color: #fff !important;
        border: 1px solid #444 !important;
        border-radius: 5px !important;
        padding: 6px 12px !important;
        font-size: 14px !important;
      }

      .btn-custom:hover {
        background-color: #777 !important;
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

      .btn-bibtex {
        font-size: 12px;
        padding: 3px 6px;
        background-color: #ddd;
        border: none;
        border-radius: 4px;
        color: #333;
        cursor: pointer;
        margin-top: 6px;
      }

      .btn-bibtex:hover {
        background-color: #ccc;
      }

      table.dataTable tbody td a {
        text-decoration: underline !important;
        color: #006ab3 !important;
        transition: color 0.2s ease-in-out;
      }

      table.dataTable tbody td a:hover {
        color: #0097ff !important;
      }

      table.dataTable tbody tr.selected a {
        color: #006ab3 !important;
      }

      table.dataTable tbody tr.selected a:hover {
        color: #0097ff !important;
      }

      table.dataTable tbody tr.selected {
        background-color: transparent !important;
      }
    "))
  ),
  titlePanel(
    div(
      tags$img(src = "logo_blue.png", height = "48px", style = "margin-right: 5px;"),
      tags$span("Semantic Paper Search", style = "font-family: 'Josefin Sans', sans-serif; font-size: 32px; font-weight: 600; color: #004f80;  letter-spacing: 1px;")
    )
  ),
  
  fluidRow(
    column(
      width = 3,
      style = "vertical-align: top;",
      wellPanel(
        h4("Enter Abstract Text"),
        textAreaInput("query", NULL, placeholder = "Paste your search text here...", rows = 8),
        selectInput("min_quantile", "Minimum Similarity Quantile",
                    choices = c("Top 0.5%" = "0.995", 
                                "Top 1%"   = "0.99", 
                                "Top 1.5%" = "0.985", 
                                "Top 2%"   = "0.98", 
                                "No restriction" = "none"),
                    selected = "none"),
        numericInput("min_year", "Minimum Year:", value = 1995, min = 1995, max = as.integer(format(Sys.Date(), "%Y"))),
        checkboxGroupInput("journal_filter", "Select Journal Categories:",
                           choices = c("Top 5 Journals", "General Interest", "AEJs", 
                                       "Top Field Journals (A)", "Second in Field Journals (B)",
                                       "Other Journals", "Working Paper Series"),
                           selected = c("Top 5 Journals", "General Interest", "AEJs", 
                                        "Top Field Journals (A)", "Second in Field Journals (B)")),
        actionButton("search", "Search", class = "btn-custom")
      )
    ),
    column(
      width = 9,
      style = "vertical-align: top;",
      withSpinner(DTOutput("results"), type = 5, color = "#006ab3")
    )
  ),
  div(
    style = "position: relative; margin-top: 20px; padding: 10px; background-color: #f8f9fa; border-top: 1px solid #dee2e6; text-align: right; font-size: 12px; color: #6c757d;",
    HTML(sprintf("&copy; Eduard Brüll | Database state: %s", db_state))
  )
)

server <- function(input, output, session) {
  result <- eventReactive(input$search, {
    req(input$query)
    res <- semantic_sort(input$query, pool, input$journal_filter, input$min_year)
    
    if (input$min_quantile != "none") {
      quant_value <- as.numeric(input$min_quantile)
      threshold <- quantile(res$similarity, quant_value)
      res <- res |> filter(similarity > threshold)
    }
    
    res |> mutate(
      Similarity = paste0(
        round(similarity, 4), "<br/>",
        "<button class='btn-bibtex' data-bibtex-content='", 
        htmltools::htmlEscape(bib_tex), "'>",
        "<i class='fa fa-copy'></i>BibTeX</button>"
      ),
      pdf_link = paste0(
        "<a href='", url, "' target='_blank' class='dt-link'>",
        "<i class='fa fa-download' style='color: black; margin-right: 5px;'></i>",
        title,
        "</a>"
      ),
      abstract = stringr::str_wrap(abstract, width = 80)
    ) |> select(
      Similarity,
      Year = year,
      Authors = authors,
      Journal = journal,
      Title = pdf_link,
      Abstract = abstract
    )
  })
  
  output$results <- renderDT({
    req(result())
    datatable(
      result(),
      escape = FALSE,
      selection = 'none',
      extensions = 'Buttons',
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
