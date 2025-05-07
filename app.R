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

#dbpath <- normalizePath("/srv/shiny-server/econpapersearch/articles_ollama_vss.duckdb")
dbpath <- "articles.duckdb"

db_state <- file.info(dbpath)$mtime |> 
  as.character() |>
  str_extract("\\d{4}-\\d{2}-\\d{2}")

pool <- pool::dbPool(
  drv = duckdb::duckdb(dbpath),
  dbdir = "articles.duckdb",
  max_connections = 5
)

# Warm up one connection to load extensions into the session's cache
con <- poolCheckout(pool)
DBI::dbExecute(con, "LOAD vss;")
DBI::dbExecute(con, "SET hnsw_enable_experimental_persistence=true;")
DBI::dbExecute(con, "SET max_expression_depth TO 2000;")
poolReturn(con)

semantic_sort <- function(.query, .pool, .journal_filter = NULL, .min_year = NULL, .max_k = 100) {
  query_vec <- unlist(ollama_embedding(.query, .model = "mxbai-embed-large")$embeddings)
  
  filters <- c()
  if (!is.null(.min_year)) filters <- c(filters, sprintf("a.year >= %d", .min_year))
  if (!is.null(.journal_filter) && length(.journal_filter) > 0) {
    cats <- paste(shQuote(.journal_filter), collapse = ", ")
    filters <- c(filters, sprintf("a.category IN (%s)", cats))
  }
  where_clause <- if (length(filters)) paste("WHERE", paste(filters, collapse = " AND ")) else ""
  
  sql <- sprintf("
    SELECT a.title, a.year, a.authors, a.journal, a.category, a.url, a.bib_tex, a.abstract,
           array_cosine_distance(a.embeddings, ?::FLOAT[1024]) AS similarity
    FROM articles a
    %s
    ORDER BY similarity ASC
    LIMIT ?
  ", where_clause)
  
  con <- poolCheckout(.pool)
  DBI::dbExecute(con, "LOAD vss;")
  DBI::dbExecute(con, "SET hnsw_enable_experimental_persistence = true;")
  
  stmt <- DBI::dbSendQuery(con, sql)
  DBI::dbBind(stmt, list(list(query_vec), .max_k)) 
  res <- DBI::dbFetch(stmt)                          
  DBI::dbClearResult(stmt)
  poolReturn(con)
  
  res
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
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
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
        numericInput("max_k", "Search result limit:", value = 200, min = 1, max = 10000),
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
    res <- semantic_sort(input$query, pool, input$journal_filter, input$min_year,input$max_k)
    

    res |> mutate(
      Similarity = paste0(
        round(1-similarity, 4), "<br/>",
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
      options = list(
        dom = 'Bfrtip',
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
