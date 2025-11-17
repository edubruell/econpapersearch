pacman::p_load(plumber,
               DBI,
               pool,
               duckdb,
               tidyverse,
               tidyllm,
               stringr)

dbpath <- "articles.duckdb"

pool <- pool::dbPool(
  drv = duckdb::duckdb(),
  dbdir = dbpath,
  max_connections = 5
)

# Warm-up
con <- poolCheckout(pool)
DBI::dbExecute(con, "LOAD vss;")
DBI::dbExecute(con, "SET hnsw_enable_experimental_persistence=true;")
DBI::dbExecute(con, "SET max_expression_depth TO 2000;")
poolReturn(con)



# ============ CORE SEARCH FUNCTION ============ #

semantic_sort_api <- function(.query,
                              .pool,
                              .journal_filter = NULL,
                              .journal_name = NULL,
                              .min_year = NULL,
                              .title_keyword = NULL,
                              .author_keyword = NULL,
                              .max_k = 100) {
  
  # helper for safe integer conversion
  safe_int <- function(x) {
    if (is.null(x) || length(x) == 0 || is.na(x) || x == "") return(NULL)
    as.integer(x)
  }
  
  .min_year <- safe_int(.min_year)
  .max_k    <- safe_int(.max_k)
  
  
  query_vec <- unlist(
    ollama_embedding(.query, .model = "mxbai-embed-large")$embeddings
  )
  
  con <- poolCheckout(.pool)
  
  filters <- c()
  
  # --- Year filter ---
  if (!is.null(.min_year)) {
    filters <- c(filters, sprintf("a.year >= %d", .min_year))
  }
  
  # --- Category filter ---
  if (!is.null(.journal_filter) && nchar(.journal_filter) > 0) {
    cats <- str_split(.journal_filter, ",", simplify = FALSE)[[1]] |> str_trim()
    cats_sql <- paste(shQuote(cats), collapse = ",")
    filters <- c(filters, sprintf("a.category IN (%s)", cats_sql))
  }
  
  # --- NEW: Journal name filter ---
  if (!is.null(.journal_name) && nchar(.journal_name) > 0) {
    journals <- str_split(.journal_name, ",", simplify = FALSE)[[1]] |> str_trim()
    
    # (title OR abstract OR journal) LIKE %keyword%
    journal_clauses <- sprintf(
      "LOWER(a.journal) LIKE LOWER('%%%s%%')",
      journals
    )
    
    filters <- c(filters, paste0("(", paste(journal_clauses, collapse = " OR "), ")"))
  }
  
  # --- Title keyword ---
  if (!is.null(.title_keyword) && nchar(.title_keyword) > 0) {
    filters <- c(filters,
                 sprintf("LOWER(a.title) LIKE LOWER('%%%s%%')", .title_keyword))
  }
  
  # --- Author keyword ---
  if (!is.null(.author_keyword) && nchar(.author_keyword) > 0) {
    filters <- c(filters,
                 sprintf("LOWER(a.authors) LIKE LOWER('%%%s%%')", .author_keyword))
  }
  
  where_clause <- if (length(filters)) {
    paste("WHERE", paste(filters, collapse = " AND "))
  } else ""
  
  sql <- sprintf("
    SELECT a.title, a.year, a.authors, a.journal, a.category, a.url,
           a.bib_tex, a.abstract,
           array_cosine_distance(a.embeddings, ?::FLOAT[1024]) AS similarity
    FROM articles a
    %s
    ORDER BY similarity ASC
    LIMIT ?
  ", where_clause)
  
  DBI::dbExecute(con, "LOAD vss;")
  stmt <- DBI::dbSendQuery(con, sql)
  DBI::dbBind(stmt, list(list(query_vec), .max_k))
  
  res <- DBI::dbFetch(stmt)
  DBI::dbClearResult(stmt)
  poolReturn(con)
  
  res
}

# ============ SMALL STATS FUNCTIONS ============ #

get_journals_entries <- function(.pool) {
  con <- poolCheckout(.pool)
  res <- DBI::dbGetQuery(con, "SELECT journal, COUNT(*) as n FROM articles GROUP BY journal ORDER BY journal") |> as_tibble()
  poolReturn(con)
  res
}


get_total_articles <- function(.pool) {
  con <- poolCheckout(.pool)
  res <- DBI::dbGetQuery(con, "SELECT COUNT(*) as n FROM articles")
  poolReturn(con)
  res$n
}

get_category_counts <- function(.pool) {
  con <- poolCheckout(.pool)
  res <- DBI::dbGetQuery(con, "SELECT category, COUNT(*) as n FROM articles GROUP BY category ORDER BY n DESC") |> as_tibble()
  poolReturn(con)
  res
}

get_last_updated <- function() {
  db_age <- file.info(dbpath)$mtime |> 
    as.character() |>
    str_extract("\\d{4}-\\d{2}-\\d{2}")
  db_age
}

# ================= PLUMBER API ================= #

#* @apiTitle Semantic Paper Search API
#* @apiDescription Vector search with deep filtering


#* @post /search
#* @param query Text query (embedded)
#* @param max_k Max results
#* @param min_year Minimum year
#* @param journal_filter Category filter
#* @param journal_name NEW: Comma-separated list of journal names or substrings
#* @param title_keyword Keyword in title
#* @param author_keyword Keyword in authors
function(query,
         max_k = 100,
         min_year = NULL,
         journal_filter = NULL,
         journal_name = NULL,
         title_keyword = NULL,
         author_keyword = NULL) {
  
  res <- semantic_sort_api(
    .query          = query,
    .pool           = pool,
    .journal_filter = journal_filter,
    .journal_name   = journal_name,
    .min_year       = min_year,
    .title_keyword  = title_keyword,
    .author_keyword = author_keyword,
    .max_k          = max_k
  )
  
  res$similarity_score <- round(1 - res$similarity, 5)
  res |> arrange(desc(similarity_score))
}


#* Get journal entry counts
#* @get /stats/journals
function() {
  get_journals_entries(pool)
}

#* Get total article count
#* @get /stats/total
function() {
  list(total_articles = get_total_articles(pool))
}

#* Get category counts
#* @get /stats/categories
function() {
  get_category_counts(pool)
}

#* Get last database update date
#* @get /stats/last_updated
function() {
  list(last_updated = get_last_updated())
}
