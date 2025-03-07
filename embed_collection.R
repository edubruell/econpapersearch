pacman::p_load(here,
               tidyverse,
               glue,
               tidyllm,
               arrow)

# 1. Functions
#---------------------------------------------------------------

# Function to load existing embeddings and get processed Handles
load_processed_handles <- function() {
  if (!dir.exists(here("embedded_articles"))) {
    dir.create(here("embedded_articles"))
    return(character(0))
  }
  
  existing_files <- dir(here("embedded_articles"), pattern = "embedded_.*\\.parquet", full.names = TRUE)
  
  if (length(existing_files) == 0) {
    return(character(0))
  }
  
  map(existing_files, ~{
    read_parquet(.x) |>
      pull(Handle)
  }) |>
    unlist() |> 
    unique()
}

generate_abstract_embeddings <- function(.article_collection) {
  # Get already processed handles
  processed_handles <- load_processed_handles()
  
  # Filter out already processed articles
  new_articles <- .article_collection %>%
    filter(!(Handle %in% processed_handles))
  
  if (nrow(new_articles) == 0) {
    cat("No new articles to process.\n")
    return(invisible(NULL))
  }
  
  # Determine the next batch number
  next_batch <- 1
  existing_files <- dir(here("embedded_articles"), pattern = "embedded_.*\\.parquet")
  if (length(existing_files) > 0) {
    batch_numbers <- existing_files %>%
      str_extract("embedded_(\\d+)\\.parquet", group = 1) %>%
      as.numeric()
    next_batch <- max(batch_numbers) + 1
  }
  
  # Create batches for new articles
  embedding_batches <- new_articles |>
    mutate(batch = floor((row_number() - 1) / 50) + next_batch) %>%
    group_by(batch) |>
    group_split()
  
  n_batches <- length(embedding_batches)
  glue("Processing {n_batches} batches of 50 abstracts, starting from batch {next_batch}") |> cat("\n")
  
  embedding_batches |>
    walk(~{
      batch_number <- pull(.x, batch) |> unique() 
      tbl <- select(.x, -batch)
      
      glue("Generate Text Embeddings for Abstract Batch: {batch_number}") |> cat("\n")
      
      emb_tbl <- tbl$abstract |>
        ollama_embedding(.model = "mxbai-embed-large")
      
      tbl |>
        bind_cols(emb_tbl |> select(-input)) %>%
        write_parquet(here("embedded_articles", glue("embedded_{batch_number}.parquet")))
    })
}

# 2. Data Cleaning
#---------------------------------------------------------------

full_article_db <- here::here("rds_archive") |>
  dir() |>
  map_dfr(~read_rds(here("rds_archive",.x)))



handle_codes_journals <- full_article_db |> 
  distinct(Handle,cr=`Creation-Date`) |>
  transmute(Handle, cr, split = str_split(Handle,":")) |>
  mutate(lengths = lengths(split)) |>
  filter(lengths> 3) |>
  mutate(archive = split |> map_chr(2),
         journal_code = split |> map_chr(3),
         year = str_extract(Handle,":y:\\d{4}:") |>
           parse_number(),
         #Unfortunately WP series have no year field!
         #Discussion Paper series have their year in a date field
         cr = str_sub(cr,1,4) |>
           parse_number(),
         is_series = if_else(is.na(year),TRUE,FALSE),
         year = if_else(is.na(year),cr,year)) |>
  select(Handle,archive,journal_code,year,is_series) |>
  #Journal Articles since 1995 and discussion paper series since 2015
  filter((year>=1995 & !is_series) | (year>=2015 & is_series))

  
no_dup <- full_article_db |>
  filter(Handle %in% handle_codes_journals$Handle) |>
  group_by(Handle) |>
  slice(1) |>
  ungroup() |>
  select(-Year) |>
  filter(!is.na(Abstract)) |>
  transmute(Handle,title = Title, 
         abstract = Abstract,
         pages = Pages, 
         vol = Volume, 
         issue =Issue,
         number = Number,
         authors = Authors) |>
  left_join(handle_codes_journals, by = "Handle") |>
  mutate(across(where(is.character),str_trim),
         journal_code = str_to_lower(journal_code) |> str_trim()) |>
  left_join(read_csv(here::here("journals.csv")) |>
              transmute(archive,
                        journal_code = journal,
                        journal = long_name), 
            by = c("archive","journal_code")) 

authours_by_handle <- no_dup |>
  unnest(authors) |>
  unnest(authors) |>
  group_by(Handle) |>
  summarise(authors = str_c(authors,collapse="; ") |> str_trim())


cleaned_collection <- no_dup |>
  select(-authors) |>
  left_join(authours_by_handle, by="Handle") |>
  filter(!is.na(abstract),nchar(abstract)>1)

# Generate embeddings for the cleaned collection
cleaned_collection %>%
  generate_abstract_embeddings()

#Write out the urls
full_article_db |>
  transmute(Handle=str_trim(Handle),Files) |>
  unnest(Files) |>
  select(Handle,url = `File-URL`, format = `File-Format`) |>
  filter(format != "application/zip") |>
  group_by(Handle) |>
  slice(1) |>
  ungroup() |>
  distinct(Handle,url) |>
  write_parquet(here("journal_urls.parquet"))
