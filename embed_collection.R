pacman::p_load(here,
               tidyverse,
               glue,
               tidyllm,
               duckdb)

#dbExecute(con, "INSTALL vss;
#                LOAD vss;")


sys_parser <- "perl"
con <- dbConnect(duckdb(), dbdir = "articles.duckdb")
#Old database
#con2 <- dbConnect(duckdb(), dbdir = "articles_ollama.duckdb")
#df <- dbReadTable(con2, "articles")
#dbExecute(con, "INSTALL vss;")
dbExecute(con, "LOAD vss;")

# Handle the case where the articles table doesn't exist yet
if ("articles" %in% dbListTables(con)) {
  processed_handles <- dbGetQuery(con, "SELECT Handle FROM articles")$Handle
} else {
  # Create the articles table if it doesn't exist
  dbExecute(con, "
    CREATE TABLE articles (
      Handle VARCHAR,
      title VARCHAR,
      abstract VARCHAR,
      pages VARCHAR,
      vol VARCHAR,
      issue VARCHAR,
      number VARCHAR,
      archive VARCHAR,
      journal_code VARCHAR,
      year INTEGER,
      is_series BOOLEAN,
      journal VARCHAR,
      category VARCHAR,
      url VARCHAR,
      authors VARCHAR,
      bib_tex VARCHAR,
      embeddings FLOAT[1024]
    )
  ")
  processed_handles <- character(0)
}


# 1. Data Cleaning
#---------------------------------------------------------------

if(sys_parser == "rnative"){
  full_article_data <- here::here("rds_archive") |>
    dir() |>
    map_dfr(~read_rds(here("rds_archive",.x)))
  
  handle_codes_journals <- full_article_data |> 
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
    filter((year>=1995 & !is_series) | (year>=2010 & is_series))
  
  
  no_dup <- full_article_data |>
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
                          journal = long_name,
                          category), 
              by = c("archive","journal_code")) 
  
  
  authours_by_handle <- no_dup |>
    unnest(authors) |>
    unnest(authors) |>
    group_by(Handle) |>
    summarise(authors = str_c(authors,collapse="; ") |> str_trim())
  
  article_urls <- full_article_data |>
    filter(Handle %in% handle_codes_journals$Handle) |>
    group_by(Handle) |>
    slice(1) |>
    ungroup() |>
    transmute(Handle=str_trim(Handle),Files,num_files = map_dbl(Files,ncol)) |>
    unnest(Files) |>
    select(Handle,url = `File-URL`, format = `File-Format`) |>
    group_by(Handle) |>
    slice(1) |>
    ungroup() |>
    distinct(Handle,url) 
  
  cleaned_collection <- no_dup |>
    select(-authors) |>
    left_join(authours_by_handle, by="Handle") |>
    left_join(article_urls, by="Handle") |>
    filter(!is.na(abstract),nchar(abstract)>1)  |>
    group_by(Handle) |>
    slice(1) |>
    ungroup() |>
    mutate(bib_tex="")
  
}

if(sys_parser == "perl"){
  full_article_data <- here::here("rds_archivep") |>
    dir() |>
    map_dfr(~read_rds(here("rds_archivep",.x)))
  
  #Use only unique ids
  no_dup <- full_article_data |>
    group_by(Handle) |>
    slice(1) |>
    ungroup()
  
  collection_all_files <- no_dup |>
    select(Handle,
           title,
           abstract,
           pages,
           vol =volume,
           issue,
           number,
           archive,
           journal_code,
           year,
           is_series,
           authors = authors_string,
           bib_tex = bib_tex,
           file) |>
    #Journal Articles since 1995 and discussion paper series since 2010
    filter((year>=1995 & !is_series) | (year>=2010 & is_series)) |>
    #At least 100 characters per abstract
    filter(nchar(abstract)>100) |>
    left_join(read_csv(here::here("journals.csv")) |>
                transmute(archive,
                          journal_code = journal,
                          journal = long_name,
                          category), 
              by = c("archive","journal_code")) |>
    mutate(journal_bt = paste0("journal = {",journal,"}"),
           bib_tex = str_replace(bib_tex,"journal = \\{\\}",journal_bt),
           bib_tex = str_replace_all(bib_tex, "\\n\\s*\\n", "\n")) |>
    select(-journal_bt)
  
  article_urls <- collection_all_files |>
    select(Handle,file) |>
    unnest(file) |>
    unnest(cols=c("format","url")) |>
    group_by(Handle) |>
    slice(1) |>
    ungroup() |>
    select(Handle,url)
  
  cleaned_collection <- collection_all_files |>
    select(-file) |>
    left_join(article_urls, by="Handle") |>
    mutate(year = str_replace_all(year,"Forthcoming","2025"))
  
}

# 2. Embedding
#---------------------------------------------------------------


process_batch <- function(batch, con, num_batches){
  current_batch <- unique(batch$batch)
  glue("Processing batch {current_batch} of {num_batches}") |> cat("\n")
  
  # Generate embeddings
  emb_result <- batch$abstract |>
    ollama_embedding(.model="mxbai-embed-large")
  #mistral_embedding()
  
  # Combine with original data
  batch_with_embeddings <- batch |>
    bind_cols(emb_result |> select(-input))
  
  # Insert into DuckDB using dbplyr
  batch_with_embeddings |>
    copy_to(
      con, 
      df = _, 
      name = "temp_batch", 
      temporary = TRUE,
      overwrite = TRUE
    )
  
  dbExecute(con, "
    INSERT INTO articles 
    SELECT 
      Handle, title, abstract, pages, vol, issue, number, 
      archive, journal_code, year, is_series, journal, 
      category, url, authors, bib_tex, embeddings
    FROM temp_batch
  ")
  
  dbExecute(con, "DROP TABLE temp_batch")
}

to_embed <- cleaned_collection |>
  filter(!Handle %in% processed_handles)

if (nrow(to_embed) == 0) {
  cat("No new articles to process.\n")
  dbDisconnect(con)
  quit(save = "no")
}

# Process in batches
batches <- to_embed  |>
  group_by(batch = ceiling(row_number() / 50)) |>
  group_split() 

batches |>
  walk(~process_batch(.x, con, length(batches)))

# Create indexes if they don't exist
dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_year ON articles(year)")
dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_category ON articles(category)")
#Load vss and index the embeddings
dbExecute(con, "LOAD vss;")
dbExecute(con, "SET hnsw_enable_experimental_persistence=true;")
dbExecute(con, "DROP INDEX IF EXISTS idx_hnsw;")
dbExecute(con, "CREATE INDEX idx_hnsw ON articles USING HNSW (embeddings);")

#Verify indices exist
dbGetQuery(con, "
  SELECT *
  FROM duckdb_indexes()
  WHERE table_name = 'articles';
")

# Cleanup
dbDisconnect(con)




