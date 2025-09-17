pacman::p_load(tidyverse,
               arrow,
               here,
               fs,
               duckdb,
               glue)

latest_pqt_archive <- here("pqt") |>
  dir_info() |>
  arrange(desc(modification_time)) |>
  slice(1) |>
  pull(path)

dbpath <- normalizePath("/srv/shiny-server/econpapersearch/articles_toc.duckdb")

#Recreate a database from a parquet dump
con <- dbConnect(duckdb(), dbdir = dbpath)
dbExecute(con, "LOAD vss;")

dbExecute(con, glue("
  DROP TABLE IF EXISTS articles;

  CREATE TABLE articles AS
  SELECT
    Handle,
    title,
    abstract,
    pages,
    vol,
    issue,
    number,
    archive,
    journal_code,
    year,
    is_series,
    journal,
    category,
    url,
    authors,
    bib_tex,
    embeddings::FLOAT[1024] AS embeddings
  FROM read_parquet('{latest_pqt_archive}');
"))


#What's in the restored table
dbGetQuery(con,"PRAGMA table_info('articles');")

# Re-Create indexes if they don't exist
dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_year ON articles(year)")
dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_category ON articles(category)")
#Load vss and index the embeddings
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