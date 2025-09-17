pacman::p_load(here,
               tidyverse,
               glue,
               tidyllm,
               duckdb)

#Dump a database to parquet
con <- dbConnect(duckdb(), dbdir = "articles.duckdb")
timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
output_path <- here::here("pqt", glue("articles_{timestamp}.parquet"))
dir.create(here::here("pqt"), showWarnings = FALSE)

# Export to parquet with timestamp in filename
dbExecute(con, glue("COPY articles TO '{output_path}' (FORMAT PARQUET)"))
dbDisconnect(con)

