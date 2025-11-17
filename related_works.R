pacman::p_load(here,
               tidyverse,
               glue,
               duckdb)

con <- dbConnect(duckdb(), dbdir = "articles.duckdb")
dbExecute(con, "LOAD vss;")
processed_handles <- dbGetQuery(con, "SELECT Handle FROM articles")$Handle


# 1. Sync cdp/conf to local archive
#---------------------------------------------------------------

sync_repec_cpd_conf <- function(
    dest_root = here::here("RePEc"),
    rsync_bin = "/opt/homebrew/bin/rsync"
) {
  if (!file.exists(rsync_bin)) rsync_bin <- "rsync"
  
  src  <- "rsync.repec.org::RePEc-ReDIF/cpd/conf/"
  dest <- file.path(dest_root, "cpd", "conf")
  
  dir.create(dest, recursive = TRUE, showWarnings = FALSE)
  dest <- normalizePath(dest, winslash = "/", mustWork = FALSE)
  
  withr::with_dir(dest, {
    args <- c("-av", "-s", "--delete", "--contimeout=20", src, "./")
    status <- system2(rsync_bin, args)
    if (status != 0) stop("rsync failed with status ", status)
  })
  
  invisible(dest)
}

#Sync the related works archive
sync_repec_cpd_conf()

# 2. Parse relatedworks.dat using a perl script
#---------------------------------------------------------------

parse_relatedworks_perl <- function(path,
                                    script_path = "parse_related_works.pl",
                                    error_on_fail = FALSE) {
  stopifnot(file.exists(path), file.exists(script_path))
  
  result <- tryCatch({
    output <- system2("perl", args = c(script_path, shQuote(path)),
                      stdout = TRUE, stderr = TRUE)
    
    # Find JSON start
    json_start <- which(stringr::str_detect(output, "^\\s*\\{"))
    if (length(json_start) == 0)
      stop("No JSON object found in parser output.")
    
    json_txt <- paste(output[json_start:length(output)], collapse = "\n")
    
    jsonlite::fromJSON(json_txt, simplifyVector = FALSE)
    
  }, error = function(e) {
    if (error_on_fail) stop(e)
    warning(sprintf("Failed to parse: %s\n%s", path, e$message))
    NULL
  })
  
  return(result)
}

related_file <- here::here("RePEc","cpd","conf","relatedworks.dat")
rw <- parse_relatedworks_perl(related_file)

# 3. Parse relatedworks.dat using a perl script
#---------------------------------------------------------------

processed_handles_norm <- tolower(processed_handles)
rw_filtered <- rw[processed_handles_norm %in% names(rw)]

version_links <- purrr::imap_dfr(
  rw_filtered,
  function(relcats, src) {
    purrr::imap_dfr(
      relcats,
      function(targets, relcat) {
        tibble::tibble(
          source = src,
          target = names(targets),
          type = relcat
        )
      }
    )
  }
)


top_links <- version_links |>
  filter(type == "redif-paper") |>
  mutate(archive_journal_source = stringr::str_extract(source, "^repec:[^:]+:[^:]+") |> str_split(":"),
         archive_journal_target = stringr::str_extract(target, "^repec:[^:]+:[^:]+") |> str_split(":"),
         archive_target = map_chr(archive_journal_target,2),
         archive_source = map_chr(archive_journal_source, 2),
         journal_target = map_chr(archive_journal_target,3),
         journal_source = map_chr(archive_journal_source, 3)) |>
  select(-archive_journal_source,-archive_journal_target) |>
  left_join(read_csv(here("journals.csv"))  |>
            select(archive_source = archive, journal_source = journal,
            category_source = category, long_name_source= long_name), by=c("archive_source","journal_source")) |>
  filter(!is.na(category_source))
 
within_db_links <- version_links |>
  filter(type == "redif-paper") |>
  mutate(archive_journal_source = stringr::str_extract(source, "^repec:[^:]+:[^:]+") |> str_split(":"),
         archive_journal_target = stringr::str_extract(target, "^repec:[^:]+:[^:]+") |> str_split(":"),
         archive_target = map_chr(archive_journal_target,2),
         archive_source = map_chr(archive_journal_source, 2),
         journal_target = map_chr(archive_journal_target,3),
         journal_source = map_chr(archive_journal_source, 3)) |>
  select(-archive_journal_source,-archive_journal_target) |>
  left_join(read_csv(here("journals.csv"))  |>
              select(archive_source = archive, journal_source = journal,
                     category_source = category, long_name_source= long_name), by=c("archive_source","journal_source")) |>
  filter(!is.na(category_source)) |>
  left_join(read_csv(here("journals.csv"))  |>
              select(archive_target = archive, journal_target= journal,
                     category_target = category, long_name_target= long_name), by=c("archive_target","journal_target")) |>
  filter(category_source != "Working Paper Series") |>
  filter(!is.na(category_target)) |>
  select(source,target) |>
  mutate(across(everything(), ~str_replace(.x,"repec","RePEc")))

  
within_db_links |>
  filter((source %in% processed_handles)) |>
  write_parquet(here("rw_pqt","links.parquet"))



