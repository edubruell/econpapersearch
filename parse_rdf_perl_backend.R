pacman::p_load(here,
               tidyverse,
               jsonlite,
               glue, 
               arrow)

r_has_name <- function(.x, .name) {
  if (is.list(.x)) {
    return(.name %in% names(.x) || any(sapply(.x, r_has_name, .name = .name)))
  }
  FALSE
}

lib <- path.expand("~/.perl5/lib/perl5")
arch <- file.path(lib, "darwin-thread-multi-2level")  # matches your perl -V archname
Sys.setenv(PERL5LIB = paste(c(lib, arch), collapse = ":"))

#This file uses the REDIF-perl scripts by repec to auomatically parse to json which
#is easier to work with from R than raw redif files. It is slower than the original redif-parser
#I hacked together but more reliable.
#This needs REDIF-perl installed on the local machine! A minimal redjson is provided by the
#parse_redif_simple.pl in the same folder

parse_redif_perl <- function(path,
                             script_path = "parse_redif_simple.pl",
                             simplify = TRUE,
                             error_on_fail = FALSE) {
  stopifnot(file.exists(path), file.exists(script_path))
  
  result <- tryCatch({
    output <- system2("perl", args = c(script_path, shQuote(path)), stdout = TRUE, stderr = TRUE)
    
    # Find where JSON starts (first line that starts with '[')
    json_start <- which(stringr::str_detect(output, "^\\s*\\["))
    if (length(json_start) == 0) stop("No valid JSON found in output.")
    
    json_txt <- paste(output[json_start:length(output)], collapse = "\n")
    
    fromJSON(json_txt, simplifyVector = FALSE) 
  }, error = function(e) {
    if (error_on_fail) stop(e)
    warning(sprintf("Failed to parse: %s\n%s", path, e$message))
    NULL
  })
  
  return(result)
}

post_process_entry <- function(entry){
  handle <- entry$ID
  if(is.null(entry$ID)){return(NULL)}
  handle_entries <- str_split(handle,":")
  if(length(handle_entries[[1]])>3){
    archive      <-  handle_entries |> map_chr(2)
    journal_code <-  handle_entries |> map_chr(3)
  } else {
    warning("ENTRY INVALID SKIPPED")
    return(NULL)
  }
  
  if(!r_has_name(entry$author,"name")){
    warning("INVALID AUTHOR FIELD - SKIPPED")
    return(NULL)
  }
  
  authors_list <- entry$author |> 
    keep(~r_has_name(.x,"name")) |>
    map_chr(~{ name_field <- .x$name[[1]]
        if(is.null(.x$name[[1]])){
         name_field <- "" 
        } 
      name_field
      }) |> 
    list()
  
  authors_string <- unlist(authors_list) |> str_c(collapse="; ")
  title    <- entry$title[[1]]
  
  #Abstract (optional)
  abstract <- ""
  if(!is.null(entry$abstract)){ 
    abstract <- entry$abstract[[1]]
  }
  
  if(entry$TYPE=="ReDIF-Article 1.0"){
    #Authors, title year should be the minimum of necessary fields for article
    year     <- entry$year[[1]]
    is_series <- FALSE
  }
  if(entry$TYPE=="ReDIF-Paper 1.0"){
    cr <- entry$`creation-date`
    year <- str_sub(cr,1,4)
    is_series <- TRUE
  }
  if(entry$TYPE=="ReDIF-Chapter 1.0"){
    year     <- entry$year[[1]]
    is_series <- FALSE
  }
  #Files (optional)
  file <- tibble()
  if(!is.null(entry$file)){
    file <- entry$file |> 
      map_dfr(as_tibble) 
    
    file_fields <- colnames(file)
    file_fields <- file_fields[file_fields %in% c("format","url")]
    
    file|> 
      unnest(cols=all_of(file_fields)) |>
      select(all_of(file_fields))
  }
  #JEL Classification (optional )
  jel_list = ""
  jel_char = ""
  if(!is.null(entry$`classification-jel`)){
    jel_list <- str_split(entry$`classification-jel`,pattern = " ") |> unlist()
    jel_char <- entry$`classification-jel` |> unlist() |> str_c(collapse = " ")
  }
  #Bibliographic info 
  journal_internal <- ""
  if(!is.null(entry$journal)){
    journal_internal <- entry$journal[[1]]
  }
  volume <- ""
  if(!is.null(entry$volume)){
    volume <- entry$volume[[1]]
  }
  issue <- ""
  if(!is.null(entry$issue)){
    issue <- entry$issue[[1]]
  }
  pages <- ""
  if(!is.null(entry$pages)){
    pages <- entry$pages[[1]]
  }
  month <- ""
  if(!is.null(entry$month)){
    month <- entry$month[[1]]
  }
  doi <- ""
  if(!is.null(entry$doi)){
    doi <- entry$doi[[1]]
  }
  number <- ""
  if(!is.null(entry$number)){
    number <- entry$number[[1]]
  }
  
  # Generate BibTeX entry type and key
  entry_type <- if (is_series) "techreport" else "article"
  
  # Use first author's last name for citation key
  first_author <- authors_list[[1]][1] |> str_split(" ") |> map_chr(last)
  bib_key <- paste0(tolower(first_author), year)
  
  # Build BibTeX entry
  bib_tex <- glue::glue("@{entry_type}{{{bib_key},
  author = {{{str_replace_all(authors_string, '; ', ' and ')}}},
  title = {{{title}}},
  year = {{{year}}},
  {if(volume != '') glue('volume = {{{volume}}},') else ''}
  {if(issue != '') glue('number = {{{issue}}},') else ''}
  {if(pages != '') glue('pages = {{{pages}}},') else ''}
  {if(month != '') glue('month = {{{month}}},') else ''}
  {if(doi != '') glue('doi = {{{doi}}},') else ''}
  {if(is_series) glue('institution = {{{journal_code}}},') else glue('journal = {{{journal_internal}}},')}
  note = {{{handle}}}
}}") |> as.character()
  
  #Output as single-row tibble
  tibble(
    Handle = handle,
    archive = archive,
    journal_code = journal_code,
    authors = authors_list,
    authors_string = authors_string,
    title = title,
    abstract = abstract,
    year = year,
    is_series = is_series,
    file = list(file),
    jel_list = list(jel_list),
    jel_char = jel_char,
    volume = volume,
    issue = issue,
    pages = pages,
    month = month,
    doi = doi,
    number = number,
    bib_tex = bib_tex
  )
}

#Find all redif files in our Repec archives
journal_base <- here("RePEc") |> 
  list.dirs(full.names = FALSE) |>
  keep(~str_detect(.x,"/")) |>
  discard(~{.x=="cpd/conf"})

redif_files <- journal_base |>
  map_dfr(~{
    archive <- str_split(.x, "/") |> map_chr(1)
    journal <- str_split(.x, "/") |> map_chr(2)
    tibble(
      archive = archive,
      journal_code = journal,
      repo_id = paste0(archive, "_", journal_code),
      file = list.files(here("RePEc", .x),
                        pattern = "\\.(redif|rdf)$",
                        full.names = TRUE)
    )
  })
  

# Create rds_archive folder if it doesn't exist
if (!dir.exists(here("rds_archivep"))) {
  dir.create(here("rds_archivep"))
}


redif_info <- redif_files |>
  mutate(
    redif_mtime = file.info(file)$mtime,
    rds_file    = here("rds_archivep", paste0(repo_id, ".rds")),
    rds_exists  = file.exists(rds_file),
    rds_mtime   = ifelse(rds_exists, file.info(rds_file)$mtime, as.POSIXct(0))
  ) |>
  mutate(
    needs_parse = (!rds_exists) | (redif_mtime > rds_mtime)
  )

redif_info <- redif_files |>
  mutate(
    redif_mtime = file.info(file)$mtime,
    rds_file    = here("rds_archivep", paste0(repo_id, ".rds")),
    rds_exists  = file.exists(rds_file),
    rds_mtime   = ifelse(rds_exists, file.info(rds_file)$mtime, as.POSIXct(0))
  ) |>
  mutate(
    needs_parse = (!rds_exists) | (redif_mtime > rds_mtime)
  )


  
updated_today <- fs::dir_info(here("rds_archivep")) |>
  filter(as_date(modification_time) == today()) |>#ymd("2025-09-14")) |>
  transmute(repo = str_remove(path,(here("rds_archivep")))|>
              str_remove("/") |>
              str_remove(".rds")) |>
  pull(repo)

paper_archives <- redif_info |>
  filter(needs_parse) |>   #Uncomment this line if working incrementally
  filter(!(repo_id %in% updated_today)) |>
  arrange() |>
  group_by(repo_id) |>
  group_split()


paper_archives |>
  walk(~{
    repo <- unique(.x$repo_id)
    paste0("Parsing Redif for: ",repo) |> cat("\n")
    .x$file |>
      map_dfr(~{
        paste0("-> File: ",.x) |> cat("\n")
        
        parsed_redif <- parse_redif_perl(.x) 
        
        parsed_redif |>
            map_dfr(post_process_entry)
        }) |>
      write_rds(here("rds_archivep",paste0(repo,".rds")))
  })


#parse_redif_perl("/Users/ebr/Seafile/Meine Bibliothek/git_projects/econpapersearch/RePEc/sae/woemps/10.1177_09500170251317407.rdf") %>%
#  .[1] |>
#  map_dfr(post_process_entry)
  
