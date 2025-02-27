pacman::p_load(here,
               tidyverse)

`%nin%` <- Negate(`%in%`)

# Helper function to append new or repeated fields.
# If a field repeats, we just store it in a character vector.
append_value <- function(out, field_name, field_value) {
  if (!field_name %in% names(out)) {
    out[[field_name]] <- field_value
  } else {
    # Already exists, so we turn it into (or extend) a character vector.
    out[[field_name]] <- c(out[[field_name]], field_value)
  }
  out
}

is_new_field <- function(line) {
  # Match only if the line starts with a capital letter and a valid field name
  grepl('^[A-Z][A-Za-z-]+:\\s?', line)
}

parse_redif_record <- function(entry_lines) {
  initial <- list(out = list(), current_field = NULL)
  
  final_acc <- reduce(entry_lines, function(acc, line) {
    out <- acc$out
    current_field <- acc$current_field
    
    if (is_new_field(line)) {
      # Extract field name and value using a stricter regex.
      field_name <- sub('^([A-Z][A-Za-z-]+):\\s?(.*)$', '\\1', line)
      field_value <- sub('^([A-Z][A-Za-z-]+):\\s?(.*)$', '\\2', line)
      
      out <- append_value(out, field_name, field_value)
      current_field <- field_name
    } else {
      # Consider the line as a continuation only if a current field exists.
      if (!is.null(current_field)) {
        last_idx <- length(out[[current_field]])
        out[[current_field]][last_idx] <- paste0(out[[current_field]][last_idx], " ", trimws(line))
      }
    }
    
    list(out = out, current_field = current_field)
  }, .init = initial)
  
  final_acc$out
}

convert_record_to_tibble <- function(rec) {
  # Take only the first title if multiple are present
  if ("Title" %in% names(rec) && length(rec$Title) > 1) {
    rec$Title <- rec$Title[1]
  }
  
  if ("Keywords" %in% names(rec) && length(rec$Keywords) > 1) {
    rec$Keywords <- str_c(rec$Keywords,collapse = " ")
  }
  
  # Create row list, keeping individual values unwrapped
  row_list <- rec
  
  # Identify and unify "Author-" fields
  author_fields <- names(row_list)[str_detect(names(row_list), "^Author-")]
  if (length(author_fields) > 0) {
    row_list$Authors <- list(row_list[author_fields])
    row_list <- row_list[!names(row_list) %in% author_fields]
  }
  
  # Identify and unify "File-" fields
  file_fields <- names(row_list)[str_detect(names(row_list), "^File-")]
  if (length(file_fields) > 0) {
    row_list$Files <- list(as_tibble(row_list[file_fields]))
    row_list <- row_list[!names(row_list) %in% file_fields]
  } else {
    row_list$Files <- list(tibble())
  }
  # Convert to tibble, wrapping in list only when needed
  result <- tibble::tibble(!!!row_list)
  
  return(result)
}


decode_rdf <- function(rdf_raw, hint = character()) {
  # Fallback encodings:
  encodings <- c(hint, "windows-1252", "UTF-8", "UTF-16", "latin1")
  
  # Check for BOM (EF BB BF) to push "UTF-8" to the front:
  if (length(rdf_raw) >= 3 && 
      all(as.integer(rdf_raw[1:3]) == c(0xEF, 0xBB, 0xBF))) {
    # Remove the BOM so it won’t interfere
    rdf_raw <- rdf_raw[-(1:3)]
    encodings <- c("UTF-8", encodings)
  }
  
  # Convert raw to a naive string once
  # (R does not let us specify an encoding directly in rawToChar)
  rdf_raw <- rdf_raw[rdf_raw != 0]
  naive_text <- rawToChar(rdf_raw, multiple = FALSE)
  
  try_decode <- function(enc) {
    # 1. Re-interpret naive_text as if it were in `enc`, convert to UTF-8
    converted <- iconv(naive_text, from = enc, to = "UTF-8")
    # 2. If iconv() fails, we get NA
    if (is.na(converted)) return(NULL)
    # 3. Check if "template-type" is in the text (case-insensitive)
    if (!str_detect(str_to_lower(converted), "template-type")) {
      return(NULL)
    }
    converted
  }
  
  # Try each encoding until one works
  for (enc in encodings) {
    result <- try_decode(enc)
    if (!is.null(result)) {
      return(result)
    }
  }
  
  stop("Decoding Error: none of the candidate encodings produced valid ReDIF text.")
}


# Main function to parse a ReDIF file into a tibble.
parse_redif <- function(file_path) {
  message <- paste0("Parsing file: ", file_path)
  cat(message, "\n")
  
  # Read the file with the determined encoding.
  lines <- file_path |>
    read_file_raw() |>
    decode_rdf() |>
    #Get rid of comment lines
    str_replace_all(
      regex("^#.*\\n?", multiline = TRUE),
      ""
    ) |>
    #Fix line endings
    str_replace_all("\r","\r\n") |>
    read_lines()
  
  # Remove BOM from the first line if present.
  if (length(lines) > 0 && grepl("^\uFEFF", lines[1])) {
    lines[1] <- sub("^\uFEFF", "", lines[1])
  }
  
  # Identify record start positions (lines starting with "Template-Type:").
  template_idx <- which(grepl('^Template-Type:', lines, ignore.case = TRUE))
  template_idx <- c(template_idx, length(lines) + 1)
  
  # Extract each record and parse.
  records <- purrr::map(seq_along(template_idx)[-length(template_idx)], function(i) {
    start_line <- template_idx[i]
    end_line <- template_idx[i + 1] - 1
    entry_lines <- lines[start_line:end_line]
    parse_redif_record(entry_lines)
  })
  
  # Combine all records into a tibble.
  purrr::map_dfr(records, convert_record_to_tibble)
}


journal_base <- here("RePEc") |> 
  list.dirs(full.names = FALSE) |>
  keep(~str_detect(.x,"/"))


redif_files <- journal_base |> 
  map_dfr(~{
    journal <- str_split(.x,"/") |> map_chr(2)
    tibble(journal_code = journal,
           file = list.files(here("RePEc",.x), 
                             pattern = "\\.(redif|rdf)$", full.names = TRUE))
    
    })
  
redif_files |> 
  arrange(journal_code) |>
  #filter(str_detect(journal_code,"zewdip")) |>
  #filter(journal_code %nin% {here("rds_archive") |> dir() |>
  #       str_remove(".rds")}) |>
  group_by(journal_code) |>
  group_split() |>
  walk(~{
    journal <- unique(.x$journal_code)
    paste0("Parsing Redif for: ",journal) |> cat("\n")
    .x$file |>
      map_dfr(parse_redif) |>
      write_rds(here("rds_archive",paste0(journal,".rds")))
  })






