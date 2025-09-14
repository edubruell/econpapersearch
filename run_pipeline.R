#!/usr/bin/env Rscript

# econpapersearch: Main launcher script
# This script sequentially executes the three main preparation steps:
# 1. Sync RePEc FTP archive
# 2. Parse ReDIF files using the Perl backend
# 3. Generate embeddings and update the DuckDB database

# Load required packages
if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")
}
pacman::p_load(here, logger)

# Setup logging
log_appender(appender_file(here::here("logs", paste0("update_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".log"))))
log_info("Starting econpapersearch data update pipeline")

# Create logs directory if it doesn't exist
if (!dir.exists(here::here("logs"))) {
  dir.create(here::here("logs"), recursive = TRUE)
}

# Function to run a script and handle errors
run_step <- function(script_name, step_description) {
  log_info(paste("Starting", step_description))
  
  tryCatch({
    # Source the script in a clean environment
    env <- new.env()
    source(here::here(script_name), local = env)
    log_success(paste("Successfully completed", step_description))
    TRUE
  }, error = function(e) {
    log_error(paste("Error in", step_description, ":", e$message))
    FALSE
  })
}

# Run the three main steps in sequence
log_info("=== STEP 1: RePEc FTP SYNCHRONIZATION ===")
sync_success <- run_step("sync_rsync_archive.R", "RePEc rsync synchronization")

# Only continue if previous step succeeded
if (sync_success) {
  log_info("=== STEP 2: PARSING ReDIF FILES ===")
  parse_success <- run_step("parse_rdf_perl_backend.R", "ReDIF file parsing")
  
  if (parse_success) {
    log_info("=== STEP 3: GENERATING EMBEDDINGS ===")
    embed_success <- run_step("embed_collection.R", "embedding generation")
    
    if (embed_success) {
      log_info("All steps completed successfully!")
    } else {
      log_error("Embedding generation failed. Database may not be fully updated.")
    }
  } else {
    log_error("Parsing step failed. Skipping embedding generation.")
  }
} else {
  log_error("RePEc synchronization failed. Skipping subsequent steps.")
}

log_info("Pipeline execution completed")