pacman::p_load(here,
               curl,
               tidyverse)

sync_repec_folder <- function(.archive, .journal = NULL) {
  if (is.na(.journal)) .journal <- NULL
  # Construct FTP URL
  ftp_url <- if (is.null(.journal)) {
    paste0("/opt/ReDIF/RePEc/", .archive, "/")
  } else {
    paste0("/opt/ReDIF/RePEc/", .archive, "/", .journal, "/")
  }
  
  # Construct local directory path
  local_path <- if (is.null(.journal)) {
    here("RePEc", .archive)
  } else {
    here("RePEc", .archive, .journal)
  }
  
  # Ensure local directory exists
  if (!dir.exists(local_path)) {
    dir.create(local_path, recursive = TRUE)
  }
  
  # Ensure proper quoting of paths
  ftp_url_quoted <- shQuote(ftp_url)
  local_path_quoted <- shQuote(normalizePath(local_path, mustWork = FALSE))
  
  # Construct lftp command (no FTP prefix in mirror command)
  cmd <- sprintf(
    'lftp ftp://anonymous:@ftp.repec.org -e "mirror --parallel=4 --verbose %s %s; bye"',
    ftp_url_quoted, local_path_quoted
  )
  
  # Execute the command using system()
  system(cmd, intern = TRUE, ignore.stderr = FALSE)
  
  invisible(NULL)
}


#h <- curl::new_handle(dirlistonly = TRUE)
## Open connection
#con <- curl::curl("ftp://anonymous:@all.repec.org/RePEc/all/", "r", h)
## Read file list
#file_list <- readLines(con, warn = FALSE) 
#close(con)
#file_list |>
#  purrr::keep(~str_detect(.x,"oup"))
## Download the file
#url <- "ftp://anonymous:@all.repec.org/RePEc/all/ouparch.rdf"
#local_path <- here("ouparch.rdf")
#download.file(url, local_path, mode = "wb")

repec_ftp_listing <- function(.archive, .journal = NULL) {
  # Construct FTP URL based on whether .journal is provided
  ftp_url <- if (is.null(.journal)) {
    paste0("ftp://anonymous:@ftp.repec.org/opt/ReDIF/RePEc/", .archive, "/")
  } else {
    paste0("ftp://anonymous:@ftp.repec.org/opt/ReDIF/RePEc/", .archive, "/", .journal, "/")
  }
  
  # Initialize cURL handle
  h <- curl::new_handle(dirlistonly = TRUE)
  
  # Open connection
  con <- curl::curl(ftp_url, "r", h)
  
  # Read file list
  file_list <- readLines(con, warn = FALSE)  # Suppress warnings if empty
  close(con)
  
  # Return file list
  file_list
}


journals <- tribble(
  ~archive, ~journal, ~long_name,
  #TOP 5
  "aea",   "aecrev"  , "American Economic Review",                                           
  "aea",   "aerins"  , "American Economic Review: Insights",                                            
  "oup",    "qjecon", "Quarterly Journal of Economics",
  "ucp",    "jpolec",  "Journal of Political Economy (JPE)",
  "ucp",    "jpemic",  "Journal of Political Economy: Microeconomics (JPE Micro)",
  "ucp",    "jpemac",  "Journal of Political Economy: Macroeconomics (JPE Macro)",
  "oup",    "restud", "Review of Economic Studies",
  "ecm",    "emetrp", "Econometrica",
  "wly",    "emetrp", "Econometrica",
  
  #AEA Journals
  "aea",   "aejapp",   "American Economic Journal: Applied Economics",                                           
  "aea",   "aejmac",   "American Economic Journal: Macroeconomics",                                             
  "aea",   "aejmic",   "American Economic Journal: Microeconomics",                                              
  "aea",   "aejpol",   "American Economic Journal: Economic Policy",    
  "aea",   "apandp"  , "American Economic Review: P&P",                                           
  "aea",   "jeclit"  , "Journal of Economic Literature",                                            
  "aea",   "jecper" , "Journal of Economic Perspectives",
  
  
  #TOP General Interest
  "oup",    "revfin", "Review of Finance",
  "tpr",    "jeurec", "Journal of the European Economic Association",
  "oup",    "jeurec", "Journal of the European Economic Association",
  "oup",    "econjl", "The Economic Journal",
  "tpr",    "restat", "Review of Economics and Statistics (RESTAT)",
  
  #Top Field Journals
  "ucp",    "jlabec", "Journal of Labor Economics (JOLE)",
  "eee",    "pubeco", "Journal of Public Economics",
  "oup",    "jecgeo", "Journal of Economic Geography",
  "uwp",    "jhriss", "Journal of Human Resources",
  "eee",    "juecon", "Journal of Urban Economics",
  "kap",    "jecgro", "Journal of Economic Growth",
  "eee",    "jhecon", "Journal of Health Economics",
  "oup",    "ecpoli", "Economic Policy",
  "oup",    "emjrnl", "Econometrics Journal",
  "eee",    "econom", "Journal of Econometrics",
  "eee",    "respol", "Research Policy",
  "cup",    "jechis", "Journal of Economic History",
  "eee",    "jeeman", "Journal of Environmental Economics and Management",
  "eee",    "eneeco", "Energy Economics",
  "oup",    "jfinec", "Journal of Financial Economics (JFE)",
  "eee",    "jetheo", "Journal of Economic Theory (JET)",
  
  #Solid B Journals
  "eee",    "eecrev", "European Economic Review",
  "eee",    "labeco", "Labour Economics",
  "spr",    "jopoec", "Journal of Population Economics",
  "jae",    "japmet", "Journal of Applied Econometrics",
  "wly",    "japmet", "Journal of Applied Econometrics",
  "eee",    "jeborg", "Journal of Economic Behavior & Organization (JEBO)",
  
  #WP Series
  "nbr",     "nberwo",  "NBER Working Papers",
  "zbw",     "zewdip",  "ZEW Discussion Papers",
  "diw",     "diwsop",  "SOEPpapers",
  "diw",     "diwwpp",  "Discussion Papers of DIW Berlin",
  "iza",     "izadps",  "IZA Discussion Papers",
  "ces",     "ceswps",  "CESifo Working Paper Series",
)



journals |>
  pwalk(function(archive, journal, long_name) {
    message("Syncing: ", long_name)
    sync_repec_folder(archive, journal)  
  })

#Write journals to disk once syncing was done
journals |> haven::write_dta(here("journals_synced.dta"))

#sync_repec_folder("oup", "restud")  


