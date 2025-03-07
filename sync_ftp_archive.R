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
  ~archive, ~journal, ~long_name, ~category,
  #TOP 5
  "aea",   "aecrev"  , "American Economic Review", "Top 5 Journals",                                           
  "aea",   "aerins"  , "American Economic Review: Insights", "Top 5 Journals",                                            
  "oup",    "qjecon", "Quarterly Journal of Economics", "Top 5 Journals",
  "ucp",    "jpolec",  "Journal of Political Economy (JPE)", "Top 5 Journals",
  "ucp",    "jpemic",  "Journal of Political Economy: Microeconomics (JPE Micro)", "Top 5 Journals",
  "ucp",    "jpemac",  "Journal of Political Economy: Macroeconomics (JPE Macro)", "Top 5 Journals",
  "oup",    "restud", "Review of Economic Studies", "Top 5 Journals",
  "ecm",    "emetrp", "Econometrica", "Top 5 Journals",
  "wly",    "emetrp", "Econometrica", "Top 5 Journals",
  
  #AEA Journals
  "aea",   "aejapp",   "American Economic Journal: Applied Economics", "AEJs",                                           
  "aea",   "aejmac",   "American Economic Journal: Macroeconomics", "AEJs",                                             
  "aea",   "aejmic",   "American Economic Journal: Microeconomics", "AEJs",                                              
  "aea",   "aejpol",   "American Economic Journal: Economic Policy", "AEJs",    
  "aea",   "apandp"  , "American Economic Review: P&P", "AEJs",                                           
  "aea",   "jeclit"  , "Journal of Economic Literature", "AEJs",                                            
  "aea",   "jecper" , "Journal of Economic Perspectives", "AEJs",
  
  #TOP General Interest
  "oup",    "revfin", "Review of Finance", "General Interest",
  "tpr",    "jeurec", "Journal of the European Economic Association", "General Interest",
  "oup",    "jeurec", "Journal of the European Economic Association", "General Interest",
  "oup",    "econjl", "The Economic Journal", "General Interest",
  "tpr",    "restat", "Review of Economics and Statistics (RESTAT)", "General Interest",
  
  #Top Field Journals
  "ucp",    "jlabec", "Journal of Labor Economics (JOLE)", "Top Field Journals (A)",
  "eee",    "pubeco", "Journal of Public Economics", "Top Field Journals (A)",
  "oup",    "jecgeo", "Journal of Economic Geography", "Second in Field Journals (B)",
  "uwp",    "jhriss", "Journal of Human Resources", "Top Field Journals (A)",
  "eee",    "juecon", "Journal of Urban Economics", "Top Field Journals (A)",
  "kap",    "jecgro", "Journal of Economic Growth", "Second in Field Journals (B)",
  "eee",    "jhecon", "Journal of Health Economics", "Top Field Journals (A)",
  "oup",    "ecpoli", "Economic Policy", "General Interest",
  "oup",    "emjrnl", "Econometrics Journal", "Top Field Journals (A)",
  "eee",    "econom", "Journal of Econometrics", "Second in Field Journals (B)",
  "eee",    "respol", "Research Policy", "Top Field Journals (A)",
  "cup",    "jechis", "Journal of Economic History", "Top Field Journals (A)",
  "eee",    "jeeman", "Journal of Environmental Economics and Management", "Top Field Journals (A)",
  "eee",    "eneeco", "Energy Economics", "Second in Field Journals (B)",
  "oup",    "jfinec", "Journal of Financial Economics (JFE)", "General Interest",
  "eee",    "jetheo", "Journal of Economic Theory (JET)", "Top Field Journals (A)",
  
  #Solid B Journals
  "eee",    "eecrev", "European Economic Review", "Second in Field Journals (B)",
  "eee",    "labeco", "Labour Economics", "Second in Field Journals (B)",
  "spr",    "jopoec", "Journal of Population Economics", "Second in Field Journals (B)",
  "jae",    "japmet", "Journal of Applied Econometrics", "Second in Field Journals (B)",
  "wly",    "japmet", "Journal of Applied Econometrics", "Second in Field Journals (B)",
  "eee",    "jeborg", "Journal of Economic Behavior & Organization (JEBO)", "Second in Field Journals (B)",
  
  #WP Series
  "nbr",     "nberwo",  "NBER Working Papers", "Working Paper Series",
  "zbw",     "zewdip",  "ZEW Discussion Papers", "Working Paper Series",
  "diw",     "diwsop",  "SOEPpapers", "Working Paper Series",
  "diw",     "diwwpp",  "Discussion Papers of DIW Berlin", "Working Paper Series",
  "iza",     "izadps",  "IZA Discussion Papers", "Working Paper Series",
  "ces",     "ceswps",  "CESifo Working Paper Series", "Working Paper Series",
  "iab",     "iabdpa",  "IAB Discussion Papers", "Working Paper Series"
)

#Write journals to disk 
journals |> write_csv(here::here("journals.csv"))

read_csv(here::here("journals.csv")) |>
  pwalk(function(archive, journal, long_name) {
    message("Syncing: ", long_name)
    sync_repec_folder(archive, journal)  
  })



#sync_repec_folder("oup", "restud")  


