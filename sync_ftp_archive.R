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
  "bla",    "jfinan",   "Journal of Finance", "General Interest",
  "oup",    "rfinst",   "Review of Financial Studies", "General Interest",
  "eee",    "jfinec",   "Journal of Financial Economics", "General Interest",
  "oup",    "rcorpf",   "Review of Corporate Finance Studies", "General Interest",
  "anr",    "reveco",   "Annual Review of Economics", "General Interest",
  #"nat",    "nature",  "Nature", "General Interest",
  

  #Top Field Journals
  "ucp",    "jlabec", "Journal of Labor Economics (JOLE)", "Top Field Journals (A)",
  "eee",    "pubeco", "Journal of Public Economics", "Top Field Journals (A)",
  "uwp",    "jhriss", "Journal of Human Resources", "Top Field Journals (A)",
  "eee",    "juecon", "Journal of Urban Economics", "Top Field Journals (A)",
  "eee",    "jaecon",   "Journal of Accounting and Economics", "Top Field Journals (A)",
  "bla",    "joares",   "Journal of Accounting Research", "Top Field Journals (A)",
  "spr",    "reaccs",   "Review of Accounting Studies", "Top Field Journals (A)",
  "eee",    "moneco",   "Journal of Monetary Economics", "Top Field Journals (A)",
  "eee",    "jfinin",   "Journal of Financial Intermediation", "Top Field Journals (A)",
  "taf",    "jnlbes",   "Journal of Business and Economic Statistics", "Top Field Journals (A)",
  "cup",    "jfinqa",     "Journal of Financial and Quantitative Analysis", "Top Field Journals (A)",
  "inm",    "ormksc",   "Marketing Science", "Top Field Journals (A)",
  "oup",    "jconrs",   "Journal of Consumer Research", "Top Field Journals (A)",
  "inm",    "ormsom",   "Manufacturing and Service Operations Management", "Top Field Journals (A)",
  "eee",    "jhecon", "Journal of Health Economics", "Top Field Journals (A)",
  "oup",    "ecpoli", "Economic Policy", "General Interest",
  "oup",    "emjrnl", "Econometrics Journal", "Top Field Journals (A)",
  "eee",    "respol", "Research Policy", "Top Field Journals (A)",
  "cup",    "jechis", "Journal of Economic History", "Top Field Journals (A)",
  "eee",    "jeeman", "Journal of Environmental Economics and Management", "Top Field Journals (A)",
  "eee",    "eneeco", "Energy Economics", "Second in Field Journals (B)",
  "eee",    "jetheo", "Journal of Economic Theory (JET)", "Top Field Journals (A)",
  "oup",    "rasset",   "Review of Asset Pricing Studies", "Top Field Journals (A)",
  "inm",    "ormnsc", "Managment Science", "Top Field Journals (A)",
  
  #Solid B Journals
  "eee",    "eecrev", "European Economic Review", "Second in Field Journals (B)",
  "eee",    "labeco", "Labour Economics", "Second in Field Journals (B)",
  "spr",    "jopoec", "Journal of Population Economics", "Second in Field Journals (B)",
  "jae",    "japmet", "Journal of Applied Econometrics", "Second in Field Journals (B)",
  "wly",    "japmet", "Journal of Applied Econometrics", "Second in Field Journals (B)",
  "eee",    "jeborg", "Journal of Economic Behavior & Organization (JEBO)", "Second in Field Journals (B)",
  "kap",    "jecgro", "Journal of Economic Growth", "Second in Field Journals (B)",
  "eee",    "econom", "Journal of Econometrics", "Second in Field Journals (B)",
  "oup",    "jecgeo", "Journal of Economic Geography", "Second in Field Journals (B)",
  "wly",    "quante",  "Quantitative Economics", "Second in Field Journals (B)",  
  "eee",    "deveco",  "Journal of Development Economics", "Second in Field Journals (B)",  
  "rje",    "randje",  "RAND Journal of Economics", "Second in Field Journals (B)",  
  "the",    "publsh",  "Theoretical Economics", "Second in Field Journals (B)",  
  "kap",    "sbusec",  "Small Business Economics", "Second in Field Journals (B)",  
  "wly",    "iecrev",  "International Economic Review", "Second in Field Journals (B)",  
  "kap",    "expeco",  "Experimental Economics", "Second in Field Journals (B)",  
  "eee",    "enepol",  "Energy Policy", "Second in Field Journals (B)",  
  
  #Other Journals
  "bpj",    "germec",  "German Economic Review", "Other Journals",
  "eee",    "ecolet",  "Economic Letters", "Other Journals",
  "spr",    "jregsc",  "Journal of Regional Science", "Other Journals",  
  "oup",    "oxecpp",  "Oxford Economic Papers", "Other Journals",  
  "spr",    "empeco",  "Empirical Economics", "Other Journals",  
  "oup",    "wbecrv",  "The World Bank Economic Review", "Other Journals",
  "bla",    "obuest",  "Oxford Bulletin of Economics and Statistics", "Other Journals",
  "bla",    "scandj",  "Scandinavian Journal of Economics", "Other Journals",
  "jns",    "jbstat",  "Journal of Economics and Statistics (Jahrbuecher fuer Nationaloekonomie und Statistik)","Other Journals",
  
  #WP Series
  "nbr",     "nberwo",  "NBER Working Papers", "Working Paper Series",
  "zbw",     "zewdip",  "ZEW Discussion Papers", "Working Paper Series",
  "diw",     "diwsop",  "SOEPpapers", "Working Paper Series",
  "diw",     "diwwpp",  "Discussion Papers of DIW Berlin", "Working Paper Series",
  "iza",     "izadps",  "IZA Discussion Papers", "Working Paper Series",
  "ces",     "ceswps",  "CESifo Working Paper Series", "Working Paper Series",
  "iab",     "iabdpa",  "IAB Discussion Papers", "Working Paper Series"
)

#Test whether some journals are double in the list
journals |>
  count(repec_id=paste0(archive,":",journal)) |>
  filter(n!=1)


#Write journals to disk 
journals |> write_csv(here::here("journals.csv"))

read_csv(here::here("journals.csv")) |>
  select(-category) |>
  pwalk(function(archive, journal, long_name) {
    message("Syncing: ", long_name)
    sync_repec_folder(archive, journal)  
  })


#Manually load incomplete syncs after first load
if(FALSE){
  loaded_journals <- fs::dir_info(here("REPEC")) |>
    transmute(path=as.character(path)) |>
    map_dfr(fs::dir_info) |>
    transmute(path=as.character(path),
              archive = str_extract(path,"REPEC/[A-Za-z]{3}") |> 
                   str_extract("/[A-Za-z]{3}")  |>
                   str_remove("/"),
              journal =  str_extract(path,"REPEC/[A-Za-z]{3}/[A-Za-z]{6}") |>
                str_extract("/[A-Za-z]{6}")  |>
                str_remove("/")
              ) 
  
  read_csv(here::here("journals.csv")) |>
    select(-category) |>
    left_join(loaded_journals) |>
    filter(is.na(path)) |>
    select(-path) |>
    pwalk(function(archive, journal, long_name) {
      message("Syncing: ", long_name)
      sync_repec_folder(archive, journal)  
    })
} 
  
  

#sync_repec_folder("oup", "restud")  


