pacman::p_load(here,
               curl,
               tidyverse)


journals <- tribble(
  ~archive, ~journal, ~long_name, ~category, ~sjr_id,
  #TOP 5
  "aea",   "aecrev"  , "American Economic Review", "Top 5 Journals",  22697,                                         
  "aea",   "aerins"  , "American Economic Review: Insights", "Top 5 Journals",    22697,                                           
  "oup",    "qjecon", "Quarterly Journal of Economics", "Top 5 Journals", 29431,  
  "ucp",    "jpolec",  "Journal of Political Economy (JPE)", "Top 5 Journals", 24404,  
  "ucp",    "jpemic",  "Journal of Political Economy: Microeconomics (JPE Micro)", "Top 5 Journals", 24404,  
  "ucp",    "jpemac",  "Journal of Political Economy: Macroeconomics (JPE Macro)", "Top 5 Journals", 24404,  
  "oup",    "restud", "Review of Economic Studies", "Top 5 Journals", 24202,  
  "ecm",    "emetrp", "Econometrica", "Top 5 Journals", 19482,  
  "wly",    "emetrp", "Econometrica", "Top 5 Journals", 19482,  
  
  #AEA Journals
  "aea",   "aejapp",   "American Economic Journal: Applied Economics", "AEJs",  19900191767,                                            
  "aea",   "aejmac",   "American Economic Journal: Macroeconomics", "AEJs",    19900191885,                                            
  "aea",   "aejmic",   "American Economic Journal: Microeconomics", "AEJs",     19900191883,                                           
  "aea",   "aejpol",   "American Economic Journal: Economic Policy", "AEJs",    19900191884,   
  "aea",   "apandp"  , "AEA Paper and Proceedings", "AEJs",  NA,                                            
  "aea",   "jeclit"  , "Journal of Economic Literature", "AEJs",  27165,                                             
  "aea",   "jecper" , "Journal of Economic Perspectives", "AEJs", 28980,  
  
  #TOP General Interest
  "oup",    "revfin", "Review of Finance", "General Interest", 16160,  
  "tpr",    "jeurec", "Journal of the European Economic Association", "General Interest", 7500153126,  
  "oup",    "jeurec", "Journal of the European Economic Association", "General Interest", 7500153126,  
  "oup",    "econjl", "The Economic Journal", "General Interest", 23011,  
  "tpr",    "restat", "Review of Economics and Statistics (RESTAT)", "General Interest", 24204,  
  "bla",    "jfinan", "Journal of Finance", "General Interest", 17500,  
  "oup",    "rfinst", "Review of Financial Studies", "General Interest", 16161,  
  "eee",    "jfinec", "Journal of Financial Economics", "General Interest", 24379,  
  "anr",    "reveco", "Annual Review of Economics", "General Interest", 19700200701,  
  "bin",    "bpeajo", "Brookings Papers on Economic Activity", "General Interest", 28418,  
  "nas",    "journl", "Proceedings of the National Academy of Sciences", "General Interest",  21121,  
  "cup",    "apsrev", "American Political Science Review", "General Interest",  15557,  
  "oup",    "ecpoli", "Economic Policy", "General Interest", 20022,  
  "sae",    "amsocr",  "American Sociological Review,","General Interest",NA,
  
  #Top Field Journals
  "bla",    "jorssb", "Journal of the Royal Statistical Society: Series B", "Top Field Journals (A)",  13531,  
  "ucp",    "jlabec", "Journal of Labor Economics (JOLE)", "Top Field Journals (A)", 20246,  
  "eee",    "pubeco", "Journal of Public Economics", "Top Field Journals (A)", 29009,  
  "uwp",    "jhriss", "Journal of Human Resources", "Top Field Journals (A)", 24870,  
  "eee",    "juecon", "Journal of Urban Economics", "Top Field Journals (A)", 24406,  
  "eee",    "jaecon", "Journal of Accounting and Economics", "Top Field Journals (A)", 29838,  
  "bla",    "joares", "Journal of Accounting Research", "Top Field Journals (A)", 29857,  
  "spr",    "reaccs", "Review of Accounting Studies", "Top Field Journals (A)", 12963,  
  "eee",    "moneco", "Journal of Monetary Economics", "Top Field Journals (A)", 17555,  
  "eee",    "jfinin", "Journal of Financial Intermediation", "Top Field Journals (A)", 28996,  
  "taf",    "jnlbes", "Journal of Business and Economic Statistics", "Top Field Journals (A)", 27162,  
  "cup",    "jfinqa", "Journal of Financial and Quantitative Analysis", "Top Field Journals (A)", 80370,  
  "inm",    "ormksc", "Marketing Science", "Top Field Journals (A)", 23714,  
  "oup",    "jconrs", "Journal of Consumer Research", "Top Field Journals (A)", 22899,  
  "inm",    "ormsom", "Manufacturing and Service Operations Management", "Top Field Journals (A)", 21789,  
  "eee",    "jhecon", "Journal of Health Economics", "Top Field Journals (A)", 14166,  
  "oup",    "emjrnl", "Econometrics Journal", "Top Field Journals (A)", 4700152266,  
  "eee",    "respol", "Research Policy", "Top Field Journals (A)", 22900,  
  "cup",    "jechis", "Journal of Economic History", "Top Field Journals (A)", 28979,  
  "eee",    "eneeco", "Energy Economics", "Second in Field Journals (B)", 29374,  
  "eee",    "jetheo", "Journal of Economic Theory (JET)", "Top Field Journals (A)", 28981,  
  "oup",  "rasset", "Review of Asset Pricing Studies", "Top Field Journals (A)", 21100926577,  
  "inm",  "ormnsc", "Management Science", "Top Field Journals (A)", 21307,  
  "eee",  "inecon", "Journal of International Economics", "Top Field Journals (A)", 24393,  
  "red",  "issued", "Review of Economic Dynamics", "Top Field Journals (A)", 26259,  
  "eee",  "corfin", "Journal of Corporate Finance", "Top Field Journals (A)", 17496,  
  "pal",  "imfecr", "IMF Economic Review", "Top Field Journals (A)", 19900192439,  
  "wly",  "ajagec", "American Journal of Agricultural Economics", "Top Field Journals (A)", 69623,  
  "aen",  "journl", "The Energy Journal", "Top Field Journals (A)", NA,  
  "oup",  "rcorpf", "Review of Corporate Finance Studies", "Top Field Journals (A)", 21100926579,  
  "taf",  "jnlasa", "Journal of the American Statistical Association", "Top Field Journals (A)", 12094,  
  "bla",  "jscmgt", "Journal of Supply Chain Management", "Top Field Journals (A)", 100147317,  
  "eee",  "ecochp", "Handbook of Econometrics", "Top Field Journals (A)", 17700155014,  
  "anr",  "refeco", "Annual Review of Financial Economics", "Top Field Journals (A)", 19700200702,
  "eee",  "indchp", "Handbook of Industrial Organization","Top Field Journals (A)",17700155016,
  "spr",  "joamsc","Journal of the Academy of Marketing Science","Top Field Journals (A)",24385,
  "sae",  "entthe","Entrepreneurship Theory and Practice","Top Field Journals (A)",100147318,
  "pal",  "joibpo","Journal of International Business Studies","Top Field Journals (A)",24392,
  
  
  #Solid B Journals
  "eee",    "eecrev", "European Economic Review", "Second in Field Journals (B)",                           20749,  
  "eee",    "labeco", "Labour Economics", "Second in Field Journals (B)",                                   16922,  
  "spr",    "jopoec", "Journal of Population Economics", "Second in Field Journals (B)",                    26878,  
  "jae",    "japmet", "Journal of Applied Econometrics", "Second in Field Journals (B)",                    28964,  
  "wly",    "japmet", "Journal of Applied Econometrics", "Second in Field Journals (B)",                    28964,  
  "eee",    "jeborg", "Journal of Economic Behavior & Organization (JEBO)", "Second in Field Journals (B)", 23865,  
  "kap",    "jecgro", "Journal of Economic Growth", "Second in Field Journals (B)",                      28978,  
  "eee",    "econom", "Journal of Econometrics", "Second in Field Journals (B)",                         28973,  
  "oup",    "jecgeo", "Journal of Economic Geography", "Second in Field Journals (B)",                   22750,  
  "wly",    "quante", "Quantitative Economics", "Second in Field Journals (B)",                    21100255526,  
  "eee",    "deveco", "Journal of Development Economics", "Second in Field Journals (B)",                14834,  
  "bla",    "randje", "RAND Journal of Economics", "Second in Field Journals (B)",                       23672,  
  "the",    "publsh", "Theoretical Economics", "Second in Field Journals (B)",                      6400153126,  
  "kap",    "sbusec", "Small Business Economics", "Second in Field Journals (B)",                        29104,  
  "wly",    "iecrev", "International Economic Review", "Second in Field Journals (B)",                   23666,  
  "kap",    "expeco", "Experimental Economics", "Second in Field Journals (B)",                          28527,  
  "eee",    "enepol", "Energy Policy", "Second in Field Journals (B)",                                   29403,  
  "bla",    "jecsur", "Journal of Economic Surveys", "Second in Field Journals (B)",                     24353,  
  "eee",    "wdevel", "World Development", "Second in Field Journals (B)",                               30060,  
  "eee",    "ecoedu", "Economics of Education Review", "Second in Field Journals (B)",                   19692,  
  "kap",    "pubcho", "Public Choice", "Second in Field Journals (B)",                                   23611,  
  "eee",    "jimfin", "Journal of International Money and Finance", "Second in Field Journals (B)",         17533,  
  "eee",    "empfin", "Journal of Empirical Finance", "Second in Field Journals (B)",                       17499,  
  "eee",    "finsta", "Journal of Financial Stability", "Second in Field Journals (B)",                    144987,  
  "taf",    "emetrv", "Econometric Reviews", "Second in Field Journals (B)",                                24381,  
  "bla",    "ecinqu", "Economic Inquiry", "Second in Field Journals (B)",                                   20002,  
  "eee",    "jcecon", "Journal of Comparative Economics", "Second in Field Journals (B)",                   23861,  
  "eee",    "ecmode", "Economic Modelling", "Second in Field Journals (B)",                                 28467,  
  "wly",    "jmoncb", "Journal of Money, Credit and Banking", "Second in Field Journals (B)",  17557,  
  "eee",    "jeeman", "Journal of Environmental Economics and Management", "Second in Field Journals (B)", 23352,  
  "eee",    "gamebe", "Games and Economic Behaviour", "Second in Field Journals (B)", 14194,  
  "eee",    "finmar", "Journal of Financial Markets", "Second in Field Journals (B)", 17528,  
  "eee",    "dyncon", "Journal of Economic Dynamics and Control", "Second in Field Journals (B)", 28976,  
  "oup",    "jfinec", "Journal of Financial Econometrics", "Second in Field Journals (B)", 145695,  
  "ijc",    "ijcjou", "International Journal of Central Banking", "Second in Field Journals (B)", 19700187515,  
  "ucp",    "renvpo",  "Review of Environmental Economics and Policy","Second in Field Journals (B)",11600153422,
  "wly",    "coacre",  "Contemporary Accounting Research","Second in Field Journals (B)",29113,
  "eee",    "proeco",  "International Journal of Production Economics","Second in Field Journals (B)",19165,
  "ucp",    "jaerec",  "Journal of the Association of Environmental and Resource Economists","Second in Field Journals (B)",21100861110,
  "eee",    "fambus",  "Journal of Family Business Strategy","Second in Field Journals (B)",19600161806,
  "kap",    "jbuset",  "Journal of Business Ethics","Second in Field Journals (B)",23859,
  "oup",    "wbrobs",  "World Bank Research Observer","Second in Field Journals (B)",18034,
  
  #Other Journals
  "bpj",    "germec",  "German Economic Review", "Other Journals",                                       25564,  
  "eee",    "ecolet",  "Economics Letters", "Other Journals",                                            28490,  
  "bla",    "jregsc",  "Journal of Regional Science", "Other Journals",                                  13933,  
  "oup",    "oxecpp",  "Oxford Economic Papers", "Other Journals",                                       22903,  
  "spr",    "empeco",  "Empirical Economics", "Other Journals",                                          20642,  
  "oup",    "wbecrv",  "The World Bank Economic Review", "Other Journals",                               27365,  
  "bla",    "obuest",  "Oxford Bulletin of Economics and Statistics", "Other Journals",                  22902,  
  "bla",    "scandj",  "Scandinavian Journal of Economics", "Other Journals",                            24849,  
  "jns",    "jbstat",  "Journal of Economics and Statistics (Jahrbuecher fuer Nationaloekonomie und Statistik)","Other Journals", 27132,  
  "jss",    "jstsof",  "Journal of Statistical Software", "Other Journals",       12137,  
  "sae",    "ilrrev",  "ILR Review", "Other Journals",                            19680,                                                
  "bla",    "revinw",  "Review of Income and Wealth",  "Other Journals",          20142,  
  "eee",    "regeco",  "Regional Science and Urban Economics", "Other Journals",   15139,  
  "wly",    "hlthec",  "Health Economics",  "Other Journals",  13356,  
  "bla",    "stratm",  "Strategic Management Journal", "Other Journals",  23157,  
  "eee",    "jbvent",  "Journal of Business Venturing", "Other Journals", 23860,  
  "taf",    "regstd",  "Regional Studies", "Other Journals", 15143,  
  "iab",    "iabjlr",  "Journal of Labour Market Research", "Other Journals", 21101210876,  
  "mgs",    "iojome",  "International Journal of Operations Management","Other Journals", 20039,  
  "kap",    "enreec",  "Environmental & Resource Economics", "Other Journals", 20928,  
  "wly",    "fistud",  "Fiscal Studies","Other Journals",27094,
  "taf",    "rripxx",  "Review of International Political Economy","Other Journals",26261,
  "taf",    "ufajxx",  "Financial Analysts Journal","Other Journals",14733,
  "cup",    "buetqu",  "Business Ethics Quarterly","Other Journals",22717,
  "sae",    "woemps",  "Work, Employment and Society","Other Journals",19251,
  "bla",    "finmgt",  "Financial Management","Other Journals",74502,
  "eee",    "jfpoli",  "Food Policy","Other Journals",35048,
  "taf",    "recgxx",  "Economic Geography","Other Journals",28092,
  "eee",    "jrpoli",  "Resources Policy","Other Journals",110031,
  "anr",    "reseco",  "Annual Review of Resource Economics","Other Journals",19700200703,
  "eee",    "recore",  "Resources, Conservation and Recycling Advances","Other Journals",21101081525,
  "kap",    "mktlet",  "Marketing Letters","Other Journals",23687,
  "eee",    "ecolec",  "Ecological Economics","Other Journals",20290,
  "ucp",    "amjhec",  "American Journal of Health Economics","Other Journals",21100829907,
  "oup",    "oxford",  "Oxford Review of Economic Policy","Other Journals",22904,
  "eee",    "exehis",  "Explorations in Economic History","Other Journals",28528,
  "eme",    "aaajpp",  "Accounting, Auditing and Accountability Journal","Other Journals",130105,
  "eee",    "finana",  "International Review of Financial Analysis","Other Journals",17385,
  "kap",    "reveho",  "Review of Economics of the Household","Other Journals",144933,
  "wly",    "apecpp",  "Applied Economic Perspectives and Policy","Other Journals",19700176401,
  "eee",    "chieco",  "China Economic Review","Other Journals",20566,
  "bla",    "inecol",  "Journal of Industrial Ecology","Other Journals",23411,
  "spr",    "elmark",  "Electronic Markets","Other Journals",19600157911,
  "eee",    "jbfina",  "Journal of Banking and Finance","Other Journals",17472,
  "eee",    "telpol",  "Telecommunications Policy","Other Journals",20870,
  "kap",    "asiapa",  "Asia Pacific Journal of Management","Other Journals",145211,
  "bla",    "mathfi",  "Mathematical Finance","Other Journals",18173,
  "eee",    "ecanpo",  "Economic Analysis and Policy","Other Journals",5800191435,
  "oup",    "cjrecs",  "Cambridge Journal of Regions, Economy and Society","Other Journals",18400156711,
  "spr",    "revint",  "Review of International Organizations","Other Journals",4700152638,
  "eee",    "aumajo",  "Australasian Marketing Journal","Other Journals",17700155010,
  "taf",    "femeco",  "Feminist Economics","Other Journals",21255,
  "kap",    "jrisku",  "Journal of Risk and Uncertainty","Other Journals",29011,
  "bla",    "asiapr",  "Asian Economic Policy Review","Other Journals",17600155050,
  "eee",    "iepoli",  "Information Economics and Policy","Other Journals",18972,
  "taf",    "bindes",  "Bulletin of Indonesian Economic Studies","Other Journals",19966,
  "bla",    "jageco",  "Journal of Agricultural Economics","Other Journals",88586,
  "idi",    "jermbe",  "European Research on Management and Business Economics","Other Journals",21100465205,
  "kap",    "qmktec",  "Quantitative Marketing and Economics","Other Journals",144981,
  "eee",    "joepsy",  "Journal of Economic Psychology","Other Journals",22915,
  "cup",    "etheor",  "Econometric Theory","Other Journals",28464,
  "taf",    "entreg",  "Entrepreneurship and Regional Development","Other Journals",20662,
  "eee",    "streco",  "Structural Change and Economic Dynamics","Other Journals",29487,
  "cup",    "macdyn",  "Macroeconomic Dynamics","Other Journals",    20109,
  
  #WP Series
  "nbr",     "nberwo",  "NBER Working Papers", "Working Paper Series", NA,  
  "zbw",     "zewdip",  "ZEW Discussion Papers", "Working Paper Series", NA,  
  "diw",     "diwsop",  "SOEPpapers", "Working Paper Series", NA,  
  "diw",     "diwwpp",  "Discussion Papers of DIW Berlin", "Working Paper Series", NA,  
  "iza",     "izadps",  "IZA Discussion Papers", "Working Paper Series", NA,  
  "ces",     "ceswps",  "CESifo Working Paper Series", "Working Paper Series", NA,  
  "iab",     "iabdpa",  "IAB Discussion Papers", "Working Paper Series", NA,  
  "iab",     "iabfme",  "IAB FDZ Methodenreport", "Working Paper Series", NA,  
  "bos",     "iedwpr",  "Boston University -Institute for Economic Development Working Paper","Working Paper Series", NA,  
  "bfi",     "wpaper",  "Becker Friedman Institute for Research In Economics Working Papers",  "Working Paper Series",   NA,                                
  "pri",     "indrel",  "Princeton Industrial Relations Section Working Papers.",  "Working Paper Series", NA,         
  "cpr",     "ceprdp",  "CEPR Discussion Papers",  "Working Paper Series", NA,    
  "ehl",     "lserod",  "LSE Research Online Documents on Economics",  "Working Paper Series", NA,  
  "arx",     "papers",  "arXiv.org", "Working Paper Series", NA,  
  "cwl",     "cwldpp",  "Cowles Foundation Discussion Papers", "Working Paper Series", NA,   
  "hbs",     "wpaper",  "Harvard Business School Working Papers", "Working Paper Series", NA,   
  "mit",     "worpap",  "MIT Working Papers", "Working Paper Series", NA,   
  #"hal",     "journl",  "HAL Post-Print",  "Working Paper Series", NA,   
  #"hal",     "wpaper",  "HAL Working Paper",  "Working Paper Series", NA,   
  "pra",     "mprapa",  "MPRA Paper",  "Working Paper Series", NA,   
  "hrv",     "faseco",  "Harvard University Department of Economics Scholarly Articles",  "Working Paper Series", NA,   
  #"hal",     "pseptp",  "PSE-Ecole d'économie de Paris (Postprint)",  "Working Paper Series", NA,  
  "cep",     "cepdps",  "CEP Discussion Papers",  "Working Paper Series", NA,  
  "upf",     "upfgen",  "Economics Working Papers, Universitat Pompeu Fabra",  "Working Paper Series", NA,
  "ecb",     "ecbwps",  "European Central Bank Working Paper Series",  "Working Paper Series", NA,
  "fip",     "fedpwp",  "Federal Reserve Bank of Philadelphia Working Papers",  "Working Paper Series", NA,
  "wbk",     "wbrwps",  "The World Bank - Policy Research Working Paper Series",  "Working Paper Series", NA,
  "crm",     "wpaper",  "RFBerlin Discussion Paper Series",  "Working Paper Series", NA,
)

#Test whether some journals are double in the list
journals |> 
  count(repec_id=paste0(archive,":",journal)) |>
  filter(n!=1)


#Write journals to disk 
journals |> write_csv(here::here("journals.csv"))

sync_repec_folder <- function(.archive, .journal = NULL,
                              dest_root = here::here("RePEc"),
                              rsync_bin = "/opt/homebrew/bin/rsync") {
  if (is.na(.journal)) .journal <- NULL
  if (!file.exists(rsync_bin)) rsync_bin <- "rsync"  # fallback if needed
  
  module <- if (is.null(.journal))
    sprintf("RePEc-ReDIF/%s/", .archive)
  else
    sprintf("RePEc-ReDIF/%s/%s/", .archive, .journal)
  
  src <- sprintf("rsync.repec.org::%s", module)
  
  # Build/normalize destination
  dest <- if (is.null(.journal)) file.path(dest_root, .archive)
  else file.path(dest_root, .archive, .journal)
  dir.create(dest, recursive = TRUE, showWarnings = FALSE)
  dest <- normalizePath(dest, winslash = "/", mustWork = FALSE)
  
  # Run rsync *from* the destination so DEST is just "./"
  # (no spaces to parse, no need for `--` separator)
  withr::with_dir(dest, {
    args <- c("-av", "-s", "--delete", "--contimeout=20", "--exclude=*.pdf", src, "./")
    status <- system2(rsync_bin, args)
    if (status != 0) stop("rsync failed with status ", status)
  })
  
  invisible(dest)
}


read_csv(here::here("journals.csv")) |>
  select(-category,-sjr_id) |>
  pwalk(function(archive, journal, long_name) {
    message("Syncing: ", long_name)
    sync_repec_folder(archive, journal)  
  })




