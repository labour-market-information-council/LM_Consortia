#' ========================================================
#' 
#'    VJ_Update_step2_LoadJSON-LMconsort.R
#' 
#'    **************************************************
#'  
#'    IMPORTANT: Make sure the \script is opened with
#'                UTF-8 encoding. Default is likely latin-1.
#'                
#'              To change this in RStudio:
#'              
#'                 File --> 
#'                   Reopen with encoding... -->
#'                       choose UTF-8 and set as default.
#'    
#'    **************************************************
#'    
#'    
#'    Vicinity Jobs JSON files loaded, cleaned and exported
#'    as comma separated files (CSVs).
#'    
#'    The "JV_Update_Step1_API-LMconsort.R" file must be executed first.
#'    
#'    
#'    This script points to download folder used in Step 1.
#'    
#'    The in a loop for each month indicated, the program:
#'    - loads all JSON files
#'    - rbinds them together in long data.table
#'    - separates "Skills" and Certifications from the loaded files
#'    - cleans and structures 3 tables: Job specific, Skills, Certifications
#'    - Skills information is merged with mapping / renaming file
#'       - any Vicinity Items not mapped are recorded, but program stops only if
#'         one of the items is assocaited with 10 or more online job postings in
#'         a month.
#'    - Geography labels are updated with mapping file that corrects for typos,
#'        issues with accented characters, etc.
#'         - any Vicinity location not mapped are recorded. Program does not stop.
#'    - Sorts columns and rows before exporting 3 tables
#'    
#'    
#'    **************************************************
#'    
#'    In Arguments:
#'     - MONTH in YYYY-MM (character) format. start and end
#'     - INPUT & OUTPUT folders (must already exist)
#'     
#'    **************************************************  
#'        
#'    Outputs: RDS files
#'     1) 'vicinity-', {YYYY-MM}, "-ALL_T.rds       ==> unique job posting data table
#'     2) 'vicinity-', {YYYY-MM}, "-ALL_skills.rds  ==> job posting items/skills data table
#'     3) 'vicinity-', {YYYY-MM}, "-ALL_Certs.rds  ==> job posting certificates data table
#'     
#'    Three tables above can be merged by their common "month" and "id" columns.
#'    
#'    To load the RDS files use the Load_VJ_data_func-LMconsort.R script, 
#'    or use the base R command readRDS()
#'    
#'    Takes approximaly 60s per month loop to run.
#'        
#'              
#'    ****************************************
#' 
#'    Required packages:
#'    - data.table (no unseemly tidyverse piping here!)
#'    - zoo
#'    - rstudioapi
#'    - jsonlite
#'    - tictoc
#' 
#' ========================================================

options(encoding = "utf8")

rm(list=ls()) # clears work space
gc()          #' garbage collector removes R's deadweight from RAM


#' 2 Preliminary Options:

#' (1) Identify last and first months to be processes
VIC_month <- "2020-02" #' (YYYY-MM) -- Month of interest (eg, most recent)

VIC_init <- VIC_month    #'  If processing only 1 month
# VIC_init <- '2018-01'  #'  January 2018 is the earliest (reliable) data available

#' (2) Point to input & output folder
json_folder <- "//LMIC-Apps01/Projects/DATA/VicinityJobs/raw_data" #' input
csv_folder  <- '//LMIC-Apps01/Projects/DATA/VicinityJobs/structured_data' #'output



# Initialization ----------------------------------------------------------

#' required packages
req_pack <- c('data.table', 'zoo', 'rstudioapi', 'jsonlite', 'tictoc')

#' list of required packages not installed. Install if missing.
to_install <- req_pack[!req_pack %in% rownames(installed.packages())] 
if(length(to_install) > 0) install.packages(to_install)

# load R packages
library(jsonlite)
library(rstudioapi)
library(zoo)
library(tictoc)
library(data.table)

#' set working directory (works in Rstudio only)
local_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(local_dir)


#' Set and create output folder
if(!file.exists(json_folder) | !file.exists(csv_folder)) {
  f_check <- c(json_folder, csv_folder)
  stop(paste0('Cannot find folder:\n', 
              paste(f_check[!sapply(f_check, file.exists)],
                    collapse='\n   or\n')))
}

#' Create subfolder to store records of unmapped/recategorized items
if(!dir.exists(file.path(csv_folder, 'Unmapped Items'))) {
  dir.create(file.path(csv_folder, 'Unmapped Items'))
}

# Load Alberta District Mapping File --------------------------------------

#' Alberta data available have municipalities in "location". This file maps these
#'  to their Census Division (as is the case with other "location" geographies)
ab_map <- fread(file.path(local_dir, 'mapping_files',
                          "Alberta Municipalities - CD Mapping.csv"))
setnames(ab_map, 1:2, c("dist", "cd"))



# Load LMIC Mapping file for VJ Work Requirements -------------------------

item_map <- fread(file.path(local_dir, 
                            'mapping_files', "VJ_ESDC_restructuring - 2020-03-18.csv"))
#' keep only columns needed
item_map <- item_map[, .(items, edit_type, LMIC_Label)]
#' sort and setkey for later merging
setkey(item_map, items)


# Accent Key Mapping File -------------------------------------------------


#' Cleaning punctuation and accent as mapped by Brittany
accent_key <- fread(file.path(local_dir, 'mapping_files', 
                              "Location Accent Key - 2020-03-18.csv"),encoding = "UTF-8")

accent_key <- accent_key[, .(location_raw, province, location_correct, source)]
accent_key[,'location':=location_raw]






# VJ Load Function - \list \apply over JSON filenames ----------------------------
vj_json_load <- function(j_name){
  as.data.table(jsonlite::fromJSON(j_name,  flatten = TRUE)[['data']])
}




# Define vector of months to loop over/load -------------------------------------

month_folders <- format(seq(as.yearmon(paste0(VIC_init, '-01')), 
                            as.yearmon(paste0(VIC_month, '-01')), 1/12), '%Y-%m')



# START OF LOADING & RESTRUCTURING LOOP ---------------------------------------

save_wd <- getwd() #' store working directory for later resetting

#' Loop over folders/months
for(m in month_folders){
  tictoc::tic() #' time tracking
  
   #' initialization
   setwd(file.path(json_folder, m)) #' fromJSON seems to require local directory to load
   message(paste("working on folder: ", getwd()))
   mon_str <- as.Date(paste0(m, '-01'))
   
   #' Load all JSON files in folder 'm'
   TT <- lapply(list.files(pattern = '*json$'), vj_json_load) #' list of JSON data.
   
   #' Bind all together into table
   TT <- rbindlist(TT, idcol = 'page_num', use.names = TRUE, fill = TRUE)
   #' Remove duplicated entires appearing on multiple pages
   TT <- TT[!duplicated(TT, by = names(TT)[!names(TT) %in% c('page_num', 'skills', 'certs')])]
   
   
   #' Create new tables with Skills and Certification separate from job-specific features 
   #' This is a multi-step procedure (a through f) that will recursively unlist these entries
   #'  for which there are multiple per job ID.


   #' (a) Create boolean columns for empty skills | certifications (also add column with month)
   TT[, c('month', 'skills_null', 'certs_null') := 
                                    .(mon_str, is.null(unlist(skills)), is.null(unlist(certs)),
                                      is.character(unlist(skills, recursive = FALSE))), by = id]
   
   #' (b) Some Skills are Stored as Lists and others as Characters (seemingly if there is 1 vs >1 skills per ID)
   #'    - make consistent, by createing boolean TRUE if atomic (character) vector
   TT[(!skills_null), skills_char := is.character(skills[[1]][[2]]), by = id]

   
   #' (c) Handle cases in which the Skill | Certs list is not NULL but NA
   TT[(!skills_null), skills_null := is.na(skills), by = id]
   TT[(!certs_null), certs_null := is.na(certs), by = id]
   
   
   #' Ensure that not all Skills are Missing
   if(!TT[, all(skills_null)]){
      
      #' (d) Create data.table table of Skills for all Job Postings that have skills data
      #'  
      #'  Note: the data.table is created from 2 parts: 
      #'      (i) those in which \items are list and (ii) \items are character
      
      #'  This is likely to break if only \1 (one) type of object contained (not yet tested for this)
      
     T_skills_obs <- rbindlist(list(
                     TT[(!skills_null) & (!skills_char), unlist(skills, recursive = FALSE), by = .(month, id)][,
                                         unlist(items,  recursive = FALSE), by = .(month, id, group)], 
                     TT[(!skills_null) & (skills_char), unlist(skills, recursive = FALSE), by = .(month, id)][,
                         unlist(items,  recursive = FALSE), by = .(month, id, group)]))
     
     setnames(T_skills_obs, 'V1', 'items')
     
     #' (e) Bind observed skills will all NA observations 
     #'      (i.e., include 1 empty row for non-skills job postings)
     ALL_skills <- rbindlist(list(T_skills_obs, 
                                  TT[(skills_null), .(month, id) ]), 
                                  use.names = TRUE, fill = TRUE)
   }else{
     #' Data problem \else when there are no cases of skills. Stop and issue error warning
      stop(paste0("Error: All skills associated with month ", m, " are empty. Review JSON files."))
   }
   
   #' (f) Merge in New Vicinity Groups
   ALL_skills[, items := gsub('\\s+$', '', items)] #' remove trailing white space!
   ALL_skills[, items := gsub('^\\s', '', items)] #' remove leading white space!
   
   #' MERGE relabelling and recategorization information to skill table
   setkey(ALL_skills, items)                       #' set key for left join with mapping file
   ALL_skills <- item_map[ ALL_skills ] #' left join item_map on to All_skills
   
   
   
   #' Check if Vicinity Items are Not Matched - but only worry if there are 10 or more observations
   #' for that item. If not, store and return to later.
   not_matched   <- ALL_skills[is.na(edit_type) & !is.na(items), uniqueN(items), by = group]
   setnames(not_matched, 2, 'N')
   not_matched_N <- sum(not_matched$N)
   
   #' Create table of not matched items for export
   dt_not_matched <- ALL_skills[is.na(edit_type) & !is.na(items), .( uniqueN(id)), by = .(month,group,items)]
   setnames(dt_not_matched, 4, c('N_jp'))
   setkey(dt_not_matched, items, group)
   
   # fwrite(dt_not_matched, file.path(csv_folder, ))
   if(dt_not_matched[N_jp >= 10, .N] > 0){
     fwrite(dt_not_matched[N_jp > 10], file.path(local_dir, 'mapping_files',
                                                 paste0('Missing items - ', m, '.csv')))
     stop(paste0('There are ', dt_not_matched[N_jp > 10, .N], ' items with 10 or more ',
                 'observations.\n Update VJ items mapping file with new file:\n\n  ',
                 '- Missing items - ', m, '.csv'))
   }else{

     fwrite(dt_not_matched, 
            file.path(csv_folder, 'Unmapped Items',
                      paste0('Full List of missing items - ', m, '.csv')))
     
     warning(paste0(not_matched_N, ' items were not matched in mapping file.\n',
                    'However, none were associated with 10 or more online job postings.\n',
                    'Records of unmatched items saved in the "Unmapped Items" subfolder.'))
   }
   
   setnames(ALL_skills, 'edit_type', 'LMIC_Group')
   
   
   #' Old approach - \DEPRECATED:
   #' #' handle non-matched items
   #' if(not_matched_N > 0){
   #'   warning(paste0(not_matched_N, ' items were not matched in the "item_type" file.\n'))
   #'   
   #'   items_over <- not_matched[ group %in% c('Tools and Equipment', 'Technologies'), sum(N)]
   #'   
   #'   fwrite(dt_not_matched,  paste0("//LMIC-Apps01/Projects/DATA/VicinityJobs/item_categorization/tools_n_tech_remappings/",
   #'                                  "Tools_n_Tech-", m, ".csv"))
   #'   
   #'   
   #'   if(items_over > 0){
   #'     warning(paste0('Of these ', items_over, ' items were "Tools and Equipment" or "Technologies"',
   #'                    ' and have been associated with "Tools and technology".\n'))
   #'     
   #'     ALL_skills[is.na(item_type)  & !is.na(items) & 
   #'            group %in% c('Tools and Equipment', 'Technologies'), item_type :=  "Tools and technology"]
   #'     
   #'      
   #'   }
   #'   items_left <-  not_matched_N - items_over
   #'   if(items_left > 0){
   #'     stop(paste0('\n\nError! ', items_left, ' of items could not be matched to skill or work requirement.\n',
   #'                 "These are as follows:\n",
   #'                 ALL_skills[is.na(item_type)  & !is.na(items) & 
   #'                            !(group %in% c('Tools and Equipment', 'Technologies')), 
   #'                            unique(paste0('  - ', items, ' in group = ', group))], '\n\n\n'))
   #'   }
   #'  
   #'  
   #' }
   
   
   # test[is.na(item_type), uniqueN(items), by = group]
   # test[is.na(item_type)]
   # 
   # setdiff(item_map[,unique(items)], test[,unique(items)])

   # test[is.na(item_type) & group %in% c('Tools and Equipment', 'Technologies'), unique(items)]
   # test[is.na(item_type) & group %in% c('Tools and Equipment', 'Technologies'), item_type :=  "Work Requirement"]
   # 
   # test[is.na(item_type) & !group %in% c('Tools and Equipment', 'Technologies'), unique(items)]
   # test[grepl('Advocacy', items), item_type :=  "Skill"]
   # test[items == "Zero-based Budgeting", item_type :=  "Work Requirement"]
   # 
   # test[,uniqueN(items)]
   # 
   # out_update <- rbindlist(list(test[!is.na(items), unique(items), by = .(group, item_type)],
   #                              item_map), use.names = TRUE, fill = TRUE)
   # out_update[is.na(items) , items := V1]
   # out_update[, V1 := NULL]
   # out_update = out_update[!duplicated(out_update)]
   # 
   # setcolorder(out_update, c('group', 'items', 'item_type'))
   # setkey(out_update, group, items)
   # 
   # 
   # out_update[items == 'Ability to Learn', items := 'Ability to Learn    ']
   # 
   # out_update[, items := gsub('\\s+$', '', items)]
   # 
   # fwrite(out_update, "R:/DATA/VicinityJobs/API/Vicinity Items organization - Sept 2019.csv")
   
   #' Extract Table of Certifications
   #' (As of 18 March 2020 LMIC has not looked closely at certifications)
   
   if(!TT[, all(certs_null)]){
     #' Create large table of Certs for all Job Postins with Certs data
     T_Certs_obs <- TT[(!certs_null), unlist(certs, recursive = FALSE), by = .(month, id)]
     
     setnames(T_Certs_obs, 'V1', 'certs')
     #' Bind observed Certs will all NA observations (i.e., 1 empty row for non-Certs job postings)
     ALL_Certs <- rbindlist(list(T_Certs_obs, 
                                  TT[(certs_null), .(month, id) ]), 
                             use.names = TRUE, fill = TRUE)
   }else{
     #' Data problem \else when there are no cases of Certs. Stop and issue error warning
     
     stop(paste0("Error: All Certifications associated with month ", m, " are empty. Review JSON files."))
   }
   
   
   
   
   
   #' Clean Unique-ID table, now called "ALL_T".
   
   #' Remove skills and certifications data 
   ALL_T <- TT[, c('skills', 'certs') := NULL]
   setkey(ALL_T, id, province, devRegion, district, location)
   
   #' Update Alberta Geographies
   ALL_T[province=="AB", district :=  ab_map[.BY[[1]] == dist, unique(cd)], by = district]
   
   #' Update "Search" Employer names with Employer
   ALL_T[empljp=="Search",   empljp := employer]
   
   #' Recode territories (which are ERs too) that are identified as "NT" in each case
   ALL_T[devRegion=="Yukon",    province := "YT"]
   ALL_T[devRegion=="Nunavut",  province := "NU"]
   
   #' In case any non-NT province values exist, remove.
   ALL_T[province=="NT" & devRegion!="Northwest Territories", province := NA]
   
   #' Generate NOC columns to uniquely identify 4-digit and 1-digt
      #' use \regexp and \BY group name to remove all non-numeric characters
   ALL_T[, noc4_code :=   gsub('[^0-9]', '', .BY[[1]]), by = noc] 
   ALL_T[, noc1_code := substr(noc4_code, 1,1)]    #' NOC1 first digit of all cases (NOC1 and NOC4)
   ALL_T[nchar(noc4_code) != 4, noc4_code := NA]   #' Replace non-NOC4 with NA
   ALL_T[noc1_code == '',       noc1_code := NA]   #' Replace non-NOC1 (2 types of "other" categories) with NA

   
   #' Identify new Education level (Management) - which may or may not be grouped with A: Univeristy 
   #' only case of non-University code is:  0621 == Z: Unknown
   setkey(ALL_T, noc4_code)
   ALL_T[substr(noc4_code,1,2) %in% c("00","01","02","03","04","05","06","07","08","09"),
                                               nocSkillLevel := "A: Management"]
      
 
   #' Correct and Clean up Location names

   #' (a) Outer joing JP table with updated names
   ALL_T <- merge(ALL_T, accent_key, all=TRUE, by=c('location','province')) 
   
   #'Check if any locations are misssing in the match. This could happen if new locations are added in vicinity.
   missing <- ALL_T[is.na(source), .(location,location_raw,location_correct,province)]
   missing <- unique(missing)
   missing <- missing[!grepl('Unidentified', location)] #' remove "unidentified" locations
   
   if(missing[, .N] > 0){
   
      fwrite(missing, 
            file.path(csv_folder, 'Unmapped Items',
                      paste0('Full List of missing locations - ', m, '.csv')))
     
     warning(paste0(missing[, .N], ' locations were not matched in mapping file.\n',
                    'These geographies are recorded with their original location names.\n',
                    'Records of unmatched locations saved in the "Unmapped Items" subfolder.'))
   }
   
   ALL_T[!is.na(source), location := location_correct]
   
   
   #' Recode district
   ALL_T[location == "Trail",             district := "Kootenay Boundary"]   # both district and devRegion
   ALL_T[location == "Sainte-Sabine",     district := "Brome-Missisquoi"]    # district only
   ALL_T[location == "Sainte-Monique",    district := "Lac-Saint-Jean-Est"]  # district only
   ALL_T[location == "Saint-Stanislas",   district := "Francheville"]        # both district and devRegion
   ALL_T[location == "Saint-Louis-de-Gonzague", district := "Beauharnois-Salaberry"] # district only
   ALL_T[location == "Lindsay",           district := "Kawartha Lakes"] # district only
   ALL_T[location == "Lac-Simon",         district := "Papineau"]             # both district and devRegion
   ALL_T[location == "King",              district := "York Regional Municipality"] # district only
   ALL_T[location == "Kawartha Lakes",    district := "Kawartha Lakes"] # district only
   ALL_T[location == "Blenheim",          district := "Kent County"]           # both district and devRegion
   ALL_T[province == 'ON' & location == 'Caledonia',   district:='Haldimand County']
   ALL_T[province == 'ON' & location == 'Cayuga',      district:='Haldimand County']
   ALL_T[province == 'ON' & location == 'Delhi',       district:='Norfolk County']
   ALL_T[province == 'ON' & location == 'Dunnville',   district:='Haldimand County']
   ALL_T[province == 'ON' & location == 'Hagersville', district:='Haldimand County']
   ALL_T[province == 'ON' & location == 'Jarvis',      district:='Haldimand County']
   ALL_T[province == 'ON' & location == 'Norfolk',     district:='Norfolk County']
   ALL_T[province == 'ON' & location == 'Port Dover',  district:='Norfolk County']
   ALL_T[province == 'ON' & location == 'Townsend',    district:='Norfolk County']
   ALL_T[province == 'ON' & location == 'Waterford',   district:='Norfolk County']
   ALL_T[province == 'ON' & location == 'Iroquois',    district:='Stormont, Dundas and Glengarry']
   ALL_T[province == 'ON' & location == 'Kemptville',  district:='Leeds and Grenville']
   ALL_T[province == 'ON' & location == 'Lansdowne',   district:='Leeds and Grenville']
   ALL_T[province == 'ON' & location == 'Mallorytown', district:='Leeds and Grenville']
   ALL_T[province == 'ON' & location == 'Napanee',     district:='Lennox and Addington']
   ALL_T[province == 'ON' & location == 'Rockport',    district:='Leeds and Grenville']
   ALL_T[province == 'ON' & location == 'Blandford',   district:='Oxford']
   ALL_T[province == 'ON' & location == 'Saint-Thomas',district:=	'Elgin']
   ALL_T[province == 'ON' & location == 'Anson',       district:= 'Hastings County']
   ALL_T[province == 'ON' & location == 'Bobcaygeon',  district:=	'Kawartha Lakes']
   ALL_T[province == 'ON' & location == 'Campbellford',district:=	'Northumberland County']
   ALL_T[province == 'ON' & location == 'Carden',      district:=	'Kawartha Lakes']
   ALL_T[province == 'ON' & location == 'Cardiff',     district:=	'Haliburton']
   ALL_T[province == 'ON' & location == 'Colborne',    district:=	'Northumberland County']
   ALL_T[province == 'ON' & location == 'Dalton',      district:=	'Kawartha Lakes']
   ALL_T[province == 'ON' & location == 'Eldon',       district:=	'Kawartha Lakes']
   ALL_T[province == 'ON' & location == 'Emily',       district:=	'Kawartha Lakes']
   ALL_T[province == 'ON' & location == 'Fenelon',     district:=	'Kawartha Lakes']
   ALL_T[province == 'ON' & location == 'Laxton',      district:=	'Kawartha Lakes']
   ALL_T[province == 'ON' & location == 'Grafton',     district:=	'Northumberland County']
   ALL_T[province == 'ON' & location == 'Haldimand',   district:=	'Northumberland County']
   ALL_T[province == 'ON' & location == 'Longford',    district:=	'Simcoe']
   ALL_T[province == 'ON' & location == 'Mariposa',    district:=	'Kawartha Lakes']
   ALL_T[province == 'ON' & location == 'Minden',      district:=	'Haliburton']
   ALL_T[province == 'ON' & location == 'Omemee',      district:=	'Kawartha Lakes']
   ALL_T[province == 'ON' & location == 'Percy',       district:=	'Northumberland County']
   ALL_T[province == 'ON' & location == 'Somerville',  district:=	'Kawartha Lakes']
   ALL_T[province == 'ON' & location == 'Stanhope',    district:=	'Lennox and Addington']
   ALL_T[province == 'ON' & location == 'Woodville',   district:=	'Kawartha Lakes']
   ALL_T[province == 'ON' & location == 'Limerick',    district:=	'Stormont, Dundas and Glengarry']
   ALL_T[province == 'ON' & location == 'Snowdon',     district:=	'Haliburton']
   ALL_T[province == 'ON' & location == 'Elora',       district:= 'Wellington County']
   ALL_T[province == 'ON' & location == 'Eramosa',     district:= 'Wellington County']
   ALL_T[province == 'ON' & location == 'Mount Forest', district:= 'Wellington County']
   ALL_T[province == 'ON' & location == 'Grand Valley', district:= 'Dufferin County']
   
   ALL_T[province == 'ON' & location == 'Perth',               district:= 'Lanark County']
   ALL_T[province == 'ON' & location == 'Matheson',            district:= 'Cochrane District']
   ALL_T[province == 'ON' & location == 'Black River',         district:= 'Cochrane District']
   ALL_T[province == 'ON' & location == 'Gogama',              district:= 'Sudbury District']
   
   ALL_T[province == 'ON' & location == 'Bayfield',            district:= 'Huron County']
   ALL_T[province == 'ON' & location == 'Blyth',               district:= 'Huron County']
   ALL_T[province == 'ON' & location == 'Brussels',            district:= 'Huron County']
   ALL_T[province == 'ON' & location == 'Centralia',           district:= 'Huron County']
   ALL_T[province == 'ON' & location == 'Clinton',             district:= 'Huron County']
   ALL_T[province == 'ON' & location == 'Dashwood',            district:= 'Huron County']
   ALL_T[province == 'ON' & location == 'Dungannon',           district:= 'Huron County']
   ALL_T[province == 'ON' & location == 'Exeter',              district:= 'Huron County']
   ALL_T[province == 'ON' & location == 'Fordwich',            district:= 'Huron County']
   ALL_T[province == 'ON' & location == 'Gorrie',              district:= 'Huron County']
   ALL_T[province == 'ON' & location == 'Hensall',             district:= 'Huron County']
   ALL_T[province == 'ON' & location == 'Huron Park',          district:= 'Huron County']
   ALL_T[province == 'ON' & location == 'Kinburn',             district:= 'Huron County']
   ALL_T[province == 'ON' & location == 'Morris',              district:= 'Huron County']
   ALL_T[province == 'ON' & location == 'Seaforth',            district:= 'Huron County']
   ALL_T[province == 'ON' & location == 'Walton',              district:= 'Huron County']
   ALL_T[province == 'ON' & location == 'Wingham',             district:= 'Huron County']
   ALL_T[province == 'ON' & location == 'Woodham',             district:= 'Huron County']
   ALL_T[province == 'ON' & location == 'Wroxeter',            district:= 'Huron County']
   ALL_T[province == 'ON' & location == 'Zurich',              district:= 'Huron County']
   
   ALL_T[province == 'ON' & location == 'Bornholm',            district:= 'Perth County']
   ALL_T[province == 'ON' & location == 'Gowanstown',          district:= 'Perth County']
   ALL_T[province == 'ON' & location == 'St. Marys',           district:= 'Perth County']
   ALL_T[province == 'ON' & location == 'Kirkton',             district:= 'Perth County']
   ALL_T[province == 'ON' & location == 'Listowel',            district:= 'Perth County']
   ALL_T[province == 'ON' & location == 'Millbank',            district:= 'Perth County']
   ALL_T[province == 'ON' & location == 'Milverton',           district:= 'Perth County']
   ALL_T[province == 'ON' & location == 'Mitchell',            district:= 'Perth County']
   ALL_T[province == 'ON' & location == 'Sebringville',        district:= 'Perth County']
   ALL_T[province == 'ON' & location == 'Shakespeare',         district:= 'Perth County']
   
   ALL_T[province == 'ON' & location == 'Singhampton',         district:= 'Simcoe County']
   
   ALL_T[province == 'ON' & location == 'Allenford',           district:= 'Stratford--Bruce Peninsula']
   ALL_T[province == 'ON' & location == 'Arran',               district:= 'Stratford--Bruce Peninsula']
   ALL_T[province == 'ON' & location == 'Bruce Peninsula',     district:= 'Stratford--Bruce Peninsula']
   ALL_T[province == 'ON' & location == 'Chepstow',            district:= 'Stratford--Bruce Peninsula']
   ALL_T[province == 'ON' & location == 'Chesley',             district:= 'Stratford--Bruce Peninsula']
   ALL_T[province == 'ON' & location == 'Elmwood',             district:= 'Stratford--Bruce Peninsula']
   ALL_T[province == 'ON' & location == 'Hepworth',            district:= 'Stratford--Bruce Peninsula']
   ALL_T[province == 'ON' & location == 'Lucknow',             district:= 'Stratford--Bruce Peninsula']
   ALL_T[province == 'ON' & location == 'Mildmay',             district:= 'Stratford--Bruce Peninsula']
   ALL_T[province == 'ON' & location == 'Paisley',             district:= 'Stratford--Bruce Peninsula']
   ALL_T[province == 'ON' & location == 'Point Clark',         district:= 'Stratford--Bruce Peninsula']
   ALL_T[province == 'ON' & location == 'Port Elgin',          district:= 'Stratford--Bruce Peninsula']
   ALL_T[province == 'ON' & location == 'Ripley',              district:= 'Stratford--Bruce Peninsula']
   ALL_T[province == 'ON' & location == 'Sauble Beach',        district:= 'Stratford--Bruce Peninsula']
   ALL_T[province == 'ON' & location == 'Southampton',         district:= 'Stratford--Bruce Peninsula']
   ALL_T[province == 'ON' & location == 'Teeswater',           district:= 'Stratford--Bruce Peninsula']
   ALL_T[province == 'ON' & location == 'Tiverton',            district:= 'Stratford--Bruce Peninsula']
   ALL_T[province == 'ON' & location == 'Tobermory',           district:= 'Stratford--Bruce Peninsula']
   ALL_T[province == 'ON' & location == 'Walkerton',           district:= 'Stratford--Bruce Peninsula']
   ALL_T[province == 'ON' & location == 'Wiarton',             district:= 'Stratford--Bruce Peninsula']
   ALL_T[province == 'ON' & location == 'Whitechurch',         district:= 'Stratford--Bruce Peninsula']
   
   ALL_T[province == 'QC' & location == 'La Nouvelle-Beauce', district:='Chaudière-Appalaches']
   ALL_T[province == 'AB' & location == 'Grandin',    district:= 'Division 12']
   
   # Recode devRegion
   ALL_T[location == "Trail",             devRegion := "Kootenay"] # both district and devRegion
   ALL_T[location == "Saint-Stanislas",   devRegion := "Mauricie"] # both district and devRegion
   ALL_T[location == "Saint Elzéar",      devRegion := "Chaudière-Appalaches"] # devRegion only
   ALL_T[location == "Saint-Cyprien",     devRegion := "Bas-Saint-Laurent"] # devRegion only
   ALL_T[location == "Saint Augustin",    devRegion := "Côte-Nord"] # devRegion only
   ALL_T[location == "Montcalm",          devRegion := "Lanaudière"] # devRegion only
   ALL_T[location == "Lac-Simon",         devRegion := "Outaouais"] # both district and devRegion
   ALL_T[location == "Kawawachikamach",   devRegion := "Côte-Nord"] # devRegion only
   ALL_T[location == "Blenheim",          devRegion := "Windsor--Sarnia"] # both district and devRegion
   
   #' Location with two names for the same place
   ALL_T[province == 'ON' & location == 'Saint Thomas', location:=	'St. Thomas']
   ALL_T[province == 'ON' & location == 'St George',    location:=	'St. George']
   
   #' Correct Typos Found
   ALL_T[province == 'ON' & district == 'Waterloo Regional Muncicipality', district:=	'Waterloo Regional Municipality']
   ALL_T[province == 'QC' & district == 'Le Saguenary-et-son-Fjord',       district:= 'Le Saguenay-et-son-Fjord']
   
   
   #' remove unnecessary files
   rm(TT, T_skills_obs, T_Certs_obs)
   
   
   #' Prep for Export:
   #' Set column order
   setcolorder(ALL_T,
               c("id", "month", "dateFound", 
                 "province","devRegion", "district", "location", 
                 "noc", "nocSkillLevel","noc4_code", "noc1_code",
                 "jobTitle", "empljp", "employer", "naics",
                 "experience", "education", "advertisedBy", 
                 "dataSource", "type", "duration", 
                 "remuneration.min", "remuneration.max", 
                 "remuneration.per", "remuneration.hrly",
                 "page_num", "skills_null", "certs_null"))
   setcolorder(ALL_skills,
               c("id", "month", "items", "group", "LMIC_Label", "LMIC_Group"))
   
   setcolorder(ALL_Certs,
               c("id", "month", 'certs'))
   
   #' set row order
   setkey(ALL_T,      month,  id)
   setkey(ALL_skills, month,  id)
   setkey(ALL_Certs,  month,  id)
   
   
   message("completed, saving new file")
     
   
   #' Export unique ID table and Skills Table
   saveRDS(ALL_T,      file = paste0(csv_folder, '/vicinity-', m, "-ALL_T.rds"))
   saveRDS(ALL_skills, file = paste0(csv_folder, '/vicinity-', m, "-ALL_skills.rds"))
   saveRDS(ALL_Certs,  file = paste0(csv_folder, '/vicinity-', m, "-ALL_Certs.rds"))

   tictoc::toc()
   
   #'   rm(ALL_T, ALL_skills, ALL_Certs); gc()  #' run in each loop if memory becomes an issue
   
   
 }
 
#' return
setwd(save_wd)
