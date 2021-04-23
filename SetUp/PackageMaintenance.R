
cohort.diag.path<-"/home/eburn/CovCoagOutcomeDiagnostics-main/diagCovCoagOutcomes"
# path to the cohort diagnostics package

# copy in cohorts
# remove existing
unlink(paste0(here("OutcomeCohorts", "settings"), "/*"))
unlink(paste0(here("OutcomeCohorts", "json"), "/*"))
unlink(paste0(here("OutcomeCohorts", "sql"), "/*"))
#bring in current

jsons<-list.files(paste0(cohort.diag.path, "/inst/cohorts"))
# drop hosp cohorts
jsons<-jsons[str_detect(jsons,"hosp", negate = TRUE)]
jsons<-jsons[str_detect(jsons,"InclusionRules", negate = TRUE)]
for(i in 1:length(jsons)){
  file.copy(from=paste0(cohort.diag.path, "/inst/cohorts/",jsons[i]), 
            to=here("OutcomeCohorts", "json"), 
            overwrite = TRUE, recursive = FALSE, 
            copy.mode = TRUE)
}

sqls<-list.files(paste0(cohort.diag.path, "/inst/sql/sql_server"))
# drop hosp cohorts
sqls<-sqls[str_detect(sqls,"hosp", negate = TRUE)]
for(i in 1:length(sqls)){
  file.copy(from=paste0(cohort.diag.path, "/inst/sql/sql_server/",sqls[i]), 
            to=here("OutcomeCohorts", "sql"), 
            overwrite = TRUE, recursive = FALSE, 
            copy.mode = TRUE)
}
