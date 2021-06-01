source(here("Functions", "Functions.R"))

# link to db tables -----
person_db<-tbl(db, sql(paste0("SELECT * FROM ",
                       cdm_database_schema,
                       ".person")))
observation_period_db<-tbl(db, sql(paste0("SELECT * FROM ",
                       cdm_database_schema,
                       ".observation_period")))
visit_occurrence_db<-tbl(db, sql(paste0("SELECT * FROM ",
                       cdm_database_schema,
                       ".visit_occurrence")))
condition_era_db<-tbl(db, sql(paste0("SELECT * FROM ",
                       cdm_database_schema,
                       ".condition_era")))
drug_era_db<-tbl(db, sql(paste0("SELECT * FROM ",
                       cdm_database_schema,
                       ".drug_era")))
concept_ancestor_db<-tbl(db, sql(paste0("SELECT * FROM ",
                       cdm_database_schema,
                       ".concept_ancestor")))



# specifications ----
years.of.interest<-c("all")
# years.of.interest<-c("all", 2017,2018,2019)

exposure.pop.defs<-c(1,2,3)
# 1 from start of calendar year
# 2 from visit
# 3 from visit, with TAR censored at 28 days post visit
# exposure.pop.defs<-c(1) # for now just the primary cohort of interest

prior.hist.req<-c(1,2)
# 1 no restriction
# 2 prior year required
# prior.hist.req<-1 # for now, just run for the former



cond.codes<-c("434621", 
                  "4098292", 
                  "4125650",  
                  "317009",   
                  "313217",   
                  "443392", 
                  "201820",
                  "433736",
                  "321588",
                  "316866",
                  "4030518",
                  "255573",
                  "4182210")
cond.names<-c("autoimmune_disease",
              "antiphospholipid_syndrome",
              "thrombophilia",
              "asthma",
              "atrial_fibrillation",
              "malignant_neoplastic_disease",
              "diabetes_mellitus",
              "obesity",
              "heart_disease",
              "hypertensive_disorder",
              "renal_impairment",
              "copd",
              "dementia")
# these are the conditions we´ll extract for our table 1

drug.codes<-c("21603933", 
                  "21603991",
                  "21602722",
                  "21600961",
                  "21601853",
                  "21601386",
                  "21602472",
                  "21603831",
                  "21602471")
drug.names<-c("antiinflamatory_and_antirheumatic", 
                  "coxibs",
                  "corticosteroids",
                  "antithrombotic",
                  "lipid_modifying",
                  "antineoplastic_immunomodulating",
                  "hormonal_contraceptives",
                  "tamoxifen",
                  "sex_hormones_modulators")
# these are the medications we´ll extract for our table 1

# instantiate outcome tables -----
cohort.sql<-list.files(here("OutcomeCohorts", "sql"))
cohort.sql<-cohort.sql[cohort.sql!="CreateCohortTable.sql"]
outcome.cohorts<-tibble(id=1:length(cohort.sql),
                        file=cohort.sql,
                        name=str_replace(cohort.sql, ".sql", ""))  


if(create.outcome.cohorts=="FALSE"){
print(paste0("- Skipping creating outcome cohorts"))
} else { 
print(paste0("- Getting outcomes"))

  
conn <- connect(connectionDetails)
# create empty cohorts table
print(paste0("Create empty cohort table")) 
sql<-readSql(here("OutcomeCohorts", "sql","CreateCohortTable.sql"))
sql<-SqlRender::translate(sql, targetDialect = targetDialect)
renderTranslateExecuteSql(conn=conn, 
                          sql,
                          cohort_database_schema =  results_database_schema,
                          cohort_table = cohortTableOutcomes)
rm(sql)

for(cohort.i in 1:length(outcome.cohorts$id)){
  
  working.id<-outcome.cohorts$id[cohort.i]

  print(paste0("- Getting outcome: ", 
               outcome.cohorts$name[cohort.i],
              " (", cohort.i, " of ", length(outcome.cohorts$name), ")"))
  sql<-readSql(here("OutcomeCohorts", "sql",
                    outcome.cohorts$file[cohort.i])) 
  
  sql <- sub("BEGIN: Inclusion Impact Analysis - event.*END: Inclusion Impact Analysis - person", "", sql)
  
  sql<-SqlRender::translate(sql, targetDialect = targetDialect)
  renderTranslateExecuteSql(conn=conn, 
                          sql, 
                          cdm_database_schema = cdm_database_schema,
                          vocabulary_database_schema = cdm_database_schema,
                          target_database_schema = results_database_schema,
                          # results_database_schema = results_database_schema,
                          target_cohort_table = cohortTableOutcomes,
                          target_cohort_id = working.id)  
  }
disconnect(conn)
} 
# link 
outcome_db<-tbl(db, sql(paste0("SELECT * FROM ",
                       results_database_schema,
                       ".", cohortTableOutcomes)))    

# drop any outcome cohorts with less than 5 people
outcome.cohorts<-outcome.cohorts %>% 
  inner_join(outcome_db %>% 
  group_by(cohort_definition_id) %>% 
  tally() %>% 
  collect() %>% 
  filter(n>5) %>% 
  select(cohort_definition_id),
  by=c("id"="cohort_definition_id"))  
# all those instantiated from outcome diagnostics

# specify which events we´ll also combine with thrombocytopenia ------
outcome.cohorts.thromb.10_10<-outcome.cohorts %>% 
  filter(name %in% 
         c("all stroke","CVST","DIC",
           "DVT broad","DVT narrow",
           "VTE broad", "VTE narrow",
           "PE", "hem stroke",
           "hepatic vein" ,"intest infarc",
           "isc stroke", "IVT", "MACE",
           "MI isc stroke", "PE",
           "portal vein","splenic infarc",
           "SVT" , "visc venous","VTE broad" ,"VTE narrow", 
            "splenic vein", "splenic artery",
            "splenic infarc", "hepatic artery", 
                    "intest infarc",
                     "mesen vein", "CAT"))
outcome.cohorts.thromb.10_10$name<-paste0(outcome.cohorts.thromb.10_10$name, " (with thrombocytopenia 10 days pre to 10 days post)")

outcome.cohorts.thromb.42_14<-outcome.cohorts %>% 
  filter(name %in% 
           c("all stroke","CVST","DIC",
           "DVT broad","DVT narrow",
           "VTE broad", "VTE narrow",
           "PE", "hem stroke",
           "hepatic vein" ,"intest infarc",
           "isc stroke", "IVT", "MACE",
           "MI isc stroke", "PE",
           "portal vein","splenic infarc",
           "SVT" , "visc venous","VTE broad" ,"VTE narrow", 
            "splenic vein", "splenic artery",
            "splenic infarc", "hepatic artery", 
                    "intest infarc",
                     "mesen vein", "CAT"))
outcome.cohorts.thromb.42_14$name<-paste0(outcome.cohorts.thromb.42_14$name, " (with thrombocytopenia 42 days pre to 14 days post)")

# add to outcomes
outcome.cohorts<-bind_rows(outcome.cohorts,
outcome.cohorts.thromb.10_10,
outcome.cohorts.thromb.42_14)



# run analysis ----
# Initiate lists to store output
Patient.characteristcis<-list()
Patient.characteristcis.for.plotting<-list()
IR.summary<-list()

for(i in 1:length(years.of.interest)){
for(e in 1:length(exposure.pop.defs)){ # type of exposure pop 
for(o in 1:length(prior.hist.req)){  # with and without requirement for year of prior history

print(paste0("- Getting year: ", years.of.interest[i]))

working.exposure.pop.def<- exposure.pop.defs[[e]]
  
if(working.exposure.pop.def==1){ 
pop.type<-"general.pop.all"  
# i.e. we go from the start of the year for all
  }
if(working.exposure.pop.def==2){ 
pop.type<-"general.pop.with.visit" 
# here we go from the first observed visit
}  
if(working.exposure.pop.def==3){ 
pop.type<-"general.pop.with.visit.28.days" 
# here we go from the first observed visit, and then just follow up for 28 days
  } 
  
# set working.year
if(years.of.interest[i]=="all"){
  # for all, we start in 2017
working.year<-2017
working.start.date<-as.Date(dmy(paste0("01-01-","2017")))
} else {
working.year<-as.numeric(years.of.interest[i])
working.start.date<-as.Date(dmy(paste0("01-01-",working.year)))
}
 
#set end date  
if(years.of.interest[i]=="all"){
end.date<-as.Date(dmy(paste0("31-12-","2019")))
}
if(years.of.interest[i]=="2017"){
end.date<-as.Date(dmy(paste0("31-12-","2017")))
}
if(years.of.interest[i]=="2018"){
end.date<-as.Date(dmy(paste0("31-12-","2018")))
  }
if(years.of.interest[i]=="2019"){
end.date<-as.Date(dmy(paste0("31-12-","2019")))
  }

  
  
  
# study population -----
# define our study population using the sql in the Cohorts folder
# the accompanying folder has the jsons (which can be imported into Atlas)
  
# The population are identified and added to the cohortTableExposures table
  
# connect to database
conn <- connect(connectionDetails)
# create empty cohorts table
print(paste0("Create empty cohort table")) 
sql<-readSql(here("ExposureCohorts", "sql","CreateCohortTable.sql"))
sql<-SqlRender::translate(sql, targetDialect = targetDialect)
renderTranslateExecuteSql(conn=conn, 
                          sql,
                          cohort_database_schema =  results_database_schema,
                          cohort_table = cohortTableExposures)
rm(sql)
  
if(working.year==2017){
if(working.exposure.pop.def==1){  
sql<-readSql(here("ExposureCohorts", "sql","general_pop_2017.sql"))
}
if(working.exposure.pop.def %in% c(2,3) & years.of.interest[i]=="all"){  
sql<-readSql(here("ExposureCohorts", "sql","general_pop_visit_2017_to_2019.sql"))  # to fix- max 2019
}
if(working.exposure.pop.def %in% c(2,3) & years.of.interest[i]=="2017"){  
sql<-readSql(here("ExposureCohorts", "sql","general_pop_visit_2017_only.sql")) 
}  
sql<-SqlRender::translate(sql, targetDialect = targetDialect)
renderTranslateExecuteSql(conn=conn, 
                          sql, 
                          cdm_database_schema = cdm_database_schema,
                          target_database_schema = results_database_schema,
                          target_cohort_table = cohortTableExposures,
                          target_cohort_id = 1)  
}  

if(working.year==2018){
if(working.exposure.pop.def==1){
sql<-readSql(here("ExposureCohorts", "sql","general_pop_2018.sql")) 
}
if(working.exposure.pop.def %in% c(2,3)){  
sql<-readSql(here("ExposureCohorts", "sql","general_pop_visit_2018_only.sql")) 
}
sql<-SqlRender::translate(sql, targetDialect = targetDialect)
renderTranslateExecuteSql(conn=conn, 
                          sql, 
                          cdm_database_schema = cdm_database_schema,
                          target_database_schema = results_database_schema,
                          target_cohort_table = cohortTableExposures,
                          target_cohort_id = 1)  
}  
if(working.year==2019){
if(working.exposure.pop.def==1){
sql<-readSql(here("ExposureCohorts", "sql","general_pop_2019.sql")) 
}
if(working.exposure.pop.def %in% c(2,3)){  
sql<-readSql(here("ExposureCohorts", "sql","general_pop_visit_2019_only.sql")) 
} 
sql<-SqlRender::translate(sql, targetDialect = targetDialect)
renderTranslateExecuteSql(conn=conn, 
                          sql, 
                          cdm_database_schema = cdm_database_schema,
                          target_database_schema = results_database_schema,
                          target_cohort_table = cohortTableExposures,
                          target_cohort_id = 1)  
}  
disconnect(conn)
  
cohortTableExposures_db<-tbl(db, sql(paste0("SELECT * FROM ",
                       results_database_schema,
                       ".", cohortTableExposures)))
# Create Pop df ----  
Pop<-person_db %>% 
  inner_join(cohortTableExposures_db %>% 
               select(subject_id,cohort_start_date) %>% 
               rename("person_id"="subject_id")) %>% 
     select(person_id,gender_concept_id, 
            year_of_birth, month_of_birth, day_of_birth,
            cohort_start_date) %>% 
  left_join(observation_period_db %>% 
     select("person_id",  "observation_period_start_date", "observation_period_end_date")) %>% 
     collect()

# add age and gender -----
Pop$age<- NA
if(sum(is.na(Pop$day_of_birth))==0 & sum(is.na(Pop$month_of_birth))==0){
 # if we have day and month 
Pop<-Pop %>%
  mutate(age=floor(as.numeric((ymd(cohort_start_date)-
                    ymd(paste(year_of_birth,
                                month_of_birth,
                                day_of_birth, sep="-"))))/365.25))
} else { 
Pop<-Pop %>% 
  mutate(age= year(cohort_start_date)-year_of_birth)
}

Pop<-Pop %>% 
  mutate(age_gr=ifelse(age<20,  "<20",
                ifelse(age>=20 &  age<=44,  "20-44",
                ifelse(age>=45 & age<=54,  "45-54",
                ifelse(age>=55 & age<=64,  "55-64",
                ifelse(age>=65 & age<=74, "65-74", 
                ifelse(age>=75 & age<=84, "75-84",      
                ifelse(age>=85, ">=85",
                       NA)))))))) %>% 
  mutate(age_gr= factor(age_gr, 
                   levels = c("<20", "20-44","45-54", "55-64",
                              "65-74", "75-84",">=85")))
# wider age groups
Pop<-Pop %>% 
  mutate(age_gr2=ifelse(age<=44,  "<=44",
                 ifelse(age>=45 & age<=64,  "45-64",    
                 ifelse(age>=55, ">=65",
                       NA)))) %>% 
  mutate(age_gr2= factor(age_gr2, 
                   levels = c("<=44", "45-64",">=65")))

# another alternative set of age groups
Pop<-Pop %>% 
  mutate(age_gr3=ifelse(age<20,  "<20",
                ifelse(age>=20 &  age<=29,  "20-29",
                ifelse(age>=30 &  age<=39,  "30-39",
                ifelse(age>=40 &  age<=49,  "40-49",
                ifelse(age>=50 &  age<=59,  "50-59",
                ifelse(age>=60 & age<=69,  "60-69",
                ifelse(age>=70 & age<=79,  "70-79",      
                ifelse(age>=80, ">=80",
                       NA))))))))) %>% 
  mutate(age_gr3= factor(age_gr3, 
                   levels = c("<20", "20-29","30-39","40-49", "50-59",
                              "60-69", "70-79",">=80")))



# gender
#8507 male
#8532 female
Pop<-Pop %>% 
  mutate(gender= ifelse(gender_concept_id==8507, "Male",
                 ifelse(gender_concept_id==8532, "Female", NA ))) %>% 
  mutate(gender= factor(gender, 
                   levels = c("Male", "Female")))


# if missing age or gender, drop
Pop<-Pop %>% 
  filter(!is.na(age))

Pop<-Pop %>% 
  filter(!is.na(gender))

# add prior observation time -----
Pop<-Pop %>%  
  mutate(prior_obs_days=as.numeric(difftime(cohort_start_date,
                                          observation_period_start_date,
                                        units="days"))) %>% 
  mutate(prior_obs_years=prior_obs_days/365.25)

if(o==1){
print(paste0("- Without requirement for prior observation time"))
    } else {
print(paste0("- With requirement for prior observation time"))
    Pop<-Pop %>% 
      filter(prior_obs_years>=1)
    }

# condition history ------

# add the concept ids of interest to the cohortTableProfiles table in the results 
# schema in the cdm
# these will be the code of interest and all of its descendants

# get any instances over prior history
# for everyone in the database (everyone in the condition_occurrence table),
# left_join to population of interest we´ve established

print(paste0("-- Getting codes for conditions"))
conn<-connect(connectionDetails)
# table with all the concept ids of interest (code and descendants)
insertTable(connection=conn,
            tableName=paste0(results_database_schema, ".",cohortTableProfiles),
            data=data.frame(condition_id=integer(), concept_id =integer()),
            createTable = TRUE,
            progressBar=FALSE)
for(n in 1:length(cond.codes)){ # add codes for each condition
working.code<-cond.codes[n]
working.name<-cond.names[n]
sql<-paste0("INSERT INTO ", results_database_schema, ".",cohortTableProfiles, " (condition_id, concept_id) SELECT DISTINCT ", n ,", descendant_concept_id FROM ", cdm_database_schema, ".concept_ancestor WHERE ancestor_concept_id IN (",working.code, ");")
suppressMessages(executeSql(conn, sql, progressBar = FALSE))
}

#link to table
cohortTableProfiles_db<-tbl(db, sql(paste0("SELECT * FROM ",
                       results_database_schema,
                       ".", cohortTableProfiles)))
print(paste0("-- Getting conditions for study population"))

#people with at least one of the conditions
cond.persons <- condition_era_db %>%
  select(person_id, condition_concept_id, condition_era_start_date) %>% 
  inner_join(cohortTableProfiles_db ,
             by=c("condition_concept_id"="concept_id")) %>% 
  inner_join(cohortTableExposures_db %>% 
               select(subject_id, cohort_start_date) %>% 
               rename("person_id"="subject_id"),
             by = "person_id") %>% 
  filter(condition_era_start_date < cohort_start_date) %>% 
  select(person_id, condition_id) %>% 
  distinct() %>% 
  collect() 

# add each condition to pop
for(n in 1:length(cond.codes)){# add each to Pop
working.code<-cond.codes[n]
working.name<-cond.names[n]

working.persons <- cond.persons %>% 
  filter(condition_id==n) %>% 
  select(person_id) %>% 
  mutate(working.cond=1)

if(nrow(working.persons)>0){
Pop<-Pop %>%
  left_join(working.persons,
             by = "person_id") %>% 
  rename(!!working.name:="working.cond")
} else {
 Pop$working.cond<-0
 Pop<-Pop %>% 
  rename(!!working.name:="working.cond")
}

}
disconnect(conn)

#to zero if absent
Pop<-Pop %>%
  mutate(across(all_of(cond.names), ~ replace_na(.x, 0)))


# medication history ----
# 183 days prior to four days prior index date

print(paste0("-- Getting codes for medications"))
conn<-connect(connectionDetails)
# table with all the concept ids of interest (code and descendants)
insertTable(connection=conn,
            tableName=paste0(results_database_schema, ".",cohortTableProfiles),
            data=data.frame(drug_id=integer(), concept_id =integer()),
            createTable = TRUE,
            progressBar=FALSE)
for(n in 1:length(drug.codes)){ # add codes for each condition
working.code<-drug.codes[n]
working.name<-drug.names[n]
sql<-paste0("INSERT INTO ", results_database_schema, ".",cohortTableProfiles, " (drug_id, concept_id) SELECT DISTINCT ", n ,", descendant_concept_id FROM ", cdm_database_schema, ".concept_ancestor WHERE ancestor_concept_id IN (",working.code, ");")
suppressMessages(executeSql(conn, sql, progressBar = FALSE))
}

#link to table
cohortTableProfiles_db<-tbl(db, sql(paste0("SELECT * FROM ",
                       results_database_schema,
                       ".", cohortTableProfiles)))
print(paste0("-- Getting  medications for study population"))

med.persons <- drug_era_db %>%
        filter(drug_era_start_date>as.Date("2015-01-01")) %>%  
  select(person_id, drug_concept_id, drug_era_start_date, drug_era_end_date) %>% 
  inner_join(cohortTableProfiles_db ,
             by=c("drug_concept_id"="concept_id")) %>% 
  inner_join(cohortTableExposures_db %>% 
               select(subject_id, cohort_start_date) %>% 
               rename("person_id"="subject_id"),
             by = "person_id") %>% 
  filter(drug_era_start_date < cohort_start_date) %>% 
  select(person_id, drug_id, drug_era_start_date, drug_era_end_date,
         cohort_start_date) %>% 
  distinct() %>% 
  collect() %>%
  filter(drug_era_start_date<=(cohort_start_date-days(4))
          & drug_era_start_date>=(cohort_start_date-days(183)) |
         drug_era_end_date<=(cohort_start_date-days(4))
                 & drug_era_end_date>=(cohort_start_date-days(183))) %>% 
  select(person_id, drug_id) %>% 
  distinct() 



for(n in 1:length(drug.codes)){# extract condition info and add to Pop
working.code<-drug.codes[n]
working.name<-drug.names[n]
working.persons <- med.persons %>% 
  filter(drug_id==n) %>% 
  select(person_id) %>% 
  mutate(working.drug=1)

if(nrow(working.persons)>0){
Pop<-Pop %>%
  left_join(working.persons,
             by = "person_id") %>% 
  rename(!!working.name:="working.drug")
} else {
 Pop$working.drug<-0
 Pop<-Pop %>% 
  rename(!!working.name:="working.drug")
}

}
disconnect(conn)

#to zero if absent
Pop<-Pop %>%
  mutate(across(all_of(drug.names), ~ replace_na(.x, 0)))


# summarise characteristics -----
summary.characteristics<-bind_rows(
data.frame(Overall=t(Pop %>% 
  mutate(index_year=year(cohort_start_date)) %>% 
  summarise(n=nice.num.count(length(person_id)),
    age=paste0(nice.num.count(median(age)),  " [",
                     nice.num.count(quantile(age,probs=0.25)),  " to ",
                     nice.num.count(quantile(age,probs=0.75)),   "]" ),
    sex.male=paste0(nice.num.count(sum(gender=="Male")),
                    " (",  nice.num((sum(gender=="Male")/length(person_id))*100),
    "%)"),
  prior_obs_years=paste0(nice.num(median(prior_obs_years)),
                     " [", nice.num(quantile(prior_obs_years,probs=0.25)), 
                     " to ", nice.num(quantile(prior_obs_years,probs=0.75)),   "]" ),
  index.year_2017= paste0(nice.num.count(sum(index_year==2017)), 
                          " (", nice.num((sum(index_year==2017)/length(person_id))*100), "%)"),
  index.year_2018= paste0(nice.num.count(sum(index_year==2018)), 
                          " (", nice.num((sum(index_year==2018)/length(person_id))*100), "%)"),
  index.year_2019= paste0(nice.num.count(sum(index_year==2019)), 
                          " (", nice.num((sum(index_year==2019)/length(person_id))*100), "%)"))
  )),
# and all the conds and medications
data.frame(Overall=t(Pop %>% 
  summarise_at(.vars = all_of(c(cond.names, drug.names)), 
               .funs = function(x, tot){
  paste0(nice.num.count(sum(x)), " (", nice.num((sum(x)/tot)*100), "%)")
} , tot=nrow(Pop))) ))
summary.characteristics$Overall<-as.character(summary.characteristics$Overall)

rownames(summary.characteristics)<-str_to_sentence(rownames(summary.characteristics))
rownames(summary.characteristics)<-str_replace(rownames(summary.characteristics),
                                      "Sex.male", "Sex: Male")
rownames(summary.characteristics)<-str_replace(rownames(summary.characteristics),
                                      "Prior_obs_years", "Years of prior observation time")
rownames(summary.characteristics)<-str_replace(rownames(summary.characteristics),
                                      "Index.year_", "Index year: ")
rownames(summary.characteristics)<-str_replace_all(rownames(summary.characteristics) , "_", " ")

#obscure any counts less than 5
summary.characteristics$Overall<-
  ifelse(str_sub(summary.characteristics$Overall, 1, 2) %in%  c("1 ","2 ", "3 ","4 "),
              "<5",summary.characteristics$Overall)
summary.characteristics$var<-row.names(summary.characteristics)
row.names(summary.characteristics)<-1:nrow(summary.characteristics)

Pop.summary.characteristics<-summary.characteristics


Patient.characteristcis.for.plotting[[paste0("exposure population",";",years.of.interest[i],";",o,";",pop.type,";","age_gr")]]<- Pop %>% 
  group_by(age_gr) %>% 
  tally() %>% 
  mutate(group="exposure population") %>% 
  mutate(type="age_gr") %>% 
  mutate(study.year=years.of.interest[i]) %>% 
  mutate(prior.obs.required=ifelse(o==1, "No", "Yes")) %>% 
  mutate(pop.type=pop.type)

Patient.characteristcis.for.plotting[[paste0("exposure population",";",years.of.interest[i],";",o,";",pop.type,";","age_gr_gender")]]<- Pop %>% 
  group_by(age_gr,gender) %>% 
  tally() %>% 
  mutate(group="exposure population") %>% 
  mutate(type="age_gr_gender") %>% 
  mutate(study.year=years.of.interest[i]) %>% 
  mutate(prior.obs.required=ifelse(o==1, "No", "Yes")) %>% 
  mutate(pop.type=pop.type)

Patient.characteristcis.for.plotting[[paste0("exposure population",";",years.of.interest[i],";",o,";",pop.type,";","age_gr2")]]<- Pop %>% 
  group_by(age_gr2) %>% 
  tally() %>% 
  mutate(group="exposure population") %>% 
  mutate(type="age_gr2") %>% 
  mutate(study.year=years.of.interest[i]) %>% 
  mutate(prior.obs.required=ifelse(o==1, "No", "Yes")) %>% 
  mutate(pop.type=pop.type)

Patient.characteristcis.for.plotting[[paste0("exposure population",";",years.of.interest[i],";",o,";",pop.type,";","age_gr2_gender")]]<- Pop %>% 
  group_by(age_gr2,gender) %>% 
  tally() %>% 
  mutate(group="exposure population") %>% 
  mutate(type="age_gr2_gender") %>% 
  mutate(study.year=years.of.interest[i]) %>% 
  mutate(prior.obs.required=ifelse(o==1, "No", "Yes")) %>% 
  mutate(pop.type=pop.type)

Patient.characteristcis.for.plotting[[paste0("exposure population",";",years.of.interest[i],";",o,";",pop.type,";","age_gr3")]]<- Pop %>% 
  group_by(age_gr3) %>% 
  tally() %>% 
  mutate(group="exposure population") %>% 
  mutate(type="age_gr3") %>% 
  mutate(study.year=years.of.interest[i]) %>% 
  mutate(prior.obs.required=ifelse(o==1, "No", "Yes")) %>% 
  mutate(pop.type=pop.type)

Patient.characteristcis.for.plotting[[paste0("exposure population",";",years.of.interest[i],";",o,";",pop.type,";","age_gr3_gender")]]<- Pop %>% 
  group_by(age_gr3,gender) %>% 
  tally() %>% 
  mutate(group="exposure population") %>% 
  mutate(type="age_gr3_gender") %>% 
  mutate(study.year=years.of.interest[i]) %>% 
  mutate(prior.obs.required=ifelse(o==1, "No", "Yes")) %>% 
  mutate(pop.type=pop.type)

Patient.characteristcis.for.plotting[[paste0("exposure population",";",years.of.interest[i],";",o,";",pop.type,";","gender")]]<- Pop %>% 
  group_by(gender) %>% 
  tally() %>% 
  mutate(group="exposure population") %>% 
  mutate(type="gender") %>% 
  mutate(study.year=years.of.interest[i]) %>% 
  mutate(prior.obs.required=ifelse(o==1, "No", "Yes")) %>% 
  mutate(pop.type=pop.type)


# working outcome -----
for(j in 1:length(outcome.cohorts$id)){ # for each outcome of interest
working.outcome<-outcome.cohorts$id[j]
working.outcome.name<-outcome.cohorts$name[j]

print(paste0("- Getting ", working.outcome.name,
        " (", j, " of ", length(outcome.cohorts$id), ")"))
working.Pop<-Pop 
  
# drop time-varying covariates, we will get them again for the index date of the event 
working.Pop<-working.Pop %>% 
  select(-age, age_gr, age_gr2, age_gr2, prior_obs_years) %>% 
  select(-all_of(cond.names)) %>% 
  select(-all_of(drug.names))

# event of interest ------
if(working.outcome.name=="imm throm"){
  # for imm throm, either this specific cohort based on diagnosis codes or HIT (which included drug eras)
 ids<-c(outcome.cohorts %>% 
          filter(name=="HIT") %>% 
          select(id) %>% pull(),
        working.outcome)
  working.outcomes<-outcome_db %>%
    filter(cohort_definition_id %in% 
             ids) %>%
    select(subject_id, cohort_start_date) %>% 
    collect()
} else {
working.outcomes<-outcome_db %>%
  filter(cohort_definition_id %in% working.outcome) %>%
  select(subject_id, cohort_start_date) %>% 
  collect()
}

# thrombocytopenia window ----
if(str_detect(working.outcome.name, "(with thrombocytopenia 10 days pre to 10 days post)")){
  thrombocyt.id<-outcome.cohorts %>% 
                   filter(name=="thrombocyt") %>% 
                   select(id) %>% pull() 
  thromb.outcomes<-outcome_db %>%
    filter(cohort_definition_id ==thrombocyt.id) %>% 
    select(subject_id, cohort_start_date) %>% 
    rename("thromb.date"="cohort_start_date") %>% 
    collect()
  # find any outcomes with thrombocytopenia also observed
  working.outcomes<-working.outcomes %>% 
    inner_join(thromb.outcomes)
  # find any outcomes with thrombocytopenia in the time window
  working.outcomes$dtime<-as.numeric(difftime(working.outcomes$thromb.date,
                                              working.outcomes$cohort_start_date, units="days"))
  working.outcomes<-working.outcomes %>% 
    filter(dtime>=(-10)) %>% 
    filter(dtime<=10)
  
  working.outcomes<-working.outcomes %>% 
    select(-dtime) %>% 
    select(-thromb.date)
}

if(str_detect(working.outcome.name, "(with thrombocytopenia 42 days pre to 14 days post)")){
  thrombocyt.id<-outcome.cohorts %>% 
    filter(name=="thrombocyt") %>% 
    select(id) %>% pull() 
  thromb.outcomes<-outcome_db %>%
    filter(cohort_definition_id ==thrombocyt.id) %>% 
    select(subject_id, cohort_start_date) %>% 
    rename("thromb.date"="cohort_start_date") %>% 
    collect()
  # find any outcomes with thrombocytopenia also observed
  working.outcomes<-working.outcomes %>% 
    inner_join(thromb.outcomes)
  # find any outcomes with thrombocytopenia in the time window
  working.outcomes$dtime<-as.numeric(difftime(working.outcomes$thromb.date,
                                              working.outcomes$cohort_start_date, units="days"))
  working.outcomes<-working.outcomes %>% 
    filter(dtime>=(-42)) %>% 
    filter(dtime<=14)
  
  working.outcomes<-working.outcomes %>% 
    select(-dtime) %>% 
    select(-thromb.date)
}


# drop anyone with the outcome in the year prior to the index ------
washout.outcomes<-working.outcomes %>% 
  inner_join(working.Pop %>% 
               select(person_id,cohort_start_date) %>% 
               rename("subject_id"="person_id") %>% 
               rename("Pop_cohort_start_date"="cohort_start_date")) %>% 
  filter(cohort_start_date>= (Pop_cohort_start_date-years(1))) %>% 
  filter(cohort_start_date<= Pop_cohort_start_date) 

working.Pop<-working.Pop %>% 
  anti_join(washout.outcomes %>% 
  select(subject_id) %>% 
  distinct(),
  by=c("person_id"="subject_id"))

# history of outcome event -----
history.outcomes<-working.outcomes %>%  
  inner_join(working.Pop %>% 
               select(person_id,cohort_start_date) %>% 
               rename("subject_id"="person_id") %>% 
               rename("Pop_cohort_start_date"="cohort_start_date")) %>% 
  filter(cohort_start_date< (Pop_cohort_start_date-years(1))) 
working.Pop<-working.Pop %>% 
  left_join(history.outcomes  %>% 
  select(subject_id) %>% 
  distinct() %>% 
  mutate(history_outcome=1),
  by=c("person_id"="subject_id"))
working.Pop<-working.Pop %>% 
  mutate(history_outcome=ifelse(is.na(history_outcome),0,1))

# first event after index date up to  -----
# where e==1 or e==2
# where years.of.interest is "all", we want all four years of potential follow
# where it is a specific year, just that year of follow up
# see end.date defined above
if(working.exposure.pop.def==1 | working.exposure.pop.def==2 ){
f_u.outcome<-working.outcomes %>%  
  inner_join(working.Pop %>% 
               select(person_id,cohort_start_date) %>% 
               rename("subject_id"="person_id") %>% 
               rename("Pop_cohort_start_date"="cohort_start_date"))  %>% 
  filter(cohort_start_date> Pop_cohort_start_date) %>% 
  filter(cohort_start_date<= end.date)}

# where e==3
# just up to 28 days 
if(working.exposure.pop.def==3){
f_u.outcome<-working.outcomes %>%  
  inner_join(working.Pop %>% 
               select(person_id,cohort_start_date) %>% 
               rename("subject_id"="person_id") %>% 
               rename("Pop_cohort_start_date"="cohort_start_date"))  %>% 
  filter(cohort_start_date> Pop_cohort_start_date) %>% 
  filter(cohort_start_date<= (Pop_cohort_start_date+days(28)))
}


if(nrow(f_u.outcome)>=5){ # 5 or more cases
f_u.outcome<-f_u.outcome %>% 
  group_by(subject_id) %>%
  arrange(cohort_start_date) %>% 
  mutate(seq=1:length(subject_id)) %>% 
  filter(seq==1) %>% 
  select(subject_id,cohort_start_date)  %>% 
  rename("f_u.outcome_date"="cohort_start_date") %>% 
  mutate(f_u.outcome=1)
working.Pop<-working.Pop %>% 
  left_join(f_u.outcome,
  by=c("person_id"="subject_id"))
working.Pop<-working.Pop %>% 
  mutate(f_u.outcome=ifelse(is.na(f_u.outcome),0,1))


# TAR -----
# censor at first of outcome, end of observation period, one year

# if event, date of event
# if no event,  censor at tar.end.date or end of observation period, whichever comes first
if(working.exposure.pop.def==1 | working.exposure.pop.def==2 ){
working.Pop<-working.Pop %>%
  mutate(f_u.outcome_date=if_else(f_u.outcome==1,
                                  f_u.outcome_date, 
                               if_else(observation_period_end_date < end.date,
                                       observation_period_end_date, 
                                       end.date )))
working.Pop<-working.Pop %>% 
  mutate(f_u.outcome.days=as.numeric(difftime(f_u.outcome_date,
                                              working.start.date, 
                                                   units="days")))
}
if(working.exposure.pop.def==3){
working.Pop<-working.Pop %>%
  mutate(f_u.outcome_date=if_else(f_u.outcome==1,
                                  f_u.outcome_date, 
                               if_else(observation_period_end_date < (cohort_start_date+days(28)),
                                       observation_period_end_date, 
                                       (cohort_start_date+days(28)) )))
working.Pop<-working.Pop %>% 
  mutate(f_u.outcome.days=as.numeric(difftime(f_u.outcome_date,
                                              cohort_start_date, 
                                                   units="days")))
}

# characteristics ----
working.Pop.w.outcome<-working.Pop %>% filter(f_u.outcome==1) 

# add age and gender -----
working.Pop.w.outcome$age<- NA
if(sum(is.na(working.Pop.w.outcome$day_of_birth))==0 & sum(is.na(working.Pop.w.outcome$month_of_birth))==0){
 # if we have day and month 
working.Pop.w.outcome<-working.Pop.w.outcome %>%
  mutate(age=floor(as.numeric((ymd(cohort_start_date)-
                    ymd(paste(year_of_birth,
                                month_of_birth,
                                day_of_birth, sep="-"))))/365.25))
} else { 
working.Pop.w.outcome<-working.Pop.w.outcome %>% 
  mutate(age= year(cohort_start_date)-year_of_birth)
}

working.Pop.w.outcome<-working.Pop.w.outcome %>% 
  mutate(age_gr=ifelse(age<20,  "<20",
                ifelse(age>=20 &  age<=44,  "20-44",
                ifelse(age>=45 & age<=54,  "45-54",
                ifelse(age>=55 & age<=64,  "55-64",
                ifelse(age>=65 & age<=74, "65-74", 
                ifelse(age>=75 & age<=84, "75-84",      
                ifelse(age>=85, ">=85",
                       NA)))))))) %>% 
  mutate(age_gr= factor(age_gr, 
                   levels = c("<20", "20-44","45-54", "55-64",
                              "65-74", "75-84",">=85")))
# wider age groups
working.Pop.w.outcome<-working.Pop.w.outcome %>% 
  mutate(age_gr2=ifelse(age<=44,  "<=44",
                 ifelse(age>=45 & age<=64,  "45-64",    
                 ifelse(age>=55, ">=65",
                       NA)))) %>% 
  mutate(age_gr2= factor(age_gr2, 
                   levels = c("<=44", "45-64",">=65")))

# another alternative set of age groups
working.Pop.w.outcome<-working.Pop.w.outcome %>% 
  mutate(age_gr3=ifelse(age<20,  "<20",
                ifelse(age>=20 &  age<=29,  "20-29",
                ifelse(age>=30 &  age<=39,  "30-39",
                ifelse(age>=40 &  age<=49,  "40-49",
                ifelse(age>=50 &  age<=59,  "50-59",
                ifelse(age>=60 & age<=69,  "60-69",
                ifelse(age>=70 & age<=79,  "70-79",      
                ifelse(age>=80, ">=80",
                       NA))))))))) %>% 
  mutate(age_gr3= factor(age_gr3, 
                   levels = c("<20", "20-29","30-39","40-49", "50-59",
                              "60-69", "70-79",">=80")))

# add prior observation time -----
working.Pop.w.outcome<-working.Pop.w.outcome %>%  
  mutate(prior_obs_days=as.numeric(difftime(f_u.outcome_date,
                                          observation_period_start_date,
                                        units="days"))) %>% 
  mutate(prior_obs_years=prior_obs_days/365.25)


# condition history -----
print(paste0("-- Getting codes for conditions"))
conn<-connect(connectionDetails)
# table with all the concept ids of interest (code and descendants)
insertTable(connection=conn,
            tableName=paste0(results_database_schema, ".",cohortTableProfiles),
            data=data.frame(condition_id=integer(), concept_id =integer()),
            createTable = TRUE,
            progressBar=FALSE)
for(n in 1:length(cond.codes)){ # add codes for each condition
working.code<-cond.codes[n]
working.name<-cond.names[n]
sql<-paste0("INSERT INTO ", results_database_schema, ".",cohortTableProfiles, " (condition_id, concept_id) SELECT DISTINCT ", n ,", descendant_concept_id FROM ", cdm_database_schema, ".concept_ancestor WHERE ancestor_concept_id IN (",working.code, ");")
suppressMessages(executeSql(conn, sql, progressBar = FALSE))
}

#link to table
cohortTableProfiles_db<-tbl(db, sql(paste0("SELECT * FROM ",
                       results_database_schema,
                       ".", cohortTableProfiles)))
print(paste0("-- Getting  conditions"))

cond.persons <- outcome_db %>% 
  filter(cohort_definition_id==working.outcome) %>% 
  select(subject_id, cohort_start_date) %>% 
  distinct() %>% 
  left_join(condition_era_db,
            by=c("subject_id"="person_id")
            ) %>% 
  rename("person_id"="subject_id")  %>%
  select(person_id, condition_concept_id, condition_era_start_date) %>% 
  inner_join(cohortTableProfiles_db ,
             by=c("condition_concept_id"="concept_id")) %>% 
  inner_join(cohortTableExposures_db %>% 
               select(subject_id, cohort_start_date) %>% 
               rename("person_id"="subject_id"),
             by = "person_id") %>% 
  filter(condition_era_start_date < cohort_start_date) %>% 
  select(person_id, condition_id) %>% 
  distinct() %>% 
  collect() 


for(n in 1:length(cond.codes)){# extract condition info and add to working.Pop.w.outcome
working.code<-cond.codes[n]
working.name<-cond.names[n]
working.persons <-   cond.persons %>% 
  filter(condition_id==n) %>% 
  select(person_id) %>% 
  mutate(working.cond=1)

if(nrow(working.persons)>0){
working.Pop.w.outcome<-working.Pop.w.outcome %>%
  left_join(working.persons,
             by = "person_id") %>% 
  rename(!!working.name:="working.cond")
} else {
 working.Pop.w.outcome$working.cond<-0
 working.Pop.w.outcome<-working.Pop.w.outcome %>% 
  rename(!!working.name:="working.cond")
}

}
disconnect(conn)


#to zero if absent
working.Pop.w.outcome<-working.Pop.w.outcome %>%
  mutate(across(all_of(cond.names), ~ replace_na(.x, 0)))

# medication history
print(paste0("-- Getting codes for medications"))
conn<-connect(connectionDetails)
# table with all the concept ids of interest (code and descendants)
insertTable(connection=conn,
            tableName=paste0(results_database_schema, ".",cohortTableProfiles),
            data=data.frame(drug_id=integer(), concept_id =integer()),
            createTable = TRUE,
            progressBar=FALSE)
for(n in 1:length(drug.codes)){ # add codes for each condition
working.code<-drug.codes[n]
working.name<-drug.names[n]
sql<-paste0("INSERT INTO ", results_database_schema, ".",cohortTableProfiles, " (drug_id, concept_id) SELECT DISTINCT ", n ,", descendant_concept_id FROM ", cdm_database_schema, ".concept_ancestor WHERE ancestor_concept_id IN (",working.code, ");")
suppressMessages(executeSql(conn, sql, progressBar = FALSE))
}


print(paste0("-- Getting  medications"))
cohortTableProfiles_db<-tbl(db, sql(paste0("SELECT * FROM ",
                       results_database_schema,
                       ".", cohortTableProfiles)))

med.persons <- outcome_db %>% 
  filter(cohort_definition_id==working.outcome) %>% 
  select(subject_id, cohort_start_date) %>% 
  distinct() %>% 
  left_join(drug_era_db,
            by=c("subject_id"="person_id")
            ) %>% 
  rename("person_id"="subject_id") %>%
        filter(drug_era_start_date>as.Date("2015-01-01")) %>%  
  filter(drug_era_start_date < cohort_start_date) %>% 
  inner_join(cohortTableProfiles_db ,
             by=c("drug_concept_id"="concept_id")) %>% 
  select(person_id,drug_id, drug_era_start_date, drug_era_end_date, cohort_start_date) %>% 
  distinct() %>% 
  collect() %>%
  filter(drug_era_start_date<=(cohort_start_date-days(4))
          & drug_era_start_date>=(cohort_start_date-days(183)) |
         drug_era_end_date<=(cohort_start_date-days(4))
                 & drug_era_end_date>=(cohort_start_date-days(183))) %>% 
  select(person_id, drug_id)%>% 
  distinct()

for(n in 1:length(drug.codes)){# extract condition info and add to Pop
working.code<-drug.codes[n]
working.name<-drug.names[n]
working.persons <-med.persons %>% 
  filter(drug_id==n) %>% 
  select(person_id) %>% 
  mutate(working.drug=1)

if(nrow(working.persons)>0){
working.Pop.w.outcome<-working.Pop.w.outcome %>%
  left_join(working.persons,
             by = "person_id") %>% 
  rename(!!working.name:="working.drug")
} else {
 working.Pop.w.outcome$working.drug<-0
 working.Pop.w.outcome<-working.Pop.w.outcome %>% 
  rename(!!working.name:="working.drug")
}

}
disconnect(conn)

#to zero if absent
working.Pop.w.outcome<-working.Pop.w.outcome %>%
  mutate(across(all_of(drug.names), ~ replace_na(.x, 0)))


# summarise
summary.characteristics<-bind_rows(
data.frame(Overall=t(working.Pop.w.outcome %>% 
  mutate(index_year=year(f_u.outcome_date)) %>% 
  summarise(n=nice.num.count(length(person_id)),
    age=paste0(nice.num.count(median(age)),
                     " [",
                     nice.num.count(quantile(age,probs=0.25)), 
                     " to ",
                     nice.num.count(quantile(age,probs=0.75)), 
                     "]" ),
    sex.male=paste0(nice.num.count(sum(gender=="Male")),
                    " (",
                    nice.num((sum(gender=="Male")/length(person_id))*100),
    "%)"), 
  prior_obs_years=paste0(nice.num(median(prior_obs_years)),
                     " [",
                     nice.num(quantile(prior_obs_years,probs=0.25)), 
                     " to ",
                     nice.num(quantile(prior_obs_years,probs=0.75)), 
                     "]" ),
  index.year_2017= paste0(nice.num.count(sum(index_year==2017)), 
                          " (", nice.num((sum(index_year==2017)/length(person_id))*100), "%)"),
  index.year_2018= paste0(nice.num.count(sum(index_year==2018)), 
                          " (", nice.num((sum(index_year==2018)/length(person_id))*100), "%)"),
  index.year_2019= paste0(nice.num.count(sum(index_year==2019)), 
                          " (", nice.num((sum(index_year==2019)/length(person_id))*100), "%)"))
  
)),
# and all the conds and medications
data.frame(Overall=t(working.Pop.w.outcome %>% 
  summarise_at(.vars = all_of(c(cond.names, drug.names)), 
               .funs = function(x, tot){
  paste0(nice.num.count(sum(x)), " (", nice.num((sum(x)/tot)*100), "%)")
} , tot=nrow(working.Pop.w.outcome))) ))
summary.characteristics$Overall<-as.character(summary.characteristics$Overall)

rownames(summary.characteristics)<-str_to_sentence(rownames(summary.characteristics))
rownames(summary.characteristics)<-str_replace(rownames(summary.characteristics),
                                      "Sex.male", "Sex: Male")
rownames(summary.characteristics)<-str_replace(rownames(summary.characteristics),
                                      "Prior_obs_years", "Years of prior observation time")
rownames(summary.characteristics)<-str_replace(rownames(summary.characteristics),
                                      "Index.year_", "Index year: ")
rownames(summary.characteristics)<-str_replace_all(rownames(summary.characteristics) , "_", " ")

#obscure any counts less than 5
summary.characteristics$Overall<-
  ifelse(str_sub(summary.characteristics$Overall, 1, 2) %in%  c("1 ","2 ", "3 ","4 "),
              "<5",summary.characteristics$Overall)

summary.characteristics$var<-row.names(summary.characteristics)
row.names(summary.characteristics)<-1:nrow(summary.characteristics)


Pop.summary.characteristics<-left_join(Pop.summary.characteristics,
  summary.characteristics %>% 
    rename(!!working.outcome.name:="Overall"),
  by="var")



Patient.characteristcis.for.plotting[[paste0(working.outcome.name,";",years.of.interest[i],";",o,";",pop.type,";","age_gr")]]<- working.Pop.w.outcome %>% 
  group_by(age_gr) %>% 
  tally() %>% 
  mutate(group=working.outcome.name) %>% 
  mutate(type="age_gr") %>% 
  mutate(study.year=years.of.interest[i]) %>% 
  mutate(prior.obs.required=ifelse(o==1, "No", "Yes")) %>% 
  mutate(pop.type=pop.type)

Patient.characteristcis.for.plotting[[paste0(working.outcome.name,";",years.of.interest[i],";",o,";",pop.type,";","age_gr_gender")]]<- working.Pop.w.outcome %>% 
  group_by(age_gr,gender) %>% 
  tally() %>% 
  mutate(group=working.outcome.name) %>% 
  mutate(type="age_gr_gender") %>% 
  mutate(study.year=years.of.interest[i]) %>% 
  mutate(prior.obs.required=ifelse(o==1, "No", "Yes")) %>% 
  mutate(pop.type=pop.type)

Patient.characteristcis.for.plotting[[paste0(working.outcome.name,";",years.of.interest[i],";",o,";",pop.type,";","age_gr2")]]<- working.Pop.w.outcome %>% 
  group_by(age_gr2) %>% 
  tally() %>% 
  mutate(group=working.outcome.name) %>% 
  mutate(type="age_gr2") %>% 
  mutate(study.year=years.of.interest[i]) %>% 
  mutate(prior.obs.required=ifelse(o==1, "No", "Yes")) %>% 
  mutate(pop.type=pop.type)

Patient.characteristcis.for.plotting[[paste0(working.outcome.name,";",years.of.interest[i],";",o,";",pop.type,";","age_gr2_gender")]]<- working.Pop.w.outcome %>% 
  group_by(age_gr2,gender) %>% 
  tally() %>% 
  mutate(group=working.outcome.name) %>% 
  mutate(type="age_gr2_gender") %>% 
  mutate(study.year=years.of.interest[i]) %>% 
  mutate(prior.obs.required=ifelse(o==1, "No", "Yes")) %>% 
  mutate(pop.type=pop.type)

Patient.characteristcis.for.plotting[[paste0(working.outcome.name,";",years.of.interest[i],";",o,";",pop.type,";","age_gr3")]]<- working.Pop.w.outcome %>% 
  group_by(age_gr3) %>% 
  tally() %>% 
  mutate(group=working.outcome.name) %>% 
  mutate(type="age_gr3") %>% 
  mutate(study.year=years.of.interest[i]) %>% 
  mutate(prior.obs.required=ifelse(o==1, "No", "Yes")) %>% 
  mutate(pop.type=pop.type)

Patient.characteristcis.for.plotting[[paste0(working.outcome.name,";",years.of.interest[i],";",o,";",pop.type,";","age_gr3_gender")]]<- working.Pop.w.outcome %>% 
  group_by(age_gr3,gender) %>% 
  tally() %>% 
  mutate(group=working.outcome.name) %>% 
  mutate(type="age_gr3_gender") %>% 
  mutate(study.year=years.of.interest[i]) %>% 
  mutate(prior.obs.required=ifelse(o==1, "No", "Yes")) %>% 
  mutate(pop.type=pop.type)

Patient.characteristcis.for.plotting[[paste0(working.outcome.name,";",years.of.interest[i],";",o,";",pop.type,";","gender")]]<- working.Pop.w.outcome %>% 
  group_by(gender) %>% 
  tally() %>% 
  mutate(group=working.outcome.name) %>% 
  mutate(type="gender") %>% 
  mutate(study.year=years.of.interest[i]) %>% 
  mutate(prior.obs.required=ifelse(o==1, "No", "Yes")) %>% 
  mutate(pop.type=pop.type)


# IRs  ------
# overall
IR.summary[[paste0(working.outcome.name,";",years.of.interest[i],";",o,";",pop.type,";","overall")]]<-working.Pop %>% 
  summarise(n=length(person_id),
            days=sum(f_u.outcome.days),
            years=(days/365.25),
            events=sum(f_u.outcome)) %>% 
  mutate(ir_100000=(events/years)*100000) %>% 
  mutate(strata="overall") %>% 
  mutate(outcome=working.outcome) %>% 
  mutate(outcome.name=working.outcome.name) %>% 
  mutate(study.year=years.of.interest[i]) %>% 
  mutate(prior.obs.required=ifelse(o==1, "No", "Yes")) %>% 
  mutate(pop.type=pop.type)

# by gender
IR.summary[[paste0(working.outcome.name,";",years.of.interest[i],";",o,";",pop.type,";","gender")]]<-working.Pop %>%  
  group_by(gender) %>% 
  summarise(n=length(person_id),
            days=sum(f_u.outcome.days),
            years=(days/365.25),
            events=sum(f_u.outcome)) %>% 
  mutate(ir_100000=(events/years)*100000) %>% 
  mutate(strata="gender") %>% 
  mutate(outcome=working.outcome) %>% 
  mutate(outcome.name=working.outcome.name) %>% 
  mutate(study.year="all") %>% 
  mutate(prior.obs.required=ifelse(o==1, "No", "Yes")) %>% 
  mutate(pop.type=pop.type)   
   
# by age and gender
IR.summary[[paste0(working.outcome.name,";",years.of.interest[i],";",o,";",pop.type,";","age_gr_gender")]]<-working.Pop %>%  
  group_by(age_gr, gender) %>% 
  summarise(n=length(person_id),
            days=sum(f_u.outcome.days),
            years=(days/365.25),
            events=sum(f_u.outcome)) %>% 
  mutate(ir_100000=(events/years)*100000) %>% 
  mutate(strata="age_gr_gender") %>% 
  mutate(outcome=working.outcome) %>% 
  mutate(outcome.name=working.outcome.name) %>% 
  mutate(study.year="all") %>% 
  mutate(prior.obs.required=ifelse(o==1, "No", "Yes")) %>% 
  mutate(pop.type=pop.type)

# by age (fewer groups) and gender
IR.summary[[paste0(working.outcome.name,";",years.of.interest[i],";",o,";",pop.type,";","age_gr2_gender")]]<-working.Pop %>%  
  group_by(age_gr2, gender) %>% 
  summarise(n=length(person_id),
            days=sum(f_u.outcome.days),
            years=(days/365.25),
            events=sum(f_u.outcome)) %>% 
  mutate(ir_100000=(events/years)*100000) %>% 
  mutate(strata="age_gr2_gender") %>% 
  mutate(outcome=working.outcome) %>% 
  mutate(outcome.name=working.outcome.name) %>% 
  mutate(study.year="all") %>% 
  mutate(prior.obs.required=ifelse(o==1, "No", "Yes")) %>% 
  mutate(pop.type=pop.type)

# by age (thrid definition) and gender
IR.summary[[paste0(working.outcome.name,";",years.of.interest[i],";",o,";",pop.type,";","age_gr3_gender")]]<-working.Pop %>%  
  group_by(age_gr3, gender) %>% 
  summarise(n=length(person_id),
            days=sum(f_u.outcome.days),
            years=(days/365.25),
            events=sum(f_u.outcome)) %>% 
  mutate(ir_100000=(events/years)*100000) %>% 
  mutate(strata="age_gr3_gender") %>% 
  mutate(outcome=working.outcome) %>% 
  mutate(outcome.name=working.outcome.name) %>% 
  mutate(study.year="all") %>% 
  mutate(prior.obs.required=ifelse(o==1, "No", "Yes")) %>% 
  mutate(pop.type=pop.type)

# by age 
IR.summary[[paste0(working.outcome.name,";",years.of.interest[i],";",o,";",pop.type,";","age_gr")]]<-working.Pop %>%  
  group_by(age_gr) %>% 
  summarise(n=length(person_id),
            days=sum(f_u.outcome.days),
            years=(days/365.25),
            events=sum(f_u.outcome)) %>% 
  mutate(ir_100000=(events/years)*100000) %>% 
  mutate(strata="age_gr") %>% 
  mutate(outcome=working.outcome) %>% 
  mutate(outcome.name=working.outcome.name) %>% 
  mutate(study.year="all") %>% 
  mutate(prior.obs.required=ifelse(o==1, "No", "Yes")) %>% 
  mutate(pop.type=pop.type)

# by age (fewer groups) and gender
IR.summary[[paste0(working.outcome.name,";",years.of.interest[i],";",o,";",pop.type,";","age_gr2")]]<-working.Pop %>%  
  group_by(age_gr2) %>% 
  summarise(n=length(person_id),
            days=sum(f_u.outcome.days),
            years=(days/365.25),
            events=sum(f_u.outcome)) %>% 
  mutate(ir_100000=(events/years)*100000) %>% 
  mutate(strata="age_gr2") %>% 
  mutate(outcome=working.outcome) %>% 
  mutate(outcome.name=working.outcome.name) %>% 
  mutate(study.year="all") %>% 
  mutate(prior.obs.required=ifelse(o==1, "No", "Yes")) %>% 
  mutate(pop.type=pop.type)

# by age (thrid definition) and gender
IR.summary[[paste0(working.outcome.name,";",years.of.interest[i],";",o,";",pop.type,";","age_gr3")]]<-working.Pop %>%  
  group_by(age_gr3) %>% 
  summarise(n=length(person_id),
            days=sum(f_u.outcome.days),
            years=(days/365.25),
            events=sum(f_u.outcome)) %>% 
  mutate(ir_100000=(events/years)*100000) %>% 
  mutate(strata="age_gr3") %>% 
  mutate(outcome=working.outcome) %>% 
  mutate(outcome.name=working.outcome.name) %>% 
  mutate(study.year="all") %>% 
  mutate(prior.obs.required=ifelse(o==1, "No", "Yes")) %>% 
  mutate(pop.type=pop.type)



# by age, gender, and history of event
IR.summary[[paste0(working.outcome.name,";",years.of.interest[i],";",o,";",pop.type,";","age_gr_gender_history")]]<-working.Pop %>%  
  group_by(age_gr, gender,history_outcome) %>% 
  summarise(n=length(person_id),
            days=sum(f_u.outcome.days),
            years=(days/365.25),
            events=sum(f_u.outcome)) %>% 
  mutate(ir_100000=(events/years)*100000) %>% 
  mutate(strata="age_gr_gender_history") %>% 
  mutate(outcome.name=working.outcome.name) %>% 
  mutate(outcome=working.outcome) %>% 
  mutate(study.year="all")  %>% 
  mutate(prior.obs.required=ifelse(o==1, "No", "Yes")) %>% 
  mutate(pop.type=pop.type)
}
}


Patient.characteristcis[[paste0("pop.",pop.type,"_", "years.", years.of.interest[i], 
                                "_", "hist.",prior.hist.req[o])]]<-Pop.summary.characteristics %>% 
mutate(study.year=as.character(working.year))  %>% 
  mutate(end.year=as.character(year(end.date))) %>% 
  mutate(prior.obs.required=ifelse(o==1, "No", "Yes")) %>% 
  mutate(pop.type=pop.type)

}
}
}

IR.summary<-bind_rows(IR.summary, .id = NULL)
IR.summary$db<-db.name

# save ----
save(IR.summary, file = paste0(output.folder, "/IR.summary_", db.name, ".RData"))
save(Patient.characteristcis, file = paste0(output.folder, "/Patient.characteristcis_", db.name, ".RData"))
save(Patient.characteristcis.for.plotting, file = paste0(output.folder, "/Patient.characteristcis.for.plotting_", db.name, ".RData"))

# # zip results
print("Zipping results to output folder")
unlink(paste0(output.folder, "/OutputToShare_", db.name, ".zip"))
zipName <- paste0(output.folder, "/OutputToShare_", db.name, ".zip")

files<-c(paste0(output.folder, "/IR.summary_", db.name, ".RData"),
         paste0(output.folder, "/Patient.characteristcis_", db.name, ".RData"),
         paste0(output.folder, "/Patient.characteristcis.for.plotting_", db.name, ".RData"))
files <- files[file.exists(files)==TRUE]

createZipFile(zipFile = zipName,
              rootFolder=output.folder,
              files = files)

print("Done!")
print("-- If all has worked, there should now be a zip folder with your results in the output folder to share")
print("-- Thank you for running the study!")


