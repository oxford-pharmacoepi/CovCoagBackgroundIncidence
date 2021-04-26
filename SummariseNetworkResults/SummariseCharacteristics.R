# summary of patient characteristics -----
output.folder<-here::here("output")
output.files<-list.files(output.folder)

# dbs
db.names<-output.files[str_detect(output.files, "IR.summary")]
db.names<-str_replace(db.names, "IR.summary_","")
db.names<-str_replace(db.names, ".RData","")

Network.patient.characteristcis<-list()
for(i in 1:length(db.names)){
 load(paste0(here("output"),"/Patient.characteristcis_", db.names[i], ".RData"))

for(l in 1:length(Patient.characteristcis)){
    Patient.characteristcis[[l]]$db<- db.names[i]
  }
 Network.patient.characteristcis[[db.names[i]]]<-bind_rows(Patient.characteristcis, .id = "id") 
 rownames(Network.patient.characteristcis[[db.names[i]]])<-1:nrow(Network.patient.characteristcis[[db.names[i]]])
 rm(Patient.characteristcis)
}
Network.patient.characteristcis<-bind_rows(Network.patient.characteristcis)


# summarise overall study populations -----

table.data<-Network.patient.characteristcis %>%
  filter(prior.obs.required=="No") %>% 
  filter(pop.type=="general.pop.all") %>% 
  select("var", 
         "Overall",
         "db") 

table.data<-table.data %>% 
  pivot_wider(names_from = db, 
              values_from = Overall)

table.data<-bind_rows(
     table.data[c(1:7),],
    table.data[c(1),]  %>%
      mutate_at(vars(names(table.data)), ~ replace(., !is.na(.), NA)) %>%
      mutate(var="Comorbidities"),
     table.data[c(8:20),],
    table.data[c(1),]  %>%
      mutate_at(vars(names(table.data)), ~ replace(., !is.na(.), NA)) %>%
      mutate(var="Medication use (183 days prior to four days prior)"),
    table.data[c(21:29),]
    ) 

table.data<-table.data %>%
     mutate(var=ifelse(var=="Copd", "COPD", var)) %>%
     mutate(var=ifelse(var=="Antiinflamatory and antirheumatic", "Non-steroidal anti-inflammatory drugs ", var)) %>%
     mutate(var=ifelse(var=="Coxibs", "Cox2 inhibitors ", var)) %>%
     mutate(var=ifelse(var=="Corticosteroids", "Systemic corticosteroids ", var)) %>%
     mutate(var=ifelse(var=="Antithrombotic", "Antithrombotic and anticoagulant therapies", var)) %>%
     mutate(var=ifelse(var=="Lipid modifying", "Lipid modifying agents ", var)) %>%
     mutate(var=ifelse(var=="Antineoplastic immunomodulating", "antineoplastic and immunomodulating agents ", var)) %>%
     mutate(var=ifelse(var=="Hormonal contraceptives", "Hormonal contraceptives for systemic use ", var)) %>%
     mutate(var=ifelse(var=="Sex hormones modulators", "Sex hormones and modulators of the genital system", var))
 

write.csv(table.data,
          here("SummariseNetworkResults", "table1.csv"))


# ----


# table.data<-Network.patient.characteristcis %>% 
#   select("var", 
#          "Overall", 
#          "imm throm", 
#          "CVST",
# "SVT" ,"DVT broad (with thrombocytopenia 10 days pre to 10 days post)"    ,    
# "PE (with thrombocytopenia 10 days pre to 10 days post)",
# "isc stroke (with thrombocytopenia 10 days pre to 10 days post)" ) %>% 
#   rename("Immune thrombocytopenia"="imm throm")%>% 
#   rename("Cerebral venous sinus thrombosis"="CVST")%>% 
#   rename("Splanchnic vein thrombosis"="SVT")%>% 
#   rename("Ischemic stroke (with thrombocytopenia 10 days pre to 10 days post)"="isc stroke (with thrombocytopenia 10 days pre to 10 days post)")
# 
# write.csv(table.data %>% 
#             select(var, Overall) %>% 
#             rename("SIDIAP_H"="Overall"),
#           here("SummariseNetworkResults", "table1.csv"))


