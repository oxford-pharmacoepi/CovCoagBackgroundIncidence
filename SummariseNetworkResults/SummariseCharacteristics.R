# summary of patient characteristics -----
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



table.data<-Network.patient.characteristcis %>% 
  select("var", 
         "Overall", 
         "imm throm", 
         "CVST",
"SVT" ,"DVT broad (with thrombocytopenia 10 days pre to 10 days post)"    ,    
"PE (with thrombocytopenia 10 days pre to 10 days post)",
"isc stroke (with thrombocytopenia 10 days pre to 10 days post)" ) %>% 
  rename("Immune thrombocytopenia"="imm throm")%>% 
  rename("Cerebral venous sinus thrombosis"="CVST")%>% 
  rename("Splanchnic vein thrombosis"="SVT")%>% 
  rename("Ischemic stroke (with thrombocytopenia 10 days pre to 10 days post)"="isc stroke (with thrombocytopenia 10 days pre to 10 days post)")

write.csv(table.data %>% 
            select(var, Overall) %>% 
            rename("SIDIAP_H"="Overall"),
          here("SummariseNetworkResults", "table1.csv"))


