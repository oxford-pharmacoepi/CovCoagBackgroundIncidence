source(here("Functions", "Functions.R"))

output.folder<-here::here("output")
output.files<-list.files(output.folder)

# load  -----
# dbs
db.names<-output.files[str_detect(output.files, "IR.summary")]
db.names<-str_replace(db.names, "IR.summary_","")
db.names<-str_replace(db.names, ".RData","")


# plot patient characteristics -----
Network.patient.characteristcis.for.plotting<-list()
for(i in 1:length(db.names)){
  if(file.exists(paste0(output.folder,"/Patient.characteristcis.for.plotting_", db.names[i], ".RData"))){
 load(paste0(output.folder,"/Patient.characteristcis.for.plotting_", db.names[i], ".RData"))
 Network.patient.characteristcis.for.plotting[[db.names[i]]]<-bind_rows(Patient.characteristcis.for.plotting)
 rm(Patient.characteristcis.for.plotting)
 }
}
Network.patient.characteristcis.for.plotting<-bind_rows(Network.patient.characteristcis.for.plotting)



gg.general.format.facet(Network.patient.characteristcis.for.plotting %>%
                          filter(group %in% c("exposure population","CVST")) %>% 
                          filter(type=="age_gr_gender") %>% 
                          mutate(db="sidiap_h") %>% 
  ggplot()+
  geom_col(aes(age_gr,n, fill=gender),width=1,
           colour="black")+
  facet_grid(group~ db, scales = "free_y", switch="y")+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold")) +
  theme_bw()+
  ylab("N")+
  xlab("Age")+
  scale_fill_manual(values=c("#F21A00", "#3B9AB2")))





