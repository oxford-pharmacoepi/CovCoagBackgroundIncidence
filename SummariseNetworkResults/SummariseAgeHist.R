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
 Network.patient.characteristcis.for.plotting[[db.names[i]]]<-bind_rows(Patient.characteristcis.for.plotting) %>% 
   mutate(db=db.names[i])
 rm(Patient.characteristcis.for.plotting)
 }
}
Network.patient.characteristcis.for.plotting<-bind_rows(Network.patient.characteristcis.for.plotting)


plot.data<-Network.patient.characteristcis.for.plotting %>%
                          filter(group %in% 
           c("CVST",
               "CVST (with thrombocytopenia 10 days pre to 10 days post)",
               "DIC",
               "DIC (with thrombocytopenia 10 days pre to 10 days post)",
                   "PE (with thrombocytopenia 10 days pre to 10 days post)", 
                   "DVT broad (with thrombocytopenia 10 days pre to 10 days post)", 
                 "imm throm",
                "all stroke (with thrombocytopenia 10 days pre to 10 days post)",
               "SVT",
                "SVT (with thrombocytopenia 10 days pre to 10 days post)")) %>% 
  mutate(group=ifelse(group=="PE (with thrombocytopenia 10 days pre to 10 days post)", 
                      "Pulmonary\nembolism -\nthrombocytopenia",
                      group)) %>%
    mutate(group=ifelse(group=="CVST (with thrombocytopenia 10 days pre to 10 days post)",
                               "Cerebral venous\nsinus thrombosis -\nthrombocytopenia",
                      group)) %>%
    mutate(group=ifelse(group=="CVST",
                               "Cerebral venous\nsinus thrombosis",
                      group)) %>%
    mutate(group=ifelse(group=="DIC",
                               "Disseminated\nintravascular\ncoagulation",
                      group)) %>%
    mutate(group=ifelse(group=="DIC (with thrombocytopenia 10 days pre to 10 days post)",
                               "Disseminated\nintravascular\ncoagulation -\nthrombocytopenia",
                      group)) %>%
  mutate(group=ifelse(group=="DVT broad (with thrombocytopenia 10 days pre to 10 days post)", 
                      "Deep vein\nthrombosis -\nthrombocytopenia",
                      group)) %>%
    mutate(group=ifelse(group=="all stroke (with thrombocytopenia 10 days pre to 10 days post)", "Stroke -\nthrombocytopenia",
                      group)) %>%
  mutate(group=ifelse(group=="imm throm", "Immune\nthrombocytopenia",
                      group))  %>%
  mutate(group=ifelse(group=="SVT", 
                             "Splanchnic vein\nthrombosis",
                      group))%>%
  mutate(group=ifelse(group=="SVT (with thrombocytopenia 10 days pre to 10 days post)", 
                             "Splanchnic vein\nthrombosis -\nthrombocytopenia",
                      group)) %>%
  mutate(group=factor(group, 
                levels=c("Cerebral venous\nsinus thrombosis",
                         "Cerebral venous\nsinus thrombosis -\nthrombocytopenia",
                         "Deep vein\nthrombosis -\nthrombocytopenia",
                         "Disseminated\nintravascular\ncoagulation",
                         "Disseminated\nintravascular\ncoagulation -\nthrombocytopenia",
                         "Immune\nthrombocytopenia",
                         "Pulmonary\nembolism -\nthrombocytopenia",
                         "Splanchnic vein\nthrombosis",
                         "Splanchnic vein\nthrombosis -\nthrombocytopenia",
                         "Stroke -\nthrombocytopenia"
                         )
                )) %>%
                          filter(type=="age_gr2_gender") %>% 
                          # mutate(db="sidiap_h") %>%
  filter(prior.obs.required=="No") %>% 
  filter(pop.type=="general.pop.all") 
                           
  
plot.data<-plot.data %>% 
  group_by(db,group ) %>% 
  mutate(perc = n/ sum(n)) 
#check
plot.data%>% 
  summarise(sum(perc))
plot.data%>% 
  summarise(sum(n))

p<-gg.general.format.facet.perc(
  plot.data%>% 
  ggplot()+
  geom_col(aes(age_gr2,perc, fill=gender),width=1,
           colour="black")+
  facet_grid(group~ db, switch="y")+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold")) +
  theme_bw()+
  ylab("Proportion of cohort\n")+
  xlab("Age")+
  scale_fill_manual(values=c("#F21A00", "#3B9AB2"))) +
     theme(axis.text.x = element_text(angle = 90, vjust=0.5, hjust=1))

# colours by type
p
g <- ggplot_gtable(ggplot_build(p))
strip_both <- which(grepl('strip-l', g$layout$name))
fills <- c("#f7f7f7",
           rep("#d9d9d9",2),
           rep("#bdbdbd",1),
           rep("#969696",3))
k <- 1
for (i in strip_both) {
j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
k <- k+1
}
grid::grid.draw(g)

