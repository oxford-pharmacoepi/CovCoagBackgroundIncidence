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
                          filter(group %in% c("exposure population","CVST",
                                              "PE (with thrombocytopenia 10 days pre to 10 days post)", 
                                              "DVT broad (with thrombocytopenia 10 days pre to 10 days post)", 
                                              "imm throm",
                                              "isc stroke (with thrombocytopenia 10 days pre to 10 days post)",
                                              "SVT")) %>%
  mutate(group=ifelse(group=="exposure population", "Study\npopulation",
                      group)) %>%
  mutate(group=ifelse(group=="PE (with thrombocytopenia 10 days pre to 10 days post)", 
                      "Pulmonary\nembolism -\nthrombocytopenia",
                      group)) %>%
    mutate(group=ifelse(group=="CVST", "Cerebral venous\nsinus thrombosis",
                      group)) %>%
  mutate(group=ifelse(group=="DVT broad (with thrombocytopenia 10 days pre to 10 days post)", 
                      "Deep vein\nthrombosis -\nthrombocytopenia",
                      group)) %>%
    mutate(group=ifelse(group=="isc stroke (with thrombocytopenia 10 days pre to 10 days post)", "Ischemic stroke -\nthrombocytopenia",
                      group)) %>%
  mutate(group=ifelse(group=="imm throm", "Immune\nthrombocytopenia",
                      group)) %>%
  mutate(group=ifelse(group=="SVT", "Splanchnic vein\nthrombosis",
                      group)) %>%
                          filter(type=="age_gr_gender") %>% 
  mutate(group=factor(group, 
                levels=c("Study\npopulation",
                         "Deep vein\nthrombosis -\nthrombocytopenia","Pulmonary\nembolism -\nthrombocytopenia", 
                         "Ischemic stroke -\nthrombocytopenia",
                         "Cerebral venous\nsinus thrombosis","Immune\nthrombocytopenia",
                         "Splanchnic vein\nthrombosis"
                         )
                )) %>% 
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
  geom_col(aes(age_gr,perc, fill=gender),width=1,
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

