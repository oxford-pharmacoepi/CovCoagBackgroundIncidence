options(scipen=999) 
# network results in Output folder

# summary of patient characteristics -----
output.files<-list.files(output.folder)

# dbs
db.names<-output.files[str_detect(output.files, "IR.summary")]
db.names<-str_replace(db.names, "IR.summary_","")
db.names<-str_replace(db.names, ".RData","")

db.names<-db.names[db.names %in% c("TEST_SIDIAP", "SIDIAP", "SIDIAP_h")]# for now just sidiap

Network.patient.characteristcis<-list()
for(i in 1:length(db.names)){
 load(paste0(output.folder,"/Patient.characteristcis_", db.names[i], ".RData"))
 Network.patient.characteristcis[[db.names[i]]]<-Patient.characteristcis
 rm(Patient.characteristcis)
}
# nested list with characteristics from each db  
SummaryPatientCharacteristics<-list()
for(i in 1:length(db.names)){
table.data<-full_join(Network.patient.characteristcis[[db.names[i]]]$general.pop.all_Overall %>% 
              mutate(vars=row.names(Network.patient.characteristcis[[db.names[i]]]$general.pop.all_Overall)),
  Network.patient.characteristcis[[db.names[i]]]$`general.pop.all_Immune thrombocytopenia`%>% 
              mutate(vars=row.names(Network.patient.characteristcis[[db.names[i]]]$`general.pop.all_Immune thrombocytopenia`)))
  
table.data<-full_join( table.data,        
  Network.patient.characteristcis[[db.names[i]]]$`general.pop.all_Disseminated intravascular coagulation`%>% 
              mutate(vars=row.names(Network.patient.characteristcis[[db.names[i]]]$`general.pop.all_Disseminated intravascular coagulation`)))
  
table.data<-full_join( table.data,    
       Network.patient.characteristcis[[db.names[i]]]$`general.pop.all_Cerebral venous sinus thrombosis`%>% 
              mutate(vars=row.names(Network.patient.characteristcis[[db.names[i]]]$`general.pop.all_Cerebral venous sinus thrombosis`)))

table.data<-full_join( table.data,    
       Network.patient.characteristcis[[db.names[i]]]$`general.pop.all_Intracranial venous thrombosis`%>% 
              mutate(vars=row.names(Network.patient.characteristcis[[db.names[i]]]$`general.pop.all_Intracranial venous thrombosis`)))

table.data<-full_join( table.data,    
       Network.patient.characteristcis[[db.names[i]]]$`general.pop.all_Thrombotic thrombocytopenia purpura`%>% 
              mutate(vars=row.names(Network.patient.characteristcis[[db.names[i]]]$`general.pop.all_Thrombotic thrombocytopenia purpura`)))
                  
table.data<-table.data %>% 
  select("vars", "Overall", 
         "Immune thrombocytopenia", "Disseminated intravascular coagulation",
"Cerebral venous sinus thrombosis" ,"Intracranial venous thrombosis"    ,    
"Thrombotic thrombocytopenia purpura")


myHeader <- c(" " = 1, kw0 = 6)
names(myHeader) <- c(" ", db.names[i])
SummaryPatientCharacteristics[[db.names[i]]]<-kable(table.data) %>% 
 kable_styling(bootstrap_options = c("striped", "bordered"))%>%
 add_header_above(header = myHeader)

}


# plot patient characteristics -----
Network.patient.characteristcis.for.plotting<-list()
for(i in 1:length(db.names)){
  if(file.exists(paste0(output.folder,"/Patient.characteristcis.for.plotting_", db.names[i], ".RData"))){
 load(paste0(output.folder,"/Patient.characteristcis.for.plotting_", db.names[i], ".RData"))
 Network.patient.characteristcis.for.plotting[[db.names[i]]]<-Patient.characteristcis.for.plotting
 rm(Patient.characteristcis.for.plotting)
 }
}

# 
# plot.data<-bind_rows(Network.patient.characteristcis.for.plotting$SIDIAP_h$Overall_age_gr_gender %>% 
#                        mutate(group="General population"),
#        Network.patient.characteristcis.for.plotting$SIDIAP_h$`Immune thrombocytopenia_age_gr_gender`%>% 
#                        mutate(group="Immune thrombocytopenia"),
#        Network.patient.characteristcis.for.plotting$SIDIAP_h$`Disseminated intravascular coagulation_age_gr_gender`%>% 
#                        mutate(group="Disseminated intravascular coagulation"),
#        Network.patient.characteristcis.for.plotting$SIDIAP_h$`Cerebral venous sinus thrombosis_age_gr_gender`%>% 
#                        mutate(group="Cerebral venous sinus thrombosis"),
#        Network.patient.characteristcis.for.plotting$SIDIAP_h$`Intracranial venous thrombosis_age_gr_gender`%>% 
#                        mutate(group="Intracranial venous thrombosis"),
#        Network.patient.characteristcis.for.plotting$SIDIAP_h$`Thrombotic thrombocytopenia purpura_age_gr_gender`%>% 
#                        mutate(group="Thrombotic thrombocytopenia purpura")) %>% 
#   mutate(db="SIDIAP") %>% 
#   mutate(group=factor(group,
#                       levels=c("General population",
#                                "Cerebral venous sinus thrombosis",
#                                "Disseminated intravascular coagulation",
#                                "Immune thrombocytopenia",
#                                "Intracranial venous thrombosis",
#                                "Thrombotic thrombocytopenia purpura")))
# 
# gg.general.format.facet(plot.data %>% 
#   ggplot()+
#   geom_col(aes(age_gr,n, fill=gender),width=1,
#            colour="black")+
#   facet_grid(group~ db, scales = "free_y", switch="y")+
#   theme(axis.text=element_text(size=12),
#         axis.title=element_text(size=14,face="bold")) +
#   theme_bw()+
#   ylab("N")+
#   xlab("Age")+
#   scale_fill_manual(values=c("#F21A00", "#3B9AB2")))





# summarise IRs ----
Network.IR<-list()
for(i in 1:length(db.names)){
  load(paste0(output.folder,"/IR.summary_", db.names[i], ".RData"))
 Network.IR[[db.names[i]]]<-IR.summary
 rm(IR.summary)
}
Network.IR<-bind_rows(Network.IR)


 # add CIs
IR.conf<-epi.conf(as.matrix(cbind(Network.IR$events, Network.IR$years)),
         ctype = "inc.rate", method = "exact", conf.level = 0.95)
Network.IR$ir_100000_lower<-IR.conf$lower* 100000
Network.IR$ir_100000_upper<-IR.conf$upper* 100000
rm(IR.conf) 

outcome.order<-Network.IR %>% 
  filter(strata=="overall") %>% 
  filter(study.year=="all") %>% 
  group_by(outcome.name) %>% 
  arrange(ir_100000) %>% 
  select(outcome.name) %>% 
  distinct() %>% 
  pull()

plot.data<-Network.IR %>% 
  mutate(outcome.name=factor(outcome.name, 
                             levels=outcome.order))

plot.data %>% 
  filter(strata=="overall") %>% 
  filter(study.year=="all") %>% 
  ggplot(aes(outcome.name,ir_100000, colour=db, shape=pop.type)) +
  geom_point()+ 
  geom_errorbar(aes(ymin=ir_100000_lower,ymax=ir_100000_upper), width=.1)+
  coord_flip()+
  theme_bw()
  
  

# focus on 
# Cerebral venous sinus thrombosis
# Thrombotic thrombocytopenia purpura
# Disseminated intravascular coagulation 
# Immune thrombocytopenia
# Intracranial venous thrombosis

# overall, and by year
plot.data<-Network.IR %>% 
  filter(outcome.name %in% 
           c("Cerebral venous sinus thrombosis",
           "Thrombotic thrombocytopenia purpura",
           "Disseminated intravascular coagulation",
           "Immune thrombocytopenia",
           "Intracranial venous thrombosis") )%>% 
  filter(strata=="overall") %>% 
  mutate(study.year=ifelse(study.year=="all","Overall", study.year))


gg.general.format.facet(
  plot.data %>% 
  ggplot(aes(db,ir_100000, shape=pop.type)) +
  facet_grid(outcome.name~ study.year, scales = "free_y", switch="y")+
  geom_point(position=position_dodge(width=0.5) )+ 
  geom_errorbar(aes(ymin=ir_100000_lower,ymax=ir_100000_upper), width=0,
                position=position_dodge(width=0.5) )+
  theme_bw()+
  ylab("Incidence rate\nper 100,000 person-years\n")+
  xlab("Database")
  )

# overall
plot.data<-Network.IR %>% 
  filter(outcome.name %in% 
           c("Cerebral venous sinus thrombosis",
           "Thrombotic thrombocytopenia purpura",
           "Disseminated intravascular coagulation",
           "Immune thrombocytopenia",
           "Intracranial venous thrombosis") )%>% 
  filter(strata=="overall")  %>% 
  filter(study.year=="all") %>% 
  mutate(study.year="Overall")

gg.general.format.facet(
  plot.data %>% 
  ggplot(aes(db,ir_100000, shape=pop.type)) +
  facet_grid(outcome.name~ study.year, scales = "free_y", switch="y")+
  geom_point(position=position_dodge(width=0.5) )+ 
  geom_errorbar(aes(ymin=ir_100000_lower,ymax=ir_100000_upper), width=0,
                position=position_dodge(width=0.5) )+
  theme_bw()+
  ylab("Incidence rate\nper 100,000 person-years\n")+
  xlab("Database")
  )


# by age and sex
plot.data<-Network.IR %>% 
  filter(outcome.name %in% 
           c("Cerebral venous sinus thrombosis",
           "Thrombotic thrombocytopenia purpura",
           "Disseminated intravascular coagulation",
           "Immune thrombocytopenia",
           "Intracranial venous thrombosis") )%>% 
  filter(strata=="age_gr_gender") %>% 
  filter(study.year=="all") 

gg.general.format.facet(
  plot.data %>% 
  ggplot(aes(age_gr,ir_100000, colour=db, shape=pop.type)) +
  facet_grid(outcome.name~  gender, scales = "free_y", switch="y")+
  geom_point(position=position_dodge(width=0.5) )+
  geom_errorbar(aes(ymin=ir_100000_lower,ymax=ir_100000_upper, linetype=pop.type), 
                width=0,position=position_dodge(width=0.5) )+
  theme_bw()+
  ylab("Incidence rate\nper 100,000 person-years\n")+
  xlab("Age group")
  )


  # by age (fewer groups) and sex
plot.data<-Network.IR %>% 
  filter(outcome.name %in% 
           c("Cerebral venous sinus thrombosis",
           "Thrombotic thrombocytopenia purpura",
           "Disseminated intravascular coagulation",
           "Immune thrombocytopenia",
           "Intracranial venous thrombosis") )%>% 
  filter(strata=="age_gr2_gender") %>% 
  filter(study.year=="all") %>% 
  mutate(db=ifelse(db=="SIDIAP_h", "SIDIAP", db))

gg.general.format.facet(
  plot.data %>% 
  ggplot(aes(age_gr2,ir_100000, colour=db, shape=pop.type, group=paste0(db, pop.type))) +
  facet_grid(outcome.name~  gender, scales = "free_y", switch="y")+
  geom_errorbar(aes(ymin=ir_100000_lower,ymax=ir_100000_upper, linetype=pop.type), size=1,
                width=0,position=position_dodge(width=0.5) )+
  geom_point(position=position_dodge(width=0.5), size=3 )+
  theme_bw()+
  ylab("Incidence rate\nper 100,000 person-years\n")+
  xlab("Age group")+
    scale_color_manual(values=c("#e41a1c", "#377eb8"))
  )



# IR per month for given pop-----

pop.size<-10000000

est<-Network.IR %>% 
  filter(outcome.name %in% 
           c("Cerebral venous sinus thrombosis",
             "Thrombotic thrombocytopenia purpura",
           "Disseminated intravascular coagulation",
           "Immune thrombocytopenia",
           "Intracranial venous thrombosis"))%>% 
  filter(strata=="age_gr2_gender") %>% 
  filter(study.year=="all") %>% 
  select(outcome.name,ir_100000,
         outcome.name,ir_100000_lower,
         outcome.name,ir_100000_upper,
         db, age_gr2,gender,
         pop.type) %>% 
  filter(db=="SIDIAP_H")

est$per.year<-est$ir_100000/1000000
est$per.year.pop<-est$per.year*pop.size
est$per.month<-est$per.year.pop/12

est$per.year_lower<-est$ir_100000_lower/1000000
est$per.year.pop_lower<-est$per.year_lower*pop.size
est$per.month_lower<-est$per.year.pop_lower/12

est$per.year_upper<-est$ir_100000_upper/1000000
est$per.year.pop_upper<-est$per.year_upper*pop.size
est$per.month_upper<-est$per.year.pop_upper/12






ggplot(data = est, aes(x =db , y = outcome.name)) + 
  facet_grid(outcome.name+pop.type~  age_gr2+ gender, scales = "free", switch="y")+
  geom_tile(aes(fill = per.month_upper), color = "white", size = 1)+
  geom_text(aes(label=paste0(round(per.month),
                             "\n(", round(per.month_lower), " to ",
                round(per.month_upper), ")"),
                fontface = "bold")) +
  theme(panel.spacing = unit(0.1, "lines"),
        legend.title = element_blank(),
        axis.text =element_blank(),
        axis.ticks = element_blank(),
        axis.title=element_blank(),
        strip.text = element_text(size=14, face="bold"),
        strip.text.y.left = element_text(angle = 0),
        strip.background = element_rect( fill="#f7f7f7"),
       axis.title.y =  element_blank(),
        legend.text=element_text(size=14)) +
  scale_x_discrete(expand = c(0, 0))+
  scale_y_discrete(expand = c(0, 0))+ 
  scale_fill_gradient(low = "#fee0d2", 
                      high = "#ef3b2c") 





           


# ----------
plot.age<-Pop %>% 
  ggplot()+
  geom_histogram(aes(x=age, y=..density..),
                 colour="black", 
                 binwidth = 4, boundary = 0,
                 fill="grey")+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold")) +
  theme_bw()+
  ylab("Density")+
  xlab("Age")

plot.data<-Pop %>% 
  group_by(gender) %>% 
  summarise(n=length(gender), 
            fraction=length(gender)/nrow(Pop))
plot.data$ymax = cumsum(plot.data$fraction)
plot.data$ymin = c(0, head(plot.data$ymax, n=-1))
plot.data$labelPosition <- (plot.data$ymax + plot.data$ymin) / 2
plot.data$label <- paste0(plot.data$gender, "\n", 
                          nice.num(plot.data$fraction*100), "%")

plot.gender<-ggplot(plot.data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=gender)) +
     geom_rect(colour="black", size=1.4) +
     coord_polar(theta="y") + # Try to remove that to understand how the chart is built initially
     xlim(c(2.25, 4)) +
  geom_label( x=3.5,label.size=NA,
              aes(y=labelPosition, 
                  label=label), size=6) +
  theme_void()+
  theme(legend.position = "none")+
  scale_fill_manual(values=c("#F21A00", "#3B9AB2"))

plot_grid(plot.age, plot.gender,ncol = 2, rel_heights = c(0.65, 0.35))