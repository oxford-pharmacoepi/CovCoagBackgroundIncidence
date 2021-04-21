source(here("Functions", "Functions.R"))

output.folder<-here::here("output")
output.files<-list.files(output.folder)

# load  -----
# dbs
db.names<-output.files[str_detect(output.files, "IR.summary")]
db.names<-str_replace(db.names, "IR.summary_","")
db.names<-str_replace(db.names, ".RData","")


Network.IR<-list()
for(i in 1:length(db.names)){
  load(paste0(output.folder,"/IR.summary_", db.names[i], ".RData"))
 Network.IR[[db.names[i]]]<-IR.summary
 rm(IR.summary)
}
Network.IR<-bind_rows(Network.IR)


# add CIs -----
IR.conf<-epi.conf(as.matrix(cbind(Network.IR$events, Network.IR$years)),
         ctype = "inc.rate", method = "exact", conf.level = 0.95)
Network.IR$ir_100000_lower<-IR.conf$lower* 100000
Network.IR$ir_100000_upper<-IR.conf$upper* 100000
rm(IR.conf) 

# Venous thromb events ----
plot.data<-Network.IR %>% 
  filter(outcome.name %in% 
           c("DVT broad",
           # "DVT narrow",
           "PE",
           "VTE broad",
           "VTE narrow") )%>% 
  mutate(study.year=ifelse(study.year=="all","Overall", study.year)) %>% 
  filter(prior.obs.required=="No") %>% 
  filter(pop.type=="general.pop.all")

gg.general.format.facet(
  plot.data %>% 
  filter(strata %in% c("age_gr_gender")) %>% 
    # mutate(outcome.name=ifelse(outcome.name=="MI", "Myocardial infarction", outcome.name)) %>% 
    # mutate(outcome.name=ifelse(outcome.name=="MI isc stroke", "Myocardial infarction\nor ischemic stroke", outcome.name)) %>% 
    # mutate(outcome.name=ifelse(outcome.name=="isc stroke", "Ischemic stroke", outcome.name)) %>% 
    # mutate(outcome.name=factor(outcome.name, 
    #                               levels=c("Myocardial infarction",
    #                                        "Ischemic stroke",
    #                                        "Myocardial infarction\nor ischemic stroke"))) %>% 
  ggplot(aes(db,ir_100000, colour=gender)) +
  facet_grid(outcome.name~ age_gr, scales = "free", switch="y")+
  geom_point(position=position_dodge(width=0.5) )+ 
  geom_errorbar(aes(ymin=ir_100000_lower,ymax=ir_100000_upper), width=0,
                position=position_dodge(width=0.5))+
  theme_bw()+
  ylab("Incidence rate\nper 100,000 person-years\n")+
  xlab("Database")
  )+theme(panel.spacing = unit(0, "lines"))


# Art thromb events ----
plot.data<-Network.IR %>% 
  filter(outcome.name %in% 
           c("MI isc stroke",
           "isc stroke",
           "MI") )%>% 
  mutate(study.year=ifelse(study.year=="all","Overall", study.year)) %>% 
  filter(prior.obs.required=="No") %>% 
  filter(pop.type=="general.pop.all")

gg.general.format.facet(
  plot.data %>% 
  filter(strata %in% c("age_gr_gender")) %>% 
    mutate(outcome.name=ifelse(outcome.name=="MI", "Myocardial infarction", outcome.name)) %>% 
    mutate(outcome.name=ifelse(outcome.name=="MI isc stroke", "Myocardial infarction\nor ischemic stroke", outcome.name)) %>% 
    mutate(outcome.name=ifelse(outcome.name=="isc stroke", "Ischemic stroke", outcome.name)) %>% 
    mutate(outcome.name=factor(outcome.name, 
                                  levels=c("Myocardial infarction",
                                           "Ischemic stroke",
                                           "Myocardial infarction\nor ischemic stroke"))) %>% 
  ggplot(aes(db,ir_100000, colour=gender)) +
  facet_grid(outcome.name~ age_gr, scales = "free", switch="y")+
  geom_point(position=position_dodge(width=0.5) )+ 
  geom_errorbar(aes(ymin=ir_100000_lower,ymax=ir_100000_upper), width=0,
                position=position_dodge(width=0.5))+
  theme_bw()+
  ylab("Incidence rate\nper 100,000 person-years\n")+
  xlab("Database")
  )+theme(panel.spacing = unit(0, "lines"))

# rare events (w/o thrombocytop) ------
plot.data<-Network.IR %>% 
  filter(outcome.name %in% 
           c("CVST",
             "DIC","hepatic vein","HIT",
            "imm throm","intest infarc","IVT",
            "portal vein","splenic infarc","SVT",
           "TP",
           "TTP", "visc venous" ) )%>% 
  mutate(study.year=ifelse(study.year=="all","Overall", study.year)) %>% 
  filter(prior.obs.required=="No") %>% 
  filter(pop.type=="general.pop.all")

gg.general.format.facet(
  plot.data %>% 
  filter(strata %in% c("age_gr2_gender")) %>% 
  ggplot(aes(db,ir_100000, colour=gender)) +
  facet_grid(outcome.name~ age_gr2, scales = "free", switch="y")+
  geom_point(position=position_dodge(width=0.5) )+ 
  geom_errorbar(aes(ymin=ir_100000_lower,ymax=ir_100000_upper), width=0,
                position=position_dodge(width=0.5))+
  theme_bw()+
  ylab("Incidence rate\nper 100,000 person-years\n")+
  xlab("Database")
  )+theme(panel.spacing = unit(0, "lines"))

# ------

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
  facet_grid(.~ db, scales = "free_y", switch="y")+
  geom_errorbar(aes(ymin=ir_100000_lower,ymax=ir_100000_upper), width=.1)+
  coord_flip()+
  theme_bw()
  
  

# focus on 
# Cerebral venous sinus thrombosis
# Thrombotic thrombocytopenia purpura
# Disseminated intravascular coagulation 
# Immune thrombocytopenia
# Intracranial venous thrombosis

table.2<-Network.IR %>% 
  filter(db=="CPRD") %>% 
  filter(outcome.name %in% 
           c("Cerebral venous sinus thrombosis",
           "Thrombotic thrombocytopenia purpura",
           "Disseminated intravascular coagulation",
           "Immune thrombocytopenia",
           "Intracranial venous thrombosis") )%>% 
  filter(strata=="age_gr2_gender")  %>% 
  mutate(study.year=ifelse(study.year=="all","Overall", study.year)) %>% 
  filter(pop.type=="general.pop.all") %>% 
  select(n,age_gr2, gender, years, events, outcome.name, db,ir_100000,ir_100000_lower,ir_100000_upper) %>% 
  mutate(n.CPRD=paste0(nice.num.count(n))) %>% 
  mutate(years.CPRD=paste0(nice.num.count(years))) %>% 
  mutate(events.CPRD=paste0(nice.num.count(events))) %>% 
  mutate(ir_100000_summ.CPRD=paste0(nice.num(ir_100000),
                               " (", nice.num(ir_100000_lower), " to ", 
                               nice.num(ir_100000_upper),")" )) %>% 
  select(outcome.name, age_gr2, gender, n.CPRD, years.CPRD, events.CPRD, ir_100000_summ.CPRD) %>% 
  full_join(
Network.IR %>% 
  filter(db=="SIDIAP_H") %>% 
  filter(outcome.name %in% 
           c("Cerebral venous sinus thrombosis",
           "Thrombotic thrombocytopenia purpura",
           "Disseminated intravascular coagulation",
           "Immune thrombocytopenia",
           "Intracranial venous thrombosis") )%>% 
  filter(strata=="age_gr2_gender")  %>% 
  mutate(study.year=ifelse(study.year=="all","Overall", study.year)) %>% 
  filter(pop.type=="general.pop.all") %>% 
  select(n,age_gr2, gender, years, events, outcome.name, db,ir_100000,ir_100000_lower,ir_100000_upper) %>% 
  mutate(n.SIDIAP_H=paste0(nice.num.count(n))) %>% 
  mutate(years.SIDIAP_H=paste0(nice.num.count(years))) %>% 
  mutate(events.SIDIAP_H=paste0(nice.num.count(events))) %>% 
  mutate(ir_100000_summ.SIDIAP_H=paste0(nice.num(ir_100000),
                               " (", nice.num(ir_100000_lower), " to ", 
                               nice.num(ir_100000_upper),")" )) %>% 
  select(outcome.name, age_gr2, gender, n.SIDIAP_H, years.SIDIAP_H, events.SIDIAP_H, ir_100000_summ.SIDIAP_H))

myHeader <- c(" " = 3, a = 4, b=4)
names(myHeader) <- c(" ", "CPRD", "SIDIAP")

kable(table.2,
      col.names = c("Outcome",
                    "Age",
                    "Sex",
                    "n",
                   "PYs",
                    "Events",
                    "IR per 100,000 PYs",
                    "n",
                   "PYs",
                    "Events",
                    "IR per 100,000 PYs")) %>% 
 kable_styling(bootstrap_options = c("striped", "bordered"))%>%
 add_header_above(header = myHeader)


SummaryPatientCharacteristics[[db.names[i]]]<-kable(table.data) %>% 
 kable_styling(bootstrap_options = c("striped", "bordered"))%>%
 add_header_above(header = myHeader)


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
  ggplot(aes(db,ir_100000, shape=pop.type, colour=db)) +
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
  filter(study.year=="all") 

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

plot.data<-Network.IR %>% 
  filter(outcome.name %in% 
           c("Cerebral venous sinus thrombosis",
           "Thrombotic thrombocytopenia purpura",
           "Disseminated intravascular coagulation",
           "Immune thrombocytopenia",
           "Intracranial venous thrombosis") )%>% 
  filter(strata %in% c("overall","age_gr2_gender")) %>% 
  filter(study.year=="all") %>% 
  filter(pop.type=="general.pop.all") %>% 
  mutate(age_gr2=ifelse(strata=="overall", "Overall", as.character(age_gr2))) %>% 
  mutate(age_gr2=factor(age_gr2,
                        levels=c("<=44","45-64",">=65", "Overall" ))) %>% 
  mutate(gender=ifelse(strata=="overall", "Overall", as.character(gender))) 
  
  

gg.general.format.facet(
  plot.data %>% 
  ggplot(aes(age_gr2,ir_100000, colour=db, type, group=paste0(db, pop.type))) +
  facet_grid(outcome.name~  gender, scales = "free", switch="y")+
  geom_errorbar(aes(ymin=ir_100000_lower,ymax=ir_100000_upper), size=1,
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
  filter(pop.type=="general.pop.all")
  

est$per.year<-est$ir_100000/1000000
est$per.year.pop<-est$per.year*pop.size
est$per.month<-est$per.year.pop/12

est$per.year_lower<-est$ir_100000_lower/1000000
est$per.year.pop_lower<-est$per.year_lower*pop.size
est$per.month_lower<-est$per.year.pop_lower/12

est$per.year_upper<-est$ir_100000_upper/1000000
est$per.year.pop_upper<-est$per.year_upper*pop.size
est$per.month_upper<-est$per.year.pop_upper/12





ggplot(data = est, 
           aes(x =1 , y = db)) + 
  facet_grid(outcome.name~  age_gr2+ gender, scales = "free", switch="y")+
  geom_tile(aes(fill = per.month_upper), color = "white", size = 1)+
  geom_text(aes(label=paste0(round(per.month),
                             "\n(", round(per.month_lower), " to ",
                round(per.month_upper), ")"),
                fontface = "bold")) +
  theme(panel.spacing.x = unit(0, "lines"),
        panel.spacing.y = unit(1, "lines"),
        legend.title = element_blank(),
        axis.text =element_text(size=12, colour="black", face="bold"),
        axis.ticks = element_blank(),
        axis.title=element_blank(),
        strip.text = element_text(size=14, face="bold"),
        strip.text.y.left = element_text(angle = 0),
        strip.background = element_rect( fill="#f7f7f7"),
       axis.title.y =  element_blank(),
        legend.text=element_text(size=14),
      legend.position = "bottom") +
  scale_x_discrete(expand = c(0, 0))+
  scale_y_discrete(expand = c(0, 0),position = "right")+ 
  scale_fill_gradient(low = "#fee0d2", 
                      high = "#ef3b2c") 



           


