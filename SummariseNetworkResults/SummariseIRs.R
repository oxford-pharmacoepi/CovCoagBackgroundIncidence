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

# IRs for events of interest ----
plot.data<-Network.IR %>% 
  filter(outcome.name %in% 
           c(c("CVST",
               "CVST (with thrombocytopenia 10 days pre to 10 days post)",
               "DIC",
               "DIC (with thrombocytopenia 10 days pre to 10 days post)",
                   "PE (with thrombocytopenia 10 days pre to 10 days post)", 
                   "DVT broad (with thrombocytopenia 10 days pre to 10 days post)", 
                 "imm throm",
                "all stroke (with thrombocytopenia 10 days pre to 10 days post)",
               "SVT",
               "SVT (with thrombocytopenia 10 days pre to 10 days post)")) )%>% 
  mutate(outcome.name=ifelse(outcome.name=="PE (with thrombocytopenia 10 days pre to 10 days post)", 
                      "Pulmonary\nembolism -\nthrombocytopenia",
                      outcome.name)) %>%
    mutate(outcome.name=ifelse(outcome.name=="CVST (with thrombocytopenia 10 days pre to 10 days post)",
                               "Cerebral venous\nsinus thrombosis -\nthrombocytopenia",
                      outcome.name)) %>%
    mutate(outcome.name=ifelse(outcome.name=="CVST",
                               "Cerebral venous\nsinus thrombosis",
                      outcome.name)) %>%
    mutate(outcome.name=ifelse(outcome.name=="DIC",
                               "Disseminated\nintravascular\ncoagulation",
                      outcome.name)) %>%
    mutate(outcome.name=ifelse(outcome.name=="DIC (with thrombocytopenia 10 days pre to 10 days post)",
                               "Disseminated\nintravascular\ncoagulation -\nthrombocytopenia",
                      outcome.name)) %>%
  mutate(outcome.name=ifelse(outcome.name=="DVT broad (with thrombocytopenia 10 days pre to 10 days post)", 
                      "Deep vein\nthrombosis -\nthrombocytopenia",
                      outcome.name)) %>%
    mutate(outcome.name=ifelse(outcome.name=="all stroke (with thrombocytopenia 10 days pre to 10 days post)", "Stroke -\nthrombocytopenia",
                      outcome.name)) %>%
  mutate(outcome.name=ifelse(outcome.name=="imm throm", "Immune\nthrombocytopenia",
                      outcome.name))  %>%
  mutate(outcome.name=ifelse(outcome.name=="SVT", 
                             "Splanchnic vein\nthrombosis",
                      outcome.name))%>%
  mutate(outcome.name=ifelse(outcome.name=="SVT (with thrombocytopenia 10 days pre to 10 days post)", 
                             "Splanchnic vein\nthrombosis -\nthrombocytopenia",
                      outcome.name)) %>%
  mutate(outcome.name=factor(outcome.name, 
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
  mutate(study.year=ifelse(study.year=="all","Overall", study.year)) %>% 
  filter(prior.obs.required=="No") %>% 
  filter(pop.type=="general.pop.all")

equal_breaks <- function(n = 3, s = 0.05, ...){
  function(x){
   
    # rescaling
    # d <- s * diff(range(x)) / (1+2*s)
    # round(seq(min(x)+d, max(x)-d, length=n))[1:(n-1)]
    # round(c(0, as.numeric(quantile(x, 1/3)),as.numeric(quantile(x, 2/3) )))
    round(c(0, as.numeric(quantile(x, 0.5)) ))
  }
}

p<-gg.general.format.facet(
  plot.data %>% 
  filter(strata %in% c("age_gr2_gender")) %>% 
  ggplot(aes(age_gr2,ir_100000, colour=db)) +
  facet_grid(outcome.name~ gender, scales = "free", switch="y")+
  geom_point(position=position_dodge(width=0.5) )+ 
  geom_errorbar(aes(ymin=ir_100000_lower,ymax=ir_100000_upper), width=0,
                position=position_dodge(width=0.5))+
  theme_bw()+
  ylab("Incidence rate\nper 100,000 person-years\n")+
  xlab("Database")
  )+theme(panel.spacing = unit(0.15, "lines"))+
  scale_y_continuous(position = "right", limits=c(0,NA),
                     breaks=equal_breaks(n=4))

# colours by type
p
# g <- ggplot_gtable(ggplot_build(p))
# strip_both <- which(grepl('strip-l', g$layout$name))
# fills <- c(rep("#d9d9d9",2),
#            rep("#bdbdbd",1),
#            rep("#969696",3))
# k <- 1
# for (i in strip_both) {
# j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
# g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
# k <- k+1
# }
# grid::grid.draw(g)


# IR per month for given pop-----
pop.size<-10000000

# by age and gender
est<-plot.data %>%
  filter(strata=="overall") %>%
  filter(study.year=="Overall") %>%
  select(outcome.name,ir_100000,
         outcome.name,ir_100000_lower,
         outcome.name,ir_100000_upper,
         db, age_gr2,gender,
         pop.type) %>%
  filter(pop.type=="general.pop.all")
  

est$per.year<-est$ir_100000/100000
est$per.year.pop<-est$per.year*pop.size
est$per.28.day<-(est$per.year.pop/365.25)*28

est$per.year_lower<-est$ir_100000_lower/100000
est$per.year.pop_lower<-est$per.year_lower*pop.size
est$per.28.day_lower<-(est$per.year.pop_lower/365.25)*28

est$per.year_upper<-est$ir_100000_upper/100000
est$per.year.pop_upper<-est$per.year_upper*pop.size
est$per.28.day_upper<-(est$per.year.pop_upper/365.25)*28

p<-ggplot(data = est, 
           aes(x =1 , y = 1)) + 
  facet_grid(outcome.name~  db, scales = "free", switch="y")+
  geom_tile(aes(fill = per.28.day_upper), color = "white", size = 1)+
  geom_text(aes(label=paste0(nice.num.count(per.28.day),
                             "\n(", nice.num.count(per.28.day_lower), " to ",
                nice.num.count(per.28.day_upper), ")"),
                fontface = "bold")) +
  theme(axis.text.y = element_blank(),
    panel.spacing = unit(0, "lines"),
        legend.title = element_blank(),
        axis.text =element_text(size=6, colour="black", face="bold"),
        axis.ticks = element_blank(),
        axis.title=element_blank(),
        strip.text = element_text(size=12, face="bold"),
        strip.text.y.left = element_text(angle = 0),
        strip.background = element_rect( fill="#f7f7f7"),
       axis.title.y =  element_blank(),
        legend.text=element_text(size=10),
      legend.position = "bottom") +
  scale_x_discrete(expand = c(0, 0))+
  scale_y_discrete(expand = c(0, 0),position = "right")+ 
  scale_fill_gradient(low = "#fee0d2", 
                      high = "#ef3b2c")

# colours by type
p
# g <- ggplot_gtable(ggplot_build(p))
# strip_both <- which(grepl('strip-l', g$layout$name))
# fills <- c(rep("#d9d9d9",2),
#            rep("#bdbdbd",1),
#            rep("#969696",3))
# k <- 1
# for (i in strip_both) {
# j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
# g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
# k <- k+1
# }
# grid::grid.draw(g)

# table ------
table.data<-Network.IR %>% 
  filter(outcome.name %in% 
           c(c("CVST",
               "CVST (with thrombocytopenia 10 days pre to 10 days post)",
               "DIC",
               "DIC (with thrombocytopenia 10 days pre to 10 days post)",
                   "PE (with thrombocytopenia 10 days pre to 10 days post)", 
                   "DVT broad (with thrombocytopenia 10 days pre to 10 days post)", 
                 "imm throm",
                "all stroke (with thrombocytopenia 10 days pre to 10 days post)",
               "SVT",
               "SVT (with thrombocytopenia 10 days pre to 10 days post)")) )%>% 
  mutate(outcome.name=ifelse(outcome.name=="PE (with thrombocytopenia 10 days pre to 10 days post)", 
                      "Pulmonary embolism - thrombocytopenia",
                      outcome.name)) %>%
    mutate(outcome.name=ifelse(outcome.name=="CVST (with thrombocytopenia 10 days pre to 10 days post)",
                               "Cerebral venous sinus thrombosis - thrombocytopenia",
                      outcome.name)) %>%
    mutate(outcome.name=ifelse(outcome.name=="CVST",
                               "Cerebral venous sinus thrombosis",
                      outcome.name)) %>%
    mutate(outcome.name=ifelse(outcome.name=="DIC",
                               "Disseminated intravascular coagulation",
                      outcome.name)) %>%
    mutate(outcome.name=ifelse(outcome.name=="DIC (with thrombocytopenia 10 days pre to 10 days post)",
                               "Disseminated intravascular coagulation - thrombocytopenia",
                      outcome.name)) %>%
  mutate(outcome.name=ifelse(outcome.name=="DVT broad (with thrombocytopenia 10 days pre to 10 days post)", 
                      "Deep vein thrombosis - thrombocytopenia",
                      outcome.name)) %>%
    mutate(outcome.name=ifelse(outcome.name=="all stroke (with thrombocytopenia 10 days pre to 10 days post)", "Stroke - thrombocytopenia",
                      outcome.name)) %>%
  mutate(outcome.name=ifelse(outcome.name=="imm throm", "Immune thrombocytopenia",
                      outcome.name))  %>%
  mutate(outcome.name=ifelse(outcome.name=="SVT", 
                             "Splanchnic vein thrombosis",
                      outcome.name))%>%
  mutate(outcome.name=ifelse(outcome.name=="SVT (with thrombocytopenia 10 days pre to 10 days post)", 
                             "Splanchnic vein thrombosis - thrombocytopenia",
                      outcome.name)) %>%
  mutate(study.year=ifelse(study.year=="all","Overall", study.year)) %>% 
  filter(prior.obs.required=="No") %>% 
  filter(pop.type=="general.pop.all")

table.data<-table.data %>% 
  filter(strata=="overall" ) %>% 
  filter(study.year=="Overall") %>% 
  filter(pop.type=="general.pop.all")%>% 
  select(n,years,events,
         outcome.name,ir_100000,
         outcome.name,ir_100000_lower,
         outcome.name,ir_100000_upper,
         db, age_gr2,gender) %>% 
  mutate(n=nice.num.count(n),
         years=nice.num.count(years),
         events=nice.num.count(events),
         ir=paste0(nice.num(ir_100000), 
                   " (", nice.num(ir_100000_lower), 
                   " to ", nice.num(ir_100000_upper), ")" )) %>% 
  select(db, 
         outcome.name,
         n,years,events,
         ir) 

table.data<-table.data %>% 
  pivot_wider(
    names_from = db, 
    names_glue = "{db} {.value}",
    values_from = c(n,years,events,ir))

table.data<-bind_cols(
  table.data %>% select(outcome.name),
  table.data %>% 
    select(-outcome.name) %>% 
  select(order(colnames(table.data %>% 
    select(-outcome.name))))
)

write.csv(table.data,
          here("SummariseNetworkResults", "table2.csv"))


           


