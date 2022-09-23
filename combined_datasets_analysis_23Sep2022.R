## Comparing various data sources about school reopening policies - UPDATED ANALYSES

library(dplyr)
library(tidyr)
library(ecodist)
library(data.table)
library(ggplot2)
library(irr)
library(maps)
library(glmnet)
library(lme4)
library(ggpubr)
library(wCorr)
library(reshape2)
library(pastecs)
library(varhandle)
library(sjPlot)
library(Metrics)

### DATA ###
#setwd()
all_state <- readRDS("AllMonthlyState.RDS")
all_county <- readRDS("AllMonthlyCounty.RDS")
all_county$fips <- unfactor(all_county$fips)
all_top_county <- readRDS("AllMonthlyTopCounty.RDS")
all_top_county$fips <- unfactor(all_top_county$fips)
all_district <- readRDS("AllJanuaryDistrict.RDS")

burbio_12_4 <- read.csv("schoolOpeningTracker+-+12-4-20.csv", header=T)
burbio_12_4$County.Pop <- as.numeric(gsub(",","", burbio_12_4$County.Pop))
all_top_county <- burbio_12_4 %>% 
  rename(fips=FIPS) %>%
  select(fips,County.Pop) %>%
  right_join(all_top_county,by="fips")


### STATE COMPARISONS ###
# figure
all_state %>%
  group_by(State) %>%
  summarise(Burbio = mean(AnyInPerson.Burbio,na.rm=T),
            CTIS = weighted.mean(AnyInPerson.FB.weight,weight,na.rm=T),
            CTIS_unweight = mean(AnyInPerson.FB.unweight,na.rm=T),
            SCDL_unweight = mean(AnyInPerson.Phone,na.rm=T),
            SCDL = mean(AnyInPerson.Phone.weight,na.rm=T)) %>%
  pivot_longer(c(Burbio,CTIS,SCDL),names_to = "Source",values_to = "Percent") %>%
  ggplot(aes(x=State,y=Percent,group=Source,color=Source)) +
  geom_line() + geom_point(aes(shape=Source), size=3) +
  theme_minimal(base_size=13)

# appendix figure
all_state_adj <- rename(all_state,state=State)
ctis_states <- usmap::plot_usmap(regions = "states",
                  data = select(all_state_adj,state,AnyInPerson.FB.weight),
                  values = "AnyInPerson.FB.weight") +
  ggtitle("CTIS") +
  colorspace::scale_fill_continuous_divergingx(palette = "Tropic",mid=50, 
                                               limits=c(0,100), name="Percent Onsite") +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, vjust = -1, size = 12))
burb_states <- usmap::plot_usmap(regions = "states",
                  data = select(all_state_adj,state,AnyInPerson.Burbio),
                  values = "AnyInPerson.Burbio") +
  ggtitle("Burbio") +
  colorspace::scale_fill_continuous_divergingx(palette = "Tropic",mid=50, 
                                               limits=c(0,100), name="Percent Onsite") +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, vjust = -1, size = 12))
scdl_states <- usmap::plot_usmap(regions = "states",
                  data = select(all_state_adj,state,AnyInPerson.Phone.weight),
                  values = "AnyInPerson.Phone.weight") +
  ggtitle("SCDL") +
  colorspace::scale_fill_continuous_divergingx(palette = "Tropic",mid=50, 
                                               limits=c(0,100), name="Percent Onsite") +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, vjust = -1, size = 12))
ggarrange(ctis_states,burb_states,scdl_states)

# appendix figure B&W
ctis_states <- usmap::plot_usmap(regions = "states",
                                 data = select(all_state_adj,state,AnyInPerson.FB.weight),
                                 values = "AnyInPerson.FB.weight") +
  ggtitle("CTIS") +
  scale_fill_continuous(low="white",high="black", limits=c(0,100), name="Percent Onsite") +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, vjust = -1, size = 12))
burb_states <- usmap::plot_usmap(regions = "states",
                                 data = select(all_state_adj,state,AnyInPerson.Burbio),
                                 values = "AnyInPerson.Burbio") +
  ggtitle("Burbio") +
  scale_fill_continuous(low="white",high="black", limits=c(0,100), name="Percent Onsite") +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, vjust = -1, size = 12))
scdl_states <- usmap::plot_usmap(regions = "states",
                                 data = select(all_state_adj,state,AnyInPerson.Phone.weight),
                                 values = "AnyInPerson.Phone.weight") +
  ggtitle("SCDL") +
  scale_fill_continuous(low="white",high="black", limits=c(0,100), name="Percent Onsite") +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, vjust = -1, size = 12))
ggarrange(ctis_states,burb_states,scdl_states)

# correlations
cor.test(all_state$AnyInPerson.Burbio,all_state$AnyInPerson.FB.weight)
cor.test(all_state$AnyInPerson.Burbio,all_state$AnyInPerson.Phone.weight)
cor.test(all_state$AnyInPerson.FB.weight,all_state$AnyInPerson.Phone.weight)
# 75% cutoff
cor.test(all_state$AnyInPerson.Burbio,all_state$AnyInPerson.Phone.75Percent.weight)
cor.test(all_state$AnyInPerson.FB.weight,all_state$AnyInPerson.Phone.75Percent.weight)

# rmse
rmse(all_state$AnyInPerson.Burbio,all_state$AnyInPerson.FB.weight)
rmse(all_state$AnyInPerson.Burbio,all_state$AnyInPerson.Phone.weight)
rmse(all_state$AnyInPerson.FB.weight,all_state$AnyInPerson.Phone.weight)

rmse(all_state$AnyInPerson.Burbio,all_state$AnyInPerson.Phone.75Percent.weight)
rmse(all_state$AnyInPerson.FB.weight,all_state$AnyInPerson.Phone.75Percent.weight)

# differences
all_state_diff <- all_state %>%
  mutate(CTIS_Burb = AnyInPerson.FB.weight-AnyInPerson.Burbio,
         CTIS_SCDL = AnyInPerson.FB.weight-AnyInPerson.Phone,
         Burb_SCDL = AnyInPerson.Burbio-AnyInPerson.Phone)
mean(all_state_diff$CTIS_Burb)
sd(all_state_diff$CTIS_Burb)
mean(all_state_diff$CTIS_SCDL)
sd(all_state_diff$CTIS_SCDL)
mean(all_state_diff$Burb_SCDL)
sd(all_state_diff$Burb_SCDL)



### COUNTY COMPARISONS ###
# figure
ctis_burb <- all_top_county %>%
  group_by(fips) %>%
  summarise(Burbio = mean(AnyInPerson.Burbio,na.rm=T),
            CTIS = weighted.mean(AnyInPerson.FB.weight,weight,na.rm=T),
            SCDL = mean(AnyInPerson.Phone,na.rm=T)) %>%
  ggplot(aes(x=Burbio,
             y=CTIS)) +
  geom_point(shape=21,size=2,color="black",fill="grey") +
  xlab("Burbio Percent") + ylab("CTIS Percent") +
  ylim(0,100) +
  theme(axis.text.x = element_text(size=13),
        axis.text.y = element_text(size=13),
        axis.title = element_text(size=13),
        legend.title = element_text(size=13),
        legend.text = element_text(size=13),
        panel.background = element_rect(fill="white",colour="grey"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "grey"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "grey")) +
  geom_abline(slope=1,intercept=0)
ctis_scdl <- all_top_county %>%
  group_by(fips) %>%
  summarise(Burbio = mean(AnyInPerson.Burbio,na.rm=T),
            CTIS = weighted.mean(AnyInPerson.FB.weight,weight,na.rm=T),
            SCDL = mean(AnyInPerson.Phone,na.rm=T)) %>%
  ggplot(aes(x=SCDL,
             y=CTIS)) +
  geom_point(shape=21,size=2,color="black",fill="grey") +
  xlab("SCDL Percent") + ylab("CTIS Percent") +
  ylim(0,100) +
  theme(axis.text.x = element_text(size=13),
        axis.text.y = element_text(size=13),
        axis.title = element_text(size=13),
        legend.title = element_text(size=13),
        legend.text = element_text(size=13),
        panel.background = element_rect(fill="white",colour="grey"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "grey"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "grey")) +
  geom_abline(slope=1,intercept=0)
scdl_burb <- all_top_county %>%
  group_by(fips) %>%
  summarise(Burbio = mean(AnyInPerson.Burbio,na.rm=T),
            CTIS = weighted.mean(AnyInPerson.FB.weight,weight,na.rm=T),
            SCDL = mean(AnyInPerson.Phone,na.rm=T)) %>%
  ggplot(aes(x=Burbio,
             y=SCDL)) +
  geom_point(shape=21,size=2,color="black",fill="grey") +
  xlab("Burbio Percent") + ylab("SCDL Percent") +
  ylim(0,100) +
  theme(axis.text.x = element_text(size=13),
        axis.text.y = element_text(size=13),
        axis.title = element_text(size=13),
        legend.title = element_text(size=13),
        legend.text = element_text(size=13),
        panel.background = element_rect(fill="white",colour="grey"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "grey"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "grey")) +
  geom_abline(slope=1,intercept=0)
ggarrange(ctis_burb,ctis_scdl,scdl_burb)

# figure not averaged
ctis_burb2 <- all_top_county %>%
  ggplot(aes(x=AnyInPerson.Burbio,
             y=AnyInPerson.FB.weight,
             size=County.Pop/1000,
             color=factor(month,levels=c("12","1","2","3","4","5")))) +
  geom_point() +
  xlab("Burbio Percent") + ylab("CTIS Percent") +
  ylim(0,100) +
  labs(size = "Population (1000s)",
       color = "Month") +
  theme(axis.text.x = element_text(size=13),
        axis.text.y = element_text(size=13),
        axis.title = element_text(size=13),
        panel.background = element_rect(fill="white",colour="grey"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "grey"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "grey")) +
  geom_abline(slope=1,intercept=0)
ctis_scdl2 <- all_top_county %>%
  ggplot(aes(x=AnyInPerson.Phone,
             y=AnyInPerson.FB.weight,
             size=County.Pop/1000,
             color=factor(month,levels=c("12","1","2","3","4","5")))) +
  geom_point() +
  xlab("SCDL Percent") + ylab("CTIS Percent") +
  ylim(0,100) +
  labs(size = "Population (1000s)",
       color = "Month") +
  theme(axis.text.x = element_text(size=13),
        axis.text.y = element_text(size=13),
        axis.title = element_text(size=13),
        panel.background = element_rect(fill="white",colour="grey"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "grey"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "grey")) +
  geom_abline(slope=1,intercept=0)
scdl_burb2 <- all_top_county %>%
  ggplot(aes(x=AnyInPerson.Burbio,
             y=AnyInPerson.Phone,
             size=County.Pop/1000,
             color=factor(month,levels=c("12","1","2","3","4","5")))) +
  geom_point() +
  xlab("Burbio Percent") + ylab("SCDL Percent") +
  ylim(0,100) +
  labs(size = "Population (1000s)",
       color = "Month") +
  theme(axis.text.x = element_text(size=13),
        axis.text.y = element_text(size=13),
        axis.title = element_text(size=13),
        panel.background = element_rect(fill="white",colour="grey"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "grey"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "grey")) +
  geom_abline(slope=1,intercept=0)
ggarrange(ctis_burb2,ctis_scdl2,scdl_burb2)

# other figure
figure2dat <- all_top_county %>%
  mutate(CTIS_Burb = AnyInPerson.FB.weight-AnyInPerson.Burbio,
         CTIS_SCDL = AnyInPerson.FB.weight-AnyInPerson.Phone,
         Burb_SCDL = AnyInPerson.Burbio-AnyInPerson.Phone)
hist_diffs <- function(col,mo,xlabel,ylabel) {
  xint <- mean(figure2dat[,col],na.rm=T)
  figure2dat %>%
    filter(month==mo) %>%
    ggplot(aes_string(x=col)) +
    geom_histogram(binwidth=5,color="black",fill="grey") +
    geom_vline(aes(xintercept=xint),color="blue",linetype="dashed") +
    xlab(xlabel) +
    ylab(ylabel) +
    xlim(-100,100) +
    theme(panel.background = element_rect(fill="white",colour="grey"),
          panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                          colour = "grey"), 
          panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                          colour = "grey"))
}
ggarrange(hist_diffs("CTIS_Burb","12","CTIS-Burbio","December"),
          hist_diffs("CTIS_SCDL","12","CTIS-SCDL","December"),
          hist_diffs("Burb_SCDL","12","Burbio-SCDL","December"),
          hist_diffs("CTIS_Burb","1","CTIS-Burbio","January"),
          hist_diffs("CTIS_SCDL","1","CTIS-SCDL","January"),
          hist_diffs("Burb_SCDL","1","Burbio-SCDL","January"),
          hist_diffs("CTIS_Burb","2","CTIS-Burbio","February"),
          hist_diffs("CTIS_SCDL","2","CTIS-SCDL","February"),
          hist_diffs("Burb_SCDL","2","Burbio-SCDL","February"),
          hist_diffs("CTIS_Burb","3","CTIS-Burbio","March"),
          hist_diffs("CTIS_SCDL","3","CTIS-SCDL","March"),
          hist_diffs("Burb_SCDL","3","Burbio-SCDL","March"),
          hist_diffs("CTIS_Burb","4","CTIS-Burbio","April"),
          hist_diffs("CTIS_SCDL","4","CTIS-SCDL","April"),
          hist_diffs("Burb_SCDL","4","Burbio-SCDL","April"),
          hist_diffs("CTIS_Burb","5","CTIS-Burbio","May"),
          hist_diffs("CTIS_SCDL","5","CTIS-SCDL","May"),
          hist_diffs("Burb_SCDL","5","Burbio-SCDL","May"),
          nrow=6,ncol=3)

t.test(all_top_county$AnyInPerson.FB.weight,all_top_county$AnyInPerson.Burbio,paired=T,alternative="two.sided")
t.test(all_top_county$AnyInPerson.FB.weight,all_top_county$AnyInPerson.Phone,paired=T,alternative="two.sided")
t.test(all_top_county$AnyInPerson.Burbio,all_top_county$AnyInPerson.Phone,paired=T,alternative="two.sided")

summary(figure2dat$CTIS_Burb)
sd(figure2dat$CTIS_Burb)
summary(figure2dat$CTIS_SCDL)
sd(figure2dat$CTIS_SCDL)
summary(figure2dat$Burb_SCDL)
sd(figure2dat$Burb_SCDL)

# correlations
cor.test(all_top_county$AnyInPerson.Burbio,all_top_county$AnyInPerson.FB.weight)
cor.test(all_top_county$AnyInPerson.Burbio,all_top_county$AnyInPerson.Phone)
cor.test(all_top_county$AnyInPerson.FB.weight,all_top_county$AnyInPerson.Phone)

cor.test(all_top_county$AnyInPerson.Burbio,all_top_county$AnyInPerson.Phone.75Percent)
cor.test(all_top_county$AnyInPerson.FB.weight,all_top_county$AnyInPerson.Phone.75Percent)

# rmse
rmse(all_top_county$AnyInPerson.Burbio,all_top_county$AnyInPerson.FB.weight)
rmse(all_top_county$AnyInPerson.Burbio,all_top_county$AnyInPerson.Phone)
rmse(all_top_county$AnyInPerson.FB.weight,all_top_county$AnyInPerson.Phone)

rmse(all_top_county$AnyInPerson.Burbio,all_top_county$AnyInPerson.Phone.75Percent)
rmse(all_top_county$AnyInPerson.FB.weight,all_top_county$AnyInPerson.Phone.75Percent)


## County Factor Comparisons ##
county_factors <- readRDS("county_factors.RDS") %>%
  select(-c("Aug19.UnemployRate","Sep19.UnemployRate","Oct19.UnemployRate","Nov19.UnemployRate","Dec19.UnemployRate",
            "Jan20.UnemployRate","Feb20.UnemployRate","Mar20.UnemployRate","Apr20.UnemployRate","May20.UnemployRate",
            "Jun20.UnemployRate","Jul20.UnemployRate","Aug20.UnemployRate","Sep20p.UnemployRate"))
county_factors$fips <- as.numeric(county_factors$fips)

unemp <- readRDS("MonthlyUnemployment.RDS")
unemp$fips <- as.numeric(unemp$fips)

us_cases_deaths <- readRDS("US Cases and Deaths.RDS")
us_cases_deaths <- us_cases_deaths %>%
  mutate(month=month(date),year=year(date),month.year=paste(month,year,sep="-")) %>%
  filter(month.year %in% c("12-2020","1-2021","2-2021","3-2021","4-2021","5-2021")) %>%
  rename(fips=FIPS) %>%
  group_by(fips,month) %>%
  summarise(monthly_cases = sum(daily_cases,na.rm = T),
            monthly_deaths = sum(daily_deaths,na.rm = T))
  
top_county_factors <- all_top_county %>%
  left_join(county_factors,by="fips") %>%
  left_join(unemp,by=c("fips","month")) %>%
  left_join(us_cases_deaths,by=c("fips","month")) %>%
  mutate(cases_perpop = monthly_cases/Population,
         deaths_perpop = monthly_deaths/Population,
         CTIS_Burb = AnyInPerson.FB.weight-AnyInPerson.Burbio,
         CTIS_SCDL = AnyInPerson.FB.weight-AnyInPerson.Phone,
         Burb_SCDL = AnyInPerson.Burbio-AnyInPerson.Phone)
all_county_factors <- all_county %>%
  left_join(county_factors,by="fips") %>%
  left_join(unemp,by=c("fips","month")) %>%
  left_join(us_cases_deaths,by=c("fips","month")) %>%
  mutate(cases_perpop = monthly_cases/Population,
         deaths_perpop = monthly_deaths/Population,
         CTIS_Burb = AnyInPerson.FB.weight-AnyInPerson.Burbio,
         CTIS_SCDL = AnyInPerson.FB.weight-AnyInPerson.Phone,
         Burb_SCDL = AnyInPerson.Burbio-AnyInPerson.Phone)

# descriptive statistics
top_desc <- top_county_factors %>%
  ungroup %>%
  mutate(Black=Black*100,
         PubAssist.HH=PubAssist.HH*100,
         NoHealthIns=NoHealthIns*100,
         Employed.Essential=Employed.Essential*100,
         Computer.HH=Computer.HH*100,
         AdultwHSdeg=AdultwHSdeg*100) %>%
  select(AnyInPerson.FB.weight,AnyInPerson.Burbio,AnyInPerson.Phone,Population,Black,
         MedHouseIncome,PubAssist.HH,GINI,PopPerSqMile,NoHealthIns,
         Employed.Essential,Computer.HH,AdultwHSdeg,Unemploy.Rate,
         monthly_cases,monthly_deaths) %>%
  stat.desc() %>%
  data.frame()
top_desc_table <- t(top_desc[c(9,13),]) %>% 
  round(1) %>%
  data.frame() %>%
  mutate("Top Counties Mean (SD)"=paste(mean,paste(std.dev,")",sep=""),sep=" (")) %>%
  select(-mean,-std.dev)
rownames(top_desc_table) <- c("CTIS","Burbio","SCDL","Population","Black",
                              "MedHouseIncome","PubAssist.HH","GINI","PopPerSqMile","NoHealthIns",
                              "Employed.Essential","Computer.HH","AdultwHSdeg","Unemploy.Rate",
                              "monthly_cases","monthly_deaths")
all_desc <- all_county_factors %>%
  ungroup %>%
  mutate(Black=Black*100,
         PubAssist.HH=PubAssist.HH*100,
         NoHealthIns=NoHealthIns*100,
         Employed.Essential=Employed.Essential*100,
         Computer.HH=Computer.HH*100,
         AdultwHSdeg=AdultwHSdeg*100) %>%
  select(AnyInPerson.FB.weight,AnyInPerson.Burbio,AnyInPerson.Phone,Population,Black,
         MedHouseIncome,PubAssist.HH,GINI,PopPerSqMile,NoHealthIns,
         Employed.Essential,Computer.HH,AdultwHSdeg,Unemploy.Rate,
         monthly_cases,monthly_deaths) %>%
  stat.desc() %>%
  data.frame()
all_desc_table <- t(all_desc[c(9,13),]) %>% 
  round(1) %>%
  data.frame() %>%
  mutate("All Counties Mean (SD)"=paste(mean,paste(std.dev,")",sep=""),sep=" (")) %>%
  select(-mean,-std.dev)
rownames(all_desc_table) <- c("CTIS","Burbio","SCDL","Population","Black",
                              "MedHouseIncome","PubAssist.HH","GINI","PopPerSqMile","NoHealthIns",
                              "Employed.Essential","Computer.HH","AdultwHSdeg","Unemploy.Rate",
                              "monthly_cases","monthly_deaths")
desc <- cbind(top_desc_table,all_desc_table)
write.csv(desc,"Table 3. Descriptives.csv")


# correlations
covars <- select(top_county_factors,Population,White,Black,WorkAtHome,MarriedCouple.HH,MedHouseIncome,Households,
                 PubAssist.HH,GINI,BBInternet.HH,PopPerSqMile,Age.Adjust.Mortality:deaths_perpop)
outs <- select(top_county_factors,AnyInPerson.Burbio,AnyInPerson.FB.weight,AnyInPerson.Phone,
               CTIS_Burb,CTIS_SCDL,Burb_SCDL)
cor(outs,covars) %>% round(3) %>% melt() %>%
  filter(Var1 != "fips" & Var2 != "fips") %>%
  ggplot(aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-.5,.5), space = "Lab", 
                       name="Correlation") +
  xlab("") + ylab("")

# regression
top_county_factors_long <- top_county_factors %>%
  select(fips,County.Pop,month,AnyInPerson.Burbio,AnyInPerson.FB.weight,AnyInPerson.Phone,White,PubAssist.HH,WorkAtHome,GINI,
         PopPerSqMile,Employed.Essential,Computer.HH,AdultwHSdeg,Unemploy.Rate,cases_perpop) %>%
  pivot_longer(c(AnyInPerson.Burbio,AnyInPerson.FB.weight,AnyInPerson.Phone),names_to="Source",values_to="PercentOnsite")
for (i in 4:13) {
  top_county_factors_long[,i] <- scale(top_county_factors_long[,i])
}
top_county_factors_long$Source <- ifelse(top_county_factors_long$Source=="AnyInPerson.Burbio","Burbio",
                                         ifelse(top_county_factors_long$Source=="AnyInPerson.FB.weight","CTIS","SCDL"))
top_county_factors_long$Source <- factor(top_county_factors_long$Source, levels=c("CTIS","Burbio","SCDL"))
top_county_factors_long$month <- factor(top_county_factors_long$month, levels=c("12","1","2","3","4","5"))
reg_diff <- lm(PercentOnsite ~ Source*month + Source*White + Source*PubAssist.HH + Source*WorkAtHome + Source*GINI + Source*PopPerSqMile +
             Source*Employed.Essential + Source*Computer.HH + Source*AdultwHSdeg + Source*Unemploy.Rate + Source*cases_perpop,
           weights = County.Pop,
           data = top_county_factors_long)
summary(reg_diff)

## mixed effects model
library(lme4)
top_county_factors_long$fips <- factor(top_county_factors_long$fips)
reg_diff_mixed <- lmer(PercentOnsite ~ Source*month + Source*White + Source*PubAssist.HH + Source*WorkAtHome + Source*GINI + Source*PopPerSqMile +
                 Source*Employed.Essential + Source*Computer.HH + Source*AdultwHSdeg + Source*Unemploy.Rate + Source*cases_perpop + (1|fips),
               weights = log(County.Pop),
               data = top_county_factors_long)
summary(reg_diff_mixed)

library(gtsummary)
tbl_regression(reg_diff)
tbl_regression(reg_diff, label = c("Intercept","Source [Burbio]","Source [SCDL]","month [Jan]","month [Feb]",
                                                     "month [Mar]","month [Apr]","month [May]","% White","% on Public Assistance",
                                                     "% Working at Home","GINI","Population Density","% Essential Employees",
                                                     "% with Computers","% Adults with High School Degree","Unemployment Rate",
                                                     "Cases per Population","Source [Burbio] * month [Jan]","Source [SCDL] * month [Jan]",
                                                     "Source [Burbio] * month [Feb]","Source [SCDL] * month [Feb]",
                                                     "Source [Burbio] * month [Mar]","Source [SCDL] * month [Mar]",
                                                     "Source [Burbio] * month [Apr]","Source [SCDL] * month [Apr]",
                                                     "Source [Burbio] * month [May]","Source [SCDL] * month [May]",
                                                     "Source [Burbio] * White","Source [SCDL] * White","Source [Burbio] * % on Public Assistance",
                                                     "Source [SCDL] * % on Public Assistance","Source [Burbio] * % Working at Home",
                                                     "Source [SCDL] * % Working at Home","Source [Burbio] * GINI","Source [SCDL] * GINI",
                                                     "Source [Burbio] * Population Density","Source [SCDL] * Population Density",
                                                     "Source [Burbio] * % Essential Employees","Source [SCDL] * % Essential Employees",
                                                     "Source [Burbio] * % with Computers","Source [SCDL] * % with Computers",
                                                     "Source [Burbio] * % Adults with High School Degree","Source [SCDL] * % Adults with High School Degree",
                                                     "Source [Burbio] * Unemployment Rate","Source [SCDL] * Unemployment Rate",
                                                     "Source [Burbio] * Cases per Population","Source [SCDL] * Cases per Population"))
setwd("~/Desktop")
tab_model(reg_diff_mixed, ci.hyphen = "&comma;&nbsp", file = "RegResults.html",
          pred.labels = c("Intercept","Source [Burbio]","Source [SCDL]","month [Jan]","month [Feb]",
                                    "month [Mar]","month [Apr]","month [May]","% White","% on Public Assistance",
                                    "% Working at Home","GINI","Population Density","% Essential Employees",
                                    "% with Computers","% Adults with High School Degree","Unemployment Rate",
                                    "Cases per Population","Source [Burbio] * month [Jan]","Source [SCDL] * month [Jan]",
                                    "Source [Burbio] * month [Feb]","Source [SCDL] * month [Feb]",
                                    "Source [Burbio] * month [Mar]","Source [SCDL] * month [Mar]",
                                    "Source [Burbio] * month [Apr]","Source [SCDL] * month [Apr]",
                                    "Source [Burbio] * month [May]","Source [SCDL] * month [May]",
                                    "Source [Burbio] * White","Source [SCDL] * White","Source [Burbio] * % on Public Assistance",
                                    "Source [SCDL] * % on Public Assistance","Source [Burbio] * % Working at Home",
                                    "Source [SCDL] * % Working at Home","Source [Burbio] * GINI","Source [SCDL] * GINI",
                                    "Source [Burbio] * Population Density","Source [SCDL] * Population Density",
                                    "Source [Burbio] * % Essential Employees","Source [SCDL] * % Essential Employees",
                                    "Source [Burbio] * % with Computers","Source [SCDL] * % with Computers",
                                    "Source [Burbio] * % Adults with High School Degree","Source [SCDL] * % Adults with High School Degree",
                                    "Source [Burbio] * Unemployment Rate","Source [SCDL] * Unemployment Rate",
                                    "Source [Burbio] * Cases per Population","Source [SCDL] * Cases per Population"))




### DISTRICT COMPARISONS ###
districtplot <- function(method,title) {
  all_district %>%
    rename(Burbio = AnyInPerson.Burbio, SCDL = AnyInPerson.Phone) %>%
    filter(TeachingMethod==method) %>%
    pivot_longer(c(Burbio,SCDL),names_to="Source",values_to="PercentOnsite") %>%
    ggplot(aes(PercentOnsite,fill=Source)) +
    geom_histogram(color="black",binwidth=10) +
    xlab("Percent of Any Onsite Instruction") +
    ylab("Number of Districts") +
    ggtitle(title) +
    theme(plot.title = element_text(hjust = 0.5))
}

ggarrange(districtplot("On Premises","MCH: On Premises"),
          districtplot("Hybrid","MCH: Hybrid"),
          districtplot("Online Only","MCH: Online Only"),
          districtplot("Other","MCH: Other"))

desc_district <- data.frame(n = c(nrow(filter(all_district,TeachingMethod=="On Premises")),
                                  nrow(filter(all_district,TeachingMethod=="Hybrid")),
                                  nrow(filter(all_district,TeachingMethod=="Online Only")),
                                  nrow(filter(all_district,TeachingMethod=="Other"))),
  Mean.Burbio = c(mean(filter(all_district,TeachingMethod=="On Premises")$AnyInPerson.Burbio),
                  mean(filter(all_district,TeachingMethod=="Hybrid")$AnyInPerson.Burbio),
                  mean(filter(all_district,TeachingMethod=="Online Only")$AnyInPerson.Burbio),
                  mean(filter(all_district,TeachingMethod=="Other")$AnyInPerson.Burbio)),
  SD.Burbio = c(sd(filter(all_district,TeachingMethod=="On Premises")$AnyInPerson.Burbio),
                sd(filter(all_district,TeachingMethod=="Hybrid")$AnyInPerson.Burbio),
                sd(filter(all_district,TeachingMethod=="Online Only")$AnyInPerson.Burbio),
                sd(filter(all_district,TeachingMethod=="Other")$AnyInPerson.Burbio)),
  Mean.Phone = c(mean(filter(all_district,TeachingMethod=="On Premises")$AnyInPerson.Phone),
                  mean(filter(all_district,TeachingMethod=="Hybrid")$AnyInPerson.Phone),
                  mean(filter(all_district,TeachingMethod=="Online Only")$AnyInPerson.Phone,na.rm=T),
                  mean(filter(all_district,TeachingMethod=="Other")$AnyInPerson.Phone)),
  SD.Phone = c(sd(filter(all_district,TeachingMethod=="On Premises")$AnyInPerson.Phone),
                sd(filter(all_district,TeachingMethod=="Hybrid")$AnyInPerson.Phone),
                sd(filter(all_district,TeachingMethod=="Online Only")$AnyInPerson.Phone,na.rm=T),
                sd(filter(all_district,TeachingMethod=="Other")$AnyInPerson.Phone))
) %>% round(1) %>% 
  mutate("Burbio Mean(SD)" = paste(Mean.Burbio,paste(SD.Burbio,")",sep=""),sep=" ("),
         "SCDL Mean(SD)" = paste(Mean.Phone,paste(SD.Phone,")",sep=""),sep=" (")) %>%
  select(n,"Burbio Mean(SD)","SCDL Mean(SD)")
rownames(desc_district) <- c("On Premises","Hybrid","Online Only","Other")
write.csv(desc_district,"District Comparisons.csv")

cor.test(all_district$AnyInPerson.Burbio,all_district$AnyInPerson.Phone)




### OTHER COMPARISONS ###
# variation of CMU
summary(all_top_county$num_respondents)

summary(all_top_county$AnyInPerson.FB.unweightsd)
burb0 <- filter(all_top_county,AnyInPerson.Burbio==0)
#summary(burb0$AnyInPerson.FB.unweightsd)
#sd_all <- data.frame(sd = all_top_county$AnyInPerson.FB.unweightsd,Data="All Top Counties")
#sd_0 <- data.frame(sd = burb0$AnyInPerson.FB.unweightsd,Data = "No Onsite According to Burbio")
#varplot <- rbind(sd_all,sd_0)
#ggplot(varplot, aes(sd, colour=Data)) + 
#  geom_density() +
#  xlab("Monthly County Standard Deviation in CTIS") +
#  ylab("Density")
b0 <- burb0 %>%
  select(AnyInPerson.FB.weight,AnyInPerson.Phone) %>%
  rename(CTIS = AnyInPerson.FB.weight,
         SCDL = AnyInPerson.Phone) %>%
  mutate(CTIS = CTIS/100,
         SCDL = SCDL/100) %>%
  pivot_longer(CTIS:SCDL,names_to = "Source",
               values_to = "Percent") %>%
  ggplot(aes(x = Percent, group = Source)) +
  geom_density(aes(linetype=Source)) +
  xlab("Percentage of Any Onsite Schooling when Burbio Reports 0% (N=281)") + ylab("Density") +
  scale_x_continuous(labels = scales::percent, limits=c(0,1), breaks=c(0,.20,.40,.60,.80,1.00)) +
  theme(text=element_text(size=15))
all_top_county %>%
  select(AnyInPerson.FB.weight,AnyInPerson.Phone) %>%
  rename(CTIS = AnyInPerson.FB.weight,
         SCDL = AnyInPerson.Phone) %>%
  mutate(CTIS = CTIS/100,
         SCDL = SCDL/100) %>%
  pivot_longer(CTIS:SCDL,names_to = "Source",
               values_to = "Percent") %>%
  ggplot(aes(x = Percent, group = Source)) +
  geom_density(aes(linetype=Source)) +
  xlab("Percentage of Any Onsite Schooling") + ylab("Density") +
  scale_x_continuous(labels = scales::percent) +
  theme(text=element_text(size=15))

burb100 <- filter(all_top_county,AnyInPerson.Burbio==100)
b100 <- burb100 %>%
  select(AnyInPerson.FB.weight,AnyInPerson.Phone) %>%
  rename(CTIS = AnyInPerson.FB.weight,
         SCDL = AnyInPerson.Phone) %>%
  mutate(CTIS = CTIS/100,
         SCDL = SCDL/100) %>%
  pivot_longer(CTIS:SCDL,names_to = "Source",
               values_to = "Percent") %>%
  ggplot(aes(x = Percent, group = Source)) +
  geom_density(aes(linetype=Source)) +
  xlab("Percentage of Any Onsite Schooling when Burbio Reports 100% (N=1,636)") + ylab("Density") +
  scale_x_continuous(labels = scales::percent, limits=c(0,1), breaks=c(0,.20,.40,.60,.80,1.00)) +
  theme(text=element_text(size=15))
ggarrange(b0,b100)

# county population vs sample size
cor.test(top_county_factors$County.Pop,top_county_factors$num_respondents)

# policies over time
top_50 <- all_top_county[order(-all_top_county$County.Pop),]
top_50 <- top_50[1:120,]

top_50 %>%
  select(fips,month,AnyInPerson.Burbio,AnyInPerson.FB.weight,AnyInPerson.Phone) %>%
  rename(CTIS = AnyInPerson.FB.weight,
         Burbio = AnyInPerson.Burbio,
         SCDL = AnyInPerson.Phone) %>%
  mutate(fips = factor(fips)) %>%
  pivot_longer(cols = c(CTIS,Burbio,SCDL),
               names_to = "Source",values_to = "PercentOnsite") %>%
  filter(Source=="Burbio") %>%
  ggplot(aes(x = factor(month,levels = c("12","1","2","3","4","5")),y=PercentOnsite),group=fips,color=fips) +
  geom_line(aes(group=fips,color=fips)) +
  xlab("Month") + ylab("Percentage of Any Onsite School")

# rural urban
library(readr)
ruralurban <- read_csv("~/Documents/Grad School Year 1/Stuart Lab/School Openings Data/Dropbox Files/Code/Data/ruralurbancodes2013.csv")
ruralurban <- ruralurban %>%
  mutate(Urban = ifelse(RUCC_2013 %in% c(1,2,3),"Metro","Nonmetro"),
         FIPS = as.numeric(FIPS)) %>%
  select(FIPS,Urban,Population_2010) %>%
  rename(fips=FIPS)
urb_percents <- left_join(all_top_county, ruralurban, by="fips")
sum(filter(urb_percents,Urban=="Metro")$num_respondents)/sum(urb_percents$num_respondents)
sum(filter(urb_percents,Urban=="Metro")$County.Pop)/sum(urb_percents$County.Pop)
sum(filter(urb_percents,Urban=="Metro")$Population_2010)/sum(urb_percents$Population_2010)

urb_percents2 <- left_join(all_county,ruralurban,by="fips")
sum(filter(urb_percents2,Urban=="Metro")$num_respondents,na.rm=T)/sum(urb_percents2$num_respondents,na.rm=T)
sum(filter(urb_percents2,Urban=="Metro")$Population_2010,na.rm=T)/sum(urb_percents2$Population_2010,na.rm=T)

# district descriptives
setwd("~/Documents/Grad School Year 1/Stuart Lab/School Openings Data/Paper/Updated Analyses/Data/Combined Data")
mch <- read.csv("MCH 1-31.csv") %>% filter(Control=="Public")
mch$DistrictNCES <- as.character(mch$DistrictNCES)
all_district_desc <- left_join(all_district,mch,by=c("ID" = "DistrictNCES"))
mean(all_district_desc$Enrollment)
sd(all_district_desc$Enrollment)
