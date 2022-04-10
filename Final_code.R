# Load necessary packages
my_packages <- c("stargazer", "haven", "ggplot2","AER","ivmodel","map","dplyr","tibble")
lapply(my_packages, require, character.only = TRUE) 

# Import data
mydata <- read_dta("slave_trade_QJE.dta")
View(mydata)


################################ Visual data analysis #######################################
#Plot relationship between GDP and slave exports
#FIGURE III
ggplot(mydata, aes(x=ln_export_area, y=ln_maddison_pcgdp2000))+
  geom_point(shape=0.5)+
  geom_text(label=mydata$isocode)+
  geom_smooth(method=lm, se=FALSE)+
  ggtitle("Relationship between Log Slave Exports Normalized by Land Area,
ln(exports/area), and Log Real Per Capita GDP in 2000, ln y")+
  xlab("ln (export / area)")+
  ylab("Log Real Per Capita GDP in 2000, ln y")+ theme_gray()+ 
  theme(plot.title = element_text(size=11))

###################### Descriptive Statistics and figures #######################
summary(mydata)

#ln_maddison_pcgdp2000~ln_export_area scatterplot
ggplot(data = mydata, aes(x = ln_export_area, y = ln_maddison_pcgdp2000, colour = country)) + geom_point()+theme_classic()

# with colour representing regions
ggplot(data = mydata, aes(x = ln_maddison_pcgdp2000, fill = country)) + 
  geom_bar(stat = "bin")+theme_classic()+
  xlab("Log GDP") + ggtitle("Bar chart of Log GDP coloured by country")

ggplot(data = mydata, aes(x = ln_export_area, fill = country)) + 
  geom_bar(stat = "bin")+
  xlab("Log slave exports normalised by area") + ggtitle("Bar chart of slave exports coloured by country")+theme_classic()

ggplot(data = mydata, aes(x = country, y = ln_export_area)) + 
  geom_boxplot()+ # creates box plots
  xlab("Log slave exports normalised by area") + 
  ggtitle("Box plots of slave exports in different countries")+theme_classic()

# summary table 
stargazer(as.data.frame(mydata), type = "html", title="Descriptive statistics", digits=2, out="table1.htm")

symbols(data$Longitude, data$Latitude,bg = 'red', fg = 'red', squares =rep(1, length(data$Longitude)), inches=0.03, add=TRUE)

map(database="data$country")

# Summary by group using dplyr
vr<- data %>%                              
  group_by(country) %>% 
  summarize(min = min(x),
            q1 = quantile(x, 0.25),
            median = median(x),
            mean = mean(x),
            q3 = quantile(x, 0.75),
            max = max(x))

###################### OLS Estimates #######################
#Table III

#only colonizer effects
ols_col<- lm(ln_maddison_pcgdp2000~ln_export_area+colony1+colony2+colony3+
            colony4+colony5+colony6+colony7, data=mydata)
#colonizer + geographic effects
ols_col_geo<- lm(ln_maddison_pcgdp2000~ln_export_area+abs_latitude+longitude+
            rain_min+humid_max+low_temp+ln_coastline_area+
            colony1+colony2+colony3+
            colony4+colony5+colony6+colony7, data=mydata)
#removing islands and North countries
removed_countries <- c('Morocco','Algeria','Tunisia','Libya','Egypt','Seychelles',
                         'Mauritius','Comoros','Sao Tome & Principe','Cape Verde Islands')
new_sample<-mydata[!mydata$country %in% removed_countries,]

#colonizer + geographic effects in the new sample
ols_col_geo_restricted<- lm(ln_maddison_pcgdp2000~ln_export_area+abs_latitude+longitude+
            rain_min+humid_max+low_temp+ln_coastline_area+
            colony1+colony2+colony3+
            colony4+colony5+colony6+colony7, data=new_sample)

#island fixed effect + North Africa fixed effect + French legal origin + Islamic
ols_controls<- lm(ln_maddison_pcgdp2000~ln_export_area+abs_latitude+longitude+
            rain_min+humid_max+low_temp+ln_coastline_area+region_n+legor_fr+island_dum+islam+
            colony1+colony2+colony3+
            colony4+colony5+colony6+colony7, data=mydata)

#log of the annual average per capita production of gold, oil, diamonds
ols_production<- lm(ln_maddison_pcgdp2000~ln_export_area+abs_latitude+longitude+
            rain_min+humid_max+low_temp+ln_coastline_area+region_n+legor_fr+island_dum+islam+
            ln_avg_gold_pop+ln_avg_oil_pop+ln_avg_all_diamonds_pop+
            colony1+colony2+colony3+
            colony4+colony5+colony6+colony7, data=mydata)

#all controls with smaller sample
ols_complete<- lm(ln_maddison_pcgdp2000~ln_export_area+abs_latitude+longitude+
            rain_min+humid_max+low_temp+ln_coastline_area+region_n+legor_fr+island_dum+islam+
            ln_avg_gold_pop+ln_avg_oil_pop+ln_avg_all_diamonds_pop+
            colony1+colony2+colony3+
            colony4+colony5+colony6+colony7, data=new_sample)
#All summaries
summary(ols_col)
summary(ols_col_geo)
summary(ols_col_geo_restricted)
summary(ols_controls)
summary(ols_production)
summary(ols_complete)

#Table III
stargazer(ols_col,ols_col_geo,ols_col_geo_restricted,ols_controls,ols_production,ols_complete,
          header = FALSE,
          digits = 3,font.size= "small",style= "aer", float= TRUE, out="1.htm",
          title="RELATIONSHIP BETWEEN SLAVE EXPORTS AND INCOME",
          type = "text",
          dep.var.labels.include = FALSE,
          dep.var.caption =
            "Dependent variable is log real per capita GDP in 2000, ln y",
          omit=c('colony1','colony2','colony3','colony4','colony5','colony6','colony7','Constant'),
          add.lines=list(c('Colonizer fixed effects',"Yes","Yes","Yes","Yes","Yes","Yes")))

#Heteroskedasticity-Consistent standard errors 
stand_errors_1<-coeftest(ols_col, vcov = vcovHC, type = "HC1")
stand_errors_2<-coeftest(ols_col_geo, vcov = vcovHC, type = "HC1")
stand_errors_3<-coeftest(ols_col_geo_restricted, vcov = vcovHC, type = "HC1")
stand_errors_4<-coeftest(ols_controls, vcov = vcovHC, type = "HC1")
stand_errors_5<-coeftest(ols_production, vcov = vcovHC, type = "HC1")
stand_errors_6<-coeftest(ols_complete, vcov = vcovHC, type = "HC1")

#New version of Table III including Heteroskedasticity-Consistent standard errors 
stargazer(stand_errors_1,stand_errors_2,stand_errors_3,stand_errors_4,stand_errors_5,stand_errors_6,
          header = FALSE,
          digits = 3,
          title="Relationship between Slave Exports and Income - corrected for HC std.err.",
          type = "text",
          dep.var.labels.include = FALSE,
          dep.var.caption =
            "Dependent variable is log real per capita GDP in 2000, ln y",
          omit=c('colony1','colony2','colony3','colony4','colony5','colony6','colony7','Constant'),
          add.lines=list(c('Colonizer fixed effects',"Yes","Yes","Yes","Yes","Yes","Yes")))

###################################2° part of the analysis#####################################
#Figure IV
#plot relationship between Population Density and Slave Exports 
ggplot(mydata, aes(x=ln_pop_dens_1400, y=ln_export_area))+
  geom_point(shape=1)+
  geom_text(label=mydata$isocode)+
  geom_smooth(method=lm, se=FALSE)+
  ggtitle("Relationship between Initial Population Density and Slave Exports")+
  xlab("Log population density in 1400")+
  ylab("Slave exports, ln (exports / area)")+ theme_gray()+ 
  theme(plot.title = element_text(size=11))

################################# IV 2SLS ESTIMATES #######################################
#SECOND STAGE

#IV without controls
iv1<-ivreg(ln_maddison_pcgdp2000~ln_export_area|saharan_distance_minimum+
                atlantic_distance_minimum+indian_distance_minimum+red_sea_distance_minimum, data=mydata)

#with colonizer fixed effects
iv2<-ivreg(ln_maddison_pcgdp2000~ln_export_area+colony1+colony2+colony3+
                colony4+colony5+colony6+colony7|colony1+colony2+colony3+
                colony4+colony5+colony6+colony7+saharan_distance_minimum+
                atlantic_distance_minimum+indian_distance_minimum+red_sea_distance_minimum, data=mydata)

# colonizer fixed effects + geographic controls
iv3<-ivreg(ln_maddison_pcgdp2000~ln_export_area+abs_latitude+longitude+
                rain_min+humid_max+low_temp+ln_coastline_area+
                colony1+colony2+colony3+
                colony4+colony5+colony6+colony7|abs_latitude+longitude+
                rain_min+humid_max+low_temp+ln_coastline_area+
                colony1+colony2+colony3+
                colony4+colony5+colony6+colony7+
                saharan_distance_minimum+
                atlantic_distance_minimum+indian_distance_minimum+red_sea_distance_minimum, data=mydata)

# colonizer fixed effects + geographic controls in small sample
iv4<-ivreg(ln_maddison_pcgdp2000~ln_export_area+abs_latitude+longitude+
                rain_min+humid_max+low_temp+ln_coastline_area+
                colony1+colony2+colony3+
                colony4+colony5+colony6+colony7|abs_latitude+longitude+
                rain_min+humid_max+low_temp+ln_coastline_area+
                colony1+colony2+colony3+
                colony4+colony5+colony6+colony7+
                saharan_distance_minimum+
                atlantic_distance_minimum+indian_distance_minimum+red_sea_distance_minimum, data=new_sample)

#FIRST STAGE
s1_1<-lm(ln_export_area~saharan_distance_minimum+
               atlantic_distance_minimum+indian_distance_minimum+red_sea_distance_minimum, data=mydata)

s1_2<-lm(ln_export_area~saharan_distance_minimum+
               atlantic_distance_minimum+indian_distance_minimum+red_sea_distance_minimum+colony1+colony2+colony3+
               colony4+colony5+colony6+colony7, data=mydata)

s1_3<-lm(ln_export_area~saharan_distance_minimum+
               atlantic_distance_minimum+indian_distance_minimum+red_sea_distance_minimum+colony1+colony2+colony3+
               colony4+colony5+colony6+colony7+abs_latitude+longitude+
               rain_min+humid_max+low_temp+ln_coastline_area, data=mydata)

s1_4<-lm(ln_export_area~saharan_distance_minimum+
               atlantic_distance_minimum+indian_distance_minimum+red_sea_distance_minimum+colony1+colony2+colony3+
               colony4+colony5+colony6+colony7+abs_latitude+longitude+
               rain_min+humid_max+low_temp+ln_coastline_area, data=new_sample)

#Diagnostics tests (Weak instruments + Wu-Hausman + Sargan)
summary(iv1, diagnostics=TRUE)
summary(iv2, diagnostics=TRUE)
summary(iv3, diagnostics=TRUE)
summary(iv4, diagnostics=TRUE)

#TABLE IV
#panel 1/2
stargazer(iv1,iv2,iv3,iv4,
          header = FALSE,
          digits = 3,font.size= "small",style= "aer", float= TRUE, out="2.htm",
          omit.table.layout = "n",
          title="ESTIMATES OF THE RELATIONSHIP BETWEEN SLAVE EXPORTS AND INCOME",
          type = "html",
          dep.var.labels.include = FALSE,
          summary=TRUE,
          dep.var.caption =
            "Second Stage. Dependent variable is log income in 2000, ln y",
          omit=c('colony1','colony2','colony3','colony4','colony5','colony6','colony7','Constant','abs_latitude',
                 'longitude','rain_min','humid_max','low_temp','ln_coastline_area'),
          add.lines=list(c('Colonizer fixed effects',"No","Yes","Yes","Yes"),
                         c('Geography controls','No','No','Yes','Yes'),
                         c('Restricted sample','No','No','No','Yes')))
#P-values for panel 2
#Hausman test pvalue
pH_1<-round(summary(iv1, diagnostics = TRUE)$diagnostics[2,4], digits=2)
pH_2<-round(summary(iv2, diagnostics = TRUE)$diagnostics[2,4], digits=2)
pH_3<-round(summary(iv3, diagnostics = TRUE)$diagnostics[2,4], digits=2)
pH_4<-round(summary(iv4, diagnostics = TRUE)$diagnostics[2,4], digits=2)

#Sargan test pvalue
pS_1<-round(summary(iv1, diagnostics = TRUE)$diagnostics[3,4], digits=2)
pS_2<-round(summary(iv2, diagnostics = TRUE)$diagnostics[3,4], digits=2)
pS_3<-round(summary(iv3, diagnostics = TRUE)$diagnostics[3,4], digits=2)
pS_4<-round(summary(iv4, diagnostics = TRUE)$diagnostics[3,4], digits=2)

#panel 2/2
stargazer(s1_1,s1_2,s1_3,s1_4,
          header = FALSE,
          digits = 3,
          type = "html",font.size= "small",style= "aer", float= TRUE, out="3.htm",
          dep.var.labels.include = FALSE,
          summary=TRUE,
          dep.var.caption =
            "First Stage. Dependent variable is slave exports, ln(exports/area)",
          omit=c('colony1','colony2','colony3','colony4','colony5','colony6','colony7','Constant','abs_latitude',
                 'longitude','rain_min','humid_max','low_temp','ln_coastline_area'),
          add.lines=list(c('Colonizer fixed effects',"No","Yes","Yes","Yes"),
                         c('Geography controls','No','No','Yes','Yes'),
                         c('Restricted sample','No','No','No','Yes'),
                         c('Hausman test (pvalue)',pH_1,pH_2,pH_3,pH_4),
                         c('Sargan test (pvalue',pS_1,pS_2,pS_3,pS_4)))

#New version of Table IV including Heteroskedasticity-Consistent standard errors 
model.lst = list(iv1, iv2, iv3, iv4)
model.lst2 = list(s1_1, s1_2, s1_3, s1_4)

#Panel 1/2
stargazer(iv1, iv2, iv3, iv4,
          header = FALSE,
          digits = 3,
          omit.table.layout = "n",
          title="Estimates of the relationship between Slave Exports and Income - corrected for HC std.err.",
          type = "text",
          dep.var.labels.include = FALSE,
          summary=TRUE,
          se=lapply(model.lst, function(x) sqrt(diag(sandwich::vcovHC(x, type = "HC1")))),
          dep.var.caption =
            "Second Stage. Dependent variable is log income in 2000, ln y",
          omit=c('colony1','colony2','colony3','colony4','colony5','colony6','colony7','Constant','abs_latitude',
                 'longitude','rain_min','humid_max','low_temp','ln_coastline_area'),
          add.lines=list(c('Colonizer fixed effects',"No","Yes","Yes","Yes"),
                         c('Geography controls','No','No','Yes','Yes'),
                         c('Restricted sample','No','No','No','Yes')))
#Panel 2/2
stargazer(s1_1, s1_2, s1_3, s1_4,
          header = FALSE,
          digits = 3,
          type = "text",
          dep.var.labels.include = FALSE,
          summary=TRUE,
          se=lapply(model.lst2, function(x) sqrt(diag(sandwich::vcovHC(x, type = "HC1")))),
          dep.var.caption =
            "First Stage. Dependent variable is slave exports, ln(exports/area)",
          omit=c('colony1','colony2','colony3','colony4','colony5','colony6','colony7','Constant','abs_latitude',
                 'longitude','rain_min','humid_max','low_temp','ln_coastline_area'),
          add.lines=list(c('Colonizer fixed effects',"No","Yes","Yes","Yes"),
                         c('Geography controls','No','No','Yes','Yes'),
                         c('Restricted sample','No','No','No','Yes')))

####################### Analysis of new channels affecting economic development #################

#Figure VI
#Ties between villages
ggplot(mydata, aes(x=ln_export_area, y=ethnic_fractionalization))+
  geom_point(shape=1)+
  geom_text(label=mydata$isocode)+
  geom_smooth(method=lm, se=FALSE)+
  ggtitle("Relationship between Slave Exports and Current Ethnic Fractionalization")+
  xlab("ln(exports / area)")+
  ylab("Ethnic fractionalization")+ theme_gray()+
  theme(plot.title = element_text(size=11))

#Figure VII
#Underdevelopment of states
ggplot(mydata, aes(x=ln_export_area, y=state_dev))+
  geom_point(shape=1)+
  geom_text(label=mydata$isocode)+
  geom_smooth(method=lm, se=FALSE)+
  ggtitle("Relationship between Slave Exports and Nineteenth-Century State Development")+
  xlab("ln(exports / area)")+
  ylab("19th century state development")+ theme_gray()+
  theme(plot.title = element_text(size=11))

########################################################################################
#######################Implementation of new methodology################################
N<- 52
K<- 52
set.seed(123)
G<- as.matrix(data$saharan_distance_minimum)

#normal and uniform distribution 
#w_means <- rnorm(K,4,4)
w_means <- runif(K,0,1)
w <- matrix(sapply(w_means, function(saharan_distance_minimum) rnorm(1,saharan_distance_minimum,2)),nrow=K)
Z <- G%*%t(w)
mu <- G%*%t(w_means)
Z_rc <- Z-mu
G2<-as.matrix(data$atlantic_distance_minimum)

#normal and uniform distribution 
#w_means2 <- rnorm(K,4,4)
w_means2 <- runif(K,0,1)
w2 <- matrix(sapply(w_means2, function(atlantic_distance_minimum) rnorm(1,atlantic_distance_minimum,2)),nrow=K)

Z2 <- G2%*%t(w2)
mu2 <- G2%*%t(w_means2)
Z_rc2 <- Z2-mu2
G3<-as.matrix(data$indian_distance_minimum)

#normal and uniform distribution 
#w_means3 <- rnorm(K,4,4)
w_means3 <- runif(K,0,1)
w3 <- matrix(sapply(w_means3, function(indian_distance_minimum) rnorm(1,indian_distance_minimum,2)),nrow=K)

Z3 <- G3%*%t(w3)
mu3 <- G3%*%t(w_means3)
Z_rc3 <- Z3-mu3
G4<-as.matrix(data$red_sea_distance_minimum)

#normal and uniform distribution 
#w_means4 <- rnorm(K,4,4)
w_means4 <- runif(K,0,1)
w4 <- matrix(sapply(w_means4, function(red_sea_distance_minimum) rnorm(1,red_sea_distance_minimum,2)),nrow=K)

Z4 <- G4%*%t(w4)
mu4 <- G4%*%t(w_means4)
Z_rc4 <- Z4-mu4
G5<- as.matrix(new_sample$saharan_distance_minimum)

#normal and uniform distribution 
#w_means5 <- rnorm(K,4,4)
w_means5 <- runif(K,0,1)
w5 <- matrix(sapply(w_means5, function(saharan_distance_minimum) rnorm(1,saharan_distance_minimum,2)),nrow=K)
Z5 <- G5%*%t(w5)
mu5 <- G5%*%t(w_means5)
Z_rc5 <- Z5-mu5
G6<-as.matrix(new_sample$atlantic_distance_minimum)

#normal and uniform distribution 
#w_means6 <- rnorm(K,4,4)
w_means6 <- runif(K,0,1)
w6 <- matrix(sapply(w_means6, function(atlantic_distance_minimum) rnorm(1,atlantic_distance_minimum,2)),nrow=K)

Z6 <- G6%*%t(w6)
mu6 <- G6%*%t(w_means6)
Z_rc6 <- Z6-mu6
G7<-as.matrix(new_sample$indian_distance_minimum)

#normal and uniform distribution 
#w_means7 <- rnorm(K,4,4)
w_means7 <- runif(K,0,1)
w7 <- matrix(sapply(w_means7, function(indian_distance_minimum) rnorm(1,indian_distance_minimum,2)),nrow=K)

Z7 <- G7%*%t(w7)
mu7 <- G7%*%t(w_means7)
Z_rc7 <- Z7-mu7
G8<-as.matrix(new_sample$red_sea_distance_minimum)

#normal and uniform distribution 
#w_means8 <- rnorm(K,4,4)
w_means8 <- runif(K,0,1)
w8 <- matrix(sapply(w_means8, function(red_sea_distance_minimum) rnorm(1,red_sea_distance_minimum,2)),nrow=K)
Z8 <- G8%*%t(w8)
mu8 <- G8%*%t(w_means8)
Z_rc8 <- Z8-mu8


################################# IV 2SLS ESTIMATES #######################################
#SECOND STAGE

#IV without controls
iv1b<-ivreg(ln_maddison_pcgdp2000~ln_export_area|Z_rc+Z_rc2+Z_rc3+Z_rc4, data=mydata)

#with colonizer fixed effects
iv2b<-ivreg(ln_maddison_pcgdp2000~ln_export_area+colony1+colony2+colony3+
              colony4+colony5+colony6+colony7|colony1+colony2+colony3+
              colony4+colony5+colony6+colony7+Z_rc+Z_rc2+Z_rc3+Z_rc4, data=mydata)

# colonizer fixed effects + geographic controls
iv3b<-ivreg(ln_maddison_pcgdp2000~ln_export_area+abs_latitude+longitude+
              rain_min+humid_max+low_temp+ln_coastline_area+
              colony1+colony2+colony3+
              colony4+colony5+colony6+colony7|abs_latitude+longitude+
              rain_min+humid_max+low_temp+ln_coastline_area+
              colony1+colony2+colony3+
              colony4+colony5+colony6+colony7+
              Z_rc+Z_rc2+Z_rc3+Z_rc4, data=mydata)

# colonizer fixed effects + geographic controls in small sample
iv4b<-ivreg(ln_maddison_pcgdp2000~ln_export_area+abs_latitude+longitude+
              rain_min+humid_max+low_temp+ln_coastline_area+
              colony1+colony2+colony3+
              colony4+colony5+colony6+colony7|abs_latitude+longitude+
              rain_min+humid_max+low_temp+ln_coastline_area+
              colony1+colony2+colony3+
              colony4+colony5+colony6+colony7+Z_rc5+Z_rc6+Z_rc7+Z_rc8, data=new_sample)

#FIRST STAGE
s1_1b<-lm(n_export_area~Z_rc+Z_rc2+Z_rc3+Z_rc4, data=mydata)
s1_2b<-lm(ln_export_area~Z_rc+Z_rc2+Z_rc3+Z_rc4+colony1+colony2+colony3+colony4+colony5+colony6+colony7, data=mydata)
s1_3b<-lm(ln_export_area~Z_rc+Z_rc2+Z_rc3+Z_rc4+colony1+colony2+colony3+colony4+colony5+colony6+colony7+abs_latitude+longitude+rain_min+humid_max+low_temp+ln_coastline_area, data=mydata)
s1_4b<-lm(ln_export_area~Z_rc5+Z_rc6+Z_rc7+Z_rc8+colony1+colony2+colony3+colony4+colony5+colony6+colony7+abs_latitude+longitude+rain_min+humid_max+low_temp+ln_coastline_area, data=new_sample)

#Diagnostics tests (Weak instruments + Wu-Hausman + Sargan)
summary(iv1b, diagnostics=TRUE)
summary(iv2b, diagnostics=TRUE)
summary(iv3b, diagnostics=TRUE)
summary(iv4b, diagnostics=TRUE)

#TABLE IV
#panel 1/2
stargazer(iv1b,iv2b,iv3b,iv4b,
          header = FALSE,
          digits = 3,font.size= "small",style= "aer", float= TRUE, out="2.htm",
          omit.table.layout = "n",
          title="ESTIMATES OF THE RELATIONSHIP BETWEEN SLAVE EXPORTS AND INCOME",
          type = "html",
          dep.var.labels.include = FALSE,
          summary=TRUE,
          dep.var.caption =
            "Second Stage. Dependent variable is log income in 2000, ln y",
          omit=c('colony1','colony2','colony3','colony4','colony5','colony6','colony7','Constant','abs_latitude',
                 'longitude','rain_min','humid_max','low_temp','ln_coastline_area'),
          add.lines=list(c('Colonizer fixed effects',"No","Yes","Yes","Yes"),
                         c('Geography controls','No','No','Yes','Yes'),
                         c('Restricted sample','No','No','No','Yes')))
#P-values for panel 2
#Hausman test pvalue
pH_1b<-round(summary(iv1b, diagnostics = TRUE)$diagnostics[2,4], digits=2)
pH_2b<-round(summary(iv2b, diagnostics = TRUE)$diagnostics[2,4], digits=2)
pH_3b<-round(summary(iv3b, diagnostics = TRUE)$diagnostics[2,4], digits=2)
pH_4b<-round(summary(iv4b, diagnostics = TRUE)$diagnostics[2,4], digits=2)

#Sargan test pvalue
pS_1b<-round(summary(iv1, diagnostics = TRUE)$diagnostics[3,4], digits=2)
pS_2b<-round(summary(iv2, diagnostics = TRUE)$diagnostics[3,4], digits=2)
pS_3b<-round(summary(iv3, diagnostics = TRUE)$diagnostics[3,4], digits=2)
pS_4b<-round(summary(iv4, diagnostics = TRUE)$diagnostics[3,4], digits=2)

#panel 2/2
stargazer(s1_1b,s1_2b,s1_3b,s1_4b,
          header = FALSE,
          digits = 3,
          type = "html",font.size= "small",style= "aer", float= TRUE, out="3.htm",
          dep.var.labels.include = FALSE,
          summary=TRUE,
          dep.var.caption =
            "First Stage. Dependent variable is slave exports, ln(exports/area)",
          omit=c('colony1','colony2','colony3','colony4','colony5','colony6','colony7','Constant','abs_latitude',
                 'longitude','rain_min','humid_max','low_temp','ln_coastline_area'),
          add.lines=list(c('Colonizer fixed effects',"No","Yes","Yes","Yes"),
                         c('Geography controls','No','No','Yes','Yes'),
                         c('Restricted sample','No','No','No','Yes'),
                         c('Hausman test (pvalue)',pH_1,pH_2,pH_3,pH_4),
                         c('Sargan test (pvalue',pS_1,pS_2,pS_3,pS_4)))

#New version of Table IV including Heteroskedasticity-Consistent standard errors 
model.lst = list(iv1b, iv2b, iv3b, iv4b)
model.lst2 = list(s1_1b, s1_2b, s1_3b, s1_4b)

#Panel 1/2
stargazer(iv1b, iv2b, iv3b, iv4b,
          header = FALSE,
          digits = 3,
          omit.table.layout = "n",
          title="Estimates of the relationship between Slave Exports and Income - corrected for HC std.err.",
          type = "text",
          dep.var.labels.include = FALSE,
          summary=TRUE,
          se=lapply(model.lst, function(x) sqrt(diag(sandwich::vcovHC(x, type = "HC1")))),
          dep.var.caption =
            "Second Stage. Dependent variable is log income in 2000, ln y",
          omit=c('colony1','colony2','colony3','colony4','colony5','colony6','colony7','Constant','abs_latitude',
                 'longitude','rain_min','humid_max','low_temp','ln_coastline_area'),
          add.lines=list(c('Colonizer fixed effects',"No","Yes","Yes","Yes"),
                         c('Geography controls','No','No','Yes','Yes'),
                         c('Restricted sample','No','No','No','Yes')))
#Panel 2/2
stargazer(s1_1b, s1_2b, s1_3b, s1_4b,
          header = FALSE,
          digits = 3,
          type = "text",
          dep.var.labels.include = FALSE,
          summary=TRUE,
          se=lapply(model.lst2, function(x) sqrt(diag(sandwich::vcovHC(x, type = "HC1")))),
          dep.var.caption =
            "First Stage. Dependent variable is slave exports, ln(exports/area)",
          omit=c('colony1','colony2','colony3','colony4','colony5','colony6','colony7','Constant','abs_latitude',
                 'longitude','rain_min','humid_max','low_temp','ln_coastline_area'),
          add.lines=list(c('Colonizer fixed effects',"No","Yes","Yes","Yes"),
                         c('Geography controls','No','No','Yes','Yes'),
                         c('Restricted sample','No','No','No','Yes')))

######### End of the code







