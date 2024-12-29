#This file analyzes the collective action of MMC.
#We are especially interested in the impact of directors network variables

#The analyses are based on the files created through stata and the network variables I created

rm(list=ls()) 

library(tidyverse)
library(dplyr)
library(Rtsne)
library(rsvd)
library(geometry)
library(igraph)
library(readstata13)

#uploading files

Firms.2006.2013 <- read.dta13("C:/Users/erezi/OneDrive - Tel-Aviv University/MMC and ACR/Rami/Asset4TCC_after_analysis2.dta",nonint.factors = TRUE)

Network.vars.2006 <- read.csv("C:/Users/erezi/OneDrive - Tel-Aviv University/MMC and ACR/data and analysis/Firms.net.vars.2006.csv", header=T)
Network.vars.2010 <- read.csv("C:/Users/erezi/OneDrive - Tel-Aviv University/MMC and ACR/data and analysis/Firms.net.vars.2010.csv", header=T)
Network.vars.2013 <- read.csv("C:/Users/erezi/OneDrive - Tel-Aviv University/MMC and ACR/data and analysis/Firms.net.vars.2013.csv", header=T)

#if working from office
Firms.2006.2013 <- read.dta13("D:/OneDrive - Tel-Aviv University/Rami/Asset4TCC_after_analysis2.dta",nonint.factors = TRUE)


Network.vars.2006 <- read.csv("D:/OneDrive - Tel-Aviv University/MMC and ACR/data and analysis/Firms.net.vars.2006.csv", header=T)
Network.vars.2010 <- read.csv("D:/OneDrive - Tel-Aviv University/MMC and ACR/data and analysis/Firms.net.vars.2010.csv", header=T)
Network.vars.2013 <- read.csv("D:/OneDrive - Tel-Aviv University/MMC and ACR/data and analysis/Firms.net.vars.2013.csv", header=T)


#merging network variables
#IMPORTANT - To merge network data for 2006 I use tcc.id, and for 2010 I use a4id. 
#THERE ARE 80 FIRMS from 2013 and 2006 THAT DONT HAVE TCC.ID NEED TO SEE WHAT TO DO WITH THEM


Firms.2006.2013 <- merge(Firms.2006.2013,Network.vars.2006,by.x = c("tcc_id","year"),
                         by.y = c("ID","year"),all.x = TRUE)

Firms.2006.2013 <- merge(Firms.2006.2013,Network.vars.2010,by.x = c("a4id","year"),
                         by.y = c("Asset4.ID","year"),all.x = TRUE)

Firms.2006.2013 <- merge(Firms.2006.2013,Network.vars.2013,by.x = c("tcc_id","year"),
                         by.y = c("ID","year"),all.x = TRUE)

Firms.2006.2013 <- Firms.2006.2013[order(Firms.2006.2013$a4id,Firms.2006.2013$year),]
#deleting years with no data (leaving only 2006,2010,2013)

Firms.anlys <- Firms.2006.2013[Firms.2006.2013$year==2006|Firms.2006.2013$year==2010|Firms.2006.2013$year==2013,]


#creating network vars
Firms.anlys <- Firms.anlys[order(Firms.anlys$a4id,Firms.anlys$year),]

Firms.anlys$nation.degree.new <- NA
Firms.anlys$nation.weight.degree.new <- NA
Firms.anlys$nation.betweeness.new <- NA
Firms.anlys$nation.desity.new <- NA
Firms.anlys$inter.nation.degree.new <- NA
Firms.anlys$inter.nation.weight.degree.new <- NA
Firms.anlys$inter.nation.betweeness.new <- NA
Firms.anlys$complete.degree.new <- NA
Firms.anlys$complete.weight.degree.new <- NA
Firms.anlys$complete.betweeness.new <- NA


for (i in 1:nrow(Firms.anlys)){
  
  if(Firms.anlys$year[i]==2006){
    
    Firms.anlys$nation.degree.new[i] <- Firms.anlys$nation.degree.x[i] 
    Firms.anlys$nation.weight.degree.new[i] <- Firms.anlys$nation.weight.degree.x[i]
    Firms.anlys$nation.betweeness.new[i] <- Firms.anlys$nation.betweeness.x[i]
    Firms.anlys$nation.desity.new[i] <- Firms.anlys$nation.desity.x[i]
    Firms.anlys$inter.nation.degree.new[i] <- Firms.anlys$inter.nation.degree.x[i]
    Firms.anlys$inter.nation.weight.degree.new[i] <- Firms.anlys$inter.nation.weight.degree.x[i]
    Firms.anlys$inter.nation.betweeness.new[i] <- Firms.anlys$inter.nation.betweeness.x[i]
    Firms.anlys$complete.degree.new[i] <- Firms.anlys$complete.degree.x[i]
    Firms.anlys$complete.weight.degree.new[i] <- Firms.anlys$complete.weight.degree.x[i]
    Firms.anlys$complete.betweeness.new[i] <- Firms.anlys$complete.betweeness.x[i]
    
    
  }
  
  else if (Firms.anlys$year[i]==2010){
    
    Firms.anlys$nation.degree.new[i] <- Firms.anlys$nation.degree.y[i] 
    Firms.anlys$nation.weight.degree.new[i] <- Firms.anlys$nation.weight.degree.y[i]
    Firms.anlys$nation.betweeness.new[i] <- Firms.anlys$nation.betweeness.y[i]
    Firms.anlys$nation.desity.new[i] <- Firms.anlys$nation.desity.y[i]
    Firms.anlys$inter.nation.degree.new[i] <- Firms.anlys$inter.nation.degree.y[i]
    Firms.anlys$inter.nation.weight.degree.new[i] <- Firms.anlys$inter.nation.weight.degree.y[i]
    Firms.anlys$inter.nation.betweeness.new[i] <- Firms.anlys$inter.nation.betweeness.y[i]
    Firms.anlys$complete.degree.new[i] <- Firms.anlys$complete.degree.y[i]
    Firms.anlys$complete.weight.degree.new[i] <- Firms.anlys$complete.weight.degree.y[i]
    Firms.anlys$complete.betweeness.new[i] <- Firms.anlys$complete.betweeness.y[i]
    
    
  }
  
  else{
    Firms.anlys$nation.degree.new[i] <- Firms.anlys$nation.degree[i] 
    Firms.anlys$nation.weight.degree.new[i] <- Firms.anlys$nation.weight.degree[i]
    Firms.anlys$nation.betweeness.new[i] <- Firms.anlys$nation.betweeness[i]
    Firms.anlys$nation.desity.new[i] <- Firms.anlys$nation.desity[i]
    Firms.anlys$inter.nation.degree.new[i] <- Firms.anlys$inter.nation.degree[i]
    Firms.anlys$inter.nation.weight.degree.new[i] <- Firms.anlys$inter.nation.weight.degree[i]
    Firms.anlys$inter.nation.betweeness.new[i] <- Firms.anlys$inter.nation.betweeness[i]
    Firms.anlys$complete.degree.new[i] <- Firms.anlys$complete.degree[i]
    Firms.anlys$complete.weight.degree.new[i] <- Firms.anlys$complete.weight.degree[i]
    Firms.anlys$complete.betweeness.new[i] <- Firms.anlys$complete.betweeness[i]
    
  }
}

#counting how many firms NEVER had interlocks

never.contect <- Firms.anlys %>% group_by(a4id) %>% summarise(mean.complete.degree = mean(complete.degree.new))

hist(Firms.anlys$nation.degree.new)
hist(Firms.anlys$inter.nation.degree.new)

cor(Firms.anlys$nation.degree.new, Firms.anlys$inter.nation.degree.new,use = "complete.obs")


Firms.new <- Firms.anlys[,c("nation.degree.new","inter.nation.degree.new")]

Firms.new  <- Firms.new %>% drop_na()

cor(Firms.new$nation.degree.new, Firms.new$inter.nation.degree.new, method = c("pearson"))

plot(Firms.new$nation.degree.new, Firms.new$inter.nation.degree.new, 
     xlab="NUmber of Intra-national Interlocks", ylab="Number of Transnational Interlocks", pch=19)
lines(lowess(Firms.new$nation.degree.new, Firms.new$inter.nation.degree.new,), col="grey") # lowess line (x,y)


#merging another dependent variable

library("foreign")

library(readstata13)
Asset4 <- read.dta13("C:/Users/erezi/OneDrive - Tel-Aviv University/Rami/Asset4TCC.dta")

Asset4 <- read.dta13("D:/OneDrive - Tel-Aviv University/Rami/Asset4TCC.dta")

Asset4.new <- Asset4[,c("a4id","year","OECDGuidelinesforMultinationalEn")]

Firms.anlys <- merge(Firms.anlys,Asset4.new,by = c("a4id","year"),all.x = TRUE)

#############################################################################
####Descriptive

#Changes in outcomes through time

outcomes.years <- Firms.anlys %>% group_by(year) %>% summarise(OECDguide = sum(OECDGuidelinesforMultinationalEn,na.rm = T)/n(),
                                                               gri = sum(grireport,na.rm = T)/n(),
                                                               global.compact = sum(globalcmpt,na.rm = T)/n(),
                                                               iso14 = sum(iso14,na.rm = T)/n(),
                                                               )


colnames(outcomes.years) <- c("year","OECD Guidelines","GRI","UN Global Compact","ISO1400")

#outcomes.years$`OECD Guidelines` <- outcomes.years$`OECD Guidelines`*100 
#outcomes.years$GRI <- outcomes.years$GRI*100
#outcomes.years$`UN Global Compact` <- outcomes.years$`UN Global Compact`*100
#outcomes.years$ISO1400 <- outcomes.years$ISO1400*100


Outcomes.long <- outcomes.years %>%
                  select('year', 'OECD Guidelines', 'GRI','UN Global Compact', 'ISO1400') %>%
                          gather(key = "Policy", value = "value", -year)

Outcomes.long$value <- Outcomes.long$value*100
Outcomes.long$Standard <- factor(Outcomes.long$Policy, levels = c('GRI', 'ISO1400', 'UN Global Compact', 'OECD Guidelines'))

pdf(file = "C:/Users/erezi/OneDrive - Tel-Aviv University/MMC and ACR/data and analysis/Standard.adoption.pdf", width = 7, height = 7)

ggplot(Outcomes.long, aes(x=year,y=value,group=Standard)) +
  geom_line(aes(linetype=Standard,color=Standard),linewidth = 2) +
  geom_point(aes(shape=Standard,color=Standard), size=4) +
  #scale_color_grey() +
  scale_color_manual(values = c("gray70","black","gray50","gray80"))+
  theme_classic() +
  scale_x_continuous("Year",breaks = c(2006,2010,2013)) +
  scale_y_continuous("Adoption Rate",limits = c(1,80))
  
dev.off()
  
  
####distribution of key network variables

hist(Firms.anlys$nation.degree.new)
hist(Firms.anlys$inter.nation.degree.new)

Networks.long <- Firms.anlys %>%
  select('a4id','year','nation.degree.new','inter.nation.degree.new') %>%
  gather(key = "Degree Centrality", value = "value", -c("a4id","year"))

Networks.freq <- Networks.long %>% group_by(`Degree Centrality`,value) %>%
              summarise(Frequency = n())

colnames(Networks.freq) <- c("Type of Interlock","Number of Interlocks","Number of Firms")

Networks.freq$`Type of Interlock`[Networks.freq$`Type of Interlock`=="inter.nation.degree.new"] <- "Transnational"
Networks.freq$`Type of Interlock`[Networks.freq$`Type of Interlock`=="nation.degree.new"] <- "Glocal"

pdf(file = "C:/Users/erezi/OneDrive - Tel-Aviv University/MMC and ACR/data and analysis/Net.vars.distribution.pdf", width = 7, height = 7)

ggplot(Networks.freq, aes(x=`Number of Interlocks`,y=`Number of Firms`,group=`Type of Interlock`)) +
  geom_line(aes(linetype=`Type of Interlock`,color=`Type of Interlock`),linewidth = 1.5) +
  #geom_point(aes(shape=Policy,color=Policy), size=3) +
  #scale_color_grey() +
  scale_color_manual(values = c("black","gray50"))+
  theme_classic() +
  scale_x_continuous("Number of Interlocks",breaks = c(0,2,4,6,8,10,12)) +
  scale_y_continuous("Number of Firms",limits = c(0,270),
                     breaks = c(0,50,100,150,200,250))

dev.off()


ggplot(Firms.anlys,aes(nation.degree.new))+
  geom_freqpoly(binwidth = 1,linewidth=2) +
  scale_x_continuous("National degree centrality",limits = c(0,12),
                     breaks = c(0,2,4,6,8,10,12)) +
  scale_y_continuous("Frequency",limits = c(0,270),
                     breaks = c(0,50,100,150,200,250)) +
  theme_classic()


ggplot(Firms.anlys,aes(inter.nation.degree.new))+
  geom_freqpoly(binwidth = 1,linewidth=2) +
  scale_x_continuous("Transnational degree centrality",limits = c(0,12),
                     breaks = c(0,2,4,6,8,10,12)) +
  scale_y_continuous("Frequency",limits = c(0,270),
                     breaks = c(0,50,100,150,200,250)) +
  theme_classic()


Firms.new$nation.degree.new, Firms.new$inter.nation.degree.new
############################################################################################################################
#######################################MODELS WITH THESE NETWORK VARS########################################################

library(MatchIt)
library(lmtest)
library(sandwich)
library(aod)
library(ggplot2)
library(miceadds)
library(sandwich)

Firms.anlys.new <- Firms.anlys[,c("grireport","wbcsd","tc","icc","envcont_d","logungc","logiorgs","logrevenue","roa",
        "ind10_edt","region","year","a4id","globalcmpt","iso14","evscore","complete.degree.new",
      "nation.degree.new","inter.nation.degree.new", "OECDGuidelinesforMultinationalEn")]

Clean.data  <- Firms.anlys.new %>% drop_na()

##############################################################################

match_obj <- matchit(OECDGuidelinesforMultinationalEn ~ wbcsd + tc + icc +  
                       envcont_d + logungc + logiorgs + logrevenue + roa +
                       factor(ind10_edt) + factor(region),
                     data = Clean.data,method = "subclass", distance ="glm",
                     ratio = 1,
                     replace = FALSE)
summary(match_obj)  

#plotting the balance 
plot(match_obj, type = "jitter", interactive = FALSE)
plot(summary(match_obj), abs = FALSE)

matched_data <- match.data(match_obj)


oecd.eq.compl <- OECDGuidelinesforMultinationalEn ~ nation.degree.new + inter.nation.degree.new + 
  wbcsd + tc + icc +  
  envcont_d + logungc + logiorgs + logrevenue + roa +
  factor(ind10_edt) + factor(region) + factor(year) + 
  factor(a4id)



oecd.mod.compl <- glm.cluster(formula = oecd.eq.compl,
                                   cluster= "subclass", 
                                   data = matched_data,
                                   family = "gaussian",
                                   weights = matched_data$weights)

summary(oecd.mod.compl)




###############################################################################
match_obj <- matchit(grireport ~ wbcsd + tc + icc +  
                       envcont_d + logungc + logiorgs + logrevenue + roa +
                       factor(ind10_edt) + factor(region),
                     data = Clean.data,method = "subclass", distance ="glm",
                     ratio = 1,
                     replace = FALSE)
summary(match_obj)  

#plotting the balance 
plot(match_obj, type = "jitter", interactive = FALSE)
plot(summary(match_obj), abs = FALSE)

matched_data <- match.data(match_obj)




grireport.eq.compl <- grireport ~ nation.degree.new + inter.nation.degree.new + 
                        wbcsd + tc + icc +  
                        envcont_d + logungc + logiorgs + logrevenue + roa +
                        factor(ind10_edt) + factor(region) + factor(year) + 
                        factor(a4id)
            
                        

grireport.mod.compl <- glm.cluster(formula = grireport.eq.compl,
                                   cluster= "subclass", 
                                   data = matched_data,
                                   family = "gaussian",
                                   weights = matched_data$weights)

summary(grireport.mod.compl)

#######################

match_obj <- matchit(globalcmpt ~ wbcsd + tc + icc +  
                       envcont_d + logungc + logiorgs + logrevenue + roa +
                       factor(ind10_edt) + factor(region) + factor(year),
                     data = Clean.data,method = "subclass", distance ="glm",
                     ratio = 1,
                     replace = FALSE)
summary(match_obj)  

#plotting the balance 
plot(match_obj, type = "jitter", interactive = FALSE)
plot(summary(match_obj), abs = FALSE)

matched_data <- match.data(match_obj)

globalcmpt.eq.decomposition <- globalcmpt ~ nation.degree.new + inter.nation.degree.new + 
                               wbcsd + tc + icc +  
                               envcont_d + logungc + logiorgs + logrevenue + roa +
                               factor(ind10_edt) + factor(region) + factor(year) + 
                               factor(a4id)

globalcmpt.mod.decomposition <- glm.cluster(formula = globalcmpt.eq.decomposition,
                                            cluster= "subclass", 
                                            data = matched_data,
                                            family = "gaussian",
                                            weights = matched_data$weights)

summary(globalcmpt.mod.decomposition)

############################################

match_obj <- matchit(iso14 ~ wbcsd + tc + icc +  
                       envcont_d + logungc + logiorgs + logrevenue + roa +
                       factor(ind10_edt) + factor(region) + factor(year),
                     data = Clean.data,method = "full", distance ="glm",
                     ratio = 1,
                     replace = FALSE)
summary(match_obj)  

#plotting the balance 
plot(match_obj, type = "jitter", interactive = FALSE)
plot(summary(match_obj), abs = FALSE)

matched_data <- match.data(match_obj)

iso14.eq.decompose <- iso14 ~ nation.degree.new + inter.nation.degree.new + 
                              wbcsd + tc + icc +  
                              envcont_d + logungc + logiorgs + logrevenue + roa +
                              factor(ind10_edt) + factor(region) + factor(year) +
                              factor(a4id)

iso14.mod.decompose <- glm.cluster(formula = iso14.eq.decompose,
                                   cluster= "subclass", 
                                   data = matched_data,
                                   family = "gaussian",
                                   weights = matched_data$weights)

summary(iso14.mod.decompose)


#############################################################################################################################
library(pglm)

oecd.eq.decompose <- OECDGuidelinesforMultinationalEn ~ nation.degree.new + inter.nation.degree.new + 
  wbcsd + tc + icc +  
  envcont_d + logungc + logiorgs + logrevenue + roa +
  factor(ind10_edt) + factor(region) 
  

oecd.mod.decompose <- pglm(oecd.eq.decompose,Firms.anlys, effect = c("individual"), useNA = "ifany", 
                                family =  binomial('logit'),index = c("a4id","year"),
                                model = "random",  method = "bfgs", print.level = 3, R = 5)

summary(oecd.mod.decompose)

#############


grireport.eq.compl <- grireport ~ complete.degree.new + wbcsd + tc + icc +  
  envcont_d + logungc + logiorgs + logrevenue + roa +
  factor(ind10_edt) + factor(region)
 

grireport.mod.compl <- pglm(grireport.eq.compl,Firms.anlys, effect = c("individual"), useNA = "ifany", 
                            family = binomial('logit'),index = c("a4id","year"),
                            model = "random",  method = "bfgs", print.level = 3, R = 5)


#coef(grireport.mod.compl)
#vcov(grireport.mod.compl)
summary(grireport.mod.compl)


grireport.eq.decompose <- grireport ~ nation.degree.new + inter.nation.degree.new +
                                      wbcsd + tc + icc +  
                                      envcont_d + logungc + logiorgs + logrevenue + roa +
                                      factor(ind10_edt) + factor(country) + factor(year)
                                      

grireport.mod.decompose <- pglm(grireport.eq.decompose,Firms.anlys, effect = c("individual"), useNA = "ifany", 
                                family =  binomial('logit'),index = c("a4id","year"),
                                model = "random",  method = "bfgs", print.level = 3, R = 5)

summary(grireport.mod.decompose)


####################

globalcmpt.eq.compl <- globalcmpt ~ complete.degree.new + wbcsd + tc + icc +  
                                    envcont_d + logungc + logiorgs + logrevenue + roa +
                                    factor(ind10_edt) + factor(region)
  

globalcmpt.mod.compl <- pglm(globalcmpt.eq.compl,Firms.anlys, effect = c("individual"), useNA = "ifany", 
                             family = binomial('probit'),index = c("a4id","year"),
                             model = "random",  method = "bfgs", print.level = 3, R = 5)

summary(globalcmpt.mod.compl)

globalcmpt.eq.decomposition <- globalcmpt ~ nation.degree.new + inter.nation.degree.new + 
                                            wbcsd + tc + icc +  
                                            envcont_d + logungc + logiorgs + logrevenue + roa +
                                            factor(ind10_edt) + factor(region) + factor(year) 
                                            

globalcmpt.mod.decomposition <- pglm(globalcmpt.eq.decomposition,Firms.anlys, effect = c("individual"), useNA = "ifany", 
                                     family = binomial('logit'),index = c("a4id","year"),
                                     model = "random", method = "bfgs", print.level = 3, R = 5)

summary(globalcmpt.mod.decomposition)


################

iso14.eq.compl <- iso14 ~ complete.degree.new + wbcsd + tc + icc +  
                          envcont_d + logungc + logiorgs + logrevenue + roa +
                          factor(ind10_edt) + factor(year)

iso14.mod.compl <- pglm(iso14.eq.compl,Firms.anlys, effect = c("individual"), useNA = "ifany", 
                        family = binomial('probit'),index = c("a4id","year"),
                        model = "random",  method = "bfgs", print.level = 3, R = 5)

summary(iso14.mod.compl)

iso14.eq.decompose <- iso14 ~ nation.degree.new + inter.nation.degree.new + 
                              wbcsd + tc + icc +  
                              envcont_d + logungc + logiorgs + logrevenue + roa +
                              factor(ind10_edt) + factor(year)
                                

iso14.mod.decompose <- pglm(iso14.eq.decompose,Firms.anlys, effect = c("individual"), useNA = "ifany", 
                            family = binomial('logit'),index = c("a4id","year"),
                            model = "random",  method = "bfgs", print.level = 3, R = 5)

summary(iso14.mod.decompose)

###################

library(plm)
library(coeftest) 
library(sandwich)
library(clubSandwich)


evscore.eq.compl <- evscore ~ complete.degree.new + wbcsd + tc + icc +  
  envcont_d + logungc + logiorgs + logrevenue + roa +
  factor(ind10_edt) + factor(region) + factor(year)                

evscore.mod.compl <- plm(evscore.eq.compl, 
                         Firms.anlys, 
                         index = c("a4id","year"), 
                         model = "random", 
                         random.models = c("within", "between"))

summary(evscore.mod.compl)


evscore.eq.decompose <- evscore ~ nation.degree.new + inter.nation.degree.new + 
                          wbcsd + tc + icc +  
                          envcont_d + logungc + logiorgs + logrevenue + roa +
                          factor(ind10_edt) + factor(region) + factor(year)


evscore.mod.decompose <- plm(evscore.eq.decompose, 
                             Firms.anlys, 
                             index = c("a4id","year"), 
                             model = "random", 
                             random.models = c("within", "between"))


summary(evscore.mod.decompose)
















grireport.eq.compl <- grireport ~ complete.degree.new + wbcsd + tc + icc +  
  envcont_d + logungc + logiorgs + logrevenue + roa +
  factor(ind10_edt) + factor(region) + factor(year)



grireport.mod.compl <- plm(grireport.eq.compl, 
                           Firms.anlys, 
                           index = "a4id", 
                           model = "within")


grireport.eq.decompose <- grireport ~ nation.degree.new + inter.nation.degree.new +
  wbcsd + tc + icc +  
  envcont_d + logungc + logiorgs + logrevenue + roa +
  factor(ind10_edt) + factor(region)


grireport.mod.decompose <- plm(grireport.eq.decompose, 
                               Firms.anlys, 
                               index = c("a4id","year"), 
                               model = "within")

#model = "random", 
#random.models = c("within", "between"))

summary(grireport.mod.decompose)


###############################################################################################

library(aod)
library(ggplot2)
library(miceadds)
library(sandwich)

oecd.eq.decompose <- OECDGuidelinesforMultinationalEn ~ nation.degree.new + inter.nation.degree.new +
  wbcsd + tc + icc +  
  envcont_d + logungc + logiorgs + logrevenue + roa +
  factor(ind10_edt) + factor(region) + factor(year) +
  factor(a4id)



oecd.mod.decompose <- glm.cluster(formula = oecd.eq.decompose,
                                       cluster= "a4id", 
                                       data = Firms.anlys,
                                       family = "binomial")

summary(oecd.mod.decompose)


#

grireport.eq.compl <- grireport ~ complete.degree.new + wbcsd + tc + icc +  
                              envcont_d + logungc + logiorgs + logrevenue + roa +
                              factor(ind10_edt) + factor(region) + factor(year) +
                              factor(a4id)

grireport.mod.compl <- glm.cluster(formula = grireport.eq.compl,
                                   cluster= "a4id", 
                                   data = Firms.anlys,
                                   family = "gaussian")

#coef(grireport.mod.compl)
#vcov(grireport.mod.compl)
summary(grireport.mod.compl)


grireport.eq.decompose <- grireport ~ nation.degree.new + inter.nation.degree.new +
                                      wbcsd + tc + icc +  
                                      envcont_d + logungc + logiorgs + logrevenue + roa +
                                      factor(ind10_edt) + factor(region) + factor(year) +
                                      factor(a4id)

grireport.mod.decompose <- glm.cluster(formula = grireport.eq.decompose,
                                       cluster= "a4id", 
                                       data = Firms.anlys,
                                       family = "binomial")

summary(grireport.mod.decompose)

######

globalcmpt.eq.compl <- globalcmpt ~ complete.degree.new + wbcsd + tc + icc +  
                                  envcont_d + logungc + logiorgs + logrevenue + roa +
                                  factor(ind10_edt) + factor(region) + factor(year) +
                                  factor(a4id)

globalcmpt.mod.compl <- glm.cluster(formula = globalcmpt.eq.compl,
                                    cluster= "a4id", 
                                    data = Firms.anlys,
                                    family = "gaussian")

summary(globalcmpt.mod.compl)

globalcmpt.eq.decomposition <- globalcmpt ~ nation.degree.new + inter.nation.degree.new + 
                                            wbcsd + tc + icc +  
                                            envcont_d + logungc + logiorgs + logrevenue + roa +
                                            factor(ind10_edt) + factor(region) + factor(year) + 
                                            factor(a4id)

globalcmpt.mod.decomposition <- glm.cluster(formula = globalcmpt.eq.decomposition,
                                            cluster= "a4id", 
                                            data = Firms.anlys,
                                            family = "gaussian")

summary(globalcmpt.mod.decomposition)


######

iso14.eq.compl <- iso14 ~ complete.degree.new + wbcsd + tc + icc +  
                          envcont_d + logungc + logiorgs + logrevenue + roa +
                          factor(ind10_edt) + factor(region) + factor(year) +
                          factor(a4id)

iso14.mod.compl <- glm.cluster(formula = iso14.eq.compl,
                               cluster= "a4id", 
                               data = Firms.anlys,
                               family = "gaussian")

summary(iso14.mod.compl)

iso14.eq.decompose <- iso14 ~ nation.degree.new + inter.nation.degree.new + 
                              wbcsd + tc + icc +  
                              envcont_d + logungc + logiorgs + logrevenue + roa +
                              factor(ind10_edt) + factor(region) + factor(year) +
                              factor(a4id)

iso14.mod.decompose <- glm.cluster(formula = iso14.eq.decompose,
                                   cluster= "a4id", 
                                   data = Firms.anlys,
                                   family = "gaussian")

summary(iso14.mod.decompose)

###########


evscore.eq.compl <- evscore ~ complete.degree.new + wbcsd + tc + icc +  
                              envcont_d + logungc + logiorgs + logrevenue + roa +
                              factor(ind10_edt) + factor(region) + factor(year)                

evscore.mod.compl <- plm(evscore.eq.compl, 
                         Firms.anlys, 
                         index = "a4id", 
                         model = "within")

summary(evscore.mod.compl)


evscore.eq.decompose <- evscore ~ nation.degree.new + inter.nation.degree.new + 
                                  wbcsd + tc + icc +  
                                  envcont_d + logungc + logiorgs + logrevenue + roa +
                                  factor(ind10_edt) + factor(region) 


evscore.mod.decompose <- plm(evscore.eq.decompose, 
                               Firms.anlys, 
                               index = c("a4id","year"), 
                               model = "random", 
                               random.models = c("within", "between"))


summary(evscore.mod.decompose)



grireport.eq.decompose <- grireport ~ nation.degree.new + inter.nation.degree.new +
  wbcsd + tc + icc +  
  envcont_d + logungc + logiorgs + logrevenue + roa +
  factor(ind10_edt) + factor(region)


grireport.mod.decompose <- plm(grireport.eq.decompose, 
                               Firms.anlys, 
                               index = c("a4id","year"), 
                               model = "within")

#model = "random", 
#random.models = c("within", "between"))

summary(grireport.mod.decompose)



############################################################################################################################
#creating network lagged network vars
Firms.anlys <- Firms.anlys[order(Firms.anlys$a4id,Firms.anlys$year.x),]

colnames(Firms.anlys[,-10:-1])

Firms.anlys$nation.degree.lag <- NA
Firms.anlys$nation.weight.degree.lag <- NA
Firms.anlys$nation.betweeness.lag <- NA
Firms.anlys$nation.desity.lag <- NA
Firms.anlys$nation.desity <- NA
Firms.anlys$inter.nation.degree.lag <- NA
Firms.anlys$inter.nation.weight.degree.lag <- NA
Firms.anlys$inter.nation.betweeness.lag <- NA
Firms.anlys$complete.degree.lag <- NA
Firms.anlys$complete.weight.degree.lag <- NA
Firms.anlys$complete.betweeness.lag <- NA


for (i in 2:nrow(Firms.anlys)){
  
  if(Firms.anlys$year.x[i]==2010 & Firms.anlys$year.x[i-1]==2006){
  
    Firms.anlys$nation.degree.lag[i] <- Firms.anlys$nation.degree.x[i-1] 
    Firms.anlys$nation.weight.degree.lag[i] <- Firms.anlys$nation.weight.degree.x[i-1]
    Firms.anlys$nation.betweeness.lag[i] <- Firms.anlys$nation.betweeness.x[i-1]
    Firms.anlys$nation.desity.lag[i] <- Firms.anlys$nation.desity.x[i-1]
    Firms.anlys$nation.desity[i] <- Firms.anlys$nation.desity.y[i]
    Firms.anlys$inter.nation.degree.lag[i] <- Firms.anlys$inter.nation.degree.x[i-1]
    Firms.anlys$inter.nation.weight.degree.lag[i] <- Firms.anlys$inter.nation.weight.degree.x[i-1]
    Firms.anlys$inter.nation.betweeness.lag[i] <- Firms.anlys$inter.nation.betweeness.x[i-1]
    Firms.anlys$complete.degree.lag[i] <- Firms.anlys$complete.degree.x[i-1]
    Firms.anlys$complete.weight.degree.lag[i] <- Firms.anlys$complete.weight.degree.x[i-1]
    Firms.anlys$complete.betweeness.lag[i] <- Firms.anlys$complete.betweeness.x[i-1]
    
    
  }
  
  else if (Firms.anlys$year.x[i]==2013 & Firms.anlys$year.x[i-1]==2010){
    
    Firms.anlys$nation.degree.lag[i] <- Firms.anlys$nation.degree.y[i-1] 
    Firms.anlys$nation.weight.degree.lag[i] <- Firms.anlys$nation.weight.degree.y[i-1]
    Firms.anlys$nation.betweeness.lag[i] <- Firms.anlys$nation.betweeness.y[i-1]
    Firms.anlys$nation.desity.lag[i] <- Firms.anlys$nation.desity.y[i-1]
    Firms.anlys$nation.desity[i] <- NA
    Firms.anlys$inter.nation.degree.lag[i] <- Firms.anlys$inter.nation.degree.y[i-1]
    Firms.anlys$inter.nation.weight.degree.lag[i] <- Firms.anlys$inter.nation.weight.degree.y[i-1]
    Firms.anlys$inter.nation.betweeness.lag[i] <- Firms.anlys$inter.nation.betweeness.y[i-1]
    Firms.anlys$complete.degree.lag[i] <- Firms.anlys$complete.degree.y[i-1]
    Firms.anlys$complete.weight.degree.lag[i] <- Firms.anlys$complete.weight.degree.y[i-1]
    Firms.anlys$complete.betweeness.lag[i] <- Firms.anlys$complete.betweeness.y[i-1]
    
    
  }
  
  else{next}
}


########################################MODELS#################################################

library(aod)
library(ggplot2)
library(miceadds)
library(sandwich)

grireport.eq.compl <- grireport ~ complete.betweeness.lag + wbcsd + tc + icc +  
                      envcont_d + logungc + logiorgs + logrevenue + roa +
                      factor(ind10_edt) + factor(region) + factor(year.x)                

grireport.mod.compl <- glm.cluster(formula = grireport.eq.compl,
                                   cluster= "a4id", 
                                   data = Firms.anlys,
                                   family = "gaussian")

coef(grireport.mod.compl)
vcov(grireport.mod.compl)
summary(grireport.mod.compl)


grireport.eq.decompose <- grireport ~ nation.degree.lag + inter.nation.degree.lag +
                       wbcsd + tc + icc +  
                       envcont_d + logungc + logiorgs + logrevenue + roa +
                       factor(ind10_edt) + factor(region) + factor(year.x)                

grireport.mod.decompose <- glm.cluster(formula = grireport.eq.decompose,
                                   cluster= "a4id", 
                                   data = Firms.anlys,
                                   family = "binomial")

summary(grireport.mod.decompose)
#grireport.eq.inter.nation <- grireport ~ inter.nation.betweeness.lag + wbcsd + tc + icc +  
#  envcont_d + logungc + logiorgs + logrevenue + roa +
#  factor(ind10_edt) + factor(region) + factor(year.x)                

#grireport.mod.inter.nation <- glm.cluster(formula = grireport.eq.inter.nation,
#                                    cluster= "a4id", 
#                                    data = Firms.anlys,
#                                    family = "binomial")

#################################################################################################

globalcmpt.eq.compl <- globalcmpt ~ complete.degree.lag + wbcsd + tc + icc +  
  envcont_d + logungc + logiorgs + logrevenue + roa +
  factor(ind10_edt) + factor(region) + factor(year.x)                

globalcmpt.mod.compl <- glm.cluster(formula = globalcmpt.eq.compl,
                                   cluster= "a4id", 
                                   data = Firms.anlys,
                                   family = "gaussian")

summary(globalcmpt.mod.compl)

globalcmpt.eq.decomposition <- globalcmpt ~ nation.degree.lag + inter.nation.degree.lag + 
                                            wbcsd + tc + icc +  
                                            envcont_d + logungc + logiorgs + logrevenue + roa +
                                            factor(ind10_edt) + factor(region) + factor(year.x)                

globalcmpt.mod.decomposition <- glm.cluster(formula = globalcmpt.eq.decomposition,
                                    cluster= "a4id", 
                                    data = Firms.anlys,
                                    family = "gaussian")

summary(globalcmpt.mod.decomposition)

#globalcmpt.eq.inter.nation <- globalcmpt ~ inter.nation.betweeness.lag + wbcsd + tc + icc +  
#  envcont_d + logungc + logiorgs + logrevenue + roa +
#  factor(ind10_edt) + factor(region) + factor(year.x)                

#globalcmpt.mod.inter.nation <- glm.cluster(formula = globalcmpt.eq.inter.nation,
#                                     cluster= "a4id", 
#                                     data = Firms.anlys,
#                                     family = "gaussian")

###########################################################################################

iso14.eq.compl <- iso14 ~ complete.degree.lag + wbcsd + tc + icc +  
  envcont_d + logungc + logiorgs + logrevenue + roa +
  factor(ind10_edt) + factor(region) + factor(year.x)                

iso14.mod.compl <- glm.cluster(formula = iso14.eq.compl,
                                    cluster= "a4id", 
                                    data = Firms.anlys,
                                    family = "gaussian")

summary(iso14.mod.compl)

iso14.eq.decompose <- iso14 ~ nation.degree.lag + inter.nation.degree.lag + 
                           wbcsd + tc + icc +  
                           envcont_d + logungc + logiorgs + logrevenue + roa +
                           factor(ind10_edt) + factor(region) + factor(year.x)                

iso14.mod.decompose <- glm.cluster(formula = iso14.eq.decompose,
                                     cluster= "a4id", 
                                     data = Firms.anlys,
                                     family = "gaussian")

summary(iso14.mod.decompose)

#iso14.eq.inter.nation <- iso14 ~ inter.nation.betweeness.lag + wbcsd + tc + icc +  
#  envcont_d + logungc + logiorgs + logrevenue + roa +
#  factor(ind10_edt) + factor(region) + factor(year.x)                

#iso14.mod.inter.nation <- glm.cluster(formula = iso14.eq.inter.nation,
#                                           cluster= "a4id", 
#                                           data = Firms.anlys,
#                                           family = "gaussian")


################################################################################################


evscore.eq.compl <- evscore ~ complete.degree.lag + wbcsd + tc + icc +  
                              envcont_d + logungc + logiorgs + logrevenue + roa +
                              factor(ind10_edt) + factor(region) + factor(year.x)                

evscore.mod.compl <- lm.cluster(formula = evscore.eq.compl,
                                    cluster= "a4id", 
                                    data = Firms.anlys)

summary(evscore.mod.compl)

evscore.eq.decomposition <- evscore ~ nation.degree.lag + inter.nation.degree.lag + 
  wbcsd + tc + icc +  
  envcont_d + logungc + logiorgs + logrevenue + roa +
  factor(ind10_edt) + factor(region) + factor(year.x)                

evscore.mod.decomposition <- lm.cluster(formula = evscore.eq.decomposition,
                                            cluster= "a4id", 
                                            data = Firms.anlys)


summary(evscore.mod.decomposition)




###################################################plm models##################################
library(plm)
library(coeftest) 
library(sandwich)
library(clubSandwich)

grireport.eq.compl <- grireport ~ complete.degree.new + wbcsd + tc + icc +  
  envcont_d + logungc + logiorgs + logrevenue + roa +
  factor(ind10_edt) + factor(region) + factor(year)



grireport.mod.compl <- plm(grireport.eq.compl, 
                           Firms.anlys, 
                           index = "a4id", 
                           model = "within")


grireport.eq.decompose <- grireport ~ nation.degree.new + inter.nation.degree.new +
  wbcsd + tc + icc +  
  envcont_d + logungc + logiorgs + logrevenue + roa +
  factor(ind10_edt) + factor(region)


grireport.mod.decompose <- plm(grireport.eq.decompose, 
                               Firms.anlys, 
                               index = c("a4id","year"), 
                               model = "within")

#model = "random", 
#random.models = c("within", "between"))

summary(grireport.mod.decompose)

####

globalcmpt.eq.decomposition <- globalcmpt ~ nation.degree.new + inter.nation.degree.new + 
  wbcsd + tc + icc +  
  envcont_d + logungc + logiorgs + logrevenue + roa +
  factor(ind10_edt) + factor(region)


globalcmpt.mod.decomposition <- plm(grireport.eq.decompose, 
                                    Firms.anlys, 
                                    index = c("a4id","year"), 
                                    model = "random", 
                                    random.models = c("within", "between"))

#model = "random", 
#random.models = c("within", "between"))


summary(globalcmpt.mod.decomposition)

########

iso14.eq.decompose <- iso14 ~ nation.degree.new + inter.nation.degree.new + 
  wbcsd + tc + icc +  
  envcont_d + logungc + logiorgs + logrevenue + roa +
  factor(ind10_edt) + factor(region) + factor(year) +
  factor(a4id)

iso14.mod.decompose <- glm.cluster(formula = iso14.eq.decompose,
                                   cluster= "a4id", 
                                   data = Firms.anlys,
                                   family = "gaussian")

summary(iso14.mod.decompose)




#mylogit <- glm(admit ~ gre + gpa + rank, data = mydata, family = "binomial")
#exp(coef(mylogit))

#exp(cbind(OR = coef(mylogit), confint(mylogit)))

#confint(mylogit) #confidence intervals

#confint.default(mylogit)
#wald.test(b = coef(mylogit), Sigma = vcov(mylogit), Terms = 4:6)
