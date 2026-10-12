library(lavaan)
library(dplyr)
library(jtools)
library(labelled)
library(lme4)
library(stargazer)

final.merge3 <- readRDS("data/final.merge.rds")

final.merge3 %>% as.data.frame()


#final.merge3<-
 # final.merge [sample(nrow(final.merge), nrow(final.merge)*0.05), ]

names(final.merge3)

#COSMOPOLITAN
#Peace'; 'Democracy'; 'Freedom to travel, study and work anywhere in the EU'
#'Cultural diversity'; and 'Stronger say in the world'.

#UTILITARIAN
#economic prosperity and social protection

#COMMUNITATARIAN
#'Loss of our cultural identity', 'More crime', 'Not
#enough control at external borders', and
#'Unemployment'

#LIBERTARIAN
#Bureaucracy' and 'Waste of money'

HS.model1 <- ' cosmopolitan  =~ qa12_1+ qa12_3 + qa12_5+qa12_6+qa12_7
              utilitarian =~ qa12_2+ qa12_4
              communitarian  =~ qa12_12 + qa12_13+ qa12_14+ qa12_9
              libertarian=~ qa12_10 + qa12_11 '

fit5 <- lavaan(HS.model1, data=final.merge3,
              auto.var=TRUE, auto.fix.first=TRUE,
              auto.cov.lv.x=TRUE)

summary(fit5, standardized=TRUE)



fit6 <- sem(HS.model1, data=final.merge3,
               auto.var=TRUE, auto.fix.first=TRUE,
               auto.cov.lv.x=TRUE)

summary(fit6, standardized=TRUE)

inspect(fit5, what = 'std')
inspect(fit5, what = 'r2')
fitmeasures(fit5)

###############################

final.merge3<-final.merge3 %>% 
  mutate(wave1 = recode(wave, 
                        "EB843" =1,
                        "EB883"=2,
                        "EB891"=3,
                        "EB812" =4,
                        "EB801" =5,
                        "EB852" =6,
                        "EB915"=7,
                        "EB873" =8,
                        "EB912"=9,
                        "EB833" =10,
                        "EB823"=11,
                        "EB903"=12,
                        "EB862"=13,
                        "EB814"=14
                        
                        ))

final.merge3$wave1



val_labels(final.merge3) <- NULL

varsToFactor <- c('qa12_1', 'qa12_3', 'qa12_5', 'qa12_6', 'qa12_7')
final.merge3[varsToFactor] <- lapply(final.merge3[varsToFactor], as.numeric)


match(c('qa12_1', 'qa12_3', 'qa12_5', 'qa12_6', 'qa12_7'),names(final.merge3))



#is.na(final.merge3[c(13,15,17:19)]) <-final.merge3[c(13,15,17:19)] == 0 #changes 0 values to NA
#final.merge$cosmo <- round(rowMeans(final.merge[, c(13,15,17:19)], na.rm=TRUE),2)
#final.merge1<-final.merge3 %>% 
#  mutate(cosmo=rowMeans(.[ , c('qa12_1', 'qa12_3', 'qa12_5', 'qa12_6', 'qa12_7')], na.rm=TRUE))

# PATCH: Teney (2016, p. 623) relative-frequency DV:
# cosmopolitan items mentioned / total of the 13 substantive items mentioned
items13     <- paste0("qa12_", c(1:7, 9:14))      # excludes 8 = Euro, 15 = Other, 16 = DK
cosmo_items <- paste0("qa12_", c(1, 3, 5, 6, 7))
final.merge3[items13] <- lapply(final.merge3[items13], function(x) as.numeric(x))

final.merge1 <- final.merge3 %>%
  mutate(n_mentioned = rowSums(across(all_of(items13)), na.rm = TRUE),
         cosmo = ifelse(n_mentioned > 0,
                        rowSums(across(all_of(cosmo_items)), na.rm = TRUE) / n_mentioned,
                        NA_real_))

   final.merge3 %>% group_by(wave) %>%
     summarise(across(paste0("qa12_", 1:16), ~ round(mean(as.numeric(.x), na.rm = TRUE), 2))) %>%
     print(width = Inf)

final.merge1$cosmo 



library(naniar)
na_strings1 <- c( 10, 11, 97, 98)
final.merge2 <- final.merge1%>%
    replace_with_na_at(.vars = 'd8r2',
                     condition = ~.x %in% na_strings1)

na_strings <- 8
final.merge2 <- final.merge2%>%
  replace_with_na_at(.vars = 'd25',
                     condition = ~.x %in% na_strings)

final.merge4<-final.merge2 %>%
  select(cosmo, d11, d8r2, d15a_r2,d10, d25, w1, country, wave1, UNEMPL, GDP
) %>% 
na.omit()


#model<- lm(cosmo~d11+factor(d8r2)+ relevel(factor(d15a_r2), ref = "4")
           #, data = final.merge4, weights = w1)

#summary(model)


varsToNumeric <- c('UNEMPL', 'GDP')
final.merge4[varsToNumeric] <- lapply(final.merge4[varsToNumeric ], as.numeric)


varsToFactor <- c('country', 'wave1', 'd8r2', 'd10', 'd25', 'd15a_r2')
final.merge4[varsToFactor] <- lapply(final.merge4[varsToFactor], as.factor)


class(final.merge4$country)

#model1<- lmer(cosmo~1+d11+d10+d8r2+relevel(d25, ref = "3")+
#                relevel(d15a_r2, ref = "4")+ UNEMPL+
#                I(UNEMPL^2)+GDP+I(GDP^2)+
#                d8r2*GDP+d8r2*UNEMPL+
#                +(1|wave1)+
#                +(1|country),data = final.merge4, REML=FALSE, weights = w1)


# PATCH: remove squaree terms (Teney 2016, p. 627 Table 3, model 1 shows no squared terms!)

model1<- lmer(cosmo~1+d11+d10+d8r2+relevel(d25, ref = "3")+
                
                relevel(d15a_r2, ref = "4")+ UNEMPL+
                GDP+
                d8r2*GDP+d8r2*UNEMPL+
                +(1|wave1)+
                +(1|country),data = final.merge4, REML=FALSE, weights = w1)

summary(model1)

?summ
summ(model1, round=4)

?stargazer
stargazer(model1,
          type= "html", 
          column.separate = c(2,1,3,4),
          covariate.labels = c("Age Exact", "Gender",
                              " Middle Edu",  "High Edu", 
                              'Rural', "Town",
                             ' Self-employed',
                             ' Managers', 
                             ' Other white collars ',
                            '  House persons',
                             ' Unemployed ',
                             ' Retired ',
                              "Unemployment", 
                            "Unemployment square", 
                              "Growth",
                            "Growth Square",
                              "Middle Edu*Growth",
                            "High Edu*Growth",
                              "Middle Edu*Unemployment",
                            "High Edu*Unemployment"
                               ),
       
                    #keep=c('UNEMPL'),
          out="teney1_updated.html", report =("vc*ps"))
browseURL("teney1_updated.html")


?stargazer

stargazer(model1,
          type= "html",
          
          keep=c("UNEMPL", "GDP"),
          out="model_updated.html", report =("vc*ps"))
browseURL('model_updated.html')
