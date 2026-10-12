data <- read.csv("data/roberts_2010_OYX0_data.csv")
#####################1. LOAD SPSS FILE AS DATA FRAME
#ENGLISH
table(data$X4LANGST)#home language of child - this is measured spring of first grade, originally they used the language spoken at home measured during kindergarten,
#but this is the closest measurement to that we have
#original coding:
#1 = non-english
#2 = english
#3 = can't choose which is primary
data$primary_language <- ifelse(data$X4LANGST==2, "English", NA)

#SPANISH
table(data$C1SPHOME)#1=yes
data$primary_language <- ifelse(data$C1SPHOME==1 , "Spanish", data$primary_language)#indicates they speak spanish at home

#ASIAN LANGUAGE
#the original paper uses ethnicity to identify Asian ELLs, so we do the same here
table(data$X_ASIAN_R)
#data$primary_language <- ifelse(data$X_ASIAN_R==1, "Asian", data$primary_language) # original categorisation includes Native English speakers
data$primary_language <- ifelse(data$X_ASIAN_R==1 & data$X4LANGST==1, "Asian", data$primary_language)

#sample sizes for each language before excluding participants
table(data$primary_language)

write.csv(data,"data/roberts_2010_OYX0_data_recoded.csv", row.names = FALSE)