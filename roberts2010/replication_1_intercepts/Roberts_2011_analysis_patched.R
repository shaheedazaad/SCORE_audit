library(tidyverse)
library(nlme)
library(emmeans)
options(scipen = 999)

# load the data
ECLS_2011 <- read_csv("ECLS_2011.csv") %>% 
  select(-c(S2_ID,S4_ID,S6_ID,S7_ID,S8_ID,S9_ID))

# put the data in long format to build growth models
ECLS_2011 = ECLS_2011 %>% 
  pivot_longer(-c(CHILDID,
                  X_CHSEX_R,
                  PRIM_LANG,
                  X12SESL,
                  W1C0), 
  values_to = "SCORE", 
  names_to = "ROUND")

# recode home language for easier interpretation
ECLS_2011 = ECLS_2011 %>% 
  mutate(PRIM_LANG = case_when(
    PRIM_LANG == 1 | PRIM_LANG == -1 ~ "english",
    PRIM_LANG == 2 ~ "spanish",
    PRIM_LANG == 3 ~ "asian",
    TRUE ~ "another"
  )) %>% 
  transform(ROUND = as.numeric(ROUND)) %>% 
  fastDummies::dummy_cols(select_columns = 'PRIM_LANG')

# get the 5% of the data
#ECLS_2011 = ECLS_2011 %>% 
 # sample_frac(0.05)

# TABLE 2
model_all = nlme::lme(
  SCORE ~ ROUND*PRIM_LANG,
  random = ~ 1|CHILDID,
  data = ECLS_2011,
  method = "ML", 
  weights = varFixed(~CHILDID|W1C0))

anova(model_all) 
print(broom.mixed::tidy(model_all, effects = "fixed"), n=100)
summary(model_all)

model_eng = nlme::lme(
  SCORE ~ ROUND*PRIM_LANG_english,
  random = ~ 1|CHILDID,
  data = ECLS_2011,
  method = "ML",
  weights = varFixed(~CHILDID|W1C0))
anova(model_eng)
broom.mixed::tidy(model_eng, effects = "fixed")
summary(model_eng)

model_spa = nlme::lme(
  SCORE ~ ROUND*PRIM_LANG_spanish,
  random = ~ 1|CHILDID,
  data = ECLS_2011,
  method = "ML",
  weights = varFixed(~CHILDID|W1C0))
anova(model_spa)
broom.mixed::tidy(model_spa, effects = "fixed")
summary(model_spa)

model_asi = nlme::lme(
  SCORE ~ ROUND*PRIM_LANG_asian,
  random = ~ 1|CHILDID,
  data = ECLS_2011,
  method = "ML",
  weights = varFixed(~CHILDID|W1C0))
anova(model_asi)
broom.mixed::tidy(model_asi, effects = "fixed")
summary(model_asi)

anova(model_eng, model_all, model_asi, model_spa)
#compare_table2 = emmeans(model_all, "PRIM_LANG")
# Patch: compare at around 0
compare_table2 = emmeans(model_all, "PRIM_LANG", at = list(ROUND = 0))
pairs(compare_table2)
summary(pairs(compare_table2))$p.value

# TABLE 3
model_all2 = nlme::lme(
  SCORE ~ ROUND + X12SESL + X12SESL*PRIM_LANG,
  random = ~ 1|CHILDID,
  data = ECLS_2011,
  method = "ML",
  weights = varFixed(~CHILDID|W1C0))
anova(model_all2)
broom.mixed::tidy(model_all2, effects = "fixed")
summary(model_all2)

model_eng2 = nlme::lme(
  SCORE ~ ROUND + X12SESL + X12SESL*PRIM_LANG_english,
  random = ~ 1|CHILDID,
  data = ECLS_2011,
  method = "ML",
  weights = varFixed(~CHILDID|W1C0))
anova(model_eng2)
broom.mixed::tidy(model_eng2, effects = "fixed")
summary(model_eng2)

model_spa2 = nlme::lme(
  SCORE ~ ROUND + X12SESL + X12SESL*PRIM_LANG_spanish,
  random = ~ 1|CHILDID,
  data = ECLS_2011,
  method = "ML",
  weights = varFixed(~CHILDID|W1C0))
anova(model_spa2)
broom.mixed::tidy(model_spa2, effects = "fixed")
summary(model_spa2)

model_asi2 = nlme::lme(
  SCORE ~ ROUND + X12SESL + X12SESL*PRIM_LANG_asian,
  random = ~ 1|CHILDID,
  data = ECLS_2011,
  method = "ML",
  )
anova(model_asi2)
broom.mixed::tidy(model_asi2, effects = "fixed")
summary(model_asi2)

anova(model_eng2, model_spa2, model_asi2)
compare_table3 = emmeans(model_all2, "PRIM_LANG")
pairs(compare_table3)
summary(pairs(compare_table3))$p.value
