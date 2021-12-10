library(survey)
library(dplyr)
library(simPop)
library(purrr)
library(sampling)

data('eusilcS', package = 'simPop') # Dados sintéticos gerados através de dados da EU-SILC


R <- 10000 # Número de iterações no bootstrap
set.seed(2801)


####################################### SIMPLE RANDOM SAMPLING #########################################


# Teste de independência entre pb220a (citizenship) e rb090 (gender)

srsvector <- c()
srsvectorsd <- c()

for (i in 1:R){
  
  srs_sample <- sample_n(eusilcS,1000, replace = T)
  
  srs_design <- svydesign(ids = ~1,
                          weights = NULL,
                          strata = NULL,
                          data = srs_sample)
  
  cqtsd <- svychisq(~rb090 + pb220a, design = srs_design)
  
  srsvectorsd <- c(srsvectorsd, 
                   ifelse(cqtsd$p.value<0.05,1,0))
  
  cqt <- chisq.test(srs_sample$pb220a,srs_sample$rb090)
  
  srsvector <- c(srsvector, 
                 ifelse(cqt$p.value<0.05,1,0))
}

mean(srsvectorsd)
mean(srsvector)

# RESULTADOS EQUIVALENTES



#################################### STRATIFIED SAMPLING ################################################




counttable <- eusilcS %>%
  count(db040)


db040 <- eusilcS$db040


fpc_str <- recode(db040,
       'Burgenland' = 433,
       'Carinthia' = 927,
       'Lower Austria' = 2080,
       'Salzburg' = 779,
       'Styria' = 1859,
       'Tyrol' = 1119,
       'Upper Austria' = 2120,
       'Vienna' = 1789,
       'Vorarlberg' = 619)

eusilcS <- eusilcS %>%
  mutate(fpc = fpc_str)



strvec <- c()
strvecsd <- c()


for(i in 1:R){
  
  str_sample <- sampling::strata(eusilcS,
                                 stratanames = 'db040',
                                 size = round(counttable$n/10), # tamanho total = 1173
                                 method = 'srswor')
  
  str_sample <- eusilcS[str_sample$ID_unit,]
  
  
  str_design <- svydesign(ids = ~1,
                          strata = ~db040,
                          weights = ~fpc,
                          data = str_sample)
  
  cqtsd <- svychisq(~rb090 + pb220a, design = str_design)
  
  strvecsd <- c(strvecsd, 
                   ifelse(cqtsd$p.value<0.05,1,0))
  
  cqt <- chisq.test(str_sample$pb220a,str_sample$rb090)
  
  strvec <- c(strvec, 
                 ifelse(cqt$p.value<0.05,1,0))
  
  
}

mean(strvec)

mean(strvecsd, na.rm = T)

# Diferença Considerável 


#################################### CLUSTER SAMPLING ###############################################

cluvec <- c()
cluvecsd <- c()

for(i in 1:R){
  
  selected_households <- sample(unique(eusilcS$db030), size = 1000)
  
  selected_households_df <- eusilcS[eusilcS$db030 %in% selected_households,]
  
  
  clu_design <- svydesign(ids = ~db040,
                          strata = NULL,
                          weights = ~hsize,
                          nest = TRUE,
                          data = selected_households_df)
    
    
  cqtsd <- svychisq(~rb090 + pb220a, design = clu_design)
  
  cluvecsd <- c(cluvecsd,
                ifelse(cqtsd$p.value<0.05,1,0))
  
  
  
  cqt <- chisq.test(selected_households_df$pb220a,selected_households_df$rb090)
  
  cluvec <- c(cluvec,
              ifelse(cqt$p.value<0.05,1,0))
  
  
}

mean(cluvecsd)

mean(cluvec)

# Diferença considerável

