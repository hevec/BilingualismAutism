######################################################### INSTALL PACKAGES #########################################################

if(!"readr" %in% rownames(installed.packages())) {
  install.packages("readr") 
}
library(readr) # For importing data

if(!"ggplot2" %in% rownames(installed.packages())) {
  install.packages("ggplot2") 
}
library(ggplot2) # For plotting

if(!"dplyr" %in% rownames(installed.packages())) {
  install.packages("dplyr") 
}
library(dplyr) # For pipes

if(!"tidyr" %in% rownames(installed.packages())) {
  install.packages("tidyr") 
}
library(tidyr) # For ease of data editing

if(!"Amelia" %in% rownames(installed.packages())) {
  install.packages("Amelia") 
}
library(Amelia) # For missing map

if(!"corrplot" %in% rownames(installed.packages())) {
  install.packages("corrplot") 
}
library(corrplot) # For correlation plot

if(!"GGally" %in% rownames(installed.packages())) {
  install.packages("GGally") 
}
library(GGally) # For pairs plot

if(!"MASS" %in% rownames(installed.packages())) {
  install.packages("MASS") 
}
library(MASS) # For Box-Cox transform

if(!"lavaan" %in% rownames(installed.packages())) {
  install.packages("lavaan") 
}
library(lavaan) # For CFA

######################################################### LOAD DATA #########################################################

ASD.data <- read.csv('Raw Data.csv')






######################################################### EXPLORATORY ANALYSIS ######################################################### 

ASD.plotting <- ASD.data

ASD.plotting[which(ASD.plotting$where_english == 1),]$where_english <- "Home"
ASD.plotting[which(ASD.plotting$where_english == 2),]$where_english <- "Nursery"
ASD.plotting[which(ASD.plotting$where_english == 3),]$where_english <- "Playgroup"
ASD.plotting[which(ASD.plotting$where_english == 4),]$where_english <- "School"

ASD.plotting[ASD.plotting$gender == 0,]$gender <- "Male"
ASD.plotting[ASD.plotting$gender == 1,]$gender <- "Female"
ASD.plotting[ASD.plotting$diagnosis == 0,]$diagnosis <- "Neurotypical"
ASD.plotting[ASD.plotting$diagnosis == 1,]$diagnosis <- "Autistic"

nrow(ASD.plotting) # 89 observations
missmap(ASD.plotting) # No NAs but missingness is present (see below - 888 often used)

# Demographics and Language
ASD.plotting %>% 
  count(var = gender, diag = diagnosis) %>% 
  mutate(pct = n/rep(c(38, 51), length(unique(var)))) %>% 
  ggplot(aes(x = var, y = n, fill = diag, label = scales::percent(pct))) + 
  geom_col(position = 'dodge') + 
  geom_text(position = position_dodge(width = .9),    
            vjust = -0.5, 
            size = 3)
table(ASD.plotting$gender,ASD.plotting$diagnosis) %>% sum
ASD.plotting  %>% filter(tomi_compmean!=888) %>% group_by(diagnosis) %>% summarise(mean = mean(tomi_compmean), sd=sd(tomi_compmean))


ASD.plotting %>% ggplot(aes(x=age_m/12)) + geom_histogram(bins = 17) + facet_grid(col=vars(diagnosis))
table(ASD.plotting$age_m)
ASD.plotting %>% group_by(diagnosis) %>% summarise(mean = mean(age_m), sd=sd(age_m), min=min(age_m), max=max(age_m))

ASD.plotting %>% ggplot(aes(x=diagnosis)) + geom_bar(stat="count")
table(ASD.plotting$diagnosis)

ASD.plotting %>% filter(diagnosis=="Neurotypical") %>% ggplot(aes(x=SCQ)) + geom_histogram(bins=11) # social communication questionnaire total score (NT only)

ASD.plotting %>% ggplot(aes(x=bpvs_raw)) + geom_histogram(bins = 17) + facet_grid(col=vars(diagnosis)) # BPVS scores: Knowledge of English vocab
ASD.plotting %>% group_by(diagnosis) %>% summarise(mean = mean(bpvs_raw), sd=sd(bpvs_raw))

ASD.plotting[which(ASD.plotting$vocabprocess_processing_speed_target==888),]
ASD.plotting %>% filter(vocabprocess_processing_speed_target>888) %>% ggplot(aes(x=vocabprocess_processing_speed_target)) + geom_histogram(bins = 17) + facet_grid(col=vars(diagnosis)) # Language processing speed
ASD.plotting  %>% filter(vocabprocess_processing_speed_target>888) %>% group_by(diagnosis) %>% summarise(mean = mean(vocabprocess_processing_speed_target), sd=sd(vocabprocess_processing_speed_target))

table(ASD.plotting$wasi_sum_rawscores)
table(ASD.plotting$wasi_sum_rawscores == 0, ASD.plotting$diagnosis)
ASD.plotting %>% filter(wasi_sum_rawscores>0) %>% ggplot(aes(x=wasi_sum_rawscores)) + geom_histogram(bins = 17) + facet_grid(col=vars(diagnosis)) # IQ
ASD.plotting %>% filter(wasi_sum_rawscores>0) %>% group_by(diagnosis) %>% summarise(mean = mean(wasi_sum_rawscores), sd=sd(wasi_sum_rawscores), min=min(wasi_sum_rawscores), max=max(wasi_sum_rawscores))


# Bilingualism indicators: input
table(ASD.plotting$bilec_home_input)
ASD.plotting %>% group_by(diagnosis) %>% summarise(mean = mean(bilec_home_input), sd=sd(bilec_home_input), min=min(bilec_home_input), max=max(bilec_home_input))
ASD.plotting %>% ggplot(aes(x=bilec_home_input)) + geom_histogram(aes(fill=diagnosis))  # Non english
ASD.plotting %>% ggplot(aes(x=bilec_english_input)) + geom_histogram(aes(fill=diagnosis)) # English
# These don't always add up:
length(which(ASD.plotting$bilec_home_input+ASD.plotting$bilec_english_input <100))
ASD.plotting %>% ggplot(aes(x=bilec_home_input+bilec_english_input)) + geom_histogram(aes(fill=diagnosis)) 
ASD.plotting %>% ggplot(aes(x=bilec_total_input)) + geom_histogram(aes(fill=diagnosis))  # Overall bilingualism ability

which(ASD.plotting$bilec_total_input != 2*pmin(ASD.plotting$bilec_home_input, ASD.plotting$bilec_english_input)) # Observation 56 doesn't equal 2*min so amend this
ASD.plotting[56,]$bilec_total_input <- 2*min(ASD.plotting[56,]$bilec_home_input, ASD.plotting[56,]$bilec_english_input)
ASD.plotting %>% group_by(diagnosis) %>% summarise(mean = mean(bilec_total_input), sd=sd(bilec_total_input), min=min(bilec_total_input), max=max(bilec_total_input))



# Bilingualism indicators: output
ASD.plotting %>% ggplot(aes(x=bilec_home_output)) + geom_histogram(aes(fill=diagnosis)) # Non english
ASD.plotting %>% ggplot(aes(x=bilec_english_output)) + geom_histogram(aes(fill=diagnosis)) # English
# These don't always add up:
ASD.plotting %>% ggplot(aes(x=bilec_home_output+bilec_english_output)) + geom_histogram(aes(fill=diagnosis)) 
ASD.plotting %>% ggplot(aes(x=bilec_total_output)) + geom_histogram(aes(fill=diagnosis))  # Overall bilingualism ability


# Bilingualism factors
# Age of second language learning
ASD.plotting %>%
  count(var = age_acquisition, diag = diagnosis) %>% 
  mutate(pct = n/c(rep(c(38, 51), 4), 38, 38, 51)) %>% 
  ggplot(aes(x = var, y = n, fill = diag, label = scales::percent(pct))) + 
  geom_col(position = 'dodge') + 
  geom_text(position = position_dodge(width = .9),    
            vjust = -0.5,   
            size = 3) # https://stackoverflow.com/questions/40249943/adding-percentage-labels-to-a-bar-chart-in-ggplot2

# Where they learned ENGLISH
ASD.plotting %>% 
  count(var = where_english, diag = diagnosis) %>% 
  mutate(pct = n/rep(c(38, 51), length(unique(var)))) %>% 
  ggplot(aes(x = var, y = n, fill = diag, label = scales::percent(pct))) + 
  geom_col(position = 'dodge') + 
  geom_text(position = position_dodge(width = .9),    
            vjust = -0.5, 
            size = 3)



# Theory of Mind
table(ASD.plotting$tomi_compmean==888, ASD.plotting$diagnosis)
ASD.plotting  %>% filter(tomi_compmean!=888) %>% group_by(diagnosis) %>% summarise(mean = mean(tomi_compmean), sd=sd(tomi_compmean))

ASD.plotting %>% filter(tomi_early != 888) %>% ggplot(aes(x=tomi_early)) + geom_histogram(bins = 21) + facet_grid(col=vars(diagnosis)) # Early abilities
ASD.plotting %>% filter(tomi_basic != 888) %>% ggplot(aes(x=tomi_basic)) + geom_histogram(bins = 21) + facet_grid(col=vars(diagnosis)) # Basic abilities
ASD.plotting %>% filter(tomi_advanced != 888) %>% ggplot(aes(x=tomi_advanced)) + geom_histogram(bins = 21) + facet_grid(col=vars(diagnosis)) # Advanced abilities
ASD.plotting %>% filter(tomi_compmean != 888) %>% ggplot(aes(x=tomi_compmean)) + geom_histogram(bins = 21) + facet_grid(col=vars(diagnosis)) # Composite score
ASD.plotting %>% filter(tom_tb_totalscore != 888) %>% ggplot(aes(x=tom_tb_totalscore)) + geom_histogram(bins = 16) + facet_grid(col=vars(diagnosis)) # Composite score
table((ASD.plotting$tomi_early + ASD.plotting$tomi_basic + ASD.plotting$tomi_advanced)/3 -ASD.plotting$tomi_compmean)
which(((ASD.plotting$tomi_early + ASD.plotting$tomi_basic + ASD.plotting$tomi_advanced)/3 -ASD.plotting$tomi_compmean)==4.5)
# observations 27 differs by 4.5
mean((ASD.plotting$tomi_early + ASD.plotting$tomi_basic + ASD.plotting$tomi_advanced)/3 -ASD.plotting$tomi_compmean)
sd((ASD.plotting$tomi_early + ASD.plotting$tomi_basic + ASD.plotting$tomi_advanced)/3 -ASD.plotting$tomi_compmean)

table(ASD.plotting$tom_tb_totalscore==888, ASD.plotting$diagnosis)
ASD.plotting  %>% filter(tom_tb_totalscore!=888) %>% group_by(diagnosis) %>% summarise(mean = mean(tom_tb_totalscore), sd=sd(tom_tb_totalscore))




# Social cognition
# Interacting and not interacting
ASD.plotting %>% filter(et_figurestask_dwell_time_interacting!=888) %>% ggplot(aes(x=et_figurestask_dwell_time_interacting)) + geom_histogram(bins = 21) + facet_grid(col=vars(diagnosis)) 
ASD.plotting %>% filter(et_figurestask_dwell_time_not_interacting!=888) %>% ggplot(aes(x=et_figurestask_dwell_time_not_interacting)) + geom_histogram(bins = 21) + facet_grid(col=vars(diagnosis)) 
ASD.plotting %>% filter(et_figurestask_dwell_time_not_interacting!=888) %>% ggplot(aes(x=et_figurestask_dwell_time_interacting/et_figurestask_dwell_time_not_interacting)) + geom_histogram(bins = 21) + facet_grid(col=vars(diagnosis))

table(ASD.plotting$et_figurestask_dwell_time_interacting==888, ASD.plotting$diagnosis)
ASD.plotting  %>% filter(et_figurestask_dwell_time_interacting!=888) %>% group_by(diagnosis) %>% summarise(mean = mean(et_figurestask_dwell_time_interacting), sd=sd(et_figurestask_dwell_time_interacting))
table(ASD.plotting$et_figurestask_dwell_time_not_interacting==888, ASD.plotting$diagnosis)
ASD.plotting  %>% filter(et_figurestask_dwell_time_not_interacting!=888) %>% group_by(diagnosis) %>% summarise(mean = mean(et_figurestask_dwell_time_not_interacting), sd=sd(et_figurestask_dwell_time_not_interacting))
ASD.plotting  %>% filter(et_figurestask_dwell_time_not_interacting!=888) %>% group_by(diagnosis) %>% summarise(mean = mean(et_figurestask_dwell_time_interacting-et_figurestask_dwell_time_not_interacting), sd=sd(et_figurestask_dwell_time_interacting-et_figurestask_dwell_time_not_interacting))


# Correct picture and not correct picture (Theory of Mind)
ASD.plotting %>% filter(et_falsebelief_Testtrial_dwell_time_to_correct!=888) %>% ggplot(aes(x=et_falsebelief_Testtrial_dwell_time_to_correct)) + geom_histogram(bins = 21) + facet_grid(col=vars(diagnosis)) 
ASD.plotting %>% filter(et_falsebelief_testtrial_dwell_time_to_incorrect!=888) %>% ggplot(aes(x=et_falsebelief_testtrial_dwell_time_to_incorrect)) + geom_histogram(bins = 21) + facet_grid(col=vars(diagnosis)) 
ASD.plotting %>% filter(et_falsebelief_testtrial_preference_score!=888) %>% ggplot(aes(x=et_falsebelief_testtrial_preference_score)) + geom_histogram(bins = 30) + facet_grid(col=vars(diagnosis))

table(ASD.plotting$et_falsebelief_Testtrial_dwell_time_to_correct==888, ASD.plotting$diagnosis)
ASD.plotting  %>% filter(et_falsebelief_Testtrial_dwell_time_to_correct!=888) %>% group_by(diagnosis) %>% summarise(mean = mean(et_falsebelief_Testtrial_dwell_time_to_correct), sd=sd(et_falsebelief_Testtrial_dwell_time_to_correct))
table(ASD.plotting$et_falsebelief_testtrial_dwell_time_to_incorrect==888, ASD.plotting$diagnosis)
ASD.plotting  %>% filter(et_falsebelief_testtrial_dwell_time_to_incorrect!=888) %>% group_by(diagnosis) %>% summarise(mean = mean(et_falsebelief_testtrial_dwell_time_to_incorrect), sd=sd(et_falsebelief_testtrial_dwell_time_to_incorrect))
table(ASD.plotting$et_falsebelief_testtrial_preference_score==888, ASD.plotting$diagnosis)
ASD.plotting  %>% filter(et_falsebelief_testtrial_preference_score!=888) %>% group_by(diagnosis) %>% summarise(mean = mean(et_falsebelief_testtrial_preference_score), sd=sd(et_falsebelief_testtrial_preference_score))



ASD.plotting$falsebelief_binary <- "Incorrect"
ASD.plotting[which(ASD.plotting$et_falsebelief_testtrial_preference_score==888),]$falsebelief_binary <- NA
ASD.plotting[which(ASD.plotting$et_falsebelief_testtrial_preference_score>0),]$falsebelief_binary <- "Correct"
ASD.plotting %>%
  count(var = falsebelief_binary, diag = diagnosis) %>% 
  mutate(pct = n/rep(c(38, 51), length(unique(var)))) %>% 
  ggplot(aes(x = var, y = n, fill = diag, label = scales::percent(pct))) + 
  geom_col(position = 'dodge') + 
  geom_text(position = position_dodge(width = .9),    
            vjust = -0.5,   
            size = 3) 


# BRIEF
table(ASD.plotting$brief_raw_working_memory==888, ASD.plotting$diagnosis)
ASD.plotting %>% filter(brief_raw_emotional_control!=888) %>% ggplot(aes(x=brief_raw_emotional_control)) + geom_histogram(bins=17) + facet_grid(col=vars(diagnosis)) 
ASD.plotting %>% filter(brief_raw_inhibit!=888) %>% ggplot(aes(x=brief_raw_inhibit)) + geom_histogram(bins=17) + facet_grid(col=vars(diagnosis)) 
ASD.plotting %>% filter(brief_raw_self.monitor!=888) %>% ggplot(aes(x=brief_raw_self.monitor)) + geom_histogram(bins=17) + facet_grid(col=vars(diagnosis)) 
ASD.plotting %>% filter(brief_raw_shift!=888) %>% ggplot(aes(x=brief_raw_shift)) + geom_histogram(bins=17) + facet_grid(col=vars(diagnosis)) 
ASD.plotting %>% filter(brief_raw_initiate!=888) %>% ggplot(aes(x=brief_raw_initiate)) + geom_histogram(bins=17) + facet_grid(col=vars(diagnosis)) 
ASD.plotting %>% filter(brief_raw_working_memory!=888) %>% ggplot(aes(x=brief_raw_working_memory)) + geom_histogram(bins=17) + facet_grid(col=vars(diagnosis)) 
ASD.plotting %>% filter(brief_raw_plan_organise!=888) %>% ggplot(aes(x=brief_raw_plan_organise)) + geom_histogram(bins=17) + facet_grid(col=vars(diagnosis)) 
ASD.plotting %>% filter(brief_raw_task_monitor!=888) %>% ggplot(aes(x=brief_raw_task_monitor)) + geom_histogram(bins=17) + facet_grid(col=vars(diagnosis)) 
ASD.plotting %>% filter(brief_raw_organisation_of_materials!=888) %>% ggplot(aes(x=brief_raw_organisation_of_materials)) + geom_histogram(bins=17) + facet_grid(col=vars(diagnosis)) 

ASD.plotting %>%  filter(brief_raw_emotional_control!=888) %>% group_by(diagnosis) %>% summarise(mean = mean(brief_raw_organisation_of_materials), sd=paste0("(", signif(sd(brief_raw_organisation_of_materials),3),")"))


# Flanker
table(ASD.plotting$flanker_percenterrors_congruent==888, ASD.plotting$diagnosis)
ASD.plotting %>% filter(flanker_percenterrors_congruent!=888) %>% ggplot(aes(x=flanker_percenterrors_congruent)) + geom_histogram(bins=50) + facet_grid(col=vars(diagnosis)) 
ASD.plotting %>% filter(flanker_percenterrors_incongruent!=888) %>% ggplot(aes(x=flanker_percenterrors_incongruent)) + geom_histogram(bins=50) + facet_grid(col=vars(diagnosis)) 
ASD.plotting %>% filter(flanker_mean_rt_congruent!=888) %>% ggplot(aes(x=flanker_mean_rt_congruent)) + geom_histogram(bins=50) + facet_grid(col=vars(diagnosis)) 
ASD.plotting %>% filter(flanker_mean_rt_incongruent!=888) %>% ggplot(aes(x=flanker_mean_rt_incongruent)) + geom_histogram(bins=50) + facet_grid(col=vars(diagnosis)) 

ASD.plotting %>%  filter(flanker_percenterrors_incongruent!=888) %>% group_by(diagnosis) %>% summarise(mean = mean(flanker_mean_rt_incongruent), sd=paste0("(", signif(sd(flanker_mean_rt_incongruent),3),")"))


# PVT
ASD.plotting$pvt_mean_rt <- as.character(ASD.plotting$pvt_mean_rt)
ASD.plotting[which(ASD.plotting$pvt_mean_rt=="11.40.8333333"),]$pvt_mean_rt <- "1140.8333333" # Assumption
ASD.plotting$pvt_mean_rt <- as.numeric(ASD.plotting$pvt_mean_rt)
table(ASD.plotting$pvt_mean_rt==888, ASD.plotting$diagnosis)
ASD.plotting %>% filter(pvt_mean_rt!=888) %>% ggplot(aes(x=as.numeric(pvt_mean_rt))) + geom_histogram(bins=50) + facet_grid(col=vars(diagnosis)) 
ASD.plotting %>% filter(pvt_number_of_lapses!=888) %>% ggplot(aes(x=as.numeric(pvt_number_of_lapses))) + geom_histogram(bins=25) + facet_grid(col=vars(diagnosis)) 
ASD.plotting %>% filter(pvt_mean_lapse_rt!=888) %>% ggplot(aes(x=as.numeric(pvt_mean_lapse_rt))) + geom_histogram(bins=25) + facet_grid(col=vars(diagnosis)) 
ASD.plotting %>% filter(pvt_count_falsestarts!=888) %>% ggplot(aes(x=as.numeric(pvt_count_falsestarts))) + geom_histogram(bins=25) + facet_grid(col=vars(diagnosis)) 

ASD.plotting %>%  filter(pvt_count_falsestarts!=888) %>% group_by(diagnosis) %>% summarise(mean = mean(pvt_count_falsestarts), sd=paste0("(", signif(sd(pvt_count_falsestarts),3),")"))









######################################################### CLEANING ######################################################### 

ASD.data[which(ASD.data$where_english == 1),]$where_english <- "Home"
ASD.data[which(ASD.data$where_english == 2),]$where_english <- "Nursery"
ASD.data[which(ASD.data$where_english == 3),]$where_english <- "Playgroup"
ASD.data[which(ASD.data$where_english == 4),]$where_english <- "School"


ASD.data[which(ASD.data$diagnosis==1),]$SCQ <- NA # Although won't be using this anyway


# Vocab processing
ASD.data[which(ASD.data$vocabprocess_processing_speed_target==888),]$vocabprocess_processing_speed_target <- NA

# IQ
ASD.data[which(ASD.data$wasi_sum_rawscores==0),]$wasi_sum_rawscores <- NA


# ToM
ASD.data[which(ASD.data$tomi_early==888),]$tomi_early <- NA
ASD.data[which(ASD.data$tomi_basic==888),]$tomi_basic <- NA
ASD.data[which(ASD.data$tomi_advanced==888),]$tomi_advanced <- NA
ASD.data[which(ASD.data$tomi_compmean==888),]$tomi_compmean <- NA
ASD.data[which(ASD.data$tom_tb_totalscore==888),]$tom_tb_totalscore <- NA
missmap(ASD.data) # The same four who have missing scores!


# Total bilingual input (exposure) inconsistent
ASD.data$biblec.input.inconsistent <- 0
ASD.data[which(ASD.data$bilec_home_input+ASD.data$bilec_english_input != 100),]$biblec.input.inconsistent <- 1
table(ASD.data$biblec.input.inconsistent)

# Total bilingual output (speaking) inconsistent
ASD.data$biblec.output.inconsistent <- 0
ASD.data[which(ASD.data$bilec_home_output+ASD.data$bilec_english_output != 100),]$biblec.output.inconsistent <- 1
table(ASD.data$biblec.output.inconsistent)

# Social cognition
ASD.data[which(ASD.data$et_figurestask_dwell_time_interacting==888),]$et_figurestask_dwell_time_interacting <- NA
ASD.data[which(ASD.data$et_figurestask_dwell_time_not_interacting==888),]$et_figurestask_dwell_time_not_interacting <- NA
ASD.data[which(ASD.data$et_falsebelief_Testtrial_dwell_time_to_correct==888),]$et_falsebelief_Testtrial_dwell_time_to_correct <- NA
ASD.data[which(ASD.data$et_falsebelief_testtrial_dwell_time_to_incorrect==888),]$et_falsebelief_testtrial_dwell_time_to_incorrect <- NA
ASD.data[which(ASD.data$et_falsebelief_testtrial_preference_score==888),]$et_falsebelief_testtrial_preference_score <- NA
ASD.data$et_figurestask_dwell_time_preference <- ASD.data$et_figurestask_dwell_time_interacting - ASD.data$et_figurestask_dwell_time_not_interacting # More parsimonious model and the thing thats really of interest

# BRIEF
ASD.data[which(ASD.data$brief_raw_inhibit==888),]$brief_raw_inhibit <- NA
ASD.data[which(ASD.data$brief_raw_self.monitor==888),]$brief_raw_self.monitor <- NA
ASD.data[which(ASD.data$brief_raw_shift==888),]$brief_raw_shift <- NA
ASD.data[which(ASD.data$brief_raw_emotional_control==888),]$brief_raw_emotional_control <- NA
ASD.data[which(ASD.data$brief_raw_initiate==888),]$brief_raw_initiate <- NA
ASD.data[which(ASD.data$brief_raw_working_memory==888),]$brief_raw_working_memory <- NA
ASD.data[which(ASD.data$brief_raw_plan_organise==888),]$brief_raw_plan_organise <- NA
ASD.data[which(ASD.data$brief_raw_task_monitor==888),]$brief_raw_task_monitor <- NA
ASD.data[which(ASD.data$brief_raw_organisation_of_materials==888),]$brief_raw_organisation_of_materials <- NA


# Flanker
ASD.data[which(ASD.data$flanker_percenterrors_congruent==888),]$flanker_percenterrors_congruent <- NA
ASD.data[which(ASD.data$flanker_percenterrors_incongruent==888),]$flanker_percenterrors_incongruent <- NA
ASD.data[which(ASD.data$flanker_mean_rt_congruent==888),]$flanker_mean_rt_congruent <- NA
ASD.data[which(ASD.data$flanker_mean_rt_incongruent==888),]$flanker_mean_rt_incongruent <- NA

ASD.data$flanker_mean_rt <- (ASD.data$flanker_mean_rt_congruent + ASD.data$flanker_mean_rt_incongruent)/2 # Add in new variables as a result of EDA (correlation plots)
ASD.data$flanker_percenterrors <- (ASD.data$flanker_percenterrors_congruent + ASD.data$flanker_percenterrors_incongruent)/2

# PVT
ASD.data$pvt_mean_rt <- as.character(ASD.data$pvt_mean_rt) # Mean rt stored as factor so firstly change this
ASD.data[which(ASD.data$pvt_mean_rt=="11.40.8333333"),]$pvt_mean_rt <- "1140.8333333" # Assumption
ASD.data$pvt_mean_rt <- as.numeric(ASD.data$pvt_mean_rt)

ASD.data[which(ASD.data$pvt_mean_rt==888),]$pvt_mean_rt <- NA
ASD.data[which(ASD.data$pvt_number_of_lapses==888),]$pvt_number_of_lapses <- NA
ASD.data[which(ASD.data$pvt_mean_lapse_rt==888),]$pvt_mean_lapse_rt <- NA
ASD.data[which(ASD.data$pvt_count_falsestarts==888),]$pvt_count_falsestarts <- NA


missmap(ASD.data)


# Groups of measurements: potentially useful for subsetting in future code
social.cognition.domain <- c("et_figurestask_dwell_time_interacting", "et_figurestask_dwell_time_not_interacting", "et_figurestask_dwell_time_preference",
                             "tomi_early", "tomi_basic", "tomi_advanced", "tomi_compmean",
                             "tom_tb_totalscore",
                             "et_falsebelief_Testtrial_dwell_time_to_correct", "et_falsebelief_testtrial_dwell_time_to_incorrect", "et_falsebelief_testtrial_preference_score")

language.domain <- c("bpvs_raw", 
                     "vocabprocess_processing_speed_target") 

executive.functions.domain <- c(ASD.data %>% dplyr::select(starts_with("brief")) %>% colnames,
                                ASD.data %>% dplyr::select(starts_with("flanker")) %>% colnames,
                                ASD.data %>% dplyr::select(starts_with("pvt")) %>% colnames)

bilingualism <- c(ASD.data %>% dplyr::select(starts_with("biblec")) %>% colnames,
                  "age_acquisition", "where_english")

demographics <- c("gender", "age_m", "diagnosis", "SCQ", "wasi_sum_rawscores")


# Type of measurement

eye.tracking <- c("et_figurestask_dwell_time_interacting", "et_figurestask_dwell_time_not_interacting",
                  "et_falsebelief_Testtrial_dwell_time_to_correct", "et_falsebelief_testtrial_dwell_time_to_incorrect", "et_falsebelief_testtrial_preference_score",
                  "vocabprocess_processing_speed_target")

parent.report <- c("tomi_early", "tomi_basic", "tomi_advanced", "tomi_compmean",
                   ASD.data %>% dplyr::select(starts_with("brief")) %>% colnames,
                   ASD.data %>% dplyr::select(starts_with("biblec")) %>% colnames,
                   "age_acquisition", "where_english")

direct.assessment <- c("tom_tb_totalscore", 
                       "flanker_mean_rt", "flanker_percenterrors",
                       ASD.data %>% dplyr::select(starts_with("pvt")) %>% colnames,
                       "SCQ", "wasi_sum_rawscores", "bpvs_raw")














######################################################### DIFFERENCES BETWEEN MEASUREMENT TYPES ######################################################### 

ASD.plotting <- ASD.data
ASD.plotting <- ASD.plotting[-c(3, 78),] # Because of outliers removed later for the CFA analysis - more consistent to use the same data

# Agreement of social cognition measures
soc.cog <- c("tomi_compmean", "tom_tb_totalscore", "et_figurestask_dwell_time_preference", "et_falsebelief_testtrial_preference_score")
soc.cog.data <- ASD.plotting[,which(names(ASD.plotting) %in% soc.cog)]
corrplot(cor(soc.cog.data, use="pairwise.complete.obs"))
ggpairs(soc.cog.data)+ theme_bw()
# Percentages of ToM scores from parent vs TB compared
soc.cog.data$parent_percentage <- soc.cog.data$tomi_compmean/20*100 
soc.cog.data$TB_percentage <- soc.cog.data$tom_tb_totalscore/15*100
hist(soc.cog.data$parent_percentage-soc.cog.data$TB_percentage)
which(soc.cog.data$parent_percentage-soc.cog.data$TB_percentage >0) %>% length



# Agreement of language measures
lang <- c("bpvs_raw", "vocabprocess_processing_speed_target", "age_m", "gender")
lang.data <- ASD.plotting[,which(names(ASD.plotting) %in% lang)]
corrplot(cor(lang.data, use="pairwise.complete.obs"))
ggpairs(lang.data)+ theme_bw()



# Agreement of executive function measures
ef <- c("brief_raw_inhibit", "brief_raw_self.monitor","brief_raw_shift",
        "brief_raw_emotional_control" ,"brief_raw_initiate",
        "brief_raw_working_memory","brief_raw_plan_organise",
        "brief_raw_task_monitor" ,"brief_raw_organisation_of_materials",
        "flanker_percenterrors","pvt_count_falsestarts" )
exec.func <- ASD.plotting[,which(names(ASD.plotting) %in% ef)]
corrplot(cor(exec.func, use="pairwise.complete.obs"))
ggpairs(exec.func)+ theme_bw()
ASD.data %>% dplyr::select(starts_with("brief")) %>% ggpairs(.) + theme_bw()
ASD.data %>% dplyr::select(starts_with("flanker")) %>% ggpairs(.) + theme_bw()
ASD.data %>% dplyr::select(starts_with("pvt")) %>% ggpairs(.) + theme_bw()




# Outliers
# Observation three is outlying for BRIEF inhibit and self monitor, 78 odd for initiate
ASD.data$plotcol <- "Not 3 or 78"
ASD.data[3,]$plotcol <- "3"
ASD.data[78,]$plotcol <- "78"
ASD.data %>% dplyr::select(starts_with("brief") | plotcol) %>% ggpairs(., aes(colour=plotcol))+ theme_bw()
everything.3 <- ASD.data[-c(3, 78),which(names(ASD.data) %in% social.cognition.domain | names(ASD.data) %in% language.domain | names(ASD.data) %in% executive.functions.domain)]
corrplot(cor(everything.3, use="pairwise.complete.obs"))
# 26  is an outlier for time not interacting but preference score fine
ggpairs(ASD.data %>% dplyr::select(starts_with("et")))+ theme_bw()
table(ASD.data$et_figurestask_dwell_time_not_interacting); which(ASD.data$et_figurestask_dwell_time_not_interacting==2432) 




# All together
everything <- ASD.data[,which(names(ASD.data) %in% social.cognition.domain | names(ASD.data) %in% language.domain | names(ASD.data) %in% executive.functions.domain)]
corrplot(cor(everything, use="pairwise.complete.obs"))




# Other plots/tables for specific comparisons/variables that may be of interest included here but commented out:
# plot(ASD.data$tomi_compmean, ASD.data$tom_tb_totalscore)
# plot(ASD.data$bpvs_raw, ASD.data$vocabprocess_processing_speed_target)
# plot(ASD.data$bilec_english_input, ASD.data$vocabprocess_processing_speed_target)
# plot(ASD.data$bilec_english_input, ASD.data$bpvs_raw)
# hist(ASD.data$et_falsebelief_Testtrial_dwell_time_to_correct+ASD.data$et_falsebelief_testtrial_dwell_time_to_incorrect)
# table(ASD.data$et_falsebelief_testtrial_dwell_time_to_incorrect)
# plot(ASD.data$et_falsebelief_Testtrial_dwell_time_to_correct, ASD.data$et_falsebelief_testtrial_dwell_time_to_incorrect)
# plot(ASD.data$brief_raw_inhibit, ASD.data$brief_raw_self.monitor)
# plot(ASD.data$flanker_percenterrors_congruent, ASD.data$flanker_percenterrors_incongruent)
# hist(ASD.data$flanker_percenterrors_congruent-ASD.data$flanker_percenterrors_incongruent)









######################################################### CFA  ######################################################### 


# 1. INSPECT HISTOGRAMS, REMOVE OUTLIERS
CFA.vars <- c(social.cognition.domain, language.domain, executive.functions.domain)
CFA.data <- ASD.data[,which(names(ASD.data) %in% CFA.vars)]
plotdata <- gather(CFA.data[,1:15])
plotdata$colour <- rep(c(rep("Neither",2), "3", rep("Neither", 74), "78", rep("Neither", 11)), 15)
ggplot(plotdata, aes(value)) + # Inspect histograms for each: https://stackoverflow.com/questions/35372365/how-do-i-generate-a-histogram-for-each-column-of-my-table
  geom_histogram(bins = 20, aes(fill=colour)) + 
  facet_wrap(~key, scales = 'free_x')
plotdata <- gather(CFA.data[,16:32])
plotdata$colour <- rep(c(rep("Neither",2), "3", rep("Neither", 74), "78", rep("Neither", 11)), 17)
ggplot(plotdata, aes(value)) + 
  geom_histogram(bins = 20, aes(fill=colour)) + 
  facet_wrap(~key, scales = 'free_x')
ASD.plotting[c(3, 78),]
CFA.data <- ASD.data
CFA.data <- CFA.data[-c(3, 78),] # Take out number three as they skew the data too much


# 2. MAKE DATA NORMAL: Box cox all commented out as ultimately not used, but code here for completeness.

# BRIEF_RAW_INTIIATE
hist(CFA.data$brief_raw_initiate, breaks=10)
hist(log(CFA.data$brief_raw_initiate), breaks=10)
# y = CFA.data$brief_raw_initiate
# result = boxcox(y~1, lambda = seq(-5,5,0.5))
# mylambda = result$x[which.max(result$y)]
# y2 = (y^mylambda-1)/mylambda
# hist(y2, breaks=10)
CFA.data$brief_raw_initiate.normalised <- log(CFA.data$brief_raw_initiate)

# BRIEF_RAW_SELF MONITOR
hist(CFA.data$brief_raw_self.monitor, breaks=10)
hist(log(CFA.data$brief_raw_self.monitor[-3]), breaks=10)
# y = CFA.data$brief_raw_self.monitor
# result = boxcox(y~1, lambda = seq(-5,5,0.5))
# mylambda = result$x[which.max(result$y)]
# y2 = (y^mylambda-1)/mylambda
# hist(y2, breaks=10)
CFA.data$brief_raw_self.monitor.normalised <- log(CFA.data$brief_raw_self.monitor)


# BRIEF_RAW_SHIFT
hist(CFA.data$brief_raw_shift, breaks=10)
hist(log(CFA.data$brief_raw_shift), breaks=10)
# y = CFA.data$brief_raw_shift
# result = boxcox(y~1, lambda = seq(-5,5,0.5))
# mylambda = result$x[which.max(result$y)]
# y2 = (y^mylambda-1)/mylambda
# hist(y2, breaks=10)
CFA.data$brief_raw_shift.normalised <- log(CFA.data$brief_raw_shift)

# BRIEF_RAW_EMOTIONAL CONTROL
hist(CFA.data$brief_raw_emotional_control, breaks=10)
hist(log(CFA.data$brief_raw_emotional_control), breaks=10)
# y = CFA.data$brief_raw_emotional_control
# result = boxcox(y~1, lambda = seq(-5,5,0.5))
# mylambda = result$x[which.max(result$y)]
# y2 = (y^mylambda-1)/mylambda
# hist(y2, breaks=10)
CFA.data$brief_raw_emotional_control.normalised <- log(CFA.data$brief_raw_emotional_control)


# BRIEF RAW INHIBIT
hist(CFA.data$brief_raw_inhibit, breaks=10)
hist(log(CFA.data$brief_raw_inhibit), breaks=10)
# y = CFA.data$brief_raw_inhibit
# result = boxcox(y~1, lambda = seq(-5,5,0.5))
# mylambda = result$x[which.max(result$y)]
# y2 = (y^mylambda-1)/mylambda
# hist(y2, breaks=10)
CFA.data$brief_raw_inhibit.normalised <- log(CFA.data$brief_raw_inhibit)


# BRIEF RAW WOKRING MEMORY
hist(CFA.data$brief_raw_working_memory, breaks=10)
hist(log(CFA.data$brief_raw_working_memory), breaks=10)
# y = CFA.data$brief_raw_working_memory
# result = boxcox(y~1, lambda = seq(-5,5,0.5))
# mylambda = result$x[which.max(result$y)]
# y2 = (y^mylambda-1)/mylambda
# hist(y2, breaks=10)
CFA.data$brief_raw_working_memory.normalised <- log(CFA.data$brief_raw_working_memory)


# BRIEF RAW PLAN ORGANISE
hist(CFA.data$brief_raw_plan_organise, breaks=10)
hist(log(CFA.data$brief_raw_plan_organise), breaks=10)
# y = CFA.data$brief_raw_plan_organise
# result = boxcox(y~1, lambda = seq(-5,5,0.5))
# mylambda = result$x[which.max(result$y)]
# y2 = (y^mylambda-1)/mylambda
# hist(y2, breaks=10)
CFA.data$brief_raw_plan_organise.normalised <- log(CFA.data$brief_raw_plan_organise)


# BRIEF RAW TASK MONITOR
hist(CFA.data$brief_raw_task_monitor, breaks=10)
hist(log(CFA.data$brief_raw_task_monitor), breaks=10)
# y = CFA.data$brief_raw_task_monitor
# result = boxcox(y~1, lambda = seq(-5,5,0.5))
# mylambda = result$x[which.max(result$y)]
# y2 = (y^mylambda-1)/mylambda
# hist(y2, breaks=10)
CFA.data$brief_raw_task_monitor.normalised <- log(CFA.data$brief_raw_task_monitor)



# BRIEF RAW ORGANISATION OF MATERIALS
hist(CFA.data$brief_raw_organisation_of_materials, breaks=10)
hist(log(CFA.data$brief_raw_organisation_of_materials), breaks=10)
# y = CFA.data$brief_raw_organisation_of_materials
# result = boxcox(y~1, lambda = seq(-5,5,0.5))
# mylambda = result$x[which.max(result$y)]
# y2 = (y^mylambda-1)/mylambda
# hist(y2, breaks=10)
CFA.data$brief_raw_organisation_of_materials.normalised <- log(CFA.data$brief_raw_organisation_of_materials)


# FLANKER_PERCENTERRORS: high number of zero counts so struggle to do anything. omit. (otherwise almost two separate sets of data)
hist(CFA.data$flanker_percenterrors, breaks=20)
hist(CFA.data$flanker_percenterrors^(1/3), breaks=20)
hist(log(CFA.data$flanker_percenterrors+0.001), breaks=20)

# PVT FALSE STARTS: high number of zero counts so struggle to do anything. omit.
hist(CFA.data$pvt_count_falsestarts, breaks=20)
hist(log(CFA.data$pvt_count_falsestarts), breaks=10)


# VOCAB PROCESSING SPEED: boxcox doesn't do anything (optimal lamda leads to tiny values), omit.
hist(CFA.data$vocabprocess_processing_speed_target, breaks=20)
hist(log(CFA.data$vocabprocess_processing_speed_target), breaks=20)
hist(sqrt(CFA.data$vocabprocess_processing_speed_target), breaks=20)
hist((CFA.data$vocabprocess_processing_speed_target)^(1/10), breaks=20)
# y = CFA.data$vocabprocess_processing_speed_target
# result = boxcox(y~1, lambda = seq(-50,50,0.5))
# mylambda = result$x[which.max(result$y)]
# y2 = (y^mylambda-1)/mylambda
# hist(y2, breaks=20)


# BPVS: already reasonably normal. don't do anything for simplicity.
hist(CFA.data$bpvs_raw, breaks=20)
# Seemingly too low for observation 79 (0) input with missing instead
CFA.data[79,]$bpvs_raw <- NA


table(rowSums(is.na(CFA.data[,-c(5)]))) # which rows have missing data (do not include SCQ)


# 3. CONFIRMATORY FACTOR ANALYSIS

# Standardise data (should help with negative variance estimations)
scaled.data <- CFA.data %>% mutate_if(is.numeric, scale)


# 1. False belief trial included in language
CFA.model.reduced.variables <- 'language =~ bpvs_raw + vocabprocess_processing_speed_target + et_falsebelief_testtrial_preference_score
              social.cognition  =~ et_figurestask_dwell_time_preference + tomi_compmean + tom_tb_totalscore + et_falsebelief_testtrial_preference_score
              executive.function   =~ brief_raw_inhibit.normalised + brief_raw_self.monitor.normalised + brief_raw_shift.normalised + brief_raw_emotional_control.normalised  + brief_raw_initiate.normalised + brief_raw_working_memory.normalised + brief_raw_plan_organise.normalised + brief_raw_task_monitor.normalised + brief_raw_organisation_of_materials.normalised +  flanker_percenterrors   + pvt_count_falsestarts
              bpvs_raw ~~ 0*bpvs_raw'
fit.reduced.vars <- cfa(CFA.model.reduced.variables, data=scaled.data, missing="fiml", std.lv=TRUE)
varTable(fit.reduced.vars)
summary(fit.reduced.vars, fit.measures = TRUE, standardized=TRUE)




# 2. False belief trial NOT included in language
CFA.model.reduced.variables.2itemlang <- 'language =~ bpvs_raw + vocabprocess_processing_speed_target
              social.cognition  =~ et_figurestask_dwell_time_preference + tomi_compmean + tom_tb_totalscore + et_falsebelief_testtrial_preference_score
              executive.function   =~ brief_raw_inhibit.normalised + brief_raw_self.monitor.normalised + brief_raw_shift.normalised + brief_raw_emotional_control.normalised  + brief_raw_initiate.normalised + brief_raw_working_memory.normalised + brief_raw_plan_organise.normalised + brief_raw_task_monitor.normalised + brief_raw_organisation_of_materials.normalised +  flanker_percenterrors   + pvt_count_falsestarts
              bpvs_raw ~~ 0*bpvs_raw'
fit.reduced.vars.2itemlang <- cfa(CFA.model.reduced.variables.2itemlang, data=scaled.data, missing="fiml", std.lv=TRUE)
varTable(fit.reduced.vars.2itemlang)
lavInspect(fit.reduced.vars.2itemlang, "cor.lv")
summary(fit.reduced.vars.2itemlang, fit.measures = TRUE)
fitted(fit.reduced.vars.2itemlang)$cov


anova(fit.reduced.vars, fit.reduced.vars.2itemlang)
# No significant difference in fit. Use the one with 2 items (less complex and more aligned with theory)




# 3. Behavioural Regulation (Inhibit, shift and emotional control) and 
#    Metacognition (Initiate, Working Memory, Plan/Organise, Organisation of Materials, Monitor). 
# Flanker is described as an inhibition task (Executive function and metacognition: Towards a unifying framework of cognitive self-regulation)
# PVT is behavioural alertness which could fall into either
CFA.model.reduced.variables.2execfunc <- 'language =~ bpvs_raw + vocabprocess_processing_speed_target
              social.cognition  =~ et_figurestask_dwell_time_preference + tomi_compmean + tom_tb_totalscore + et_falsebelief_testtrial_preference_score
              executive.function.behavioural   =~ brief_raw_inhibit.normalised  + brief_raw_shift.normalised + brief_raw_emotional_control.normalised  +  flanker_percenterrors + pvt_count_falsestarts
              executive.function.metacog =~ brief_raw_self.monitor.normalised  + brief_raw_initiate.normalised + brief_raw_working_memory.normalised + brief_raw_plan_organise.normalised + brief_raw_task_monitor.normalised + brief_raw_organisation_of_materials.normalised + pvt_count_falsestarts          
              bpvs_raw ~~ 0*bpvs_raw'
fit.reduced.vars.2execfunc <- cfa(CFA.model.reduced.variables.2execfunc, data=scaled.data, missing="fiml")
varTable(fit.reduced.vars.2execfunc)
lavInspect(fit.reduced.vars.2execfunc, "cor.lv")
summary(fit.reduced.vars.2execfunc, fit.measures = TRUE, standardized=TRUE)
# Can't do ANOVA as not nested
# Comparative Fit Index (CFI)   0.876 compared to 0.869 for model 2.


# exec function is a measure of how poor the executive functioning is.


ggplot(CFA.data, aes(x=pvt_mean_lapse_rt, y=brief_raw_organisation_of_materials.normalised)) + geom_point()








# Preditions of latent factors
CFA.data <- cbind(CFA.data, lavPredict(fit.reduced.vars.2itemlang, type = "lv"))

CFA.data[which(CFA.data$diagnosis==1),]$diagnosis <- "ASD"
CFA.data[which(CFA.data$diagnosis==0),]$diagnosis <- "NT"
CFA.data$diagnosis <- factor(CFA.data$diagnosis, level=c("NT", "ASD"))



# Effect of bilingualism on these factors
lang.lm.1 <- lm(language~bilec_total_input*diagnosis + age_m*wasi_sum_rawscores, data=CFA.data)
lang.lm.2 <- lm(language~bilec_total_input*diagnosis + age_m+wasi_sum_rawscores, data=CFA.data)
lang.lm.3 <- lm(language~bilec_total_input*diagnosis*age_m+wasi_sum_rawscores, data=CFA.data)
AIC(lang.lm.1); AIC(lang.lm.2); AIC(lang.lm.3) # Non-interation term better
summary(lang.lm.2)

soc.cog.lm.1 <- lm(social.cognition~bilec_total_input*diagnosis + age_m*wasi_sum_rawscores, data=CFA.data)
soc.cog.lm.2 <- lm(social.cognition~bilec_total_input*diagnosis + age_m+wasi_sum_rawscores, data=CFA.data)
soc.cog.lm.3 <- lm(social.cognition~bilec_total_input*diagnosis*age_m+wasi_sum_rawscores, data=CFA.data)
AIC(soc.cog.lm.1); AIC(soc.cog.lm.2); AIC(soc.cog.lm.3) # Non-interation term better
summary(soc.cog.lm.2)

exec.func.lm.1 <- lm(-executive.function~bilec_total_input*diagnosis + age_m*wasi_sum_rawscores, data=CFA.data)
exec.func.lm.2 <- lm(-executive.function~bilec_total_input*diagnosis + age_m + wasi_sum_rawscores, data=CFA.data)
exec.func.lm.3 <- lm(-executive.function~bilec_total_input*diagnosis*age_m + wasi_sum_rawscores, data=CFA.data)
AIC(exec.func.lm.1); AIC(exec.func.lm.2); AIC(exec.func.lm.3) # Non-interation term better
summary(exec.func.lm.2)
