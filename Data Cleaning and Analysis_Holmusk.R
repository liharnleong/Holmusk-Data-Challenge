##########################################################
#### Analysis of Healthcare Data and Healthcare Costs ####
##########################################################

################################
# Part 1: Cleaning and Merging #
################################

### Load packages and raw data ###
library(dplyr)

bill_amount <- read.csv("C:/Users/Li Harn/Desktop/Healthcare Data Challenge Data/bill_amount.csv")
bill_id <- read.csv("C:/Users/Li Harn/Desktop/Healthcare Data Challenge Data/bill_id.csv")
clinical_data <- read.csv("C:/Users/Li Harn/Desktop/Healthcare Data Challenge Data/clinical_data.csv")
demographics <- read.csv("C:/Users/Li Harn/Desktop/Healthcare Data Challenge Data/demographics.csv")

### Trimming function ###
trim<-function(x) gsub("^//s+|//s+$", "", x)

### Cleaning bill_amount ###
class(bill_amount$bill_id)
class(bill_amount$amount) # No cleaning needed

### Cleaning bill_id ###
class(bill_id$date_of_admission)
bill_id <- bill_id %>% mutate(date_of_admission = as.Date(date_of_admission, format='%Y-%m-%d'))
class(bill_id$patient_id) # No further cleaning needed

### Cleaning clinical_data ###
table(clinical_data$medical_history_1)
table(clinical_data$medical_history_2)
table(clinical_data$medical_history_3)
table(clinical_data$medical_history_4)
table(clinical_data$medical_history_5)
table(clinical_data$medical_history_6)
table(clinical_data$medical_history_7)
table(clinical_data$preop_medication_1)
table(clinical_data$preop_medication_2)
table(clinical_data$preop_medication_3)
table(clinical_data$preop_medication_4)
table(clinical_data$preop_medication_5)
table(clinical_data$preop_medication_6)
table(clinical_data$symptom_1)
table(clinical_data$symptom_2)
table(clinical_data$symptom_3)
table(clinical_data$symptom_4)
table(clinical_data$symptom_5)
class(clinical_data$lab_result_1)
class(clinical_data$lab_result_2)
class(clinical_data$lab_result_3)
class(clinical_data$weight)
class(clinical_data$height)

clinical_data <- clinical_data %>% mutate(date_of_admission = as.Date(date_of_admission, format='%Y-%m-%d'),
                                          date_of_discharge = as.Date(date_of_discharge, format='%Y-%m-%d'),
                                          medical_history_2 = ifelse(is.na(medical_history_2), 0, medical_history_2),
                                          medical_history_3 = trim(medical_history_3),
                                          medical_history_3 = ifelse(medical_history_3 == "No", 0,
                                                                     ifelse(medical_history_3 == "Yes", 1, medical_history_3)),
                                          medical_history_3 = as.numeric(medical_history_3),
                                          medical_history_5 = ifelse(is.na(medical_history_5), 0, medical_history_5),
                                          ) %>% rename('patient_id' = 'id') # No further cleaning needed

### Cleaning demographics ###
table(demographics$race)
table(demographics$gender)
table(demographics$resident_status)

demographics <- demographics %>% mutate(gender = ifelse(gender == 'f', 'Female',
                                                        ifelse(gender == 'm', 'Male', gender)),
                                        race = ifelse(race == 'chinese', 'Chinese',
                                                      ifelse(race == 'India', 'Indian', race)),
                                        resident_status = ifelse(resident_status == 'Singapore citizen', 'Singaporean', resident_status),
                                        date_of_birth = as.Date(date_of_birth, format='%Y-%m-%d')) # No further cleaning needed

### Merge done separately for own clarification ###
bill_merge <- merge(bill_amount, bill_id)
patient_merge <- merge(clinical_data, demographics, by = 'patient_id', all = FALSE)

### Overall merge of cleaned data ###
raw_merge <- merge(bill_merge, patient_merge)
write.csv(raw_merge, file = "C:/Users/Li Harn/Desktop/Healthcare Data Challenge Data/raw_merge.csv", row.names = FALSE)

### Addition of variables, and sum costs per patient (Adding up individual bills) ###
# Removed bill id column as its unnecessary
test.data <- raw_merge %>% select(, -3) %>% group_by(patient_id) %>% mutate(N_visits = n_distinct(date_of_admission)) %>% group_by(date_of_admission) %>% mutate(admission_duration = date_of_discharge - date_of_admission,
                                                                                                                                                                 amount = sum(amount)) %>% rename("cost" = "amount")

test.data <- distinct(test.data)

# Rearrange columns for easy viewing
col.order <- c("patient_id", "date_of_admission", "date_of_discharge", "admission_duration", "N_visits", "cost",
               "medical_history_1", "medical_history_2", "medical_history_3", "medical_history_4",
               "medical_history_5", "medical_history_6", "medical_history_7", "preop_medication_1", "preop_medication_2",
               "preop_medication_3", "preop_medication_4", "preop_medication_5", "preop_medication_6", "symptom_1",
               "symptom_2", "symptom_3", "symptom_4", "symptom_5", "lab_result_1", "lab_result_2", "lab_result_3",
               "weight", "height", "gender", "race", "resident_status", "date_of_birth")

test.data <- test.data[, col.order]
write.csv(test.data, file = "C:/Users/Li Harn/Desktop/Healthcare Data Challenge Data/test.data.csv", row.names = FALSE)


####################
# Part 2: Analysis #
####################
library(car)

### Summary statistics of combined aggregate cost data ###
table(test.data$N_visits)
table(test.data$admission_duration)
summary(test.data$cost)

# Compare simple average within binary variables
t.test(test.data$cost ~ test.data$medical_history_1)
t.test(test.data$cost ~ test.data$medical_history_2)
t.test(test.data$cost ~ test.data$medical_history_3)
t.test(test.data$cost ~ test.data$medical_history_4)
t.test(test.data$cost ~ test.data$medical_history_5)
t.test(test.data$cost ~ test.data$medical_history_6)
t.test(test.data$cost ~ test.data$medical_history_7)
t.test(test.data$cost ~ test.data$preop_medication_1)
t.test(test.data$cost ~ test.data$preop_medication_2)
t.test(test.data$cost ~ test.data$preop_medication_3)
t.test(test.data$cost ~ test.data$preop_medication_4)
t.test(test.data$cost ~ test.data$preop_medication_5)
t.test(test.data$cost ~ test.data$preop_medication_6)
t.test(test.data$cost ~ test.data$symptom_1)
t.test(test.data$cost ~ test.data$symptom_2)
t.test(test.data$cost ~ test.data$symptom_3)
t.test(test.data$cost ~ test.data$symptom_4)
t.test(test.data$cost ~ test.data$symptom_5) # Significance of MH1, SY1, SY2, SY4, SY5

# Test linear relationship with cost
scatterplot(cost ~ lab_result_1, data = test.data, smooth = F, grid = F, frame = F)
scatterplot(cost ~ lab_result_2, data = test.data, smooth = F, grid = F, frame = F)
scatterplot(cost ~ lab_result_3, data = test.data, smooth = F, grid = F, frame = F)
scatterplot(cost ~ admission_duration, data = test.data, smooth = F, grid = F, frame = F) # Not much significant relationship
summary(lm(cost ~ lab_result_1, data = test.data))
summary(lm(cost ~ lab_result_2, data = test.data))
summary(lm(cost ~ lab_result_3, data = test.data))
summary(lm(cost ~ admission_duration, data = test.data))

# Testing between significant variables

# Find correlations?
cor(test.data[,unlist(lapply(test.data, is.numeric))]) # Most are binary variables, doesn't seem to work well so do not use
