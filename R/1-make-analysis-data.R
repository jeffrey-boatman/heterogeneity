# R command for batch: 
# "C:\Program Files\R\R-3.6.0\bin\R.exe" CMD BATCH --vanilla R\1-make-analysis-data.R

library(tidyverse)

# data ----
p1s1 <- read.csv("../data/cenicp1s1_data_forJeffB_TEH_20190816.csv",
  na.strings = c("NA", "9995", "9999", "99999", "99995"), as.is = TRUE)
# p1s2 <- read.csv("../data/cenicp1s2_data_forJeffB_TEH_20190816.csv",
#   na.strings = c("NA", "9995", "9999", "-9999"), as.is = TRUE) 
p2 <- read.csv("../data/cenicp2_data_forJeffB_20190925.csv", as.is = TRUE)

summary(p1s1)
# summary(p1s2)
summary(p2)

lapply(Filter(is.numeric, p1s1), range, na.rm = TRUE)
lapply(Filter(is.numeric, p2), range, na.rm = TRUE)


# replace names for combining ----
var_map <- read.csv("misc/variable_mapping.csv",
  na.strings = "", as.is = TRUE)

# ~ p1s1 ----
to_replace <- names(p1s1) %in% var_map$p1s1
old_names  <- names(p1s1)[to_replace]
new_names  <- var_map$new_name[match(names(p1s1)[to_replace], 
  var_map$p1s1, nomatch = NULL)] 
#cbind(old_names, new_names)
names(p1s1)[to_replace] <- new_names

# ~ p1s2 ----
# to_replace <- names(p1s2) %in% var_map$p1s2
# old_names  <- names(p1s2)[to_replace]
# new_names  <- var_map$new_name[match(names(p1s2)[to_replace], 
#   var_map$p1s2, nomatch = NULL)] 
# #cbind(old_names, new_names)
# names(p1s2)[to_replace] <- new_names

# ~ p2 ----
to_replace <- names(p2) %in% var_map$p2
old_names  <- names(p2)[to_replace]
new_names  <- var_map$new_name[match(names(p2)[to_replace], 
  var_map$p2, nomatch = NULL)] 
#cbind(old_names, new_names)
names(p2)[to_replace] <- new_names

# data.frame to tibble
p1s1 <- as_tibble(p1s1)
# p1s2 <- as_tibble(p1s2)
p2   <- as_tibble(p2)

# create treatment, control, and study indicators ----
p1s1$study <- "P1S1"
# p1s2$study <- "P1S2"
p2$study   <- "P2"

# ~ p1s1 ----
p1s1 <- p1s1 %>%
  mutate(teh_treatment = treatment_forTEH,
         teh_control   = 1 - teh_treatment)

# p1s1 %>% select(treatment_label, treatment_forTEH, teh_treatment, teh_control)

table(p1s1$teh_treatment, p1s1$treatment_label)
table(p1s1$teh_control, p1s1$treatment_label)



# ~ p1s2 ----
#table(p1s2$treatment, p1s2$treatment_label)
# p1s2 <- p1s2 %>%
#   mutate(teh_treatment = 1 * treatment %in% c("A", "B"),
#          teh_control   = 1 - teh_treatment)
# p1s2 %>% select(treatment, treatment_label, teh_treatment, teh_control)

# ~ p2 ----
# A = gradual, B = immediate, C = control
p2 <- p2 %>%
  mutate(teh_treatment = 1 * (Treatment == "B"),
         teh_control   = 1 * (Treatment == "C"))
p2 <- p2 %>% filter(teh_control + teh_treatment == 1)

# p2 %>% select(Treatment, teh_treatment, teh_control)
table(p2$teh_treatment, p2$Treatment)
table(p2$teh_control, p2$Treatment)


# combine data ----
p1s1 <- rename(p1s1, id = CENIC_subject_id)
# p1s2 <- rename(p1s2, id = CENIC_subject_id)
p2   <- rename(p2, id = studyid)

# ~ making menthol the same ----
# p1s2 <- p1s2 %>% mutate(menthol = 1 * (menthol == "Menthol"))
p2   <- p2   %>% mutate(menthol = 1 * (menthol == "Menthol"))


# ~ making race the same
message("This assumes that P2 1-2-3 is equivalent to 0-1-2 coding of P1S1 and P1S2")
p2$race <- p2$race - 1

# ~ make gender the same ----
#message("This assumes that P2 0-1 coding is equivalent to 1-2 coding of P1")
#p1s1$gender <- p1s1$gender - 1
table(p1s1$gender)
table(p2$gender)
# p1s1$gender: 1 = male, 2 = female
# p2$gender,   1 = male, 0 = female
p2$gender <- ifelse(p2$gender == 0, 2, p2$gender)
# p1s2$gender <- p1s2$gender - 1

# ~ check weight units ----
summary(p2$weight_visit0)
tail(sort(p2$weight_visit0))
tail(sort(p2$weight_visit20))

with(p2, plot(weight_visit20 ~ weight_visit0))

# ~ weight gain ----
p2$weight_gain <- p2$weight_visit20 - p2$weight_visit0

# ~ making race the same ----
# message("This assumes that P2 1-2-3 is equivalent to 0-1-2 coding of P1S1 and P1S2")
# p2$race <- p2$race - 1 # wrong!

message("This code will drop columns from each data set if the column names don't
  appear in the other data sets")
# but be sure to keep week 20 outcomes and week 20 tne in p2

intersect(names(p1s1), names(p2))
setdiff(names(p1s1), names(p2))
setdiff(names(p2), names(p1s1))

p2_uniques <- c("tne_20", "total_cpd_20", "cesd_20", "co_20", "nnal_visit0", 
  "phet_visit0", "cema_visit0", "pgem_visit0", "iso_visit0", "nnal_visit20", 
  "phet_visit20", "cema_visit20", "pgem_visit20", "iso_visit20", "weight_visit0",
  # "weight_visit20", 
  "weight_gain")

inames <- intersect(names(p1s1), names(p2))


p1s1[, setdiff(names(p1s1), inames)] <- NULL

# p1s1 na.omit ----
# p1s1 <- na.omit(p1s1)

p1s1[, p2_uniques] <- NA

p2[, setdiff(names(p2), c(inames, p2_uniques))] <- NULL

# p2 na.omit ----
# p2 <- na.omit(p2)


# ~ label education ----
table(p2$edu)
p2$edu <- factor(p2$edu, 
  labels = c("HSorLess", "HSgrad", "SomeCollegeOrMore"))
table(p2$edu)


table(p1s1$edu)
p1s1$edu <- factor(p1s1$edu)
levels(p1s1$edu) <- c(1, 1, 2, 3, 3, 3)
p1s1$edu <- factor(p1s1$edu, 
  labels = c("HSorLess", "HSgrad", "SomeCollegeOrMore"))
table(p1s1$edu)

with(p1s1, table(edu))
with(p2, table(edu))


# checking. this should equal TRUE
all(sort(names(p1s1)) == sort(names(p2)))

analysis <- rbind(p1s1, p2)


# ~ label race ----
table(analysis$race)
analysis$race <- factor(analysis$race, labels = c("white", "black", "other"))
table(analysis$race)

# ~ label gender ---- 
table(analysis$gender)
analysis$gender <- factor(analysis$gender, labels = c("male", "female"))
with(analysis, table(gender, teh_treatment, study))

# analysis <- na.omit(analysis)

# drop redundant teh_control variable
analysis[, "teh_control"] <- NULL

# save to RData ---- 
save(list = "analysis", file = "../RData/analysis.RData")
