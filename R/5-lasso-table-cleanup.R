itt <- read.table("tables/lasso/ITT/lasso-coefs.txt", 
  header = FALSE,
  sep    = "&",
  skip   = 1,
  as.is  = TRUE)

com <- read.table("tables/lasso/compliant/lasso-coefs.txt", 
  header = FALSE,
  sep    = "&",
  skip   = 1,
  as.is  = TRUE)

colnames(itt) <- c("var", "coef", "con", "itt")
colnames(com) <- c("var", "coef", "con", "com")

# itt$var <- trimws(itt$var, "r")
# com$var <- trimws(com$var, "r")

itt[] <- lapply(itt, trimws)
com[] <- lapply(com, trimws)

subset(itt, coef == "lnnal_visit0")
subset(com, coef == "lnnal_visit0")


com$con <- NULL

library(dplyr)

both <- full_join(itt, com, by = c("var", "coef"))


naTodash <- function(x) {
  x[is.na(x)] <- "-"
  x
}

both[] <- lapply(both, naTodash)
both <- arrange(both, var)

# generate desired ordering
both$ord <- seq_len(nrow(both))
var_ord <- c(
  "total_cpd_20",   # Total CPD	
  "cesd_20",        # CESD	
  "weight_gain",    # Weight Gain	
  "co_20",          # CO	
  "ltne_20",        # TNE	
  "lnnal_visit20",  # NNAL	
  "lphet_visit20",  # PheT	
  "lcema_visit20", # CEMA	
  "lpgem_visit20",  # PGEM	
  "liso_visit20"    # 8-iso-〖PGF〗_2α
) 

both <- both %>%
  mutate(var_ord = match(var, var_ord))
both <- both %>%
  arrange(var_ord, ord)
both[c('var_ord', 'ord')] <- NULL


write.table(both, 
  sep       = " & ",
  row.names = FALSE,
  quote     = FALSE)
