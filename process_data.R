library(readxl)

dat <- read_excel("data/Five Expert Opinions - Combo RA for process modelling.xlsx", sheet = 1, skip=4)
out <- read_excel("data/Ranking by %STEC 2014 abridged  .xlsx", sheet = 1, col_names = FALSE)

# rearrange data into some sort of meaningful form.

# Remove all empty columns
wch <- 1:max(which(names(dat) != ""))
dat = dat[,wch]
out = out[,wch]

# Remove all empty rows
dat = dat[rowSums(is.na(dat)) < ncol(dat),]
out = out[rowSums(is.na(out)) < ncol(out),]

# Copy down column 1
vals <- which(!is.na(dat[,1]))
copy_start <- vals+1
copy_end   <- c(vals[-1]-1, nrow(dat))
for (i in seq_along(vals)) 
  dat[copy_start[i]:copy_end[i],1] <- dat[vals[i],1]
dat = dat[-vals,]
dat$Variable = apply(dat[,1:2], 1, paste, collapse=":")
# Capitalise anything after a space
simplify_var <- function(x) {
  x = x[nchar(x) > 0]
  wch_colon = which(grepl(".*:.*", x))
  if (length(wch_colon) == 0)
    wch_colon = 1
  wch_dash = which(x[wch_colon[1]:length(x)] == "-")+(wch_colon[1]-1)
  if (length(wch_dash) > 0) {
    x = x[1:(wch_dash[1]-1)]
  }
  first_char = toupper(substring(x, 1, 1))
  subsq_char = substring(x, 2, nchar(x))
  paste0(first_char, subsq_char, collapse="")
}
dat$Variable = unlist(lapply(strsplit(dat$Variable, " "), simplify_var))

# remove empty first line
dat = dat[-1,]

# grab out dat we want
wch = 3:22
wch = wch[colSums(is.na(dat[,wch])) != nrow(dat)]
plants = dat[,wch]

plant_out = out[c(5,7,8),wch]
rownames(plant_out) = c("Kill", "Days", "STEC")
colnames(plant_out) = paste0('Plant', 1:ncol(plants))

# remove some spaces at end of values
library(stringr)
plants = apply(plants, 2, function(x) { str_replace_all(x, " ", "") })

colnames(plants) = paste0('Plant', 1:ncol(plants))
rownames(plants) = dat$Variable
plants[plants == "na"] = NA

# Additional processing care-of Bob...
plants[plants == "ME78"] = "N"

final = data.frame(cbind(t(plants), t(plant_out)))
final$Plant = rownames(final)

final$Kill = as.numeric(as.character(final$Kill))
final$Days = as.numeric(as.character(final$Days))
final$STEC = as.numeric(as.character(final$STEC))

library(dplyr)
final <- final %>% mutate(YCutting.ClearingBrisket = ifelse(YCutting.ClearingBrisket != "N", "Y", "N"),
                          YCutting.YCutterFollowingCarcassRatherThanStationary = ifelse(YCutting.YCutterFollowingCarcassRatherThanStationary %in% c("FthenS", "S2/F2"), "F", "S"),
                          AnalBunging.CleaningMethodOfRodUsedToInsertBung = ifelse(AnalBunging.CleaningMethodOfRodUsedToInsertBung %in% c("None", "none"), "N", "Y"),
                          AnalBunging.TypeOfAnalBung = ifelse(AnalBunging.TypeOfAnalBung %in% c("A/Pl", "B", "Pl", "Pl/Sp", "PL/Sp"), "B/Pl", "Other"),
                          NeckOpening.BloodCollection = ifelse(NeckOpening.BloodCollection == "N", "N", "Y"),
                          Restrainer.MethodOfCleaningRestrainer = ifelse(Restrainer.MethodOfCleaningRestrainer %in% c("C", "CS"), "C", "H"))


summary(final)

# write file
write.csv(final, "data/final_data.csv", row.names = FALSE)
