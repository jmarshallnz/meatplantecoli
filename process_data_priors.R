library(readxl)

dat <- read_excel("data/Five Expert Opinions - RA for process modelling.xlsx", sheet = 1, skip=4)
# rearrange data into some sort of meaningful form.

# Remove all empty columns
wch <- 1:max(which(names(dat) != ""))
dat = dat[,wch]

# Remove all empty rows
dat = dat[rowSums(is.na(dat)) < ncol(dat),]

# Copy down column 1
vals <- which(!is.na(dat[,1]))
copy_start <- vals+1
copy_end   <- c(vals[-1]-1, nrow(dat))
for (i in seq_along(vals)) 
  dat[copy_start[i]:copy_end[i],1] <- dat[vals[i],1]
dat = dat[-vals,]

# the other datasheet didn't have separate unloading ramps/pens
ramps <- c(FALSE, dat[-1,1] == "Unloading Ramps & Pens")
dat[ramps,1] = "Plant details"

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
wch = 3:ncol(dat)
wch = wch[colSums(is.na(dat[,wch])) != nrow(dat)]
plants = dat[,wch]

# remove some spaces at end of values
library(stringr)
plants = apply(plants, 2, function(x) { str_replace_all(x, " ", "") })

plants[plants == "na"] = NA

library(dplyr)
final = data.frame(plants[,c(5,1:4)], stringsAsFactors = FALSE)
names(final) <- c("Variable", "LikelyIncrease", "LikelyDecrease", "NoEffect", "Unknown")
final[,2:5] <- lapply(final[,2:5], function(x) { as.numeric(as.character(x)) })
rowSums(final[,2:5], na.rm=TRUE)

#redo the variables as needed
final$Variable <- str_replace_all(final$Variable, "[^a-zA-Z0-9]",".")

# Fixups
final$Variable <- str_replace(final$Variable, "NotShortenedBeforeHidePuller", "LeftUnshortenedBeforeHidePuller")
final$Variable <- str_replace(final$Variable, "MethodUsedI.e.Knife.AirknifeOrRoller", "MethodUsedI.e.FlayingByKnifeOrAirknifeOrByRoller")
final$Variable <- str_replace(final$Variable, "PizzleOccludedBeforeHidePuller", "PizzleOccludedBeforeHidePulle")
final$Variable <- str_replace(final$Variable, "WaterContact.CabinetWashPriorToChiller", "WaterContact.CabinetWashPriorToChiller.BR")
final$Variable <- str_replace(final$Variable, "Flaying.OperatorFollowingCarcass", "Flaying.OperatorFollowingCarcass.StationaryOrBoth")
final$Variable <- str_replace(final$Variable, "OpeningUp.TypeOfOpeningCutUsed", "OpeningUp..7.OpeningCutUsed")

# Likely variables are combined (merged cells FTW???!!?), so combine them
final <- final %>% mutate(Effect = rowSums(cbind(LikelyIncrease,LikelyDecrease),na.rm=TRUE)) %>% select(-LikelyIncrease,-LikelyDecrease)

# get rid of NAs
final[is.na(final)] <- 0

# write file
write.csv(final, "data/final_prior_data.csv", row.names = FALSE)


# test we have all the variables we want
dat <- read_csv("data/final_data.csv")


missing1 <- !names(dat) %in% final$Variable
missing2 <- !final$Variable %in% names(dat)
names(dat)[missing1]
final$Variable[missing2]


