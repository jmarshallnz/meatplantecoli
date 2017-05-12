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
final = data.frame(plants[,c(5,1:4)])
names(final) <- c("Variable", "Likely", "Neutral", "Unlikely", "Unknown")
final[,2:5] <- lapply(final[,2:5], function(x) { as.numeric(as.character(x)) })
rowSums(final[,2:5], na.rm=TRUE)

# write file
write.csv(final, "data/final_prior_data.csv", row.names = FALSE)
