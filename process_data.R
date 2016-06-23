library(readxl)

dat <- read_excel("data/Five Expert Opinions - Combo RA for process modelling.xlsx", sheet = 1, skip=4)

# rearrange data into some sort of meaningful form.

# Remove all empty columns
dat = dat[,1:max(which(names(dat) != ""))]

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
plants = dat[,3:22]
plants = plants[,colSums(is.na(plants)) != nrow(plants)]

# remove some spaces at end of values
library(stringr)
plants = apply(plants, 2, function(x) { str_replace_all(x, " ", "") })

colnames(plants) = paste('Plant', 1:ncol(plants))
rownames(plants) = dat$Variable
plants[plants == "na"] = NA

final = data.frame(t(plants))
summary(final)

