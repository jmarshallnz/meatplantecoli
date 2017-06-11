library(dplyr)
library(ggplot2)

# Simplify down the dataset
lasso <- read.csv("temp/lasso.csv", stringsAsFactors=FALSE) %>%
  filter(round(alpha*5) == alpha*5, alpha > 0, round(penalty*2.5) == penalty*2.5)

head(lasso)

lasso_group <- lasso %>%
  group_by(Coefficient) %>%
  summarize(coefNonZero = sum(value == 0)/n())

lasso <- lasso %>% left_join(lasso_group) %>% filter(coefNonZero < 1)
write.csv(lasso, "temp/lasso_coef.csv", row.names=FALSE)

# Generate the variable map
sub_len <- function(x) {
  x <- as.character(x)
  unlist(lapply(x, function(x) {
    i <- 1
    while(i < nchar(x)) {
      match <- which(variable_table$VarName == substring(x,1,nchar(x)-i))
      if (length(match) > 0)
        break
      i <- i + 1
    }
    i
  }))
}

var_map <- lasso_group %>% filter(Coefficient != '(Intercept)') %>% 
  mutate(SubLength = sub_len(Coefficient),
         VarName = substring(Coefficient, 1, nchar(Coefficient)-SubLength),
         Value=substring(Coefficient, nchar(Coefficient)-SubLength+1)) %>%
  left_join(variable_table, by=c("VarName")) %>% select(Coefficient,VarName,Category,Variable,Value)
write.csv(var_map, "temp/variable_names.csv", row.names=FALSE)



# NOW DO SOME OTHER SHIT

lasso <- read.csv("temp/lasso_coef.csv")
lasso_group <- lasso %>% select(Coefficient, coefNonZero) %>% unique

# grab the important ones
top <- lasso_group %>% filter(Coefficient != '(Intercept)') %>% arrange(coefNonZero)

# plot lambda vs coef for alpha etc.
coef_lass <- lasso %>% filter(Coefficient %in% top$Coefficient[1:4]) %>% droplevels

prior_labeller <- function(x) {
  x <- ifelse(x == "0", "No expert reliance", ifelse(x == "2", "Strong expert reliance", ""))
}

lasso_labeller <- function(x) {
  x <- ifelse(x == "0.2", "Ridge", ifelse(x == "1", "LASSO", ""))
}

ggplot(coef_lass) +
  geom_line(aes(x=-log10(lambda), y=exp(value), colour = Coefficient)) +
  facet_grid(alpha ~ penalty, labeller=labeller(penalty=as_labeller(prior_labeller),
                                                alpha=as_labeller(lasso_labeller))) +
  theme(legend.position='bottom') +
  ylab("Odds ratio for prevalence of STEC") +
  xlab("LASSO/Ridge penalty") +
  scale_x_continuous(breaks=NULL) + 
  scale_y_log10(breaks=c(0.5,1,2), limits=c(0.25,4)) +
  guides(colour=guide_legend(title=NULL,nrow=2,byrow=TRUE)) + 
  ggtitle("Effect size of process variables")
