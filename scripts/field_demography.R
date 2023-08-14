#' Author: Dan McGlinn
#' Date: 2023-08-13

library(ggplot2)

#' load data
dat <- read.csv('../data/canbys_data - field.csv')

#' examine data
head(dat)

dat$status.1 <- as.factor(dat$status.1)
with(dat, table(status.1, wetland))

boxplot(ht_cm.1 ~ wetland, data = dat)
t.test(ht_cm.1 ~ wetland, data = dat)


ht_avg <- with(dat, tapply(ht_cm.1, wetland, mean, na.rm = TRUE))
ht_sd <- with(dat, tapply(ht_cm.1, wetland, sd, na.rm = TRUE))
ht_n <- with(dat, tapply(ht_cm.1, wetland, function(x) sum(!is.na(x))))
ht_se <- ht_sd / sqrt(ht_n)

#pdf('./figs/ht_vs_trt.pdf')
ht_plt <- barplot(ht_avg, width = 0.25, ylim = c(0, 25), 
                  ylab = 'Plant height (cm)', col = 'lightgreen')
arrows(ht_plt, ht_avg - (ht_se * 1.96), y1 = ht_avg + (ht_se * 1.96),
       angle = 90, code = 3, length = 0.1, lwd=2)
#dev.off()

gr <- dat$ht_cm.1 - dat$ht_cm
# drop plant that lost ht - this appears to be due to damage
gr <- ifelse(gr < 0, NA, gr)

boxplot(gr ~ wetland, data = dat)
t.test(gr ~ wetland, data = dat)

gr_avg <- with(dat, tapply(gr, wetland, mean, na.rm = TRUE))
gr_sd <- with(dat, tapply(gr, wetland, sd, na.rm = TRUE))
gr_n <- with(dat, tapply(gr, wetland, function(x) sum(!is.na(x))))
gr_se <- gr_sd / sqrt(gr_n)

#pdf('./figs/gr_vs_trt.pdf')
gr_plt <- barplot(gr_avg, width = 0.25, ylim = c(0, 3), 
                  ylab = 'Plant Growth (cm)', col = 'lightgreen')
arrows(gr_plt, gr_avg - (gr_se * 1.96),
       y1 = gr_avg + (gr_se * 1.96),
       angle = 90, code = 3, length = 0.1, lwd = 2)
#dev.off()


wet <- ifelse(dat$water.depth_cm > 0, 'wet', 'dry')
boxplot(gr ~ wet)

gr_mod <- lm(gr ~ wet + wetland, data = dat)
summary(gr_mod)

alive <- ifelse(dat$status.1 == 'd', 0, 1)
alive

#' test if probability of plant being alive is higher
#' in a specific wetland
log_mod <- glm(alive ~ wetland, data = dat, family = binomial)
summary(log_mod)

#' you can see above not quite statistically significant

pseudo_r2 = function(glm_mod) {
  1 -  glm_mod$deviance / glm_mod$null.deviance
}

#' how much variation does the model explain
pseudo_r2(log_mod)

#' not much at all only around 4% of the variation

#' what is the probability of survival in each wetland?
predict(log_mod, 
        newdata = data.frame(wetland = c('unburned', 'burned')),
        type = 'r')


# another way to calculate the above is as follows
logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}

logit2prob(coef(log_mod)[1])
logit2prob(coef(log_mod)[1] + coef(log_mod)[2])


#' 95% confidence interval (CI) for raw coefficients
confint(log_mod)

#' burned wetland 95% CI
logit2prob(2.2617)
logit2prob(4.6512)

#' unburned wetland 95% CI
logit2prob(coef(log_mod)[1] - 2.7005)
logit2prob(coef(log_mod)[1] + 0.08756)


