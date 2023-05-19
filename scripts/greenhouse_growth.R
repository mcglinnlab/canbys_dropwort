
# Read in greenhouse data

dat <- read.csv('./data/canbys_data - greenhouse.csv') 

head(dat)

summary(dat)

class(10)
class("10")
class(dat$tray)
class(dat$status)
table(dat$status)

table(dat$date)

dat_09 <- subset(dat, date == '05/09/2023')

hist(dat_09$ht_cm)
boxplot(ht_cm ~ tray, data = dat_09)
boxplot(ht_cm ~ date, data = dat)

t.test(ht_cm ~ date, data = dat)

# no signal at population scale

#let's look at individual level variation
# drop any double measurements from single cell 
head(dat)

uni_id <- with(dat, 
               paste(date, tray, column, row, sep='-'))
duplicated(uni_id)
dat_sub <- subset(dat, !duplicated(uni_id))
# don't include date this time so that they can be compared
uni_id <- with(dat_sub, 
               paste(tray, column, row, sep='-'))
table(uni_id)
well_id <- unique(uni_id)

ht_diff <- NULL
for (i in seq_along(well_id)) {
  ht_diff <- c(ht_diff, 
               diff(dat_sub$ht_cm[uni_id == well_id[i]]))
}
ht_diff
hist(ht_diff)

well_id

tray <- sapply(strsplit(well_id,'-'), function(x)x[[1]])

boxplot(ht_diff ~ tray)


y <- rnorm(100)
x <- replicate(50, rnorm(100))

dim(x)
length(y)

summary(lm(y ~ x))




















