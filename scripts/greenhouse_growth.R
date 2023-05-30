
# Read in greenhouse data

dat <- read.csv('./data/canbys_data - greenhouse.csv') 

head(dat)

summary(dat)

class(dat$tray)
class(dat$status)

# compute growth between 05/25 and 05/15
gr_25 <- with(dat, len_0525 - len_0515)
summary(gr_25)
hist(gr_25)

dat[which(gr_25 > 7), ]

# compute growth between 05/15 and 05/09
gr_15 <- with(dat, len_0515 - len_0509)
summary(gr_15)
hist(gr_15)

# examine rows in which gr15 growth was negative
dat[which(gr_15 < 0), ]
dat[which(gr_15 > 10), ]


boxplot(gr_25 ~ dat$tray)


summary(lm(gr_25 ~ as.factor(dat$tray)))
car::Anova(lm(gr_25 ~ as.factor(dat$tray)))

summary(lm(len_0525 ~ as.factor(tray), data = dat))

dat$gr_25 <- gr_25

plot(gr_25 ~ len_0525, data = dat)
lines(lowess(dat$len_0525[!is.na(dat$len_0525)], gr_25[!is.na(dat$len_0525)]),
      col='red', lwd =2)
abline(lm(gr_25 ~ len_0525, data = dat), col='blue', lwd =2)
summary(lm(gr_25 ~ len_0525, data = dat))
points(gr_25 ~ len_0525, data = dat, subset = tray == 1, col='green3', pch = 19)
points(gr_25 ~ len_0525, data = dat, subset = tray == 2, col='purple3', pch = 19)
points(gr_25 ~ len_0525, data = dat, subset = tray == 3, col='dodgerblue', pch =19)







