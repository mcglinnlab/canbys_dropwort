
# Read in greenhouse data

dat <- read.csv('./data/canbys_data - greenhouse.csv') 

head(dat)

summary(dat)

class(dat$tray)
class(dat$status)

any(duplicated(dat$well_id_all))

# compute growth between 05/24 and 05/15
gr_15 <- with(dat, len_0515 - len_0509) / 6
gr_24 <- with(dat, len_0524 - len_0515) / 9
gr_31 <- with(dat, len_0531 - len_0524) / 7
gr_18 <- with(dat, len_0618 - len_0531) / 18

gr <- data.frame(gr_15, gr_24, gr_31, gr_18)
gr_avg <- rowMeans(gr, na.rm =TRUE)

tmp <- data.frame(gr_avg, ht = dat$len_0618)


plot(gr_avg ~ ht, data = tmp, subset = gr_avg > 0 )
summary(lm(gr_avg ~ ht, data = tmp, subset = gr_avg > 0))
sqrt(0.33)

# consider dropping all plants less than 5 cm in ht on 06/18
sum(dat$len_0618 >= 5, na.rm = TRUE)
sum(dat$len_0618 >= 0, na.rm = TRUE)
160/2

# divide plants into 3 size groups
hist(dat$len_0618[dat$len_0618 >= 5])
quantile(dat$len_0618[dat$len_0618 >= 5], na.rm = TRUE, probs = c(0.33, 0.67))
abline(v = c(14.5, 20.5), col='red')

table(cut(dat$len_0618, breaks = c(0, 14, 20.5, 50), include.lowest = TRUE))

tmp <- dat[dat$len_0618 >= 5, ]
gd_wells <- tmp$well_id_all[order(tmp$len_0618)]
gd_wells <- gd_wells[!is.na(gd_wells)]

sm <- gd_wells[(1:52) + 4]
md <- gd_wells[(53+4):(52+52+4)]
lg <- gd_wells[(52*2+1+4):(52*3+4)]

trt <- data.frame(sm = sample(sm), md = sample(md), lg = sample(lg))
trt

write.csv(trt, file = './data/planting_design.csv', row.names = FALSE)

length(gd_wells)


hist(log2(dat$len_0618))
quantile(log2(dat$len_0618), na.rm = TRUE, probs = c(0.25, 0.75))
abline(v = log2(c(14, 20.5)), col='red')
abline(v = c(3.58, 4.49), col='blue')



summary(gr_24)
hist(gr_24)

summary(gr_31)
hist(gr_31)


dat[which(gr_24 > 7), ]
dat[which(gr_31 < -3), ]




# compute growth between 05/15 and 05/09
gr_15 <- with(dat, len_0515 - len_0509)
summary(gr_15)
hist(gr_15)

# examine rows in which gr15 growth was negative
dat[which(gr_15 < 0), ]
dat[which(gr_15 > 10), ]


boxplot(gr_24 ~ dat$tray)


summary(lm(gr_24 ~ as.factor(dat$tray)))
car::Anova(lm(gr_24 ~ as.factor(dat$tray)))

summary(lm(len_0524 ~ as.factor(tray), data = dat))

dat$gr_24 <- gr_24

plot(gr_24 ~ len_0524, data = dat)
lines(lowess(dat$len_0524[!is.na(dat$len_0524)], gr_24[!is.na(dat$len_0524)]),
      col='red', lwd =2)
abline(lm(gr_24 ~ len_0524, data = dat), col='blue', lwd =2)
summary(lm(gr_24 ~ len_0524, data = dat))
points(gr_24 ~ len_0524, data = dat, subset = tray == 1, col='green3', pch = 19)
points(gr_24 ~ len_0524, data = dat, subset = tray == 2, col='purple3', pch = 19)
points(gr_24 ~ len_0524, data = dat, subset = tray == 3, col='dodgerblue', pch =19)


# more growth data












