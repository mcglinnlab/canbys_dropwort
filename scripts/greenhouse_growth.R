
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

# compute growth between 05/15 and 05/09
gr_15 <- with(dat, len_0515 - len_0509)
summary(gr_15)
hist(gr_15)

# examine rows in which gr15 growth was negative
dat[which(gr_15 < 0), ]














