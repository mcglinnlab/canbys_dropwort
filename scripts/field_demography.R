#' Author: Dan McGlinn
#' Date: 2023-08-13

library(ggplot2)
library(ggdist)
library(patchwork)
library(tidyr)
library(pdp)
library(marginaleffects)

pseudo_r2 <- function(glm_mod) {
  1 -  glm_mod$deviance / glm_mod$null.deviance
}

#' load data
dat <- read.csv('./data/canbys_data - field_long.csv')
green <- read.csv('./data/canbys_data - greenhouse.csv')


# fix loc variable so its numeric
dat$loc[dat$loc == "up1_lft"] <- 0.1 
dat$loc[dat$loc == "up2_rt"] <- 0.2
dat$loc <- as.numeric(dat$loc)


table(dat$date)
# water depth was sporadically recorded at the site so it would be good to 
# take on of the dates with a lot of observations and use it as a spatial predictor
table(dat$date[(!is.na(dat$water_depth_cm))])
# did a good job collecting data on 2023-09-07 and 2024-07-03 on water depth
boxplot(water_depth_cm ~ date, data = dat)
# let's graph those two dates
plot(jitter(dat$water_depth_cm[dat$date=="9/7/2023"]),
     dat$water_depth_cm[dat$date=="7/3/2024"])
abline(a=0, b=1) # one to one
# so it looks like the 2024-07-03 shows a bit more nuance with fewer zeros
# compared to the 2023-09-07 date. 
# for now let's just use the 07-03 depth measurements

dat$water_depth_fixed <- dat$water_depth_cm[dat$date == "7/3/2024"]
dat$water_depth_fixed2 <- dat$water_depth_cm[dat$date == "9/7/2023"]



# merge the datasets
dat <- merge(dat, green[ , c('well_id_all','len_0618')],
             all.x= TRUE, all.y = FALSE,
             by.x = 'plant_id', by.y = 'well_id_all')

#' clean up date a bit
dat$date <- as.Date(dat$date, format = "%m/%d/%Y")
dat$soil_moist <- as.numeric(dat$soil_moist)
dat$dist_cm <- as.numeric(dat$dist_cm)
dat$alive <- ifelse(dat$status == 'a' | dat$status == 'w', 1, 0)
table(dat$alive)


  


#' define growth variable relative to greenhouse start
dat$gr_start <- dat$ht_cm - dat$len_0618

table(dat$status)
summary(dat)

# soil moisture was only recorded once per location on a transect
# find that observation for a particular date and copy it to the other 
# positions at that location

uni_id <- paste(dat$plant_id3, dat$date, sep="--")
table(table(uni_id[which(!is.na(dat$soil_moist))]))
# so it does appear that soil moisture was only recorded once per location 
# per date so these need to be copied

# observations with soil moisture
table(dat$soil_moist)
moist_rows <- which(!is.na(dat$soil_moist))
moist_vals <- dat$soil_moist[moist_rows]
moist_ids <- uni_id[moist_rows]
moist_dates <- dat$date[moist_rows]
moist_wetland <- sapply(strsplit(moist_ids, '-', fixed=TRUE), function(x) x[1])
moist_wetland <- ifelse(moist_wetland == "BUR", "burned", "mowed")
moist_loc <- sapply(strsplit(moist_ids, '-', fixed=TRUE), function(x) x[2])
moist_pos <- sapply(strsplit(moist_ids, '-', fixed=TRUE), function(x) x[3])

for (i in seq_along(moist_ids)) {
   dat$soil_moist[dat$date == moist_dates[i] & 
                  dat$wetland == moist_wetland[i] &
                  dat$loc == moist_loc[i] &
                  dat$pos != moist_pos[i]] <- moist_vals[i]
}

table(dat$soil_moist)


# remove rows that have no meaningful information
dat <- subset(dat, subset = status != '')
dim(dat)

head(dat)

#' graphical exploration of data
hist(dat$water_depth_cm)
hist(dat$soil_moist)
plot(soil_moist ~ water_depth_cm, data = dat)
# so lots of times water depth was zero but there was varying degrees of moisture
# let's compute an average moisture per location across dates if possible
# first let's look if a particular date has more moisture measurements

with(dat, tapply(soil_moist, list(date), function(x) sum(!is.na(x))))

# it looks like on 2023-12-15 we did a pretty good job collecting moisture data
# so let's use that as our metric

dat$soil_moist[dat$date == "2023-12-15"]
dat[is.na(dat$soil_moist) & dat$date == "2023-12-15", ]
# only one location has missing information pos 16 in unburned
# pull out just that date's moisture data
moist_dat <- subset(dat, date == "2023-12-15", 
                    select = c(wetland, loc, pos, soil_moist))
names(moist_dat)[4] <- 'soil_moist_fixed'
head(moist_dat)
# now merge it back into the main dataset to be used in regression analysis

dat <- merge(dat, moist_dat)
head(dat)



# re-examine graphical relationship between water depth and this fixed 
# soil moisture variable
plot(water_depth_cm ~ soil_moist_fixed, data = dat)
# this provides a bit more information now that more values are interpolated
# it appears broadly to be a positive relationship which make sense. 



plot(jitter(water_depth_fixed) ~ soil_moist_fixed, data = dat)

par(mfrow=c(3,2))
plot(soil_moist_fixed ~ loc, data = dat, subset = wetland == 'mowed')
plot(soil_moist_fixed ~ loc, data = dat, subset = wetland == 'burned')
plot(water_depth_fixed ~ loc, data = dat, subset = wetland == 'mowed')
plot(water_depth_fixed ~ loc, data = dat, subset = wetland == 'burned')
plot(soil_moist_fixed ~ water_depth_fixed, data = dat, subset = wetland == 'mowed')
plot(soil_moist_fixed ~ water_depth_fixed, data = dat, sbuset = wetland == 'burned')


# no relationship between soil moisture and water depth which is strange...

plot(water_depth_fixed ~ loc, data = dat, subset = wetland == 'burned')

#' Let's explore some key response variables just for the last time point
#' 2024-07-03. 
#' 
#' plant length exploration
# what about relationship between starting height and ending height?
plot(ht_cm ~ len_0618, data = dat, subset = date == "2024-07-03")
# no relationship above, 
# is length related to soil moisture?
plot(ht_cm ~ soil_moist_fixed, data = dat, subset = date == "2024-07-03")
# yes it does appear that this exists and is positive
# what about management / wetland effect
boxplot(ht_cm ~ wetland, data = dat , subset = date == "2024-07-03")
# the burned is slightly taller than unburned but pretty negligible

# take last survey and look at survival
# compute growth difference between last date and
# first date
dat_last <- subset(dat, date == '2024-07-03')

# plot of plant height through time
boxplot(ht_cm ~ date + wetland, data = dat,
        lex.order = TRUE, col = c("#A9D18E", "#56B4E9"))

tmp <- subset(dat, date != "2023-12-15")
tmp$pdate <- as.Date(ifelse(tmp$wetland == 'burned',
                     tmp$date - 3,
                     tmp$date + 3))
tmp$pdate 
tmp$fdate <- as.factor(tmp$date)
tmp$fdate

ggplot(tmp, aes(pdate, ht_cm)) + 
  geom_point(aes(col = wetland)) + 
  geom_jitter(aes(col = wetland)) + 
  geom_smooth(aes(col = wetland), se = FALSE) + 
  xlab("Date") + 
  ylab("Plant Height (cm)") + 
  scale_color_manual(values=c("#A9D18E", "#56B4E9")) + 
  theme_classic()

ggplot(tmp, aes(fdate, ht_cm)) + 
  geom_boxplot(aes(col = wetland)) + 
  xlab("Date") + 
  ylab("Plant Height (cm)") + 
  scale_color_manual(values=c("#A9D18E", "#56B4E9")) + 
  theme_classic()

### multi-panel plot Fig. 3 ----------------------------------
ht_mod <- lm(ht_cm ~ wetland + water_depth_fixed + len_0618, data = dat_last)
summary(ht_mod)

p1 <- plot_predictions(ht_mod, condition = "water_depth_fixed", points = 1) +
  xlab('Water Depth (cm)') + 
  ylab('Plant Height (cm)') + 
  ylim(0, 160) + 
  theme_minimal() 

p2 <- plot_predictions(ht_mod, condition = "wetland", points = 1) +
  xlab('wetland') + 
  ylab('Plant Height (cm)') + 
  ylim(0, 160) + 
  theme_minimal() 

p3 <- plot_predictions(ht_mod, condition = "len_0618", points = 1) +
  xlab('Starting Height (cm)') + 
  ylab('Plant Height (cm)') + 
  ylim(0, 160) + 
  theme_minimal() 

p1 + p2 + p3 + plot_layout(axes = 'collect')


ht_mod <- lm(ht_cm ~ wetland + soil_moist_fixed + len_0618, data = dat_last)
summary(ht_mod)

p1 <- plot_predictions(ht_mod, condition = "soil_moist_fixed", points = 1) +
  xlab('Soil Moisture (m^3/m^3)') + 
  ylab('Plant Height (cm)') + 
  ylim(0, 160) + 
  theme_minimal() 

p2 <- plot_predictions(ht_mod, condition = "wetland", points = 1) +
  xlab('wetland') + 
  ylab('Plant Height (cm)') + 
  ylim(0, 160) + 
  theme_minimal() 

p3 <- plot_predictions(ht_mod, condition = "len_0618", points = 1) +
  xlab('Starting Height (cm)') + 
  ylab('Plant Height (cm)') + 
  ylim(0, 160) + 
  theme_minimal() 

p1 + p2 + p3 + plot_layout(axes = 'collect')

# plots of raw data
ggplot(dat_last, aes(soil_moist_fixed, ht_cm)) + 
  geom_point(aes(col = wetland)) + 
 # geom_smooth(method = 'lm', aes(col = wetland)) + 
  geom_smooth(method = 'lm', col = 'black')+
  ylab("Plant height (cm)") + 
  xlab("Soil Moisture (m^3/m^3)") + 
  scale_color_manual(values=c("#A9D18E", "#56B4E9")) + 
  theme_classic()

ggplot(dat_last, aes(wetland, ht_cm)) + 
  geom_boxplot(aes(col = wetland)) + 
  ylab("Plant height (cm)") + 
  scale_color_manual(values=c("#A9D18E", "#56B4E9")) + 
  theme_classic()

ggplot(dat_last, aes(len_0618, ht_cm)) + 
  geom_point(aes(col = wetland)) + 
  geom_smooth(method = 'lm', col = 'black') + 
  ylab("Plant height (cm)") + 
  xlab("Planting height (cm)") + 
  scale_color_manual(values=c("#A9D18E", "#56B4E9")) + 
  theme_classic()

# Fig. 5 flowering models using last survey -------------------
#' 
flo_mod <- glm(flowering ~ wetland + water_depth_fixed + len_0618 + ht_cm,
               data = dat_last, family = 'binomial')
summary(flo_mod)
pseudo_r2(flo_mod)

flo_mod <- glm(flowering ~ wetland + soil_moist_fixed + len_0618 + ht_cm,
               data = dat_last, family = 'binomial')
summary(flo_mod)
pseudo_r2(flo_mod)
# drop un-informative variables
flo_mod <- glm(flowering ~ ht_cm, data = dat_last, family = 'binomial')
summary(flo_mod)
pseudo_r2(flo_mod)

p1 <- plot_predictions(flo_mod, condition = "ht_cm") +
  geom_dots(
    alpha = .8,
    scale = .3,
    pch = 18,
    data = dat_last, aes(
      x = ht_cm,
      y = flowering, 
      side = ifelse(flowering == 1, "bottom", "top"))) + 
  xlab('Plant Height (cm)') + 
  ylab('Flowering (=1) or not (=0)') + 
  theme_minimal()

p2 <- plot_predictions(flo_mod, condition = "wetland") +
  geom_dots(
    alpha = .8,
    scale = .3,
    pch = 18,
    data = dat_last, aes(
      x = wetland,
      y = flowering, 
      side = ifelse(flowering == 1, "bottom", "top"))) + 
  xlab('Wetland') + 
  ylab('Flowering (=1) or not (=0)') + 
  theme_minimal()

p3 <- plot_predictions(flo_mod, condition = "water_depth_fixed") +
  geom_dots(
    alpha = .8,
    scale = .3,
    pch = 18,
    data = dat_last, aes(
      x = water_depth_fixed,
      y = flowering, 
      side = ifelse(flowering == 1, "bottom", "top"))) + 
  xlab('Water Depth (cm)') + 
  ylab('Flowering (=1) or not (=0)') + 
  theme_minimal()

p4 <- plot_predictions(flo_mod, condition = "len_0618") +
  geom_dots(
    alpha = .8,
    scale = .3,
    pch = 18,
    data = dat_last, aes(
      x = len_0618,
      y = flowering, 
      side = ifelse(flowering == 1, "bottom", "top"))) + 
  xlab('Initial height (cm)') + 
  ylab('Flowering (=1) or not (=0)') + 
  theme_minimal()

p1 + p2 + p3 + p4 + plot_layout(axes = 'collect')

#' probability of being dead at time of last survey?
#' use logistic regression

boxplot(gr_start ~ wetland, data = dat_last)

# test if survival depended on wetland, water depth,
# or size of plant at planting. 

# Fig. 6 ----------------------------
sur_mod <- glm(alive ~ wetland + water_depth_fixed + len_0618,
                    data = dat_last, family = binomial)
summary(sur_mod)
pseudo_r2(sur_mod)


sur_mod <- glm(alive ~ wetland + soil_moist_fixed + len_0618,
               data = dat_last, family = binomial)
summary(sur_mod)
pseudo_r2(sur_mod)
#[1] 0.08046798

plot_predictions(sur_mod, condition = "water_depth_fixed") +
  geom_dots(
    alpha = .8,
    scale = .3,
    pch = 18,
    data = dat_last, aes(
      x = water_depth_fixed,
      y = alive, 
      side = ifelse(alive == 1, "bottom", "top"))) +
  xlab('Water Depth (cm)') + 
  ylab('Alive (=1) or not (=0)') + 
  theme_minimal()

plot_predictions(sur_mod, condition = "wetland") +
  geom_dots(
    alpha = .8,
    scale = .3,
    pch = 18,
    data = dat_last, aes(
      x = wetland,
      y = alive, 
      side = ifelse(alive == 1, "bottom", "top"))) +
  xlab('Initial Height (cm)') + 
  ylab('Alive (=1) or not (=0)') + 
  theme_minimal()

plot_predictions(sur_mod, condition = "len_0618") +
  geom_dots(
    alpha = .8,
    scale = .3,
    pch = 18,
    data = dat_last, aes(
      x = len_0618,
      y = alive, 
      side = ifelse(alive == 1, "bottom", "top"))) +
  xlab('Water Depth (cm)') + 
  ylab('Alive (=1) or not (=0)') + 
  theme_minimal()

# clone analysis -----------------------------
# need to compute through time how many clones there are at each plant
# would be good to average the clone sizes
head(dat)

# make a plant id that does not have clone info then 
# you can aggregate information at that plant

tmp <- sub('_.', '', dat$plant_id, fixed = FALSE)
tmp
data.frame(dat$plant_id3, tmp)

# survival analysis ------------------------
# first restructure data
# if plant is alive at last survey
#    then it was alive for the entire study
# if not alive at last survey
#    then need to check next to last survey
#    and so on until all plants gone through


uni_plant_ids <- unique(dat$plant_id3)
survey_dates <- sort(unique(dat$date))
plant_date <- as.Date("2023-06-21")
max_date <- max(dat$date)
end_date <- NULL
for (i in seq_along(uni_plant_ids)) { 
  # this is the last time plant was seen alive or wilted
  end_date[i] <- with(dat, 
                      max(date[plant_id3 == uni_plant_ids[i] & 
                               status %in% c('a', 'w')], na.rm = TRUE))
  
  
}
end_date <- as.Date(end_date)
end_date <- ifelse(is.infinite(end_date), )
#end_date <- as.Date(ifelse(end_date == max_date,  # if alive at last survey then  
#                           end_date,              # last observed is last survey
#                           survey_dates[match(end_date, survey_dates) + 1])) # if not then go one more date prior


dat_surv <- data.frame(plant_id3 = uni_plant_ids, 
                       plant_date,
                       last_obs = end_date)
dat_surv$Age <- dat_surv$last_obs - dat_surv$plant_date
dat_surv$status <- with(dat, ifelse(end_date == max_date, 1, 0))

head(dat_surv)
with(dat_surv, Surv(plant_date, end_date))



# first need to go through the dataset and see 





#' compute growth parameters between each survey
gr2 <- with(dat, ht_cm.1 - ht_cm)
gr3 <- with(dat, ht_cm.2 - ht_cm.1)
gr4 <- with(dat, ht_cm.3 - ht_cm.2)
gr5 <- with(dat, ht_cm.4 - ht_cm.3)

td2 <- dat$date.1 - dat$date

plot(dates, 1:length(dates), type = 'n', 
     ylim = with(dat, range(ht_cm, ht)))


dat$status.1 <- as.factor(dat$status.1)
with(dat, table(status.1, wetland))

boxplot(dat$ht_cm.4 ~ wetland, data = dat)

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


