######################
###   0. Setting   ###
######################

library(tidyverse)
library(lubridate)
library(sp)
library(spacetime)
library(geoR)
library(gstat)
library(spatstat)

setwd("C:/Users/User/Desktop/공간통계/프로젝트")
rm(list = ls())
dat = read.csv("C:/Users/SJ/Downloads/open/train.csv", encoding = 'UTF-8')
dat %>% head
dat %>% dim


######################################
###   1. Preprocessing: Variable   ###
######################################

# 변수별 유니크값 개수
for (i in 1:ncol(dat)) {
  cat(names(dat)[i], " : ", nrow(unique(select(dat,names(dat)[i]))),"\n")
}

# 변수 변환, 추가 및 제거
dat %>%
  mutate(base_hour = as.factor(base_hour)) %>%
  ggplot(aes(x = target, col = base_hour)) +
  geom_density() +
  theme_bw() +
  facet_wrap(vars(base_hour))

dat = dat %>%
  mutate(base_date = as.Date(as.character(base_date),"%Y%m%d"),
         base_wday = as.POSIXlt(base_date, origin = '1970-01-01')$wday,
         rush_hour = ifelse((base_wday %in% c(0,1)) & (base_hour %in% c(8,9,17,18,19)), 1, 0),
         t = base_date + hours(base_hour),
         start_turn_restricted = ifelse(start_turn_restricted == '있음',1,0),
         end_turn_restricted = ifelse(end_turn_restricted == '있음',1,0)) %>%
  select(-c(id, base_date, day_of_week, base_hour, base_wday,
            vehicle_restricted, height_restricted)) # -> 유니크값 1개인 변수

# 도로 시작지점, 도착지점 분리
dat %>%
  filter((start_latitude == end_latitude) & (start_longitude == end_longitude))

start_dat = dat %>%
  rename(lat = start_latitude,
         long = start_longitude,
         turn_restricted = start_turn_restricted,
         node_name = start_node_name) %>%
  select(-c(end_latitude, end_longitude, end_turn_restricted, end_node_name))

end_dat = dat %>%
  rename(lat = end_latitude,
         long = end_longitude,
         turn_restricted = end_turn_restricted,
         node_name = end_node_name) %>%
  select(-c(start_latitude, start_longitude, start_turn_restricted, start_node_name))

dat = rbind(start_dat, end_dat)
rm(start_dat,end_dat)

#################################
###   1. Preprocessing: Row   ###
#################################

# 하나의 시공간 정보에 여러 행이 대응되는 경우 처리
dat = dat %>%
  select(-c(road_name, node_name)) %>%
  group_by(t, lat, long) %>%
  summarize(across(everything(), list(mean)), .groups = 'drop') %>% 
  as.data.frame
colnames(dat) = c('t','lat','long','lane_count','road_rating',
                  'multi_linked','connect_code','maximum_speed_limit',
                  'weight_restricted','road_type','turn_restricted',
                  'target','rush_hour')

# 결측치 개수 확인
check.na = dat %>%
  select(c(lat,long,t,target)) %>%
  mutate(loc = paste0(lat,",",long)) %>%
  select(-c(lat,long)) 

check.na = check.na %>%
  pivot_wider(id_cols = t, names_from = loc, values_from = target)

# 결측치 제거 (공간)
na.s = check.na[,-1] %>% is.na() %>% colSums()
na.s = data.frame(
  'loc' = names(na.s),
  'NA_count' = na.s
)

na.s.cnt = na.s %>%
  group_by(NA_count) %>%
  summarize(n = n()) %>%
  mutate(cum_cnt = cumsum(n))

na.s.cnt %>%
  ggplot(aes(x = NA_count, y = cum_cnt)) + geom_line() + 
  scale_y_continuous(n.breaks = 100) + 
  labs(x = '결측치 개수', y = '해당 개수 이하의 결측치를 갖는 공간의 개수') + 
  theme_bw()

na.s.cnt %>%
  filter(cum_cnt >= 200 & cum_cnt <= 240) %>%
  print(n = 50) # <= 135 (212개 추출)

selected.s = na.s %>%
  filter(NA_count <= 135) %>%
  separate(loc, c('lat','long'), sep = ',') %>%
  mutate(lat = as.numeric(lat),
         long = as.numeric(long)) %>%
  select(-NA_count)
rownames(selected.s) = 1:nrow(selected.s)

dat2 = dat %>%
  inner_join(selected.s, by = c('lat','long'))

# 결측치 제거 (시간)
check.na = dat2 %>%
  select(c(lat,long,t,target)) %>%
  mutate(loc = paste0(lat,",",long)) %>%
  select(-c(lat,long)) 

check.na = check.na %>%
  pivot_wider(id_cols = t, names_from = loc, values_from = target)

na.t = check.na %>% is.na() %>% rowSums()
na.t = data.frame(
  't' = check.na$t,
  'NA_count' = na.t
)
na.t %>%
  group_by(NA_count) %>%
  summarize(n = n()) %>%
  print(n = 40)

selected.t = na.t %>%
  filter(NA_count == 0)

dat3 = dat2 %>%
  inner_join(selected.t, by = 't') %>%
  select(-NA_count)

dat = dat3
rm(dat2,dat3,na.s,na.t,selected.t,selected.s,check.na, na.s.cnt)

for (i in 1:ncol(dat)) {
  cat(names(dat)[i], " : ", nrow(unique(select(dat,names(dat)[i]))),"\n")
}

dat = dat %>% select(-c(multi_linked, connect_code)) # 최종 데이터에 대해 유니크값 1개

# time lag 일정하게
time = dat %>%
  arrange(t) %>%
  select(t) %>%
  unique

time1 = time[2:nrow(time),]
time0 = time[1:(nrow(time)-1),]

time = data.frame(
  time1 = time1,
  time0 = time0,
  lag = time1 - time0
)

time = time %>%
  mutate(lag1 = ifelse(lag == 1, 1, 0))

not.lag1 = time %>%
  rownames_to_column('time_idx') %>%
  mutate(time_idx = as.integer(time_idx)) %>%
  filter(lag1 == 0)

lag1.len = data.frame(
  lag0.start = not.lag1[1:(nrow(not.lag1)-1),'time_idx'],
  lag0.end = not.lag1[2:nrow(not.lag1),'time_idx'],
  len =  not.lag1[2:nrow(not.lag1),'time_idx'] - not.lag1[1:(nrow(not.lag1)-1),'time_idx']
)

max(lag1.len$len); which.max(lag1.len$len)
lag1.len[324,]
time[5086:5217,]
start.time = time[5087,'time0'] # "2022-07-07 05:00:00 UTC"
end.time = time[5216,'time1'] # "2022-07-12 15:00:00 UTC"

dat = dat %>%
  filter((t >= start.time) & (t <= end.time))
rm(lag1.len,not.lag1,time,time0,time1,start.time,end.time,i)

# training set / test set 분리
set.seed(42)
loc.idx = sample(1:212, 190)

all.loc = dat %>%
  select(lat,long) %>%
  unique

train.loc = all.loc[loc.idx,]
test.loc = all.loc[-loc.idx,]

train = dat %>%
  inner_join(train.loc, by = c('lat','long'))

test = dat %>%
  inner_join(test.loc, by = c('lat','long'))

rm(loc.idx, all.loc, train.loc, test.loc)

hist(train$target,
     col = 'lightblue',
     prob = T,
     main = "Histogram of Z(s,t)")
lines(density(train$target),
      lwd = 1.5,
      col = 'blue')

save.image("dataset.Rdata")
write.csv(dat,'dat.csv',row.names = F)
load("dataset.Rdata")



####################
###   2-1. OLS   ###
####################

# Estimate drift
names(train)
fit.ols = lm(target ~ .-t-lat-long, data = train)
summary(fit.ols)


##########################
###   2-2. Variogram   ###
##########################

# STFDF 형태로 변환
train$res = residuals(fit.ols)
names(train)
sp = SpatialPoints(train[,2:3] %>% unique)
time = train %>% arrange(t) %>% dplyr::select(t) %>% unique
rownames(time) = 1:nrow(time)
stfdf.tr = STFDF(sp, time$t, endTime = delta(time$t), train)

# --- Temporal variogram --- #
locs = train %>%
  select(c(lat, long)) %>%
  unique
loc1 = locs[1,]; loc1

dat.loc1 = train %>%
  filter(lat == loc1$lat, long == loc1$long) %>%
  select(-c(lat,long)) %>%
  arrange(t) %>%
  select(res)
dat.loc1$t = 1:nrow(dat.loc1) * 15/3600
dat.loc1$pseudo.t = rep(0,nrow(dat.loc1))

names(dat.loc1)
dat.loc1 = as.geodata(dat.loc1, coords.col = 2:3, data.col = 1)  

vario.cd.loc1 = variog(dat.loc1, option = 'cloud')
plot(vario.cd.loc1) # -> max dist: 0.5

rm(locs,loc1,vario.cd.loc1)

vario.mom.loc1 = variog(dat.loc1, option = 'bin',
                        estimator.type = 'classical', max.dist = 0.5)

AWLS.loc1 = variofit(vario.mom.loc1, cov.model = 'mat', ini = c(10, 0.2), 
                     kappa = 1.5, fix.kappa = F,
                     nug = 0, fix.nugget = F, weights = 'cressie', 
                     message = F)

# --- Spatial variogram --- #
t1 = time[4,'t']; t1

dat.t1 = train %>%
  filter(t == t1) %>%
  select(c(res,lat,long))

names(dat.t1)
dat.t1 = as.geodata(dat.t1, coords.col = 2:3, data.col = 1)  

vario.cd.t1 = variog(dat.t1, option = 'cloud')
plot(vario.cd.t1) # -> max dist: 0.3

rm(t1,vario.cd.t1)

vario.mom.t1 = variog(dat.t1, option = 'bin',
                      estimator.type = 'classical', max.dist = 0.3)

AWLS.t1 = variofit(vario.mom.t1, cov.model = 'mat', ini = c(500, 0.2), 
                   kappa = 1.5, fix.kappa = F, 
                   nug = 0, fix.nugget = F,
                   weights = 'cressie', message = F)

# Plotting
par(mfrow = c(1,2))
plot(vario.mom.loc1, xlab = 'time lag', main = 'Temporal Variogram')
lines(AWLS.loc1, col = 'blue')
plot(vario.mom.t1, main = 'Spatial Variogram')
lines(AWLS.t1, col = 'blue')

rm(dat.loc1,dat.t1,vario.mom.loc1,vario.mom.t1)


# --- spatio-temporal variogram --- #
set.seed(42)
varioST = variogramST(res ~ 1, 
                      data = stfdf.tr)
plot(varioST, wireframe = T)

# --- Separable Variogram --- #
set.seed(42)
varioSEP = vgmST(stModel = "separable",
                 time = vgm(20, "Mat", 0.1, nugget = 3),
                 space = vgm(100, "Mat", 0.1, nugget = 50),
                 sill = 100)

fitSEP = fit.StVariogram(varioST, varioSEP, 
                         method = 'L-BFGS-B',
                         fit.method = 2)
attr(fitSEP, 'MSE') # 1340.803
extractPar(fitSEP)

# --- Product Sum Variogram --- #
set.seed(42)
varioPS = vgmST(stModel = "productSum",
                time = vgm(20, "Mat", 0.1, nugget = 3),
                space = vgm(100, "Mat", 0.1, nugget = 50),
                k = 2)

fitPS = fit.StVariogram(varioST, varioPS, 
                        method = 'L-BFGS-B',
                        fit.method = 2)
attr(fitPS, 'MSE') # 247.9027
extractPar(fitPS)

# --- Metric Variogram --- #
set.seed(42)
varioMET = vgmST(stModel = "metric",
                 joint = vgm(50, "Mat", 0.1, nugget = 5),
                 stAni = 500)

fitMET = fit.StVariogram(varioST, varioMET, 
                         method = 'L-BFGS-B')
attr(fitMET, 'MSE') # 1325.039
extractPar(fitMET)

# --- Sum Metric Variogram --- #
set.seed(42)
varioSM = vgmST(stModel = "sumMetric",
                time = vgm(20, "Mat", 0.1, nugget = 3),
                space = vgm(100, "Mat", 0.1, nugget = 50),
                joint = vgm(30, "Mat", 0.1, nugget = 50),
                stAni = 500)

fitSM = fit.StVariogram(varioST, varioSM, 
                        method = 'L-BFGS-B',
                        fit.method = 2)
attr(fitSM, 'MSE') # 229.9447
extractPar(fitSM)

# --- Simple Sum Metric Variogram --- #
set.seed(42)
varioSSM = vgmST(stModel = "simpleSumMetric",
                 time = vgm(20, "Mat", 0.1, nugget = 3),
                 space = vgm(100, "Mat", 0.1, nugget = 50),
                 joint = vgm(30, "Mat", 0.1, nugget = 50),
                 nugget = 5, stAni = 500)

fitSSM = fit.StVariogram(varioST, varioSSM, 
                         method = 'L-BFGS-B',
                         fit.method = 2)
attr(fitSSM, 'MSE') # 265.6075
extractPar(fitSSM)


# --- Final model --- #
plot(varioST, fitSM, all = T, wireframe = T)

extractPar(fitSM)
vario.fin = vgmST(stModel = "sumMetric",
                  time = vgm(64.27272795, "Mat", 8.51056092, nugget = 0.00000000),
                  space = vgm(104.38384235, "Mat", 0.08867291, nugget = 22.92725529),
                  joint = vgm(2.30605245, "Mat", 4.19557666, nugget = 11.83042850),
                  stAni = 500,
                  temporalunit = "hours")



########################
###   2-3. Kriging   ###
########################

# STFDF로 형태 변환
sp = SpatialPoints(test[,2:3] %>% unique)
time = test %>% arrange(t) %>% dplyr::select(t) %>% unique
rownames(time) = 1:nrow(time)
stfdf.te = STFDF(sp, time$t, endTime = delta(time$t), test)

# Predicted drift
mu.hat = predict(fit.ols, newdata = test)
sum((test$target - mu.hat)^2) # baseline(OLS): 437480

# Predicted residual (by Ordinary kriging)
pred = krigeST(res ~ 1, data = stfdf.tr, modelList = vario.fin, 
               newdata = stfdf.te,
               computeVar = F)
stplot(pred)

# Final Prediction
y.hat = mu.hat + pred@data
sum((test$target - y.hat)^2) # 122573.9



#######################
###   3. Plotting   ###
#######################

y.hat = y.hat$var1.pred
test$pred = y.hat
locs = test %>%
  select(c(lat, long)) %>%
  unique

# 33.24691 126.5682
test %>%
  filter(lat == locs$lat[1], long == locs$long[1]) %>%
  select(c(t,target,pred)) %>%
  gather(target, pred, key = "variable", value = "value") %>%
  ggplot(aes(x = t, y = value)) +
  geom_line(aes(col = variable)) +
  scale_color_manual(values = c("darkred", "steelblue")) +
  theme_bw() +
  xlab("Hour") +
  ylab("Value")

# 33.25044 126.4798
test %>%
  filter(lat == locs$lat[3], long == locs$long[3]) %>%
  select(c(t,target,pred)) %>%
  gather(target, pred, key = "variable", value = "value") %>%
  ggplot(aes(x = t, y = value)) +
  geom_line(aes(col = variable)) +
  scale_color_manual(values = c("darkred", "steelblue")) +
  theme_bw() +
  xlab("Hour") +
  ylab("Value")

# 33.47106 126.5455
test %>%
  filter(lat == locs$lat[16], long == locs$long[16]) %>%
  select(c(t,target,pred)) %>%
  gather(target, pred, key = "variable", value = "value") %>%
  ggplot(aes(x = t, y = value)) +
  geom_line(aes(col = variable)) +
  scale_color_manual(values = c("darkred", "steelblue")) +
  theme_bw() +
  xlab("Hour") +
  ylab("Value")

# 33.49421 126.4367
test %>%
  filter(lat == locs$lat[19], long == locs$long[19]) %>%
  select(c(t,target,pred)) %>%
  gather(target, pred, key = "variable", value = "value") %>%
  ggplot(aes(x = t, y = value)) +
  geom_line(aes(col = variable)) +
  scale_color_manual(values = c("darkred", "steelblue")) +
  theme_bw() +
  xlab("Hour") +
  ylab("Value")

# 33.49686 126.5812
test %>%
  filter(lat == locs$lat[20], long == locs$long[20]) %>%
  select(c(t,target,pred)) %>%
  gather(target, pred, key = "variable", value = "value") %>%
  ggplot(aes(x = t, y = value)) +
  geom_line(aes(col = variable)) +
  scale_color_manual(values = c("darkred", "steelblue")) +
  theme_bw() +
  xlab("Hour") +
  ylab("Value")