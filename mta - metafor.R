library(tidyverse)

install.packages("metafor")
library(metafor)

#### 3.1 독립인 두 그룹

# 연속형 자료
# data
nmd <- get(data("dat.normand1999"))

nmd %>% str()
nmd %>% head()

escalc(measure = "MD", n1i = n1i, m1i = m1i, sd1i = sd1i, n2i = n2i, m2i = m2i, sd2i = sd2i, data = nmd) %>% summary()
# pval <- h0 : MD가 유효하지 않다(차이가 없다)

escalc(measure = "SMD", n1i = n1i, m1i = m1i, sd1i = sd1i, n2i = n2i, m2i = m2i, sd2i = sd2i, data = nmd) %>% summary()


# 이산형 자료
# data
data(dat.bcg)

bcg <- dat.bcg

bcg %>% str()

# relative risk
escalc(measure = "RR", ai = tpos, bi = tneg, ci = cpos, di = cneg, data = bcg, append = TRUE) %>% summary()

# 직접구하기
# bcg$InOR <- with(bcg, log((tpos*cneg)/(tneg*cpos)))
# bcg <- mutate(bcg, lnOR = log((tpos*cneg)/(tneg*cpos)))
# bcg <- mutate(bcg, Var_lnOR = 1/tpos + 1/cneg + 1/tneg + 1/cpos)

# odds ratio
escalc(measure = "OR", ai = tpos, bi = tneg, ci = cpos, di = cneg, data = bcg, append = TRUE) %>% summary()
# yi = lnOR, vi = VarlnOR

# relative risk difference
escalc(measure = "RD", ai = tpos, bi = tneg, ci = cpos, di = cneg, data = bcg, append = TRUE) %>% summary()

# 계수형 자료
hart <- get(data(dat.hart1999))

escalc(measure = "IRR", x1i = x1i, x2i = x2i, t1i = t1i, t2i = t2i, data = hart) %>% summary()
# yi = ln(IRR), vi = var(ln(IRR))


#### 3.2단일군
df <- data.frame(mi = 5, sdi = 10, ni = 50)
escalc(measure = "MN", mi = mi, sdi = sdi, ni = ni, data = df) %>% summary()
# "MN" for the raw mean

# 대응 그룹이나 단일군의 사전-사후 비교
df <- data.frame(m1i = 105, m2i = 100, sd1i = 10, sd2i = 10, ri = 0.5, ni = 50)
# ri = corr
escalc(measure = "MC", m1i = m1i, m2i = m2i, sd1i = sd1i, sd2i = sd2i, ri = ri, ni = ni, data = df) %>% summary()
# "MC" for the raw mean change

# 상관계수
mcd <- get(data(dat.mcdaniel1994))
mcd %>% str()

escalc(measure = "ZCOR", ri = ri, ni = ni, data = mcd) %>% summary()

# 이분형 자료
prz <- get(data(dat.pritz1997))
prz %>% str()

escalc(measure = "PR", xi = xi, ni = ni, data = prz) %>% summary()
# pr : proportion

escalc(measure = "PLO", xi = xi, ni = ni, data = prz) %>% summary()
# PLO : logit transformed proportion

escalc(measure = "PAS", xi = xi, ni = ni, data = prz) %>% summary()
# PAS : arcsine square root transformed proportion

# 계수형 자료
hart <- get(data(dat.hart1999))

escalc(measure = "IR", xi = x1i, ti = t1i, data = hart) %>% summary()
# IR : for the raw incidence rate

escalc(measure = "IRLN", xi = x1i, ti = t1i, data = hart) %>% summary()
# IRLN : log transformed incidence rate

escalc(measure = "IRS", xi = x1i, ti = t1i, data = hart) %>% summary()
# IRS : square root transformed incidence rate


#### 4. 통합추정치 추정
#### 4.1 고정효과모형
## 4.1.1 역분산 가중치 방법
data(dat.normand1999)
nmd <- dat.normand1999

nmd %>% str()
nmd %>% head()

# MD와 MD의 분산 계산
nmd_md <- escalc(measure = "MD", m1i = m1i, sd1i = sd1i, n1i = n1i, m2i = m2i, sd2i = sd2i, n2i = n2i, data = nmd)

# MD의 역분산가중치 통합추정치 계산 Random-Effects Model Analysis
rma(yi, vi, data = nmd_md, method = "FE")
# 가설검정 H0:θF=0 (θ는 효과 크기) Fixed effects model = FE
# Cochran's Q-test H0 = 모든 개별 연구의 효과크기는 동일
# I^2 : 통계적 이질성을 나타내는 통계량
# 이분산성이다 = 효과크기가 다르다 -> 회귀분석으로 따지면 yhat들의 분산이 고르지 않고 제각각이다 즉, 효과크기가 존재한다.

data(dat.bcg)
bcg <- dat.bcg

bcg_lnOR <- escalc(measure = "OR", ai = tpos, bi = tneg, ci = cpos, di = cneg, data = bcg, append = TRUE)

# ln(OR)의 역분산가중치 통합추정치 계산 Random-Effects Model Analysis
rma(yi, vi, data = bcg_lnOR, method = "FE")


## 4.1.2 멘텔-헨젤 추정법
rma.mh(measure = "OR", ai = tpos, bi = tneg, ci = cpos, di = cneg, data = bcg) %>% summary()
# Model Results (log scale) : 로그(오즈비)
# Cochran-Mantel-Haenszel Test -> H0 : OR1 = OR2 = ... = ORk = 1

# relative risk
rma.mh(measure = "RR", ai = tpos, bi = tneg, ci = cpos, di = cneg, data = bcg) %>% summary()

# relative risk difference
rma.mh(measure = "RD", ai = tpos, bi = tneg, ci = cpos, di = cneg, data = bcg) %>% summary()

# peto
rma.peto(ai = tpos, bi = tneg, ci = cpos, di = cneg, data = bcg) %>% summary()


#### 4.2 랜덤효과모형
data(dat.normand1999)
nmd <- dat.normand1999

nmd_md <- escalc(measure = "MD", n1i = n1i, m1i = m1i, sd1i = sd1i, n2i = n2i, m2i = m2i, sd2i = sd2i, data = nmd)

rma(yi, vi, data = nmd_md, method = "DL")
# DerSimonian-Laird estimator 
# 가설검정 H0:θF=0 (θ는 효과 크기)

data(dat.bcg)
bcg <- dat.bcg

bcg_lnOR <- escalc(measure = "OR", ai = tpos, bi = tneg, ci = cpos, di = cneg, data = bcg, append = TRUE)

# ln(OR)의 역분산가중치 통합추정치 계산 Random-Effects Model Analysis
rma(yi, vi, data = bcg_lnOR, method = "DL")



#### 5. 통계적 이질성
## 5.1 통계적 이질성 평가
# 5.1.1 숲그림
data(dat.bcg)
bcg <- dat.bcg

bcg_lnOR <- escalc(measure = "OR", ai = tpos, bi = tneg, ci = cpos, di = cneg, data = bcg, append = TRUE)
bcg_rma <- rma(yi, vi, data = bcg_lnOR, method = "DL")

forest(bcg_rma, xlim = c(-16, 6), at = log(c(.05, .25, 1, 4)), atransf = exp, ilab = cbind(bcg$tpos, bcg$tneg, bcg$cpos, bcg$cneg), ilab.xpos = c(-10, -8, -6, -4), cex = 1, ylim = c(-1, 27), order = order(bcg$alloc), rows = c(3:4, 9:15, 20:23), xlab = "Relative Risk", mlab = "RE Model for All studies")

# 5.1.2 통계적 이질성 검토를 위한 통계량
data(dat.bcg)
bcg <- dat.bcg
rma(measure = "OR", ai = tpos, bi = tneg, ci = cpos, di = cneg, data = bcg, method = "DL") %>% summary() 
# Cochran's Q-test H0 = 모든 개별 연구의 효과크기는 동일
# I^2 : 통계적 이질성을 나타내는 통계량
# H^2 Q통계량과 자유도의 비로 정의, H > 1 경우 설명할 수 없는 이질성이 있다고 판단 H^2 = Q/(k-1)
# tau : T 통계량, tau^2 : T^2 통계량 // 효과크기의 연구간 변동을 나타내는 분산 tau^2, tau의 추정량이 T^2, T 
rma(measure = "OR", ai = tpos, bi = tneg, ci = cpos, di = cneg, data = bcg, method = "DL") %>% confint()

## 5.2 하위그룹분석
data(dat.bcg)
bcg <- dat.bcg

rma(measure = "RR", ai = tpos, bi = tneg, ci = cpos, di = cneg, data = bcg, method = "DL", subset = (alloc == "systematic")) %>% summary()

rma(measure = "RR", ai = tpos, bi = tneg, ci = cpos, di = cneg, data = bcg, method = "DL", subset = (alloc == "random")) %>% summary()

rma(measure = "RR", ai = tpos, bi = tneg, ci = cpos, di = cneg, data = bcg, method = "DL", subset = (alloc == "alternate")) %>% summary()

rma(measure = "RR", ai = tpos, bi = tneg, ci = cpos, di = cneg, data = bcg, method = "DL", slab = paste(author, year, sep = ",")) %>% 
forest(xlim = c(-16, 6), at = log(c(.05, .25, 1, 4)), atransf = exp,
       ilab = cbind(bcg$tpos, bcg$tneg, bcg$cpos, bcg$cneg),
       ilab.xpos = c(-9.5, -8, -6, -4), cex = .75, ylim = c(-1, 27),
       order = order(bcg$alloc), rows= c(3:4, 9:15, 20:23),
       xlab = "Relative Risk", mlab = "RE Model for all studies")

## 5.3 메타회귀분석
## 5.3.1 고정효과 메타회귀모형
data(dat.bcg)
bcg <- dat.bcg

rma(measure = "RR", ai = tpos, bi = tneg, ci = cpos, di = cneg, data = bcg, method = "FE", mods = ablat, intercept = TRUE) %>% summary()
# Fixed effects model = FE
# ablat : 위도
# Cochran's Q-test H0 = 모든 개별 연구의 효과크기는 동일
# Test of Moderators <- 모형 
# 추정된 모형 ln(RR) = 0.3436 + (-0.0292)*mods(위도)

## 5.3.2 랜덤효과 메타회귀모형
rma(measure = "RR", ai = tpos, bi = tneg, ci = cpos, di = cneg, data = bcg, method = "DL", mods = ablat, intercept = TRUE) %>% summary()
# Test for Residual Heterogeneity 영가설: 모형으로 설명이 안되는 분산 = 0
# Test of Moderators 영가설 : 모든 회귀계수 = 0
# 추정된 모형 ln(RR) = (-0.0292)*mods(위도)
# I^2 : 통계적 이질성을 나타내는 통계량
# H^2 Q통계량과 자유도의 비로 정의, H > 1 경우 설명할 수 없는 이질성이 있다고 판단 H^2 = Q/(k-1)
# tau : T 통계량, tau^2 : T^2 통계량 // 효과크기의 연구간 변동을 나타내는 분산 tau^2, tau의 추정량이 T^2, T 
rma(measure = "RR", ai = tpos, bi = tneg, ci = cpos, di = cneg, data = bcg, method = "DL", mods = ~ ablat + year, intercept = TRUE) %>% influence()

rma(measure = "RR", ai = tpos, bi = tneg, ci = cpos, di = cneg, data = bcg, method = "DL", mods = ~ ablat + year, intercept = TRUE) %>% influence() %>% 
  plot()
