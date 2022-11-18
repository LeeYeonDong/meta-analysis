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
# Cochran's Q-test 
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

