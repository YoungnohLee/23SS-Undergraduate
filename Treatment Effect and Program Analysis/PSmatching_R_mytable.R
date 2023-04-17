# Propensity Score Matching
# 2023/03/24

dat = read.csv('C:/Users/아로미/Downloads/data.csv')
cols_fac = c('sex','drk_st','HBV','HCV','LC') # columns that are factors
dat[,cols_fac] <- lapply(dat[,cols_fac], as.factor)
str(dat)

# PSmatching은 실험군과 대조군의 imbalancing을 피하는 방법이다.
# imbalance란, observation의 covariate이 이질적인 상태를 말한다.
# Observational Study 에서는 실험군과 대조군의 covariate의 분포가 유사해야한다.
# treatment만 이질적이고, 나머지 condition은 동일하게 설정해야 treatment effect를 측정할 수 있기 때문이다.

##----------------------------------
library(moonBook)
mytable(drk_st ~ ., data=dat) # 앞에변수를 제외한 모든 변수 : . 

# NOTE
# mytable()에서 p-value는 범주형의 경우 chi-squared test, 연속형의 경우 정규분포를 가정하여
# t-test를 시행하게 된다.


# NOTE
# Association test
# 분할표에서 cell count가 충분한 경우 : chi-squared test
# 분할표에서 cell count가 적은 경우 : Fisher's exact test
# cell count의 criteria는 E < 5인 경우

# Comaparing two means test
# 두 그룹 모두 정규성을 만족 : two sample t-test
# 어느 한 그룹이 정규성 충족 못함 : Wilcox test

##----------------------------------
mytable(drk_st ~ ., data=dat, method=3, catMethod=0)

# sex, age, drk_freq, exercise Covariate에서 차이를 보임
# 이러한 경우에 인과관계 파악이 어려움. 

##----------------------------------
mod <- glm(drk_st ~ sex + age + drk_freq + HBV + HCV + exercise, family=binomial, data=dat)
summary(mod)

score.table <- data.frame(ps=predict(mod, type='response'), drk_st = mod$model$drk_st)
library(dplyr)
library(ggplot2)
score.table %>% mutate(drk_st = ifelse(drk_st==1, 'Drinking', 'Non-Dringking')) %>%
  ggplot(aes(x=ps, fill=drk_st)) +
  geom_histogram(color='white', position = 'dodge') +
  theme_bw() + ylim(c(0,100))

# 결과해석부 : drk_st==1 : Drinking, drk_st==0 : Non-Drinking
# propensity score는 given X(sex,age,drk_freq...)일때 y 값이다. prediction 값
# given X일때 1과0중 어디에 속할 확률이 높은가 의 문제이다.
# (중요!)
# propensity score의 분포가 비슷하다는 건 그만큼 X(sex,age,drk_freq...)가 유사하다는 뜻이다.

##----------------------------------
library(MatchIt)
match_fit <- matchit(drk_st ~ sex + age + drk_freq + HBV + HCV + exercise, method='nearest', data=dat)
matched_dat <- match.data(match_fit)

# 이렇게 했을때, propensity score가 비슷한 것들끼리 matching 해주었다!
mytable(drk_st ~ ., data=matched_dat, method=3, catMethod=0)

# 그러나 결과는 제대로 나오지 않았음.
