## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, fig.width=5, fig.height=4, fig.align ="center", out.width = "50%", warning=FALSE, message=FALSE, collapse=TRUE) # global options
library(knitr)


## ---- message=FALSE, warning=FALSE---------------------------------------
library(tidyverse)


## ---- echo=FALSE---------------------------------------------------------
library(dslabs)
data(murders)
head(murders)


## ---- echo=FALSE---------------------------------------------------------
library(dslabs)
data("gapminder")
tidy_data <- gapminder %>% 
  filter(country %in% c("South Korea", "Germany") & !is.na(fertility)) %>%
  select(country, year, fertility)
head(tidy_data, 6) %>% kable()


## ---- echo=FALSE, message=FALSE------------------------------------------
path <- system.file("extdata", package="dslabs")
filename <- file.path(path, "fertility-two-countries-example.csv")
wide_data <- read_csv(filename)
select(wide_data, country, `1960`:`1962`) %>% as.data.frame %>% kable()


## ---- message=FALSE------------------------------------------------------
library(dslabs)
data("murders")
murders <- mutate(murders, rate = total / population * 100000)
head(murders)


## ------------------------------------------------------------------------
filter(murders, rate <= 0.71)


## ------------------------------------------------------------------------
new_table <- select(murders, state, region, rate)
filter(new_table, rate <= 0.71)


## ---- eval=FALSE---------------------------------------------------------
filter(murders, state == "New York")


## ------------------------------------------------------------------------
murders %>% select(state, region, rate) %>% 
  filter(rate <= 0.71)


## ------------------------------------------------------------------------
16 %>% sqrt()
16 %>% sqrt() %>% log2()


## ------------------------------------------------------------------------
16 %>% sqrt() %>% log(base = 2)


## ---- eval=FALSE---------------------------------------------------------
## murders %>% select(state, region, rate) %>%
##   filter(rate <= 0.71)


## ---- eval=FALSE---------------------------------------------------------
## murders <- mutate(murders,
##                   rate =  total / population * 100000,
##                   rank = rank(-rate))


## ---- eval=FALSE---------------------------------------------------------
## my_states <- filter(murders, region %in%
##                     c("Northeast", "West") & rate < 1)
## select(my_states, state, rate, rank)


## ---- eval=FALSE---------------------------------------------------------
## mutate(murders, rate =  total / population * 100000,
##        rank = rank(-rate)) %>%
##   select(state, rate, rank)


## ------------------------------------------------------------------------
library(dplyr)
library(dslabs)
data(heights)


## ------------------------------------------------------------------------
s <- heights %>% 
  filter(sex == "Female") %>%
  summarize(average = mean(height), 
            standard_deviation = sd(height))
s


## ------------------------------------------------------------------------
heights %>% 
  filter(sex == "Female") %>%
  summarize(median = median(height), 
            minimum = min(height), 
            maximum = max(height))


## ------------------------------------------------------------------------
murders %>% mutate(rate = total/population*100000) %>%
  summarize(avg_rate=mean(rate)) # Incorrect!!!

# 각각 rate의 평균은 전체 rate의 평균을 나타내지 못한다. 
# 각각 rate마다 total 이 다르기 때문이다.

## ------------------------------------------------------------------------
us_murder_rate <- murders %>%
  summarize(avg_rate = sum(total) / sum(population) * 100000)
us_murder_rate


## ------------------------------------------------------------------------
class(us_murder_rate)


## ------------------------------------------------------------------------
us_murder_rate %>% pull(avg_rate)

class(us_murder_rate %>%pull(avg_rate))

## ------------------------------------------------------------------------
heights %>% group_by(sex) %>% head(6)


## ------------------------------------------------------------------------
heights %>% 
  group_by(sex) %>%
  summarize(average = mean(height), 
            standard_deviation = sd(height))


## ------------------------------------------------------------------------
murders %>% 
  group_by(region) %>%
  summarize(median_rate = median(rate))


## ------------------------------------------------------------------------
murders %>%
  arrange(population) %>%
  head()


## ------------------------------------------------------------------------
murders %>% 
  arrange(rate) %>%
  head()


## ------------------------------------------------------------------------
murders %>% 
  arrange(desc(rate)) %>%
  head()


## ------------------------------------------------------------------------
murders %>% 
  arrange(region, rate) %>% 
  head()


## ------------------------------------------------------------------------
murders %>% top_n(5, rate)


## ------------------------------------------------------------------------
murders %>% group_by(region) %>% head(5)


## ------------------------------------------------------------------------
murders %>% group_by(region) %>% class()


## ------------------------------------------------------------------------
class(murders[,4])


## ------------------------------------------------------------------------
class(as_tibble(murders)[,4])


## ------------------------------------------------------------------------
tibble(id = c(1, 2, 3), func = c(mean, median, sd))


## ------------------------------------------------------------------------
grades <- tibble(names = c("John", "Juan", "Jean", "Yao"), 
                 exam_1 = c(95, 80, 90, 85), 
                 exam_2 = c(90, 85, 85, 90))


## ------------------------------------------------------------------------
grades <- data.frame(names = c("John", "Juan", "Jean", "Yao"), 
                     exam_1 = c(95, 80, 90, 85), 
                     exam_2 = c(90, 85, 85, 90))
class(grades$names)


## ------------------------------------------------------------------------
grades <- data.frame(names = c("John", "Juan", "Jean", "Yao"), 
                     exam_1 = c(95, 80, 90, 85), 
                     exam_2 = c(90, 85, 85, 90),
                     stringsAsFactors = FALSE)
class(grades$names)


## ------------------------------------------------------------------------
as_tibble(grades) %>% class()


## ------------------------------------------------------------------------
filter(murders, region == "South") %>% 
  mutate(rate = total / population * 10^5) %>% 
  summarize(median = median(rate)) %>%
  pull(median)


## ------------------------------------------------------------------------
rates <- filter(murders, region == "South") %>% 
  mutate(rate = total / population * 10^5) %>% 
  .$rate
median(rates)


## ---- eval=FALSE---------------------------------------------------------
## data(heights)
## heights %>%
##   filter(sex == "Female") %>%
##   summarize(range = quantile(height, c(0, 0.5, 1))) ## ERROR!!!


## ------------------------------------------------------------------------
my_summary <- function(dat){
  x <- quantile(dat$height, c(0, 0.5, 1))
  tibble(min = x[1], median = x[2], max = x[3])
}


## ------------------------------------------------------------------------
heights %>% 
  group_by(sex) %>% 
  my_summary


## ------------------------------------------------------------------------
heights %>% 
  group_by(sex) %>% 
  do(my_summary(.))


## ---- eval=FALSE---------------------------------------------------------
## heights %>%
##   group_by(sex) %>%
##   do(my_summary)


## ------------------------------------------------------------------------
compute_s_n <- function(n){
  x <- 1:n
  sum(x)
}
n <- 1:25
s_n <- sapply(n, compute_s_n)


## ------------------------------------------------------------------------
library(purrr)
s_n <- map(n, compute_s_n)
class(s_n)


## ------------------------------------------------------------------------
s_n <- map_dbl(n, compute_s_n)
class(s_n)


## ---- eval=FALSE---------------------------------------------------------
## s_n <- map_df(n, compute_s_n)


## ------------------------------------------------------------------------
compute_s_n <- function(n){
  x <- 1:n
  tibble(sum = sum(x))
}
s_n <- map_df(n, compute_s_n)


## ------------------------------------------------------------------------
x <- c(-2, -1, 0, 1, 2)
case_when(x < 0 ~ "Negative", x > 0 ~ "Positive", TRUE ~ "Zero")


## ------------------------------------------------------------------------
murders %>% 
  mutate(group = case_when(
    abb %in% c("ME", "NH", "VT", "MA", "RI", "CT") ~ "New England",
    abb %in% c("WA", "OR", "CA") ~ "West Coast",
    region == "South" ~ "South",
    TRUE ~ "Other")) %>%
  group_by(group) %>%
  summarize(rate = sum(total) / sum(population) * 10^5) 


## ---- eval=FALSE---------------------------------------------------------
## x >= a & x <= b


## ---- eval = FALSE-------------------------------------------------------
## between(x, a, b)

