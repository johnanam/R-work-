#install.packages("installr")
#library(installr)
#updateR()

## Explore 

#install.packages("tidyverse")
library(tidyverse)
data()
?starwars
dim(starwars)
str(starwars)
glimpse(starwars)
view(starwars)
head(starwars)

attach(starwars) ## attach the data sets 

height

names(starwars) ## know the names of your variables 
length(starwars)
class(hair_color)
length(hair_color)
unique(hair_color)


View(sort(table(hair_color),decreasing = TRUE))
barplot(sort(table(hair_color),decreasing = TRUE))
barplot(sort(table(eye_color),decreasing = TRUE))

## Piping operator 

starwars %>% 
  select(hair_color) %>% 
 View()

hair_color

starwars$hair_color

## Viewing rows in hair_color columns with missing values 

starwars[is.na(hair_color),]
view(starwars[is.na(hair_color),])

class(height)
summary(height)


## Cleaning data 

# What is clean data 
#Variable types 
#select and filter 
#Find and deal with missing data 
#Find and deal with duplicates 
#Recording values 


library(tidyverse)
view(starwars)
attach(starwars)

# Data types 

glimpse(starwars)
class(gender)
unique(gender)

starwars$gender <- as.factor(starwars$gender) ## categorical variables 
class(starwars$gender)

levels(starwars$gender) ## levels 

starwars$gender <- factor((starwars$gender),
                          levels = c("Masculine",
                                     "Feminine"))
levels(starwars$gender)

## Select variables 

starwars %>% 
  select(name,height,ends_with("color"))

starwars %>% 
  select(name,height,ends_with("color")) %>% 
  names()


## filter 

unique(hair_color)

starwars %>% 
        select(name,height,ends_with("color")) %>% 
        filter(hair_color %in% c("blond","brown") & 
              height < 180)


## Missing data 

mean(height)
mean(height,na.rm = TRUE)


starwars %>% 
    select(name,gender,hair_color,height)

starwars %>% 
     select(name,gender,hair_color,height) %>% 
     na.omit()

starwars %>% 
    select(name,gender,hair_color,height) %>% 
    filter(!complete.cases(.))

starwars %>% 
    select(name,gender,hair_color,height) %>% 
    filter(!complete.cases(.)) %>% 
    drop_na(height)

starwars %>% 
  select(name,gender,hair_color,height) %>% 
  drop_na(height) %>% 
   view()

starwars %>% 
  select(name,gender,hair_color,height) %>% 
  filter(!complete.cases(.)) %>% 
  mutate(hair_color = replace_na(hair_color,"none"))


## Duplicates 

Names <- c("Peter","John","Andrew","Peter")
Age <- c(22,33,44,22)

friends <- data.frame(Names,Age)
duplicated(friends)

friends[duplicated(friends),]     

friends[!duplicated(friends),]     

Clean_data <-friends %>% 
                 distinct()    
view(Clean_data)


# Recording Variables 

starwars %>% 
      select(name,gender) %>% 
       mutate(gender = recode(gender,
                        "Masculine" = 1,
                        "Feminine"= 2)) %>% 
                       view()

# Filter and arrange data

msleep %>% 
  filter((order == "Carnivora" |
            order == "Primates") &
           sleep_total > 8) %>% 
  select(name,order,sleep_total) %>% 
  arrange(-sleep_total) %>% 
  View()

    ####### other options 

msleep %>% 
  filter(order %n% c("Carnivora","Primates") &
           sleep_total > 8) %>% 
  select(name,order,sleep_total) %>% 
  arrange(order) %>% 
  view()

# Change observations (Mutate)


msleep %>% 
  mutate(brainwt = brainwt * 1000) %>% 
  view()

msleep %>%  
  mutate(brain_in_grams = brainwt * 1000) %>% 
  view()


# Conditional formatting (if_else)
#logical vector based on a condition 

msleep$brainwt
msleep$brainwt > 0.01


size_of_brain <- msleep  %>% 
     select(name,brainwt) %>% 
    drop_na(brainwt) %>% 
    mutate(brain_size = if_else(brainwt > 0.01,
                                "large",
                                "small")) %>% 
                                 view()


# Recode data and rename a variable 
# change observations of "large" and "small" into 1 and 2 

size_of_brain %>% 
        mutate(brain_size = recode(brain_size,
                                  "large" = 1,
                                  "small" = 2)) %>% 
                                  view()
# Reshaping the data 
?gapminder
library(gapminder)
view(gapminder)


data <- select(gapminder,country,year,lifeExp)
view(data)

wide_data <- data %>% 
      pivot_wider(names_from = year,values_from = lifeExp )

view(wide_data)

long_data <- wide_data %>%  
             pivot_longer(2:13,
                          names_to = "year",
                          values_to = "lifeExp")
view(long_data)


## Describe and summaries

# Range / spread 
# centrality
# variance
# summarize your data 
# create tables 


library(tidyverse)
attach(msleep)

min(awake)
max(awake)
msleep %>% 
  select(awake,brainwt) %>% 
  summary()

# create a summary table 

msleep %>% 
  drop_na(vore) %>% 
  group_by(vore) %>% 
  summarise(Lower = min(sleep_total),
            Average = mean(sleep_total),
            Upper = max(sleep_total),
            Difference = 
              max(sleep_total)- min(sleep_total)) %>% 
             arrange(Average) %>% 
              view()

## creating contingency tables 

library(MASS)
attach(Cars93)

glimpse(Cars93)

table(Origin)

table(AirBags,Origin)

addmargins(table(AirBags,Origin),1)

addmargins(table(AirBags,Origin),2)

addmargins(table(AirBags,Origin))

### proportions 

prop.table(table(AirBags,Origin),1)*100
prop.table(table(AirBags,Origin),2)*100

round(prop.table(table(AirBags,Origin),2)*100
)


Cars93 %>% 
    group_by(Origin,AirBags) %>% 
    summarise(number = n()) %>% 
    pivot_wider(names_from = Origin,
                values_from = number)



## printing output #

sink("output.txt")
print(names(cars93))
print(summary(cars93))
sink()


Cars93
view(Cars93)
attach(Cars93)


### T-test and Analysis of Variance 

setwd("C:/Users/HP/OneDrive/Desktop/R")


#install.packages('agricolae')  # install.package agricolae

#install.packages('devtools')
library(devtools)

#install_github("cran/cwhmisc") # install package cwhmisc
 
#install.packages('doBy')
library(doBy)

#install.packages('agricolae')

library(agricolae)

#install.packages("cwhmisc")
library(cwhmisc)

#install.packages('lattice')
library(lattice)
install.packages('effects')
library(effects)
#install.packages('multcomp')
library(multcomp)
#install.packages('ggplot2')
library(ggplot2)
#install.packages('car')
library(car)


## Test test 

library(readr)
mydata <- pollination <- read_csv("pollination.csv")
View(pollination)
head(mydata)
glimpse(mydata)

attach(mydata)

t.test(crosspol, selfpol, paired = TRUE ,alternative = "two.sided")

# ignore the fact that we are dealing with pair samples

t.test(crosspol,selfpol,paired = FALSE, var.equal = TRUE ,alternative = 'two.sided' )
detach(mydata)


# independent variables 

library(readr)

newts <- read_csv("newts.csv")
View(newts)

attach(newts)

par(mfrow = c(1,1))

boxplot(length~sex,ylab = "Length(cm)",col = "green")

#### t.test 


t.test(length~sex,paired = FALSE,var.equal = TRUE ,alternative = "two.sided")


### 
A <- c(27,28,30)
A
var(A)
sd(A)

B <- c(15,13,10)
cd <- c(20,19,22)
D <- c(40,42,50)


bonetime <- c(A,B,cd,D)

TreatA <- rep("A",3)
TreatB <- rep("B",3)
TreatC <- rep("cd",3)
TreatD <- rep("D",3)

Treatment <- c( TreatA,TreatB,TreatC,TreatD)
Treatment

Reps <- c(rep(c(1,2,3),4))
Reps

bonedata <- cbind(Reps,Treatment,bonetime)  ## Data frames 
bonedata
str(bonedata)

bonedata <- data.frame(bonedata)
names(bonedata)
bonedata$Reps <- as.factor(bonedata$Reps)  ### making Reps as a factor 
bonedata$Treatment <- as.factor(bonedata$Treatment)
bonedata$bonetime <- as.numeric(bonedata$bonetime)

str(bonedata)

## 

boxplot(bonetime~Treatment,data = bonedata,main = "Boxplot")


# aggregates 

aggregate(bonetime~Treatment,bonedata,summary) # summary
aggregate(bonetime~Treatment,bonedata,var) # variance
aggregate(bonetime~Treatment,bonedata,sd) # standard deviation
aggregate(bonetime~Reps,bonedata,summary) # summary 
aggregate(bonetime~Treatment,bonedata,var) # variance
aggregate(bonetime~Treatment,bonedata,sd)  # standard deviation   


# gtsummary package 

mtcars
library(tidyverse)
library(gtsummary)

data <- trial %>% select(trt,age,grade,response)

data

?trial

# summarize data 

data$trt

tbl_summary(data, by= trt)


data %>% 
  tbl_strata(
    strata = grade,
    ~.x %>% 
      tbl_summary(by = trt))


# Summarize statistical tests 

data %>% 
  tbl_summary( by = trt) %>% 
    add_difference() %>% 
    add_q() %>% 
    add_overall() %>% 
    add_n() %>% 
    add_ci() %>% 
    add_stat_label(
        label = all_continuous()~"Median(IQR)")


trial %>% 
  tbl_summary(
    by = trt,
    statistic = all_continuous()~"{mean} ({sd})") %>% 
  add_p(test = list(
    all_continuous() ~ "t.test",
    all_categorical() ~ "fisher.test"))

trial %>% 
  tbl_summary(
    by = trt,
    statistic = list(
      age ~ "{mean} ({sd})",
      marker ~ "{mean} ({min},{p25}, {p75},{max})",
      stage ~ "{n}/ {N} ({P}%)")) %>% 
  add_p(test = list(
     age ~ "t.test",
     marker ~ "wilcox.test",
     stage ~ "fisher.test")) %>% 
  separate_p_footnotes()


trial %>% 
  tbl_summary(
    by = trt,
    statistic = list(
      age ~ "{mean} ({sd})",
      marker ~ "{mean} ({min},{p25}, {p75},{max})",
      stage ~ "{mean} ({min},{p25}, {p75},{max})"),  # Change the summary type to continuous for stage
    type = list(stage ~ 'continuous')) %>%  # Specify stage as a continuous variable
  add_p(test = list(
    age ~ "t.test",
    marker ~ "wilcox.test",
    stage ~ "wilcox.test")) %>%  # Use Wilcoxon rank-sum test for stage
  separate_p_footnotes()

# paired test 1

trial_paired <- 
  trial %>% 
  select(trt,marker,response) %>% 
  group_by(trt) %>% 
  mutate(id = row_number()) %>% 
  ungroup()

# Paired test 2

trial_paired %>% 
  filter(complete.cases(.)) %>% 
  group_by(id) %>% 
  filter(n() == 2) %>% 
  ungroup() %>% 
  tbl_summary(by = trt,include = -id) %>% 
  add_p(test = list(
    marker ~ "paired.t.test",
    response ~ "mcnemar.test"),
    group = id)



trial %>% 
  tbl_summary(
    by = trt,
    statistic = age ~ "{mean} ({sd})",
    label = grade ~ "New Name - Tumor ~ Tumor Grade",
    digits = all_continuous() ~ 1,)
    
    # missing = "no"

trial %>%
  tbl_summary(
    missing_text = "Missing values",
    type = list(
      response ~ "categorical",
      death ~ "categorical"
    ),
    sort = everything() ~ "frequency",
    percent = "cell",
    include = -ttdeath
  ) %>% 
  add_p(
    pvalue_fn = ~style_pvalue(.x, digits = 3)
  )



# summarize regression models 
library(arm)
bm <- arm::bayesglm(
  response ~ trt + age + grade,
  data = trial, family = binomial)

bm_table <- tbl_regression(bm, exponentiate = TRUE)

bm_table

library(survival)

cm <- coxph(
        Surv(ttdeath,death) ~ trt + age + grade,
        data = trial)

cm_table <- tbl_regression(cm,exponentiate = TRUE)
                           
cm_table



# several univariate models 
library(dplyr)

uvml_table <- trial %>% 
  select(response, trt, age, grade) %>% 
  tbl_uvregression(
    method =  glm,
    y      = response,
    method.args = list(family = binomial),
    exponentiate = TRUE)

uvml_table

library(tidyverse)
library(gtsummary)

uvml_table <- trial %>% 
  select(trt,age,marker,grade,response) %>% 
  tbl_uvregression(
    method = glm,
    y = response,
    method.args = list(family = binomial),
    exponentiate = TRUE
  )

uvml_table

library(arm)

uvcm_table <- tbl_uvregression(
  trial %>% 
    select(ttdeath,death,trt,age,grade),
  method = coxph,
     y    = Surv(ttdeath,death),
   exponentiate = TRUE)

uvcm_table


# side - by side Regression model 

library(gt)
fancy_table <- 
  tbl_merge(
    tbls = list(bm_table,cm_table),
    tab_spanner('Tumor Response','Time to Death')
     )

fancy_table



# univariate and multivariate Analysis in 1 table

uni_multi <- tbl_merge(
  tbls = list(
    tbl_summary(data),uvml_table,bm_table),
  tab_spanner = c(
    "**Describe**",
    "**Univariate Models**",
    "**Multivariate**")
    
  )

uni_multi


?gml

# gml 

glm(response ~ trt+ age + marker + ttdeath + grade,
    data = trial , family = binomial) %>%  
  tbl_regression(
    # pvalue fun = ~ style_pvalue(.x, digits = 3),
    exponentiate = TRUE
  ) %>% 
  #add_helpers
  
  add_n(location = "level") %>% 
  add_nevent(location = "level") %>% 
  add_global_p() %>% 
  add_significance_stars(
    hide_p = F,hide_se = T , hide_ci = F) %>% 
  add_vif() %>% 
  #modify * helpers 
  modify_header(label = "**Predictor**") %>% 
  modify_caption("Table1.Cool looking table!") %>% 
  modify_footnote(
    ci = "CI = Credible Intervals are increadible :)",
    abbreviation = TRUE ) %>% 
  sort_p() %>% 
  # ascethics helpers 
  
  bold_p(t = 0.10 , q = TRUE) %>% 
  bold_labels() %>%   #bold levels 
  italicize_levels()

library(flextable)

fancy_table  %>% 
  as_flex_table() %>% 
  save_as_image(path = "fancy_table.png")

  
fancy_table  %>% 
  as_flex_table() %>% 
  save_as_docx(path = "fancy_table.docx")

fancy_table














  

  