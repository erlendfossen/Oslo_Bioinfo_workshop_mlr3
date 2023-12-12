library(mlr3verse)
library(mlr3proba)
library(mlr3extralearners)
library(tidyverse)
#library(skimr)

# Survival Tasks
task=tsk("lung") # use lung dataset
task

?survival::lung
class(survival::lung)
class(task) # R6, task is an object, not just a dataframe
task$censtype #right censored
task$status() # use $ to get info. 1 is death, 0 censored (or alive)
task$nrow
task$ncol
task$unique_times()
task$target_names # this is for survival, if it was classification it would only be one target (e.g. health/sick)
task$truth() # + for event

task=tsk("grace")
task=tsk("whas")
mlr_tasks  #all available tasks

# which task avaialbe for survival
as.data.table(mlr_tasks)[task_type == 'surv'] # tasks to "play"

autoplot(task) # kaplain meier plot
task$kaplan() #survfit object
plot(task$kaplan()) # kaplan meier with CI

# make dataset into a task ----
?as_task_surv
data = survival::rats
data$sex = factor(data$sex, levels = c("f", "m"))
tsk_rats = as_task_surv(x = data, time = "time",
                        event = "status", type = "right", id = "rats")
tsk_rats$head()

lung = survival::lung
lung %>% as_tibble()
skimr::skim(lung)

lung$status = (lung$status == 2L) # 2 is death so convert to 1
lung = lung %>% select(-inst) # remove Institution code (irrelevant for us)
lung$ph.ecog = as.integer(lung$ph.ecog)

task = as_task_surv(x = lung, time = 'time', event = 'status', id = 'lung')
## Note: sex here is as double, need to convert to factor
task$missings() # missing values!
task$truth()
task$status()
task$times()


# other code ----
# Preprocessing pipeline!
?mlr_pipeops

# Encode factors
task=tsk("lung")
task # want sex as factor
po("encode") #pipe operator
poe <- po("encode")
poe$input
poe$output
poe$param_set # one-hot default
poe$help() # see methods
poe = po("encode", method = "one-hot")

poe$train(input = list(task)) # make list since can do this for many tasks/datasets
t2 = poe$train(input = list(task))[[1]] 

poe2 = po("encode", method = "treatment") # treatment best if only 2 levels

poe2$train(input = list(task))
t3 = poe2$train(input = list(task))[[1]] 
t3$data(cols = "sex")
t2$data(cols = "sex")
task$data(cols = "sex")
# want ideally to have factors with 2 levels as 0 and 1 since all packages/models can handle this, but not all handle factors
# here originally m/f, now made into 0/1 with treatment, and made 2 variables in one-hot encoding
# one-hot can be useful if you want all levels as separate variables. With treatment, the baseline factor is important
## OBS: one-hot is not possible if binary since one variable is exactly redundant to the other one (just flipped 0 and 1s) 



# Model-based missing data imputation ----
mlr_pipeops #many different pipes
?mlr_pipeops_imputelearner
task <- t3
task$missings()

poimpmean <- po("imputemean")
poimpmean$param_set # affect colums is the columns you want to do the missing on etc

taskimp <- poimpmean$train(input = list(task))[[1]]
taskimp$missings()

po_imp = po("imputelearner", learner = lrn('regr.rpart'))
learner <- lrn("regr.rpart") #regression tree, from rpart package
learner$param_set
learner 

po_imp2 = po("imputelearner", learner = learner)
po_imp2$param_set
taskimp2 <- po_imp2$train(input = list(task))[[1]]
taskimp2$missings()

# could use random forest 
lrn("regr.ranger") #note that it does NOT support missing values. If it did, it would be under properties

poi_ranger = po("imputelearner", 
                learner = po("imputemean") %>>% lrn("regr.ranger"))
## here it first imputes on mean and uses this to impute the values again
## %>>% is specific for the mlr3 pipeline
?'%>>%'
poi_ranger
taskimp3 <- poi_ranger$train(input = list(task))[[1]]
taskimp3
taskimp3$data(cols="wt.loss")[[1]]
task$data(cols="wt.loss")[[1]]

# OBS: if missing survival time, cannot impute! Remove subject or look at unsupervised clustering based on only features

# Learners - CoxPH, KM ----

task <- taskimp3
skimr::skim(survival::lung) # good way to get idea of the data

# how to get info about a model? help()

cox = lrn("surv.coxph")
cox #object etc
cox$help()

km = lrn("surv.kaplan")
surv_tree = lrn("surv.rpart")

# Train + test split
part = partition(task, ratio = 0.8) # will by default be stratified
## Stratification for ML: stratified by target, here based on status.
## can stratify on other values if you want
task_strata = task$col_roles$stratum = "sex" #make sex a stratum
part0 <- mlr3::partition(task=task_strata)  # code did not work, but get an idea and can check myself later

### Can be a problem if you end up with a different proportion of patients that were censored in train vs test
table(task$status()) # censored and dead
prop.table(table(task$status())) # proportion, 28% censored

prop.table(table(task$status(rows=part$train))) # proportion, 27% censored
prop.table(table(task$status(rows=part$test))) # proportion, 28% censored
## similar proportions due to stratification

part$train # which subjects are used for training
task$missings()

# CoxPH
cox$model # empty
cox$train(task, row_ids = part$train)
cox$model # gives the cox model!
summary((cox$model)) # summary as normal etc
## here using all covariates, may want to use fewer, probably possible to specify
task$formula() # ~ . ## i.e. all covariates

cox$reset() # removes model
cox$train(task) #testing for all patients, not only for train
cox$model # gives the cox model
summary((cox$model))

cox$reset()

# KM

# Prediction types
# distr, crank, lp, response

# Evaluate performance
mlr_measures$keys(pattern = "surv")

# measures
mlr_measures$keys(pattern = "surv")

# discrimination
m = msr("surv.cindex") # harrell's c-index

# calibration
m = msr("surv.dcalib")

# putting it all together to a resample/benchmark

