library(mlr3verse)
library(mlr3proba)
library(mlr3extralearners)
library(tidyverse)
library(skimr)

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
poe$help() # see methods
poe = po("encode", method = "one-hot")

poe$train(input = list(task))
t2 = poe$train(input = list(task))[[1]] 

poe2 = po("encode", method = "treatment") # treatment best if only 2 levels

poe2$train(input = list(task))
t3 = poe2$train(input = list(task))[[1]] 
t3$data(cols = "sex")
t2$data(cols = "sex")
task$data(cols = "sex")
# want ideally to have factors with 2 levels as 0 and 1 since all packages/models can handle this, but not all handle factors
# here originally m/f, now made into 0/1 with treatment, and made 2 variables in one-hot encoding
# one-hot can be useful if you want all levels as separate variables



# Model-based missing data imputation
?mlr_pipeops_imputelearner
po_imp = po("imputelearner", learner = lrn('regr.rpart'))
task


# Learners - CoxPH, KM
# how to get info about a model? help()

cox = lrn("surv.coxph")
km = lrn("surv.km")
surv_tree = lrn("surv.rpart")

# Train + test split
part = partition(task)

# CoxPH

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

