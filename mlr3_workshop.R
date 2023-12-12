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



mlr_tasks
as.data.table(mlr_tasks)[task_type == 'surv'] # tasks to "play"

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
task$missings() # missing values!
task$truth()
task$status()
task$times()
