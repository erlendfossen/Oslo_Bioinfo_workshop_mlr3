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

lrn()# list of learners
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

# train a model
cox$train(task, row_ids = part$train)
cox$model

pred_test <- cox$predict(task = task, row_ids = part$test)
# Prediction types
# distr, crank, lp, response
# crank = general continous ranking (risk) -  higher, more risk
# lp = linear predictors (only for cox type, linear models). 1 lp per patient, here crank and lp is the same since linear
## 
#distr 
pred_test$response
pred_test$distr # from distr6 package
head(pred_test$data$distr) # matrix formant
## for each patient and each time point, starts at 1 and may go to 0 survival prob
plot(pred_test$data$distr[1,]) # surv curve for ind 1
plot(pred_test$data$distr[2,]) # surv curve for ind 1

mlr_learners$keys(pattern="surv") #surv models

#coxnet
coxnet = lrn("surv.cv_glmnet") #unclear if this is actually coxnet
coxnet
coxnet$train(task, row_ids = part$train)
coxnet$model
pred_test2 <- coxnet$predict(task = task, row_ids = part$test)
pred_test2

#surv tree
survtree = lrn("surv.rpart")
survtree
survtree$train(task, row_ids = part$train)
survtree$model
pred_test3 <- survtree$predict(task = task, row_ids = part$test)
pred_test3

# Evaluate performance
mlr_measures$keys(pattern = "surv")

pred_test
pred_test2
pred_test3

# C-index (default)
pred_test$score()
pred_test2$score()
pred_test3$score()

cindex = msr("surv.cindex")
cindex # object, minimize = F (so higher is better), 0.5 is no discrimination. Uses crank as predict type
cindex_uno <- msr("surv.cindex", weight_meth = "G2") # use Unos method (G2) to handle censoring (weighted cindex)

pred_test$score(measures = cindex_uno, task=task, train_set = part$train)
pred_test2$score(measures = cindex_uno, task=task, train_set = part$train)
pred_test3$score(measures = cindex_uno, task=task, train_set = part$train)

# measures
mlr_measures$keys(pattern = "surv")

# 3 types: discimination indexes, discrimination with time dependence, calibration etc

unoauc = msr("surv.uno_auc")
unoauc # requires lp predict type
unoauc$help()
unoauc$param_set #integrated uses an overal auc across time

unoauc$param_set$values$integrated = FALSE
unoauc$param_set$values$times = 100 # only check AUC at time = 100

#or:
unoauc = msr("surv.uno_auc", integrated=FALSE, times = 100)

unoauc = msr("surv.uno_auc", integrated=TRUE)
pred_test$score(measures = unoauc, task=task, train_set = part$train)
pred_test2$score(measures = unoauc, task=task, train_set = part$train)
pred_test3$score(measures = unoauc, task=task, train_set = part$train) # did not work becaus of no lp

# unos AUC(t) is better to run at specific times and have integrated = F

# discrimination
m = msr("surv.cindex") # harrell's c-index

# calibration
mcali= msr("surv.dcalib")
mcali
mcali$help()
pred_test$score(measures = mcali) 
pred_test2$score(measures = mcali)
pred_test3$score(measures = mcali)
## only works for cox here, as expected since distr predict type is needed

#brier score: combo of disc and calibration
sbrier = msr("surv.brier")
sbrier
sbrier$help()
pred_test$score(measures = sbrier) 
pred_test2$score(measures = sbrier)
pred_test3$score(measures = sbrier)


# putting it all together to a resample/benchmark ----

?resample
# bootstrap, CV, holdout etc

mlr_resamplings

# Cross validation (CV)
## here we do CV instead of train-test. Meaning its only internal validation. 
### But uses all data instead of only train data. 
### Note: here train-test is also only internal validation since we use the same dataset for all
rsmp("cv") # 10 fold default
rsmp("cv", folds=5)
rsmp_cv <- rsmp("cv", folds=5)
cox$reset()
task #task
cox #model

resample(task=task, learner=cox, resampling = rsmp_cv)
res <- resample(task=task, learner=cox, resampling = rsmp_cv)

res$score() #c index for each fold
res$score(mcali) # d calibration per fold
res$score(measures=c(mcali, cindex, sbrier)) # all types

rsmp_rcv = rsmp("repeated_cv", folds=5, repeats=5)
rsmp_rcv

res2 <- resample(task=task, learner=cox, resampling = rsmp_rcv)
res2
res2$score(measures=c(mcali, cindex, sbrier)) # all types
autoplot(res2) # boxplot of cindex

# benchmarking ----
?benchmark
task
cox
surv_tree
coxnet

design <- benchmark_grid(tasks = list(task), learners = list(cox, coxnet, surv_tree),
               resamplings = list(rsmp_cv))
design #what you want to do

bm <- benchmark(design = design, store_models = TRUE, store_backends = TRUE) 
#store backends stores train set etc that is needed to calculate e.g. uno auc

bm
bm$score(measures = c(mcali, cindex, sbrier))
autoplot(bm) #cindex
autoplot(bm, measure=mcali)

#add grace task/dataset
design2 <- benchmark_grid(tasks = list(task, tsk("grace")), learners = list(cox, coxnet, surv_tree),
                         resamplings = list(rsmp_cv))
design2
bm2 <- benchmark(design = design2, store_models = TRUE, store_backends = TRUE)
bm2$score(measures = c(mcali, cindex, sbrier))
autoplot(bm2)

# add KM model 

km = lrn("surv.kaplan")
km

design3 <- benchmark_grid(tasks = list(task, tsk("grace")), learners = list(cox, coxnet, surv_tree, km),
                          resamplings = list(rsmp_cv))
design3
bm3 <- benchmark(design = design3, store_models = TRUE, store_backends = TRUE)
bm3$score(measures = c(mcali, cindex, sbrier))
autoplot(bm3) # always cindex = 0.5 for KM
autoplot(bm3, measure=mcali) # but better calibration for KM
autoplot(bm3, measure=sbrier)
## Better calibration for KM, suggesting that using features may not give that much.
## KM can be used as a general benchmark

#Uno AUC: AUC for survival, don't get a confusion matrix in the same way since its time-dependent
## Can get AUC for specific time points, but can also get it for several specific time points
## similar can be done with briers scores, to get intrated values across time
## OK to integrate (and get one brier score), but not the best to integrate AUC


# imputation pipeline ----
task <- tsk("lung")
task
task$missings()

# make pipeline that first does factor encoding, then imputon -> train/predict/resample etc
# pipeline as a learner
# have task, have cox learner, have poe for encoding, have poi_tree as imputation

poi_tree <- po("imputelearner", learner=lrn("regr.rpart"))

gr <- poe2 %>>% poi_tree %>>% cox #pipeline
gr 
plot(gr) # shows the workflow that is done

res <- resample(task=task, learner=gr, resampling = rsmp_cv)
res$score() # works

grlrn <- as_learner(gr)
grlrn # make it the same type of object as other learners

# Can also add feature selection etc, or variables to tune etc and add in the graph learner (gr)

# more advanced model ----
#TCGA-BRCA
data=readRDS(file=gzcon(url("https://github.com/ocbe-uio/survomics/blob/main/data.rds?raw=True")))

task = mlr3proba::as_task_surv(x=data, time="time", event="status", id="BRCA-TCGA")
task # 52 features, 50 genes, 1047 patients

# cv_glmnet
lrn("cv_glmnet")
surv_glmnet <- lrn("surv.cv_glmnet")
surv_glmnet$help()
# penalty factor is currently 1 on each varaible, want to make age and ethnicity not penalized
surv_glmnet <- lrn("surv.cv_glmnet", 
                   penalty.factor=c(rep(1,length(task$feature_names)-2),0,0)) # the last 2 are not penalized

aorsf <- lrn("surv.aorsf")
rang <- lrn("surv.ranger")


design4 <- benchmark_grid(tasks = list(task), learners = list(surv_glmnet, aorsf, km),
                          resamplings = rsmp_cv) #could add ranger but time consuming
design4
bm4 <- benchmark(design = design4, store_models = TRUE, store_backends = TRUE)
bm4$score(measures = c(mcali, cindex, sbrier))
autoplot(bm4) # always cindex = 0.5 for KM
autoplot(bm4, measure=mcali) # 
autoplot(bm4, measure=sbrier)


