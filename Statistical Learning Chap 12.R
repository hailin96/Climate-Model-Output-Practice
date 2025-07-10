# Statistical Learning Chapter 12

setwd("Documents/Documents/SFU/NSERC/USRA 2/Data/Temp")

remotes::install_github("mlr-org/mlr3extralearners@*release")
install.packages("mlr3proba", repos = "https://mlr-org.r-universe.dev")
install.packages("ooplah") #needed for ml3proba()
install.packages("dictionar6")

library(sf)
library(terra)
library(tmap)
library(spDataLarge)
library(dplyr)
library(future)             # parallel processing
library(lgr)                # logging framework for R
library(mlr3)               # unified interface to machine learning algorithms
library(mlr3learners)       # most important machine learning algorithms
library(mlr3extralearners)  # access to even more learning algorithms
library(mlr3proba)          # here needed for mlr3extralearners::list_learners()
library(mlr3spatiotempcv)   # spatiotemporal resampling strategies
library(mlr3tuning)         # hyperparameter tuning
library(mlr3viz)            # plotting functions for mlr3 objects
library(progressr)          # report progress updates
library(pROC)               # compute roc values

#Statistical learning identifies patterns then predicts based on those patterns

##Supervised - GLMs as there is a training dataset, we are modelling the relationship bw response and predictor variables

#Response: Binary, Categorical, Integer or Numeric

#Model the occurence of landslides -----------------------

#Using a support vector machine, a typical machine learning algorithm

#Predictive performance can be assessed using spatial cross-validation

data("lsl", "study_mask", package = "spDataLarge")
ta = terra::rast(system.file("raster/ta.tif", package = "spDataLarge"))

plot(study_mask, main = "Landslide Points") #mask shape

plot(lsl, main = "Landslide Points") #gridded
summary(lsl$lslpts)

class(lsl)

#convert landslide points to sf
lsl_sf = sf::st_as_sf(lsl, coords = c("x", "y"), crs = "EPSG:32717")
hs = terra::shade(slope = ta$slope * pi / 180,
                  terra::terrain(ta$elev, v = "aspect", unit = "radians"))
#so far tmaptools does not support terra objects
bbx = tmaptools::bb(raster::raster(hs), xlim = c(-0.0001, 1),
                    ylim = c(-0.0001, 1), relative = TRUE)
map = tm_shape(hs, bbox = bbx) +
  tm_grid(col = "black", n.x = 1, n.y = 1, labels.inside.frame = FALSE,
          labels.rot = c(0, 90), lines = FALSE) +
  tm_raster(col.scale = tm_scale(values = gray(0:100 / 100), n = 100), col.legend = tm_legend_hide()) +
  tm_shape(ta$elev) +
  tm_raster(col_alpha = 0.6, col.scale = tm_scale(values = hcl.colors(25, "Geyser")), col.legend = tm_legend_hide()) +
  tm_shape(lsl_sf) +
  tm_symbols(fill = "lslpts", size = 0.5, col = "white",
             fill.scale = tm_scale(values = c("#0071A6", "#C73000")), fill.legend = tm_legend(title = "Landslide: ")) +
  tm_layout(inner.margins = rep(0, 4), legend.bg.color = "white", legend.position = tm_pos_in())
map


#landslide occurence is a function of slope angle, plan curvature, profile curvature, elevation and water flow
fit = glm(lslpts ~ slope + cplan + cprof + elev + log10_carea,
          family = binomial(),
          data = lsl)

class(fit)

fit

#predicted probabilities of landslide occurence for each observation
pred_glm = predict(object = fit, type = "response")
head(pred_glm)

#spatial prediction 
#applies coefficients to predictor rasters
pred = terra::predict(ta, model = fit, type = "response")
class(pred)

#use my GLM model fit to predict landslide probability across the entire raster grid ta (terrain attributes)
#give me the actual predicted values (not log-odds)
#a pixel with a value of 0.87 = model thinks there's an 87% chance of landslide occurring at that location
pred
#on the legend is susceptibility to landslides
plot(pred, main= "Prediction?")

#Area under the receiver operator characteristic curve (AUROC)
#assesses the predictive performance of a binomial model
#bw 0.5 and 1.0 (the closer to 1, the more perfect a prediction is)
#roc computes auroc (response and predicted value as inputs) and auc computes area under curve
pROC::auc(pROC::roc(lsl$lslpts, fitted(fit)))

#spatial cross validation

#1. create task (make sure to drop geometry column)

task = mlr3spatiotempcv::as_task_classif_st(
  mlr3::as_data_backend(lsl), 
  target = "lslpts", 
  id = "ecuador_lsl",
  positive = "TRUE",
  coordinate_names = c("x", "y"),
  crs = "EPSG:32717",
  coords_as_features = FALSE
)

#2. specify learner
#classif.log_reg specifies binomial classification
#predicted probability for landslide occurrence bw 0 and 1
learner = mlr3::lrn("classif.log_reg", predict_type = "prob") 

learner$train(task)
learner$model

learner$model$formula
task$data()
learner$model

fit = glm(lslpts ~ ., family = binomial(link = "logit"), 
          data = select(lsl, -x, -y))
identical(fit$coefficients, learner$model$coefficients)

#3. specify resampling
#will compute 500 resampling partitions and 500 models
#auroc is still a performance score
resampling = mlr3::rsmp("repeated_spcv_coords", folds = 5, repeats = 100)

#error here - reduce verbosity
#lgr::get_logger("mlr3")$set_threshold("warn")
# run spatial cross-validation and save it to resample result glm (rr_glm)
#rr_spcv_glm = mlr3::resample(task = task,
#                             learner = learner,
#                             resampling = resampling)
#compute the AUROC as a data.table
#score_spcv_glm = rr_spcv_glm$score(measure = mlr3::msr("classif.auc"))
# keep only the columns you need
#score_spcv_glm = dplyr::select(score_spcv_glm, task_id, learner_id, 
#                               resampling_id, classif.auc)

#fixed auroc score
score = readRDS("12-bmr_score.rds")
score_spcv_glm = dplyr::filter(score, learner_id == "classif.log_reg", 
                               resampling_id == "repeated_spcv_coords")

mean(score_spcv_glm$classif.auc) |>
  round(2)


library(ggplot2)
#rename the levels of resampling_id
score[, resampling_id := as.factor(resampling_id) |>
        forcats::fct_recode("conventional CV" = "repeated_cv", 
                            "spatial CV" = "repeated_spcv_coords") |> 
        forcats::fct_rev()]
#create the boxplot
ggplot2::ggplot(data = score[learner_id == "classif.log_reg"], 
                mapping = ggplot2::aes(x = resampling_id, y = classif.auc)) +
  ggplot2::geom_boxplot(fill = c("lightblue2", "mistyrose2")) +
  ggplot2::theme_bw() +
  ggplot2::labs(y = "AUROC", x = "")