library(magrittr)
library(ggplot2)
library(ggpubr)
library(tidyverse)
library(Metrics)
library(caret)
library(cowplot)
library(pls)

#Preparazione dei dataset di train e di test
dataset_filtered = read.csv('train_NoStd.csv', sep = ';')
set.seed(1)
data_set_filtered_correct = dataset_filtered[c(1:21263),colSums(is.na(dataset_filtered))<nrow(dataset_filtered)]
partition = createDataPartition(data_set_filtered_correct$critical_temp,times = 1,p = 0.7, list = FALSE,groups = min(5, length(data_set_filtered_correct$critical_temp)))
train_set = data_set_filtered_correct[partition,]
test_set = data_set_filtered_correct[-partition,]

#Fit multilineare
fit = lm(critical_temp~.-critical_temp,data = train_set)
fit_parameters = summary(fit)

#Stampo i risultati del fit in file.txt
sink('fit.txt')
print(fit_parameters)
sink()

#outputs previsti del test_set
predicted_outputs = predict(fit, newdata = test_set[,-66])

"
# Voglio calcolare le differenze tra i real_outputs (gli output del test data) e i predicted_outputs per WTDMEAN
mean_critical_temp = mean(test_set$critical_temp)
median_critical_temp = median(test_set$critical_temp)
real_outputs = test_set[,66]
differences = real_outputs - predicted_outputs
mean_difference = mean(differences)
mean_relative_error = mean_difference/mean(real_outputs)
median_difference = median(differences)
median_relative_error = median_difference/median(real_outputs)

# Voglio plottare le differences
grafico_differences = plot(c(1:6377),differences, main = 'Differences between real and predicted outputs', xlab = 'Observation index',ylab = 'Difference',col = 'green')
abline(h = mean_difference, col = 'red')
abline(h = median_difference, col = 'blue')
"

# Performace fit multilineare con il test_set
RMSE_test_intero = rmse(predicted_outputs, test_set$critical_temp)
MSE_test_intero = mse(test_set$critical_temp, predicted_outputs)
#RMSE_intero = sqrt(mean(differences^2))


# Voglio determinare il miglior subset di variabili
library(leaps)
backward_subset = regsubsets(critical_temp ~.-critical_temp , data = train_set, nvmax = 66, really.big = TRUE, method = "backward") #backward
forward_subset = regsubsets(critical_temp ~.-critical_temp , data = train_set, nvmax = 66, really.big = TRUE, method = "forward") #forward
#exh_subset = regsubsets(critical_temp ~.-critical_temp , data = train_set, nvmax = 25, really.big = TRUE, method = "exhaustive") exhaustive
summary_backward = summary(backward_subset)
summary_forward = summary(forward_subset)
#summary_exh = summary(exh_subset)

#Grafici statistiche subsets
par(mfrow=c(2,2))
plot(summary_backward$cp, xlab = 'Number of Variables', ylab = expression('C'['p']), type = 'l', main = 'Backward', col = 'red')
min_CP_backward = which.min(summary_backward$cp)
min_CP_plot_backward = points(min_CP_backward,summary_backward$cp[min_CP_backward],col = 'blue', cex = 2, pch = 20)
plot(summary_backward$adjr2, xlab = 'Number of Variables', ylab = 'Adjusted R^2', type = 'l',main = 'Backward', col = 'red')
max_adjr2_backward = which.max(summary_backward$adjr2)
max_adjr2_plot_backward = points(max_adjr2_backward,summary_backward$adjr2[max_adjr2_backward],col = 'blue', cex = 2, pch = 20)
plot(summary_forward$cp, xlab = 'Number of Variables', ylab = expression('C'['p']), type = 'l',main = 'Forward',col = 'red')
min_CP_forward = which.min(summary_forward$cp)
min_CP_plot_forward = points(min_CP_forward,summary_forward$cp[min_CP_forward],col = 'blue', cex = 2, pch = 20)
plot(summary_forward$adjr2, xlab = 'Number of Variables', ylab = 'Adjusted R^2', type = 'l', main = 'Forward', col = 'red')
max_adjr2_forward = which.max(summary_forward$adjr2)
max_adjr2_plot_forward = points(max_adjr2_forward,summary_forward$adjr2[max_adjr2_forward],col = 'blue', cex = 2, pch = 20)

#validation (un po' inutile)
test_set.mat = model.matrix(critical_temp ~ .-critical_temp, data = test_set)
val.errors = rep(NA,65)
for(i in 1:65){
  
  coefi = coef(forward_subset, id = i)
  pred = test_set.mat[,names(coefi)]%*%coefi
  val.errors[i] = mean((test_set$critical_temp-pred)^2)
  
}

variables_validation = coef(forward_subset,which.min(val.errors))

#creo un metodo predict per regsubsets
predict.regsubsets = function(object, newdata, id, ...){
  
  form = as.formula(object$call[[2]])
  mat = model.matrix(form, newdata)
  coefi = coef(object, id = id)
  xvars = names(coefi)
  mat[,xvars]%*%coefi
  
}

forward_subset_dataset_intero = regsubsets(critical_temp ~ .-critical_temp, data = data_set_filtered_correct, nvmax = 66, method = 'forward', really.big = TRUE)
coef(forward_subset_dataset_intero,60)

# Cross-validation
k = 10
n = nrow(data_set_filtered_correct)
folds = sample(rep(1:k,length = n))
cv.errors = matrix(NA, k, 65, dimnames = list(NULL, paste(1:65)))

for(j in 1:k){
  
  best.fit = regsubsets(critical_temp ~ .-critical_temp, data = data_set_filtered_correct[folds!=j,], nvmax = 66, method = 'forward', really.big = TRUE)
  for(i in 1:65){
    
    pred = predict(best.fit, data_set_filtered_correct[folds==j,], id=i)
    cv.errors[j,i] = mean((data_set_filtered_correct$critical_temp[folds==j]-pred)^2)
    
  }
  
}

mean.cv.errors = apply(cv.errors,2,mean)
which.min(mean.cv.errors)
plot(mean.cv.errors, type = 'b')
subset_dataset_intero = regsubsets(critical_temp ~ .-critical_temp, data = data_set_filtered_correct, nvmax = 66, really.big = TRUE, method = 'forward')
coeff_subset_dataset_intero = coef(subset_dataset_intero, 64)


#PLS regression
set.seed(1)
pls.fit = plsr(critical_temp ~ ., data = train_set, scale = TRUE, validation = 'CV')
summary_pls = summary(pls.fit)  #49 è il numero delle componenti migliori
plot_pls = validationplot(pls.fit, val.type = 'RMSE')
pls.pred = predict(pls.fit, test_set, ncomp = 49)
MSE_test_pls = mean((pls.pred - test_set$critical_temp)^2)
RMSE_test_pls = sqrt(MSE_test_pls)
pls.fit_def = plsr(critical_temp ~ ., data = data_set_filtered_correct, scale = TRUE, ncomp = 49)
summary(pls.fit_def)

#Principal component regression
set.seed(1)
pcr.fit = pcr(critical_temp ~ ., data = data_set_filtered_correct, scale = TRUE, validation = 'CV')
summary_pcr = summary(pcr.fit)
plot_pcr = validationplot(pcr.fit, val.type = 'RMSE', type = 'l', col = 'blue')
pcr.pred = predict(pcr.fit, test_set, ncomp = 49)
MSE_test_pcr = mean((pcr.pred - test_set$critical_temp)^2)
RMSE_test_pcr = sqrt(MSE_test_pcr)
pcr.fit_def = pcr(critical_temp ~ ., data = data_set_filtered_correct, scale = TRUE, ncomp = 49)
summary(pcr.fit_def)

#pcr vs pls
plot_pcr
plot_pls = validationplot(pls.fit, val.type = 'RMSE', type = 'l', col = 'red', add = TRUE)
legenda = legend(50, 30, legend = c('PCR','PLS'), col = c('blue','red'),lty=1, cex=0.8, )

#Forward subset regression
forward.fit = lm(critical_temp ~ .-wtd_range_atomic_mass-wtd_mean_fie-wtd_gmean_fie-entropy_fie-mean_atomic_radius-mean_Density-wtd_range_Density-wtd_mean_ElectronAffinity-gmean_ElectronAffinity-mean_FusionHeat-wtd_mean_FusionHeat-gmean_FusionHeat-wtd_gmean_FusionHeat-mean_ThermalConductivity-entropy_ThermalConductivity-entropy_Valence-critical_temp, data = train_set)
forward.pred = predict(forward.fit, test_set, ncomp = 49)
MSE_test_forward = mean((forward.pred - test_set$critical_temp)^2)
RMSE_test_forward = sqrt(MSE_test_forward)

#lasso regression
library(glmnet)
x_dataset = model.matrix(critical_temp ~ ., data = data_set_filtered_correct)
y_dataset = data_set_filtered_correct$critical_temp
x_train = model.matrix(critical_temp ~ ., train_set)[,-1]
y_train = train_set$critical_temp
x_test = model.matrix(critical_temp ~ ., test_set)[,-1]
y_test = test_set$critical_temp
lambdas = 10^seq(2, -3, by = -.1)
lasso.mod = glmnet(x_train, y_train, alpha = 1, lambda = lambdas)
library(plotmo) # for plot_glmnet
plot_lasso_mod = plot_glmnet(lasso.mod)
set.seed(1)
cv.out = cv.glmnet(x_train, y_train, alpha = 1)
plot_cv_out = plot(cv.out)
bestlam = cv.out$lambda.min
lasso.pred = predict(lasso.mod, s = bestlam, newx = x_test)
MSE_lasso = mean((lasso.pred - y_test)^2)
RMSE_lasso = sqrt(MSE_lasso)
out = glmnet(x_dataset, y_dataset, alpha = 1, lambda = lambdas)
lasso.coef = predict(out, type = 'coefficients', s = bestlam)[1:66,]

# spline cubiche di regressione
library(splines)
spline_fit = lm(critical_temp ~ bs(. -wtd_range_atomic_mass-wtd_mean_fie-wtd_gmean_fie-entropy_fie-mean_atomic_radius-mean_Density-wtd_range_Density-wtd_mean_ElectronAffinity-gmean_ElectronAffinity-mean_FusionHeat-wtd_mean_FusionHeat-gmean_FusionHeat-wtd_gmean_FusionHeat-mean_ThermalConductivity-entropy_ThermalConductivity-entropy_Valence-critical_temp), data = data_set_filtered_correct)




