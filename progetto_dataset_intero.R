library(magrittr)
library(ggplot2)
library(ggpubr)
library(tidyverse)
library(Metrics)
library(caret)
library(cowplot)
dataset_filtered = read.csv('train_NoStd.csv', sep = ';')
train_set = dataset_filtered[c(1:14884),colSums(is.na(dataset_filtered))<nrow(dataset_filtered)]
fit = lm(critical_temp~.-critical_temp,data = train_set)
fit_parameters = summary(fit)

#Stampo i risultati del fit in file.txt
sink('fit.txt')
print(fit_parameters)
sink()

#Apertura dataset di test
test_set = dataset_filtered[c(14884:21263),colSums(is.na(dataset_filtered))<nrow(dataset_filtered)]

#outputs previsti del test_set
predicted_outputs = predict(fit, newdata = test_set[,-66])

"# Voglio calcolare le differenze tra i real_outputs (gli output del test data) e i predicted_outputs per WTDMEAN
mean_critical_temp = mean(test_set$critical_temp)
median_critical_temp = median(test_set$critical_temp)
real_outputs = test_set[,66]
differences = real_outputs - predicted_outputs
mean_difference = mean(differences)
mean_relative_error = mean_difference/mean(real_outputs)
median_difference = median(differences)
median_relative_error = median_difference/median(real_outputs)

# Voglio plottare le differences
grafico_differences = plot(c(1:6380),differences, main = 'Differences between real and predicted outputs', xlab = 'Observation index',ylab = 'Difference',col = 'green')
abline(h = mean_difference, col = 'red')
abline(h = median_difference, col = 'blue')"

# Model performance (eseguono la stessa cosa)
RMSE_test_intero = rmse(predicted_outputs, test_set$critical_temp)
#RMSE_intero = sqrt(mean(differences^2))


# Voglio determinare il miglior subset di variabili
library(leaps)
backward_subset = regsubsets(critical_temp ~.-critical_temp , data = train_set, nvmax = 25, really.big = TRUE, method = "backward") #backward
forward_subset = regsubsets(critical_temp ~.-critical_temp , data = train_set, nvmax = 25, really.big = TRUE, method = "forward") #forward
#exh_subset = regsubsets(critical_temp ~.-critical_temp , data = train_set, nvmax = 25, really.big = TRUE, method = "exhaustive") exhaustive
summary_backward = summary(backward_subset)
summary_forward = summary(forward_subset)
#summary_exh = summary(exh_subset)

#Trasformo i cp in dataframe per poterli così plottarli con ggplot2
df_cp_backward = data.frame(unclass(summary_backward$cp), check.names = FALSE, stringsAsFactors = FALSE)
df_cp_forward = data.frame(unclass(summary_forward$cp), check.names = FALSE, stringsAsFactors = FALSE)
df_r2adj_forward = data.frame(unclass(summary_forward$adjr2), check.names = FALSE, stringsAsFactors = FALSE)
#df_cp_exh = data.frame(unclass(summary_sub_sel_exh$cp), check.names = FALSE, stringsAsFactors = FALSE)
names(df_cp_backward) = c('cp')
names(df_cp_forward) = c('cp')
"
par(mfrow=c(2,2))

# Ggplot dei Cp
df_cp_backward %>% 
    ggplot(aes(x = c(1:65), y = cp)) +       # mappatura delle variabili
    geom_point(shape=21, color='blue', fill ='blue' ) +
    geom_line( color='red') +
    theme_classic() +
    ggtitle('Backward: Cp vs Number of predictors') +
    xlab('Number of predictors') +
    ylab('Cp')

df_cp_forward %>% 
  ggplot(aes(x = c(1:65), y = cp)) +       # mappatura delle variabili
  geom_point(shape=21, color='blue', fill ='blue' ) +
  geom_line( color='red') +
  theme_classic() +
  ggtitle('Forward: Cp vs Number of predictors') +
  xlab('Number of predictors') +
  ylab('Cp')

plot(summary_backward$rss, xlab = 'Number of Variables', ylab = 'RSS', type = 'l', main = 'Backward')
plot(summary_backward$adjr2, xlab = 'Number of Variables', ylab = 'Adjusted R^2', type = 'l',main = 'Backward')
plot(summary_forward$rss, xlab = 'Number of Variables'', ylab = 'RSS', type = 'l',main = 'Forward')
plot(summary_forward$adjr2, xlab = 'Number of Variables', ylab = 'Adjusted R^2', type = 'l',main = 'Forward')
"
plot(summary_forward$adjr2, xlab = 'Number of Variables', ylab = 'Adjusted R^2', type = 'l',main = 'Forward')
abline(h=0.66, col = 'red')
#adj_r2_max = which.max(summary_forward$adjr2)




