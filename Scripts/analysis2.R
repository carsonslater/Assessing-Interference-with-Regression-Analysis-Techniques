# Carson Slater -----------------------------------------------------------
# Second Iteration of Model Building for Prediction of Aggregate Int. -----
# Began 1.17.2024 ---------------------------------------------------------


# Sourcing Old File for Update Environment

source(here::here("analysis1.R"))
library("plotly")

# Data Transformation -----------------------------------------------------

aggregate_data <- aggregate_data |> 
  mutate(transform1 = distance^(-1),
         transform2 = distance^(-1/2),
         transform3 = distance^(-1/4),
         transform4 = distance^(-1/8),
         transform5 = distance^(-1/1000),
         shifted_scaled_psd = scaled_psd + 195, 
         log_shifted_scaled_psd = log(scaled_psd + 195)
         )


# Plotting Transformed Data -----------------------------------------------

aggregate_data |> 
  ggplot(aes(distance, scaled_psd)) +
  geom_point(alpha = 0.05) +
  labs(title = "Scaled Recieved PSD Over Distance (All Input PSD)") +
  xlab("Distance") +
  ylab("Recieved PSD")

aggregate_data |> 
  ggplot(aes(transform1, shifted_scaled_psd)) +
  geom_point(alpha = 0.01) +
  labs(title = "Scaled Recieved PSD Over Distance^(-1) (All Input PSD)") +
  xlab("Distance") +
  ylab("Recieved PSD")

aggregate_data |> 
  ggplot(aes(transform2, shifted_scaled_psd)) +
  geom_point(alpha = 0.01) +
  labs(title = "Scaled Recieved PSD Over Distance^(-1/2) (All Input PSD)") +
  xlab("Distance") +
  ylab("Recieved PSD")

aggregate_data |> 
  ggplot(aes(transform3, shifted_scaled_psd)) +
  geom_point(alpha = 0.01) +
  labs(title = "Scaled Recieved PSD Over Distance^(-1/4) (All Input PSD)") +
  xlab("Distance") +
  ylab("Recieved PSD")

aggregate_data |> 
  ggplot(aes(transform4, shifted_scaled_psd)) +
  geom_point(alpha = 0.01) +
  labs(title = "Scaled Recieved PSD Over Distance^(-1/8) (All Input PSD)") +
  xlab("Distance") +
  ylab("Recieved PSD")

aggregate_data |> 
  ggplot(aes(transform5, scaled_psd)) +
  geom_point(alpha = 0.05) +
  labs(title = "Scaled Recieved PSD Over Distance^(-0.001) (All Input PSD)") +
  xlab("Distance") +
  ylab("Recieved PSD") # make viridis

# Seems like the ideal linear transformation is distance^(-1/16).

# Modeling ----------------------------------------------------------------

# Performing Cross Validation

set.seed(613)
mod_split <- aggregate_data |> 
  initial_split(
    prop = 0.6
  )

mod_test <- testing(mod_split)
mod_train <- training(mod_split)

# `tidymodels` Procedure for Fitting Linear Model.
my_recipe <- recipe(scaled_psd ~ input_psd + transform5, 
                    data = aggregate_data)

baked_recipe <- bake(prep(my_recipe), new_data = NULL)

lm_wkflow <- workflow() |> 
  add_model(lm_mod) |> 
  add_recipe(my_recipe)

# Specifying Model Metrics
my_metrics <- metric_set(mape, rsq, rmse)

# Fit the model (not with CV)
lm_fit <- fit(lm_wkflow, 
              data = mod_train)

lm_pred_test <- augment(lm_fit, 
                        new_data = mod_test)

my_metrics(lm_pred_test, 
           truth = scaled_psd, 
           estimate = .pred)

# THE MODEL ---------------------------------------------------------------
lm_fit |> 
  tidy()

m1 <- lm(scaled_psd ~ input_psd + transform5, 
   data = aggregate_data)
summary(m1)
# -------------------------------------------------------------------------

# Creating CV Folds to Confirm Model Metrics
lm_folds <- vfold_cv(mod_train, v = 10, repeats = 5)

# Fitting the Model (with CV)

lm_folds_fitted <- lm_wkflow |>
  fit_resamples(resamples = lm_folds, 
                metrics = my_metrics)

collect_metrics(lm_folds_fitted, summarize = F) |> 
  filter(.metric == "mape") |> 
  arrange(desc(.estimate)) |>
  kable()

collect_metrics(lm_folds_fitted, summarize = T) |>
  select(.metric, mean, n, std_err) ##|>
  # kable(,col.names = c("Metric", "Mean", "Number of Trials", "Standard Error"))

# Benchmark ---------------------------------------------------------------
columns <- c("Expression",
             "Min. Runtime",
             "Med. Runtime",
             "Itr/Sec",
             "Memory Allocated")

bnch <- bench::mark(tidymodels = lm_wkflow |> 
                      fit_resamples(resamples = lm_folds, metrics = my_metrics),
                    check = FALSE)
bnch |> 
  select(expression, min, 
         median, `itr/sec`, 
         mem_alloc) |> 
  knitr::kable(digits = 3, 
               col.names = columns) |> 
  kableExtra::kable_styling(font_size = 10,
                            latex_options = "HOLD_position") 



# -------------------------------------------------------------------------
# Trying Αnalysis with Noise ----------------------------------------------
# -------------------------------------------------------------------------

# Data Transformation -----------------------------------------------------

# So to find the variance, we know the noise density(variance) at room temp is
# -174 dBm/Hz, and so we can convert to dBW/200MHz by using the following:
# -174 + 10*log10(200e6) - 30 = -12.86172. Since this is negative we take the
# square root of the absolute value and then we have the standard deviation for
# the distribution of noise for the outcome of interest.

set.seed(613)

noise <- rnorm(71070, 0, 10.99953)

aggregate_data$scaled_psd <- aggregate_data$scaled_psd + noise


aggregate_data |> filter(input_psd == -55) |> 
  ggplot(aes(distance, scaled_psd)) +
  geom_point(alpha = 0.05) +
  labs(title = "Recieved PSD Over Distance (Transmitted PSD = -55)") +
  xlab("Distance") +
  ylab("Recieved PSD")

aggregate_data |> filter(input_psd == -55) |>
  ggplot(aes(transform5, scaled_psd)) +
  geom_point(alpha = 0.05) +
  labs(title = "Noisy Recieved PSD Over Distance^(-0.001) (Transmitted PSD = -55)") +
  xlab("Transformed Distance") +
  ylab("Recieved PSD")

# Modeling ----------------------------------------------------------------

# Performing Model Training

set.seed(613)
mod_split <- aggregate_data |> 
  initial_split(
    prop = 0.8
  )

mod_test <- testing(mod_split)
mod_train <- training(mod_split)

# `tidymodels` Procedure for Fitting Linear Model.
my_recipe <- recipe(scaled_psd ~ input_psd + transform5, 
                    data = aggregate_data)

baked_recipe <- bake(prep(my_recipe), new_data = NULL)

lm_wkflow <- workflow() |> 
  add_model(lm_mod) |> 
  add_recipe(my_recipe)

# Specifying Model Metrics
my_metrics <- metric_set(mape, rsq, rmse)

# Fit the model (not with v-fold CV)
lm_fit <- fit(lm_wkflow, 
              data = mod_train)

lm_pred_test <- augment(lm_fit, 
                        new_data = mod_test)

my_metrics(lm_pred_test, 
           truth = scaled_psd, 
           estimate = .pred)

# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------



# THE MODEL ---------------------------------------------------------------
lm_fit |> 
  tidy()

m1 <- lm(scaled_psd ~ input_psd + transform5, 
         data = mod_train)
summary(m1)
# -------------------------------------------------------------------------

set.seed(613)
# Creating CV Folds to Confirm Model Metrics
lm_folds <- vfold_cv(mod_train, v = 10, repeats = 5)

# Fitting the Model (with CV)

lm_folds_fitted <- lm_wkflow |>
  fit_resamples(resamples = lm_folds, 
                metrics = my_metrics)

collect_metrics(lm_folds_fitted, summarize = F) |> 
  filter(.metric == "mape") |> 
  arrange(desc(.estimate))

collect_metrics(lm_folds_fitted, summarize = T) |>
  select(.metric, mean, n, std_err) ##|>
# kable(,col.names = c("Metric", "Mean", "Number of Trials", "Standard Error"))

# Benchmark ---------------------------------------------------------------
columns <- c("Expression",
             "Min. Runtime",
             "Med. Runtime",
             "Itr/Sec",
             "Memory Allocated")

bnch <- bench::mark(tidymodels = lm_wkflow |> 
                      fit_resamples(resamples = lm_folds, metrics = my_metrics),
                    check = FALSE)
bnch |> 
  select(expression, min, 
         median, `itr/sec`, 
         mem_alloc) |> 
  knitr::kable(digits = 3, 
               col.names = columns) |> 
  kableExtra::kable_styling(font_size = 10,
                            latex_options = "HOLD_position") 

# Plotting Model Fit ------------------------------------------------------
library("patchwork")

top_plot <- lm_pred_test |> 
  filter(input_psd == -55) |> 
  ggplot(aes(transform5, scaled_psd)) +
    geom_point(alpha = 0.05, size = 3) +
    geom_line(aes(transform5, .pred), color = "red") +
    labs(title = "Regression Model Fit (Transmitted PSD = -55)") +
    xlab("Transformed Distance") +
    ylab("Recieved PSD")

top_plot

# bottom_plot <- lm_pred_test |> 
#   filter(input_psd == -55) |> 
#   ggplot(aes(transform5, scaled_psd)) +
#   geom_point(alpha = 0.05, size = 3) +
#   geom_line(aes(transform5, .pred), color = "red") +
#   labs(title = "Regression Model Fit") +
#   xlab("Transformed Distance") +
#   ylab("Scaled PSD")


# -------------------------------------------------------------------------
# Calculating Metrics for Multiple Regression -----------------------------
# -------------------------------------------------------------------------

# Performing Model Training

set.seed(613)
mod_split <- aggregate_data |>
  initial_split(
    prop = 0.8
  )

mod_test <- testing(mod_split)
mod_train <- training(mod_split)

my_recipe2 <- recipe(scaled_psd ~ input_psd + distance,
                     data = aggregate_data)

baked_recipe2 <- bake(prep(my_recipe2), new_data = NULL)

lm_wkflow2 <- workflow() |>
  add_model(lm_mod) |>
  add_recipe(my_recipe2)

# Specifying Model Metrics
my_metrics <- metric_set(mape, rsq, rmse)

# Fit the model (not with v-fold CV)
lm_fit2 <- fit(lm_wkflow2,
              data = mod_train)

lm_pred_test2 <- augment(lm_fit2,
                        new_data = mod_test)

my_metrics(lm_pred_test2,
           truth = scaled_psd,
           estimate = .pred)

# THE MODEL ---------------------------------------------------------------
# lm_fit |> 
#   tidy()


# Plotly ------------------------------------------------------------------

pred_plotly <- augment(lm_fit, new_data = aggregate_data)
inputpsd_55 <- aggregate_data |> filter(input_psd == -55)

# Raw (Noisy) Data PLot

# switch first arg of plot_ly() to be inputpsd55 to see a plane
fig1 <- plot_ly(aggregate_data, x = ~input_psd, y = ~distance, z = ~scaled_psd,
               marker = list(color = ~scaled_psd, colorscale = 'Viridis', showscale = TRUE))
fig1 <- fig1 |> add_markers(opacity = 0.1)
fig1 <- fig1 |> layout(scene = list(xaxis = list(title = 'Transmitted PSD'),
                                   yaxis = list(title = 'Transformed Distance'),
                                   zaxis = list(title = 'Recieved PSD')),
                      annotations = list(
                        x = 1.13,
                        y = 1.05,
                        text = 'Recieved PSD',
                        xref = 'paper',
                        yref = 'paper',
                        showarrow = FALSE
                      ))
fig1

pred_inputpsd55 <- pred_plotly |> filter(input_psd == -55)

# Model Plot
fig2 <- plot_ly(pred_plotly, x = ~.pred, y = ~transform5, z = ~scaled_psd,
                marker = list(color = ~.pred, colorscale = 'Viridis', showscale = TRUE))
fig2 <- fig2 |> add_markers(opacity = 0.1)
fig2 <- fig2 |> layout(scene = list(xaxis = list(title = 'Transmitted PSD'),
                                   yaxis = list(title = 'Transformed Distance'),
                                   zaxis = list(title = 'Predicted Recieved PSD')),
                      annotations = list(
                        x = 1.13,
                        y = 1.05,
                        text = 'Recieved PSD',
                        xref = 'paper',
                        yref = 'paper',
                        showarrow = FALSE
                      ))
fig2

input_sim <- seq(from = -70, to = -41, length.out = 3)
input_dis <- (seq(from = 50, to = 1600, length.out = 3))^(-1/1000)

my_pred <- -8883 + 0.995*input_sim + 8824*input_dis

sim_data <- cbind(input_sim, input_dis, my_pred) |> as.data.frame()

fig3 <- plot_ly(sim_data, x = ~input_sim, y = ~input_dis, z = ~my_pred,
                marker = list(color = ~scaled_psd, colorscale = 'Viridis', showscale = TRUE))
fig3 <- fig1 |> add_markers(opacity = 0.1)
fig3 <- fig1 |> layout(scene = list(xaxis = list(title = 'Transmitted PSD'),
                                    yaxis = list(title = 'Transformed Distance'),
                                    zaxis = list(title = 'Recieved PSD')),
                       annotations = list(
                         x = 1.13,
                         y = 1.05,
                         text = 'Recieved PSD',
                         xref = 'paper',
                         yref = 'paper',
                         showarrow = FALSE
                       ))

plot_ly(sim_data, x = ~input_sim, y = ~input_dis, z = ~my_pred) |> add_surface()


# Using reshape2 ----------------------------------------------------------

plot_matrix <- t(reshape2::acast(y, input_dis~input_sim, value.var="my_pred"))
plot_matrix

plot_ly(
  x = ~as.numeric(colnames(plot_matrix)), 
  y = ~as.numeric(rownames(plot_matrix)), 
  z = ~plot_matrix
) |> 
  add_surface() |> 
  layout(
    title = "",
    scene = list(
      xaxis = list(type = "log", title = "Total observations"),
      yaxis = list(type = "log", title = "Firm size"),
      zaxis = list(title = "Median"),
      camera = list(eye = list(x = 1.95, y = -1.25, z = 1.25))
    ))



pred_plotly |> filter(input_psd == -55) |>
  ggplot(aes(transform5, scaled_psd)) +
  geom_point(alpha = 0.1) +
  geom_line(aes(transform5, .pred), alpha = 1, fill = "red") +
  labs(title = "Noisy Recieved PSD Over Distance^(-0.001) (Transmitted PSD = -55)") +
  xlab("Transformed Distance") +
  ylab("Recieved PSD")

y <- matrix(0, ncol = 1500, nrow = 1500)

for (i in seq_along(1:1500)) {
  for (j in seq_along(1:1500)) {
    print(i)
    print(j)
    (y[i, j] <- -8883.4147788 + 0.9951444*input_sim[j] + 8823.8172151*input_dis[i])
    print(y[i,j])
  }
}

 pred_plotly |> 
  select(scaled_psd, input_psd, transform5, .pred) |> 
  write.csv("/Users/carson/Documents/Baylor_Statistics_HW/Spring_2023/Microwave_Project/model_data.csv")
