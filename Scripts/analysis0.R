# Carson Slater -----------------------------------------------------------
# FIS Training Data -------------------------------------------------------

library("tidymodels"); theme_set(theme_bw() + 
                                   theme(panel.border = element_blank(), 
                                   panel.grid.major = element_blank(),
                                   panel.grid.minor = element_blank(), 
                                   axis.line = element_line(colour = "black")))
library('workflowsets')
library("parsnip")
library("recipes")
library("knitr")

# -------------------------------------------------------------------------
# Working with Data Set 1 -------------------------------------------------
# -------------------------------------------------------------------------

# EDA ---------------------------------------------------------------------

tuning_data1 <- read.csv(here::here("Data", "MamdaniTuningData_1-5.csv"),
                         header = TRUE)

tuning_data2 <- read.csv(here::here("Data", "MamdaniTuningData_1-6.csv"),
                         header = TRUE)

distances <- as.vector(as.matrix(tuning_data1[ ,c("tx1_distance",
                                                  "tx2_distance",
                                                  "tx3_distance")]))

input_psd <- as.vector(as.matrix(tuning_data1[ ,c("tx1_psd",
                                                  "tx2_psd",
                                                  "tx3_psd")]))

scaled_psd <- as.vector(as.matrix(tuning_data1[ ,c("tx1_scaled_psd",
                                                   "tx2_scaled_psd",
                                                   "tx3_scaled_psd")]))
aggregate_data1 <- cbind.data.frame(rep(tuning_data1$rad_loc_idx, 3),
                         distances,
                         input_psd,
                         scaled_psd)

# Plots -------------------------------------------------------------------

pdf(here::here("nojittertotal1.pdf"), width = 7, height = 5)
aggregate_data1 |> 
  ggplot(aes(distances, scaled_psd)) +
  geom_point(alpha = 0.33) +
  labs(title = "Scaled PSD Over Distance (All Input PSD)") +
  xlab("Distance") +
  ylab("Scaled PSD")
dev.off()

# jittered
pdf(here::here("jittertotal1.pdf"), width = 7, height = 5)
aggregate_data1 |> 
  ggplot(aes(distances, scaled_psd)) +
    geom_jitter(alpha = 0.33, width = 0.5, height = 0.5) +
    labs(title = "Scaled PSD Over Distance (All Input PSD)") +
    xlab("Distance") +
    ylab("Scaled PSD")
dev.off()

pdf(here::here("inputpsd1.pdf"), width = 7, height = 5)
aggregate_data1 |> 
  ggplot(aes(input_psd, scaled_psd)) +
  geom_point(alpha = 0.1) +
  labs(title = "Scaled PSD Over Input PSD (All Input PSD)") +
  xlab("Input PSD") +
  ylab("Scaled PSD")
dev.off()

pdf(here::here("psd60constant1.pdf"), width = 7, height = 5)
aggregate_data1 |> filter(input_psd == -60) |> 
  ggplot(aes(distances, scaled_psd)) +
  geom_point(alpha = 0.33) +
  geom_smooth() + 
  labs(title = "Scaled PSD Over Distance (Input PSD = -60)") +
  xlab("Distance") +
  ylab("Scaled PSD")
dev.off()

aggregate_data1 |> 
  ggplot(aes(scaled_psd)) +
  geom_histogram() + 
  facet_wrap(. ~ input_psd) +
  labs(title = "Distribution of Scaled PSD By Input PSD") +
  xlab("Count") +
  ylab("Scaled PSD")


aggregate_data1 |> 
  ggplot(aes(input_psd)) +
  geom_histogram() 


# Preliminary Modeling ----------------------------------------------------

# appears to be quadratic relationship between the data
aggregate_data1 <- aggregate_data1 |> mutate(distances_sq = distances**2)

mod1 <- lm(data = aggregate_data1,
           scaled_psd ~ input_psd + distances + distances_sq)


# Models for Each Transmitter ---------------------------------------------

tuning_data1 <- tuning_data1 |> mutate(tx1_distance_sq = tx1_distance^2,
                                       tx2_distance_sq = tx2_distance^2,
                                       tx3_distance_sq = tx3_distance^2,)

folds <- bootstraps(tuning_data1, times = 10)

tx1_rec <- recipe(tx1_scaled_psd ~ tx1_psd + tx1_distance + tx1_distance_sq,
                  data = tuning_data1)

tx2_rec <- recipe(tx2_scaled_psd ~ tx2_psd + tx2_distance + tx2_distance_sq,
                  data = tuning_data1)

tx3_rec <- recipe(tx3_scaled_psd ~ tx3_psd + tx3_distance + tx3_distance_sq,
                  data = tuning_data1)

parsnip_mod <-
  parsnip::linear_reg() |>  
  parsnip::set_engine("lm") |>  
  parsnip::set_mode("regression")

tx_models <-
  workflowsets::workflow_set(
    preproc = list(tx1 = tx1_rec, tx2 = tx2_rec, tx3 = tx3_rec),
    models = list(lm = parsnip_mod)
  ) |> workflow_map("fit_resamples", resamples = folds)

collect_metrics(tx_models)


# Models for Each Transmitter ---------------------------------------------

tx1_data <- tuning_data1 |> select(tx1_psd, tx1_distance, tx1_distance_sq)
tx2_data <- tuning_data1 |> select(tx2_psd, tx2_distance, tx2_distance_sq)
tx3_data <- tuning_data1 |> select(tx3_psd, tx3_distance, tx3_distance_sq)


(tx1_mod <- fit(parsnip_mod, 
               tx1_scaled_psd ~ tx1_psd + tx1_distance + tx1_distance_sq,
               data = tuning_data1))

(tx2_mod <- fit(parsnip_mod, 
                tx2_scaled_psd ~ tx2_psd + tx2_distance + tx2_distance_sq,
                data = tuning_data1))

(tx3_mod <- fit(parsnip_mod, 
                tx3_scaled_psd ~ tx3_psd + tx3_distance + tx3_distance_sq,
                data = tuning_data1))

pred1 <- predict(tx1_mod, new_data = tx1_data)
pred2 <- predict(tx2_mod, new_data = tx2_data)
pred3 <- predict(tx3_mod, new_data = tx3_data)

pred1 <- bind_cols(pred1, tuning_data1 |> 
                     select(tx1_psd, 
                            tx1_distance, 
                            tx1_distance_sq,
                            tx1_scaled_psd))

pred2 <- bind_cols(pred2, tuning_data1 |> 
                     select(tx2_psd, 
                            tx2_distance, 
                            tx2_distance_sq,
                            tx2_scaled_psd))

pred3 <- bind_cols(pred3, tuning_data1 |> 
                     select(tx3_psd, 
                            tx3_distance, 
                            tx3_distance_sq,
                            tx3_scaled_psd))

tx1_mod |> tidy(); tx2_mod |> tidy(); tx3_mod |> tidy()

rmse(pred1, truth = tx1_scaled_psd, estimate = .pred)
rmse(pred2, truth = tx2_scaled_psd, estimate = .pred)
rmse(pred3, truth = tx3_scaled_psd, estimate = .pred)

rsq(pred1, truth = tx1_scaled_psd, estimate = .pred)
rsq(pred2, truth = tx2_scaled_psd, estimate = .pred)
rsq(pred3, truth = tx3_scaled_psd, estimate = .pred)

coefs <- rbind(tx1_mod$fit$coefficients |> unname(),
               tx2_mod$fit$coefficients |> unname(),
               tx3_mod$fit$coefficients |> unname())

pdf(here::here("mod1.pdf"), width = 7, height = 5)
pred1 |> ggplot(aes(tx1_distance, tx1_scaled_psd)) +
  geom_point(alpha = 0.33) +
  geom_smooth() +
  labs(title = "Regression Scaled PSD Over Distance (Holding Input PSD Fixed)") +
  xlab("Distance") +
  ylab("Scaled PSD")
dev.off()

# -------------------------------------------------------------------------
# Working with Data Set 2 -------------------------------------------------
# -------------------------------------------------------------------------

# EDA ---------------------------------------------------------------------

# making the data frame for the regression
distances2 <- as.vector(as.matrix(tuning_data2[ ,c("tx1_distance",
                                                   "tx2_distance",
                                                   "tx3_distance")]))

input_psd2 <- as.vector(as.matrix(tuning_data2[ ,c("tx1_psd",
                                                   "tx2_psd",
                                                   "tx3_psd")]))

scaled_psd2 <- as.vector(as.matrix(tuning_data2[ ,c("tx1_scaled_psd",
                                                    "tx2_scaled_psd",
                                                    "tx3_scaled_psd")]))

aggregate_data2 <- cbind.data.frame(rep(tuning_data2$rad_loc_idx, 3),
                                    distances2,
                                    input_psd2,
                                    scaled_psd2)

aggregate_data2 <- aggregate_data2 |> mutate(distances_sq2 = distances2**2)

pdf(here::here("nojittertotal2.pdf"), width = 7, height = 5)
aggregate_data2 |> 
  ggplot(aes(distances2, scaled_psd2)) +
  geom_point(alpha = 0.05) +
  labs(title = "Scaled PSD Over Distance (All Input PSD)") +
  xlab("Distance") +
  ylab("Scaled PSD")
dev.off()

pdf(here::here("jittertotal2.pdf"), width = 7, height = 5)
aggregate_data2 |> 
  ggplot(aes(distances2, scaled_psd2)) +
  geom_jitter(alpha = 0.05, width = 0.25, height = 0.25) +
  labs(title = "Scaled PSD Over Distance (All Input PSD)") +
  xlab("Distance") +
  ylab("Scaled PSD")
dev.off()


# Modeling ----------------------------------------------------------------

mod2 <- lm(data = aggregate_data2,
           scaled_psd2 ~ input_psd2 + distances2 + distances_sq2)

big_model_pred <- predict(mod2, aggregate_data2 |> select(input_psd2, distances2, distances_sq2))

big_model_pred <- bind_cols(.pred = big_model_pred, aggregate_data2 |> 
                     select(scaled_psd2))


# Model Metrics -----------------------------------------------------------

rmse(big_model_pred, truth = scaled_psd2, .pred)
rsq(big_model_pred, truth = scaled_psd2, .pred, adjusted = TRUE)
rsq_trad(big_model_pred, truth = scaled_psd2, .pred, adjusted = TRUE)
mape(big_model_pred, truth = scaled_psd2, .pred)
yardstick::mae(big_model_pred, truth = scaled_psd2, .pred)
yardstick::smape(big_model_pred, truth = scaled_psd2, .pred)
yardstick::mpe(big_model_pred, truth = scaled_psd2, .pred)

# Plotting Final Model ----------------------------------------------------

final_plot_df <- bind_cols(big_model_pred, 
                           distances2 = distances2, 
                           input_psd2 = input_psd2)

pdf(here::here("regression_facet_nojitter.pdf"), width = 10, height = 14)
final_plot_df |>
  ggplot(aes(distances2, scaled_psd2)) +
    geom_point(alpha = 0.01) +
    geom_line(aes(distances2, .pred), color = "orange") +
    facet_wrap(. ~ input_psd2, nrow = 6) +
    scale_color_manual(values = "FF6600") +
    labs(title = "Regression Model Fit (Faceted by All Input PSD)") +
    xlab("Distance") +
    ylab("Scaled PSD")
dev.off()

# EDA plot for paper
pdf(here::here("psd60constant1.pdf"), width = 7, height = 5)
aggregate_data2 |> filter(input_psd2 == -60) |> 
  ggplot(aes(distances2, scaled_psd2)) +
  geom_jitter(alpha = 0.05, width = 0.5, height = 0.5) +
  labs(title = "Scaled PSD Over Distance (Input PSD = -60)") +
  xlab("Distance") +
  ylab("Scaled PSD")
dev.off()


# -------------------------------------------------------------------------
# Testing New Models ------------------------------------------------------
# -------------------------------------------------------------------------

aggregate_data2 <- aggregate_data2 |> mutate(distances_cb2 = distances2**3,
                                             input_psd_sq2 = input_psd2**2)
                                             
lm_mod <-
  parsnip::linear_reg() |>
  parsnip::set_engine("lm") |>
  parsnip::set_mode("regression")

mod1_data <- aggregate_data2 |> select(input_psd2, distances2, distances_sq2)
mod2_data <- aggregate_data2 |> select(input_psd2, distances2, distances_sq2, distances_cb2)
mod3_data <- aggregate_data2 |> select(input_psd2, input_psd_sq2, distances2, distances_sq2)

(lm1 <- fit(lm_mod, 
            scaled_psd2 ~ input_psd2 + distances2 + distances_sq2,
            data = aggregate_data2))

(lm2 <- fit(lm_mod, 
            scaled_psd2 ~ input_psd2 + distances2 + distances_sq2 + distances_cb2,
            data = aggregate_data2))

(lm3 <- fit(lm_mod, 
            scaled_psd2 ~ input_psd2 + input_psd_sq2 + distances2 + distances_sq2,
            data = aggregate_data2))

pred1 <- predict(lm1, new_data = mod1_data)
pred2 <- predict(lm2, new_data = mod2_data)
pred3 <- predict(lm3, new_data = mod3_data)

pred1 <- bind_cols(pred1, scaled_psd2 = aggregate_data2 |> 
                     select(scaled_psd2))

pred2 <- bind_cols(pred2, scaled_psd2 = aggregate_data2 |> 
                     select(scaled_psd2))

pred3 <- bind_cols(pred3, scaled_psd2 = aggregate_data2 |> 
                     select(scaled_psd2))

lm1 |> tidy(); lm2 |> tidy(); lm3 |> tidy()

mape(pred1, truth = scaled_psd2, estimate = .pred)
mape(pred2, truth = scaled_psd2, estimate = .pred)
mape(pred3, truth = scaled_psd2, estimate = .pred)



# -------------------------------------------------------------------------
# Bootstrap and Cross Validation ------------------------------------------
# -------------------------------------------------------------------------

aggregate_data2 <- aggregate_data2 |> mutate(distances_cb2 = distances2**3,
                                             input_psd_sq2 = input_psd2**2)

lm_mod <-
  parsnip::linear_reg() |>
  parsnip::set_engine("lm") |>
  parsnip::set_mode("regression")

# Model 1
lm_rec1 <- recipe(scaled_psd2 ~ input_psd2 + distances2 + distances_sq2,
                  data = aggregate_data2)
# Model 2
lm_rec2 <- recipe(scaled_psd2 ~ input_psd2 + distances2 + distances_sq2 + distances_cb2,
                  data = aggregate_data2)
# Model 3
lm_rec3 <- recipe(scaled_psd2 ~ input_psd2 + input_psd_sq2 + distances2 + distances_sq2,
                  data = aggregate_data2)

# Bootstrap ---------------------------------------------------------------
set.seed(613)

boot_folds5 <- bootstraps(aggregate_data2, times = 5)
boot_folds10 <- bootstraps(aggregate_data2, times = 10)
boot_folds20 <- bootstraps(aggregate_data2, times = 20)

bootstrap_fits5 <-
  workflowsets::workflow_set(
    preproc = list(mod1 = lm_rec1, mod2 = lm_rec2, mod3 = lm_rec3),
    models = list(lm = lm_mod)
  ) |> workflow_map("fit_resamples", resamples = boot_folds5)

bootstrap_fits10 <-
  workflowsets::workflow_set(
    preproc = list(mod1 = lm_rec1, mod2 = lm_rec2, mod3 = lm_rec3),
    models = list(lm = parsnip_mod)
  ) |> workflow_map("fit_resamples", resamples = boot_folds10)

bootstrap_fits20 <-
  workflowsets::workflow_set(
    preproc = list(mod1 = lm_rec1, mod2 = lm_rec2, mod3 = lm_rec3),
    models = list(lm = parsnip_mod)
  ) |> workflow_map("fit_resamples", resamples = boot_folds20)

collect_metrics(bootstrap_fits5)
collect_metrics(bootstrap_fits10)
collect_metrics(bootstrap_fits20)

baked_lm_rec1 <- bake(prep(lm_rec1), new_data = aggregate_data2)

bootstrap_fits5 |> predict(baked_lm_rec1)

# V-Fold Cross Validation -------------------------------------------------

cv_folds5 <- vfold_cv(aggregate_data2, v = 5, repeats = 1, strata = input_psd2)
cv_folds10 <- vfold_cv(aggregate_data2, v = 10, repeats = 5, strata = input_psd2)
cv_folds20 <- vfold_cv(aggregate_data2, v = 20, repeats = 5, strata = input_psd2)

cv_fits5 <-
  workflowsets::workflow_set(
    preproc = list(mod1 = lm_rec1, mod2 = lm_rec2, mod3 = lm_rec3),
    models = list(lm = lm_mod)
  ) |> 
  workflow_map("fit_resamples", resamples = cv_folds5, verbose = TRUE)

cv_fits10 <-
  workflowsets::workflow_set(
    preproc = list(mod1 = lm_rec1, mod2 = lm_rec2, mod3 = lm_rec3),
    models = list(lm = lm_mod)
  ) |> workflow_map("fit_resamples", resamples = cv_folds10)

cv_fits20 <-
  workflowsets::workflow_set(
    preproc = list(mod1 = lm_rec1, mod2 = lm_rec2, mod3 = lm_rec3),
    models = list(lm = lm_mod)
  ) |> workflow_map("fit_resamples", resamples = cv_folds20)