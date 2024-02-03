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
# EDA ---------------------------------------------------------------------
# -------------------------------------------------------------------------

# Data Prep ---------------------------------------------------------------


tuning_data <- read.csv(here::here("MamdaniTuningData_1-6.csv"),
                         header = TRUE)

distance <- as.vector(as.matrix(tuning_data[ ,c("tx1_distance",
                                                  "tx2_distance",
                                                  "tx3_distance")]))

input_psd <- as.vector(as.matrix(tuning_data[ ,c("tx1_psd",
                                                  "tx2_psd",
                                                  "tx3_psd")]))

scaled_psd <- as.vector(as.matrix(tuning_data[ ,c("tx1_scaled_psd",
                                                   "tx2_scaled_psd",
                                                   "tx3_scaled_psd")]))

aggregate_data <- cbind.data.frame(rep(tuning_data$rad_loc_idx, 3),
                                    distance,
                                    input_psd,
                                    scaled_psd)

# Plotting ----------------------------------------------------------------

aggregate_data |> 
  ggplot(aes(distance, scaled_psd)) +
  geom_point(alpha = 0.05) +
  labs(title = "Recieved PSD Over Distance (All Input PSD)") +
  xlab("Distance") +
  ylab("Recieved PSD")

aggregate_data |> 
  ggplot(aes(distance, scaled_psd)) +
  geom_jitter(alpha = 0.05, width = 0.5, height = 0.5) +
  labs(title = "Recieved PSD Over Distance (All Transmitted PSD)") +
  xlab("Distance") +
  ylab("Recieved PSD")

aggregate_data |> 
  ggplot(aes(input_psd, scaled_psd)) +
  geom_point(alpha = 0.05) +
  labs(title = "Recieved PSD Over Transmitted PSD (All Transmitted PSD)") +
  xlab("Transmitted PSD") +
  ylab("Recieved PSD")

# aggregate_data |> 
#   ggplot(aes(scaled_psd)) +
#   geom_histogram() + 
#   facet_wrap(. ~ input_psd) +
#   labs(title = "Distribution of Scaled PSD By Input PSD") +
#   xlab("Count") +
#   ylab("Scaled PSD")


# -------------------------------------------------------------------------
# Modeling ----------------------------------------------------------------
# -------------------------------------------------------------------------


aggregate_data <- aggregate_data |> mutate(distance_sq = distance**2,
                                           distance_cb = distance**3,
                                           input_psd_sq = input_psd**2)

lm_mod <-
  parsnip::linear_reg() |>
  parsnip::set_engine("lm") |>
  parsnip::set_mode("regression")

mod1_data <- aggregate_data |> select(input_psd, distance, distance_sq)
mod2_data <- aggregate_data |> select(input_psd, distance, distance_sq, distance_cb)
mod3_data <- aggregate_data |> select(input_psd, input_psd_sq, distance, distance_sq)

(lm1 <- fit(lm_mod, 
            scaled_psd ~ input_psd + distance + distance_sq,
            data = aggregate_data))

(lm2 <- fit(lm_mod, 
            scaled_psd ~ input_psd + distance + distance_sq + distance_cb,
            data = aggregate_data))

(lm3 <- fit(lm_mod, 
            scaled_psd ~ input_psd + input_psd_sq + distance + distance_sq,
            data = aggregate_data))

pred1 <- predict(lm1, new_data = mod1_data)
pred2 <- predict(lm2, new_data = mod2_data)
pred3 <- predict(lm3, new_data = mod3_data)

pred1 <- bind_cols(pred1, scaled_psd = aggregate_data |> 
                     select(scaled_psd))

pred2 <- bind_cols(pred2, scaled_psd = aggregate_data |> 
                     select(scaled_psd))

pred3 <- bind_cols(pred3, scaled_psd = aggregate_data |> 
                     select(scaled_psd))

lm1 |> tidy(); lm2 |> tidy(); lm3 |> tidy()

mape(pred1, truth = scaled_psd, estimate = .pred)
mape(pred2, truth = scaled_psd, estimate = .pred)
mape(pred3, truth = scaled_psd, estimate = .pred)

# Model 2 had a MAPE of 14.2%, which beat out models 1 and 3

rsq(pred2, truth = scaled_psd, estimate = .pred)
rmse(pred2, truth = scaled_psd, estimate = .pred)


# Model Plotting ----------------------------------------------------------

plot_df <- bind_cols(.pred = pred2, 
                     distance = aggregate_data |> 
                       select(distance),
                     input_psd = aggregate_data |> 
                       select(input_psd))

pdf(here::here("regression_facet_nojitter.pdf"), width = 10, height = 14)
plot_df |>
  ggplot(aes(distance, scaled_psd)) +
  geom_point(alpha = 0.01) +
  geom_line(aes(distance, .pred), color = "red") +
  facet_wrap(. ~ input_psd, nrow = 6) +
  labs(title = "Regression Model Fit (Faceted by All Input PSD)") +
  xlab("Distance") +
  ylab("Scaled PSD")
dev.off()

pdf(here::here("regression_facet_jitter.pdf"), width = 10, height = 14)
plot_df |>
  ggplot(aes(distance, scaled_psd)) +
  geom_jitter(alpha = 0.01, width = 2, height = 2) +
  geom_line(aes(distance, .pred), color = "red") +
  facet_wrap(. ~ input_psd, nrow = 6) +
  labs(title = "Regression Model Fit (Faceted by All Input PSD)") +
  xlab("Distance") +
  ylab("Scaled PSD")
dev.off()


# -------------------------------------------------------------------------
# Benchmarking ------------------------------------------------------------
# -------------------------------------------------------------------------

lm2 |> tidy()

# stargazer(lm1 ,lm2,
#           title = "Regression Table with Stargazer",
#           label="tab2",
#           table.placement = "H",



# Notes -------------------------------------------------------------------
# Plot Polishing (units and Labels)
# Comparing Different model specifications
# Justification based on overfitted and accuracy
# Dr. Marks cares about error and overfitting
# 80/20 Train-test split



# Mass Library (Box Cox Transformation - investigate)