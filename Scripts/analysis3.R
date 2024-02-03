
# Carson Slater - Analysis 3 ----------------------------------------------
source(here::here("analysis1.R"))

# Notes -------------------------------------------------------------------


# Its good to lay out figures and tables make writing (make an outline)
# fill in the gaps

# Someone needs to be able to read the paper, understand, and go do it themselves
# with some familiarity

# Draw out design decisions


# Tables ------------------------------------------------------------------
# Update Table 1 with Adjusted Units and coefficients for other models
# Remove Table 2, and just cite that we used v-fold CV
# Table 3: Add metric for Adjusted Units/Linear Case Model Specs on Rows,
# metrics on columms
# (Modify) or Take out Table 4, if modify add additional Models


# Plots -------------------------------------------------------------------

# plots 2 a and b Plosih axes and labels and Change color to viridis
# explain very detailed what is happening in plots 2a and b

# plots 3 will be system level diagram 

# surface plots will be Jonathan

# Model Fit Plot
#     MAKE AXES CONSISTENT
#     ADD CI FOR MODEL FIT
#     MAKE COLORS MATCH



# -------------------------------------------------------------------------
# IGNORE ------------------------------------------------------------------
# -------------------------------------------------------------------------


# Unlogging the units -----------------------------------------------------
# W/200MHz

# aggregate_data <- aggregate_data |> 
#   mutate(transform5 = distance^(-1/1000),
#          lin_input_psd = 10^(input_psd/10),
#          lin_scaled_psd = 10^(scaled_psd/10))
# 
# aggregate_data <- aggregate_data |> 
#   mutate(transformed_scaled_psd = lin_scaled_psd,
#          transform6 = distance^(1/1000))
# # EDA ---------------------------------------------------------------------
# 
# aggregate_data |> 
#   ggplot(aes(distance, lin_scaled_psd)) +
#   geom_point(alpha = 0.05) +
#   labs(title = "Scaled Recieved PSD Over Distance^(-0.001) (All Input PSD)") +
#   xlab("Distance") +
#   ylab("Recieved PSD")
# 
# aggregate_data |> 
#   ggplot(aes(transform6, lin_scaled_psd)) +
#   geom_point(alpha = 0.05) +
#   labs(title = "Scaled Recieved PSD Over Distance (All Input PSD)") +
#   xlab("Distance") +
#   ylab("Recieved PSD")
# 
# aggregate_data |> 
#   ggplot(aes(transform5, transformed_scaled_psd)) +
#   geom_point(alpha = 0.05) +
#   labs(title = "Scaled Recieved PSD Over Input PSD") +
#   xlab("Distance") +
#   ylab("Recieved PSD")



