i=0.0335
library(readxl)
library(dplyr)
library(ggplot2)

# HIGH POLLUTION
df<-read_xlsx("D:/downloads/qx_predictions_monotonic_GAM.xlsx") %>%
  filter(Year %in% seq(2023, 2033, 2)) %>%   # Keep odd years only
  arrange(Year, Age)

# Interest rate and discount factor
i <- 0.0335
v <- 1 / (1 + i)
df$qx_pred <- as.numeric(df$qx_pred)

# Calculate px = 1 - qx
df <- df %>%
  mutate(px = 1 - qx_pred)

# Function to compute A_x from any starting index within a year group
compute_Ax <- function(start_age_index, df_year, v) {
  n <- nrow(df_year)
  Ax <- 0
  tp_x <- 1
  for (t in 1:(n - start_age_index)) {
    q <- df_year$qx_pred[start_age_index + t]
    v_t <- v^t
    Ax <- Ax + v_t * tp_x * q
    tp_x <- tp_x * df_year$px[start_age_index + t - 1]
  }
  return(Ax)
}

# Compute Ax for all ages in all selected years
results <- df %>%
  group_by(Year) %>%
  arrange(Age) %>%
  group_modify(~ {
    year_df <- .x
    year_df$Ax <- sapply(1:nrow(year_df), function(i) compute_Ax(i, year_df, v))
    year_df
  })
hp_results <- results

# Plot: Net Single Premiums for all selected years
ggplot(results, aes(x = Age, y = Ax, color = as.factor(Year))) +
  geom_line(size = 1) +
  labs(
    title = "Net Single Premium (Aₓ) by Age(High pollution)",
    x = "Age",
    y = "Net Single Premium (Aₓ)",
    color = "Year"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 14)
  )

########################################################

# Prepare reserve data for all years and ages 20 to 100
multi_reserve_data <- results %>%
  filter(Year %in% seq(2023, 2033, 2), Age >= 20, Age <= 100) %>%
  mutate(t = Age - 20) %>%
  select(Year, t, Age, Reserve = Ax)

# Set reserve at t = 0 to 0 for each year (equivalence principle)
multi_reserve_data <- multi_reserve_data %>%
  group_by(Year) %>%
  mutate(Reserve = ifelse(t == 0, 0, Reserve)) %>%
  ungroup()

# Plot multiple reserve paths
ggplot(multi_reserve_data, aes(x = t, y = Reserve, color = as.factor(Year))) +
  geom_line(size = 1.2) +
  labs(
    title = "Reserve Over Time for a 20-Year-Old (High Pollution Scenario)",
    x = "Time since age 20 (t)",
    y = expression(paste("Reserve ", t*V, " = ", A[20 + t])),
    color = "Projection Year"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 14),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )



############################################################
# Reserve at age 17
# Extract reserves for age 17 to 100 in year 2023
reserve_plot_data <- results %>%
  filter(Year == 2023, Age >= 17, Age <= 100) %>%
  mutate(t = Age - 17) %>%
  select(t, Age, Reserve = Ax)

# Set reserve at t = 0 to zero (by equivalence principle)
reserve_plot_data$Reserve[reserve_plot_data$t == 0] <- 0

# Plot reserve over time t = 0 to 80
ggplot(reserve_plot_data, aes(x = t, y = Reserve)) +
  geom_line(color = "steelblue", size = 1.2) +
  geom_point(color = "orange", size = 2) +
  labs(
    title = "Reserve Over Time for a 17-Year-Old (t = 0 to 80)",
    x = "Time since age 17 (t)",
    y = expression(paste("Reserve ", t*V, " = ", A[17 + t])),
    caption = "Based on 2023 life table and equivalence principle"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 14)
  )

# Reserve at age 50
# Extract reserves for age 50 to 100 in year 2023
reserve_plot_data <- results %>%
  filter(Year == 2023, Age >= 50, Age <= 100) %>%
  mutate(t = Age - 50) %>%
  select(t, Age, Reserve = Ax)

# Set reserve at t = 0 to zero (by equivalence principle)
reserve_plot_data$Reserve[reserve_plot_data$t == 0] <- 0

# Plot reserve over time t = 0 to 50
ggplot(reserve_plot_data, aes(x = t, y = Reserve)) +
  geom_line(color = "steelblue", size = 1.2) +
  geom_point(color = "orange", size = 2) +
  labs(
    title = "Reserve Over Time for a 50-Year-Old (t = 0 to 50)",
    x = "Time since age 50 (t)",
    y = expression(paste("Reserve ", t*V, " = ", A[50 + t])),
    caption = "Based on 2023 life table and equivalence principle"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 14)
  )


# Reserve for age (84)
# Extract reserves for age 84 to 100 in year 2023
reserve_plot_data <- results %>%
  filter(Year == 2023, Age >= 84, Age <= 100) %>%
  mutate(t = Age - 84) %>%
  select(t, Age, Reserve = Ax)

# Set reserve at t = 0 to zero (by equivalence principle)
reserve_plot_data$Reserve[reserve_plot_data$t == 0] <- 0

# Plot reserve over time t = 0 to 20
ggplot(reserve_plot_data, aes(x = t, y = Reserve)) +
  geom_line(color = "steelblue", size = 1.2) +
  geom_point(color = "orange", size = 2) +
  labs(
    title = "Reserve Over Time for a 84-Year-Old (t = 0 to 20)",
    x = "Time since age 84 (t)",
    y = expression(paste("Reserve ", t*V, " = ", A[84 + t])),
    caption = "Based on 2023 life table and equivalence principle"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 14)
  )


##########################################################

# GREEN FUTURE
#------------------------------------------------------------------------------------------------
df2=read_xlsx("D:/green.xlsx")%>%
  filter(Year %in% seq(2023, 2033, 2)) %>%   # Keep odd years only
  arrange(Year, Age)
df2$qx_pred <- as.numeric(df2$qx_pred)
# Interest rate and discount factor
i <- 0.0335
v <- 1 / (1 + i)

# Calculate px = 1 - qx
df2 <- df2 %>%
  mutate(px = 1 - qx_pred)

# Function to compute A_x from any starting index within a year group
compute_Ax <- function(start_age_index, df_year, v) {
  n <- nrow(df_year)
  Ax <- 0
  tp_x <- 1
  for (t in 1:(n - start_age_index)) {
    q <- df_year$qx_pred[start_age_index + t]
    v_t <- v^t
    Ax <- Ax + v_t * tp_x * q
    tp_x <- tp_x * df_year$px[start_age_index + t - 1]
  }
  return(Ax)
}

# Compute Ax for all ages in all selected years
results <- df2 %>%
  group_by(Year) %>%
  arrange(Age) %>%
  group_modify(~ {
    year_df <- .x
    year_df$Ax <- sapply(1:nrow(year_df), function(i) compute_Ax(i, year_df, v))
    year_df
  })
gf_results <- results


# Plot: Net Single Premiums for all selected years
ggplot(results, aes(x = Age, y = Ax, color = as.factor(Year))) +
  geom_line(size = 1) +
  labs(
    title = "Net Single Premium (Aₓ) by Age(Green Future)",
    x = "Age",
    y = "Net Single Premium (Aₓ)",
    color = "Year"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 14)
  )

################################################################

# Reserve at age 17
# Extract reserves for age 17 to 100 in year 2023
reserve_plot_data2 <- results %>%
  filter(Year == 2023, Age >= 17, Age <= 100) %>%
  mutate(t = Age - 17) %>%
  select(t, Age, Reserve = Ax)

# Set reserve at t = 0 to zero (by equivalence principle)
reserve_plot_data$Reserve[reserve_plot_data$t == 0] <- 0

# Plot reserve over time t = 0 to 80
ggplot(reserve_plot_data, aes(x = t, y = Reserve)) +
  geom_line(color = "steelblue", size = 1.2) +
  geom_point(color = "orange", size = 2) +
  labs(
    title = "Reserve Over Time for a 17-Year-Old (t = 0 to 80)",
    x = "Time since age 17 (t)",
    y = expression(paste("Reserve ", t*V, " = ", A[17 + t])),
    caption = "Based on 2023 life table and equivalence principle"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 14)
  )

# Reserve at age 50
# Extract reserves for age 50 to 100 in year 2023
reserve_plot_data <- results %>%
  filter(Year == 2023, Age >= 50, Age <= 100) %>%
  mutate(t = Age - 50) %>%
  select(t, Age, Reserve = Ax)

# Set reserve at t = 0 to zero (by equivalence principle)
reserve_plot_data$Reserve[reserve_plot_data$t == 0] <- 0

# Plot reserve over time t = 0 to 50
ggplot(reserve_plot_data, aes(x = t, y = Reserve)) +
  geom_line(color = "steelblue", size = 1.2) +
  geom_point(color = "orange", size = 2) +
  labs(
    title = "Reserve Over Time for a 50-Year-Old (t = 0 to 50)",
    x = "Time since age 50 (t)",
    y = expression(paste("Reserve ", t*V, " = ", A[50 + t])),
    caption = "Based on 2023 life table and equivalence principle"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 14)
  )


# Reserve for age (84)
# Extract reserves for age 84 to 100 in year 2023
reserve_plot_data <- results %>%
  filter(Year == 2023, Age >= 84, Age <= 100) %>%
  mutate(t = Age - 84) %>%
  select(t, Age, Reserve = Ax)

# Set reserve at t = 0 to zero (by equivalence principle)
reserve_plot_data$Reserve[reserve_plot_data$t == 0] <- 0

# Plot reserve over time t = 0 to 20
ggplot(reserve_plot_data, aes(x = t, y = Reserve)) +
  geom_line(color = "steelblue", size = 1.2) +
  geom_point(color = "orange", size = 2) +
  labs(
    title = "Reserve Over Time for a 84-Year-Old (t = 0 to 20)",
    x = "Time since age 84 (t)",
    y = expression(paste("Reserve ", t*V, " = ", A[84 + t])),
    caption = "Based on 2023 life table and equivalence principle"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 14)
  )

###########################################################################################

# COMPARISON OF HP AND GF

# Step 3: Compute A_x for all ages and years in both scenarios
results_high <- hp_results
results_green <- gf_results

# Step 4: Prepare reserve data (age 17 to 100)
reserve_high <- results_high %>%
  filter(Year %in% seq(2023, 2033, 2), Age >= 17, Age <= 100) %>%
  mutate(t = Age - 17) %>%
  select(Year, t, Age, Reserve_High = Ax)

reserve_green <- results_green %>%
  filter(Year %in% seq(2023, 2033, 2), Age >= 17, Age <= 100) %>%
  mutate(t = Age - 17) %>%
  select(Year, t, Age, Reserve_Green = Ax)

# Step 5: Join and compute reserve difference
reserve_diff <- reserve_high %>%
  inner_join(reserve_green, by = c("Year", "Age", "t")) %>%
  mutate(Diff = Reserve_High - Reserve_Green) %>%
  group_by(Year) %>%
  mutate(Diff = ifelse(t == 0, 0, Diff)) %>%
  ungroup()

# Step 6: Plot reserve difference
ggplot(reserve_diff, aes(x = t, y = Diff, color = as.factor(Year))) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_line(size = 1.2) +
  labs(
    title = "Difference in Reserve Over Time: High Pollution − Green Future",
    x = "Time since age 17 (t)",
    y = expression(Delta * " Reserve = " * A[17 + t]^High - A[17 + t]^Green),
    color = "Projection Year"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 14),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )

#--------------------------------------------------------------------------------------
# Step 3: Compute A_x for all ages and years in both scenarios
results_high <- hp_results
results_green <- gf_results

# Step 4: Prepare reserve data (age 50 to 100)
reserve_high <- results_high %>%
  filter(Year %in% seq(2023, 2033, 2), Age >= 50, Age <= 100) %>%
  mutate(t = Age - 50) %>%
  select(Year, t, Age, Reserve_High = Ax)

reserve_green <- results_green %>%
  filter(Year %in% seq(2023, 2033, 2), Age >= 50, Age <= 100) %>%
  mutate(t = Age - 50) %>%
  select(Year, t, Age, Reserve_Green = Ax)

# Step 5: Join and compute reserve difference
reserve_diff <- reserve_high %>%
  inner_join(reserve_green, by = c("Year", "Age", "t")) %>%
  mutate(Diff = Reserve_High - Reserve_Green) %>%
  group_by(Year) %>%
  mutate(Diff = ifelse(t == 0, 0, Diff)) %>%
  ungroup()

# Step 6: Plot reserve difference
ggplot(reserve_diff, aes(x = t, y = Diff, color = as.factor(Year))) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_line(size = 1.2) +
  labs(
    title = "Difference in Reserve Over Time: High Pollution − Green Future",
    x = "Time since age 50 (t)",
    y = expression(Delta * " Reserve = " * A[50 + t]^High - A[50 + t]^Green),
    color = "Projection Year"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 14),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10))

#---------------------------------------------------------------------------------
# Step 3: Compute A_x for all ages and years in both scenarios
results_high <- hp_results
results_green <- gf_results

# Step 4: Prepare reserve data (age 84 to 100)
reserve_high <- results_high %>%
  filter(Year %in% seq(2023, 2033, 2), Age >= 84, Age <= 100) %>%
  mutate(t = Age - 84) %>%
  select(Year, t, Age, Reserve_High = Ax)

reserve_green <- results_green %>%
  filter(Year %in% seq(2023, 2033, 2), Age >= 84, Age <= 100) %>%
  mutate(t = Age - 84) %>%
  select(Year, t, Age, Reserve_Green = Ax)

# Step 5: Join and compute reserve difference
reserve_diff <- reserve_high %>%
  inner_join(reserve_green, by = c("Year", "Age", "t")) %>%
  mutate(Diff = Reserve_High - Reserve_Green) %>%
  group_by(Year) %>%
  mutate(Diff = ifelse(t == 0, 0, Diff)) %>%
  ungroup()

# Step 6: Plot reserve difference
ggplot(reserve_diff, aes(x = t, y = Diff, color = as.factor(Year))) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_line(size = 1.2) +
  labs(
    title = "Difference in Reserve Over Time: High Pollution − Green Future",
    x = "Time since age 84 (t)",
    y = expression(Delta * " Reserve = " * A[84 + t]^High - A[84 + t]^Green),
    color = "Projection Year"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 14),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10))
    
