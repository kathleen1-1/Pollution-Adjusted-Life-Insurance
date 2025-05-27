#1 Historical PM2.5 data
pm25 <- c(78, 72, 68, 59, 66, 65, 71, 76, 69, 61, 58, 60, 61, 55, 54, 49, 47, 41, 45, 46, 45, 48, 44, 40, 42, 35, 38, 33, 38)
years <- 1995:2023

# Create data frame
historical <- data.frame(Year = years, PM2.5 = pm25)

# Simulation parameters
n_years <- 10
n_sim <- 1000

mean_val <- mean(pm25)
sd_val <- sd(pm25)

# High Pollution Scenario
set.seed(123)
high_pollution_sim <- matrix(rnorm(n_years * n_sim, mean = mean_val, sd = sd_val), ncol = n_years)
high_pollution_mean <- apply(high_pollution_sim, 2, mean)
high_pollution_ci <- apply(high_pollution_sim, 2, quantile, probs = c(0.025, 0.975))

# Green Future Scenario with 2.1% annual reduction
base_pm25 <- tail(pm25, 1)
reduction_rate <- 0.41
x <- (1 - reduction_rate)^(1/30)
reduction_per_year = 1-x
reduction_rate = reduction_per_year
green_future_mean <- base_pm25 * (1 - reduction_rate)^(1:n_years)

# Smaller sd for Green Future
green_sd <- sd_val / 2

# Simulate year-by-year using mapply for varying means
set.seed(456)
green_future_sim <- sapply(green_future_mean, function(mu) rnorm(n_sim, mean = mu, sd = green_sd))
green_future_ci <- apply(green_future_sim, 2, quantile, probs = c(0.025, 0.975))

# Combine into data frame
future_years <- 2024:2033  # Total of 10 years
sim_df <- data.frame(
  Year = rep(future_years, 2),
  Scenario = rep(c("High Pollution", "Green Future"), each = n_years),
  Mean = c(high_pollution_mean, green_future_mean),
  Lower = c(high_pollution_ci[1,], green_future_ci[1,]),
  Upper = c(high_pollution_ci[2,], green_future_ci[2,])
)


# Plot
library(ggplot2)

ggplot() +
  geom_line(data = historical, aes(x = Year, y = PM2.5), color = "black", size = 1.2) +
  geom_line(data = sim_df, aes(x = Year, y = Mean, color = Scenario), size = 1) +
  geom_ribbon(data = sim_df, aes(x = Year, ymin = Lower, ymax = Upper, fill = Scenario), alpha = 0.2) +
  scale_color_manual(values = c("High Pollution" = "red", "Green Future" = "green4")) +
  scale_fill_manual(values = c("High Pollution" = "red", "Green Future" = "green4")) +
  labs(title = "Projected PM2.5 Levels in Singapore (2024–2034)",
       y = "PM2.5 Annual Mean (µg/m³)",
       x = "Year") +
  theme_minimal()

#High pollution
# Given values
P <- 38          # Initial value in 2023
FV <- 53.30504  # Final value in 2033
n <- 10          # Number of years

# Calculate compound annual growth rate
i <- (FV / P)^(1 / n) - 1

# Generate yearly values
years <- 2023:(2023 + n)
values <- P * (1 + i)^(0:n)

# Print results
cat("Compound annual growth rate (i):", round(i * 100, 4), "%\n")
cat("1-year change (2023-2024):", round(values[2] - values[1], 4), "\n")
cat("10-year change:", round(values[11] - values[1], 4), "\n")

# Plot
plot(years, values, type = "b", pch = 19, col = "darkgreen", lwd = 2,
     main = "PM10 Growth (High Pollution Scenario)",
     xlab = "Year", ylab = "PM10 Value (µg/m³)",
     ylim = c(min(values) - 2, max(values) + 2))

#green
# Given values
P <- 38          # Initial value in 2023
FV <- 31.87138  # Final value in 2033
n <- 10          # Number of years

# Calculate compound annual growth rate
i <- (FV / P)^(1 / n) - 1

# Generate yearly values
years <- 2023:(2023 + n)
values <- P * (1 + i)^(0:n)

# Print results
cat("Compound annual growth rate (i):", round(i * 100, 4), "%\n")
cat("1-year change (2023-2024):", round(values[2] - values[1], 4), "\n")
cat("10-year change:", round(values[11] - values[1], 4), "\n")

# Plot
plot(years, values, type = "b", pch = 19, col = "darkgreen", lwd = 2,
     main = "PM10 Growth (High Pollution Scenario)",
     xlab = "Year", ylab = "PM10 Value (µg/m³)",
     ylim = c(min(values) - 2, max(values) + 2))

#---------------------------------------------------------------------------------------
#2 Historical sulphur data
sulphur <- c(0.017, 0.013, 0.011, 0.008, 0.007, 0.006, 0.005, 0.005, 0.005, 0.005, 0.005, 0.005, 0.006, 0.006, 0.005, 0.005, 0.005, 0.005, 0.006, 0.006, 0.005, 0.005, 0.005, 0.004, 0.004, 0.003, 0.003, 0.003, 0.003)

years <- 1995:2023

# Create data frame
historical <- data.frame(Year = years, sulphur = sulphur)
mean_val <- mean(sulphur)
sd_val <- sd(sulphur)

# Simulation parameters
n_years <- 10
n_sim <- 1000

# High Pollution Scenario
set.seed(123)
high_pollution_sim <- matrix(rnorm(n_years * n_sim, mean = mean_val, sd = sd_val), ncol = n_years)
high_pollution_mean <- apply(high_pollution_sim, 2, mean)
high_pollution_ci <- apply(high_pollution_sim, 2, quantile, probs = c(0.025, 0.975))

# Green Future Scenario with 91% cumulative reduction over 10 years
base_val <- tail(sulphur, 1)
reduction_rate <- 0.91
x <- (1 - reduction_rate)^(1/30)
reduction_per_year = 1-x
reduction_rate = reduction_per_year
green_future_mean <- base_val * (1 - reduction_rate)^(1:n_years)

# Smaller sd for Green Future
sd_val <- sd(sulphur)
green_sd <- sd_val / 2

# Simulate year-by-year using mapply for varying means
set.seed(456)
green_future_sim <- sapply(green_future_mean, function(mu) rnorm(n_sim, mean = mu, sd = green_sd))
green_future_ci <- apply(green_future_sim, 2, quantile, probs = c(0.025, 0.975))

# Combine into data frame
future_years <- 2024:2033
sim_df <- data.frame(
  Year = rep(future_years, 2),
  Scenario = rep(c("High Pollution", "Green Future"), each = n_years),
  Mean = c(high_pollution_mean, green_future_mean),
  Lower = c(high_pollution_ci[1,], green_future_ci[1,]),
  Upper = c(high_pollution_ci[2,], green_future_ci[2,])
)

# Plot
library(ggplot2)

ggplot() +
  geom_line(data = historical, aes(x = Year, y = sulphur), color = "black", size = 1.2) +
  geom_line(data = sim_df, aes(x = Year, y = Mean, color = Scenario), size = 1) +
  geom_ribbon(data = sim_df, aes(x = Year, ymin = Lower, ymax = Upper, fill = Scenario), alpha = 0.2) +
  scale_color_manual(values = c("High Pollution" = "red", "Green Future" = "green4")) +
  scale_fill_manual(values = c("High Pollution" = "red", "Green Future" = "green4")) +
  labs(title = "Projected Sulphur Dioxide Levels in Singapore (2024–2033)",
       y = "Sulphur Dioxide Maximum 24-Hour Mean (µg/m³)",
       x = "Year") +
  theme_minimal()

#High pollution
# Given values
P <- 0.003          # Initial value in 2023
FV <-  0.005751697  # Final value in 2033
n <- 10          # Number of years

# Calculate compound annual growth rate
i <- (FV / P)^(1 / n) - 1

# Generate yearly values
years <- 2023:(2023 + n)
values <- P * (1 + i)^(0:n)

# Print results
cat("Compound annual growth rate (i):", round(i * 100, 4), "%\n")
cat("1-year change (2023-2024):", round(values[2] - values[1], 4), "\n")
cat("10-year change:", round(values[11] - values[1], 4), "\n")

# Plot
plot(years, values, type = "b", pch = 19, col = "darkgreen", lwd = 2,
     main = "PM10 Growth (High Pollution Scenario)",
     xlab = "Year", ylab = "PM10 Value (µg/m³)",
     ylim = c(min(values) - 2, max(values) + 2))
#green 
# Given values
P <- 0.003         # Initial value in 2023
FV <- 0.001344421 # Final value in 2033
n <- 10          # Number of years

# Calculate compound annual growth rate
i <- (FV / P)^(1 / n) - 1

# Generate yearly values
years <- 2023:(2023 + n)
values <- P * (1 + i)^(0:n)

# Print results
cat("Compound annual growth rate (i):", round(i * 100, 4), "%\n")
cat("1-year change (2023-2024):", round(values[2] - values[1], 4), "\n")
cat("10-year change:", round(values[11] - values[1], 4), "\n")

# Plot
plot(years, values, type = "b", pch = 19, col = "darkgreen", lwd = 2,
     main = "PM10 Growth (High Pollution Scenario)",
     xlab = "Year", ylab = "PM10 Value (µg/m³)",
     ylim = c(min(values) - 2, max(values) + 2))
#-----------------------------------------------------------------------------------------
#3 Historical PM10 data
my_data <- c(24, 24, 28, 25, 30, 29, 25, 26, 37, 30, 31, 29, 27, 26, 29, 25, 27)

PM10 <- rev(my_data)

years <- 2007:2023

# Create data frame
historical <- data.frame(Year = years, PM10=PM10)

# Simulation parameters
n_years <- 10
n_sim <- 1000

latest_val <- tail(PM10, 1)
mean_val <- latest_val * 1.15  # High pollution: 15% increase
sd_val <- sd(PM10)

# High Pollution Scenario
set.seed(123)
high_pollution_sim <- matrix(rnorm(n_years * n_sim, mean = mean_val, sd = sd_val), ncol = n_years)
high_pollution_mean <- apply(high_pollution_sim, 2, mean)
high_pollution_ci <- apply(high_pollution_sim, 2, quantile, probs = c(0.025, 0.975))

# Green Future Scenario with 26% total reduction over 10 years
reduction_rate <- 0.26
x <- (1 - reduction_rate)^(1/30)
reduction_per_year = 1-x
reduction_rate = reduction_per_year
green_future_mean <- latest_val * (1 - reduction_rate)^(1:n_years)
green_sd <- sd_val / 2

set.seed(456)
green_future_sim <- sapply(green_future_mean, function(mu) rnorm(n_sim, mean = mu, sd = green_sd))
green_future_ci <- apply(green_future_sim, 2, quantile, probs = c(0.025, 0.975))

# Combine into data frame
future_years <- 2024:2033
sim_df <- data.frame(
  Year = rep(future_years, 2),
  Scenario = rep(c("High Pollution", "Green Future"), each = n_years),
  Mean = c(high_pollution_mean, green_future_mean),
  Lower = c(high_pollution_ci[1,], green_future_ci[1,]),
  Upper = c(high_pollution_ci[2,], green_future_ci[2,])
)

# Plot
library(ggplot2)

ggplot() +
  geom_line(data = historical, aes(x = Year, y = PM10), color = "black", size = 1.2) +
  geom_line(data = sim_df, aes(x = Year, y = Mean, color = Scenario), size = 1) +
  geom_ribbon(data = sim_df, aes(x = Year, ymin = Lower, ymax = Upper, fill = Scenario), alpha = 0.2) +
  scale_color_manual(values = c("High Pollution" = "red", "Green Future" = "green4")) +
  scale_fill_manual(values = c("High Pollution" = "red", "Green Future" = "green4")) +
  labs(title = "Projected PM10 Levels in Singapore (2024–2033)",
       y = "PM10 Annual Mean (µg/m³)",
       x = "Year") +
  theme_minimal()
# Given values
P <- 24          # Initial value in 2023
FV <- 27.44449   # Final value in 2033
n <- 10          # Number of years

# Calculate compound annual growth rate
i <- (FV / P)^(1 / n) - 1

# Generate yearly values
years <- 2023:(2023 + n)
values <- P * (1 + i)^(0:n)

# Print results
cat("Compound annual growth rate (i):", round(i * 100, 4), "%\n")
cat("1-year change (2023-2024):", round(values[2] - values[1], 4), "\n")
cat("10-year change:", round(values[11] - values[1], 4), "\n")

# Plot
plot(years, values, type = "b", pch = 19, col = "darkgreen", lwd = 2,
     main = "PM10 Growth (High Pollution Scenario)",
     xlab = "Year", ylab = "PM10 Value (µg/m³)",
     ylim = c(min(values) - 2, max(values) + 2))
#green
P <- 24          # Initial value in 2023
FV <- 21.70810   # Final value in 2033
n <- 10          # Number of years

# Calculate compound annual growth rate
i <- (FV / P)^(1 / n) - 1

# Generate yearly values
years <- 2023:(2023 + n)
values <- P * (1 + i)^(0:n)

# Print results
cat("Compound annual growth rate (i):", round(i * 100, 4), "%\n")
cat("1-year change (2023-2024):", round(values[2] - values[1], 4), "\n")
cat("10-year change:", round(values[11] - values[1], 4), "\n")

# Plot
plot(years, values, type = "b", pch = 19, col = "darkgreen", lwd = 2,
     main = "PM10 Growth (High Pollution Scenario)",
     xlab = "Year", ylab = "PM10 Value (µg/m³)",
     ylim = c(min(values) - 2, max(values) + 2))
#-------------------------------------------------------------------------------------
#4 Historical carbon data
carbon <- c(1.300, 1.200, 1.200, 1.100, 1.100, 1.000, 0.900, 0.700, 0.600, 0.600, 0.600, 0.600, 0.700, 0.600, 0.600, 0.500, 0.600, 0.500, 0.500, 0.500, 0.500, 0.500, 0.500, 0.500, 0.500, 0.500, 0.500, 0.400, 0.500)

years <- 1995:2023

# Create data frame
historical <- data.frame(Year = years, carbon = carbon)

# Simulation parameters
n_years <- 10  # Extending the years to 10
n_sim <- 1000
mean=mean(carbon)
sd=sd(carbon)
# High Pollution Scenario (no change)
set.seed(123)
high_pollution_sim <- matrix(rnorm(n_years * n_sim, mean = mean, sd = sd), ncol = n_years)
high_pollution_mean <- apply(high_pollution_sim, 2, mean)
high_pollution_ci <- apply(high_pollution_sim, 2, quantile, probs = c(0.025, 0.975))

# Green Future Scenario with 73% reduction
base_carbon <- tail(carbon, 1)  # Start with the last observed value


# Apply 73% reduction: keeping 27% of the previous year's value
reduction_rate <- 0.73
x <- (1 - reduction_rate)^(1/30)
reduction_per_year = 1-x
reduction_rate = reduction_per_year
green_future_mean <- base_carbon * (1 - reduction_rate)^(1:n_years)

# Simulate year-by-year for Green Future scenario
set.seed(456)
green_future_sim <- sapply(green_future_mean, function(mu) rnorm(n_sim, mean = mu, sd = 1.5))
green_future_ci <- apply(green_future_sim, 2, quantile, probs = c(0.025, 0.975))

# Combine into data frame for future years
future_years <- 2024:2033  # 10 years in total
sim_df <- data.frame(
  Year = rep(future_years, 2),
  Scenario = rep(c("High Pollution", "Green Future"), each = n_years),
  Mean = c(high_pollution_mean, green_future_mean),
  Lower = c(high_pollution_ci[1,], green_future_ci[1,]),
  Upper = c(high_pollution_ci[2,], green_future_ci[2,])
)

# Plot
library(ggplot2)

ggplot() +
  geom_line(data = historical, aes(x = Year, y = carbon), color = "black", size = 1.2) +
  geom_line(data = sim_df, aes(x = Year, y = Mean, color = Scenario), size = 1) +
  geom_ribbon(data = sim_df, aes(x = Year, ymin = Lower, ymax = Upper, fill = Scenario), alpha = 0.2) +
  scale_color_manual(values = c("High Pollution" = "red", "Green Future" = "green4")) +
  scale_fill_manual(values = c("High Pollution" = "red", "Green Future" = "green4")) +
  labs(title = "Projected Carbon Monoxide Levels in Singapore (2024–2033)",
       y = "Carbon Monoxide (Maximum 8-Hour Mean) Annual Mean (µg/m³)",
       x = "Year") +
  theme_minimal()

#High pollution
# Given values
P <- 0.500  # Initial value in 2023
FV <- 0.6701612   # Final value in 2033
n <- 10  # Number of years

# Calculate compound annual growth rate
i <- (FV / P)^(1 / n) - 1

# Generate yearly values
years <- 2023:(2023 + n)
values <- P * (1 + i)^(0:n)

# Print results
cat("Compound annual growth rate (i):", round(i * 100, 4), "%\n")
cat("1-year change (2023-2024):", round(values[2] - values[1], 4), "\n")
cat("10-year change:", round(values[11] - values[1], 4), "\n")

# Plot
plot(years, values, type = "b", pch = 19, col = "darkgreen", lwd = 2,
     main = "Carbon Monoxide Growth (High pollution)",
     xlab = "Year", ylab = "Value",
     ylim = c(min(values) - 5, max(values) + 5))
#green
# Given values
P <- 0.500  # Initial value in 2023
FV <- 0.3231652  # Final value in 2033
n <- 10  # Number of years

# Calculate compound annual growth rate
i <- (FV / P)^(1 / n) - 1

# Generate yearly values
years <- 2023:(2023 + n)
values <- P * (1 + i)^(0:n)

# Print results
cat("Compound annual growth rate (i):", round(i * 100, 4), "%\n")
cat("1-year change (2023-2024):", round(values[2] - values[1], 4), "\n")
cat("10-year change:", round(values[11] - values[1], 4), "\n")

# Plot
plot(years, values, type = "b", pch = 19, col = "darkgreen", lwd = 2,
     main = "Carbon Monoxide Growth (Green)",
     xlab = "Year", ylab = "Value",
     ylim = c(min(values) - 5, max(values) + 5))
#------------------------------------------------------------------------------------
#5 Historical ozone data
ozone_data <- c(0.013, 0.015, 0.016, 0.017, 0.016, 0.017, 0.015, 0.014, 0.014, 0.014, 0.017, 0.018, 0.018, 0.019, 0.021, 0.019, 0.019, 0.021, 0.022, 0.023, 0.022, 0.024, 0.025, 0.023, 0.025, 0.025, 0.028, 0.029, 0.031)

years <- 1995:2023

# Create data frame
historical <- data.frame(Year = years, Ozone = ozone_data)

# Basic statistics
mean_ozone <- mean(ozone_data)
sd_ozone <- sd(ozone_data)

# Simulation parameters
n_years <- 10
n_sim <- 1000

# High Pollution Scenario (increase mean by 20%, larger spread)
high_pollution_mean_value <- mean_ozone
high_pollution_sd <- sd_ozone

set.seed(123)
high_pollution_sim <- matrix(rnorm(n_years * n_sim, mean = high_pollution_mean_value, sd = high_pollution_sd), ncol = n_years)
high_pollution_mean <- apply(high_pollution_sim, 2, mean)
high_pollution_ci <- apply(high_pollution_sim, 2, quantile, probs = c(0.025, 0.975))

# Green Future Scenario (25% reduction over 10 years using compound decline)
base_ozone <- tail(ozone_data, 1)  # Last known ozone value

reduction_rate <- 0.25
x <- (1 - reduction_rate)^(1/30)
reduction_per_year = 1-x
reduction_rate = reduction_per_year
green_future_mean <- base_ozone * (1 - reduction_rate)^(1:n_years)

# Simulate values around green scenario mean
set.seed(456)
green_future_sim <- sapply(green_future_mean, function(mu) rnorm(n_sim, mean = mu, sd = sd_ozone))
green_future_ci <- apply(green_future_sim, 2, quantile, probs = c(0.025, 0.975))

# Combine into one data frame
future_years <- 2024:2033
sim_df <- data.frame(
  Year = rep(future_years, 2),
  Scenario = rep(c("High Pollution", "Green Future"), each = n_years),
  Mean = c(high_pollution_mean, green_future_mean),
  Lower = c(high_pollution_ci[1,], green_future_ci[1,]),
  Upper = c(high_pollution_ci[2,], green_future_ci[2,])
)

# Plotting
library(ggplot2)

ggplot() +
  geom_line(data = historical, aes(x = Year, y = Ozone), color = "black", size = 1.2) +
  geom_line(data = sim_df, aes(x = Year, y = Mean, color = Scenario), size = 1) +
  geom_ribbon(data = sim_df, aes(x = Year, ymin = Lower, ymax = Upper, fill = Scenario), alpha = 0.2) +
  scale_color_manual(values = c("High Pollution" = "red", "Green Future" = "green4")) +
  scale_fill_manual(values = c("High Pollution" = "red", "Green Future" = "green4")) +
  labs(title = "Projected Ozone Levels in Singapore (2024–2033)",
       y = "Ozone (Maximum 8-Hour Mean) Annual Mean (µg/m³)",
       x = "Year") +
  theme_minimal()


#High pollution
# Given values
P <- 0.031  # Initial value in 2023
FV <- 144.7591 # Final value in 2033
n <- 10  # Number of years

# Calculate compound annual growth rate
i <- (FV / P)^(1 / n) - 1

# Generate yearly values
years <- 2023:(2023 + n)
values <- P * (1 + i)^(0:n)

# Print results
cat("Compound annual growth rate (i):", round(i * 100, 4), "%\n")
cat("1-year change (2023-2024):", round(values[2] - values[1], 4), "\n")
cat("10-year change:", round(values[11] - values[1], 4), "\n")

# Plot
plot(years, values, type = "b", pch = 19, col = "darkgreen", lwd = 2,
     main = "Ozone Growth (High pollution)",
     xlab = "Year", ylab = "Value",
     ylim = c(min(values) - 5, max(values) + 5))
#green
# Given values
P <- 0.031  # Initial value in 2023
FV <- 0.02816537 # Final value in 2033
n <- 10  # Number of years

# Calculate compound annual growth rate
i <- (FV / P)^(1 / n) - 1

# Generate yearly values
years <- 2023:(2023 + n)
values <- P * (1 + i)^(0:n)

# Print results
cat("Compound annual growth rate (i):", round(i * 100, 4), "%\n")
cat("1-year change (2023-2024):", round(values[2] - values[1], 4), "\n")
cat("10-year change:", round(values[11] - values[1], 4), "\n")

# Plot
plot(years, values, type = "b", pch = 19, col = "darkgreen", lwd = 2,
     main = "Ozone Growth (Green)",
     xlab = "Year", ylab = "Value",
     ylim = c(min(values) - 5, max(values) + 5))
#---------------------------------------------------------------------------------------------
#6 Historical nitrogen data (reversed to be in chronological order)
nitrogen_data <- c(0.032, 0.033, 0.032, 0.030, 0.032, 0.035, 0.037, 0.036, 0.038, 0.037, 0.034, 0.036, 0.038, 0.038, 0.035, 0.034, 0.033, 0.030, 0.033, 0.033, 0.032, 0.031, 0.030, 0.028, 0.028, 0.024, 0.024, 0.021, 0.020)
years <- 1995:2023

# Create data frame with reversed data
historical <- data.frame(Year = years, Nitrogen = nitrogen_data)
mean_no <- mean(nitrogen_data)
sd_no <- sd(nitrogen_data)

# Simulation parameters
n_years <- 10  # Extending the years to 10
n_sim <- 1000

# High Pollution Scenario (realistic high pollution mean)
# For high pollution, use a higher mean and simulate around it
high_pollution_mean_value <- 30  # Adjusted to a more realistic higher value for high pollution
high_pollution_sd <- 3  # SD of 3 for high pollution scenario
set.seed(123)
high_pollution_sim <- matrix(rnorm(n_years * n_sim, mean = mean_no, sd = sd_no), ncol = n_years)
high_pollution_mean <- apply(high_pollution_sim, 2, mean)
high_pollution_ci <- apply(high_pollution_sim, 2, quantile, probs = c(0.025, 0.975))

# Green Future Scenario with 61% reduction
base_nitrogen <- tail(nitrogen_data, 1)  # Start with the last observed value (22)
reduction_rate <- 0.61  # 61% annual reduction
x <- (1 - reduction_rate)^(1/30)
reduction_per_year = 1-x
reduction_rate = reduction_per_year
green_future_mean <- base_nitrogen * (1 - reduction_rate)^(1:n_years)

# Simulate year-by-year for Green Future scenario
set.seed(456)
green_future_sim <- sapply(green_future_mean, function(mu) rnorm(n_sim, mean = mu, sd = 1.5))
green_future_ci <- apply(green_future_sim, 2, quantile, probs = c(0.025, 0.975))

# Combine into data frame for future years
future_years <- 2024:2033  # 10 years in total
sim_df <- data.frame(
  Year = rep(future_years, 2),
  Scenario = rep(c("High Pollution", "Green Future"), each = n_years),
  Mean = c(high_pollution_mean, green_future_mean),
  Lower = c(high_pollution_ci[1,], green_future_ci[1,]),
  Upper = c(high_pollution_ci[2,], green_future_ci[2,])
)

# Plot
library(ggplot2)

ggplot() +
  geom_line(data = historical, aes(x = Year, y = Nitrogen), color = "black", size = 1.2) +
  geom_line(data = sim_df, aes(x = Year, y = Mean, color = Scenario), size = 1) +
  geom_ribbon(data = sim_df, aes(x = Year, ymin = Lower, ymax = Upper, fill = Scenario), alpha = 0.2) +
  scale_color_manual(values = c("High Pollution" = "red", "Green Future" = "green4")) +
  scale_fill_manual(values = c("High Pollution" = "red", "Green Future" = "green4")) +
  labs(title = "Projected Nitrogen Levels in Singapore (2024–2033)",
       y = "Nitrogen (Annual Mean) µg/m³",
       x = "Year") +
  theme_minimal()
# High pollution
P <- 0.020
FV <- 0.03163025  
n <- 10

# Solve for i (compound annual growth rate)
i <- (FV / P)^(1 / n) - 1

# Compute future values for each year
years <- 2023:(2023 + n)
pollution <- P * (1 + i)^(0:n)

# Print results
cat("Compound annual growth rate (i):", round(i * 100, 4), "%\n")
cat("1-year growth (2023 to 2024):", round(pollution[2] - pollution[1], 4), "\n")
cat("10-year change:", round(pollution[11] - pollution[1], 4), "\n")

# Plot
plot(years, pollution, type = "b", pch = 19, col = "red", lwd = 2,
     main = "Projected Nirogen Monoxide Growth (High pollution)",
     xlab = "Year", ylab = "Pollution Level",
     ylim = c(min(pollution) - 1, max(pollution) + 1))
#green
# Given values
P <- 0.020  # Initial value in 2023
FV <- 0.01461229   # Final value in 2033
n <- 10  # Number of years

# Calculate compound annual rate
i <- (FV / P)^(1 / n) - 1

# Generate yearly values
years <- 2023:(2023 + n)
pollution <- P * (1 + i)^(0:n)

# Print results
cat("Compound annual rate of change (i):", round(i * 100, 4), "%\n")
cat("1-year change:", round(pollution[2] - pollution[1], 4), "\n")
cat("10-year change:", round(pollution[11] - pollution[1], 4), "\n")

# Plot
plot(years, pollution, type = "b", pch = 19, col = "forestgreen", lwd = 2,
     main = "Projected Pollution Decline (Compound Rate)",
     xlab = "Year", ylab = "Pollution Level",
     ylim = c(min(pollution) - 0.5, max(pollution) + 0.5))

#_____________________________________________________________________________

#O3
year <- 1995:2023
o3 <- c(0.013, 0.015, 0.016, 0.017, 0.016, 0.017, 0.015, 0.014, 0.014, 0.014,
        0.017, 0.018, 0.018, 0.019, 0.021, 0.019, 0.019, 0.021, 0.022, 0.023,
        0.022, 0.024, 0.025, 0.023, 0.025, 0.025, 0.028, 0.029, 0.031)

# Fit linear model
model <- lm(o3 ~ year)

# Summary
summary(model)

# Predict future (2024–2030)
future_years <- data.frame(year = 2024:2033)
predict(model, newdata = future_years)
