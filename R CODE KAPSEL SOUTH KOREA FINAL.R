# ---- 1. Load packages ----
# install.packages(c("readxl", "dplyr", "ggplot2", "reshape2"))  # run once
library(readxl)
library(dplyr)
library(ggplot2)
library(reshape2)

# ---- 2. Read the dataset ----
file_path <- "C:/Users/olivi/Downloads/Final Dataset_south korea.xlsx"  # adjust if in another folder
df <- read_excel(file_path, sheet = 1)

# ---- 3. Keep only numeric vars & drop Year ----
num_df <- df %>% 
  select(where(is.numeric)) %>%       # keep numeric columns
  select(-Year)                       # drop Year

# ---- 4. Compute correlations vs qx ----
corr_vec <- cor(num_df, use = "pairwise.complete.obs")["qx", ]
corr_tbl <- tibble(Variable = names(corr_vec),
                   Correlation_with_qx = as.numeric(corr_vec)) %>% 
  arrange(desc(abs(Correlation_with_qx)))

print(corr_tbl)

# ---- 5. (Optional) visualize the whole matrix ----
corr_mat <- cor(num_df, use = "pairwise.complete.obs")
melted   <- melt(corr_mat)

ggplot(melted, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(limits = c(-1, 1)) +
  labs(title = "Correlation Matrix", fill = "r") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# If you only want the qx column as a bar‑chart:
ggplot(corr_tbl, aes(x = reorder(Variable, Correlation_with_qx), 
                     y = Correlation_with_qx)) +
  geom_col() +
  coord_flip() +
  labs(title = "Correlation with qx",
       x = "", y = "Pearson r") +
  theme_minimal()

### random forest model
rf_fit <- randomForest(
  qx ~ Age + PM2.5+ NO2 + SO2 + CO,
  data = df,
  ntree = 1000,
  mtry  = 3,
  importance = TRUE
)

# View fit summary and variable importance
print(rf_fit)
print(importance(rf_fit))
### predict
proj_path <- "C:/Users/olivi/Downloads/FINAL PROJECTED_SOUTH KOREA.xlsx"
sheets    <- c("High", "Green")          # exact sheet names in the file

predict_df <- function(sheet_name) {
  
  df <- read_excel(proj_path, sheet = sheet_name) |>
    mutate(across(everything(), ~ ifelse(. == "" | is.na(.), 0, .)))  # blanks → 0
  
  # make.names so "PM2.5" becomes "PM2.5" etc.  (randomForest is OK with dots)
  names(df) <- make.names(names(df))
  
  # Random‑forest prediction & clipping
  df$qx_pred <- predict(rf_fit, newdata = df) |>
    pmax(0) |>
    pmin(1)
  
  return(df)
}

out_list <- lapply(sheets, predict_df)
names(out_list) <- sheets

############################################################################
# 3.  Write predictions to a new Excel workbook
############################################################################
out_file <- "C:/Users/olivi/Downloads/Projection_with_qx_predictions24.xlsx"
write.xlsx(out_list, file = out_file, overwrite = TRUE)

cat("✅ Finished! Workbook with predictions saved to:", out_file, "\n")
##################
# install.packages(c("scam", "readxl", "openxlsx", "dplyr"))
library(scam)
library(readxl)
library(openxlsx)
library(dplyr)

train_df <- read_excel(train_path) |> select(all_of(vars))

# Safety: keep qx strictly between 0 and 1 for a logit link
train_df <- train_df |> 
  mutate(qx_adj = pmin(pmax(qx, 1e-6), 1 - 1e-6),
         logit_qx = qlogis(qx_adj))

# Monotone‑increasing smoothers:  bs = "mpi"
scam_fit <- scam(
  logit_qx ~ s(Age,  k = 20, bs = "ps")  + 
    s(PM2.5, k = 10, bs = "mpi") +
    s(NO2,   k = 10, bs = "mpi") +
    s(SO2,   k = 10, bs = "mpi") +
    s(CO,    k = 10, bs = "mpi") +
    s(O3,    k = 10, bs = "mpi"),
  data = train_df,
  family = gaussian(),   # because we modeled the logit
  sp = -1                # let scam choose smoothing parameters
)

# Prediction helper
predict_scam <- function(df) {
  lp <- predict(scam_fit, newdata = df)          # linear predictor on logit scale
  plogis(lp)                                     # back‑transform to qx
}

# Apply to each sheet
predict_sheet_scam <- function(sh) {
  df <- read_excel(proj_path, sheet = sh) |> 
    mutate(across(everything(), ~ ifelse(. == "" | is.na(.), 0, .)))
  names(df) <- make.names(names(df))
  df$qx_pred <- predict_scam(df)
  df
}

pred_list <- lapply(sheets, predict_sheet_scam)
names(pred_list) <- sheets

out_file <- "C:/Users/olivi/Downloads/Projection_scam_monotone.xlsx"
write.xlsx(pred_list, file = out_file, overwrite = TRUE)
cat("✅ Monotone‑GAM predictions written to:", out_file, "\n")
###################XG BOOST
# === 1. Load Required Libraries ===
install.packages(c("xgboost", "readxl", "openxlsx", "dplyr"))
library(xgboost)
library(readxl)
library(openxlsx)
library(dplyr)

# === 2. Load and Prepare Training Data ===
train_path <- "C:/Users/olivi/Downloads/Final Dataset_south korea.xlsx"
train_df <- read_excel(train_path)

# Make sure column names are safe
names(train_df) <- make.names(names(train_df))

# Drop rows with missing values (or handle otherwise)
train_df <- na.omit(train_df)

# Separate predictors and target
y <- train_df$qx
X <- train_df %>% select(Age, PM2.5, NO2, SO2, CO, O3)  # Adjust as needed

# Convert to matrix for xgboost
dtrain <- xgb.DMatrix(data = as.matrix(X), label = y)

# === 3. Define Monotonic Constraints ===
# All pollutant variables and age assumed to have increasing effect on qx → constraint = 1
mono_constraints <- c(1, 1, 1, 1, 1, 1)

# === 4. Train XGBoost with Monotonic Constraints ===
params <- list(
  booster = "gbtree",
  objective = "reg:squarederror",
  eta = 0.1,
  max_depth = 6,
  subsample = 0.8,
  colsample_bytree = 0.8,
  monotone_constraints = mono_constraints
)

set.seed(42)
xgb_model <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = 300,
  verbose = 0
)

# === 5. Load Projected Data ===
proj_path <- "C:/Users/olivi/Downloads/FINAL PROJECTED_SOUTH KOREA.xlsx"
sheets <- c("High", "Green")

predict_scenario <- function(sheet) {
  df <- read_excel(proj_path, sheet = sheet)
  names(df) <- make.names(names(df))
  
  Xproj <- df %>% select(Age, PM2.5, NO2, SO2, CO, O3)
  dproj <- xgb.DMatrix(as.matrix(Xproj))
  
  df$qx_pred <- predict(xgb_model, dproj) %>% pmin(1) %>% pmax(0)
  df
}

pred_list <- lapply(sheets, predict_scenario)
names(pred_list) <- sheets

# === 6. Write Predictions to New Excel ===
out_file <- "C:/Users/olivi/Downloads/qx_predictions_monotonic_xgb.xlsx"
write.xlsx(pred_list, file = out_file, overwrite = TRUE)

cat("✅ Predictions saved to:", out_file, "\n")
### GAM MODEL
# === 1. Load Required Libraries ===
install.packages(c("scam", "readxl", "openxlsx", "dplyr"))
library(scam)
library(readxl)
library(openxlsx)
library(dplyr)

# === 2. Load and Prepare Training Data ===
train_path <- "C:/Users/olivi/Downloads/Final Dataset_south korea.xlsx"
train_df <- read_excel(train_path)
names(train_df) <- make.names(names(train_df))
train_df <- na.omit(train_df)

# Slight adjustment to qx to avoid logit issues at boundaries
train_df <- train_df %>%
  mutate(qx = pmin(pmax(qx, 1e-6), 1 - 1e-6))

# === 3. Fit Monotone GAM ===
scam_fit <- scam(
  qx ~ s(Age, bs = "mpi") +        # Monotonic increasing in Age
    s(PM2.5, bs = "mpi") +      # Monotonic increasing in PM2.5
    s(NO2, bs = "mpi") +        # Monotonic increasing in NO2
    s(SO2, bs = "mpi") +        # Monotonic increasing in SO2
    s(CO, bs = "mpi") +         # Monotonic increasing in CO
    s(O3, bs = "mpi"),          # Monotonic increasing in O3
  data = train_df,
  family = quasibinomial(link = "logit")  # ensures qx in [0,1]
)

summary(scam_fit)

# === 4. Load Projected Data ===
proj_path <- "C:/Users/olivi/Downloads/FINAL PROJECTED_SOUTH KOREA.xlsx"
sheets <- c("High", "Green")

predict_scenario <- function(sheet) {
  df <- read_excel(proj_path, sheet = sheet)
  names(df) <- make.names(names(df))
  
  # Predict qx using scam_fit
  df$qx_pred <- predict(scam_fit, newdata = df, type = "response") %>%
    pmin(1) %>% pmax(0)
  
  return(df)
}

pred_list <- lapply(sheets, predict_scenario)
names(pred_list) <- sheets

# === 5. Write Predictions to Excel ===
out_file <- "C:/Users/olivi/Downloads/qx_predictions_monotonic_GAM.xlsx"
write.xlsx(pred_list, file = out_file, overwrite = TRUE)

cat("✅ Predictions saved to:", out_file, "\n")

