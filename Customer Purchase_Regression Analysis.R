######################## MARKETING ANALYTICS REGRESSION ANALYSIS ########################


# Purpose:
# Explain customer PurchaseAmount using evidence-based regression modeling.
# The script develops three models, compares them, tests the required
# assumptions, and produces the tables and figures used in the report.


#### 1. PACKAGES AND DATA IMPORT ----

# Packages are checked but are not installed inside the analysis script.
required_packages <- c(
  "readxl", "dplyr", "tidyr", "ggplot2", "car", "lmtest", "broom"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0L) {
  stop(
    "Install these packages before running the analysis: ",
    paste(missing_packages, collapse = ", "),
    call. = FALSE
  )
}

invisible(lapply(required_packages, library, character.only = TRUE))

# The workbook is read directly from the working directory, as requested.
marketing_raw <- read_excel("marketing_analytics_dataset_500.xlsx")


#### 2. INITIAL INSPECTION AND DATA QUALITY AUDIT ----

dim(marketing_raw)
names(marketing_raw)
str(marketing_raw)
summary(marketing_raw)
head(marketing_raw)

# Preserve the imported data and clean a separate working copy.
# Clean, normalize casing, and convert Gender and Region to a categorical factor
marketing_data <- marketing_raw %>%
  mutate(
    Gender = factor(tools::toTitleCase(tolower(trimws(as.character(Gender))))),
    Region = factor(tools::toTitleCase(tolower(trimws(as.character(Region)))))
  )

# Missingness, duplicates, identifiers, and category balance.
missing_summary <- data.frame(
  Variable = names(marketing_data),
  Missing_Count = colSums(is.na(marketing_data)),
  Missing_Percent = round(colMeans(is.na(marketing_data)) * 100, 2),
  row.names = NULL
)
missing_summary

duplicate_row_count <- sum(duplicated(marketing_data))
duplicate_customer_id_count <- sum(duplicated(marketing_data$CustomerID))

data_quality_overview <- data.frame(
  Check = c(
    "Rows", "Columns", "Missing cells", "Duplicate rows",
    "Duplicate CustomerID values"
  ),
  Result = c(
    nrow(marketing_data),
    ncol(marketing_data),
    sum(is.na(marketing_data)),
    duplicate_row_count,
    duplicate_customer_id_count
  )
)
data_quality_overview

#The data set appears to have no missing cells, duplicates and unique customer IDs

table(marketing_data$Gender, useNA = "ifany")
table(marketing_data$Region, useNA = "ifany")
table(marketing_data$PromoUsed, useNA = "ifany")
table(marketing_data$Churn, useNA = "ifany")

# Check whether values are within interpretable ranges.
range_checks <- data.frame(
  Check = c(
    "Age outside 18-100",
    "Income less than or equal to zero",
    "Negative behavioural counts",
    "Clicks greater than ad impressions",
    "PromoUsed not coded 0/1",
    "SatisfactionScore outside 1-5",
    "PurchaseAmount less than or equal to zero",
    "Churn not coded 0/1"
  ),
  Flagged_Rows = c(
    sum(marketing_data$Age < 18 | marketing_data$Age > 100, na.rm = TRUE),
    sum(marketing_data$Income <= 0, na.rm = TRUE),
    sum(
      apply(
        marketing_data %>%
          select(WebsiteVisits, PagesViewed, AdImpressions, Clicks, EmailOpens),
        1,
        function(x) any(x < 0)
      )
    ),
    sum(marketing_data$Clicks > marketing_data$AdImpressions, na.rm = TRUE),
    sum(!marketing_data$PromoUsed %in% c(0, 1), na.rm = TRUE),
    sum(
      marketing_data$SatisfactionScore < 1 |
        marketing_data$SatisfactionScore > 5,
      na.rm = TRUE
    ),
    sum(marketing_data$PurchaseAmount <= 0, na.rm = TRUE),
    sum(!marketing_data$Churn %in% c(0, 1), na.rm = TRUE)
  )
)
range_checks

# There are 32 Clicks > AdImpressions cases being flagged for source validation. The IQR
# rule below will independently remove two of these rows; the remaining 30 will not be
# altered because the intended correction cannot be inferred. Clicks and
# AdImpressions will not be used in the final models.
click_impression_flags <- marketing_data %>%
  filter(Clicks > AdImpressions)
nrow(click_impression_flags)


#### 3. OUTLIER SCREENING AND TREATMENT DECISION ----

# IQR screening is appropriate for continuous/count variables. Binary fields
# are excluded because the IQR rule would incorrectly label valid 0/1 levels.
continuous_variables <- c(
  "Age", "Income", "WebsiteVisits", "TimeOnSite", "PagesViewed",
  "AdImpressions", "Clicks", "EmailOpens", "SatisfactionScore",
  "PurchaseAmount"
)

iqr_outlier_summary <- function(data, variable) {
  values <- data[[variable]]
  q1 <- quantile(values, 0.25, na.rm = TRUE)
  q3 <- quantile(values, 0.75, na.rm = TRUE)
  iqr_value <- q3 - q1
  lower_fence <- q1 - 1.5 * iqr_value
  upper_fence <- q3 + 1.5 * iqr_value
  flagged <- values < lower_fence | values > upper_fence

  data.frame(
    Variable = variable,
    Lower_Fence = lower_fence,
    Upper_Fence = upper_fence,
    Outlier_Count = sum(flagged, na.rm = TRUE),
    Outlier_Percent = round(mean(flagged, na.rm = TRUE) * 100, 2),
    row.names = NULL
  )
}

outlier_profile <- bind_rows(
  lapply(
    continuous_variables,
    function(variable) iqr_outlier_summary(marketing_data, variable)
  )
)
outlier_profile

# Explicitly identify the 25 PurchaseAmount values that were questioned. These
# observations are displayed for review and are subsequently removed as part of
# the combined IQR rule. They are not retained in analysis_data.
purchase_q1 <- quantile(marketing_data$PurchaseAmount, 0.25, na.rm = TRUE)
purchase_q3 <- quantile(marketing_data$PurchaseAmount, 0.75, na.rm = TRUE)
purchase_iqr <- purchase_q3 - purchase_q1
purchase_lower_fence <- purchase_q1 - 1.5 * purchase_iqr
purchase_upper_fence <- purchase_q3 + 1.5 * purchase_iqr

marketing_data <- marketing_data %>%
  mutate(
    Purchase_IQR_Outlier = PurchaseAmount < purchase_lower_fence |
      PurchaseAmount > purchase_upper_fence
  )

purchase_amount_outliers <- marketing_data %>%
  filter(Purchase_IQR_Outlier) %>%
  select(
    CustomerID, PurchaseAmount, Income, PromoUsed,
    PagesViewed, WebsiteVisits
  )

purchase_amount_outliers
nrow(purchase_amount_outliers)

# Create one TRUE/FALSE outlier flag for every continuous variable. The fences
# are calculated from the original data so the removal rule is fixed and fully
# reproducible. A row is removed when at least one continuous field is outside
# its original 1.5 x IQR fences.
outlier_flag_matrix <- sapply(
  continuous_variables,
  function(variable) {
    values <- marketing_data[[variable]]
    q1 <- quantile(values, 0.25, na.rm = TRUE)
    q3 <- quantile(values, 0.75, na.rm = TRUE)
    iqr_value <- q3 - q1
    lower_fence <- q1 - 1.5 * iqr_value
    upper_fence <- q3 + 1.5 * iqr_value
    values < lower_fence | values > upper_fence
  }
)

marketing_data <- marketing_data %>%
  mutate(
    Outlier_Flag_Count = rowSums(outlier_flag_matrix, na.rm = TRUE),
    Any_IQR_Outlier = Outlier_Flag_Count > 0
  )

# We keep both versions. The cleaned data are used for EDA, all three primary
# models, and all primary diagnostics. The complete data will be used only for the
# outlier sensitivity analysis in Section 8.6.
full_sample_data <- marketing_data
excluded_outlier_rows <- full_sample_data %>% filter(Any_IQR_Outlier)
analysis_data <- full_sample_data %>% filter(!Any_IQR_Outlier)

# Mandatory verification for the supplied workbook. The script stops before
# modeling if any outlier remains in the primary analysis or if the expected
# removal counts are not reproduced.
stopifnot(
  nrow(full_sample_data) == 5000L,
  nrow(purchase_amount_outliers) == 25L,
  nrow(excluded_outlier_rows) == 139L,
  nrow(analysis_data) == 4861L,
  sum(analysis_data$Any_IQR_Outlier, na.rm = TRUE) == 0L,
  !any(
    purchase_amount_outliers$CustomerID %in% analysis_data$CustomerID
  )
)

cat(
  "OUTLIER REMOVAL CONFIRMED:\n",
  "- Original observations:", nrow(full_sample_data), "\n",
  "- PurchaseAmount outliers removed:", nrow(purchase_amount_outliers), "\n",
  "- Total unique IQR-flagged rows removed:", nrow(excluded_outlier_rows), "\n",
  "- Primary model observations:", nrow(analysis_data), "\n",
  "- IQR-flagged rows remaining in primary model data:",
  sum(analysis_data$Any_IQR_Outlier, na.rm = TRUE), "\n"
)

outlier_treatment_summary <- data.frame(
  Original_Rows = nrow(full_sample_data),
  Removed_Rows = nrow(excluded_outlier_rows),
  Removed_Percent = round(
    100 * nrow(excluded_outlier_rows) / nrow(full_sample_data),
    2
  ),
  Analysis_Rows = nrow(analysis_data)
)
outlier_treatment_summary

# Result: 139 rows (2.78%) are removed, leaving 4,861 observations. The union
# is smaller than the sum of the per-variable counts because a customer can be
# flagged on more than one field.
excluded_outlier_rows %>%
  select(
    CustomerID, Outlier_Flag_Count, PurchaseAmount, Income,
    WebsiteVisits, TimeOnSite, PagesViewed, Clicks
  )


#### 4. EXPLORATORY ANALYSIS AND VARIABLE SELECTION ----

# PurchaseAmount is selected as the dependent variable because it is continuous,
# directly relevant to customer value

summary(analysis_data$PurchaseAmount)
shapiro.test(analysis_data$PurchaseAmount)

# PurchaseAmount is found to be approximately symmetric, and suitable for
# evaluating linear and alternative regression structures.

# CustomerID is an identifier, not a behavioural predictor. Churn is excluded
# because its timing relative to the purchase is unknown and it could be a
# downstream outcome rather than an antecedent predictor.
candidate_numeric <- c(
  "Age", "Income", "WebsiteVisits", "TimeOnSite", "PagesViewed",
  "AdImpressions", "Clicks", "EmailOpens", "PromoUsed",
  "SatisfactionScore", "PurchaseAmount"
)

correlation_matrix <- cor(
  analysis_data %>% select(all_of(candidate_numeric)),
  use = "complete.obs",
  method = "pearson"
)
round(correlation_matrix, 3)

correlation_with_purchase <- sort(
  correlation_matrix[, "PurchaseAmount"],
  decreasing = TRUE
)
correlation_with_purchase

# Correlation heatmap. This is the initial screen for both outcome association
# and large pairwise correlations among possible predictors.
correlation_long <- as.data.frame(as.table(correlation_matrix))
names(correlation_long) <- c("Variable_1", "Variable_2", "Correlation")

correlation_long <- correlation_long %>%
  mutate(
    Row = match(Variable_1, candidate_numeric),
    Column = match(Variable_2, candidate_numeric)
  ) %>%
  filter(Row >= Column)

plot_correlation <- ggplot(
  correlation_long,
  aes(x = Variable_2, y = Variable_1, fill = Correlation)
) +
  geom_tile(color = "white", linewidth = 0.3) +
  geom_text(aes(label = sprintf("%.2f", Correlation)), size = 3) +
  scale_fill_gradient2(
    low = "#3B73B9", mid = "white", high = "#B23A3A",
    midpoint = 0, limits = c(-1, 1)
  ) +
  scale_x_discrete(limits = candidate_numeric) +
  scale_y_discrete(limits = rev(candidate_numeric)) +
  labs(
    title = "Correlation Matrix: Candidate Numeric Variables",
    x = NULL, y = NULL, fill = "Pearson r"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold")
  )

plot_correlation
ggsave(
  "Figure_1_Correlation_Matrix.png",
  plot = plot_correlation,
  width = 10,
  height = 8,
  dpi = 300,
  bg = "white"
)

# Boxplots use the original sample so the values removed by the IQR rule remain
# visible. All relationship plots below use the cleaned analysis sample.
outlier_plot_data <- full_sample_data %>%
  select(all_of(continuous_variables)) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Value")

plot_outliers <- ggplot(outlier_plot_data, aes(x = "", y = Value)) +
  geom_boxplot(fill = "#4F81BD", alpha = 0.75, outlier.color = "#B23A3A") +
  facet_wrap(~Variable, scales = "free", ncol = 3) +
  labs(title = "IQR Outlier Screening Before Removal", x = NULL, y = NULL) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    plot.title = element_text(face = "bold")
  )

plot_outliers
ggsave(
  "Figure_2_Outlier_Boxplots.png",
  plot = plot_outliers,
  width = 10,
  height = 8,
  dpi = 300,
  bg = "white"
)

# Density evidence: PurchaseAmount is close to symmetric rather than strongly
# skewed, so a log transformation is not imposed before modeling.
plot_purchase_density <- ggplot(analysis_data, aes(x = PurchaseAmount)) +
  geom_histogram(aes(y = after_stat(density)), bins = 35,
                 fill = "#8DB3E2", color = "white") +
  geom_density(color = "#1F4E79", linewidth = 1.1) +
  labs(
    title = "Distribution of Purchase Amount",
    x = "Purchase amount (units)",
    y = "Density"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

plot_purchase_density
ggsave(
  "Figure_3_Purchase_Amount_Density.png",
  plot = plot_purchase_density,
  width = 7,
  height = 5,
  dpi = 300,
  bg = "white"
)

# Scatterplots compare a fitted straight line with a flexible LOESS smoother.
# Their close alignment is direct evidence that a linear main-effect structure
# is reasonable for the three selected quantitative predictors.
plot_income <- ggplot(analysis_data, aes(x = Income / 1000, y = PurchaseAmount)) +
  geom_point(alpha = 0.18, size = 1, color = "#17365D") +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE,
              color = "#B94A48", linewidth = 0.9) +
  geom_smooth(method = "loess", formula = y ~ x, se = FALSE,
              color = "#159D9B", linetype = "dashed", linewidth = 0.9) +
  labs(
    title = "Purchase Amount and Income",
    x = "Income (thousands)",
    y = "Purchase amount (units)"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

plot_income
ggsave(
  "Figure_4_Income_Scatterplot.png",
  plot = plot_income,
  width = 7,
  height = 5,
  dpi = 300,
  bg = "white"
)

plot_pages <- ggplot(analysis_data, aes(x = PagesViewed, y = PurchaseAmount)) +
  geom_jitter(width = 0.10, height = 0, alpha = 0.16,
              size = 1, color = "#17365D") +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE,
              color = "#B94A48", linewidth = 0.9) +
  geom_smooth(method = "loess", formula = y ~ x, se = FALSE,
              color = "#159D9B", linetype = "dashed", linewidth = 0.9) +
  labs(
    title = "Purchase Amount and Pages Viewed",
    x = "Pages viewed",
    y = "Purchase amount (units)"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

plot_pages
ggsave(
  "Figure_5_Pages_Viewed_Scatterplot.png",
  plot = plot_pages,
  width = 7,
  height = 5,
  dpi = 300,
  bg = "white"
)

plot_visits <- ggplot(analysis_data, aes(x = WebsiteVisits, y = PurchaseAmount)) +
  geom_jitter(width = 0.10, height = 0, alpha = 0.16,
              size = 1, color = "#17365D") +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE,
              color = "#B94A48", linewidth = 0.9) +
  geom_smooth(method = "loess", formula = y ~ x, se = FALSE,
              color = "#159D9B", linetype = "dashed", linewidth = 0.9) +
  labs(
    title = "Purchase Amount and Website Visits",
    x = "Website visits",
    y = "Purchase amount (units)"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

plot_visits
ggsave(
  "Figure_6_Website_Visits_Scatterplot.png",
  plot = plot_visits,
  width = 7,
  height = 5,
  dpi = 300,
  bg = "white"
)

# PromoUsed is binary, so its relationship is assessed with group distributions.
plot_promo <- ggplot(
  analysis_data,
  aes(x = factor(PromoUsed), y = PurchaseAmount, fill = factor(PromoUsed))
) +
  geom_boxplot(width = 0.55, alpha = 0.80, show.legend = FALSE) +
  scale_fill_manual(values = c("#6C757D", "#D99B2B")) +
  labs(
    title = "Purchase Amount by Promotion Use",
    x = "Promotion used (0 = No, 1 = Yes)",
    y = "Purchase amount (units)"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

plot_promo
ggsave(
  "Figure_7_Promotion_Boxplot.png",
  plot = plot_promo,
  width = 7,
  height = 5,
  dpi = 300,
  bg = "white"
)

# The four selected predictors have the clearest observed associations:
# Income r = .569, PromoUsed r = .442, PagesViewed r = .369, and
# WebsiteVisits r = .288. WebsiteVisits is retained because its positive trend
# is visually clear and it still adds meaningful explanatory power in Model 2 which will be the multiple regreesion.
selected_predictors <- c(
  "Income", "PromoUsed", "PagesViewed", "WebsiteVisits"
)

selected_correlation_matrix <- cor(
  analysis_data %>% select(PurchaseAmount, all_of(selected_predictors)),
  use = "complete.obs"
)
round(selected_correlation_matrix, 4)

# Initial multicollinearity screen: no selected predictor pair approaches |.70|.
predictor_correlation_matrix <- cor(
  analysis_data %>% select(all_of(selected_predictors)),
  use = "complete.obs"
)
round(predictor_correlation_matrix, 4)

high_correlation_positions <- which(
  abs(predictor_correlation_matrix) >= 0.70 &
    lower.tri(predictor_correlation_matrix),
  arr.ind = TRUE
)

if (nrow(high_correlation_positions) == 0L) {
  high_correlation_pairs <- data.frame(
    Predictor_1 = character(),
    Predictor_2 = character(),
    Correlation = numeric()
  )
} else {
  high_correlation_pairs <- data.frame(
    Predictor_1 = rownames(predictor_correlation_matrix)[
      high_correlation_positions[, "row"]
    ],
    Predictor_2 = colnames(predictor_correlation_matrix)[
      high_correlation_positions[, "col"]
    ],
    Correlation = predictor_correlation_matrix[high_correlation_positions]
  )
}
high_correlation_pairs


#### 5. MODEL DEVELOPMENT ----

# MODEL 1: Baseline model with the strongest single continuous predictor (Income).
model_1 <- lm(PurchaseAmount ~ Income, data = analysis_data)

# MODEL 2: Multiple regression with four evidence-supported predictors.
model_2 <- lm(
  PurchaseAmount ~ Income + PromoUsed + PagesViewed + WebsiteVisits,
  data = analysis_data
)

# MODEL 3: Alternative interaction model.
# PagesViewed is mean-centred so the PromoUsed coefficient compares promotion
# groups at a typical page-view level and interaction VIFs remain interpretable.
analysis_data <- analysis_data %>%
  mutate(PagesViewed_c = PagesViewed - mean(PagesViewed, na.rm = TRUE))

model_3 <- lm(
  PurchaseAmount ~ Income + PromoUsed * PagesViewed_c + WebsiteVisits,
  data = analysis_data
)

# The interaction asks whether promotion use changes the return associated with
# viewing additional pages. This is a testable marketing question.
plot_interaction <- ggplot(
  analysis_data,
  aes(
    x = PagesViewed,
    y = PurchaseAmount,
    color = factor(PromoUsed),
    fill = factor(PromoUsed)
  )
) +
  geom_jitter(width = 0.10, height = 0, alpha = 0.10, size = 0.9) +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE, linewidth = 1) +
  scale_color_manual(
    values = c("#17365D", "#D99B2B"),
    labels = c("No promotion", "Promotion used")
  ) +
  scale_fill_manual(
    values = c("#17365D", "#D99B2B"),
    labels = c("No promotion", "Promotion used")
  ) +
  labs(
    title = "Promotion x Page-View Interaction Check",
    x = "Pages viewed",
    y = "Purchase amount (units)",
    color = "Group",
    fill = "Group"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

plot_interaction
#The exploratory group lines are close enough to justify testing rather than
# assuming identical slopes.

ggsave(
  "Figure_8_Interaction_Check.png",
  plot = plot_interaction,
  width = 7.5,
  height = 5.2,
  dpi = 300,
  bg = "white"
)


#### 6. COMPLETE MODEL OUTPUT AND EQUATIONS ----

# Complete console output requested in the assignment.
summary(model_1)
confint(model_1, level = 0.95)

summary(model_2)
confint(model_2, level = 0.95)

summary(model_3)
confint(model_3, level = 0.95)

# Tidy coefficient tables combine estimates, standard errors, test statistics,
# p-values, and 95% confidence intervals in one reproducible output.
model_1_coefficients <- tidy(model_1, conf.int = TRUE, conf.level = 0.95)
model_2_coefficients <- tidy(model_2, conf.int = TRUE, conf.level = 0.95)
model_3_coefficients <- tidy(model_3, conf.int = TRUE, conf.level = 0.95)

model_1_coefficients
model_2_coefficients
model_3_coefficients

# Display the fitted equations using the estimated coefficients.
b1 <- coef(model_1)
b2 <- coef(model_2)
b3 <- coef(model_3)

cat(
  sprintf(
    paste0(
      "Model 1: PurchaseAmount = %.4f + %.6f(Income)\n"
    ),
    b1[1], b1[2]
  )
)

cat(
  sprintf(
    paste0(
      "Model 2: PurchaseAmount = %.4f + %.6f(Income) + ",
      "%.4f(PromoUsed) + %.4f(PagesViewed) + %.4f(WebsiteVisits)\n"
    ),
    b2[1], b2["Income"], b2["PromoUsed"],
    b2["PagesViewed"], b2["WebsiteVisits"]
  )
)

cat(
  sprintf(
    paste0(
      "Model 3: PurchaseAmount = %.4f + %.6f(Income) + ",
      "%.4f(PromoUsed) + %.4f(PagesViewed_c) + %.4f(WebsiteVisits) + ",
      "%.4f(PromoUsed x PagesViewed_c)\n"
    ),
    b3[1], b3["Income"], b3["PromoUsed"], b3["PagesViewed_c"],
    b3["WebsiteVisits"], b3["PromoUsed:PagesViewed_c"]
  )
)


#### 7. MODEL FIT AND COMPARISON ----

extract_model_fit <- function(model, model_name) {
  model_glance <- glance(model)

  data.frame(
    Model = model_name,
    R_Squared = model_glance$r.squared,
    Adjusted_R_Squared = model_glance$adj.r.squared,
    RMSE = sqrt(mean(residuals(model)^2)),
    Residual_Standard_Error = model_glance$sigma,
    AIC = AIC(model),
    BIC = BIC(model),
    F_Statistic = model_glance$statistic,
    Model_df = model_glance$df,
    Residual_df = df.residual(model),
    F_p_Value = model_glance$p.value,
    row.names = NULL
  )
}

model_fit_table <- bind_rows(
  extract_model_fit(model_1, "Model 1: Income"),
  extract_model_fit(model_2, "Model 2: Four main effects"),
  extract_model_fit(model_3, "Model 3: Page-view interaction")
)
model_fit_table

# Model 2 and Model 3 are nested; this partial F-test evaluates whether the
# interaction contributes enough information to justify the extra complexity.
model_2_vs_model_3 <- anova(model_2, model_3)
model_2_vs_model_3

# Standardized slopes permit comparison across predictors measured on very
# different scales. PromoUsed is standardized here only for importance ranking;
# its unstandardized coefficient remains the clearest business interpretation.
analysis_data <- analysis_data %>%
  mutate(
    PurchaseAmount_z = as.numeric(scale(PurchaseAmount)),
    Income_z = as.numeric(scale(Income)),
    PromoUsed_z = as.numeric(scale(PromoUsed)),
    PagesViewed_z = as.numeric(scale(PagesViewed)),
    WebsiteVisits_z = as.numeric(scale(WebsiteVisits))
  )

model_2_standardized <- lm(
  PurchaseAmount_z ~ Income_z + PromoUsed_z + PagesViewed_z + WebsiteVisits_z,
  data = analysis_data
)

standardized_coefficients <- tidy(model_2_standardized) %>%
  filter(term != "(Intercept)") %>%
  select(term, estimate, std.error, statistic, p.value)
standardized_coefficients

# Unique R-squared change shows how much explanatory power is lost when each
# predictor is removed from Model 2 while the other three remain.
unique_contribution <- bind_rows(
  lapply(selected_predictors, function(predictor) {
    reduced_predictors <- setdiff(selected_predictors, predictor)
    reduced_model <- lm(
      reformulate(reduced_predictors, response = "PurchaseAmount"),
      data = analysis_data
    )

    full_sse <- sum(residuals(model_2)^2)
    reduced_sse <- sum(residuals(reduced_model)^2)

    data.frame(
      Predictor = predictor,
      Standardized_Beta = unname(
        coef(model_2_standardized)[paste0(predictor, "_z")]
      ),
      Unique_Delta_R_Squared = summary(model_2)$r.squared -
        summary(reduced_model)$r.squared,
      Partial_R_Squared = (reduced_sse - full_sse) / reduced_sse,
      row.names = NULL
    )
  })
)
unique_contribution

# Selection result in straightforward language:
# Model 2 is the best choice for this dataset. It predicts purchase amount much
# more accurately than the one-predictor model (Model 1). Model 3 adds an interaction, but
# that extra term is not statistically significant (p = .171) and produces
# almost no improvement. Model 2 therefore gives the clearest useful answer.


#### 7.1 PLAIN-LANGUAGE RESULT INTERPRETATION ----

summary(residuals(model_2))

# Interpret the results:
#
# 1. Residuals (actual PurchaseAmount minus predicted PurchaseAmount):
#    a. The middle 50% of Model 2 prediction errors are approximately -6.90 to
#       +6.91 purchase units, so most fitted values are reasonably close.
#    b. The largest negative residual is about -41.67. This means the model
#       overpredicted that customer's purchase by about 41.67 units.
#    c. The largest positive residual is about +34.65. This means the model
#       underpredicted that customer's purchase by about 34.65 units.
#    d. The median residual is about 0.01, so the model does not systematically
#       overpredict or underpredict the cleaned sample.
#
# 2. Coefficient table for Model 2, holding the other predictors constant:
#    a. An additional 10,000 income units are associated with about 4.96 more
#       purchase units (0.000496 x 10,000).
#    b. Customers who used a promotion have an expected purchase amount about
#       20.01 units higher than otherwise similar non-users.
#    c. Each additional page viewed is associated with about 1.47 more purchase
#       units, and each additional website visit with about 1.99 more units.
#    d. All four predictor p-values are below .001, so each predictor contributes
#       evidence after controlling for the other three.
#    e. The standard errors are small relative to the slopes: approximately
#       0.00000582 for Income, 0.289 for PromoUsed, 0.026 for PagesViewed, and
#       0.047 for WebsiteVisits. This indicates precise estimates in this sample.
#    f. The intercept is not significant (p = .323), but it is not a useful
#       marketing scenario because zero income, pages, and visits are outside the
#       meaningful joint context of the data.
#
# 3. Model fit:
#    a. Model 2 explains 76.99% of the variation in PurchaseAmount (R-squared =
#       .7699); adjusted R-squared is nearly identical at .7697.
#    b. RMSE is 10.06 units, compared with 17.26 for Model 1, a reduction of
#       about 41.7% in typical prediction error.
#    c. The overall F-statistic is 4062.85 with p < .001, showing that the four
#       predictors are jointly useful.
#    d. R-squared cannot decrease when predictors are added. Adjusted R-squared
#       applies a penalty for extra terms. Model 3's adjusted R-squared changes
#       only from .7697 to .7698, so its extra interaction is not worthwhile.
#    e. These are in-sample results. A future holdout sample is still needed
#       before claiming the same prediction accuracy for new customers.
#
# Insights:
# 1. Promotion use has the largest directly actionable difference: about 20.01
#    purchase units. Test promotions with randomized holdout groups before
#    treating this association as causal sales lift.
# 2. Meaningful engagement matters. Two more visits and three more pages viewed
#    correspond to about 8.40 additional purchase units in the fitted model.
# 3. Income is the strongest statistical predictor. Use broad, consented income
#    bands for assortment or messaging, not discriminatory individual pricing.
# 4. Model 2 can support forecasts within the observed data ranges, but planning
#    should allow for a typical error of about 10 purchase units.


#### 8. DIAGNOSTIC TESTS ----

diagnostic_test_table <- function(model, model_name) {
  shapiro_result <- shapiro.test(residuals(model))
  bp_result <- bptest(model)
  reset_result <- resettest(model, power = 2:3, type = "fitted")
  cook_values <- cooks.distance(model)

  data.frame(
    Model = model_name,
    Shapiro_W = unname(shapiro_result$statistic),
    Shapiro_p = shapiro_result$p.value,
    BP_Statistic = unname(bp_result$statistic),
    BP_df = unname(bp_result$parameter),
    BP_p = bp_result$p.value,
    RESET_F = unname(reset_result$statistic),
    RESET_df1 = unname(reset_result$parameter[1]),
    RESET_df2 = unname(reset_result$parameter[2]),
    RESET_p = reset_result$p.value,
    Maximum_Cooks_D = max(cook_values),
    Cook_Above_4_n = sum(cook_values > 4 / nobs(model)),
    Abs_Studentized_Above_3 = sum(abs(rstudent(model)) > 3),
    row.names = NULL
  )
}

diagnostic_results <- bind_rows(
  diagnostic_test_table(model_1, "Model 1"),
  diagnostic_test_table(model_2, "Model 2"),
  diagnostic_test_table(model_3, "Model 3")
)
diagnostic_results


#### 8.1 Linearity: residual plots, component-plus-residual plots, RESET ----

make_residual_fitted_plot <- function(model, model_name) {
  plot_data <- data.frame(
    Fitted = fitted(model),
    Residual = residuals(model)
  )

  ggplot(plot_data, aes(x = Fitted, y = Residual)) +
    geom_point(alpha = 0.20, size = 1, color = "#17365D") +
    geom_smooth(method = "loess", formula = y ~ x, se = FALSE,
                color = "#B94A48", linewidth = 1) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(
      title = paste(model_name, "Residuals vs Fitted"),
      x = "Fitted purchase amount",
      y = "Residual"
    ) +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(face = "bold"))
}

plot_model_1_residuals <- make_residual_fitted_plot(model_1, "Model 1")
plot_model_2_residuals <- make_residual_fitted_plot(model_2, "Model 2")
plot_model_3_residuals <- make_residual_fitted_plot(model_3, "Model 3")

plot_model_1_residuals
plot_model_2_residuals
plot_model_3_residuals

ggsave(
  "Figure_9_Model_2_Residuals_vs_Fitted.png",
  plot = plot_model_2_residuals,
  width = 7,
  height = 5,
  dpi = 300,
  bg = "white"
)

# Component-plus-residual plots directly check whether each adjusted predictor
# relationship departs systematically from a straight line.
png(
  "Figure_10_Model_2_Component_Residual_Plots.png",
  width = 3000,
  height = 2400,
  res = 300
)
crPlots(model_2)
dev.off()

resettest(model_1, power = 2:3, type = "fitted")
resettest(model_2, power = 2:3, type = "fitted")
resettest(model_3, power = 2:3, type = "fitted")

# RESET gives Model 2 a small p-value, so check the three continuous predictors
# individually before deciding whether a curved term is actually useful.
diagnostic_curve_data <- analysis_data %>%
  mutate(
    Income_c = Income - mean(Income),
    WebsiteVisits_c = WebsiteVisits - mean(WebsiteVisits)
  )

model_2_income_quadratic <- lm(
  PurchaseAmount ~ Income + PromoUsed + PagesViewed + WebsiteVisits +
    I(Income_c^2),
  data = diagnostic_curve_data
)

model_2_pages_quadratic <- lm(
  PurchaseAmount ~ Income + PromoUsed + PagesViewed + WebsiteVisits +
    I(PagesViewed_c^2),
  data = diagnostic_curve_data
)

model_2_visits_quadratic <- lm(
  PurchaseAmount ~ Income + PromoUsed + PagesViewed + WebsiteVisits +
    I(WebsiteVisits_c^2),
  data = diagnostic_curve_data
)

quadratic_check_table <- data.frame(
  Added_Term = c("Income squared", "PagesViewed squared", "WebsiteVisits squared"),
  Estimate = c(
    coef(model_2_income_quadratic)["I(Income_c^2)"],
    coef(model_2_pages_quadratic)["I(PagesViewed_c^2)"],
    coef(model_2_visits_quadratic)["I(WebsiteVisits_c^2)"]
  ),
  P_Value = c(
    coef(summary(model_2_income_quadratic))["I(Income_c^2)", "Pr(>|t|)"],
    coef(summary(model_2_pages_quadratic))["I(PagesViewed_c^2)", "Pr(>|t|)"],
    coef(summary(model_2_visits_quadratic))["I(WebsiteVisits_c^2)", "Pr(>|t|)"]
  ),
  Change_in_AIC = c(
    AIC(model_2_income_quadratic) - AIC(model_2),
    AIC(model_2_pages_quadratic) - AIC(model_2),
    AIC(model_2_visits_quadratic) - AIC(model_2)
  ),
  row.names = NULL
)
quadratic_check_table

# Model 2 interpretation:
# The residual and component-plus-residual plots show only small departures from
# straight lines. RESET is significant at p = .033, which is a mild warning that
# some combined functional-form detail may be missing. However, the squared
# Income, PagesViewed, and WebsiteVisits terms are each non-significant
# (p = .531, .504, and .893) and all increase AIC. The warning is therefore
# reported as a limitation, but there is not enough evidence to replace the
# clear additive model with one of these specific curves. A spline or GAM should
# be tested on a validation sample if prediction becomes the main objective.


#### 8.2 Normality of residuals: Q-Q plot and Shapiro-Wilk ----

model_2_qq_data <- data.frame(Residual = residuals(model_2))

plot_model_2_qq <- ggplot(model_2_qq_data, aes(sample = Residual)) +
  stat_qq(color = "#17365D", alpha = 0.65, size = 1.2) +
  stat_qq_line(color = "#B94A48", linewidth = 1) +
  labs(
    title = "Model 2 Normal Q-Q Plot",
    x = "Theoretical quantiles",
    y = "Sample quantiles"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

plot_model_2_qq
ggsave(
  "Figure_11_Model_2_QQ_Plot.png",
  plot = plot_model_2_qq,
  width = 7,
  height = 5,
  dpi = 300,
  bg = "white"
)

shapiro.test(residuals(model_1))
shapiro.test(residuals(model_2))
shapiro.test(residuals(model_3))

# Model 1 fails Shapiro-Wilk (p < .001), showing that Income alone leaves a
# non-normal error pattern. Model 2 residuals closely follow the Q-Q line and
# Shapiro-Wilk is non-significant (p = .692). Normality is therefore acceptable
# for Model 2's t-tests and confidence intervals.


#### 8.3 Homoscedasticity: Breusch-Pagan and scale-location ----

bptest(model_1)
bptest(model_2)
bptest(model_3)

model_2_scale_data <- data.frame(
  Fitted = fitted(model_2),
  Sqrt_Absolute_Studentized = sqrt(abs(rstudent(model_2)))
)

plot_model_2_scale <- ggplot(
  model_2_scale_data,
  aes(x = Fitted, y = Sqrt_Absolute_Studentized)
) +
  geom_point(alpha = 0.20, size = 1, color = "#159D9B") +
  geom_smooth(method = "loess", formula = y ~ x, se = FALSE,
              color = "#B94A48", linewidth = 1) +
  labs(
    title = "Model 2 Scale-Location Plot",
    x = "Fitted purchase amount",
    y = "Sqrt(|studentized residual|)"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

plot_model_2_scale
ggsave(
  "Figure_12_Model_2_Scale_Location.png",
  plot = plot_model_2_scale,
  width = 7,
  height = 5,
  dpi = 300,
  bg = "white"
)

# Model 2's Breusch-Pagan test is non-significant (p = .581), and the
# scale-location smoother has no strong widening or narrowing pattern. There is
# no evidence that ordinary standard errors are being distorted by changing
# residual variance. If this pattern were present, HC3 robust standard errors or
# weighted regression would be considered.


#### 8.4 Multicollinearity: correlation matrix and VIF ----

model_2_vif <- data.frame(
  Predictor = names(vif(model_2)),
  VIF = as.numeric(vif(model_2)),
  row.names = NULL
)
model_2_vif

model_3_vif_values <- vif(model_3, type = "terms")
if (is.matrix(model_3_vif_values)) {
  model_3_vif <- data.frame(
    Predictor = rownames(model_3_vif_values),
    GVIF = model_3_vif_values[, "GVIF"],
    Degrees_of_Freedom = model_3_vif_values[, "Df"],
    Adjusted_GVIF = model_3_vif_values[, "GVIF^(1/(2*Df))"],
    row.names = NULL
  )
} else {
  model_3_vif <- data.frame(
    Predictor = names(model_3_vif_values),
    VIF = as.numeric(model_3_vif_values),
    row.names = NULL
  )
}
model_3_vif

# The correlation matrix screens pairwise overlap; VIF tests how well each
# predictor is explained by all other predictors jointly. Model 2 VIFs are
# approximately 1.00, so coefficient instability from multicollinearity is not
# a concern. Centering keeps Model 3 interaction VIFs below about 2.0.


#### 8.5 Influential observations: Cook's distance ----

model_2_cook <- cooks.distance(model_2)
cook_threshold <- 4 / nobs(model_2)

cook_summary <- data.frame(
  Threshold_4_n = cook_threshold,
  Maximum_Cooks_D = max(model_2_cook),
  Count_Above_4_n = sum(model_2_cook > cook_threshold),
  Count_Above_1 = sum(model_2_cook > 1)
)
cook_summary

top_cook_positions <- order(model_2_cook, decreasing = TRUE)[1:10]
top_influence_cases <- analysis_data[top_cook_positions, ] %>%
  mutate(
    Model_2_Residual = residuals(model_2)[top_cook_positions],
    Cooks_D = model_2_cook[top_cook_positions]
  ) %>%
  select(
    CustomerID, PurchaseAmount, Income, PromoUsed, PagesViewed,
    WebsiteVisits, Model_2_Residual, Cooks_D
  )
top_influence_cases

cook_plot_data <- data.frame(
  Observation = seq_along(model_2_cook),
  CustomerID = analysis_data$CustomerID,
  Cooks_D = model_2_cook
)

plot_cook <- ggplot(cook_plot_data, aes(x = Observation, y = Cooks_D)) +
  geom_segment(
    aes(xend = Observation, y = 0, yend = Cooks_D),
    color = "#17365D", alpha = 0.45, linewidth = 0.25
  ) +
  geom_hline(
    yintercept = cook_threshold,
    color = "#B94A48",
    linetype = "dashed",
    linewidth = 1
  ) +
  geom_text(
    data = cook_plot_data %>% slice_max(Cooks_D, n = 5),
    aes(label = CustomerID),
    vjust = -0.5,
    size = 3
  ) +
  labs(
    title = "Model 2 Cook's Distance",
    subtitle = "Dashed line shows the 4/n screening threshold",
    x = "Observation index",
    y = "Cook's distance"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

plot_cook
ggsave(
  "Figure_13_Model_2_Cooks_Distance.png",
  plot = plot_cook,
  width = 9,
  height = 5,
  dpi = 300,
  bg = "white"
)

# In the cleaned 4,861-row sample, the sensitive 4/n screen flags 237 cases for
# review. Maximum Cook's D is only .00415 and none exceeds 1, so no remaining
# customer controls the fitted coefficients. These cases are reviewed rather
# than automatically removed because they are not influential enough to change
# the model on their own.


#### 8.6 Outlier sensitivity analysis ----

# The primary Model 2 excludes all 139 rows flagged by the IQR rule. For the
# sensitivity check, restore those rows and refit the same equation on the
# original 5,000 observations.
model_2_with_outliers <- lm(
  PurchaseAmount ~ Income + PromoUsed + PagesViewed + WebsiteVisits,
  data = full_sample_data
)

outlier_sensitivity <- data.frame(
  Term = names(coef(model_2)),
  Cleaned_Sample_Estimate = unname(coef(model_2)),
  With_Outliers_Estimate = unname(coef(model_2_with_outliers))
) %>%
  mutate(
    Percent_Change = ifelse(
      Term == "(Intercept)",
      NA_real_,
      100 *
        (With_Outliers_Estimate - Cleaned_Sample_Estimate) /
        Cleaned_Sample_Estimate
    )
  )
outlier_sensitivity

outlier_sensitivity_fit <- data.frame(
  Sample = c("Cleaned sample", "Original sample with outliers"),
  Observations = c(nobs(model_2), nobs(model_2_with_outliers)),
  R_Squared = c(
    summary(model_2)$r.squared,
    summary(model_2_with_outliers)$r.squared
  ),
  RMSE = c(
    sqrt(mean(residuals(model_2)^2)),
    sqrt(mean(residuals(model_2_with_outliers)^2))
  )
)
outlier_sensitivity_fit

# Restoring the outliers changes the four slopes by no more than 1.32%.
# R-squared changes from .7699 to .7769 and RMSE from 10.06 to 10.13. The main
# conclusions are therefore stable, although the outlier-free model remains the
# reported model because the chosen cleaning rule excludes those observations.


#### 8.7 Supplementary independence check ----

# These are cross-sectional customer records, so independence is mainly a study-
# design assumption. CustomerID is unique, and Durbin-Watson = 2.033 indicates
# no visible residual pattern in the supplied row order.
dwtest(model_2)


#### 9. EXPORT TABLES ----

# write.csv(missing_summary, "Table_Data_Missingness.csv", row.names = FALSE)
# write.csv(range_checks, "Table_Range_Checks.csv", row.names = FALSE)
# write.csv(outlier_profile, "Table_Outlier_Profile.csv", row.names = FALSE)
# write.csv(outlier_treatment_summary, "Table_Outlier_Treatment.csv", row.names = FALSE)
# write.csv(excluded_outlier_rows, "Excluded_Outlier_Rows.csv", row.names = FALSE)
# write.csv(correlation_matrix, "Table_Correlation_Matrix.csv", row.names = TRUE)
# write.csv(model_1_coefficients, "Table_Model_1_Coefficients.csv", row.names = FALSE)
# write.csv(model_2_coefficients, "Table_Model_2_Coefficients.csv", row.names = FALSE)
# write.csv(model_3_coefficients, "Table_Model_3_Coefficients.csv", row.names = FALSE)
# write.csv(model_fit_table, "Table_Model_Fit_Comparison.csv", row.names = FALSE)
# write.csv(diagnostic_results, "Table_Diagnostic_Tests.csv", row.names = FALSE)
# write.csv(model_2_vif, "Table_Model_2_VIF.csv", row.names = FALSE)
# write.csv(model_3_vif, "Table_Model_3_VIF.csv", row.names = FALSE)
# write.csv(unique_contribution, "Table_Model_2_Unique_Contribution.csv", row.names = FALSE)
# write.csv(quadratic_check_table, "Table_Quadratic_Checks.csv", row.names = FALSE)
# write.csv(outlier_sensitivity, "Table_Outlier_Sensitivity.csv", row.names = FALSE)
# write.csv(outlier_sensitivity_fit, "Table_Outlier_Sensitivity_Fit.csv", row.names = FALSE)


#### 10. FINAL ANALYTICAL CONCLUSION ----

# Model 2 is the best overall choice. It is much more stronger and accurate than Model 1, and
# Model 3's extra interaction does not add enough useful information. Its
# coefficients are also easy to explain to a marketing manager. The RESET test
# gives a small functional-form warning, so future work should compare splines
# or a GAM on a validation sample. Because the data are observational, these
# results describe associations and predictions rather than proven causal lift.

