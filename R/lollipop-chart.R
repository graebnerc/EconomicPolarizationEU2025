# Load required libraries
here::i_am("R/radar-charts.R")
library(tidyverse)
library(here)
library(fmsb)  # for radar charts
library(RColorBrewer)
load(here("data/processed/full_data.Rdata"))


var_names <- c(
  "adj_wage_share",
  "unemployment",
  "current_account",
  "public_debt_to_gdp",
  "va_finance",
  "trade_exp_GDP",
  "abs_fdi_percGDP",
  "bond_yield",
  "debt_total_corp",
  "gdp_real_pc_ppp"
)

head(full_data)

red_data <- full_data |>
  filter(variable_name %in% var_names) |>
  select(country, variable_name, year, value) |>
  pivot_wider(names_from = variable_name, values_from = value)


# Load required libraries
library(tidyverse)
library(RColorBrewer)
library(scales)

# Step 1: Create cluster assignments based on hierarchical clustering results
cluster_assignments <- tibble(
  country = c("Austria", "Belgium", "Bulgaria", "Cyprus", "Czechia", "Germany",
              "Denmark", "Spain", "Estonia", "Finland", "France", "Greece",
              "Croatia", "Hungary", "Ireland", "Italy", "Lithuania", "Luxembourg",
              "Latvia", "Malta", "Netherlands", "Poland", "Portugal", "Romania",
              "Slovakia", "Slovenia", "Sweden"),
  cluster = c(1, 1, 2, 3, 2, 1, 1, 4, 2, 1, 4, 4, 2, 2, 3, 4, 2, 5, 2, 3, 1, 2, 4, 2, 2, 2, 1)
)

# Step 2: Join cluster assignments with your data
clustered_data <- red_data %>%
  left_join(cluster_assignments, by = "country")

# Step 3: Calculate means and standard deviations for each cluster
cluster_stats <- clustered_data %>%
  group_by(cluster) %>%
  summarise(
    across(c(current_account, unemployment, adj_wage_share, public_debt_to_gdp,
             gdp_real_pc_ppp, trade_exp_GDP, bond_yield, va_finance,
             abs_fdi_percGDP, debt_total_corp),
           list(mean = ~ mean(.x, na.rm = TRUE),
                sd = ~ sd(.x, na.rm = TRUE)),
           .names = "{.col}_{.fn}"),
    n_countries = n(),
    .groups = 'drop'
  ) %>%
  # Add cluster labels
  mutate(cluster_label = case_when(
    cluster == 1 ~ "Core",
    cluster == 2 ~ "Workbench",
    cluster == 3 ~ "Financial Hubs",
    cluster == 4 ~ "Periphery",
    cluster == 5 ~ "Luxembourg",
    TRUE ~ as.character(cluster)
  ))

# Step 4: Normalize the data first (before reshaping)
# Calculate overall mean and sd for each variable for z-score normalization
normalization_stats <- clustered_data %>%
  summarise(
    across(c(current_account, unemployment, adj_wage_share, public_debt_to_gdp,
             gdp_real_pc_ppp, trade_exp_GDP, bond_yield, va_finance,
             abs_fdi_percGDP, debt_total_corp),
           list(overall_mean = ~ mean(.x, na.rm = TRUE),
                overall_sd = ~ sd(.x, na.rm = TRUE)),
           .names = "{.col}_{.fn}")
  )

# Normalize cluster means and standard deviations
cluster_stats_normalized <- cluster_stats %>%
  # Get the mean columns
  select(cluster, cluster_label, ends_with("_mean")) %>%
  # Normalize each mean using z-score: (x - overall_mean) / overall_sd
  mutate(
    current_account_mean_norm = (current_account_mean - normalization_stats$current_account_overall_mean) / normalization_stats$current_account_overall_sd,
    unemployment_mean_norm = (unemployment_mean - normalization_stats$unemployment_overall_mean) / normalization_stats$unemployment_overall_sd,
    adj_wage_share_mean_norm = (adj_wage_share_mean - normalization_stats$adj_wage_share_overall_mean) / normalization_stats$adj_wage_share_overall_sd,
    public_debt_to_gdp_mean_norm = (public_debt_to_gdp_mean - normalization_stats$public_debt_to_gdp_overall_mean) / normalization_stats$public_debt_to_gdp_overall_sd,
    gdp_real_pc_ppp_mean_norm = (gdp_real_pc_ppp_mean - normalization_stats$gdp_real_pc_ppp_overall_mean) / normalization_stats$gdp_real_pc_ppp_overall_sd,
    trade_exp_GDP_mean_norm = (trade_exp_GDP_mean - normalization_stats$trade_exp_GDP_overall_mean) / normalization_stats$trade_exp_GDP_overall_sd,
    bond_yield_mean_norm = (bond_yield_mean - normalization_stats$bond_yield_overall_mean) / normalization_stats$bond_yield_overall_sd,
    va_finance_mean_norm = (va_finance_mean - normalization_stats$va_finance_overall_mean) / normalization_stats$va_finance_overall_sd,
    abs_fdi_percGDP_mean_norm = (abs_fdi_percGDP_mean - normalization_stats$abs_fdi_percGDP_overall_mean) / normalization_stats$abs_fdi_percGDP_overall_sd,
    debt_total_corp_mean_norm = (debt_total_corp_mean - normalization_stats$debt_total_corp_overall_mean) / normalization_stats$debt_total_corp_overall_sd
  ) %>%
  # Also normalize the standard deviations (divide by overall sd)
  left_join(
    cluster_stats %>% select(cluster, ends_with("_sd")),
    by = "cluster"
  ) %>%
  mutate(
    current_account_sd_norm = current_account_sd / normalization_stats$current_account_overall_sd,
    unemployment_sd_norm = unemployment_sd / normalization_stats$unemployment_overall_sd,
    adj_wage_share_sd_norm = adj_wage_share_sd / normalization_stats$adj_wage_share_overall_sd,
    public_debt_to_gdp_sd_norm = public_debt_to_gdp_sd / normalization_stats$public_debt_to_gdp_overall_sd,
    gdp_real_pc_ppp_sd_norm = gdp_real_pc_ppp_sd / normalization_stats$gdp_real_pc_ppp_overall_sd,
    trade_exp_GDP_sd_norm = trade_exp_GDP_sd / normalization_stats$trade_exp_GDP_overall_sd,
    bond_yield_sd_norm = bond_yield_sd / normalization_stats$bond_yield_overall_sd,
    va_finance_sd_norm = va_finance_sd / normalization_stats$va_finance_overall_sd,
    abs_fdi_percGDP_sd_norm = abs_fdi_percGDP_sd / normalization_stats$abs_fdi_percGDP_overall_sd,
    debt_total_corp_sd_norm = debt_total_corp_sd / normalization_stats$debt_total_corp_overall_sd
  )

# Step 5: Reshape normalized data for plotting
plot_data <- cluster_stats_normalized %>%
  select(cluster, cluster_label, ends_with("_mean_norm"), ends_with("_sd_norm")) %>%
  pivot_longer(
    cols = -c(cluster, cluster_label),
    names_to = "variable_stat",
    values_to = "value"
  ) %>%
  # Extract variable name and statistic
  mutate(
    statistic = case_when(
      str_ends(variable_stat, "_mean_norm") ~ "mean",
      str_ends(variable_stat, "_sd_norm") ~ "sd"
    ),
    variable = str_remove(variable_stat, "_(mean_norm|sd_norm)$")
  ) %>%
  select(-variable_stat) %>%
  pivot_wider(names_from = statistic, values_from = value)

# Step 5: Create better variable names for the plot
variable_labels <- c(
  "current_account" = "Current Account Balance",
  "unemployment" = "Unemployment Rate",
  "adj_wage_share" = "Adjusted Wage Share",
  "public_debt_to_gdp" = "Public Debt to GDP",
  "gdp_real_pc_ppp" = "GDP per Capita (PPP)",
  "trade_exp_GDP" = "Trade Exports to GDP",
  "bond_yield" = "Government Bond Yields",
  "va_finance" = "Financial Sector VA Share",
  "abs_fdi_percGDP" = "FDI Flows to GDP",
  "debt_total_corp" = "Total Corporate Debt"
)

plot_data <- plot_data %>%
  mutate(
    variable_clean = recode(variable, !!!variable_labels),
    cluster_label = factor(cluster_label, levels = c("Core",
                                                     "Workbench",
                                                     "Financial Hubs",
                                                     "Periphery",
                                                     "Luxembourg"))
  )

# Step 6: Create the lollipop chart with dodged positions
lollipop_plot <- plot_data %>%
  ggplot(aes(x = mean, y = variable_clean, color = cluster_label, group=cluster_label)) +
  # Add error bars for standard deviation
  geom_errorbarh(aes(xmin = mean - sd, xmax = mean + sd, group = cluster_label),
                 height = 0.15, alpha = 0.7, linewidth = 0.8,
                 position = position_dodge(width = 0.6)) +
  # Add lollipop stems
  # geom_segment(aes(x = 0, xend = mean, y = variable_clean, yend = variable_clean),
  #              alpha = 0.8, linewidth = 1,
  #              position = position_dodge(width = 0.6)) +
  # Add lollipop dots
  geom_point(size = 4, alpha = 0.9,
             position = position_dodge(width = 0.6)) +
  # Styling
  scale_color_brewer(type = "qual", palette = "Set2", name = "Cluster") +
  labs(
    title = "Economic Characteristics by Country Cluster (Standardized)",
    subtitle = "Z-score normalized values with standardized deviation bars",
    x = "Standardized Value (Z-score)",
    y = "Economic Variables",
    caption = "Values normalized using z-scores: (value - overall mean) / overall SD\nError bars represent standardized within-cluster variation"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12, color = "grey60"),
    axis.title = element_text(size = 11),
    axis.text = element_text(size = 10),
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 10),
    panel.grid.major.y = element_line(color = "grey90", linetype = "dashed"),
    panel.grid.major.x = element_line(color = "grey95"),
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  ) +
  guides(color = guide_legend(override.aes = list(size = 3)))

# Display the plot
print(lollipop_plot)



# Step 9: Save the plots
# Uncomment to save
ggsave(here("output/fig/lollipop_clusters.pdf"), lollipop_plot, width = 12, height = 8, dpi = 300)

# Step 10: Create summary statistics table (optional)
summary_table <- cluster_stats %>%
  select(cluster_label, ends_with("_mean")) %>%
  rename_with(~ str_remove(.x, "_mean"), ends_with("_mean")) %>%
  mutate(across(where(is.numeric) , ~ round(.x, 2))) |>
  pivot_longer(cols = -cluster_label) |>
  pivot_wider(names_from = cluster_label, values_from = value) |>
  mutate(
    variable_clean = recode(name, !!!variable_labels),
  ) |>
  select(variable_clean, everything(), -name) |>
  arrange(variable_clean) |>
  rename(Variable=variable_clean)
summary_table

print(summary_table)

summary_table %>%
  knitr::kable(format = "pipe", digits = 2, align = "lccccr")

summary_table %>%
  mutate(across(where(is.numeric), ~ round(.x, 2))) %>%
  write.table(file = pipe("pbcopy"), sep = "\t", row.names = FALSE, quote = FALSE)

