# ====================================================================
# Latin America and Caribbean Trade Diversification Analysis
# Author: Julia Gomez-Coronado
# Date: October 2026
# Source: IMF International Trade in Goods by Partner Country (IMTS)
# Data: Annual merchandise exports, 2001–2025
# ====================================================================

# 1. SETUP ------
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(ggtext)


# 2. LOAD RAW IMF DATA ------
# Load annual IMF trade data
lac_exports_annual_raw <- read_csv(
  "data_raw/imf_imts_lac_exports_annual_2001_2025_raw.csv",
  show_col_types = FALSE) |>
  mutate(
    TIME_PERIOD = as.integer(TIME_PERIOD),
    OBS_VALUE = as.numeric(OBS_VALUE))


# 3. DATA QUALITY CHECK ------
# Identify country-years with missing China observations
china_missing <- expand_grid(
  COUNTRY = unique(lac_exports_annual_raw$COUNTRY),
  TIME_PERIOD = 2001:2025) |>
  anti_join(
    lac_exports_annual_raw |>
      filter(COUNTERPART_COUNTRY == "CHN") |>
      distinct(COUNTRY, TIME_PERIOD),
    by = c("COUNTRY", "TIME_PERIOD"))

# Add world exports for missing country-years
world_exports <- lac_exports_annual_raw |>
  filter(COUNTERPART_COUNTRY == "G001") |>
  select(COUNTRY, TIME_PERIOD, world_exports = OBS_VALUE)

missing_exports <- china_missing |>
  left_join(world_exports, by = c("COUNTRY", "TIME_PERIOD"))

# Calculate maximum share of exports affected by missing China data
missing_check <- world_exports |>
  group_by(TIME_PERIOD) |>
  summarise(
    total_exports = sum(world_exports, na.rm = TRUE),
    total_ex_mex = sum(world_exports[COUNTRY != "MEX"], na.rm = TRUE),
    .groups = "drop") |>
  left_join(
    missing_exports |>
      group_by(TIME_PERIOD) |>
      summarise(
        missing_exports = sum(world_exports, na.rm = TRUE),
        missing_ex_mex = sum(world_exports[COUNTRY != "MEX"], na.rm = TRUE),
        .groups = "drop"),
    by = "TIME_PERIOD") |>
  mutate(
    missing_share = 100 * missing_exports / total_exports,
    missing_share_ex_mex = 100 * missing_ex_mex / total_ex_mex)

max_missing_all <- max(missing_check$missing_share, na.rm = TRUE)
max_missing_ex_mex <- max(missing_check$missing_share_ex_mex, na.rm = TRUE)


# 4. FIGURE 1 DATA ANALYSIS ------
# Prepare U.S., China, and World export data
figure1_base <- lac_exports_annual_raw |>
  filter(COUNTERPART_COUNTRY %in% c("USA", "CHN", "G001")) |>
  select(COUNTRY, TIME_PERIOD, COUNTERPART_COUNTRY, OBS_VALUE) |>
  pivot_wider(names_from = COUNTERPART_COUNTRY, values_from = OBS_VALUE)

# Calculate export shares for all LAC
figure1_all <- figure1_base |>
  group_by(TIME_PERIOD) |>
  summarise(
    total_exports = sum(G001, na.rm = TRUE),
    exports_us = sum(USA, na.rm = TRUE),
    exports_china = sum(CHN, na.rm = TRUE),
    .groups = "drop") |>
  mutate(
    year = TIME_PERIOD,
    us_share = 100 * exports_us / total_exports,
    china_share = 100 * exports_china / total_exports,
    region = "All LAC economies")

# Calculate export shares excluding Mexico
figure1_ex_mex <- figure1_base |>
  filter(COUNTRY != "MEX") |>
  group_by(TIME_PERIOD) |>
  summarise(
    total_exports = sum(G001, na.rm = TRUE),
    exports_us = sum(USA, na.rm = TRUE),
    exports_china = sum(CHN, na.rm = TRUE),
    .groups = "drop") |>
  mutate(
    year = TIME_PERIOD,
    us_share = 100 * exports_us / total_exports,
    china_share = 100 * exports_china / total_exports,
    region = "LAC excluding Mexico")

# Combine and reshape for plotting
figure1_plot_data <- bind_rows(figure1_all, figure1_ex_mex) |>
  select(year, region, us_share, china_share) |>
  pivot_longer(
    cols = c(us_share, china_share),
    names_to = "partner",
    values_to = "share") |>
  mutate(
    partner = recode(
      partner,
      us_share = "United States",
      china_share = "China"),
    partner = factor(partner, levels = c("China", "United States")),
    region = factor(
      region,
      levels = c("All LAC economies", "LAC excluding Mexico")))


# 5. CALCULATE U.S.–CHINA CROSSOVER POINT ------
# Find when China's share overtakes the U.S.
crossover_data <- figure1_ex_mex |>
  arrange(year) |>
  mutate(difference = china_share - us_share)

first_positive <- which(crossover_data$difference >= 0)[1]

year_before <- crossover_data$year[first_positive - 1]
year_after <- crossover_data$year[first_positive]
diff_before <- crossover_data$difference[first_positive - 1]
diff_after <- crossover_data$difference[first_positive]

# Estimate crossover between the two years
crossover_year <- year_before +
  (0 - diff_before) / (diff_after - diff_before) * (year_after - year_before)

# Reference line for Figure 1
crossover_line <- data.frame(
  region = factor(
    "LAC excluding Mexico",
    levels = c("All LAC economies", "LAC excluding Mexico")),
  crossover_year = crossover_year)


# 6. FIGURE 1 ------
figure1 <- ggplot(
  figure1_plot_data,
  aes(x = year, y = share, color = partner)
) +
  geom_vline(
    data = crossover_line,
    aes(xintercept = crossover_year),
    inherit.aes = FALSE,
    color = "#D98C3F",
    linewidth = 0.4
  ) +
  geom_line(linewidth = 1.1) +
  facet_wrap(~ region, nrow = 1) +
  scale_x_continuous(
    breaks = c(2001, 2005, 2010, 2015, 2020, 2025)
  ) +
  scale_y_continuous(
    limits = c(0, 60),
    breaks = seq(0, 60, 10),
    labels = function(x) paste0(x, "%")
  ) +
  scale_color_manual(
    values = c(
      "China" = "#B23A3A",
      "United States" = "#1F4E79")
  ) +
  labs(
    title = "Figure 1: U.S. and China Shares of LAC Goods Exports, 2001–2025",
    x = NULL,
    y = "Share of goods exports",
    color = NULL,
    caption = paste0(
      "**Author’s data analysis and calculations** based on IMF International Trade in Goods by Partner Country (IMTS) data.<br>",
      "**Note:** Missing China observations affect at most ",
      sprintf("%.1f", max_missing_all),
      "% of LAC goods exports (",
      sprintf("%.1f", max_missing_ex_mex),
      "% excluding Mexico) in any year.")
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "top",
    legend.text = element_text(size = 11, color = "black"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey88", linewidth = 0.35),
    panel.spacing = grid::unit(2, "lines"),
    strip.text = element_text(
      size = 13, face = "bold", color = "black",
      margin = margin(b = 6)),
    plot.title = element_text(
      size = 14, face = "bold", color = "black",
      hjust = 0.5, margin = margin(b = 12)),
    axis.title.y = element_text(
      size = 12, face = "bold", color = "black",
      margin = margin(r = 20)),
    axis.text.x = element_text(
      size = 10.5, color = "black",
      margin = margin(t = 8)),
    axis.text.y = element_text(
      size = 10.5, color = "black",
      margin = margin(r = 6)),
    plot.caption = element_markdown(
      size = 10, color = "black",
      hjust = 0, lineheight = 1.05,
      margin = margin(t = 20)),
    plot.caption.position = "plot",
    plot.margin = margin(t = 12, r = 14, b = 22, l = 12))

figure1


# 7. SAVE FIGURE 1 ------
dir.create("figures", showWarnings = FALSE)

ggsave(
  "figures/Figure_1_LAC_US_China_Exports.png",
  plot = figure1,
  width = 8,
  height = 5,
  dpi = 600,
  bg = "white")


# 8. PREPARE FIGURE 2 DATA ------
# Calculate five-year average China export shares
figure2_plot_data <- lac_exports_annual_raw |>
  filter(
    COUNTERPART_COUNTRY %in% c("CHN", "G001"),
    COUNTRY %in% c(
      "CHL", "PER", "BRA", "URY", "COL",
      "MEX", "PAN", "GTM", "JAM")
  ) |>
  select(COUNTRY, TIME_PERIOD, COUNTERPART_COUNTRY, OBS_VALUE) |>
  pivot_wider(
    names_from = COUNTERPART_COUNTRY,
    values_from = OBS_VALUE
  ) |>
  mutate(
    china_share = 100 * CHN / G001,
    period = case_when(
      TIME_PERIOD %in% 2001:2005 ~ "2001–2005",
      TIME_PERIOD %in% 2021:2025 ~ "2021–2025")
  ) |>
  filter(!is.na(period)) |>
  group_by(COUNTRY, period) |>
  summarise(
    years_available = sum(!is.na(china_share)),
    china_share = mean(china_share, na.rm = TRUE),
    .groups = "drop"
  ) |>
  group_by(COUNTRY) |>
  filter(all(years_available == 5)) |>
  ungroup() |>
  mutate(
    country_name = recode(
      COUNTRY,
      CHL = "Chile",
      PER = "Peru",
      BRA = "Brazil",
      URY = "Uruguay",
      COL = "Colombia",
      MEX = "Mexico",
      PAN = "Panama",
      GTM = "Guatemala",
      JAM = "Jamaica"),
    period = factor(period, levels = c("2001–2005", "2021–2025")))

# Order countries by their 2021–2025 share
country_order <- figure2_plot_data |>
  filter(period == "2021–2025") |>
  arrange(desc(china_share)) |>
  pull(country_name)

figure2_plot_data <- figure2_plot_data |>
  mutate(
    country_name = factor(country_name, levels = country_order))


# 9. FIGURE 2 ------
pd <- position_dodge2(width = 0.8, reverse = TRUE)

figure2 <- ggplot(
  figure2_plot_data,
  aes(x = country_name, y = china_share, fill = period)
) +
  geom_col(position = pd, width = 0.7) +
  geom_text(
    aes(label = paste0(round(china_share, 1), "%")),
    position = pd,
    hjust = -0.15,
    size = 3.2,
    fontface = "bold"
  ) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.4) +
  coord_flip() +
  scale_fill_manual(
    values = c(
      "2001–2005" = "#8FAF7A",
      "2021–2025" = "#3F6B4B"),
    name = NULL
  ) +
  scale_y_continuous(
    breaks = seq(0, 40, 5),
    labels = function(x) paste0(x, "%"),
    expand = expansion(mult = c(0, 0.12))
  ) +
  labs(
    title = "Figure 2: China’s Share of Goods Exports in Selected LAC Economies",
    subtitle = "Average export shares, 2001–2005 and 2021–2025",
    x = NULL,
    y = "Share of goods exported to China",
    caption = paste0(
      "**Author’s data analysis and calculations** based on IMF International Trade in Goods by Partner Country (IMTS) data.<br>",
      "**Note:** Nine countries were selected to illustrate contrasting trade patterns across South America, Mexico, Central America,<br>",
      "and the Caribbean. Bars show average annual export shares within each five-year period.")
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, margin = margin(b = 16)),
    axis.title.x = element_text(face = "bold", size = 11.5, margin = margin(t = 8)),
    axis.text.x = element_text(size = 10, color = "black"),
    axis.text.y = element_text(size = 10, color = "black"),
    axis.line.x = element_line(color = "black", linewidth = 0.2),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = "grey88", linewidth = 0.35),
    legend.position = "bottom",
    legend.text = element_text(size = 10, margin = margin(l = 4, r = 8)),
    legend.spacing.x = unit(12, "pt"),
    plot.caption.position = "plot",
    plot.caption = ggtext::element_markdown(
      size = 10,
      hjust = 0,
      lineheight = 1.15,
      margin = margin(t = 10)),
    plot.margin = margin(10, 15, 10, 15))

figure2


# 10. SAVE FIGURE 2 ------
ggsave(
  "figures/Figure_2_China_Export_Shares_Selected_LAC.png",
  plot = figure2,
  width = 8,
  height = 5,
  dpi = 600,
  bg = "white")
