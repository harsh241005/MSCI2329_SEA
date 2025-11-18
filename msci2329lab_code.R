# COVID-19 Global Data Analysis and Visualization
# Load required libraries
library(tidyverse)
library(scales)
library(gridExtra)
library(RColorBrewer)

# Read the data
covid_data <- read.csv("country_wise_latest.csv", stringsAsFactors = FALSE)

# Data preprocessing
covid_data <- covid_data %>%
  mutate(
    Deaths.Per.100.Cases = as.numeric(Deaths...100.Cases),
    Recovered.Per.100.Cases = as.numeric(Recovered...100.Cases),
    Week.Percent.Increase = as.numeric(X1.week...increase)
  )

# Create PDF for all visualizations
pdf("COVID19_Analysis_Visualizations.pdf", width = 12, height = 8)

# ===== 1. TOP 15 COUNTRIES BY CONFIRMED CASES =====
top_countries <- covid_data %>%
  arrange(desc(Confirmed)) %>%
  head(15)

p1 <- ggplot(top_countries, aes(x = reorder(Country.Region, Confirmed), y = Confirmed)) +
  geom_bar(stat = "identity",fill = "#E74C3C") +
  coord_flip() +
  scale_y_continuous(labels = comma) +
  labs(title = "Top 15 Countries by Confirmed COVID-19 Cases",
       x = "Country",
       y = "Confirmed Cases") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))
print(p1)

# ===== 2. DEATH RATE ANALYSIS =====
top_death_rate <- covid_data %>%
  filter(Confirmed > 1000) %>%
  arrange(desc(Deaths.Per.100.Cases)) %>%
  head(15)

p2 <- ggplot(top_death_rate, aes(x = reorder(Country.Region, Deaths.Per.100.Cases), 
                                 y = Deaths.Per.100.Cases)) +
  geom_bar(stat = "identity", fill = "#8E44AD") +
  coord_flip() +
  labs(title = "Top 15 Countries by Death Rate (Cases > 1000)",
       subtitle = "Deaths per 100 Confirmed Cases",
       x = "Country",
       y = "Death Rate (%)") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
print(p2)

# ===== 3. RECOVERY RATE ANALYSIS =====
top_recovery <- covid_data %>%
  filter(Confirmed > 1000) %>%
  arrange(desc(Recovered.Per.100.Cases)) %>%
  head(15)

p3 <- ggplot(top_recovery, aes(x = reorder(Country.Region, Recovered.Per.100.Cases), 
                               y = Recovered.Per.100.Cases)) +
  geom_bar(stat = "identity", fill = "#27AE60") +
  coord_flip() +
  labs(title = "Top 15 Countries by Recovery Rate (Cases > 1000)",
       subtitle = "Recovered per 100 Confirmed Cases",
       x = "Country",
       y = "Recovery Rate (%)") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
print(p3)



# ===== 5. SCATTER PLOT: CONFIRMED VS DEATHS =====
p5 <- ggplot(covid_data %>% filter(Confirmed > 1000), 
             aes(x = Confirmed, y = Deaths, color = WHO.Region)) +
  geom_point(alpha = 0.6, size = 3) +
  scale_x_log10(labels = comma) +
  scale_y_log10(labels = comma) +
  labs(title = "Relationship between Confirmed Cases and Deaths",
       subtitle = "Log scale, Countries with >1000 cases",
       x = "Confirmed Cases (log scale)",
       y = "Deaths (log scale)",
       color = "WHO Region") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "bottom")
print(p5)


# ===== 7. ACTIVE CASES DISTRIBUTION =====
top_active <- covid_data %>%
  arrange(desc(Active)) %>%
  head(15)

p7 <- ggplot(top_active, aes(x = reorder(Country.Region, Active), y = Active)) +
  geom_bar(stat = "identity", fill = "#F39C12") +
  coord_flip() +
  scale_y_continuous(labels = comma) +
  labs(title = "Top 15 Countries by Active Cases",
       x = "Country",
       y = "Active Cases") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))
print(p7)




# ===== 10. NEW CASES VS TOTAL CASES =====
new_cases_data <- covid_data %>%
  filter(Confirmed > 5000, New.cases > 0) %>%
  mutate(New_Case_Rate = (New.cases / Confirmed) * 100)

p10 <- ggplot(new_cases_data, aes(x = Confirmed, y = New.cases, 
                                  color = WHO.Region, size = New_Case_Rate)) +
  geom_point(alpha = 0.6) +
  scale_x_log10(labels = comma) +
  scale_y_log10(labels = comma) +
  labs(title = "New Cases vs Total Confirmed Cases",
       subtitle = "Size indicates new case rate, Countries with >5000 cases",
       x = "Total Confirmed Cases (log scale)",
       y = "New Cases (log scale)",
       color = "WHO Region",
       size = "New Case Rate (%)") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "bottom")
print(p10)

# ===== 11. PIE CHART: TOP 10 COUNTRIES SHARE OF GLOBAL CASES =====
top10_pie <- covid_data %>%
  arrange(desc(Confirmed)) %>%
  head(10) %>%
  mutate(Percentage = (Confirmed / sum(covid_data$Confirmed)) * 100)

# Add "Others" category
others_pct <- 100 - sum(top10_pie$Percentage)
pie_data <- rbind(
  top10_pie %>% select(Country.Region, Percentage),
  data.frame(Country.Region = "Others", Percentage = others_pct)
)

pie_colors <- c(brewer.pal(10, "Spectral"), "#CCCCCC")

pie(pie_data$Percentage, 
    labels = paste0(pie_data$Country.Region, "\n", 
                    round(pie_data$Percentage, 1), "%"),
    col = pie_colors,
    main = "Top 10 Countries Share of Global COVID-19 Cases",
    cex = 0.8)



# ===== 14. DONUT CHART: DEATH DISTRIBUTION BY TOP 10 COUNTRIES =====
top10_deaths <- covid_data %>%
  arrange(desc(Deaths)) %>%
  head(10) %>%
  mutate(Percentage = (Deaths / sum(covid_data$Deaths)) * 100)

others_deaths <- 100 - sum(top10_deaths$Percentage)
donut_data <- rbind(
  top10_deaths %>% select(Country.Region, Deaths, Percentage),
  data.frame(Country.Region = "Others", 
             Deaths = sum(covid_data$Deaths) - sum(top10_deaths$Deaths),
             Percentage = others_deaths)
)

pie(donut_data$Percentage,
    labels = paste0(donut_data$Country.Region, "\n",
                    round(donut_data$Percentage, 1), "%"),
    col = brewer.pal(11, "RdYlBu"),
    main = "Top 10 Countries Share of Global Deaths",
    cex = 0.8)



# ===== 20. DENSITY PLOT: WEEKLY GROWTH DISTRIBUTION =====
density_data <- covid_data %>%
  filter(Confirmed > 1000, Week.Percent.Increase >= 0, Week.Percent.Increase < 100)

p20 <- ggplot(density_data, aes(x = Week.Percent.Increase, fill = WHO.Region)) +
  geom_density(alpha = 0.5) +
  labs(title = "Distribution of Weekly Growth Rates by Region",
       subtitle = "Countries with >1000 cases and growth <100%",
       x = "Weekly Growth Rate (%)",
       y = "Density",
       fill = "WHO Region") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "bottom")
print(p20)

# Close PDF device
dev.off()

# Print summary statistics
cat("\n=== COVID-19 DATA SUMMARY ===\n\n")
cat("Total Countries/Regions:", nrow(covid_data), "\n")
cat("Total Confirmed Cases:", format(sum(covid_data$Confirmed), big.mark = ","), "\n")
cat("Total Deaths:", format(sum(covid_data$Deaths), big.mark = ","), "\n")
cat("Total Recovered:", format(sum(covid_data$Recovered, na.rm = TRUE), big.mark = ","), "\n")
cat("Global Death Rate:", round(sum(covid_data$Deaths) / sum(covid_data$Confirmed) * 100, 2), "%\n")
cat("Global Recovery Rate:", round(sum(covid_data$Recovered, na.rm = TRUE) / sum(covid_data$Confirmed) * 100, 2), "%\n\n")

cat("PDF file 'COVID19_Analysis_Visualizations.pdf' has been created successfully!\n")

