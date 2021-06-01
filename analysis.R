# Analysis

# Set up - make sure to set your working directory using RStudio
library(tidyr)
library(dplyr)
library(ggplot2)

# Create the `charts/` directory (you can do this from R!)
dir.create("charts", showWarnings = FALSE)
# Load prepped data

prepped_data <- read.csv("./data/prepped/all_data.csv")
View(prepped_data)

# Are HALE and life expectancy correlated?
# - Plot 2016 life expectancy against 2016 HALE. Save the graph to `charts/`
# - Compute the correlation between 2016 life expectancy against 2016 HALE
plot_16<- prepped_data %>%
  filter(year == 2016)

ggplot(plot_16) +
  geom_point(mapping = aes(x = le, y = hale)) +
  labs(
    title = "Life Expectancy vs HALE",
    x = "Life Expectancy",
    y = "HALE"
  )

ggsave("charts/le_hale.png")

cor(plot_16$le, plot_16$hale)

# Are HALE and DALYs correlated?
# - Plot 2016 HALE against 2016 DALYs. Save the graph to `charts/`
# - Compute the correlation between 2016 HALE and DALYs

ggplot(plot_16) +
  geom_point(mapping = aes(x = hale, y = dalys)) +
  labs(
    title = "HALE vs DALYs",
    x = "HALE",
    y = "DALYs"
  )

ggsave("charts/hale_dalys.png")

cor(plot_16$hale, plot_16$dalys)


# As people live longer, do they live healthier lives 
# (i.e., is a smaller fraction of life spent in poor health)?
# Follow the steps below to attempt to answer this question.

# First, you will need to reshape the data to create columns *by metric-year*
# This will create `hale_2016`, `hale_1990`, `le_2016`, etc.
# To do this, I suggest that you use the `pivot` function in the new
# tidyverse release:https://tidyr.tidyverse.org/articles/pivot.html#wider

wide <- prepped_data %>%
  pivot_wider(names_from = year,
              values_from = c(hale, le, dalys))

# Create columns to store the change in life expectancy, and change in hale
wide <- wide %>%
  mutate(hale_difference = hale_2016 - hale_1990,
         le_difference = le_2016 - le_1990)

# Plot the *change in hale* against the *change in life expectancy*
# Add a 45 degree line (i.e., where x = y), and save the graph to `charts/`

ggplot(wide) +
  geom_point(mapping = aes(x = le_difference, y = hale_difference)) +
  labs(
    title = "Life Expectancy vs HALE difference",
    x = "Change in Life Expectancy",
    y = "Change in HALE"
  ) +
  geom_abline(intercept = 0, slope = 1) +
  xlim(-15, 20) +
  ylim(-15, 20)

ggsave("charts/changed_plot.png")
# What does this mean?!?! Put your interpretation below

# Life expectancy and HALE values are very similar to each other, but HALE is generally
# lower; this could be contributed to modern medicine implementations or improvements
# in public health policies.