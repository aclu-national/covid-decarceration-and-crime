fs::dir_walk(here::here("code"), source)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

### VISUALIZATIONS ###

# We create the two graphics used in the blog below. Graphics are exported and tidied up in Adobe Illustrator. 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Sourcing in all the cleaned up categorized data & decarceration data (this will also run the original clean script)
source(here::here("scripts/04_analysis.R"))

#### 1. Small Multiples Lines ---------
## - Shows the trend in monthly crime over time (since 2018 for most locations), by locality. 
## - Highlights the COVID-19 period of Mar - May 2020.
## - Ordered by percent change in jail population. 

# Setting pretty breaks for x axis 
x <- seq.Date(as.Date("2018-01-01"), as.Date("2020-04-01"), by = "month")
breaks <- pretty_dates(x, 4)


# Want to add in a descriptive legend that uses another location's data. 
# Adding in sample data to come first, modeling off of Atlanta
sample <- monthly_crimes_with_pre %>% 
  filter(location == "Atlanta, GA") %>% 
  mutate(location = "Location",
         location_annotate = "Location")

sample_jail_info <- percent_change_crime %>% 
  ungroup() %>% 
  filter(location == "Atlanta, GA") %>% 
  mutate(percent_change = .5, 
         location = "Location",
         location_annotate = "Location")

# Creating the order we want the small multiples to appear in, based on percent change in jail pop. 
order <- percent_change_crime %>%
  ungroup() %>% 
  bind_rows(sample_jail_info) %>% 
  arrange(-percent_change) %>% 
  pull(location_annotate)


monthly_crimes_with_pre  %<>% 
  # Adding in sample data 
  bind_rows(sample) %>% 
  # Removing any outlier months
  filter(month < as.Date("2020-06-01") & month >= as.Date("2018-01-01")) %>% 
  # Highlighting the months of Mar - May 2020
  mutate(covid = if_else(month > as.Date("2020-02-01"), TRUE, FALSE),
         location_annotate = factor(location_annotate, levels = order))

  # Plotting
  monthly_crimes_with_pre %>%   
  ggplot(aes(x = month, y = n)) +
  # Add a grey rectangle for range of monthly crime between 2018 and Feb 2020
  geom_rect(aes(ymin = low_range_pre, ymax = high_range_pre, xmin = as.Date("2018-01-01"), xmax = as.Date("2020-05-01")), fill = "#efecea", color = "#efecea") +
  # Line of monthly crime over time
  geom_line(color = "black") +
  # Orange line highlighting COVID period
  geom_line(size = 1.2, data = monthly_crimes_with_pre %>% filter(month >= as.Date("2020-03-01") & month < as.Date("2020-06-01")), color = "#faac17") +
  # White straight line for average monthly crimes during the "pre" period of Jan. 2018 through Feb. 2020
  geom_line(aes(x = month, y = avg_monthly_crimes_pre), color = "white", size = 0.5) +
  # Forcing the charts to all start at zero 
  geom_point(aes(x = as.Date("2018-02-01"), y = 0), col = "white") +
  facet_wrap(~location_annotate, scales = "free_y", nrow = 5) +
  scale_color_manual(values = c("#3c3532", "#fcaa17"), guide = FALSE) +
  scale_y_continuous(labels = scales::comma, expand = c(0,0)) +
  scale_x_date(breaks = as.Date(breaks), date_labels  = "%b-%y") +
  labs( x = "", y = "") +
  theme(strip.text.x = element_text(hjust = -0.01, face = "bold")) 

# Save plot in graphics/ folder

#ggsave(plot = last_plot(), filename = here::here("graphics/crime_lines.pdf"), width = 12, height = 6.75)

#### 2. Arrows ---------

## - Shows the percent change in average crime between Mar and May 2019 and 2020, ordered
# by how much the location increased or decreased their jail population. 

percent_change_crime %>% 
  ggplot() +
  geom_segment(aes(x = reorder(location_annotate, percent_change), 
                   xend = reorder(location_annotate, percent_change),
                   y = 0, yend = perc_change_avg_crime), color = "#0055aa", 
               size = 0.9,
               arrow = arrow(length = unit(0.25, "cm"))) +
  scale_y_continuous(limits = c(-0.3, 0.3), labels = scales::percent) +
  coord_flip() +
  labs(x = "", y = "") +
  theme(axis.text.y = element_text(size = 10, face = "bold"))

#ggsave(plot = last_plot(), filename = here::here("graphics/arrows.pdf"), useDingbats = FALSE, width = 12, height = 6.75)

