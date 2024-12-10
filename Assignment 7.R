library(dplyr)
library(ggplot2)
library(forcats)
library(cowplot)

raw = read.csv("C:/Users/f9tbqno/Downloads/ImportReliance.csv")
raw$Imports = raw$Import.Reliance...Export.Share
relationships = read.csv("C:/Users/f9tbqno/Downloads/countries.csv")

relationships <- relationships %>%
  mutate(Country = fct_reorder(Country, Country, .fun = identity))

plot1_df = relationships
plot1_df <- plot1_df %>% filter(Country != "USA")
#----Bilateral Relationships With Countries----

ggplot(plot1_df, aes(x = Country, y = Frequency, fill = Relationship)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "USA Bilateral Relations for Critical Mineral Import",
       x = "Country",
       y = "Frequency") +
  scale_fill_manual(values = c("Ally" = "#a7c957", "Neutral" = "#669bbc", "Competitor" = "#bc4749")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#----Bar Chart Import Reliance----
get_predominant_relationship <- function(major_sources, relationships_df) {
  countries <- strsplit(major_sources, ",\\s*")[[1]]
  rels <- relationships_df %>% filter(Country %in% countries) %>% pull(Relationship)
  weights <- rev(seq_along(countries)) # Generate weights in reverse order
  
  relationship_weights <- sapply(rels, function(rel, weights) {
    sum(weights[rels == rel])
  }, weights)
  
  rel_counts <- tapply(relationship_weights, rels, sum, simplify = TRUE)
  
  if (length(rel_counts) == 0) {
    return("Unknown") # Default value when no matches
  }
  
  predominant_rel <- names(which.max(rel_counts))
  return(predominant_rel)
}

# Apply the function to determine the predominant relationship for each mineral
raw <- raw %>%
  rowwise() %>%
  mutate(Predominant_Relationship = get_predominant_relationship(Major_Import_Sources_2019_2022, relationships))

# Define a color palette for relationships
relationship_colors <- c("Ally" = "#a7c957", "Neutral" = "#669bbc", "Competitor" = "#bc4749", "Self" = "#cebebe")

# Sort data by Imports for plotting
sorted_raw <- raw %>%
  arrange(Imports)

# Convert Commodity to a factor with levels ordered by Imports
sorted_raw$Commodity <- factor(sorted_raw$Commodity, levels = sorted_raw$Commodity)

# Plot
main_plot <- ggplot(sorted_raw, aes(x = Imports, y = Commodity, fill = Predominant_Relationship)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "USA Import Reliance and Export Share for Critical Minerals",
       x = "Percentage",
       y = "Commodity") +
  theme_minimal() +
  scale_fill_manual(values = relationship_colors) +
  theme(legend.position = "bottom")

right_plot <- ggplot(sorted_raw, aes(y = Commodity, x = 1, label = Major_Import_Sources_2019_2022)) +
  geom_text(hjust = 0) +
  theme_void() +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.margin = margin(t = 20, r = 20, b = 20, l = -30)) +
  labs(x = NULL, y = NULL)

combined_plot <- plot_grid(main_plot, right_plot, nrow = 1, rel_widths = c(1, 1))
print(combined_plot)
print(main_plot)
#----World Map----
library(dplyr)
library(ggplot2)
library(maps)
library(sf)

world <- st_as_sf(maps::map("world", plot = FALSE, fill = TRUE))

# Join the relationships data with the world map data
world_relationships <- world %>%
  left_join(relationships, by = c("ID" = "Country"))

# Get country centroids for plotting points accurately
world_countries_df <- map_data("world") %>%
  group_by(region) %>%
  summarise(long = mean(long), lat = mean(lat))

relationships_df <- plot1_df %>%
  rename(region = Country)

country_centroids <- world_countries_df %>%
  inner_join(relationships_df, by = "region")

# Plotting the world map with the relationships
map_plot <- ggplot(data = world_relationships) +
  geom_sf(aes(fill = Relationship), color = "white") +
  scale_fill_manual(values = relationship_colors) +
  theme_minimal() +
  labs(title = "Relationship of Major Import Sources for Critical Minerals",
       fill = "Relationship") +
  theme(panel.grid.major = element_blank(), # Remove major grid lines
        panel.grid.minor = element_blank())  # Remove minor grid lines)

# Add points for frequency
frequency_plot <- map_plot +
  geom_point(data = country_centroids, 
             aes(x = long, y = lat, size = Frequency),
             color = "#495057", alpha = 0.6) +
  scale_size(range = c(3, 10), name = "Trade Frequency")

print(frequency_plot)
