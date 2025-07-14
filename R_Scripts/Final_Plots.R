# Plot 1: Bray-Curtis and Sorenson Relationship

library(ggplot2)

ggplot(betadiv_data, aes(x = bray_distance, y = sorenson)) +
  geom_point(color = "steelblue", alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "darkred") +
  labs(
    title = "Relationship Between Bray-Curtis Distance and Sørensen Similarity",
    x = "Bray-Curtis Distance",
    y = "Sørensen Similarity"
  ) +
  theme_minimal()


# Plot 2: Initial patient comparison at first time point using Bray Curtis 

colnames(table1_data)

# Load libraries
library(ggplot2)
library(dplyr)

# Load data
betadiv_data <- read.csv("CSV: Other/dat_betadiv_ucsp_2025.csv")

# Convert date if not already in date format
betadiv_data$collection_date <- as.Date(betadiv_data$collection_date)

# Get only the first collection date per subject
entry_distances <- betadiv_data %>%
  group_by(subject_id) %>%
  arrange(collection_date) %>%
  slice(1) %>%  # keep only first time point
  ungroup()

# Create dot plot of Bray-Curtis distances at first time point
ggplot(entry_distances, aes(x = reorder(subject_id, bray_distance), y = bray_distance)) +
  geom_point(color = "steelblue", size = 2) +
  coord_flip() +
  labs(
    title = "Bray–Curtis Distance to Healthy Population at Trial Entry",
    x = "Subject ID",
    y = "Bray–Curtis Distance (First Time Point)"
  ) +
  theme_minimal()


# Plot 3: Beta Diversity trajectories over time

library(ggplot2)
library(dplyr)

# Load the data (adjust path if needed)
betadiv_data <- read.csv("CSV: Other/dat_betadiv_ucsp_2025.csv")

# Convert date column
betadiv_data$collection_date <- as.Date(betadiv_data$collection_date)

# Plot trajectories, one panel per subject
ggplot(betadiv_data, aes(x = collection_date, y = bray_distance)) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_point(color = "black", size = 1.5) +
  facet_wrap(~ subject_id, scales = "free_x") +
  labs(
    title = "Subject-Specific Beta-Diversity Trajectories Over Time",
    x = "Collection Date",
    y = "Bray–Curtis Distance to Healthy"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


# Plot 4: Subject Trajectories FMT vs. No FMT

library(dplyr)
library(ggplot2)
library(scales)

# 1) Read in your data (using your working paths)
betadiv_data <- read.csv("CSV: Other/dat_betadiv_ucsp_2025.csv", stringsAsFactors = FALSE)
table1_data  <- read.csv("CSV: Other/table_1_data.csv",           stringsAsFactors = FALSE)

# 2) Create matching IDs
table1_data <- table1_data %>%
  mutate(subject_id = sprintf("PMTS_%04d", as.integer(study_id)))

# 3) Merge on subject_id → bring in fmt_yn (“FMT” / “No FMT”)
merged <- betadiv_data %>%
  left_join(table1_data %>% select(subject_id, fmt_yn), by = "subject_id")

# 4) Convert date and make sure fmt_yn is a factor in the right order
merged <- merged %>%
  mutate(
    collection_date = as.Date(collection_date),
    fmt_yn = factor(fmt_yn, levels = c("No FMT", "FMT"))
  )

# 5) Compute group‐means by date & FMT status
group_means <- merged %>%
  group_by(fmt_yn, collection_date) %>%
  summarise(mean_bray = mean(bray_distance, na.rm = TRUE), .groups = "drop")

# 6) Plot: facet by FMT vs. No FMT
ggplot(merged, aes(x = collection_date, y = bray_distance)) +
  # faint individual trajectories
  geom_line(aes(group = subject_id), color = "grey80", alpha = 0.4) +
  # bold group‐mean trajectories
  geom_line(data = group_means, aes(y = mean_bray), color = "steelblue", size = 1) +
  facet_wrap(~ fmt_yn, ncol = 1) +
  scale_x_date(
    date_breaks = "3 months",
    date_labels = "%b %Y",
    expand = expansion(add = c(0, 5))
  ) +
  labs(
    title    = "Beta‑Diversity Trajectories by FMT Status",
    subtitle = "Bold = group mean",
    x        = "Collection Date",
    y        = "Bray–Curtis Distance to Healthy"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    strip.text       = element_text(face = "bold"),
    axis.text.x      = element_text(angle = 45, hjust = 1),
    plot.subtitle    = element_text(size = 10, color = "grey30"),
    panel.spacing    = unit(1, "lines")
  )

# Plot 5: Subject Trajectories Upper vs. Lower

library(dplyr)
library(ggplot2)

# 1) Read in your data
betadiv_data <- read.csv("CSV: Other/dat_betadiv_ucsp_2025.csv",
                         stringsAsFactors = FALSE)
table1_data  <- read.csv("CSV: Other/table_1_data.csv",
                         stringsAsFactors = FALSE)

# 2) Build the same subject_id key so we can join
#    (betadiv_data has IDs like "PMTS_0004")
table1_data <- table1_data %>%
  mutate(subject_id = sprintf("PMTS_%04d", study_id))

# 3) Pull in only the FMT recipients and their route (gi_entry)
fmt_meta <- table1_data %>%
  filter(fmt_yn == "FMT") %>%
  select(subject_id, route = gi_entry) %>%
  # make it a factor so the panels come in the right order
  mutate(route = factor(route, levels = c("Lower GI", "Upper GI")))

# 4) Join to the beta‐diversity table and keep only those FMT rows
fmt_trajs <- betadiv_data %>%
  mutate(collection_date = as.Date(collection_date)) %>%
  inner_join(fmt_meta, by = "subject_id")

# 5) Compute the mean Bray–Curtis per route and date
route_means <- fmt_trajs %>%
  group_by(route, collection_date) %>%
  summarize(mean_bray = mean(bray_distance, na.rm = TRUE), .groups = "drop")

# 6) Plot: one facet for each route
ggplot() +
  # a) individual subject lines (grey, semi‑transparent)
  geom_line(
    data  = fmt_trajs,
    aes(x = collection_date, y = bray_distance, group = subject_id),
    color = "grey60", size = 0.6, alpha = 0.5
  ) +
  # b) bold mean line for each route (colored by route)
  geom_line(
    data  = route_means,
    aes(x = collection_date, y = mean_bray, color = route),
    size = 1.2
  ) +
  facet_wrap(~ route, ncol = 1) +
  scale_color_manual(
    values = c("Lower GI" = "forestgreen", "Upper GI" = "darkorange")
  ) +
  scale_x_date(
    date_breaks = "4 months",
    date_labels = "%b\n%Y",
    expand      = expansion(add = c(0, 5))
  ) +
  labs(
    title    = "Beta‑Diversity Trajectories by FMT Route",
    subtitle = "colored = route mean",
    x        = "Collection Date",
    y        = "Bray–Curtis Distance to Healthy",
    color    = "FMT Route"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    strip.text    = element_text(face = "bold"),
    axis.text.x   = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    panel.spacing = unit(1, "lines")
  )
