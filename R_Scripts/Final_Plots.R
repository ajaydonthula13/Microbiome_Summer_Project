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

library(ggplot2)
library(dplyr)

# Load and preprocess
betadiv_data <- read.csv("CSV: Other/dat_betadiv_ucsp_2025.csv") %>%
  mutate(collection_date = as.Date(collection_date))

# Grab each subject’s first sample
entry_distances <- betadiv_data %>%
  group_by(subject_id) %>%
  arrange(collection_date) %>%
  slice(1) %>%
  ungroup() %>%
  # make subject_id a factor in numeric order
  mutate(subject_id = factor(subject_id, levels = sort(unique(subject_id))))

# ——— Bray–Curtis at Entry ———
ggplot(entry_distances, aes(x = subject_id, y = bray_distance)) +
  geom_point(size = 2, color = "steelblue") +
  labs(
    title = "Bray–Curtis Distance to Healthy Population at Trial Entry",
    x     = "Subject ID",
    y     = "Bray–Curtis Distance\n(First Time Point)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
    axis.title  = element_text(face = "bold")
  )

# ——— Sørensen at Entry ———
ggplot(entry_distances, aes(x = subject_id, y = sorenson)) +
  geom_point(size = 2, color = "steelblue") +
  labs(
    title = "Sørensen Similarity to Healthy Population at Trial Entry",
    x     = "Subject ID",
    y     = "Sørensen Similarity\n(First Time Point)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
    axis.title  = element_text(face = "bold")
  )

# Plot 3: Beta Diversity trajectories over time
library(ggplot2)
library(dplyr)

# Load + preprocess
betadiv_data <- read.csv("CSV: Other/dat_betadiv_ucsp_2025.csv") %>%
  mutate(
    collection_date = as.Date(collection_date),
    # factor for coloring
    fmt_route = factor(gi_entry, levels = c("No FMT","Upper GI","Lower GI"))
  )

# ——— Plot 3A: Bray–Curtis Trajectories ———
ggplot(betadiv_data, 
       aes(x = collection_date, 
           y = bray_distance, 
           group = subject_id, 
           color = fmt_route)) +
  geom_line(size = 1) +
  geom_point(size = 1.5) +
  facet_wrap(~ subject_id, scales = "free") +
  scale_color_manual(
    values = c("No FMT" = "#999999", "Upper GI" = "#E69F00", "Lower GI" = "#56B4E9"),
    name = "FMT Route"
  ) +
  labs(
    title = "Subject‑Specific Bray–Curtis Trajectories Over Time",
    x     = "Collection Date",
    y     = "Bray–Curtis Distance to Healthy"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    strip.text      = element_text(face = "bold"),
    axis.text.x     = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    legend.title    = element_text(face = "bold")
  )

# ——— Plot 3B: Sørensen Trajectories ———
ggplot(betadiv_data, 
       aes(x = collection_date, 
           y = sorenson, 
           group = subject_id, 
           color = fmt_route)) +
  geom_line(size = 1) +
  geom_point(size = 1.5) +
  facet_wrap(~ subject_id, scales = "free") +
  scale_color_manual(
    values = c("No FMT" = "#999999", "Upper GI" = "#E69F00", "Lower GI" = "#56B4E9"),
    name = "FMT Route"
  ) +
  labs(
    title = "Subject‑Specific Sørensen Trajectories Over Time",
    x     = "Collection Date",
    y     = "Sørensen Similarity to Healthy"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    strip.text      = element_text(face = "bold"),
    axis.text.x     = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    legend.title    = element_text(face = "bold")
  )


# Plot 4: Subject Trajectories FMT vs. No FMT

library(dplyr)
library(ggplot2)
library(scales)

# 1) Read in your data
betadiv_data <- read.csv("CSV: Other/dat_betadiv_ucsp_2025.csv", stringsAsFactors = FALSE)
table1_data  <- read.csv("CSV: Other/table_1_data.csv",           stringsAsFactors = FALSE)

# 2) Create matching IDs
table1_data <- table1_data %>%
  mutate(subject_id = sprintf("PMTS_%04d", as.integer(study_id)))

# 3) Merge on subject_id → bring in fmt_yn (“FMT” / “No FMT”)
merged <- betadiv_data %>%
  left_join(table1_data %>% select(subject_id, fmt_yn), by = "subject_id")

# 4) Convert date and ensure fmt_yn is a factor in the right order
merged <- merged %>%
  mutate(
    collection_date = as.Date(collection_date),
    fmt_yn = factor(fmt_yn, levels = c("No FMT", "FMT"))
  )

# 5) Compute group‐means by date & FMT status for both metrics
group_means <- merged %>%
  group_by(fmt_yn, collection_date) %>%
  summarise(
    mean_bray    = mean(bray_distance, na.rm = TRUE),
    mean_sorenson = mean(sorenson,     na.rm = TRUE),
    .groups = "drop"
  )

# ——— Plot 4A: Bray–Curtis Trajectories ———
ggplot(merged, aes(x = collection_date, y = bray_distance)) +
  geom_line(aes(group = subject_id), color = "grey80", alpha = 0.4) +
  geom_line(data = group_means, aes(y = mean_bray), color = "steelblue", size = 1) +
  facet_wrap(~ fmt_yn, ncol = 1) +
  scale_x_date(
    date_breaks = "3 months",
    date_labels = "%b %Y",
    expand = expansion(add = c(0, 5))
  ) +
  labs(
    title    = "Beta‑Diversity Trajectories by FMT Status",
    subtitle = "Metric: Bray–Curtis (bold = group mean)",
    x        = "Collection Date",
    y        = "Bray–Curtis Distance to Healthy"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    strip.text    = element_text(face = "bold"),
    axis.text.x   = element_text(angle = 45, hjust = 1),
    plot.subtitle = element_text(size = 10, color = "grey30"),
    panel.spacing = unit(1, "lines")
  )

# ——— Plot 4B: Sørensen Trajectories ———
ggplot(merged, aes(x = collection_date, y = sorenson)) +
  geom_line(aes(group = subject_id), color = "grey80", alpha = 0.4) +
  geom_line(data = group_means, aes(y = mean_sorenson), color = "steelblue", size = 1) +
  facet_wrap(~ fmt_yn, ncol = 1) +
  scale_x_date(
    date_breaks = "3 months",
    date_labels = "%b %Y",
    expand = expansion(add = c(0, 5))
  ) +
  labs(
    title    = "Beta‑Diversity Trajectories by FMT Status",
    subtitle = "Metric: Sørensen (bold = group mean)",
    x        = "Collection Date",
    y        = "Sørensen Similarity to Healthy"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    strip.text    = element_text(face = "bold"),
    axis.text.x   = element_text(angle = 45, hjust = 1),
    plot.subtitle = element_text(size = 10, color = "grey30"),
    panel.spacing = unit(1, "lines")
  )

# Plot 5: Subject Trajectories Upper vs. Lower

library(dplyr)
library(ggplot2)

# 1) Read in your data
betadiv_data <- read.csv("CSV: Other/dat_betadiv_ucsp_2025.csv", stringsAsFactors = FALSE)
table1_data  <- read.csv("CSV: Other/table_1_data.csv",           stringsAsFactors = FALSE)

# 2) Build the same subject_id key so we can join
table1_data <- table1_data %>%
  mutate(subject_id = sprintf("PMTS_%04d", study_id))

# 3) Pull in only the FMT recipients and their route (gi_entry)
fmt_meta <- table1_data %>%
  filter(fmt_yn == "FMT") %>%
  select(subject_id, route = gi_entry) %>%
  mutate(route = factor(route, levels = c("Lower GI", "Upper GI")))

# 4) Join to the beta‑diversity table and keep only those FMT rows
fmt_trajs <- betadiv_data %>%
  mutate(collection_date = as.Date(collection_date)) %>%
  inner_join(fmt_meta, by = "subject_id")

# 5) Compute the mean Bray–Curtis and Sørensen per route and date
route_means <- fmt_trajs %>%
  group_by(route, collection_date) %>%
  summarise(
    mean_bray     = mean(bray_distance, na.rm = TRUE),
    mean_sorenson = mean(sorenson,      na.rm = TRUE),
    .groups = "drop"
  )

# ——— Plot 5A: Bray–Curtis Trajectories by Route ———
ggplot() +
  # individual subject lines
  geom_line(
    data  = fmt_trajs,
    aes(x = collection_date, y = bray_distance, group = subject_id),
    color = "grey60", size = 0.6, alpha = 0.5
  ) +
  # bold mean line
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
    title    = "FMT Recipients: Bray–Curtis Trajectories by Route",
    subtitle = "Bold = route mean",
    x        = "Collection Date",
    y        = "Bray–Curtis Distance to Healthy",
    color    = "FMT Route"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    strip.text      = element_text(face = "bold"),
    axis.text.x     = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    panel.spacing   = unit(1, "lines")
  )

# ——— Plot 5B: Sørensen Trajectories by Route ———
ggplot() +
  # individual subject lines
  geom_line(
    data  = fmt_trajs,
    aes(x = collection_date, y = sorenson, group = subject_id),
    color = "grey60", size = 0.6, alpha = 0.5
  ) +
  # bold mean line
  geom_line(
    data  = route_means,
    aes(x = collection_date, y = mean_sorenson, color = route),
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
    title    = "FMT Recipients: Sørensen Trajectories by Route",
    subtitle = "Bold = route mean",
    x        = "Collection Date",
    y        = "Sørensen Similarity to Healthy",
    color    = "FMT Route"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    strip.text      = element_text(face = "bold"),
    axis.text.x     = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    panel.spacing   = unit(1, "lines")
  )

# 6) Plot: FMT vs No FMT (with subject ID labels)

library(ggrepel)
library(dplyr)
library(ggplot2)

# Prepare the label data: last time‐point for each subject
label_data <- merged %>%
  group_by(fmt_yn, subject_id) %>%
  arrange(collection_date) %>%
  slice_tail(n = 1) %>%
  ungroup()

# ——— Plot 6A: Bray–Curtis Trajectories with Subject Labels ———
ggplot(merged, aes(x = collection_date, y = bray_distance)) +
  geom_line(aes(group = subject_id), color = "steelblue", alpha = 0.5) +
  geom_text_repel(
    data            = label_data,
    aes(label = subject_id, y = bray_distance),
    size            = 2.5,
    direction       = "y",
    segment.alpha   = 0.2,
    min.segment.length = 0
  ) +
  facet_wrap(~ fmt_yn, ncol = 1) +
  scale_x_date(
    date_breaks = "3 months",
    date_labels = "%b %Y",
    expand      = expansion(add = c(0, 5))
  ) +
  labs(
    title    = "Subject-Level Bray–Curtis Trajectories (Labeled)",
    subtitle = "Stratified by FMT vs No FMT",
    x        = "Collection Date",
    y        = "Bray–Curtis Distance to Healthy"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    strip.text    = element_text(face = "bold"),
    axis.text.x   = element_text(angle = 45, hjust = 1),
    panel.spacing = unit(1, "lines")
  )

# ——— Plot 6B: Sørensen Trajectories with Subject Labels ———
ggplot(merged, aes(x = collection_date, y = sorenson)) +
  geom_line(aes(group = subject_id), color = "steelblue", alpha = 0.5) +
  geom_text_repel(
    data            = label_data,
    aes(label = subject_id, y = sorenson),
    size            = 2.5,
    direction       = "y",
    segment.alpha   = 0.2,
    min.segment.length = 0
  ) +
  facet_wrap(~ fmt_yn, ncol = 1) +
  scale_x_date(
    date_breaks = "3 months",
    date_labels = "%b %Y",
    expand      = expansion(add = c(0, 5))
  ) +
  labs(
    title    = "Subject-Level Sørensen Trajectories (Labeled)",
    subtitle = "Stratified by FMT vs No FMT",
    x        = "Collection Date",
    y        = "Sørensen Similarity to Healthy"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    strip.text    = element_text(face = "bold"),
    axis.text.x   = element_text(angle = 45, hjust = 1),
    panel.spacing = unit(1, "lines")
  )

# 7) Plot: Upper vs Lower GI (with subject ID labels)

library(ggrepel)
library(dplyr)
library(ggplot2)

# Prepare the label data: last time‐point for each subject within each route
label_data_route <- fmt_trajs %>%
  group_by(route, subject_id) %>%
  arrange(collection_date) %>%
  slice_tail(n = 1) %>%
  ungroup()

# ——— Plot 7A: Bray–Curtis Trajectories by Route with Labels ———
ggplot(fmt_trajs, aes(x = collection_date, y = bray_distance)) +
  geom_line(aes(group = subject_id), color = "darkorange", size = 0.6, alpha = 0.5) +
  geom_text_repel(
    data                = label_data_route,
    aes(label = subject_id, y = bray_distance),
    size                = 2.5,
    direction           = "y",
    segment.alpha       = 0.2,
    min.segment.length  = 0
  ) +
  facet_wrap(~ route, ncol = 1) +
  scale_x_date(
    date_breaks = "4 months",
    date_labels = "%b %Y",
    expand      = expansion(add = c(0, 5))
  ) +
  labs(
    title    = "Subject-Level Bray–Curtis Trajectories by Route (Labeled)",
    subtitle = "Upper vs Lower GI with subject ID at endpoint",
    x        = "Collection Date",
    y        = "Bray–Curtis Distance to Healthy"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    strip.text    = element_text(face = "bold"),
    axis.text.x   = element_text(angle = 45, hjust = 1),
    panel.spacing = unit(1, "lines")
  )

# ——— Plot 7B: Sørensen Trajectories by Route with Labels ———
ggplot(fmt_trajs, aes(x = collection_date, y = sorenson)) +
  geom_line(aes(group = subject_id), color = "darkorange", size = 0.6, alpha = 0.5) +
  geom_text_repel(
    data                = label_data_route,
    aes(label = subject_id, y = sorenson),
    size                = 2.5,
    direction           = "y",
    segment.alpha       = 0.2,
    min.segment.length  = 0
  ) +
  facet_wrap(~ route, ncol = 1) +
  scale_x_date(
    date_breaks = "4 months",
    date_labels = "%b %Y",
    expand      = expansion(add = c(0, 5))
  ) +
  labs(
    title    = "Subject-Level Sørensen Trajectories by Route (Labeled)",
    subtitle = "Upper vs Lower GI with subject ID at endpoint",
    x        = "Collection Date",
    y        = "Sørensen Similarity to Healthy"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    strip.text    = element_text(face = "bold"),
    axis.text.x   = element_text(angle = 45, hjust = 1),
    panel.spacing = unit(1, "lines")
  )

# Plot 8: Boxplots comparing FMT groups

library(dplyr)
library(ggplot2)

# 1) Read in your data
betadiv_data <- read.csv("CSV: Other/dat_betadiv_ucsp_2025.csv", stringsAsFactors = FALSE)
table1_data  <- read.csv("CSV: Other/table_1_data.csv",           stringsAsFactors = FALSE)

# 2) Build subject IDs in the metadata
table1_data <- table1_data %>%
  mutate(subject_id = sprintf("PMTS_%04d", as.integer(study_id)))

# 3) Pull in only the FMT recipients and their route
fmt_meta <- table1_data %>%
  filter(fmt_yn == "FMT") %>%
  select(subject_id, route = gi_entry)

# 4) Merge on subject_id and create group factor
merged <- betadiv_data %>%
  mutate(
    collection_date = as.Date(collection_date),
    subject_id      = as.character(subject_id)
  ) %>%
  left_join(fmt_meta, by = "subject_id") %>%
  mutate(
    group = case_when(
      is.na(route)         ~ "No FMT",
      route == "Upper GI"  ~ "FMT Upper",
      route == "Lower GI"  ~ "FMT Lower"
    ),
    group = factor(group, levels = c("No FMT", "FMT Upper", "FMT Lower"))
  )

# —— Plot 8A: All Sørensen values ——
ggplot(merged, aes(x = group, y = sorenson, fill = group)) +
  geom_boxplot(width = 0.7, outlier.size = 1) +
  scale_fill_manual(
    values = c("No FMT" = "#999999", "FMT Upper" = "#E69F00", "FMT Lower" = "#56B4E9")
  ) +
  labs(
    title = "All Sørensen Similarity Values by Treatment Group",
    x     = "Treatment Group",
    y     = "Sørensen Similarity"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    axis.text.x     = element_text(face = "bold", angle = 30, hjust = 1),
    axis.title      = element_text(face = "bold"),
    plot.title      = element_text(face = "bold", hjust = 0.5)
  )

# —— Plot 8B: Mean Sørensen per patient ——
# 5) Compute per‑patient mean Sørensen
patient_means <- merged %>%
  group_by(subject_id, group) %>%
  summarise(mean_sorenson = mean(sorenson, na.rm = TRUE), .groups = "drop")

# 6) Boxplot of per‑patient means
ggplot(patient_means, aes(x = group, y = mean_sorenson, fill = group)) +
  geom_boxplot(width = 0.7) +
  scale_fill_manual(
    values = c("No FMT" = "#999999", "FMT Upper" = "#E69F00", "FMT Lower" = "#56B4E9")
  ) +
  labs(
    title = "Mean Sørensen Similarity per Patient by Group",
    x     = "Treatment Group",
    y     = "Mean Sørensen Similarity"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    axis.text.x     = element_text(face = "bold", angle = 30, hjust = 1),
    axis.title      = element_text(face = "bold"),
    plot.title      = element_text(face = "bold", hjust = 0.5)
  )


# Model 9: Regression Model
# Run same structure with the other outcome variables (in notes)
# Ex. Excluding first date, means, and all values 









