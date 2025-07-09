# ------------------------------
# 0. Install & load packages
# ------------------------------
library(phyloseq)
library(tidyverse)
library(tibble)   # for rownames_to_column()

# ------------------------------
# 1. Load data
# ------------------------------
setwd("~/Downloads/Summer Project/HMPData-master/data")
load("V13p4.rda")   # loads the phyloseq object V13p4

# ------------------------------
# 2. Tidy the OTU table
# ------------------------------
otu_long <- V13p4 |>
  otu_table() |>
  as.data.frame() |>
  rownames_to_column("OTU") |>
  pivot_longer(
    cols      = -OTU,
    names_to  = "SampleID",
    values_to = "Count"
  )
head(otu_long)

# ------------------------------
# 3. Add sample metadata
# ------------------------------
otu_long <- otu_long |>
  left_join(
    data.frame(sample_data(V13p4)) |>
      rownames_to_column("SampleID") |>
      select(SampleID, SampleType = sample_type),
    by = "SampleID"
  )
head(otu_long)

# ------------------------------
# 4. Visualizations
# ------------------------------

# 4a. Sequencing depth by sample type
otu_long |>
  group_by(SampleType, SampleID) |>
  summarize(depth = sum(Count), .groups="drop") |>
  ggplot(aes(x = SampleType, y = depth)) +
  geom_boxplot() +
  labs(
    title = "Sequencing Depth by Sample Type",
    x     = "Sample Type",
    y     = "Total Reads"
  )
otu_long

# 4b. OTU richness (Observed OTUs) by sample type
otu_long |>
  filter(Count > 0) |>
  group_by(SampleType, SampleID) |>
  summarize(richness = n(), .groups="drop") |>
  ggplot(aes(x = SampleType, y = richness)) +
  geom_boxplot() +
  labs(
    title = "OTU Richness by Sample Type",
    x     = "Sample Type",
    y     = "Observed OTUs"
  )
otu_long

# 4c. Distribution of non-zero counts (log scale)
otu_long |>
  filter(Count > 0) |>
  ggplot(aes(x = Count)) +
  geom_histogram() +
  scale_x_log10() +
  labs(
    title = "Distribution of Non-Zero Counts (Log Scale)",
    x     = "Count (log10)",
    y     = "Frequency"
  )
otu_long

# 4d. Top 10 most abundant OTUs across all samples
otu_long |>
  group_by(OTU) |>
  summarize(total_abund = sum(Count), .groups="drop") |>
  arrange(desc(total_abund)) |>
  slice_head(n = 10) |>
  mutate(OTU = fct_reorder(OTU, total_abund)) |>
  ggplot(aes(x = OTU, y = total_abund)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Top 10 Most Abundant OTUs",
    x     = "OTU",
    y     = "Total Read Count"
  )
otu_long

# 4e. Save the Top-10 barplot
ggsave(
  filename = "top10_otu_barplot.png",
  width    = 8,
  height   = 5,
  dpi      = 300
)

# 1. Compute sequencing depth per sample
sample_depths <- otu_long |>
  group_by(SampleID) |>
  summarize(depth = sum(Count), .groups="drop")
head(sample_depths)

# 2. Calculate mean & standard deviation
mean_depth <- mean(sample_depths$depth)
sd_depth   <- sd(sample_depths$depth)
mean_depth
sd_depth

# 3. Plot histogram + normal density overlay
sample_depths |>
  ggplot(aes(x = depth)) +
  # histogram, scaled to density so we can overlay a PDF
  geom_histogram(aes(y = ..density..),
                 bins  = 30,
                 fill  = "lightblue",
                 color = "black") +
  # normal curve with our mean & sd
  stat_function(
    fun = dnorm,
    args = list(mean = mean_depth, sd = sd_depth),
    color = "red",
    size  = 1
  ) +
  labs(
    title = "Sequencing Depth Distribution\nwith Normal Fit",
    x     = "Total Reads per Sample",
    y     = "Density"
  )

library(phyloseq)
library(tidyverse)

# Agglomerate at the Genus level
genus_level <- V13p4 |>
  tax_glom(taxrank = "Genus")        # merges OTUs into Genus bins

# Tidy into long form, now with Genus replacing OTU
otu_genus_long <- genus_level |>
  otu_table()    |> as.data.frame() |>
  rownames_to_column("Genus") |>
  pivot_longer(
    -Genus, names_to = "SampleID", values_to = "Count"
  ) |>
  left_join(
    data.frame(sample_data(genus_level)) |>
      rownames_to_column("SampleID") |>
      select(SampleID, SampleType = sample_type),
    by="SampleID"
  )

# Now your “OTU” column is a genuine Genus name:
otu_genus_long |> glimpse()

library(phyloseq)
library(tidyverse)

# Agglomerate at the Genus level
genus_level <- V13p4 |>
  tax_glom(taxrank = "Genus")        # merges OTUs into Genus bins

# Tidy into long form, now with Genus replacing OTU
otu_genus_long <- genus_level |>
  otu_table()    |> as.data.frame() |>
  rownames_to_column("Genus") |>
  pivot_longer(
    -Genus, names_to = "SampleID", values_to = "Count"
  ) |>
  left_join(
    data.frame(sample_data(genus_level)) |>
      rownames_to_column("SampleID") |>
      select(SampleID, SampleType = sample_type),
    by="SampleID"
  )

# Now your “OTU” column is a genuine Genus name:
otu_genus_long |> glimpse()
) 
# Diversity of stool and saliva samples (1) Alpha diversity vegan (2) beta diversity
# Box plots for the alpha diversity for stool and saliva --> saliva and stool samples come from the
# same person. Dot plot, compare saliva and stool linked by the same person 
# Beta diversity --> spit out square distance matrix --> check out vegdist() function
# Dist structure --> lower triangle of a distance matrix 
