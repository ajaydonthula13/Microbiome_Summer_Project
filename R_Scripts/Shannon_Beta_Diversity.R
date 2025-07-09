# ------------------------------
# Load packages & resolve conflicts
# ------------------------------
# install if you haven’t already:
# install.packages("conflicted")

library(conflicted)    # must come before tidyverse / ape
library(phyloseq)
library(tidyverse)
library(vegan)
library(ape)

# Tell conflicted which version you want when names clash:
conflict_prefer("filter", "dplyr")  # use dplyr::filter(), not stats::filter()
conflict_prefer("lag",    "dplyr")  # use dplyr::lag(),    not stats::lag()
conflict_prefer("where",  "dplyr")  # use dplyr::where(),  not ape::where()
# ------------------------------
# Load & subset data
# ------------------------------
setwd("~/Downloads/Summer Project/Microbiome Summer Project")
load("V13p4.rda")    # loads phyloseq object V13p4

phy_sub <- V13p4 |>
  subset_samples(sample_type %in% c("Stool", "Saliva"))

# ------------------------------
# Alpha diversity (Shannon)
# ------------------------------

# 1. Take a peek at your metadata column names:
phy_sub |>
  sample_data() |>
  as_tibble(rownames="SampleID") |>
  names()

# ------------------------------
# Alpha diversity (Shannon)
# ------------------------------
alpha_df <- phy_sub |>
  otu_table() |>
  as.data.frame() |>
  t() |>
  diversity(index = "shannon") |>
  enframe(name = "SampleID", value = "Shannon") |>
  left_join(
    phy_sub |>
      sample_data() |>
      as_tibble(rownames = "SampleID") |>
      select(
        SampleID,
        sample_type,
        SubjectID = host_subject_id   # <— use the real column name here
      ),
    by = "SampleID"
  )
view(alpha_df)

# Box plot for alpha diversity
alpha_df |>
  ggplot(aes(x = sample_type, y = Shannon, fill = sample_type)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.7) +
  geom_jitter(width = 0.1, alpha = 0.4, color = "black", size = 1) +
  labs(
    title = "Shannon α-Diversity by Sample Type",
    x     = "Sample Type",
    y     = "Shannon Index"
  ) +
  theme_minimal() +
  theme(legend.position = "none")
wilcox.test(Shannon ~ sample_type, data = alpha_df) |> 
  broom:tidy() # Displaying output differently

# ------------------------------
# Beta diversity (Bray–Curtis)
# ------------------------------
bray_mat_lower <- phy_sub |>
  otu_table() |>
  as.data.frame() |>
  t() |>
  vegdist(method = "bray") |>
  as.matrix() |>
  (\(m) m[lower.tri(m)])()

# inspect the values
print(bray_mat_lower)

# ------------------------------
# PCoA on Bray–Curtis (dropping empty samples)
# ------------------------------
phy_sub_filt <- prune_samples(
  sample_sums(phy_sub) > 0,   # logical vector of samples to KEEP
  phy_sub                      # your phyloseq object
)
pcoa_df <- phy_sub_filt |>
  otu_table() |>
  as.data.frame() |>
  t() |>
  vegdist(method = "bray") |>
  pcoa() |>
  (\(res) res$vectors[,1:2])() |>
  as_tibble(rownames = "SampleID", .name_repair = "minimal") |>
  rename(PCoA1 = V1, PCoA2 = V2) |>
  left_join(
    phy_sub_filt |>
      sample_data() |>
      as_tibble(rownames = "SampleID") |>
      select(SampleID, sample_type),
    by = "SampleID"
  )

# ------------------------------
# 4. PCoA scatter for reference
# ------------------------------
pcoa_df <- phy_sub_filt |>
  otu_table() |>
  as.data.frame() |>
  t() |>
  vegdist(method = "bray") |>
  pcoa() |>
  (\(res) res$vectors[, 1:2])() |>
  as_tibble(rownames = "SampleID", .name_repair = "minimal") |>
  rename(
    PCoA1 = `Axis.1`,
    PCoA2 = `Axis.2`
  ) |>
  left_join(
    phy_sub_filt |>
      sample_data() |>
      as_tibble(rownames = "SampleID") |>
      select(SampleID, sample_type),
    by = "SampleID"
  )

pcoa_df |>
  ggplot(aes(x = PCoA1, y = PCoA2, color = sample_type)) +
  geom_point(size = 3) +
  labs(
    title = "PCoA of Bray–Curtis Distances",
    x     = "PCoA Axis 1",
    y     = "PCoA Axis 2"
  ) +
  theme_minimal()

# ------------------------------
# PCoA on Bray–Curtis (dropping empty samples) (3D Model)
# ------------------------------

# Filter out empty samples
phy_sub_filt <- prune_samples(
  sample_sums(phy_sub) > 0,
  phy_sub
)

pcoa_df <- phy_sub_filt |>
  otu_table() |>
  as.data.frame() |>
  t() |>
  vegdist(method = "bray") |>
  pcoa() |>
  (\(res) {
    vecs <- res$vectors[, 1:3]
    colnames(vecs) <- c("PCoA1", "PCoA2", "PCoA3")
    vecs
  })() |>
  as_tibble(rownames = "SampleID") |>
  left_join(
    phy_sub_filt |>
      sample_data() |>
      as_tibble(rownames = "SampleID") |>
      select(SampleID, sample_type),
    by = "SampleID"
  )

library(plotly)

plot_ly(
  data = pcoa_df,
  x = ~PCoA1,
  y = ~PCoA2,
  z = ~PCoA3,
  color = ~sample_type,
  type = "scatter3d",
  mode = "markers",
  marker = list(size = 4)
) |>
  layout(
    title = "3D PCoA (Bray–Curtis)",
    scene = list(
      xaxis = list(title = "PCoA1"),
      yaxis = list(title = "PCoA2"),
      zaxis = list(title = "PCoA3")
    )
  )

# ------------------------------
# 1. Compute full Bray–Curtis distance matrix
# ------------------------------
dist_mat <- phy_sub_filt |>
  otu_table() |>
  as.data.frame() |>
  t() |>
  vegdist(method = "bray") |>
  as.matrix()

# Print the full square matrix
print(dist_mat)
View(dist_mat)

# ------------------------------
# 2. Tidy into a lower-triangle table (Removes Redundancy)
# ------------------------------
library(tibble)
library(tidyr)

dist_tbl <- dist_mat |>
  as.data.frame() |>
  rownames_to_column("Sample1") |>
  pivot_longer(
    cols      = -Sample1,
    names_to  = "Sample2",
    values_to = "BrayCurtis"
  ) |>
  # keep only one of (i,j) vs (j,i)
  filter(Sample1 < Sample2)

# View the tidy table
print(dist_tbl)
View(dist_tbl)

# ------------------------------
# 3. Heatmap of distances
# ------------------------------
library(tibble)
library(tidyr)
library(ggplot2)
library(viridis)

# turn your square matrix into a long data frame
heatmap_df <- dist_mat |>
  as.data.frame() |>
  rownames_to_column("SampleA") |>
  pivot_longer(
    cols      = -SampleA,
    names_to  = "SampleB",
    values_to = "BrayCurtis"
  )

# draw the heatmap
heatmap_df |>
  ggplot(aes(x = SampleB, y = SampleA, fill = BrayCurtis)) +
  geom_tile() +
  scale_fill_viridis_c(option = "C", name = "Bray–Curtis") +
  labs(
    title = "Pairwise Bray–Curtis Dissimilarity Heatmap",
    x     = "Sample B",
    y     = "Sample A"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    axis.text.y = element_text(size = 6),
    plot.title  = element_text(face = "bold", hjust = 0.5)
  )

# ------------------------------
# 5. Identify common taxa between Stool & Saliva
# ------------------------------
stool_phy <- phy_sub |>
  subset_samples(sample_type == "Stool") |>
  (\(x) prune_samples(sample_sums(x) > 0, x))()

saliva_phy <- phy_sub |>
  subset_samples(sample_type == "Saliva") |>
  (\(x) prune_samples(sample_sums(x) > 0, x))()

stool_taxa  <- taxa_names(stool_phy)
saliva_taxa <- taxa_names(saliva_phy)
common_taxa <- intersect(stool_taxa, saliva_taxa)
common_phy  <- prune_taxa(common_taxa, phy_sub_filt)

# ------------------------------
# 6. Distance & similarity on common taxa
# ------------------------------
dist_common <- common_phy |>
  otu_table() |>
  as.data.frame() |>
  t() |>
  vegdist(method = "bray") |>
  as.matrix()

# convert to similarity
sim_common <- 1 - dist_common

# (optional) tidy lower triangle for export
dist_common_tbl <- dist_common |>
  as.data.frame() |>
  rownames_to_column("SampleA") |>
  pivot_longer(-SampleA,
               names_to  = "SampleB",
               values_to = "BrayCurtis") |>
  filter(SampleA < SampleB)

# ------------------------------
# 7. PCoA on common-taxa Bray–Curtis
# ------------------------------
pcoa_common_df <- common_phy |>
  otu_table() |>
  as.data.frame() |>
  t() |>
  vegdist("bray") |>
  pcoa() |>
  (\(res) res$vectors[, 1:2])() |>
  as_tibble(rownames = "SampleID", .name_repair = "minimal") |>
  rename(PCoA1 = `Axis.1`, PCoA2 = `Axis.2`) |>
  left_join(
    common_phy |>
      sample_data() |>
      as_tibble(rownames = "SampleID") |>
      select(SampleID, sample_type),
    by = "SampleID"
  )

pcoa_common_df |>
  ggplot(aes(x = PCoA1, y = PCoA2, color = sample_type)) +
  geom_point(size = 3) +
  labs(
    title = "PCoA of Bray–Curtis on Shared Taxa",
    x     = "PCoA Axis 1",
    y     = "PCoA Axis 2"
  ) +
  theme_minimal()

# ------------------------------
# 8. Heatmap of Bray–Curtis on Common Taxa (Dissimilarity)
# ------------------------------
library(pheatmap)
library(viridis)

# 8a) Build an ordering from the common‐taxa PCoA
order_common <- pcoa_common_df |>
  arrange(sample_type, PCoA1) |>
  pull(SampleID)

# 8b) Reorder the common‐taxa distance matrix
mat_common_ord <- dist_common[order_common, order_common]

# 8c) Build annotation strip
annot_common <- common_phy |>
  sample_data() |>
  as_tibble(rownames="SampleID") |>
  select(SampleID, sample_type) |>
  column_to_rownames("SampleID")

# 8d) Draw the heatmap
pheatmap(
  mat_common_ord,
  cluster_rows   = FALSE,
  cluster_cols   = FALSE,
  show_rownames  = FALSE,
  show_colnames  = FALSE,
  annotation_row = annot_common,
  annotation_col = annot_common,
  color          = viridis(50),
  main           = "Bray–Curtis on Shared Taxa"
)

# ------------------------------
# 8. Heatmap of shared-taxa similarity
# ------------------------------
library(pheatmap)
library(viridis)

order_common <- pcoa_common_df |>
  arrange(sample_type, PCoA1) |>
  pull(SampleID)

mat_sim_ord <- sim_common[order_common, order_common]

annot_common <- common_phy |>
  sample_data() |>
  as_tibble(rownames = "SampleID") |>
  select(SampleID, sample_type) |>
  column_to_rownames("SampleID")

pheatmap(
  mat_sim_ord,
  cluster_rows   = FALSE,
  cluster_cols   = FALSE,
  show_rownames  = FALSE,
  show_colnames  = FALSE,
  annotation_row = annot_common,
  annotation_col = annot_common,
  color          = viridis(50),
  main           = "Shared-Taxa Similarity (1 – Bray–Curtis)"
)

# ------------------------------
# SECTION 13: LEfSe Analysis (LDA Effect Size Biomarkers) (Using theme Function)
# ------------------------------

# 1. Load packages
library(phyloseq)
library(microbiomeMarker)
library(ggplot2)

# 2. Set working directory and load phyloseq object
setwd("~/Downloads/Summer Project/HMPData-master/data/")
load("V13p4.rda")    # loads phyloseq object 'V13p4'

# 3. Subset just Stool and Saliva samples
phy_sub <- subset_samples(V13p4, sample_type %in% c("Stool", "Saliva"))

# 4. Remove samples with zero reads (recommended)
phy_sub_filt <- prune_samples(sample_sums(phy_sub) > 0, phy_sub)

# 5. Run LEfSe biomarker discovery (no LDA cutoff so we see all features)
lefse_res <- run_lefse(
  phy_sub_filt,
  group      = "sample_type",
  taxa_rank  = "all",
  transform  = "identity",
  norm       = "TSS",
  lda_cutoff = 0
)

# 6. Base barplot via microbiomeMarker
p <- plot_ef_bar(lefse_res)
p

# 7. Tweak theme so the long y‐axis labels are legible
p + 
  theme_minimal(base_size  = 8, base_family = "sans") +
  theme(
    axis.text.y  = element_text(size = 1, angle = 0, family = "sans"),  # smaller, straight labels
    axis.title.y = element_text(size = 8, face = "bold"),              # y‐axis title
    plot.title   = element_text(size = 10, face = "bold", hjust = 0.5), # center bold title
    legend.position = "right"
  ) +
  labs(
    title = "LEfSe Biomarker Discovery (Stool vs. Saliva)",
    y     = "Taxon (full names shown in table)",
    x     = "LDA Score (log10)"
  )

# ------------------------------
# SECTION 14: LEfSe Analysis Cleaned up ggplot2 LEfSe Biomarker *TOP 20*
# ------------------------------
X# 1. Extract from S4
mt <- lefse_res@marker_table
slot_names <- mt@names
slot_list  <- mt@.Data
marker_table <- as.data.frame(setNames(slot_list, slot_names), stringsAsFactors = FALSE)

# 2. Truncate feature names for display
marker_table$short_feature <- substr(marker_table$feature, 1, 40)

# 3. Pick the top N features by LDA score (now includes short_feature)
library(dplyr)
library(ggplot2)
topN <- 20
marker_table_top <- marker_table %>%
  arrange(desc(ef_lda)) %>%
  slice_head(n = topN)

# 4. Plot (now short_feature definitely exists)
ggplot(marker_table_top, aes(
  x = reorder(short_feature, ef_lda),
  y = ef_lda,
  fill = enrich_group
)) +
  geom_col(show.legend = TRUE) +
  coord_flip() +
  labs(
    title = "LEfSe Biomarker Discovery (Stool vs. Saliva)",
    subtitle = paste0("Top ", topN, " discriminative features by LDA Effect Size"),
    x = "Taxon (truncated)",
    y = "LDA Score (log10)",
    fill = "Enriched Group"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    plot.subtitle = element_text(size = 13, hjust = 0.5),
    axis.text.y = element_text(size = 7),
    axis.title = element_text(face = "bold", size = 14),
    legend.position = "right"
  )

# ------------------------------
# 9. List of taxa shared by most samples
# ------------------------------

# 9a) Compute per‐OTU prevalence (# of samples in which each OTU appears)
common_prev <- common_phy |>
  otu_table() |>
  as.data.frame() |>
  # OTUs are rows, samples are cols
  rownames_to_column("Taxon") |>
  mutate(
    Prevalence = rowSums(across(-Taxon) > 0)
  ) |>
  arrange(desc(Prevalence))

# 9b) Grab your taxonomy table once
tax_df <- common_phy |>
  tax_table()       |>
  as.data.frame()   |>
  rownames_to_column("Taxon")

# 9c) Join and print the top 20
top20_df <- common_prev |>
  slice_head(n = 50) |>
  left_join(tax_df, by = "Taxon") |>
  rename(OTU = Taxon, `Samples Present` = Prevalence) |>
  select(
    OTU,
    Kingdom, Phylum, Class, Order, Family, Genus, Species,
    `Samples Present`
  )

# print a static table via kable
top20_df |>
  knitr::kable(
    col.names = c(
      "OTU ID", "Kingdom", "Phylum", "Class", "Order",
      "Family", "Genus", "Species", "Samples Present"
    ),
    digits = 0
  )

# 9d. Interactive table via reactable
library(reactable)

reactable(
  top20_df,
  columns = list(
    OTU = colDef(name = "OTU ID", minWidth = 200),
    Kingdom = colDef(minWidth = 100),
    Phylum  = colDef(minWidth = 100),
    Class   = colDef(minWidth = 120),
    Order   = colDef(minWidth = 120),
    Family  = colDef(minWidth = 140),
    Genus   = colDef(minWidth = 160),
    Species = colDef(minWidth = 160),
    `Samples Present` = colDef(
      name    = "Samples Present",
      aggregate = "max"
    )
  ),
  filterable      = TRUE,
  defaultPageSize = 50
)


# 9d) Just to confirm the taxonomy ranks you have:
print(rank_names(common_phy))