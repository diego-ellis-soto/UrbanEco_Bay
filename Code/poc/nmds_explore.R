# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# Explore PCA and NMDS
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

library(ggplot2)
require(viridis)
library(vegan)

# Create a numeric-only subset (excluding columns like Name, UWIN.Code, Notes, nldc_landcover, etc.)
puzzles_lauren_sf_anno_v6 = read.csv('Outdir/puzzles_lauren_sf_anno_v6_1000m_buffer.csv', sep = ',')
puzzles_lauren_sf_anno_v6$restaurant_count = as.numeric(puzzles_lauren_sf_anno_v6)

df_numeric <- puzzles_lauren_sf_anno_v6 %>%
  select_if(is.numeric)

# Check for missing values
df_numeric <- na.omit(df_numeric)  # Remove rows with any NAs (or handle them as appropriate)

df_numeric_all = df_numeric |> left_join(puzzles_lauren_sf_anno_v6)

df_numeric = df_numeric |> select(mean_income, mean_pop_density, mean_housing_density,
                     restaurant_count, road_density, distance_to_road,
                     imp_surf, ndvi, elev, 
                     bio_1, bio_12, human_mod,
                     BURDEN_RESCALED_AV_PCT, OGBURDEN_AV)


df_numeric = df_numeric |> select(mean_income, mean_pop_density, mean_housing_density,
                                  restaurant_count, road_density, distance_to_road,
                                  human_mod, imp_surf
                                  )



# df_numeric <- df_numeric %>%
#   select(,--buffer_size, -X)# if those columns are not informative

# Scale = TRUE standardizes each variable to have unit variance
pca_res <- prcomp(df_numeric, scale. = TRUE)

# Check results
summary(pca_res)

# PCA scores (coordinates of each row in the new PC space)
pca_scores <- as.data.frame(pca_res$x)

# Add back a grouping variable for plotting (e.g., 'nldc_landcover' or 'Notes')
pca_scores$nldc_landcover <- df_numeric_all$nldc_landcover

pca_scores$imp_surf =   df_numeric_all$imp_surf

# If you want the variable loadings:
pca_loadings <- as.data.frame(pca_res$rotation)



# Plot PC1 vs. PC2, colored by a categorical variable (e.g., nldc_landcover)
# ggplot(pca_scores, aes(x = PC1, y = PC2, color = imp_surf)) +
  ggplot(pca_scores, aes(x = PC1, y = PC2, color = nldc_landcover)) +
  geom_point(size = 3) +
  theme_minimal() +
  labs(
    title = "PCA (PC1 vs. PC2)",
    x = paste0("PC1 (", round(summary(pca_res)$importance[2,1] * 100, 1), "%)"),
    y = paste0("PC2 (", round(summary(pca_res)$importance[2,2] * 100, 1), "%)")
  ) +
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_viridis(option="magma")

# You can choose different distance measures (e.g., "euclidean", "bray", etc.)
dist_mat <- vegdist(df_numeric, method = "euclidean")

# metaMDS tries multiple random starts to find a stable solution
nmds_res <- metaMDS(df_numeric, distance = "euclidean", k = 2, trymax = 100)

# Inspect the stress value
nmds_res$stress

# The NMDS coordinates for each row/site
nmds_scores <- as.data.frame(nmds_res$points)

# Add back grouping info for plotting
nmds_scores$nldc_landcover <- df_numeric_all$nldc_landcover

ggplot(nmds_scores, aes(x = MDS1, y = MDS2, color = nldc_landcover)) +
  geom_point(size = 3) +
  theme_minimal() +
  labs(
    title = paste("NMDS (Stress =", round(nmds_res$stress, 3), ")"),
    x = "NMDS1",
    y = "NMDS2"
  ) +
  theme(plot.title = element_text(hjust = 0.5))



# You can experiment with different scaling factors
arrow_scale <- 5

pca_loadings$PC1 <- pca_loadings$PC1 * arrow_scale
pca_loadings$PC2 <- pca_loadings$PC2 * arrow_scale

pca_loadings$variable <- rownames(pca_loadings)

library(ggplot2)

ggplot() +
  # Plot the PCA scores, colored using viridis scale
  geom_point(
    data = pca_scores, 
    aes(x = PC1, y = PC2, color = imp_surf), 
    size = 3
  ) +
  scale_color_viridis_c(option = "plasma", direction = -1) +  # Use plasma color scheme for high contrast
  
  # Plot the arrows (segments) for the loadings
  geom_segment(
    data = pca_loadings,
    aes(x = 0, y = 0, xend = PC1, yend = PC2),
    arrow = arrow(length = unit(0.2, "cm")),  # Arrowhead size
    color = "black",
    alpha = 0.8
  ) +
  
  # Label the arrows with variable names
  geom_text(
    data = pca_loadings,
    aes(x = PC1, y = PC2, label = variable),
    color = "black",
    vjust = -0.5,
    size = 3
  ) +
  
  # Minimal theme and axis labels
  theme_minimal() +
  labs(
    title = "PCA Biplot (PC1 vs PC2)",
    x = paste0("PC1 (", round(summary(pca_res)$importance[2, 1] * 100, 1), "%)"),
    y = paste0("PC2 (", round(summary(pca_res)$importance[2, 2] * 100, 1), "%)"),
    color = "Impervious Surface (%)"
  ) +
  theme(plot.title = element_text(hjust = 0.5))
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# Human mobility data
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---


# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# Run a single racoon model run:
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
