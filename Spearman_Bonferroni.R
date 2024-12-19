# Spearman Rank Correlation -----------------------------------------------

# load in libraries
library(tidyverse)

# load in data set and select for calculated values
cor_table <- read.csv("C:/Users/User/Desktop/COOP02/code/apple_shape/output_final/20241219apple_metadata.csv") %>% 
  distinct(apple_id, .keep_all = TRUE) %>% 
  dplyr::select(17:25, 33)
  
# creating matrices to store values in
pairwise_comp <- matrix(, ncol = ncol(cor_table), nrow = ncol(cor_table))
rownames(pairwise_comp) <- colnames(cor_table)
colnames(pairwise_comp) <- colnames(cor_table)

pairwise_comp_r <- matrix(, ncol = ncol(cor_table), nrow = ncol(cor_table))
rownames(pairwise_comp_r) <- colnames(cor_table)
colnames(pairwise_comp_r) <- colnames(cor_table)

# testing each comparison
for (i in 1:ncol(cor_table)) {
  name_x = colnames(cor_table)[i]
  for (j in 1:ncol(cor_table)) {
    name_y = colnames(cor_table)[j]
    pairwise_comp[j, i] = cor.test(cor_table[,i], cor_table[,j], method = "spearman")$p.value
    pairwise_comp_r[j, i] = cor.test(cor_table[,i], cor_table[,j], method = "spearman")$estimate
  }
}


# Bonferroni Correction ---------------------------------------------------
# evaluating the upper tri
pairwise_comp[lower.tri(pairwise_comp)] = NA

# Bonferroni adjusting the p-values
pairwise_comp[upper.tri(pairwise_comp)] = p.adjust(pairwise_comp[upper.tri(pairwise_comp)], method = "bonferroni")

# 45 comparisons
length(pairwise_comp[upper.tri(pairwise_comp)] )


#how many are significant?
table(pairwise_comp[upper.tri(pairwise_comp)] <0.05)
# FALSE  TRUE 
#    25    20 
# 20/45 are significant
# 19/20 are positive


# Formatting Table --------------------------------------------------------
# creating a dataframe
correlation_data <- data.frame(
  row = rep(rownames(pairwise_comp_r), times = ncol(pairwise_comp_r)),
  col = rep(colnames(pairwise_comp_r), each = ncol(pairwise_comp_r)),
  correlation = as.vector(pairwise_comp_r),
  pval = as.vector(pairwise_comp))

# removing self comparisons and keeping one copy of each comparison
correlation_data <- correlation_data %>% 
  subset(!is.na(pval)) %>% 
  subset(row != col)

# renaming columns
correlation_data <- correlation_data %>% 
  rename("trait 1" = row, "trait 2" = col)

# renaming traits
correlation_data <- correlation_data %>% 
  mutate(`trait 1` = gsub("width_reml_lsmeans_predict", "Width", `trait 1`)) %>%
  mutate(`trait 1` = gsub("length_reml_lsmeans_predict", "Length", `trait 1`)) %>% 
  mutate(`trait 1` = gsub("area_reml_lsmeans_predict", "Area", `trait 1`)) %>% 
  mutate(`trait 1` = gsub("solidity_reml_lsmeans_predict", "Solidity", `trait 1`)) %>% 
  mutate(`trait 1` = gsub("asymmetry_reml_lsmeans_predict", "Asymmetry", `trait 1`)) %>% 
  mutate(`trait 1` = gsub("PC1_reml_lsmeans_predict", "PC1", `trait 1`)) %>% 
  mutate(`trait 1` = gsub("PC2_reml_lsmeans_predict", "PC2", `trait 1`)) %>% 
  mutate(`trait 1` = gsub("PC3_reml_lsmeans_predict", "PC3", `trait 1`)) %>% 
  mutate(`trait 1` = gsub("weight_avg_16_harv", "Harvest Weight", `trait 1`)) %>% 
  mutate(`trait 1` = gsub("aspect_ratio", "Aspect Ratio", `trait 1`)) %>% 
  mutate(`trait 1` = gsub("release_year", "Release Year", `trait 1`))

correlation_data <- correlation_data %>% 
  mutate(`trait 2` = gsub("width_reml_lsmeans_predict", "Width", `trait 2`)) %>%
  mutate(`trait 2` = gsub("length_reml_lsmeans_predict", "Length", `trait 2`)) %>% 
  mutate(`trait 2` = gsub("area_reml_lsmeans_predict", "Area", `trait 2`)) %>% 
  mutate(`trait 2` = gsub("solidity_reml_lsmeans_predict", "Solidity", `trait 2`)) %>% 
  mutate(`trait 2` = gsub("asymmetry_reml_lsmeans_predict", "Asymmetry", `trait 2`)) %>% 
  mutate(`trait 2` = gsub("PC1_reml_lsmeans_predict", "PC1", `trait 2`)) %>% 
  mutate(`trait 2` = gsub("PC2_reml_lsmeans_predict", "PC2", `trait 2`)) %>% 
  mutate(`trait 2` = gsub("PC3_reml_lsmeans_predict", "PC3", `trait 2`)) %>% 
  mutate(`trait 2` = gsub("weight_avg_16_harv", "Harvest Weight", `trait 2`)) %>% 
  mutate(`trait 2` = gsub("aspect_ratio", "Aspect Ratio", `trait 2`)) %>% 
  mutate(`trait 2` = gsub("release_year", "Release Year", `trait 2`))
  
# export table
write.csv(correlation_data, file = "C:/Users/User/Desktop/COOP02/code/apple_shape/output_final/20241219Appendix_Table_S2.csv", row.names = FALSE)

table(correlation_data$correlation < 0 & correlation_data$pval < 0.05)
# FALSE  TRUE 
#    40     5 
