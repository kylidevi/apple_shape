# Spearman Rank Correlation -----------------------------------------------

library(tidyverse)

cor_table <- read.csv("C:\\Users\\User\\Desktop\\COOP02\\code\\apple_shape\\20241029apple_metadata.csv") %>% 
  distinct(apple_id, .keep_all = TRUE) %>% 
  dplyr::select(20:29)
  
pairwise_comp <- matrix(, ncol = ncol(cor_table), nrow = ncol(cor_table))
rownames(pairwise_comp) <- colnames(cor_table)
colnames(pairwise_comp) <- colnames(cor_table)

pairwise_comp_r <- matrix(, ncol = ncol(cor_table), nrow = ncol(cor_table))
rownames(pairwise_comp_r) <- colnames(cor_table)
colnames(pairwise_comp_r) <- colnames(cor_table)


for (i in 1:ncol(cor_table)) {
  name_x = colnames(cor_table)[i]
  for (j in 1:ncol(cor_table)) {
    name_y = colnames(cor_table)[j]
    pairwise_comp[j, i] = cor.test(cor_table[,i], cor_table[,j], method = "spearman")$p.value
    pairwise_comp_r[j, i] = cor.test(cor_table[,i], cor_table[,j], method = "spearman")$estimate
  }
}


# Bonferroni Correction ---------------------------------------------------
pairwise_comp[lower.tri(pairwise_comp)] = NA

pairwise_comp[upper.tri(pairwise_comp)] = p.adjust(pairwise_comp[upper.tri(pairwise_comp)], method = "bonferroni")

# 45 comparisons
length(pairwise_comp[upper.tri(pairwise_comp)] )


#how many are significant?
table(pairwise_comp[upper.tri(pairwise_comp)] <0.05)

# FALSE  TRUE 
#    28    17 
# 17/45 are significant 


# Formatting Table --------------------------------------------------------
correlation_data <- data.frame(
  row = rep(rownames(pairwise_comp_r), times = ncol(pairwise_comp_r)),
  col = rep(colnames(pairwise_comp_r), each = ncol(pairwise_comp_r)),
  correlation = as.vector(pairwise_comp_r),
  pval = as.vector(pairwise_comp))

write.csv(correlation_data, file = "correlation_values.csv")

correlation_data <- correlation_data %>% 
  mutate(correlation = ifelse(pval >= 0.05, NA, correlation))

write.csv(correlation_data, file = "correlation_values_NA.csv")


# heat map
#pdf("file_name.pdf", width = 8.5, height = 8.5)
ggplot(correlation_data, aes(x = row, y = col, fill = correlation)) +
  geom_tile(color = "white") + 
  scale_fill_gradientn(colors = c("#0072B2", "white", "#E69F00"), 
                       values = scales::rescale(c(-1, 0, 1)), 
                       name = "Spearman\nCorrelation", 
                       na.value = "white") +  # Set NA values to white
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 10, hjust = 1), axis.text.y = element_text(size = 10), axis.title = element_blank()) +
  scale_x_discrete(labels = c("area_reml_lsmeans_predict" = "area", "aspect_ratio" = "aspect ratio", "asymmetry_reml_lsmeans_predict" = "asymmetry", "length_reml_lsmeans_predict" = "length", "PC1_reml_lsmeans_predict" = "PC1", "PC2_reml_lsmeans_predict" = "PC2", "PC3_reml_lsmeans_predict" = "PC3", "solidity_reml_lsmeans_predict" = "solidity", "weight_avg_16_harv" = "harvest weight", "width_reml_lsmeans_predict" = "width")) +
  scale_y_discrete(labels = c("area_reml_lsmeans_predict" = "area", "aspect_ratio" = "aspect ratio", "asymmetry_reml_lsmeans_predict" = "asymmetry", "length_reml_lsmeans_predict" = "length", "PC1_reml_lsmeans_predict" = "PC1", "PC2_reml_lsmeans_predict" = "PC2", "PC3_reml_lsmeans_predict" = "PC3", "solidity_reml_lsmeans_predict" = "solidity", "weight_avg_16_harv" = "harvest weight", "width_reml_lsmeans_predict" = "width")) +
  coord_fixed() +
  theme(plot.title = element_text(hjust = 0.5))
dev.off()
