## Figure 2 and 4 correlation plots
# load in libraries
library(tidyverse)
library(GGally)
library(ggthemes)
library(cowplot)
library(ggpubr)
library(viridis)

setwd("C:/Users/User/Desktop/COOP02/code/apple_shape")
# read in data
apple_data <- read.csv("20241029apple_metadata.csv") %>% 
  distinct(apple_id, .keep_all = TRUE)

## Figure 2
## plotting for 534 different apple_id with reml adjusted values
# width x length ----------------------------------------------------------
widthXlength <- ggplot(data = apple_data, aes(x = width_reml_lsmeans_predict, y = length_reml_lsmeans_predict)) +
  geom_point(colour = "grey") +
  geom_smooth(method = "lm", se = FALSE, colour = "black") +
  labs(x = "Width (cm)", y = "Length (cm)") +
  theme_few()



# length x area -----------------------------------------------------------
lengthXarea <- ggplot(data = apple_data, aes(x = length_reml_lsmeans_predict, y = area_reml_lsmeans_predict)) +
  geom_point(colour = "grey") +
  geom_smooth(method = "lm", se = FALSE, colour = "black") +
  labs(x = "Length (cm)", y = expression("Area (cm" ^ 2*")")) +
  theme_few()



# width x area ------------------------------------------------------------
widthXarea <- ggplot(data = apple_data, aes(x = width_reml_lsmeans_predict, y = area_reml_lsmeans_predict)) +
  geom_point(colour = "grey") +
  geom_smooth(method = "lm", se = FALSE, colour = "black") +
  labs(x = "Width (cm)", y = expression("Area (cm" ^ 2*")")) +
  theme_few()



# length x weight ---------------------------------------------------------
lengthXweight <- ggplot(data = apple_data, aes(x = length_reml_lsmeans_predict, y = weight_avg_16_harv)) +
  geom_point(colour = "grey") +
  geom_smooth(method = "lm", se = FALSE, colour = "black") +
  labs(x = "Length (cm)", y = "Harvest Weight (g)") +
  theme_few()



# width x weight ----------------------------------------------------------
widthXweight <- ggplot(data = apple_data, aes(x = width_reml_lsmeans_predict, y = weight_avg_16_harv)) +
  geom_point(colour = "grey") +
  geom_smooth(method = "lm", se = FALSE, colour = "black") +
  labs(x = "Width (cm)", y = "Harvest Weight (g)") +
  theme_few()



# area x weight -----------------------------------------------------------
areaXweight <- ggplot(data = apple_data, aes(x = area_reml_lsmeans_predict, y = weight_avg_16_harv)) +
  geom_point(colour = "grey") +
  geom_smooth(method = "lm", se = FALSE, colour = "black") +
  labs(x = expression("Area (cm" ^ 2*")"), y = "Harvest Weight") +
  theme_few()



# plotting ----------------------------------------------------------------
plot2 <- plot_grid(widthXlength, widthXarea, lengthXarea, lengthXweight, widthXweight, areaXweight, ncol = 3, labels = c("a", "b", "c", "d", "e", "f"))

print(plot2)

ggsave("Figure2_draft.pdf", plot = plot3, units = "in", width = 16, height = 10)




## Figure 4
## plotting for 534 different apple_id with reml adjusted values
# area x PC1 -----------------------------------------------------
areaXPC1 <- ggplot(data = apple_data, aes(y = area_reml_lsmeans_predict, x = PC1_reml_lsmeans_predict, colour = aspect_ratio)) +
  geom_point() +
  scale_color_viridis(limits = c(0.8, 1.3)) +
  geom_smooth(method = "lm", se = FALSE, colour = "black") +
  labs(x = "PC1", y = expression("Area (cm" ^ 2*")"), colour = "Aspect Ratio") +
  theme_few()



# PC1 x aspect ratio ------------------------------------------------------
PC1Xaspectratio <- ggplot(data = apple_data, aes(x = PC1_reml_lsmeans_predict, y = aspect_ratio, colour = aspect_ratio)) +
  geom_point() +
  scale_color_viridis(limits = c(0.8, 1.3)) +
  geom_smooth(method = "lm", se = FALSE, colour = "black") +
  labs(x = "PC1", y = "Aspect Ratio", colour = "Aspect Ratio") +
  theme_few()



PC1Xharvestweight <- ggplot(data = apple_data, aes(x = PC1_reml_lsmeans_predict, y = weight_avg_16_harv, colour = aspect_ratio)) +
  geom_point() +
  scale_color_viridis(limits = c(0.8, 1.3)) +
  geom_smooth(method = "lm", se = FALSE, colour = "black") +
  labs(x = "PC1", y = "Harvest Weight (g)", colour = "Aspect Ratio") +
  theme_few()


# plotting ----------------------------------------------------------------
plot4 <- ggarrange(PC1Xaspectratio, areaXPC1, PC1Xharvestweight, ncol = 3, labels = c("a", "b", "c"), legend = "bottom", common.legend = TRUE)

print(plot4)

ggsave("Figure4_draft.pdf", plot = plot4, units = "in", width = 11, height = 4.5)
