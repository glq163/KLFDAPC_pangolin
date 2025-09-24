library(KLFDAPC)
library(PCAviz)
library(ggplot2)
library(cowplot)

##########################################PCA#########
KLFDAPC_geo_sigma5$data$pop=factor(KLFDAPC_geo_sigma5$data$pop,levels = unique(KLFDAPC_geo_sigma5$data$pop))
######
KLFDAPC_geo_sigma5$sdev["PC1"]= protest_trans_sigma5$svd$d[1]
KLFDAPC_geo_sigma5$sdev["PC2"]= protest_trans_sigma5$svd$d[2]
KLFDAPC_geo_sigma5$rotation = protest_trans_sigma5$rotation
KLFDAPC_geo_sigma5_y <- pcaviz_reduce_whitespace(KLFDAPC_geo_sigma5)
KLFDAPC_geo_sigma5_y <- pcaviz_rotate(KLFDAPC_geo_sigma5_y,25)
str(KLFDAPC_geo_sigma5_y)
print(KLFDAPC_geo_sigma5$data)
print(KLFDAPC_geo_sigma5_y)
# 计算PC1与经度的相关系数
pc1_long_cor <- cor(KLFDAPC_geo_sigma5_y$data$PC1, KLFDAPC_geo_sigma5_y$data$long)
pc1_long_r2 <- pc1_long_cor^2

# 计算PC2与经度的相关系数
pc2_long_cor <- cor(KLFDAPC_geo_sigma5_y$data$PC2, KLFDAPC_geo_sigma5_y$data$long)
pc2_long_r2 <- pc2_long_cor^2

plot3 <-
  plot(KLFDAPC_geo_sigma5_y,color = "long",shape = "pop",
       col = c("blue", "purple", "orange", "yellow"))+
  
  labs(col = "long",title = paste0("PCA1 vs. PCA2 (PC1 R² = ", round(pc1_long_r2, 3), ", PC2 R² = ", round(pc2_long_r2, 3), ")"))+
  theme(plot.title = element_text(hjust = 0.5,          #字体左右的位置
                                  vjust = 0.5),
        text=element_text(size= 10 ))+ #change font size of all text
  geom_point(x=KLFDAPC_geo_sigma5_y$data$PC1[17], y=KLFDAPC_geo_sigma5_y$data$PC2[17], color = "blue",alpha = 0.1,
            shape = 16 ,size = 9,stroke = 1.5,inherit.aes = FALSE)+
  geom_point(x=KLFDAPC_geo_sigma5_y$data$PC1[63], y=KLFDAPC_geo_sigma5_y$data$PC2[63], color= "purple",alpha = 0.1,
            shape = 16 ,size = 9,stroke = 1.5,inherit.aes = FALSE)+
  geom_point(x=KLFDAPC_geo_sigma5_y$data$PC1[56], y=KLFDAPC_geo_sigma5_y$data$PC2[56], color = "orange",alpha = 0.1,
           shape = 16 ,size = 9,stroke = 1.5,inherit.aes = FALSE)+
  geom_point(x= KLFDAPC_geo_sigma5_y$data$PC1[50], y= KLFDAPC_geo_sigma5_y$data$PC2[50], color = "purple",alpha = 0.1,
        shape = 16 ,size = 9,stroke = 1.5,inherit.aes = FALSE)+
  
  geom_text(aes(x = KLFDAPC_geo_sigma5_y$data$PC1[17], y = KLFDAPC_geo_sigma5_y$data$PC2[17], label = "MPA"), size = 2.8) +
  geom_text(aes(x = KLFDAPC_geo_sigma5_y$data$PC1[63], y = KLFDAPC_geo_sigma5_y$data$PC2[63], label = "MPB1"), size = 2.8) +
  geom_text(aes(x = KLFDAPC_geo_sigma5_y$data$PC1[56], y = KLFDAPC_geo_sigma5_y$data$PC2[56], label = "MPB2"), size = 2.8) +
  geom_text(aes(x = KLFDAPC_geo_sigma5_y$data$PC1[50], y = KLFDAPC_geo_sigma5_y$data$PC2[50], label = "MPC"), size = 2.8) 
  print(plot3)

  

  
# 计算PC1与纬度的相关系数
pc1_lat_cor <- cor(KLFDAPC_geo_sigma5_y$data$PC1, KLFDAPC_geo_sigma5_y$data$lat)
pc1_lat_r2 <- pc1_lat_cor^2

# 计算PC2与纬度的相关系数
pc2_lat_cor <- cor(KLFDAPC_geo_sigma5_y$data$PC2, KLFDAPC_geo_sigma5_y$data$lat)
pc2_lat_r2 <- pc2_lat_cor^2

plot4 <-
  plot(KLFDAPC_geo_sigma5_y,color = "lat",shape = "pop",
       col = c("blue", "purple", "orange", "yellow"))+
  
  labs(col = "lat",title = paste0("PCA1 vs. PCA2 (PC1 R² = ", round(pc1_lat_r2, 3), ", PC2 R² = ", round(pc2_lat_r2, 3), ")"))+
  theme(plot.title = element_text(hjust = 0.5,          #字体左右的位置
                                  vjust = 0.5),
        text=element_text(size= 10 ))+ #change font size of all text
  geom_point(x=KLFDAPC_geo_sigma5_y$data$PC1[17], y=KLFDAPC_geo_sigma5_y$data$PC2[17], color = "orange",alpha = 0.1,
             shape = 16 ,size = 9,stroke = 1.5,inherit.aes = FALSE)+
  geom_point(x=KLFDAPC_geo_sigma5_y$data$PC1[63], y=KLFDAPC_geo_sigma5_y$data$PC2[63], color= "orange",alpha = 0.1,
             shape = 16 ,size = 9,stroke = 1.5,inherit.aes = FALSE)+
  geom_point(x=KLFDAPC_geo_sigma5_y$data$PC1[56], y=KLFDAPC_geo_sigma5_y$data$PC2[56], color = "orange",alpha = 0.1,
             shape = 16 ,size = 9,stroke = 1.5,inherit.aes = FALSE)+
  geom_point(x= KLFDAPC_geo_sigma5_y$data$PC1[50], y= KLFDAPC_geo_sigma5_y$data$PC2[50], color = "purple",alpha = 0.1,
             shape = 16 ,size = 9,stroke = 1.5,inherit.aes = FALSE)+
  
  geom_text(aes(x = KLFDAPC_geo_sigma5_y$data$PC1[17], y = KLFDAPC_geo_sigma5_y$data$PC2[17], label = "MPA"), size = 2.8) +
  geom_text(aes(x = KLFDAPC_geo_sigma5_y$data$PC1[63], y = KLFDAPC_geo_sigma5_y$data$PC2[63], label = "MPB1"), size = 2.8) +
  geom_text(aes(x = KLFDAPC_geo_sigma5_y$data$PC1[56], y = KLFDAPC_geo_sigma5_y$data$PC2[56], label = "MPB2"), size = 2.8) +
  geom_text(aes(x = KLFDAPC_geo_sigma5_y$data$PC1[50], y = KLFDAPC_geo_sigma5_y$data$PC2[50], label = "MPC"), size = 2.8) 
print(plot4)

plot34 <- plot_grid(plot3,plot4,labels = c("A","B"),align = 'h')
plot34



ggsave("long vs lat_klfdapc1.png", plot3, width = 15, height = 15, units = "cm")
ggsave("long vs lat_klfdapc1.tiff", plot3, width = 15, height = 15, units = "cm")
ggsave("long vs lat_klfdapc1.pdf", plot3, width = 15, height = 15, units = "cm")

ggsave("pca1 vs pca2_梯度图.png", plot34, width = 30, height = 15, units = "cm")
ggsave("pca1 vs pca2_梯度图.tiff", plot34, width = 30, height = 15, units = "cm")
ggsave("pca1 vs pca2_梯度图.pdf", plot34, width = 30, height = 15, units = "cm")





##########################################KLFDAPC############################
KLFDAPC_geo_sigma5$data$pop=factor(KLFDAPC_geo_sigma5$data$pop,levels = unique(KLFDAPC_geo_sigma5$data$pop))
######
KLFDAPC_geo_sigma5$sdev["PC1"]= protest_trans_sigma5$svd$d[1]
KLFDAPC_geo_sigma5$sdev["PC2"]= protest_trans_sigma5$svd$d[2]
KLFDAPC_geo_sigma5$rotation = protest_trans_sigma5$rotation
KLFDAPC_geo_sigma5_y <- pcaviz_reduce_whitespace(KLFDAPC_geo_sigma5)
KLFDAPC_geo_sigma5_y <- pcaviz_rotate(KLFDAPC_geo_sigma5_y,25)
str(KLFDAPC_geo_sigma5_y)
print(KLFDAPC_geo_sigma5$data)
print(KLFDAPC_geo_sigma5_y)
 
# 计算KLFDAPC1与经度的相关系数
klfdapc1_long_cor <- cor(KLFDAPC_geo_sigma5_y$data$PC1, KLFDAPC_geo_sigma5_y$data$long)
klfdapc1_long_r2 <- klfdapc1_long_cor^2

# 计算KLFDAPC2与经度的相关系数
klfdapc2_long_cor <- cor(KLFDAPC_geo_sigma5_y$data$PC2, KLFDAPC_geo_sigma5_y$data$long)
klfdapc2_long_r2 <- klfdapc2_long_cor^2

plot5 <-
  plot(KLFDAPC_geo_sigma5_y,color = "long",shape = "pop",
       col = c("blue", "purple", "orange", "yellow"))+
  
  labs(col = "long",title = paste0("KLFDAPC1 vs. KLFDAPC2 (KLFDAPC1 R² = ", round(klfdapc1_long_r2, 3), ", KLFDAPC2 R² = ", round(klfdapc2_long_r2, 3), ")"))+
  theme(plot.title = element_text(hjust = 0.5,          #字体左右的位置
                                  vjust = 0.5),
        text=element_text(size= 10 ))+ #change font size of all text
  geom_point(x=KLFDAPC_geo_sigma5_y$data$PC1[17], y=KLFDAPC_geo_sigma5_y$data$PC2[17], color = "blue",alpha = 0.1,
             shape = 16 ,size = 9,stroke = 1.5,inherit.aes = FALSE)+
  geom_point(x=KLFDAPC_geo_sigma5_y$data$PC1[63], y=KLFDAPC_geo_sigma5_y$data$PC2[63], color= "purple",alpha = 0.1,
             shape = 16 ,size = 9,stroke = 1.5,inherit.aes = FALSE)+
  geom_point(x=KLFDAPC_geo_sigma5_y$data$PC1[56], y=KLFDAPC_geo_sigma5_y$data$PC2[56], color = "orange",alpha = 0.1,
             shape = 16 ,size = 9,stroke = 1.5,inherit.aes = FALSE)+
  geom_point(x= KLFDAPC_geo_sigma5_y$data$PC1[50], y= KLFDAPC_geo_sigma5_y$data$PC2[50], color = "purple",alpha = 0.1,
             shape = 16 ,size = 9,stroke = 1.5,inherit.aes = FALSE)+
  
  geom_text(aes(x = KLFDAPC_geo_sigma5_y$data$PC1[17], y = KLFDAPC_geo_sigma5_y$data$PC2[17], label = "MPA"), size = 2.8) +
  geom_text(aes(x = KLFDAPC_geo_sigma5_y$data$PC1[63], y = KLFDAPC_geo_sigma5_y$data$PC2[63], label = "MPB1"), size = 2.8) +
  geom_text(aes(x = KLFDAPC_geo_sigma5_y$data$PC1[56], y = KLFDAPC_geo_sigma5_y$data$PC2[56], label = "MPB2"), size = 2.8) +
  geom_text(aes(x = KLFDAPC_geo_sigma5_y$data$PC1[50], y = KLFDAPC_geo_sigma5_y$data$PC2[50], label = "MPC"), size = 2.8) 
print(plot5)




# 计算KLFDAPC1与纬度的相关系数
klfdapc1_lat_cor <- cor(KLFDAPC_geo_sigma5_y$data$PC1, KLFDAPC_geo_sigma5_y$data$lat)
klfdapc1_lat_r2 <- klfdapc1_lat_cor^2

# 计算KLFDAPC2与纬度的相关系数
klfdapc2_lat_cor <- cor(KLFDAPC_geo_sigma5_y$data$PC2, KLFDAPC_geo_sigma5_y$data$lat)
klfdapc2_lat_r2 <- klfdapc2_lat_cor^2

plot6 <-
  plot(KLFDAPC_geo_sigma5_y,color = "lat",shape = "pop",
       col = c("blue", "purple", "orange", "yellow"))+
  
  labs(col = "lat",title = paste0("KLFDAPC1 vs. KLFDAPC2 (KLFDAPC1 R² = ", round(klfdapc1_lat_r2, 3), ", KLFDAPC2 R² = ", round(klfdapc2_lat_r2, 3), ")"))+
  theme(plot.title = element_text(hjust = 0.5,          #字体左右的位置
                                  vjust = 0.5),
        text=element_text(size= 10 ))+ #change font size of all text
  geom_point(x=KLFDAPC_geo_sigma5_y$data$PC1[17], y=KLFDAPC_geo_sigma5_y$data$PC2[17], color = "orange",alpha = 0.1,
             shape = 16 ,size = 9,stroke = 1.5,inherit.aes = FALSE)+
  geom_point(x=KLFDAPC_geo_sigma5_y$data$PC1[63], y=KLFDAPC_geo_sigma5_y$data$PC2[63], color= "orange",alpha = 0.1,
             shape = 16 ,size = 9,stroke = 1.5,inherit.aes = FALSE)+
  geom_point(x=KLFDAPC_geo_sigma5_y$data$PC1[56], y=KLFDAPC_geo_sigma5_y$data$PC2[56], color = "orange",alpha = 0.1,
             shape = 16 ,size = 9,stroke = 1.5,inherit.aes = FALSE)+
  geom_point(x= KLFDAPC_geo_sigma5_y$data$PC1[50], y= KLFDAPC_geo_sigma5_y$data$PC2[50], color = "purple",alpha = 0.1,
             shape = 16 ,size = 9,stroke = 1.5,inherit.aes = FALSE)+
  
  geom_text(aes(x = KLFDAPC_geo_sigma5_y$data$PC1[17], y = KLFDAPC_geo_sigma5_y$data$PC2[17], label = "MPA"), size = 2.8) +
  geom_text(aes(x = KLFDAPC_geo_sigma5_y$data$PC1[63], y = KLFDAPC_geo_sigma5_y$data$PC2[63], label = "MPB1"), size = 2.8) +
  geom_text(aes(x = KLFDAPC_geo_sigma5_y$data$PC1[56], y = KLFDAPC_geo_sigma5_y$data$PC2[56], label = "MPB2"), size = 2.8) +
  geom_text(aes(x = KLFDAPC_geo_sigma5_y$data$PC1[50], y = KLFDAPC_geo_sigma5_y$data$PC2[50], label = "MPC"), size = 2.8) 
print(plot6)

plot56 <- plot_grid(plot5,plot6,labels = c("C","D"),align = 'h')
plot56



ggsave("long vs lat_klfdapc1.png", plot3, width = 15, height = 15, units = "cm")
ggsave("long vs lat_klfdapc1.tiff", plot3, width = 15, height = 15, units = "cm")
ggsave("long vs lat_klfdapc1.pdf", plot3, width = 15, height = 15, units = "cm")

ggsave("klfdapc1 vs klfdapc2_梯度图.png", plot56, width = 30, height = 15, units = "cm")
ggsave("klfdapc1 vs klfdapc2_梯度图.tiff", plot56, width = 30, height = 15, units = "cm")
ggsave("klfdapc1 vs klfdapc2_梯度图.pdf", plot56, width = 30, height = 15, units = "cm")

# 打印所有R²值总结
cat("=== R² 值总结 ===\n")
cat("PC1 vs 经度: R² =", round(pc1_long_r2, 4), "\n")
cat("PC2 vs 经度: R² =", round(pc2_long_r2, 4), "\n")
cat("PC1 vs 纬度: R² =", round(pc1_lat_r2, 4), "\n")
cat("PC2 vs 纬度: R² =", round(pc2_lat_r2, 4), "\n")
cat("KLFDAPC1 vs 经度: R² =", round(klfdapc1_long_r2, 4), "\n")
cat("KLFDAPC2 vs 经度: R² =", round(klfdapc2_long_r2, 4), "\n")
cat("KLFDAPC1 vs 纬度: R² =", round(klfdapc1_lat_r2, 4), "\n")
cat("KLFDAPC2 vs 纬度: R² =", round(klfdapc2_lat_r2, 4), "\n")
cat("==================\n")

