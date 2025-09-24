### 用PC结果投影

# 加载必要的包
library(ggplot2)
library(factoextra)

## PCA结果plot
sample.id <- Geo_ind$`ID`

# 准备数据
pca_data <- data.frame(
  PC1 = pcadata$PC1,
  PC2 = pcadata$PC2,
  Population = factor(pop_code),
  Label = 1:length(pcadata$PC1)  # 添加标签
)

# 计算解释方差百分比
pc1_var <- round(pcadata$PC1[1], 1)  # 第一主成分解释方差
pc2_var <- round(pcadata$PC2[1], 1)  # 第二主成分解释方差

# 创建更美观的PCA图
p <- ggplot(pca_data, aes(x = PC1, y = PC2, color = Population, shape = Population)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  stat_ellipse(aes(group = Population, fill = Population), geom = "polygon", alpha = 0.2, level = 0.999, type = "norm") +
  geom_point(size = 3, alpha = 0.8) +
  scale_color_manual(values = c("blue", "cyan", "red", "orange"),
                    labels = c("MPB1", "MPB2", "MPA", "MPC")) +
  scale_fill_manual(values = c("blue", "cyan", "red", "orange"),
                    labels = c("MPB1", "MPB2", "MPA", "MPC")) +
  scale_shape_manual(values = c(16, 17, 15, 18),  # 圆形、三角形、方形、菱形
                    labels = c("MPB1", "MPB2", "MPA", "MPC")) +
  theme_bw() +
  theme(
    panel.grid.major = element_line(color = "grey95"),
    panel.grid.minor = element_blank(),
    legend.position = "right",
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 14),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    panel.border = element_rect(color = "black", linewidth = 0.8)
  ) +
  labs(
    x = paste0("PC1"),
    y = paste0("PC2 "),
    color = "POP",
    shape = "POP",
    fill = "POP"
  )

# 保存高质量图片
ggsave(
  "C:/Users/lin/Desktop/穿山甲/穿山甲KLFDAPC/KLFDAPC_CODE/results/pc_主成分分析图.png",
  p,
  width = 54,
  height = 15,
  units = "cm",
  dpi = 600
)