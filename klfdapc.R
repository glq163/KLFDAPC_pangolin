library(sf)
library(tidyverse)
library(vegan)
library(PCAviz)
library(KLFDAPC)
library(SNPRelate)
library(readxl)
library(ggplot2)  
library(rnaturalearthdata)  


# 上传pc数据
# 创建结果保存文件夹
results_dir <- "results"
if(!dir.exists(results_dir)) {
  dir.create(results_dir)
}

pcadata <- read_excel("C:/Users/lin/Desktop/穿山甲/穿山甲KLFDAPC/数据R/pcadata.xls", sheet = "Sheet1")
print(class(pcadata))
str(pcadata)
head(pcadata)


# 提取群体代码
pop_code <- pcadata$`pop`
pop_code <- factor(pop_code,levels=unique(pop_code))
#pop_code <- factor(pcadata[[2]], levels = unique(pcadata[[2]]))
# 提取PCA结果
pca_matrix <- as.matrix(pcadata[, 3:ncol(pcadata)])

# 定义标准化函数
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
# 标准化PCA结果
pcanorm <- apply(pca_matrix, 2, normalize)

# 计算高斯核矩阵
kmat <- kmatrixGauss(pcanorm, sigma = 1.5)
# 进行KLFDA分析
klfdapc <- KLFDA(kmat, y = pop_code, r = 3, knn = 2)
print(klfdapc)
str(klfdapc)
print(colnames(klfdapc))
print(head(klfdapc))
summary(klfdapc)
# 导出 KLFDA 结果 (Z矩阵) 到 CSV 文件
write.csv(klfdapc$Z, file = file.path(results_dir, "klfdapc_results.csv"), row.names = FALSE)


# 绘制遗传结构图
custom_colors <- c("blue", "cyan", "red", "orange")

# 创建数据框用于ggplot2绘图
pca_data <- data.frame(
  PC1 = klfdapc$Z[,1],
  PC2 = klfdapc$Z[,2],
  pop = pop_code
)

# 计算主成分解释的方差百分比
pc_var_percent <- (klfdapc$d / sum(klfdapc$d)) * 100

# 创建更美观的PCA图
plot_pca <- function() {
  ggplot(pca_data, aes(x = PC1, y = PC2, color = pop, shape = pop)) +
    geom_point(size = 3, alpha = 0.7) +
    stat_ellipse(aes(fill = pop), geom = "polygon", alpha = 0.2, level = 0.95) +
    scale_color_manual(values = c("cyan", "blue", "red", "orange"), name = "群体") +
    scale_fill_manual(values = c("cyan", "blue", "red", "orange"), name = "群体") +
    scale_shape_manual(values = c(16, 17, 15, 18), name = "群体") +  # 圆点、三角形、方形和菱形
    labs(
      title = "Individuals - KLFDAPC", 
      x = paste0("PC1 (", round(pc_var_percent[1], 1), "%)"),
      y = paste0("PC2 (", round(pc_var_percent[2], 1), "%)")
    ) +
    theme_light() +
    theme(
      panel.grid.major = element_line(color = "gray90"),
      panel.grid.minor = element_line(color = "gray95"),
      plot.title = element_text(hjust = 0.5, size = 14),
      axis.title = element_text(size = 12),
      legend.title = element_text(size = 12),
      legend.position = "right"
    ) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray50")
}

#保存图片到结果文件夹
# Save as PNG
png(file = file.path(results_dir, "klfdapc_主成分分析图.png"), width = 20, height = 15, units = "cm", res = 300)
print(plot_pca())
dev.off()
# Save as PDF
pdf(file = file.path(results_dir, "klfdapc_主成分分析图.pdf"), width = 20/2.54, height = 15/2.54)  # Convert cm to inches for PDF
print(plot_pca())
dev.off()
# Save as TIFF
tiff(file = file.path(results_dir, "klfdapc_主成分分析图.tiff"), width = 20, height = 15, units = "cm", res = 300)
print(plot_pca())
dev.off()
getwd()


# 读取Excel文件中的地理坐标数据
Geo_ind <- read_excel("C:/Users/lin/Desktop/穿山甲/穿山甲KLFDAPC/数据R/geodata.xls", sheet = 1)
str(Geo_ind)
if(!all(c("long", "lat") %in% colnames(Geo_ind))) {
  stop("The data must contain 'long' and 'lat' columns")
}


# 使用protest函数进行地理和PCA空间的比较
protest_trans_sigma5 <- protest(Y = klfdapc$Z[, 1:2], X = Geo_ind[, c("long", "lat")], scores = "sites", permutations = 100000)
print(protest_trans_sigma5)
plot_pca <- function() {plot(protest_trans_sigma5$Yrot[, 1:2],col=custom_colors[as.integer(pop_code)], xlab="eigenvector 2", ylab="eigenvector 1")}

#保存图片到结果文件夹
# Save as PNG
png(file = file.path(results_dir, "klfdapc_普式转换图.png"), width = 15, height = 15, units = "cm", res = 300)
plot_pca()
dev.off()
# Save as PDF
pdf(file = file.path(results_dir, "klfdapc_普式转换图.pdf"), width = 15/2.54, height = 15/2.54)  # Convert cm to inches for PDF
plot_pca()
dev.off()
# Save as TIFF
tiff(file = file.path(results_dir, "klfdapc_普式转换图.tiff"), width = 15, height = 15, units = "cm", res = 300)
plot_pca()
dev.off()


#计算R方
protest_trans_sigma5 


# 数据准备和列名更改
KLFDAPC_geo_prot_5 <- as.data.frame(cbind(Geo_ind[, 1:4], as.data.frame(protest_trans_sigma5$Yrot)))
colnames(KLFDAPC_geo_prot_5)[5] <- "PC1"
colnames(KLFDAPC_geo_prot_5)[6] <- "PC2"
KLFDAPC_geo_sigma5=pcaviz(dat = KLFDAPC_geo_prot_5)

#计算相关性
cor.test(KLFDAPC_geo_prot_5$PC1,Geo_ind$long)
cor.test(KLFDAPC_geo_prot_5$PC2,Geo_ind$lat)
# 导出普式转换后的数据到CSV文件
output_file <- file.path(results_dir, "KLFDAPC_geo_prot.csv")
write.csv(KLFDAPC_geo_prot_5, file = output_file, row.names = FALSE)
# 打印数据框的列名和前几行
print(colnames(KLFDAPC_geo_prot_5))
print(head(KLFDAPC_geo_prot_5))

print(colnames(KLFDAPC_geo_sigma5$data))
print(head(KLFDAPC_geo_sigma5$data))

KLFDAPC_geo_sigma5$sdev["PC1"]= protest_trans_sigma5$svd$d[1]
KLFDAPC_geo_sigma5$sdev["PC2"]= protest_trans_sigma5$svd$d[2]
KLFDAPC_geo_sigma5$rotation = protest_trans_sigma5$rotation


summary(KLFDAPC_geo_sigma5)
print(colnames(protest_trans_sigma5$rotation))
print(head(protest_trans_sigma5$rotation))
print(head(KLFDAPC_geo_sigma5))

# 线性回归拟合
fit1 <- lm(long ~ PC1, KLFDAPC_geo_sigma5$data)
fit2 <- lm(lat ~ PC2, KLFDAPC_geo_sigma5$data)
mu1 <- coef(fit1)[["(Intercept)"]]
mu2 <- coef(fit2)[["(Intercept)"]]
b1 <- coef(fit1)[["PC1"]]
b2 <- coef(fit2)[["PC2"]]


# 基因空间数据的缩放和转换
#KLFDAPC_map_5 <- pcaviz_scale(KLFDAPC_geo_sigma5, scale = c(b1, b2), dims = c("PC1", "PC2")) %>%
 # pcaviz_translate(a = c(mu1, mu2), dims = c("PC1", "PC2"))
KLFDAPC_map_5 <- pcaviz_scale(KLFDAPC_geo_sigma5, scale = c(b1,b2),dims = c("PC1","PC2")) 
KLFDAPC_map_5 <- pcaviz_translate(KLFDAPC_map_5,a = c(mu1,mu2),dims = c("PC1","PC2"))

# 确保pop列存在于数据中
KLFDAPC_map_5$data$pop <- pop_code

# 正确使用列名而不是字符串引用
KLFDAPC_map_5 <- pcaviz_abbreviate_var(KLFDAPC_map_5, "pop")

# 确认数据
print(head(KLFDAPC_map_5$data))
print(colnames(KLFDAPC_map_5))
print(head(KLFDAPC_map_5))
summary(KLFDAPC_map_5)
cor.test(KLFDAPC_map_5$data$PC1,Geo_ind$long)
cor.test(KLFDAPC_map_5$data$PC2,Geo_ind$lat)
 

# 创建结果数据框，用于 ggplot
sample.id <- Geo_ind$`ID`
tab_protest <- data.frame(
  sample.id = sample.id,
  pop = pop_code,
  EV1 = KLFDAPC_geo_prot_5$PC1,
  EV2 = KLFDAPC_geo_prot_5$PC2,
  stringsAsFactors = FALSE
)
pop <- tab_protest$pop


# 读取地图的 GeoJSON 文件
china <- sf::st_read("C:/Users/lin/Desktop/穿山甲/穿山甲KLFDAPC/数据R/中华人民共和国.json")
southeast_asia <- st_read("C:/Users/lin/Desktop/穿山甲/穿山甲KLFDAPC/数据R/世界地图.json")

# 绘制中国及东南亚地图，并将 KLFDAPC 结果投影到地图上
best_china <- ggplot() +  # 不在ggplot()中传入数据
  geom_sf(data = china) +  # 使用geom_sf添加中国地图
  geom_sf(data = southeast_asia, fill = NA, color = "gray50", size = 0.1) +  # 添加东南亚地图

  geom_point(data = KLFDAPC_map_5$data, aes(x = PC1, y = PC2, col = pop), alpha = 0.7) +  # 绘制 KLFDAPC 结果点
  coord_sf(ylim = c(12.85, 31.336), xlim = c(97.2, 122)) +  # 设置坐标范围以包括东南亚
  #labs(title = "KLFDAPC Results on Geographic Map of China and Southeast Asia") + 
  #geom_text( aes(x=102.5, y=20), label="Correlation: 0.655",size=7, family="serif", col="#333300")+
  #geom_point(aes(x = KLFDAPC_map_5$data$PC1[20], y = KLFDAPC_map_5$data$PC2[20]),col="#333300",shape = 25,size=4,fill="#333300")+#MPA
  #geom_point(aes(x = KLFDAPC_map_5$data$PC1[75], y = KLFDAPC_map_5$data$PC2[75]),col="#333300",shape = 25,size=4,fill="#333300")+#MPB1
  #geom_point(aes(x = KLFDAPC_map_5$data$PC1[86], y = KLFDAPC_map_5$data$PC2[86]),col="#333300",shape = 25,size=4,fill="#333300")+#MPB2
  #geom_point(aes(x = KLFDAPC_map_5$data$PC1[50], y = KLFDAPC_map_5$data$PC2[50]),col="#333300",shape = 25,size=4,fill="#333300")+#MPC
  xlab(NULL) + ylab(NULL) +  # 去除坐标轴标签
  labs(col = "pop")  + # 设置图例标签 
scale_color_manual(values = c("cyan", "blue", "red", "orange")) 
# 显示图形
print(best_china)


# 保存绘图结果
ggsave(file.path(results_dir, "klfdapc_投影地图.png"), best_china, width = 15, height = 15, units = "cm")
ggsave(file.path(results_dir, "klfdapc_投影地图.tiff"), best_china, width = 15, height = 15, units = "cm")
ggsave(file.path(results_dir, "klfdapc_投影地图.pdf"), best_china, width = 15, height = 15, units = "cm")

##################################################################################
# 加载必要的R包
library(rnaturalearth)
library(ggplot2)
library(sf)
library(dplyr)  # 用于数据处理



# 获取中国省级行政区划的数据
china_provinces <- ne_states(country = "China", returnclass = "sf")

# 筛选出东南亚国家
southeast_asia_countries <- c("Vietnam", "Thailand", "Malaysia", "Indonesia", 
                              "Philippines", "Laos", "Cambodia", "Myanmar", 
                              "Singapore", "Brunei", "Timor-Leste","Taiwan")

# 获取东南亚国家的地图数据
southeast_asia_map <- ne_countries(scale = "medium", returnclass = "sf") %>%
  filter(name %in% southeast_asia_countries)

# 获取东南亚国家的名字及其中心点坐标
country_labels <- southeast_asia_map %>%
  st_centroid() %>%   # 计算几何中心
  st_coordinates() %>%  # 提取坐标
  as.data.frame() %>%
  mutate(name = southeast_asia_map$name)  # 添加国家名字

country_labels <- country_labels %>%
  mutate(
    X = case_when(
      name == "Laos" ~ X - 1,
      name == "Vietnam" ~ X - 0.7,
      name == "Thailand" ~ X + 0.2,
      name == "Myanmar" ~ X + 0.8,
      TRUE ~ X
    ),
    Y = case_when(
      name == "Laos" ~ Y + 1,
      name == "Vietnam" ~ Y + 5,
      name == "Thailand" ~ Y + 0.5,
      TRUE ~ Y
    )
  )


# 定义感兴趣区域的坐标范围 (经度和纬度)
xlim_range <- c(97.2, 122) # 经度范围
ylim_range <- c(12.85, 31.336)   # 纬度范围

# 创建一个地图对象，显示中国及东南亚国家的地图
china_southeast_asia_map <- ggplot() +
  geom_sf(data = southeast_asia_map, fill = "white", color = "#29484d", size = 0.3) +  # 东南亚国家地图
  geom_sf(data = china_provinces, fill = NA, color = "#29484d", size = 0.3) +  # 中国省级区划
  coord_sf(xlim = xlim_range, ylim = ylim_range, expand = FALSE) +  # 裁剪地图
  theme_minimal() +  # 使用简洁的主题
  ggtitle("KLFDAPC") +  # 设置标题
  theme(
    #panel.grid.major = element_line(color = "lightgray"),  # 设置网格线颜色
    #panel.background = element_rect(fill = "aliceblue"),  # 设置背景颜色
    plot.title = element_text(hjust = 0.5)  # 标题居中
  ) +
  geom_text(data = country_labels, aes(x = X, y = Y, label = name), size = 3, color = "#29484d", check_overlap = TRUE)  # 添加国家名字标签
  #geom_text( aes(x=102.5, y=20), label="Correlation: 0.655",size=7, family="serif", col="#333300")+
  #geom_point(aes(x = KLFDAPC_map_5$data$PC1[20], y = KLFDAPC_map_5$data$PC2[20]),col="#333300",shape = 25,size=4,fill="#333300")+#MPA
  #geom_point(aes(x = KLFDAPC_map_5$data$PC1[75], y = KLFDAPC_map_5$data$PC2[75]),col="#333300",shape = 25,size=4,fill="#333300")+#MPB1
  #geom_point(aes(x = KLFDAPC_map_5$data$PC1[86], y = KLFDAPC_map_5$data$PC2[86]),col="#333300",shape = 25,size=4,fill="#333300")+#MPB2
  #geom_point(aes(x = KLFDAPC_map_5$data$PC1[50], y = KLFDAPC_map_5$data$PC2[50]),col="#333300",shape = 25,size=4,fill="#333300")+#MPC
# 打印地图
print(china_southeast_asia_map)


# 在裁剪后的地图上叠加 KLFDAPC 结果
best_china1 <- china_southeast_asia_map +
  geom_point(data = KLFDAPC_map_5$data, aes(x = PC1, y = PC2, col = pop), alpha = 0.7) +  # 绘制 KLFDAPC 结果点
  labs(col = "pop") +  # 设置图例标签
  xlab(NULL) + ylab(NULL) +  # 去除坐标轴标签
  scale_color_manual(values = c("cyan", "blue", "red", "orange"))  # 自定义颜色
# 显示图形
print(best_china1)
# 保存绘图结果
ggsave(file.path(results_dir, "klfdapc_投影地图1.png"), best_china1, width = 15, height = 15, units = "cm")
ggsave(file.path(results_dir, "klfdapc_投影地图1.tiff"), best_china1, width = 15, height = 15, units = "cm")
ggsave(file.path(results_dir, "klfdapc_投影地图1.pdf"), best_china1, width = 15, height = 15, units = "cm")


#########################################################################################最终效果

china_southeast_asia_map <- ggplot() +
  geom_sf(data = southeast_asia_map, fill = "white", color = "#29484d", size = 0.3) +  # 东南亚国家地图
  geom_sf(data = china_provinces, fill = NA, color = "#29484d", size = 0.3) +  # 中国省级区划
  coord_sf(xlim = xlim_range, ylim = ylim_range, expand = FALSE) +  # 裁剪地图
  theme_minimal() +  # 使用简洁的主题
  ggtitle(NULL) +  # 设置标题
  #labs(caption = "Correlation:0.655") +  # 在底部添加标题
  theme(
    plot.title = element_text(hjust = 0.5),  # 标题居中
    plot.caption = element_text(hjust = 0.5, size = 14)  # 底部标题居中，调整大小
  ) +
  geom_text(data = country_labels, aes(x = X, y = Y, label = name), size = 1.6, color = "#29484d", check_overlap = TRUE)  # 添加国家名字标签

# 打印地图
print(china_southeast_asia_map)

# 叠加 KLFDAPC 结果
best_china1 <- china_southeast_asia_map +
  geom_point(data = KLFDAPC_map_5$data, aes(x = PC1, y = PC2, col = pop), alpha = 0.7) +  # 绘制 KLFDAPC 结果点
  labs(col = "pop", caption = NULL) +  # 设置图例标签和底部标题
  xlab(NULL) + ylab(NULL) +  # 去除坐标轴标签
  scale_color_manual(values = c("red", "blue", "cyan", "orange"))  # 自定义颜色

# 打印叠加结果
print(best_china1)
# 保存绘图结果
ggsave(file.path(results_dir, "klfdapc_投影地图3.png"), best_china1, width = 15, height = 15, units = "cm")
ggsave(file.path(results_dir, "klfdapc_投影地图3.tiff"), best_china1, width = 15, height = 15, units = "cm")
ggsave(file.path(results_dir, "klfdapc_投影地图3.pdf"), best_china1, width = 15, height = 15, units = "cm")

cat("所有结果已保存到", file.path(getwd(), results_dir), "文件夹\n")






