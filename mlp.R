library(caret)
library(nnet)
library(ggplot2)
library(gridExtra)
pcaviz
# 读取数据
data1 <- read.csv("C:/Users/lin/Desktop/穿山甲神经网络模型/普式转换_KLFDAPC1-2.csv")

# 确保没有缺失值
data1 <- na.omit(data1)

# 再次检查缺失值
if (sum(is.na(data1)) > 0) {
  data1[is.na(data1)] <- lapply(data1[is.na(data1)], function(x) mean(x, na.rm = TRUE))
}

# 归一化数据
preProcValues <- preProcess(data1[, c("PC1", "PC2")], method = c("center", "scale"))
data1[, c("PC1", "PC2")] <- predict(preProcValues, data1[, c("PC1", "PC2")])

# 设置训练控制参数
train_control <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

# 设置预测变量和响应变量
x <- data1[, c("PC1", "PC2")]
y_lat <- data1$lat
y_long <- data1$long

tune_grid <- expand.grid(.size = c(1, 5, 10), .decay = c(0.0001, 0.001, 0.01))
# 调整后的参数网格

# 训练预测纬度的神经网络模型
model_lat <- train(x,
                   y_lat,
                   method = "mlpWeightDecay",
                   trControl = train_control,
                   tuneGrid = tune_grid,
                   linout = TRUE,
                   trace = FALSE,
                   maxit = 500
)
# 训练预测经度的神经网络模型
model_long <- train(x,
                    y_long,
                    method = "mlpWeightDecay",
                    trControl = train_control,
                    tuneGrid = tune_grid,
                    linout = TRUE,
                    trace = FALSE,
                    maxit = 500
)
summary(model_lat)
# 查看参数组合的性能指标1
print(model_lat$results)
# 根据RMSE值进行排序，找到表现最好的参数组合
best_model <- model_lat$results[which.min(model_lat$results$RMSE), ]
print(best_model)

str(model_lat)
# 查看参数组合的性能指标2
print(model_long$results)
# 根据RMSE值进行排序，找到表现最好的参数组合
best_model <- model_long$results[which.min(model_long$results$RMSE), ]
print(best_model)


# 结果比较
results <- resamples(list(mlp_lat = model_lat, mlp_long = model_long))
summary(results)
bwplot(results)
dotplot(results)

# 打印交叉验证结果的性能指标
results$values

R2(predict(model_lat),y_lat)

# 使用预测结果计算R^2和RMSE
pred_lat <- predict(model_lat, x)
pred_long <- predict(model_long, x)

#r2_lat <- R2(pred_lat, y_lat)
#r2_long <- R2(pred_long, y_long)

#rmse_lat <- RMSE(pred_lat, y_lat)
#rmse_long <- RMSE(pred_long, y_long)

#print(paste("R^2 for latitude prediction:", r2_lat))
#print(paste("R^2 for longitude prediction:", r2_long))
#print(paste("RMSE for latitude prediction:", rmse_lat))
#print(paste("RMSE for longitude prediction:", rmse_long))

# 创建数据框以便绘图
data_lat <- data.frame(True = y_lat, Predicted = pred_lat)
data_long <- data.frame(True = y_long, Predicted = pred_long)

# 绘制纬度的相关性图
plot_lat <- ggplot(data_lat, aes(x = True, y = Predicted)) +
  geom_point(color = "blue", shape = 1) +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  geom_smooth(method = "lm", se = FALSE, color = "darkgreen") +
  ggtitle("KLFDAPC:Correlation between True and Predicted Latitude") +
  xlab("True Latitude") +
  ylab("Predicted Latitude") +
  annotate("text", x = 13.5,
           y = 31.5,
           label = paste("R^2 =", round(r2_lat, 4), " RMSE =", round(rmse_lat, 4)),
           hjust = 0, vjust = 1, size = 4, color = "black") +
  theme_minimal() +
  coord_cartesian(xlim = c(min(data_lat$True, data_lat$Predicted), max(data_lat$True, data_lat$Predicted)),
                  ylim = c(min(data_lat$True, data_lat$Predicted), max(data_lat$True, data_lat$Predicted))) +
  theme(
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.line = element_line(size = 0.5, colour = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    plot.background = element_rect(fill = "white", color = NA)
  )

# 显示纬度的相关性图
print(plot_lat)
# 保存纬度的相关性图
ggsave("神经klfdapc_lat.png", plot = plot_lat, width = 8, height = 6)
ggsave("神经klfdapc_lat.pdf", plot_lat, width = 8, height = 6)
ggsave("神经klfdapc_lat.tiff", plot_lat, width = 8, height = 6)

# 绘制经度的相关性图
plot_long <- ggplot(data_long, aes(x = True, y = Predicted)) +
  geom_point(color = "blue", shape = 1) +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  geom_smooth(method = "lm", se = FALSE, color = "darkgreen") +
  ggtitle("KLFDAPC:Correlation between True and Predicted Longitude") +
  xlab("True Longitude") +
  ylab("Predicted Longitude") +
  annotate("text", x = 98,
           y = 121.5,
           label = paste("R^2 =", round(r2_long, 4), " RMSE =", round(rmse_long, 4)),
           hjust = 0, vjust = 1, size = 4, color = "black") +
  theme_minimal() +
  coord_cartesian(xlim = c(min(data_long$True, data_long$Predicted), max(data_long$True, data_long$Predicted)),
                  ylim = c(min(data_long$True, data_long$Predicted), max(data_long$True, data_long$Predicted))) +
  theme(
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.line = element_line(size = 0.5, colour = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    plot.background = element_rect(fill = "white", color = NA)
  )

# 显示经度的相关性图
print(plot_long)

# 保存经度的相关性图
ggsave("神经klfdapc_long.png", plot = plot_long, width = 8, height = 6)
ggsave("神经klfdapc_long.pdf", plot_long, width = 8, height = 6)
ggsave("神经klfdapc_long.tiff", plot_long, width = 8, height = 6)
getwd()
# 组合并保存两个图像为不同格式
combined_plot <- grid.arrange(plot_long, plot_lat, nrow = 2)
ggsave("神经klfdapc_combined_plot.png", plot = combined_plot, width = 8, height = 12)
ggsave("神经klfdapc_combined_plot.pdf", plot = combined_plot, width = 8, height = 12)
ggsave("神经klfdapc_combined_plot.tiff", plot = combined_plot, width = 8, height = 12)