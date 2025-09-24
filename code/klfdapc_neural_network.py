import pandas as pd
import numpy as np
import torch
import torch.nn as nn
from torch.utils.data import Dataset, DataLoader
from sklearn.model_selection import train_test_split
from sklearn.model_selection import KFold
from sklearn.preprocessing import StandardScaler
import matplotlib.pyplot as plt
import matplotlib.lines as mlines
import os
import seaborn as sns
from sklearn.metrics import r2_score, mean_squared_error, mean_absolute_error

# 创建保存结果的目录
results_dir = 'klfdapc_results'
if not os.path.exists(results_dir):
    os.makedirs(results_dir)

# 设置随机种子
torch.manual_seed(42)
np.random.seed(42)

# 数据集类
class GeoDataset(Dataset):
    def __init__(self, X, y):
        self.X = torch.FloatTensor(X)
        self.y = torch.FloatTensor(y)
    def __len__(self):
        return len(self.X)
    def __getitem__(self, idx):
        return self.X[idx], self.y[idx]

# 神经网络模型
class ImprovedGeoNet(nn.Module):
    def __init__(self, input_size=2):
        super(ImprovedGeoNet, self).__init__()
        self.network = nn.Sequential(
            nn.Linear(input_size, 128),
            nn.BatchNorm1d(128),
            nn.LeakyReLU(0.1),
            nn.Dropout(0.3),
            nn.Linear(128, 128),
            nn.BatchNorm1d(128),
            nn.LeakyReLU(0.1),
            nn.Dropout(0.3),
            nn.Linear(128, 64),
            nn.BatchNorm1d(64),
            nn.LeakyReLU(0.1),
            nn.Dropout(0.2),
            nn.Linear(64, 32),
            nn.BatchNorm1d(32),
            nn.LeakyReLU(0.1),
            nn.Dropout(0.2),
            nn.Linear(32, 16),
            nn.BatchNorm1d(16),
            nn.LeakyReLU(0.1),
            nn.Dropout(0.2),
            nn.Linear(16, 1)
        )
    def forward(self, x):
        return self.network(x)

# 读取数据
df = pd.read_csv('普式转换_KLFDAPC1-2.csv')
# 只用PC1预测经度
X_long = df[['PC1', 'PC2']].values
X_lat = df[['PC1','PC2']].values          # 仍然只用PC2预测纬度
y_long = df['long'].values.reshape(-1, 1)
y_lat = df['lat'].values.reshape(-1, 1)

# 分割数据集（先不做标准化，避免数据泄漏）
X_long_train, X_long_test, y_long_train, y_long_test = train_test_split(
    X_long, y_long, test_size=0.2, random_state=42)
X_lat_train, X_lat_test, y_lat_train, y_lat_test = train_test_split(
    X_lat, y_lat, test_size=0.2, random_state=42)

# 数据标准化（仅在训练集上fit，再transform到测试集）
scaler_X_long = StandardScaler()
scaler_X_lat = StandardScaler()
scaler_y_long = StandardScaler()
scaler_y_lat = StandardScaler()

X_long_train = scaler_X_long.fit_transform(X_long_train)
X_long_test = scaler_X_long.transform(X_long_test)
X_lat_train = scaler_X_lat.fit_transform(X_lat_train)
X_lat_test = scaler_X_lat.transform(X_lat_test)
y_long_train = scaler_y_long.fit_transform(y_long_train)
y_long_test = scaler_y_long.transform(y_long_test)
y_lat_train = scaler_y_lat.fit_transform(y_lat_train)
y_lat_test = scaler_y_lat.transform(y_lat_test)

# 数据加载器
train_dataset_long = GeoDataset(X_long_train, y_long_train)
test_dataset_long = GeoDataset(X_long_test, y_long_test)
train_dataset_lat = GeoDataset(X_lat_train, y_lat_train)
test_dataset_lat = GeoDataset(X_lat_test, y_lat_test)

train_loader_long = DataLoader(train_dataset_long, batch_size=16, shuffle=True)
test_loader_long = DataLoader(test_dataset_long, batch_size=16, shuffle=False)
train_loader_lat = DataLoader(train_dataset_lat, batch_size=16, shuffle=True)
test_loader_lat = DataLoader(test_dataset_lat, batch_size=16, shuffle=False)

# 初始化模型、损失函数和优化器
device = torch.device('cuda' if torch.cuda.is_available() else 'cpu')
model_long = ImprovedGeoNet(input_size=2).to(device)
model_lat = ImprovedGeoNet(input_size=2).to(device)
criterion = nn.MSELoss()
optimizer_long = torch.optim.AdamW(model_long.parameters(), lr=0.001, weight_decay=0.01)
optimizer_lat = torch.optim.AdamW(model_lat.parameters(), lr=0.001, weight_decay=0.01)
scheduler_long = torch.optim.lr_scheduler.ReduceLROnPlateau(optimizer_long, mode='min', factor=0.5, patience=10)
scheduler_lat = torch.optim.lr_scheduler.ReduceLROnPlateau(optimizer_lat, mode='min', factor=0.5, patience=10)

# 训练模型
num_epochs = 200
train_losses_long = []
test_losses_long = []
train_losses_lat = []
test_losses_lat = []

with open(os.path.join(results_dir, 'klfdapc_training_log.txt'), 'w', encoding='utf-8') as f:
    f.write("训练日志\n")
    f.write("="*50 + "\n")

best_test_loss_long = float('inf')
best_test_loss_lat = float('inf')
patience = 20
patience_counter_long = 0
patience_counter_lat = 0

for epoch in range(num_epochs):
    # 训练经度模型
    model_long.train()
    train_loss_long = 0
    for X_batch, y_batch in train_loader_long:
        X_batch, y_batch = X_batch.to(device), y_batch.to(device)
        optimizer_long.zero_grad()
        outputs = model_long(X_batch)
        loss = criterion(outputs, y_batch)
        loss.backward()
        optimizer_long.step()
        train_loss_long += loss.item()
    # 训练纬度模型
    model_lat.train()
    train_loss_lat = 0
    for X_batch, y_batch in train_loader_lat:
        X_batch, y_batch = X_batch.to(device), y_batch.to(device)
        optimizer_lat.zero_grad()
        outputs = model_lat(X_batch)
        loss = criterion(outputs, y_batch)
        loss.backward()
        optimizer_lat.step()
        train_loss_lat += loss.item()
    # 验证
    model_long.eval()
    model_lat.eval()
    test_loss_long = 0
    test_loss_lat = 0
    with torch.no_grad():
        for X_batch, y_batch in test_loader_long:
            X_batch, y_batch = X_batch.to(device), y_batch.to(device)
            outputs = model_long(X_batch)
            test_loss_long += criterion(outputs, y_batch).item()
        for X_batch, y_batch in test_loader_lat:
            X_batch, y_batch = X_batch.to(device), y_batch.to(device)
            outputs = model_lat(X_batch)
            test_loss_lat += criterion(outputs, y_batch).item()
    avg_train_loss_long = train_loss_long / len(train_loader_long)
    avg_test_loss_long = test_loss_long / len(test_loader_long)
    avg_train_loss_lat = train_loss_lat / len(train_loader_lat)
    avg_test_loss_lat = test_loss_lat / len(test_loader_lat)
    scheduler_long.step(avg_test_loss_long)
    scheduler_lat.step(avg_test_loss_lat)
    if avg_test_loss_long < best_test_loss_long:
        best_test_loss_long = avg_test_loss_long
        patience_counter_long = 0
        torch.save(model_long.state_dict(), os.path.join(results_dir, 'klfdapc_best_model_long.pth'))
    else:
        patience_counter_long += 1
    if avg_test_loss_lat < best_test_loss_lat:
        best_test_loss_lat = avg_test_loss_lat
        patience_counter_lat = 0
        torch.save(model_lat.state_dict(), os.path.join(results_dir, 'klfdapc_best_model_lat.pth'))
    else:
        patience_counter_lat += 1
    train_losses_long.append(avg_train_loss_long)
    test_losses_long.append(avg_test_loss_long)
    train_losses_lat.append(avg_train_loss_lat)
    test_losses_lat.append(avg_test_loss_lat)
    if (epoch + 1) % 10 == 0:
        log_message = f'Epoch [{epoch+1}/{num_epochs}]\n'
        log_message += f'经度模型 - Train Loss: {avg_train_loss_long:.4f}, Test Loss: {avg_test_loss_long:.4f}\n'
        log_message += f'纬度模型 - Train Loss: {avg_train_loss_lat:.4f}, Test Loss: {avg_test_loss_lat:.4f}'
        print(log_message)
        with open(os.path.join(results_dir, 'klfdapc_training_log.txt'), 'a', encoding='utf-8') as f:
            f.write(log_message + '\n\n')
    if patience_counter_long >= patience and patience_counter_lat >= patience:
        print(f'Early stopping at epoch {epoch+1}')
        break

# 加载最佳模型
model_long.load_state_dict(torch.load(os.path.join(results_dir, 'klfdapc_best_model_long.pth')))
model_lat.load_state_dict(torch.load(os.path.join(results_dir, 'klfdapc_best_model_lat.pth')))

# 评估
model_long.eval()
model_lat.eval()
with torch.no_grad():
    y_long_pred_scaled = model_long(torch.FloatTensor(X_long_test).to(device)).cpu().numpy()
    y_long_pred = scaler_y_long.inverse_transform(y_long_pred_scaled)
    y_long_test_original = scaler_y_long.inverse_transform(y_long_test)
    y_lat_pred_scaled = model_lat(torch.FloatTensor(X_lat_test).to(device)).cpu().numpy()
    y_lat_pred = scaler_y_lat.inverse_transform(y_lat_pred_scaled)
    y_lat_test_original = scaler_y_lat.inverse_transform(y_lat_test)

r2_long = r2_score(y_long_test_original, y_long_pred)
r2_lat = r2_score(y_lat_test_original, y_lat_pred)
rmse_long = np.sqrt(mean_squared_error(y_long_test_original, y_long_pred))
rmse_lat = np.sqrt(mean_squared_error(y_lat_test_original, y_lat_pred))
mae_long = mean_absolute_error(y_long_test_original, y_long_pred)
mae_lat = mean_absolute_error(y_lat_test_original, y_lat_pred)
bias_long = float(np.mean(y_long_pred - y_long_test_original))
bias_lat = float(np.mean(y_lat_pred - y_lat_test_original))

# 自助法（bootstrap）计算置信区间
def bootstrap_ci(y_true, y_pred, metric_fn, n_boot=1000, alpha=0.05, random_state=42):
    rng = np.random.default_rng(random_state)
    n = len(y_true)
    stats = []
    for _ in range(n_boot):
        idx = rng.integers(0, n, n)
        yt = y_true[idx]
        yp = y_pred[idx]
        stats.append(metric_fn(yt, yp))
    stats = np.asarray(stats)
    lower = np.quantile(stats, alpha/2)
    upper = np.quantile(stats, 1 - alpha/2)
    return float(lower), float(upper)

# 计算R2、RMSE、MAE的95%CI
rmse = lambda yt, yp: float(np.sqrt(mean_squared_error(yt, yp)))
r2_long_ci = bootstrap_ci(y_long_test_original.flatten(), y_long_pred.flatten(), lambda yt, yp: r2_score(yt, yp))
r2_lat_ci = bootstrap_ci(y_lat_test_original.flatten(), y_lat_pred.flatten(), lambda yt, yp: r2_score(yt, yp))
rmse_long_ci = bootstrap_ci(y_long_test_original.flatten(), y_long_pred.flatten(), rmse)
rmse_lat_ci = bootstrap_ci(y_lat_test_original.flatten(), y_lat_pred.flatten(), rmse)
mae_long_ci = bootstrap_ci(y_long_test_original.flatten(), y_long_pred.flatten(), lambda yt, yp: mean_absolute_error(yt, yp))
mae_lat_ci = bootstrap_ci(y_lat_test_original.flatten(), y_lat_pred.flatten(), lambda yt, yp: mean_absolute_error(yt, yp))

with open(os.path.join(results_dir, 'klfdapc_model_performance.txt'), 'w', encoding='utf-8') as f:
    f.write("KLFDAPC模型性能评估\n")
    f.write("="*50 + "\n")
    f.write("经度模型 (KLFDAPC1预测经度):\n")
    f.write(f"R2: {r2_long:.4f}\n")
    f.write(f"R2 95%CI: [{r2_long_ci[0]:.4f}, {r2_long_ci[1]:.4f}]\n")
    f.write(f"RMSE: {rmse_long:.4f}\n")
    f.write(f"RMSE 95%CI: [{rmse_long_ci[0]:.4f}, {rmse_long_ci[1]:.4f}]\n")
    f.write(f"MAE: {mae_long:.4f}\n")
    f.write(f"MAE 95%CI: [{mae_long_ci[0]:.4f}, {mae_long_ci[1]:.4f}]\n")
    f.write(f"Bias(预测-真实): {bias_long:.4f}\n\n")
    f.write("纬度模型 (KLFDAPC2预测纬度):\n")
    f.write(f"R2: {r2_lat:.4f}\n")
    f.write(f"R2 95%CI: [{r2_lat_ci[0]:.4f}, {r2_lat_ci[1]:.4f}]\n")
    f.write(f"RMSE: {rmse_lat:.4f}\n")
    f.write(f"RMSE 95%CI: [{rmse_lat_ci[0]:.4f}, {rmse_lat_ci[1]:.4f}]\n")
    f.write(f"MAE: {mae_lat:.4f}\n")
    f.write(f"MAE 95%CI: [{mae_lat_ci[0]:.4f}, {mae_lat_ci[1]:.4f}]\n")
    f.write(f"Bias(预测-真实): {bias_lat:.4f}\n")

print(f"\nKLFDAPC模型性能评估:")
print("经度模型 (KLFDAPC1预测经度):")
print(f"R2: {r2_long:.4f}")
print(f"RMSE: {rmse_long:.4f} | MAE: {mae_long:.4f} | Bias: {bias_long:.4f}")
print("\n纬度模型 (KLFDAPC2预测纬度):")
print(f"R2: {r2_lat:.4f}")
print(f"RMSE: {rmse_lat:.4f} | MAE: {mae_lat:.4f} | Bias: {bias_lat:.4f}")

# 保存预测结果
results_df = pd.DataFrame({
    '真实经度': y_long_test_original.flatten(),
    '预测经度': y_long_pred.flatten(),
    '真实纬度': y_lat_test_original.flatten(),
    '预测纬度': y_lat_pred.flatten()
})
results_df.to_csv(os.path.join(results_dir, 'klfdapc_prediction_results.csv'), index=False, encoding='utf-8')

# 绘制损失曲线
plt.rcParams['font.sans-serif'] = ['SimHei']  # 显示中文
plt.rcParams['axes.unicode_minus'] = False    # 显示负号
plt.style.use('seaborn-v0_8-whitegrid')
sns.set_context("talk")
plt.rcParams['font.family'] = ['SimHei', 'DejaVu Sans']
plt.rcParams['font.sans-serif'] = ['SimHei', 'DejaVu Sans']

plt.figure(figsize=(12, 5))
palette = sns.color_palette("Set2", 2)
plt.subplot(1, 2, 1)
plt.plot(train_losses_long, label='训练损失', color=palette[0], linewidth=2)
plt.plot(test_losses_long, label='测试损失', color=palette[1], linewidth=2)
plt.xlabel('Epoch', fontsize=15, fontweight='bold')
plt.ylabel('Loss', fontsize=15, fontweight='bold')
plt.tick_params(axis='both', which='major', labelsize=13)
ax = plt.gca()
for label in ax.get_xticklabels() + ax.get_yticklabels():
    label.set_fontweight('bold')
plt.grid(True, alpha=0.3)
legend1 = plt.legend(frameon=True, fancybox=True, framealpha=0.9, loc='upper left', borderpad=0.6)
plt.subplot(1, 2, 2)
plt.plot(train_losses_lat, label='训练损失', color=palette[0], linewidth=2)
plt.plot(test_losses_lat, label='测试损失', color=palette[1], linewidth=2)
plt.xlabel('Epoch', fontsize=15, fontweight='bold')
plt.ylabel('Loss', fontsize=15, fontweight='bold')
plt.tick_params(axis='both', which='major', labelsize=13)
ax = plt.gca()
for label in ax.get_xticklabels() + ax.get_yticklabels():
    label.set_fontweight('bold')
plt.grid(True, alpha=0.3)
legend2 = plt.legend(frameon=True, fancybox=True, framealpha=0.9, loc='upper left', borderpad=0.6)
plt.tight_layout()
plt.savefig(os.path.join(results_dir, 'klfdapc_loss_curves.png'), dpi=200, bbox_inches='tight')
plt.close()

# 绘制预测结果散点图（美化版）
plt.figure(figsize=(16, 7))
sns.set(style="whitegrid", font_scale=1.2)
plt.rcParams['font.family'] = ['SimHei', 'DejaVu Sans']
plt.rcParams['font.sans-serif'] = ['SimHei', 'DejaVu Sans']
plt.subplot(1, 2, 1)
blue = sns.color_palette("Set2")[0]
red = sns.color_palette("Set2")[1]
true_long = y_long_test_original.flatten()
pred_long = y_long_pred.flatten()
true_lat = y_lat_test_original.flatten()
pred_lat = y_lat_pred.flatten()

sns.regplot(x=true_long, y=pred_long,
            scatter_kws={'color': blue, 's': 40, 'alpha': 0.9, 'edgecolor': '#333333', 'linewidths': 0.5},
            line_kws={'color': blue, 'linewidth': 2},
            ci=95)
lims = [min(true_long.min(), pred_long.min()), max(true_long.max(), pred_long.max())]
plt.plot(lims, lims, linestyle='--', color='#666666', linewidth=1.5)
plt.xlim(lims)
plt.ylim(lims)
plt.gca().set_aspect('equal', adjustable='box')
plt.xlabel('True Longitude', fontsize=15, fontweight='bold')
plt.ylabel('Predicted Longitude', fontsize=15, fontweight='bold')
plt.tick_params(axis='both', which='major', labelsize=13)
ax = plt.gca()
for label in ax.get_xticklabels() + ax.get_yticklabels():
    label.set_fontweight('bold')
plt.text(0.02, 0.98, f'R² = {r2_long:.2f}\nRMSE = {rmse_long:.2f}',
         transform=plt.gca().transAxes, fontsize=14, color=blue, va='top', ha='left', bbox=dict(boxstyle='round,pad=0.3', facecolor='white', alpha=0.7, edgecolor=blue))
plt.subplot(1, 2, 2)
sns.regplot(x=true_lat, y=pred_lat,
            scatter_kws={'color': red, 's': 40, 'alpha': 0.9, 'edgecolor': '#333333', 'linewidths': 0.5},
            line_kws={'color': red, 'linewidth': 2},
            ci=95)
lims = [min(true_lat.min(), pred_lat.min()), max(true_lat.max(), pred_lat.max())]
plt.plot(lims, lims, linestyle='--', color='#666666', linewidth=1.5)
plt.xlim(lims)
plt.ylim(lims)
plt.gca().set_aspect('equal', adjustable='box')
plt.xlabel('True Latitude', fontsize=15, fontweight='bold')
plt.ylabel('Predicted Latitude', fontsize=15, fontweight='bold')
plt.tick_params(axis='both', which='major', labelsize=13)
ax = plt.gca()
for label in ax.get_xticklabels() + ax.get_yticklabels():
    label.set_fontweight('bold')
plt.text(0.02, 0.98, f'R² = {r2_lat:.2f}\nRMSE = {rmse_lat:.2f}',
         transform=plt.gca().transAxes, fontsize=14, color=red, va='top', ha='left', bbox=dict(boxstyle='round,pad=0.3', facecolor='white', alpha=0.7, edgecolor=red))
plt.tight_layout()
plt.savefig(os.path.join(results_dir, 'klfdapc_nn_predictions.png'), dpi=300, bbox_inches='tight')
plt.close() 

# ======================
# 5 折交叉验证（经度/纬度）
# ======================
kf = KFold(n_splits=5, shuffle=True, random_state=42)

def run_fold_cv(X_all, y_all, label_name):
    r2_list = []
    rmse_list = []
    mae_list = []
    fold_idx = 1
    for train_idx, test_idx in kf.split(X_all, y_all):
        scaler_X = StandardScaler()
        scaler_y = StandardScaler()
        X_tr = scaler_X.fit_transform(X_all[train_idx])
        y_tr = scaler_y.fit_transform(y_all[train_idx])
        X_te = scaler_X.transform(X_all[test_idx])
        y_te = scaler_y.transform(y_all[test_idx])

        train_ds = GeoDataset(X_tr, y_tr)
        test_ds = GeoDataset(X_te, y_te)
        train_loader = DataLoader(train_ds, batch_size=16, shuffle=True)
        test_loader = DataLoader(test_ds, batch_size=16, shuffle=False)

        model = ImprovedGeoNet(input_size=2).to(device)
        optimizer = torch.optim.AdamW(model.parameters(), lr=0.001, weight_decay=0.01)
        scheduler = torch.optim.lr_scheduler.ReduceLROnPlateau(optimizer, mode='min', factor=0.5, patience=10)
        criterion_local = nn.MSELoss()

        best_te = float('inf')
        patience_local = 20
        patience_counter = 0

        for epoch in range(200):
            model.train()
            train_loss_sum = 0.0
            for xb, yb in train_loader:
                xb, yb = xb.to(device), yb.to(device)
                optimizer.zero_grad()
                pred = model(xb)
                loss = criterion_local(pred, yb)
                loss.backward()
                optimizer.step()
                train_loss_sum += loss.item()

            model.eval()
            te_loss_sum = 0.0
            with torch.no_grad():
                for xb, yb in test_loader:
                    xb, yb = xb.to(device), yb.to(device)
                    pred = model(xb)
                    te_loss_sum += criterion_local(pred, yb).item()

            avg_te = te_loss_sum / len(test_loader)
            scheduler.step(avg_te)
            if avg_te < best_te:
                best_te = avg_te
                patience_counter = 0
                best_state = model.state_dict()
            else:
                patience_counter += 1
            if patience_counter >= patience_local:
                break

        # 评估当前折
        model.load_state_dict(best_state)
        model.eval()
        with torch.no_grad():
            y_pred_scaled = model(torch.FloatTensor(X_te).to(device)).cpu().numpy()
        y_pred = scaler_y.inverse_transform(y_pred_scaled)
        y_true = scaler_y.inverse_transform(y_te)
        r2 = r2_score(y_true, y_pred)
        rmse_val = np.sqrt(mean_squared_error(y_true, y_pred))
        mae_val = mean_absolute_error(y_true, y_pred)
        r2_list.append(r2)
        rmse_list.append(rmse_val)
        mae_list.append(mae_val)
        fold_idx += 1

    return np.array(r2_list), np.array(rmse_list), np.array(mae_list)

r2_long_cv, rmse_long_cv, mae_long_cv = run_fold_cv(X_long, y_long, 'Longitude')
r2_lat_cv, rmse_lat_cv, mae_lat_cv = run_fold_cv(X_lat, y_lat, 'Latitude')

with open(os.path.join(results_dir, 'klfdapc_cv_performance.txt'), 'w', encoding='utf-8') as f:
    f.write('5折交叉验证结果\n')
    f.write('='*50 + '\n')
    f.write('经度模型:\n')
    f.write(f"R2: mean={r2_long_cv.mean():.4f}, std={r2_long_cv.std():.4f}\n")
    f.write(f"RMSE: mean={rmse_long_cv.mean():.4f}, std={rmse_long_cv.std():.4f}\n\n")
    f.write(f"MAE: mean={mae_long_cv.mean():.4f}, std={mae_long_cv.std():.4f}\n\n")
    f.write('纬度模型:\n')
    f.write(f"R2: mean={r2_lat_cv.mean():.4f}, std={r2_lat_cv.std():.4f}\n")
    f.write(f"RMSE: mean={rmse_lat_cv.mean():.4f}, std={rmse_lat_cv.std():.4f}\n")
    f.write(f"MAE: mean={mae_lat_cv.mean():.4f}, std={mae_lat_cv.std():.4f}\n")

# 可视化：CV箱线图（R²与RMSE）
cv_df = pd.DataFrame({
    'Metric': ['R2'] * len(r2_long_cv) + ['R2'] * len(r2_lat_cv) + ['RMSE'] * len(rmse_long_cv) + ['RMSE'] * len(rmse_lat_cv),
    'Value': np.concatenate([r2_long_cv, r2_lat_cv, rmse_long_cv, rmse_lat_cv]),
    'Target': ['Longitude'] * len(r2_long_cv) + ['Latitude'] * len(r2_lat_cv) + ['Longitude'] * len(rmse_long_cv) + ['Latitude'] * len(rmse_lat_cv)
})

plt.figure(figsize=(12, 5))
palette = sns.color_palette("Set2", 2)

plt.subplot(1, 2, 1)
sns.boxplot(data=cv_df[cv_df['Metric'] == 'R2'], x='Target', y='Value', hue='Target', palette=palette, dodge=False, legend=False)
sns.stripplot(data=cv_df[cv_df['Metric'] == 'R2'], x='Target', y='Value', hue='Target', palette=['#444444', '#444444'], alpha=0.6, jitter=0.1, dodge=False, legend=False)
plt.xlabel('Target', fontsize=15, fontweight='bold')
plt.ylabel('R²', fontsize=15, fontweight='bold')
plt.tick_params(axis='both', which='major', labelsize=13)
ax = plt.gca()
for label in ax.get_xticklabels() + ax.get_yticklabels():
    label.set_fontweight('bold')

plt.subplot(1, 2, 2)
sns.boxplot(data=cv_df[cv_df['Metric'] == 'RMSE'], x='Target', y='Value', hue='Target', palette=palette, dodge=False, legend=False)
sns.stripplot(data=cv_df[cv_df['Metric'] == 'RMSE'], x='Target', y='Value', hue='Target', palette=['#444444', '#444444'], alpha=0.6, jitter=0.1, dodge=False, legend=False)
plt.xlabel('Target', fontsize=15, fontweight='bold')
plt.ylabel('RMSE', fontsize=15, fontweight='bold')
plt.tick_params(axis='both', which='major', labelsize=13)
ax = plt.gca()
for label in ax.get_xticklabels() + ax.get_yticklabels():
    label.set_fontweight('bold')

plt.tight_layout()
plt.savefig(os.path.join(results_dir, 'klfdapc_cv_boxplots.png'), dpi=300, bbox_inches='tight')
plt.close()

# 残差诊断图
def plot_residual_diagnostics(y_true, y_pred, title_prefix, filename):
    residuals = (y_pred - y_true).flatten()
    fitted = y_pred.flatten()

    plt.figure(figsize=(12, 5))
    plt.subplot(1, 2, 1)
    sns.scatterplot(x=fitted, y=residuals, s=35, alpha=0.9, edgecolor='#333333', linewidth=0.3)
    plt.axhline(0, color='#666666', linestyle='--', linewidth=1.2)
    plt.xlabel('Fitted')
    plt.ylabel('Residual')
    plt.title(f'{title_prefix} Residuals vs Fitted')

    plt.subplot(1, 2, 2)
    sns.histplot(residuals, bins=20, kde=True, color=sns.color_palette("Set2")[0])
    plt.axvline(np.mean(residuals), color='red', linestyle='--', linewidth=1.2, label='Mean')
    plt.xlabel('Residual')
    plt.ylabel('Count')
    plt.title(f'{title_prefix} Residual Distribution')
    plt.legend()

    plt.tight_layout()
    plt.savefig(os.path.join(results_dir, filename), dpi=300, bbox_inches='tight')
    plt.close()

plot_residual_diagnostics(y_long_test_original, y_long_pred, 'Longitude', 'klfdapc_residuals_long.png')
plot_residual_diagnostics(y_lat_test_original, y_lat_pred, 'Latitude', 'klfdapc_residuals_lat.png')