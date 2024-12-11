library(tidyr)
library(dplyr)

##################clean military_spending_to_GDP data##################
# 读取文件
dcad_cleaned <- read.csv("DCAD.csv", stringsAsFactors = FALSE)

# 去除每列前后的多余空格
dcad_cleaned <- data.frame(lapply(dcad_cleaned, function(x) {
  if (is.character(x)) {
    return(trimws(x)) # 去除前后空格
  } else {
    return(x) # 非字符列保持不变
  }
}))

# 保存清理后的文件
write.csv(dcad_cleaned, "DCAD_cleaned.csv", row.names = FALSE)

cat("文件清理完成，并已保存为 DCAD_cleaned.csv")

################## match military_spending_to_GDP data##################
# 加载数据
data <- read.csv("cleaned_data.csv")

# 转换为长格式
data_long <- data %>%
  pivot_longer(
    cols = starts_with("X"), # 选择年份列
    names_to = "Year",
    values_to = "military_spending_to_GDP"
  )

# 清理年份列
data_long <- data_long %>%
  mutate(Year = as.numeric(gsub("X", "", Year))) # 去掉 "X" 并转换为数字

# 删除没有值的行
data_long <- data_long %>%
  filter(!is.na(military_spending_to_GDP))

# 重命名列名
final_data <- data_long %>%
  rename(country = Country.Code) %>% # 将 Country.Code 改为 country
  select(Year, country, military_spending_to_GDP)

# 保存为新的 CSV 文件
write.csv(final_data, "military_spending.csv", row.names = FALSE)

# 检查结果
head(final_data)

#############用于过滤 military_spending_cleaned 数据中未出现在 DCAD_cleaned 的 country1 和 country2 列中的国家代码的行

military_spending <- read.csv("military_spending_cleaned.csv")
dcad <- read.csv("DCAD_cleaned.csv")

# 提取 DCAD_cleaned 中的所有国家代码
valid_countries <- unique(c(dcad$country1, dcad$country2))

# 过滤 military_spending_cleaned 中未在 DCAD_cleaned 中出现的国家代码
filtered_military_spending <- military_spending %>% 
  filter(country %in% valid_countries)

# 查看过滤后的数据
cat("Rows before filtering:", nrow(military_spending), "\n")
cat("Rows after filtering:", nrow(filtered_military_spending), "\n")

# 保存过滤后的数据
write.csv(filtered_military_spending, "military_spending_cleaned.csv", row.names = FALSE)
