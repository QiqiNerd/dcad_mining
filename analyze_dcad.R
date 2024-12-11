# 加载必要的包
library(dplyr)
library(tidyr)
library(igraph)
library(networkDynamic)
install.packages("ndtv")
library(ndtv)

# 读取数据
military_spending <- read.csv("military_spending_cleaned.csv")
dcad <- read.csv("DCAD_cleaned.csv")

# 确保年份范围是 1980-2010
military_spending <- military_spending %>% filter(Year >= 1980 & Year <= 2010)
dcad <- dcad %>% filter(Year >= 1980 & Year <= 2010)
dcad

# 初始化一个空列表存储网络切片
network_list <- list()

for (year in 1980:2010) {
  # 筛选当年的合作协议数据
  edge_data <- dcad %>% filter(Year == year)
  
  # 合并重复边并添加权重
  edges <- edge_data %>%
    group_by(country1, country2) %>%
    summarise(
      weight = n(),  # 计算重复边的数量
      type = paste(unique(type), collapse = ", "),  # 合并 type 信息
      asymmetry = paste(unique(asymmetry), collapse = ", "),  # 合并 asymmetry 信息
      .groups = "drop" # 添加此参数
    ) %>%
    ungroup()
  
  # 创建网络（仅提供边列表）
  network <- graph_from_data_frame(d = edges, directed = FALSE)
  
  # 添加节点属性（仅为协议中涉及的国家添加属性）
  node_data <- military_spending %>% filter(Year == year & country %in% V(network)$name)
  V(network)$military_spending_to_GDP <- node_data$military_spending_to_GDP[match(V(network)$name, node_data$country)]
  
  # 添加边属性
  E(network)$weight <- edges$weight
  E(network)$type <- edges$type
  E(network)$asymmetry <- edges$asymmetry
  
  # 保存到列表中
  network_list[[as.character(year)]] <- network
}

# 创建全局节点映射
all_nodes <- unique(c(dcad$country1, dcad$country2))
node_mapping <- data.frame(name = all_nodes, id = seq_along(all_nodes))
node_mapping

# 初始化动态网络
dynamic_network <- networkDynamic(base.net = network.initialize(n = length(all_nodes), directed = FALSE))

# 添加每年的边到动态网络
for (year in 1980:2010) {
  # 获取当前年份的协议数据
  edge_data <- dcad %>% filter(Year == year)
  
  # 映射边列表中的节点到全局 ID
  edge_list <- edge_data %>%
    mutate(
      tail = node_mapping$id[match(country1, node_mapping$name)],
      head = node_mapping$id[match(country2, node_mapping$name)]
    )
  
  # 添加边到动态网络
  add.edges.active(dynamic_network,
                   tail = edge_list$tail,
                   head = edge_list$head,
                   onset = year,
                   terminus = year + 1)
  
  # 打印每年的边数
  cat("Year:", year, "Edges added:", nrow(edge_list), "\n")
}
for (year in 1980:2010) {
  network_year <- network.extract(dynamic_network, at = year)
  cat("Year:", year, "Edges:", network.edgecount(network_year), "\n")
}

# 获取所有边的活动状态
edge_activity <- get.edge.activity(dynamic_network)
print(edge_activity)

# 检查动态网络的边数量
cat("Total edges:", length(edge_activity), "\n")

# 提取所有边的活动时间
onsets <- sapply(edge_activity, function(e) e[, 1])  # 开始时间
termini <- sapply(edge_activity, function(e) e[, 2])  # 结束时间

# 打印活动时间范围
cat("Edge activity onset range:", min(onsets), "-", max(onsets), "\n")
cat("Edge activity terminus range:", min(termini), "-", max(termini), "\n")

for (year in 1980:2010) {
  network_year <- network.extract(dynamic_network, at = year)
  cat("Year:", year, "Edges:", network.edgecount(network_year), "\n")
}
render.d3movie(dynamic_network, displaylabels = TRUE, vertex.col = "blue", edge.col = "black")
