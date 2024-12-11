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

# 创建全局节点映射
all_nodes <- unique(c(dcad$country1, dcad$country2))
node_mapping <- data.frame(name = all_nodes, id = seq_along(all_nodes))

# 初始化动态网络并设置节点名称
base_network <- network.initialize(n = length(all_nodes), directed = FALSE)
set.vertex.attribute(base_network, "vertex.names", node_mapping$name)
dynamic_network <- networkDynamic(base.net = base_network)

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

# 渲染动态网络
render.d3movie(dynamic_network, displaylabels = TRUE, label = "vertex.names", vertex.col = "blue", edge.col = "black")


render.d3movie(dynamic_network, 
               displaylabels = TRUE, 
               label = "vertex.names", 
               vertex.cex = 0.6,  # 调整节点大小
               vertex.col = "blue", 
               edge.col = "black")


# 定义显示标签的逻辑
important_nodes <- which(degree(dynamic_network) > 5)  # 仅显示度大于 5 的节点

render.d3movie(dynamic_network, 
               displaylabels = TRUE, 
               label = "vertex.names", 
               vertex.cex = 0.6, 
               vertex.col = "blue", 
               edge.col = "black",
               label.cex = ifelse(1:vcount(dynamic_network) %in% important_nodes, 1, 0))  # 控制标签显示

######################
library(intergraph)
# 提取 2010 年的动态网络子集
network_2010 <- network.extract(dynamic_network, at = 2010)

# 将子集转换为 igraph 对象
igraph_2010 <- asIgraph(network_2010)
plot(igraph_2010, 
     layout = layout_with_fr,  # 使用 Fruchterman-Reingold 布局
     vertex.label.cex = 0.5,  # 标签字体大小
     vertex.size = 5,         # 节点大小
     vertex.label = V(igraph_2010)$vertex.names,  # 显示国家代码作为标签
     vertex.color = "blue")
igraph_2010