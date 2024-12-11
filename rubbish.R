rm(list=ls())
# 加载必要的包
library(dplyr)
library(tidyr)
library(igraph)
library(networkDynamic)
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

# 初始化动态网络并设置节点名称
base_network <- network.initialize(n = length(all_nodes), directed = FALSE)
set.vertex.attribute(base_network, "vertex.names", node_mapping$name)

# 设置节点的初始活动时间为 NA
activate.vertices(base_network, onset = -Inf, terminus = -Inf)  # 使所有节点初始不活跃

# 添加节点属性（初始为 NA）
set.vertex.attribute(base_network, "military_spending_to_GDP", NA)

dynamic_network <- networkDynamic(base.net = base_network)

for (year in 1980:2010) {
  # 获取当前年份的协议数据
  edge_data <- dcad %>% filter(Year == year)
  
  # 映射边列表中的节点到全局 ID
  edge_list <- edge_data %>%
    mutate(
      tail = node_mapping$id[match(country1, node_mapping$name)],
      head = node_mapping$id[match(country2, node_mapping$name)]
    )
  
  # 激活边两端的节点
  unique_nodes <- unique(c(edge_list$tail, edge_list$head))
  activate.vertices(dynamic_network, v = unique_nodes, onset = year, terminus = year + 1)
  
  # 添加边到动态网络并附加属性
  add.edges.active(dynamic_network,
                   tail = edge_list$tail,
                   head = edge_list$head,
                   onset = year,
                   terminus = year + 1)
  
  # 为边添加属性
  for (i in 1:nrow(edge_list)) {
    set.edge.attribute(dynamic_network, "type", value = edge_data$type[i], e = i)
    set.edge.attribute(dynamic_network, "asymmetry", value = edge_data$asymmetry[i], e = i)
  }
  
  # 更新节点属性
  node_data <- military_spending %>% filter(Year == year)
  for (i in 1:nrow(node_data)) {
    set.vertex.attribute(dynamic_network, 
                         "military_spending_to_GDP", 
                         value = node_data$military_spending_to_GDP[i],
                         v = which(node_mapping$name == node_data$country[i]))
  }
  
  # 打印每年的边数
  cat("Year:", year, "Edges added:", nrow(edge_list), "\n")
}

# 获取节点活动状态
vertex_activity <- get.vertex.activity(dynamic_network)
print(vertex_activity)

# 检查节点属性
get.vertex.attribute(dynamic_network, "military_spending_to_GDP")
# 检查边属性
get.edge.attribute(dynamic_network, "type")
get.edge.attribute(dynamic_network, "asymmetry")


# 渲染动态网络
render.d3movie(dynamic_network, 
               displaylabels = TRUE, 
               label = "vertex.names", 
               vertex.cex = 0.6,  # 调整节点大小
               vertex.col = "blue", 
               edge.col = "black")



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

######################3

library(sna)

# 计算度中心性随时间的变化
centrality_trends <- data.frame()

for (year in 1980:2010) {
  network_year <- network.extract(dynamic_network, at = year)
  degree_centrality <- degree(network_year)  # 计算度中心性
  centrality_trends <- rbind(
    centrality_trends,
    data.frame(year = year, node = network.vertex.names(network_year), degree = degree_centrality)
  )
}

# 可视化度中心性变化
library(ggplot2)
ggplot(centrality_trends, aes(x = year, y = degree, color = node)) +
  geom_line() +
  labs(title = "Degree Centrality Over Time", x = "Year", y = "Degree Centrality")

#############################3
# 提取 2010 年网络快照
network_2010 <- network.extract(dynamic_network, at = 2010)

# 获取节点属性和中心性
centrality_2010 <- data.frame(
  node = network.vertex.names(network_2010),
  military_spending_to_GDP = get.vertex.attribute(network_2010, "military_spending_to_GDP"),
  degree = degree(network_2010)
)

# 计算军费支出与度中心性的相关性
cor(centrality_2010$military_spending_to_GDP, centrality_2010$degree, use = "complete.obs")

##################
# 提取动态网络某年的快照
network_year <- network.extract(dynamic_network, at = 1980)

# 如果节点名称丢失，则从动态网络中补充名称
if (is.null(network.vertex.names(network_year))) {
  network.vertex.names(network_year) <- get.vertex.attribute(dynamic_network, "vertex.names")
}

# 转换为 igraph 对象
igraph_year <- intergraph::asIgraph(network_year)

# 确保节点名称存在
if (is.null(V(igraph_year)$name)) {
  V(igraph_year)$name <- network.vertex.names(network_year)
}
# 检查 1980 年网络快照的节点数量和名称
cat("Number of vertices in 1980 snapshot:", network.size(network_year), "\n")
print(network.vertex.names(network_year))

# 检查所有节点属性
all_vertex_attributes <- list.vertex.attributes(network_year)
print(all_vertex_attributes)

# 删除多余的 `active` 属性
delete.vertex.attribute(network_year, "active")

# 检查属性是否被成功删除
all_vertex_attributes <- list.vertex.attributes(network_year)
print(all_vertex_attributes)
# 转换为 igraph 对象
igraph_year <- intergraph::asIgraph(network_year)

# 检查转换结果
cat("Number of vertices in igraph_year:", vcount(igraph_year), "\n")
cat("Number of edges in igraph_year:", ecount(igraph_year), "\n")
# 手动绑定节点名称
V(igraph_year)$name <- network.vertex.names(network_year)
print(V(igraph_year)$name)

##############################
# 初始化存储所有年份社区检测结果的列表
community_trends <- list()

# 遍历每一年
for (year in 1980:2010) {
  tryCatch({
    # 提取动态网络某年的快照
    network_year <- network.extract(dynamic_network, at = year)
    
    # 检查并删除不必要的 "active" 属性
    if ("active" %in% list.vertex.attributes(network_year)) {
      delete.vertex.attribute(network_year, "active")
    }
    
    # 检查属性长度并删除异常属性
    attribute_lengths <- sapply(list.vertex.attributes(network_year), function(attr) {
      length(get.vertex.attribute(network_year, attr))
    })
    for (attr in names(attribute_lengths)) {
      if (attribute_lengths[attr] != network.size(network_year)) {
        cat("Deleting inconsistent attribute:", attr, "in year", year, "\n")
        delete.vertex.attribute(network_year, attr)
      }
    }
    
    # 确保节点名称正确设置
    if (is.null(network.vertex.names(network_year)) || length(network.vertex.names(network_year)) != network.size(network_year)) {
      global_vertex_names <- get.vertex.attribute(dynamic_network, "vertex.names")
      network.vertex.names(network_year) <- global_vertex_names[1:network.size(network_year)]
    }
    
    # 转换为 igraph 对象
    igraph_year <- intergraph::asIgraph(network_year)
    
    # 确保节点名称绑定
    if (is.null(V(igraph_year)$name)) {
      V(igraph_year)$name <- network.vertex.names(network_year)
    }
    
    # 去除多重边和自环
    igraph_year <- simplify(igraph_year, remove.multiple = TRUE, remove.loops = TRUE)
    
    # 如果网络中有边，进行社区检测
    if (ecount(igraph_year) > 0) {
      communities <- cluster_fast_greedy(igraph_year)
      
      # 获取活动节点名称和社区划分
      active_nodes <- V(igraph_year)$name
      community_assignments <- membership(communities)
      
      # 映射社区划分结果到全局节点集
      membership_map <- rep(NA, length(all_nodes))  # 初始化为 NA
      names(membership_map) <- all_nodes            # 设置全局节点名称为索引
      
      # 确保对齐
      mapped_indices <- match(active_nodes, all_nodes)
      
      # 检查未匹配节点
      unmatched_nodes <- active_nodes[is.na(mapped_indices)]
      if (length(unmatched_nodes) > 0) {
        cat("Unmatched nodes in year:", year, "\n")
        print(unmatched_nodes)
      }
      
      # 更新社区划分结果
      membership_map[mapped_indices[!is.na(mapped_indices)]] <- community_assignments[!is.na(mapped_indices)]
      
      # 保存结果
      community_trends[[as.character(year)]] <- membership_map
    } else {
      # 如果网络中没有边，则保存 NA
      community_trends[[as.character(year)]] <- rep(NA, length(all_nodes))
      names(community_trends[[as.character(year)]]) <- all_nodes
    }
  }, error = function(e) {
    cat("Error in year", year, ":", e$message, "\n")
  })
}

# 查看部分年份的社区划分结果
for (year in c(1980, 1990, 2000, 2010)) {
  cat("Year:", year, "\n")
  print(community_trends[[as.character(year)]])
}

# 提取 1990 年网络快照
network_year <- network.extract(dynamic_network, at = 1990)

# 打印网络的节点和边信息
cat("Number of vertices in 1990:", network.size(network_year), "\n")
cat("Number of edges in 1990:", network.edgecount(network_year), "\n")

# 打印节点属性
vertex_attributes <- list.vertex.attributes(network_year)
cat("Vertex attributes in 1990:", "\n")
print(vertex_attributes)

# 打印每个节点属性的长度
vertex_attribute_lengths <- sapply(vertex_attributes, function(attr) {
  length(get.vertex.attribute(network_year, attr))
})
cat("Lengths of vertex attributes:", "\n")
print(vertex_attribute_lengths)

# 打印边属性
edge_attributes <- list.edge.attributes(network_year)
cat("Edge attributes in 1990:", "\n")
print(edge_attributes)

# 打印具体的节点属性值
for (attr in vertex_attributes) {
  cat("Attribute:", attr, "\n")
  print(get.vertex.attribute(network_year, attr))
}

# 打印具体的边属性值
for (attr in edge_attributes) {
  cat("Attribute:", attr, "\n")
  print(get.edge.attribute(network_year, attr))
}

# 检查节点名称
cat("Vertex names in 1990:", "\n")
print(network.vertex.names(network_year))

# 检查活动节点是否与节点名称对齐
global_vertex_names <- get.vertex.attribute(dynamic_network, "vertex.names")
if (is.null(network.vertex.names(network_year)) || length(network.vertex.names(network_year)) != network.size(network_year)) {
  network.vertex.names(network_year) <- global_vertex_names[1:network.size(network_year)]
}
cat("Updated vertex names in 1990:", "\n")
print(network.vertex.names(network_year))

# 转换为 igraph 对象
igraph_year <- intergraph::asIgraph(network_year)

# 检查 igraph 中的节点和边
cat("Number of vertices in igraph_year:", vcount(igraph_year), "\n")
cat("Number of edges in igraph_year:", ecount(igraph_year), "\n")
print(V(igraph_year)$name)

#######################使用degree centrality渲染动态图######################

# 遍历每个时间片，计算度数
for (year in 1980:2010) {
  # 提取当前时间片的网络
  network_slice <- network.extract(dynamic_network, at = year)
  
  # 计算节点度数
  degree_values <- sna::degree(network_slice, gmode = "graph")  # 使用 SNA 包
  
  # 归一化节点大小到 5-20 范围
  vertex_sizes <- scales::rescale(degree_values, to = c(0.3, 2), na.rm = TRUE)
  
  # 激活节点大小属性
  activate.vertex.attribute(dynamic_network, 
                            "degree_size", 
                            value = vertex_sizes, 
                            onset = year, 
                            terminus = year + 1)
}

# 遍历每个时间片，计算度数
for (year in 1980:2010) {
  # 提取当前时间片的网络
  network_slice <- network.extract(dynamic_network, at = year)
  
  # 计算节点度数
  degree_values <- sna::degree(network_slice, gmode = "graph")  # 使用 SNA 包
  
  # 获取所有节点的索引
  all_node_ids <- seq_len(network.size(dynamic_network))
  
  # 为所有节点设置默认值
  degree_values_full <- rep(0, length(all_node_ids))
  
  # 将计算的度数值分配给当前时间片活跃的节点
  active_node_ids <- network.vertex.names(network_slice)
  degree_values_full[match(active_node_ids, network.vertex.names(dynamic_network))] <- degree_values
  
  # 归一化节点大小到 5-20 范围
  vertex_sizes <- scales::rescale(degree_values_full, to = c(0.3, 2), na.rm = TRUE)
  print(vertex_sizes)
  
  # 激活节点大小属性
  activate.vertex.attribute(dynamic_network, 
                            "degree_size", 
                            value = vertex_sizes, 
                            onset = year, 
                            terminus = year + 1)
}

print(get.vertex.attribute.active(dynamic_network,"degree_size", at = 1982))

# 遍历每个时间片，计算度数并调试
for (year in 1980:2010) {
  # 提取当前时间片的网络
  network_slice <- network.extract(dynamic_network, at = year)
  
  # 计算节点度数
  degree_values <- sna::degree(network_slice, gmode = "graph")  # 使用 SNA 包
  
  # 获取所有节点的索引
  all_node_ids <- seq_len(network.size(dynamic_network))
  
  # 为所有节点设置默认值
  degree_values_full <- rep(0, length(all_node_ids))
  
  # 将计算的度数值分配给当前时间片活跃的节点
  active_node_ids <- network.vertex.names(network_slice)
  degree_values_full[match(active_node_ids, network.vertex.names(dynamic_network))] <- degree_values
  
  # 归一化节点大小到 5-20 范围
  vertex_sizes <- scales::rescale(degree_values_full, to = c(0.3, 2), na.rm = TRUE)
  
  # 调试：检查是否存在缺失值
  if (any(is.na(vertex_sizes))) {
    cat("Error detected in year:", year, "\n")
    cat("Vertex sizes with NA values:\n")
    print(vertex_sizes)
    stop("Illegal missing values detected in vertex_sizes.")
  }
  
  # 激活节点大小属性
  activate.vertex.attribute(dynamic_network, 
                            "degree_size", 
                            value = vertex_sizes, 
                            onset = year, 
                            terminus = year + 1)
}


render.d3movie(
  dynamic_network,
  displaylabels = TRUE,
  label = "vertex.names",
  vertex.cex = "degree_size",  # 动态调整节点大小
  vertex.col = "blue",         # 固定节点颜色
  edge.col = "gray",
  render.par = list(show.time = TRUE),
  d3.options = list(animationDuration = 1000, playControls = TRUE)
)


