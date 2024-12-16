##################################################
########## Construct the dynamic network ###########
##################################################
library(dplyr)
library(networkDynamic)
library(ndtv)

# 读取数据
military_spending <- read.csv("military_spending_cleaned.csv")
dcad <- read.csv("DCAD_endyear_cleaned.csv")

# 确保年份范围是 1980-2010
military_spending <- military_spending %>% filter(Year >= 1980 & Year <= 2010)
dcad <- dcad %>% filter(signYear >= 1980 & endYearEstimate <= 2010)

# 创建全局节点映射
all_nodes <- unique(c(dcad$country1, dcad$country2))
node_mapping <- data.frame(name = all_nodes, id = seq_along(all_nodes))

# 初始化动态网络并设置节点名称
base_network <- network.initialize(n = length(all_nodes), directed = FALSE)
base_network %v% "vertex.names" <- node_mapping$name
dynamic_network <- networkDynamic(base.net = base_network)

# 添加每年的边和节点属性到动态网络
for (year in 1980:2010) {
  # 获取当前年份及之前的协议数据
  edge_data <- dcad %>% filter(signYear <= year & endYearEstimate >= year)
  
  # 创建边列表并映射到全局 ID
  edge_list <- edge_data %>%
    mutate(
      tail = node_mapping$id[match(country1, node_mapping$name)],
      head = node_mapping$id[match(country2, node_mapping$name)]
    )
  
  # 合并重复边并计算权重
  edges <- edge_list %>%
    group_by(tail, head) %>%
    summarise(
      weight = n(),
      .groups = "drop"
    )
  
  # 获取节点属性数据
  node_data <- military_spending %>% filter(Year == year)
  node_ids <- node_mapping$id[match(node_data$country, node_mapping$name)]
  
  # 添加节点属性
  activate.vertex.attribute(
    dynamic_network, 
    prefix = "military_spending_to_GDP", 
    value = node_data$military_spending_to_GDP, 
    onset = year, 
    terminus = year + 1, 
    v = node_ids
  )
  
  # 添加边到动态网络
  add.edges.active(
    dynamic_network, 
    tail = edges$tail, 
    head = edges$head, 
    onset = year, 
    terminus = year + 1
  )
  
  # 添加边的动态属性
  activate.edge.attribute(
    dynamic_network, 
    prefix = "weight", 
    value = edges$weight, 
    onset = year, 
    terminus = year + 1
  )
  
}

# 使用 compute.animation 优化布局
compute.animation(
  net = dynamic_network,
  slice.par = list(
    start = 1980,            # 动画开始年份
    end = 2010,              # 动画结束年份
    interval = 1,            # 每帧的时间间隔
    aggregate.dur = 1,       # 聚合时间的跨度
    rule = "latest"          # 使用最新数据的规则
  ),
  animation.mode = "kamadakawai", # 使用 Kamada-Kawai 布局算法
  seed.coords = matrix(runif(network.size(dynamic_network) * 2), ncol = 2), # 随机初始位置
  # seed.coords = NULL,            # 初始节点位置（默认随机）
  layout.par = list(),           # 传递给布局算法的额外参数
  default.dist = NULL,           # 节点之间的默认距离（使用默认值）
  weight.attr = "weight",       # 使用边的权重进行布局计算
  weight.dist = FALSE,           # 边权重作为相似性（较高值表示更靠近）
  chain.direction = "forward",  # 布局链按时间前进
  verbose = TRUE                 # 显示过程信息
)

# 渲染动态网络
d3_options <- list(
  playControls = TRUE,      # 显示播放控制
  slider = TRUE,            # 显示时间滑块
  animateOnLoad = TRUE,     # 页面加载时自动播放
  nodeSizeFactor = 0.02     # 节点大小比例
)

# 动态生成 HTML 动画并显示节点名称和其他属性
render.d3movie(
  dynamic_network,
  filename = "network_animation_with_labels_final.html",
  render.par = list(
    tween.frames = 10,  # 插值帧数
    show.time = TRUE,
    initial.coords = matrix(0, ncol = 2, nrow = network.size(dynamic_network))
  ),
  plot.par = list(
    vertex.col = function(slice, onset, terminus, ...) {
      gdp_values <- get.vertex.attribute.active(slice, "military_spending_to_GDP", at = onset)
      sapply(gdp_values, function(gdp) {
        if (is.na(gdp)) return("gray")  # 无数据节点为灰色
        if (gdp > 5) return("red")  # 高开支节点为红色
        if (gdp > 2) return("orange")  # 中等开支节点为橙色
        return("green")  # 低开支节点为绿色
      })
    },
    vertex.cex = function(slice, onset, terminus, ...) {
      gdp_values <- get.vertex.attribute.active(slice, "military_spending_to_GDP", at = onset)
      min_size <- 0.3
      max_size <- 2
      gdp_normalized <- scales::rescale(gdp_values, to = c(min_size, max_size), na.rm = TRUE)
      ifelse(is.na(gdp_normalized), min_size, gdp_normalized)
    },
    edge.col = function(slice, onset, terminus, ...) {
      edge_weights <- get.edge.attribute.active(slice, "weight", at = onset)
      sapply(edge_weights, function(weight) {
        if (weight > 1) return("#F08080")  # weight大于1的边为浅红色
        return("gray")  # 其他边为灰色
      })
    },
    edge.lwd = function(slice, onset, terminus, ...) {
      edge_weights <- get.edge.attribute.active(slice, "weight", at = onset)
      scales::rescale(edge_weights, to = c(0.5, 5), na.rm = TRUE)  # 根据权重调整边的宽度
    },
    displaylabels = TRUE  # 显示节点标签
  ),
  vertex.tooltip = function(slice, onset, terminus, ...) {
    gdp_values <- get.vertex.attribute.active(slice, "military_spending_to_GDP", at = onset)
    paste0(
      "Military expenditure (% of GDP): ", ifelse(is.na(gdp_values), "N/A", gdp_values), "<br>"
    )
  },
  label = function(slice, ...) {
    network.vertex.names(slice)  # 使用节点名称作为标签
  },
  edge.tooltip = function(slice, onset, terminus, ...) {
    edge_weights <- get.edge.attribute.active(slice, "weight", at = onset)
    paste0(
      "Weight: ", edge_weights, "<br>"
    )
  },
  d3.options = list(
    playControls = TRUE,
    slider = TRUE,
    animationDuration = 800
  ),
  launchBrowser = TRUE
)

################################################################
########################## data analysis ########################
##############################################################

#################### 1. Number of edges ######################

library(ggplot2)
library(networkDynamic)

# Initialize an empty data frame to store edge counts
edge_summary <- data.frame(Year = integer(), Edges = integer())

# Loop through each year and calculate the number of edges
for (year in 1980:2010) {
  # Extract the network slice for the year
  network_slice <- network.extract(dynamic_network, at = year)
  
  # Count the number of edges in the slice
  edge_count <- network.edgecount(network_slice)
  
  # Add the results to the summary data frame
  edge_summary <- rbind(edge_summary, data.frame(Year = year, Edges = edge_count))
}

# Plot the number of edges over the years
ggplot(edge_summary, aes(x = Year, y = Edges)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "red", size = 2) +
  labs(
    title = "Number of Edges in the Network Over Years",
    x = "Year",
    y = "Number of Edges"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )


####################################################
##########  2. Centrality analysis ###############
#############################################
# 加载必要的包
# 加载必要的包
library(sna)
library(ggplot2)
library(igraph)

# 初始化存储中心性结果的表
centrality_data <- data.frame(
  Year = integer(),
  Country = character(),
  Degree = numeric(),
  Betweenness = numeric(),
  Eigenvector = numeric(),
  Closeness = numeric()
)

# 遍历每年，计算中心性指标
for (year in 1980:2010) {
  # 提取该年份的网络切片
  network_slice <- network.extract(dynamic_network, at = year)
  
  # 获取节点名称
  node_names <- network.vertex.names(network_slice)
  
  # 如果网络切片为空，跳过该年份
  if (network.size(network_slice) == 0) {
    next
  }
  
  # Degree 和 Betweenness 中心性
  degree <- sna::degree(network_slice, gmode = "graph")
  betweenness <- sna::betweenness(network_slice, gmode = "graph")
  
  # Eigenvector 中心性
  adj_matrix <- as.matrix.network.adjacency(network_slice)  # 提取邻接矩阵
  eig <- eigen(adj_matrix)                                 # 特征值分解
  eigen_centrality <- abs(eig$vectors[, 1])                # 最大特征值对应的特征向量
  
  # Closeness 中心性
  g <- graph.adjacency(adj_matrix, mode = "undirected", weighted = NULL, diag = FALSE)
  closeness <- igraph::closeness(g, normalized = TRUE)     # 使用 igraph 的 closeness 函数
  
  # 存储中心性数据
  year_data <- data.frame(
    Year = rep(year, length(node_names)),
    Country = node_names,
    Degree = degree,
    Betweenness = betweenness,
    Eigenvector = eigen_centrality,
    Closeness = closeness
  )
  
  # 合并数据
  centrality_data <- rbind(centrality_data, year_data)
}

# 查看前几行结果
head(centrality_data)

# 选择关键国家（例如：USA, China, Russia）
key_countries <- c("USA", "CHN", "RUS")
filtered_data <- centrality_data[centrality_data$Country %in% key_countries, ]

# 绘制 Degree 中心性变化的趋势图
ggplot(filtered_data, aes(x = Year)) +
  geom_line(aes(y = Degree, color = Country), size = 1) +
  facet_wrap(~Country, scales = "free_y") +
  labs(title = "Degree Centrality Over Time",
       x = "Year",
       y = "Degree Centrality") +
  theme_minimal()

# 同样绘制 Betweenness 的趋势图
ggplot(filtered_data, aes(x = Year)) +
  geom_line(aes(y = Betweenness, color = Country), size = 1) +
  facet_wrap(~Country, scales = "free_y") +
  labs(title = "Betweenness Centrality Over Time",
       x = "Year",
       y = "Betweenness Centrality") +
  theme_minimal()

ggplot(filtered_data, aes(x = Year)) +
  geom_line(aes(y = Eigenvector, color = Country), size = 1) +
  facet_wrap(~Country, scales = "free_y") +
  labs(title = "Eigenvector Centrality Over Time",
       x = "Year",
       y = "Eigenvector Centrality") +
  theme_minimal()
ggplot(filtered_data, aes(x = Year)) +
  geom_line(aes(y = Closeness, color = Country), size = 1) +
  facet_wrap(~Country, scales = "free_y") +
  labs(title = "Closeness Centrality Over Time",
       x = "Year",
       y = "Closeness Centrality") +
  theme_minimal()


################################################################################
################ 3. corralation between military expenditure and DCA###############
################################################################################
# 加载必要的包
# 加载必要的包
library(sna)
library(igraph)
library(ggplot2)
library(tidyr)
library(dplyr)

# 初始化存储结果的列表和数据框
results <- list()
correlation_summary <- data.frame()

# 遍历每一年
for (year in 1982:2010) {
  # 提取该年份的网络切片
  network_slice <- network.extract(dynamic_network, at = year)
  
  # 获取节点名称和军事开支数据
  node_names <- network.vertex.names(network_slice)
  military_spending <- get.vertex.attribute.active(dynamic_network, "military_spending_to_GDP", at = year)
  
  # 如果网络切片为空，跳过该年份
  if (network.size(network_slice) == 0) {
    next
  }
  
  # Degree 和 Betweenness 中心性
  degree <- sna::degree(network_slice, gmode = "graph")
  betweenness <- sna::betweenness(network_slice, gmode = "graph")
  
  # Eigenvector 中心性
  adj_matrix <- as.matrix.network.adjacency(network_slice)  # 提取邻接矩阵
  eig <- eigen(adj_matrix)                                 # 特征值分解
  eigen_centrality <- abs(eig$vectors[, 1])                # 最大特征值对应的特征向量
  
  # Closeness 中心性
  # 转换为 igraph 对象并提取最大连通分量
  g <- graph.adjacency(adj_matrix, mode = "undirected", weighted = NULL, diag = FALSE)
  components <- components(g) # 提取连通分量
  largest_component <- which.max(components$csize) # 最大连通分量的索引
  subgraph <- induced_subgraph(g, which(components$membership == largest_component)) # 子图
  
  # 在最大连通分量上计算 Closeness
  closeness <- rep(NA, length(node_names)) # 初始化为 NA
  subgraph_closeness <- igraph::closeness(subgraph, normalized = TRUE)
  
  # 将 closeness 结果映射回所有节点
  subgraph_nodes <- V(subgraph)$name # 子图中的节点名称
  subgraph_indices <- match(subgraph_nodes, node_names) # 找到对应原图的索引
  closeness[subgraph_indices] <- subgraph_closeness # 替换对应位置的值
  
  # 创建数据框
  year_data <- data.frame(
    Country = node_names,
    Degree = degree,
    Betweenness = betweenness,
    Eigenvector = eigen_centrality,
    Closeness = closeness,
    MilitarySpendingGDP = military_spending,
    Year = year
  )
  
  # 过滤掉没有军事开支数据的节点
  year_data <- year_data %>% filter(!is.na(MilitarySpendingGDP))
  
  if (nrow(year_data) > 1) { # 确保至少有两个数据点进行相关性计算
    # 计算相关性
    cor_degree <- cor(year_data$Degree, year_data$MilitarySpendingGDP)
    cor_betweenness <- cor(year_data$Betweenness, year_data$MilitarySpendingGDP)
    cor_eigenvector <- cor(year_data$Eigenvector, year_data$MilitarySpendingGDP)
    cor_closeness <- cor(year_data$Closeness, year_data$MilitarySpendingGDP, use = "complete.obs")
  } else {
    cor_degree <- NA
    cor_betweenness <- NA
    cor_eigenvector <- NA
    cor_closeness <- NA
  }
  
  # 保存结果
  results[[as.character(year)]] <- list(
    Data = year_data,
    Correlation = data.frame(
      Year = year,
      DegreeCorrelation = cor_degree,
      BetweennessCorrelation = cor_betweenness,
      EigenvectorCorrelation = cor_eigenvector,
      ClosenessCorrelation = cor_closeness
    )
  )
  
  # 合并到总结数据框中
  correlation_summary <- rbind(
    correlation_summary,
    data.frame(
      Year = year,
      DegreeCorrelation = cor_degree,
      BetweennessCorrelation = cor_betweenness,
      EigenvectorCorrelation = cor_eigenvector,
      ClosenessCorrelation = cor_closeness
    )
  )
}


# 转换数据框为长格式
correlation_summary_long <- correlation_summary %>%
  pivot_longer(
    cols = c(DegreeCorrelation, BetweennessCorrelation, EigenvectorCorrelation, ClosenessCorrelation),
    names_to = "CentralityType",
    values_to = "Correlation"
  )


# 绘制相关性趋势图
ggplot(correlation_summary_long, aes(x = Year, y = Correlation, color = CentralityType)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Centrality and change in the correlation of military expenditure /GDP",
    x = "Year",
    y = "Correlation",
    color = "Centrality Type"
  ) +
  theme_minimal()



######################################################
########################### 4. brokerage ###########################
######################################################

# 初始化存储结果的列表和数据框
brokerage_results <- list()
brokerage_summary <- data.frame()

# 遍历每一年进行中介分析
for (year in 1980:2010) {
  # 提取该年份的网络切片
  network_slice <- network.extract(dynamic_network, at = year)
  
  # 获取节点名称和军事开支数据
  node_names <- network.vertex.names(network_slice)
  military_spending <- get.vertex.attribute.active(dynamic_network,"military_spending_to_GDP", at = year)
  
  # 计算中介分数（使用 Betweenness 作为中介角色的近似）
  betweenness <- sna::betweenness(network_slice, gmode = "graph")
  
  # 创建数据框
  year_data <- data.frame(
    Country = node_names,
    Betweenness = betweenness,
    MilitarySpendingGDP = military_spending,
    Year = year
  )
  
  # 保存分析结果
  brokerage_results[[as.character(year)]] <- year_data
  
  # 获取中介分数最高的节点
  top_brokers <- year_data %>% 
    arrange(desc(Betweenness)) %>%
    slice_head(n = 5) %>%
    mutate(Rank = 1:n())
  
  # 合并到总结数据框中
  brokerage_summary <- rbind(brokerage_summary, top_brokers)
}

# 分析“始终作为中介”的国家
consistent_brokers <- brokerage_summary %>%
  group_by(Country) %>%
  summarize(
    TotalYears = n(),
    AvgBetweenness = mean(Betweenness, na.rm = TRUE)
  ) %>%
  arrange(desc(TotalYears))

# 可视化中介角色分布和排名
library(ggplot2)

# 可视化中介分数最高的国家随年份的变化
brokerage_summary %>%
  ggplot(aes(x = Year, y = Betweenness, color = Country, group = Country)) +
  geom_line() +
  geom_point() +
  labs(
    title = "每年中介分数最高的国家",
    x = "年份",
    y = "中介分数（Betweenness）",
    color = "国家"
  ) +
  theme_minimal()

# 取前 10 个始终作为中介的国家
top_consistent_brokers <- consistent_brokers %>%
  top_n(10, TotalYears) %>%
  arrange(desc(TotalYears))

# 绘制改进后的排名柱状图
library(ggplot2)

ggplot(top_consistent_brokers, aes(x = reorder(Country, -TotalYears), y = TotalYears, fill = AvgBetweenness)) +
  geom_bar(stat = "identity", color = "black", width = 0.7) +
  scale_fill_gradient(low = "lightblue", high = "darkblue", name = "Avg Betweenness Score") +
  geom_text(aes(label = paste("Years:", TotalYears, "\nAvg:", round(AvgBetweenness, 2))),
            hjust = -0.1, size = 3, color = "black") +
  coord_flip() +
  labs(
    title = "Top Consistent Brokers (1980-2010)",
    subtitle = "Ranked by Years of Participation and Avg Betweenness Score",
    x = "Country",
    y = "Years as Broker"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.position = "bottom"
  )


# Filter out countries with Avg Betweenness Score equal to 0
filtered_brokers <- consistent_brokers %>%
  filter(AvgBetweenness > 0) %>%
  top_n(10, TotalYears) %>%
  arrange(desc(TotalYears))

# Plot the improved bar chart with English labels
library(ggplot2)

ggplot(filtered_brokers, aes(x = reorder(Country, -TotalYears), y = TotalYears, fill = AvgBetweenness)) +
  geom_bar(stat = "identity", color = "black", width = 0.7) +
  scale_fill_gradient(low = "lightblue", high = "darkblue", name = "Avg Betweenness Score") +
  geom_text(aes(label = paste("Years:", TotalYears, "\nAvg:", round(AvgBetweenness, 2))),
            hjust = -0.1, size = 3, color = "black") +
  coord_flip() +
  labs(
    title = "Top Consistent Brokers (1980-2010)",
    subtitle = "Ranked by Years of Participation and Avg Betweenness Score",
    x = "Country",
    y = "Years as Broker"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.position = "bottom"
  )


###########################################################
#################### 5. Community Detaction ####################
############################################################
library(igraph)
library(networkDynamic)
library(dplyr)
install.packages("clue")
library(clue)    # 提供 solve_LSAP() 函数（匈牙利算法/线性分配）

# 1. Louvain 社群检测（逐年）
community_results <- list()

for (year in 1980:2010) {
  # 提取该年份网络切片
  network_slice <- network.extract(dynamic_network, at = year)
  
  # 获取该年份活动边
  edge_ages <- dyads.age.at(network_slice, at = year, format.out = "edgelist")
  
  if (is.null(edge_ages) || nrow(edge_ages) == 0) {
    cat("Year", year, "has no active edges, skipping.\n")
    next
  }
  
  # 获取节点名称
  node_names <- network.vertex.names(network_slice)
  
  # 构建 edgelist（使用节点名称）
  edge_list_named <- data.frame(
    tails = node_names[edge_ages[, 1]],
    heads = node_names[edge_ages[, 2]],
    stringsAsFactors = FALSE
  )
  
  # 创建 igraph 对象 & 移除孤立点
  g <- graph_from_data_frame(edge_list_named, directed = FALSE)
  g <- delete.vertices(g, which(degree(g) == 0))
  
  if (vcount(g) == 0 || ecount(g) == 0) {
    cat("Year", year, "has no connected components after removing isolates, skipping.\n")
    next
  }
  
  # Louvain 社群检测
  community <- cluster_louvain(g)
  mem <- membership(community)
  
  community_results[[as.character(year)]] <- list(
    graph = g,
    communities = mem,
    modularity = modularity(community)
  )
  
  cat("Year", year, "processed with", length(unique(mem)), "communities.\n")
}

# 2. 合并所有年份的社群检测结果到 community_changes 数据框
community_changes <- data.frame(
  Year = integer(),
  Country = character(),
  Community = integer(),
  stringsAsFactors = FALSE
)

for (year in names(community_results)) {
  # membership 是向量，names(membership)是国家名称，向量值是社区编号
  mem <- community_results[[year]]$communities
  
  df_year <- data.frame(
    Year = as.integer(year),
    Country = names(mem),
    Community = as.integer(mem),
    stringsAsFactors = FALSE
  )
  
  community_changes <- rbind(community_changes, df_year)
}

# 3. 分析国家频繁切换（仅基于原始编号）
community_transitions <- community_changes %>%
  group_by(Country) %>%
  summarise(Transitions = n_distinct(Community)) %>%
  arrange(desc(Transitions))

cat("\nTop 10 Countries by raw 'Community' transitions (without label alignment):\n")
print(head(community_transitions, 10))

# 选取前 10 个变动最频繁的国家
top_countries <- community_transitions %>%
  slice(1:10) %>%
  pull(Country)

# 只对这些国家绘制社群变化时间线（原始 Community 标签）
top_community_changes <- community_changes %>%
  filter(Country %in% top_countries)

# 4. 绘制原始社群变化（不做任何label对齐）
library(ggplot2)

ggplot(top_community_changes, aes(x = Year, y = Community, group = Country, color = Country)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  labs(
    title = "Raw Community Transitions of Top 10 Countries (1980-2010)",
    x = "Year",
    y = "Raw Community Label"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    text = element_text(size = 14)
  )

####### 一致社群编号的改进 #######
# 引入匈牙利算法(线性分配)来对齐年与年之间的社区编号。
# 在这里我们先对全部国家做对齐示范——也可以只对 top_countries 做。

# 5. 首先，对所有节点年份分组 -> 计算"每年社区成员"的字典
all_years <- sort(unique(community_changes$Year))
community_membership <- list()
for (y in all_years) {
  this_year <- community_changes %>% filter(Year == y)
  # 按照 Community编号分组 -> 每个社区的成员列表
  community_membership[[as.character(y)]] <- split(this_year$Country, this_year$Community)
}

# 6. 准备一个 data.frame，在其中添加列 `ConsistentCommunity`
#   初始时，ConsistentCommunity = Community(跟原始一样)
community_changes <- community_changes %>%
  mutate(ConsistentCommunity = Community)

# 7. 逐年构建 Jaccard 矩阵 & 用匈牙利算法做最佳匹配
#    使 year-1 的 ConsistentCommunity 与 year 的 ConsistentCommunity 对齐

for (i in 2:length(all_years)) {
  year_cur <- all_years[i]
  year_prev <- all_years[i-1]
  
  prev_comm_list <- community_membership[[as.character(year_prev)]]
  curr_comm_list <- community_membership[[as.character(year_cur)]]
  
  # 如果本年或前一年没有社区(极端情况)，跳过
  if (length(prev_comm_list) == 0 || length(curr_comm_list) == 0) {
    next
  }
  
  # 构建 Jaccard 矩阵: 行=前一年社区, 列=本年社区
  matrix_jaccard <- matrix(0, 
                           nrow = length(prev_comm_list),
                           ncol = length(curr_comm_list))
  
  rownames(matrix_jaccard) <- names(prev_comm_list)  # 原始的社区编号（前一年）
  colnames(matrix_jaccard) <- names(curr_comm_list)  # 原始的社区编号（当年）
  
  for (r in seq_along(prev_comm_list)) {
    for (c in seq_along(curr_comm_list)) {
      intersect_size <- length(intersect(prev_comm_list[[r]], curr_comm_list[[c]]))
      union_size <- length(union(prev_comm_list[[r]], curr_comm_list[[c]]))
      if (union_size > 0) {
        matrix_jaccard[r, c] <- intersect_size / union_size
      } else {
        matrix_jaccard[r, c] <- 0
      }
    }
  }
  
  # 用匈牙利算法做最大匹配:
  # solve_LSAP() 最小化"成本"，但我们希望最大化 Jaccard，所以这里要把相似度转换成"成本" = 1 - 相似度
  cost_matrix <- 1 - matrix_jaccard
  
  # 如果行数 != 列数，需要做填充，使得矩阵变成方阵 (Hungarian通常要求方阵)
  nr <- nrow(cost_matrix)
  nc <- ncol(cost_matrix)
  if (nr > nc) {
    # 行多于列 -> 给列填补零列
    cost_matrix <- cbind(cost_matrix, 
                         matrix(1, nrow = nr, ncol = nr - nc)) 
    # 用1填充：相似度=0的意思，也可以用一些大数值
  } else if (nc > nr) {
    # 列多于行 -> 给行填补
    cost_matrix <- rbind(cost_matrix, 
                         matrix(1, nrow = nc - nr, ncol = nc))
  }
  
  # solve_LSAP 返回的是对每个行(前一年社区)分配到哪一列(今年社区)
  assignment <- solve_LSAP(cost_matrix)
  
  # assignment 得到的是一个向量 v，每个行 i 分配到列 v[i]
  # 但现在 cost_matrix 可能是个方阵扩充版，列的实际含义只有最前面 ncol(matrix_jaccard) 列
  # 因此需要区分：若 v[i] 超过真正社群列数，就表示无法有效匹配(被分配到填充列)
  
  # 构建一个 映射: "当年社区" -> "上年社区"
  # 先获取真正有效的列数 original_nc
  original_nc <- ncol(matrix_jaccard)
  
  # 行名 = rownames(matrix_jaccard)，列名 = colnames(matrix_jaccard)
  row_labels <- rownames(matrix_jaccard)  # 前一年的社区编号（字符）
  col_labels <- colnames(matrix_jaccard)  # 本年的社区编号（字符）
  
  # assignment 是行->列对应关系
  # 我们想要列->行，从而告诉“当前年的社区 col_j”究竟应该映射成上一年的哪个 ConsistentCommunity
  matched_mapping <- rep(NA, original_nc)
  names(matched_mapping) <- col_labels
  
  # 遍历前一年每个行i
  for (r in seq_len(nrow(matrix_jaccard))) {
    c_assigned <- assignment[r]  # solve_LSAP返回的列索引(在扩展矩阵中)
    if (c_assigned <= original_nc) {
      # col_labels[c_assigned] -> row_labels[r]
      matched_mapping[col_labels[c_assigned]] <- row_labels[r]
    }
  }
  
  # matched_mapping: 当前年的某个社区编号(字符) -> (可能)对齐到上一年的社区编号(字符)
  
  # 把这个映射应用到 community_changes 的ConsistentCommunity
  # step1: 找到 year_cur 当前年份所有记录
  # step2: 对每一行，对其原始 Community(字符) 找匹配
  # step3: 如果matched_mapping[原始社区ID]非空，就用它替换ConsistentCommunity，否则保留自己的编号
  
  # 先把社区编号都变成字符方便索引
  community_changes <- community_changes %>%
    mutate(CommChar = as.character(Community))  # 备用列
  
  # 对当前年份的所有行，更新 ConsistentCommunity
  # “上一年”已经确定了consistent编号(=上一年的ConsistentCommunity)，现在
  # 今年的ConsistentCommunity要继承上一年的ConsistentCommunity编号
  # 但 matched_mapping给出的只是"上一年社区ID"，我们得再查询"上一年的一致编号"是多少
  
  # 做一张 lookup 表：上一年(Year = year_prev)的【原始社区ID -> 一致编号(ConsistentCommunity)】
  prev_lookup_df <- community_changes %>%
    filter(Year == year_prev) %>%
    distinct(CommChar, ConsistentCommunity) %>%
    rename(PrevCommunityLabel = CommChar,
           PrevConsistent = ConsistentCommunity)
  
  # 现在处理当前年
  current_year_df <- community_changes %>% filter(Year == year_cur)
  current_year_df <- current_year_df %>%
    rowwise() %>%
    mutate(
      # matched_mapping[CommChar] = (上一年的 原始社区ID)
      # 如果 matched_mapping 为 NA，则说明无法匹配(可能是新社区)
      MappedPrevComm = matched_mapping[CommChar],
      
      # 如果成功匹配，则找出对应上一年的一致编号 PrevConsistent
      # 否则使用自身现有的ConsistentCommunity（表示新出现的社区，没法继承旧编号）
      ConsistentCommunity = if(!is.na(MappedPrevComm)) {
        # 在 prev_lookup_df 中找 MappedPrevComm 对应的 PrevConsistent
        tmp_cc <- prev_lookup_df %>% 
          filter(PrevCommunityLabel == MappedPrevComm) %>%
          pull(PrevConsistent)
        if(length(tmp_cc) == 1) tmp_cc else ConsistentCommunity
      } else {
        ConsistentCommunity
      }
    ) %>%
    ungroup()
  
  # 把更新后的 current_year_df 回写回去
  community_changes <- community_changes %>%
    filter(Year != year_cur) %>%
    bind_rows(current_year_df)
  
  # 更新 community_membership[[year_cur]] 也要一致
  # 便于后续年份能拿到正确的 ConsistentCommunity
  # 先把当前年份按 ConsistentCommunity 分组
  updated_splits <- current_year_df %>% 
    split(.$ConsistentCommunity) 
  
  # 这变成一个列表: names(updated_splits) = ConsistentCommunity
  # 但 community_membership[[as.character(year_cur)]] 这边还在用 "原始社区编号" 做索引
  # 为了在下一个循环中正确计算Jaccard(原始做法)需要保留原结构，
  # HOWEVER，我会另建一个 consistent_membership 用来存储对齐后的(ConsistentCommunity)成员
}

# 整理排序一下
community_changes <- community_changes %>%
  arrange(Year, Country)

# 8. 只看 top_countries 的一致编号可视化
top_community_changes_consistent <- community_changes %>%
  filter(Country %in% top_countries)

ggplot(top_community_changes_consistent, aes(x = Year, y = ConsistentCommunity, color = Country, group = Country)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(
    title = "Consistent Community Transitions (Hungarian Matching)",
    x = "Year",
    y = "Consistent Community Label"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    text = element_text(size = 14)
  )

# 查看最终数据
head(community_changes, 20)


# 现在要额外关注特定5个国家：
focus_countries <- c("JOR","CHN","IRN","TUR","DRC")

# 从 community_changes 中筛选这 5 个国家：
five_community_changes <- community_changes %>%
  filter(Country %in% focus_countries)

five_community_changes

# 然后用 ggplot 绘制一致编号的社群变化
library(ggplot2)

ggplot(five_community_changes, aes(x = Year, y = ConsistentCommunity, color = Country, group = Country)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(
    title = "Consistent Community Transitions for Selected Countries",
    x = "Year",
    y = "Consistent Community Label"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    legend.title = element_blank()
  )






library(dplyr)
library(networkDynamic)
library(sna)

# 假设 dynamic_network 已经构建好，包含 "military_spending_to_GDP" 属性
threshold <- 2  # 示例: 大于2%算高组

brokerage_summary <- data.frame()

for (year in 1980:2010) {
  # 1. 提取该年份的网络切片
  slice <- network.extract(dynamic_network, at = year)
  
  # 如果节点或边不足，跳过
  if (network.size(slice) < 2 || network.edgecount(slice) == 0) {
    cat("Year", year, "insufficient nodes/edges, skipping.\n")
    next
  }
  
  node_names <- network.vertex.names(slice)
  if (is.null(node_names) || length(node_names) == 0) {
    cat("Year", year, "no node names, skipping.\n")
    next
  }
  
  # 2. 获取军费GDP
  mil_spending <- get.vertex.attribute.active(
    dynamic_network, 
    "military_spending_to_GDP",
    at = year
  )
  
  # 如果全是NA，跳过
  if (all(is.na(mil_spending))) {
    cat("Year", year, "all NA in mil_spending, skipping.\n")
    next
  }
  
  # 确保长度匹配
  if (length(mil_spending) < length(node_names)) {
    mil_spending <- c(mil_spending, rep(NA, length(node_names) - length(mil_spending)))
  }
  
  # 3. 分组
  group_vector <- ifelse(mil_spending > threshold, "HighSpender", "LowSpender")
  group_vector[is.na(group_vector)] <- "Unknown"
  
  # 4. 调用 brokerage() (没有 roles/complete 参数)
  bres <- brokerage(slice, cl = group_vector)
  
  z_matrix <- bres$z.nli  # 标准化Z分数 => w_I, w_O, b_IO, b_OI, b_O, t
  if (ncol(z_matrix) == 0) {
    cat("Year", year, "z.nli is empty, skipping.\n")
    next
  }
  
  z_df <- as.data.frame(z_matrix)
  # 列名应是 c("w_I","w_O","b_IO","b_OI","b_O","t")
  # 其中 t 是前5列的合计Z分数
  
  z_df$Country <- node_names
  z_df$Year <- year
  
  # 5. 找到 t 值最高的节点
  # 't' 列在第6列，也可使用名字 z_df$t
  if (!"t" %in% colnames(z_df)) {
    cat("Year", year, "has no 't' column, skipping.\n")
    next
  }
  
  top_broker <- z_df %>%
    arrange(desc(t)) %>%
    slice_head(n = 1)  # 只取“t值最高的节点”
  
  brokerage_summary <- rbind(brokerage_summary, top_broker)
  
  cat("Year", year, "processed. top-broker =", top_broker$Country, 
      " t =", top_broker$t, "\n")
}

# 转换 Year 为 factor，这样在离散轴上更好显示
brokerage_summary <- brokerage_summary %>%
  mutate(Year = factor(Year))

# 绘制条形图，每个年份一个柱子
library(RColorBrewer)
library(ggplot2)

ggplot(brokerage_summary, aes(x = Year, y = t, fill = Country)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(
    aes(label = Country), 
    color = "black",             # Pastel色系下用黑字更清晰
    size = 2, 
    fontface = "bold",
    position = position_stack(vjust = 0.5)
  ) +
  scale_fill_brewer(palette = "Pastel1") +  # 柔和的浅色调
  labs(
    title = "Yearly Top Broker by 't' Value",
    x = "Year",
    y = "Brokerage Z-score (t)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5),
    legend.position = "none"
  )

