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

# 创建全局节点映射
all_nodes <- unique(c(dcad$country1, dcad$country2))
node_mapping <- data.frame(name = all_nodes, id = seq_along(all_nodes))

# 初始化动态网络并设置节点名称
base_network <- network.initialize(n = length(all_nodes), directed = FALSE)
base_network %v% "vertex.names" <- node_mapping$name
dynamic_network <- networkDynamic(base.net = base_network)

# 添加每年的边和节点属性到动态网络
for (year in 1980:2010) {
  # 获取当前年份的协议数据
  edge_data <- dcad %>% filter(Year == year)
  
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
      type = paste(unique(type), collapse = ", "),
      asymmetry = paste(unique(asymmetry), collapse = ", "),
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
  
  activate.edge.attribute(
    dynamic_network, 
    prefix = "type", 
    value = edges$type, 
    onset = year, 
    terminus = year + 1
  )
  
  activate.edge.attribute(
    dynamic_network, 
    prefix = "asymmetry", 
    value = edges$asymmetry, 
    onset = year, 
    terminus = year + 1
  )
}

render.d3movie(dynamic_network, 
               displaylabels = TRUE, 
               label = "vertex.names", 
               vertex.cex = 0.6, 
               vertex.col = "blue", 
               edge.col = "black")


print(get.edge.attribute.active(dynamic_network, "weight", at = 1981))
print(get.edge.attribute.active(dynamic_network, "type", at = 1981))
print(get.edge.attribute.active(dynamic_network, "asymmetry", at = 1981))
print(get.vertex.attribute.active(dynamic_network,"military_spending_to_GDP", at = 1981))

# 设置节点颜色（基于 GDP 开支）
vertex_colors <- sapply(get.vertex.attribute.active(dynamic_network, 
                                                    "military_spending_to_GDP", 
                                                    at = 2007),
                        function(gdp) {
                          if (is.na(gdp)) return("gray")  # 无数据节点为灰色
                          if (gdp > 5) return("red")  # 高开支节点为红色
                          if (gdp > 2) return("orange")  # 中等开支节点为橙色
                          return("green")  # 低开支节点为绿色
                        })

# 设置节点大小（归一化处理）
gdp_values <- get.vertex.attribute.active(dynamic_network, 
                                          "military_spending_to_GDP", 
                                          at = 2007)

# 归一化节点大小到 5-20 范围
min_size <- 0.3
max_size <- 2
gdp_normalized <- scales::rescale(gdp_values, to = c(min_size, max_size), na.rm = TRUE)

# 如果某些值仍然 NA，则设置为最小大小
vertex_sizes <- ifelse(is.na(gdp_normalized), min_size, gdp_normalized)

render.d3movie(
  dynamic_network,
  displaylabels = TRUE,
  label = "vertex.names",
  vertex.col = vertex_colors,  # 基于 GDP 高亮
  vertex.cex = vertex_sizes,  # 调整节点大小
  edge.col = "black"
)

# 准备节点工具提示
vertex_tooltips <- sapply(
  get.vertex.attribute.active(dynamic_network, "military_spending_to_GDP", at = 2007),
  function(gdp) {
    if (is.na(gdp)) return("No Data")
    paste0("Military Spending (%GDP): ", round(gdp, 2))
  }
)

# 准备边工具提示
edge_weights <- get.edge.attribute.active(dynamic_network, "weight", at = 2007)
edge_types <- get.edge.attribute.active(dynamic_network, "type", at = 2007)
edge_asymmetry <- get.edge.attribute.active(dynamic_network, "asymmetry", at = 2007)

edge_colors <- ifelse(edge_weights > 1, "red", "gray")
edge_widths <- 1 + edge_weights / 2
edge_tooltips <- mapply(
  function(weight, type, asymmetry) {
    paste0("Weight: ", weight, "<br>Type: ", type, "<br>Asymmetry: ", asymmetry)
  },
  weight = edge_weights, type = edge_types, asymmetry = edge_asymmetry
)

# 渲染动态网络
render.d3movie(
  dynamic_network,
  displaylabels = TRUE,
  label = "vertex.names",
  vertex.col = vertex_colors,  # 基于属性调整颜色
  vertex.cex = vertex_sizes,  # 动态调整节点大小
  edge.col = edge_colors,     # 动态调整边颜色
  edge.lwd = edge_widths,     # 动态调整边宽度
  vertex.tooltip = vertex_tooltips,  # 添加节点工具提示
  edge.tooltip = edge_tooltips,      # 添加边工具提示
  render.par = list(
    tween.frames = 15,  # 每帧插值数量，增加动画平滑度
    show.time = TRUE    # 显示时间标签
  ),
  d3.options = list(
    animationDuration = 1000,  # 每步动画持续时间（毫秒）
    playControls = TRUE,       # 显示播放控制
    slider = TRUE,             # 显示时间滑块
    durationControl = TRUE     # 显示速度调节控制
  ),
  output.mode = "HTML",        # 输出 HTML 文件
  script.type = "embedded"     # 嵌入 JavaScript
)

################################################################
##########################data analysis########################
##############################################################
# 1. centrality analysis
# 加载必要的包
library(sna)
library(ggplot2)

# 初始化存储中心性结果的表
centrality_data <- data.frame(
  Year = integer(),
  Country = character(),
  Degree = numeric(),
  Betweenness = numeric(),
  Closeness = numeric()
)

# 遍历每年，计算中心性指标
for (year in 1980:2010) {
  # 提取该年份的网络切片
  network_slice <- network.extract(dynamic_network, at = year)
  
  # 获取节点名称
  node_names <- network.vertex.names(network_slice)
  
  # 计算中心性
  degree <- sna::degree(network_slice, gmode = "graph")
  betweenness <- sna::betweenness(network_slice, gmode = "graph")
  
  # 存储中心性数据
  year_data <- data.frame(
    Year = rep(year, length(node_names)),
    Country = node_names,
    Degree = degree,
    Betweenness = betweenness,
  )
  
  # 合并数据
  centrality_data <- rbind(centrality_data, year_data)
}

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

###############################################
# 2. Community Detection
# 加载必要的包
library(igraph)
library(networkDynamic)
library(dplyr)

# 初始化存储社群检测结果的列表
community_results <- list()

# 遍历每年的时间切片
for (year in 1980:2010) {
  # 提取当前年份的动态网络切片
  network_slice <- network.extract(dynamic_network, at = year)
  
  # 获取节点名称
  node_names <- network.vertex.names(network_slice)
  
  # 获取活动边及其年龄
  edge_ages <- dyads.age.at(network_slice, at = year, format.out = "edgelist")
  
  # 检查是否有活动边
  if (is.null(edge_ages) || nrow(edge_ages) == 0) {
    cat("Year", year, "has no active edges, skipping.\n")
    next
  }
  
  # 创建 edgelist 的节点名称
  edge_list_named <- data.frame(
    tails = node_names[edge_ages[, 1]],  # 使用节点 ID 映射名称
    heads = node_names[edge_ages[, 2]]
  )
  
  # 创建 igraph 对象
  g <- graph_from_data_frame(edge_list_named, directed = FALSE)
  
  # 移除孤立节点
  g <- delete.vertices(g, which(degree(g) == 0))
  
  # 检查是否还有边和节点
  if (vcount(g) == 0 || ecount(g) == 0) {
    cat("Year", year, "has no connected components after removing isolates, skipping.\n")
    next
  }
  
  # 社群检测（Louvain 方法）
  community <- cluster_louvain(g)
  
  # 保存社群结果
  community_results[[as.character(year)]] <- list(
    graph = g,
    communities = membership(community),
    modularity = modularity(community)
  )
  
  cat("Year", year, "processed with", length(unique(membership(community))), "communities.\n")
}

# 构建社群变化的数据框
community_changes <- data.frame(
  Year = integer(),
  Country = character(),
  Community = integer()
)

for (year in names(community_results)) {
  communities <- community_results[[year]]$communities
  community_changes <- rbind(
    community_changes,
    data.frame(
      Year = as.integer(year),
      Country = names(communities),
      Community = as.integer(communities)
    )
  )
}

# 分析哪些国家频繁更换社群
community_transitions <- community_changes %>%
  group_by(Country) %>%
  summarise(
    Transitions = n_distinct(Community)
  ) %>%
  arrange(desc(Transitions))

# 输出频繁更换社群的国家
print(head(community_transitions))

# 筛选切换次数最多的 10 个国家
top_countries <- community_transitions %>%
  arrange(desc(Transitions)) %>%
  slice(1:10) %>%
  pull(Country)

# 只对这些国家绘制社群变化时间线
top_community_changes <- community_changes %>%
  filter(Country %in% top_countries)

top_community_changes
# 绘制筛选国家的社群变化
library(ggplot2)
ggplot(top_community_changes, aes(x = Year, y = Community, group = Country, color = Country)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  labs(
    title = "Community Transitions of Top 10 Key Countries (1980-2010)",
    x = "Year",
    y = "Community"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    text = element_text(size = 14)
  )

########## consistent community calculation ############
# Jaccard 系数方法来解决跨时间社群编号不一致
# 计算每年社群成员
community_membership <- list()
for (year in 1980:2010) {
  current_year <- top_community_changes %>% filter(Year == year)
  community_membership[[as.character(year)]] <- split(current_year$Country, current_year$Community)
}

# 计算每年的社群匹配
jaccard_matrix <- list()
for (year in 1981:2010) {
  prev_year <- community_membership[[as.character(year - 1)]]
  curr_year <- community_membership[[as.character(year)]]
  
  # 初始化 Jaccard 矩阵
  matrix_jaccard <- matrix(0, nrow = length(prev_year), ncol = length(curr_year))
  rownames(matrix_jaccard) <- names(prev_year)
  colnames(matrix_jaccard) <- names(curr_year)
  
  # 计算每对社群的 Jaccard 系数
  for (i in seq_along(prev_year)) {
    for (j in seq_along(curr_year)) {
      matrix_jaccard[i, j] <- length(intersect(prev_year[[i]], curr_year[[j]])) / 
        length(union(prev_year[[i]], curr_year[[j]]))
    }
  }
  jaccard_matrix[[as.character(year)]] <- matrix_jaccard
}

# 根据 Jaccard 系数匹配社群编号
matched_communities <- list()
for (year in 1981:2010) {
  matrix_jaccard <- jaccard_matrix[[as.character(year)]]
  matches <- apply(matrix_jaccard, 2, function(col) which.max(col)) # 找到最佳匹配
  matched_communities[[as.character(year)]] <- matches
}

# 创建一个新的列存储一致编号
top_community_changes <- top_community_changes %>%
  mutate(ConsistentCommunity = Community)

# 更新社群编号
for (year in 1981:2010) {
  matches <- matched_communities[[as.character(year)]]
  
  # 获取当前年份和上一年份的社群数据
  curr_year <- top_community_changes %>% filter(Year == year)
  prev_year <- top_community_changes %>% filter(Year == year - 1)
  
  # 更新一致编号
  curr_year <- curr_year %>%
    mutate(ConsistentCommunity = matches[as.character(Community)])
  
  # 替换到主数据框
  top_community_changes <- top_community_changes %>%
    filter(Year != year) %>% 
    bind_rows(curr_year)
}

library(ggplot2)

top_community_changes

# 可视化一致编号的社群随时间的变化
ggplot(top_community_changes, aes(x = Year, y = ConsistentCommunity, color = Country)) +
  geom_point() +
  geom_line(aes(group = Country)) +
  labs(title = "Consistent Community Transitions Over Time",
       x = "Year",
       y = "Consistent Community") +
  theme_minimal()

################################################################################
################ corralation between military expenditure and DCA###############
################################################################################
# 创建保存结果的列表
# 初始化存储结果的列表和数据框
results <- list()
correlation_summary <- data.frame()

# 遍历每一年
for (year in 1980:2010) {
  # 提取该年份的网络切片
  network_slice <- network.extract(dynamic_network, at = year)
  
  # 获取节点名称和军事开支数据
  node_names <- network.vertex.names(network_slice)
  military_spending <- get.vertex.attribute.active(dynamic_network,"military_spending_to_GDP", at = year)
  
  # 计算中心性（确保使用 network 包的方法）
  degree <- sna::degree(network_slice, gmode = "graph")
  betweenness <- sna::betweenness(network_slice, gmode = "graph")
  
  # 创建数据框
  year_data <- data.frame(
    Country = node_names,
    Degree = degree,
    Betweenness = betweenness,
    MilitarySpendingGDP = military_spending,
    Year = year
  )
  
  # 过滤掉没有军事开支数据的节点
  year_data <- year_data %>% filter(!is.na(MilitarySpendingGDP))
  
  if (nrow(year_data) > 1) { # 确保至少有两个数据点进行相关性计算
    # 计算相关性
    cor_degree <- cor(year_data$Degree, year_data$MilitarySpendingGDP)
    cor_betweenness <- cor(year_data$Betweenness, year_data$MilitarySpendingGDP)
  } else {
    cor_degree <- NA
    cor_betweenness <- NA
  }
  
  # 保存结果
  results[[as.character(year)]] <- list(
    Data = year_data,
    Correlation = data.frame(
      Year = year,
      DegreeCorrelation = cor_degree,
      BetweennessCorrelation = cor_betweenness
    )
  )
  
  # 合并到总结数据框中
  correlation_summary <- rbind(
    correlation_summary,
    data.frame(
      Year = year,
      DegreeCorrelation = cor_degree,
      BetweennessCorrelation = cor_betweenness
    )
  )
}

# 绘制相关性趋势图
library(ggplot2)

correlation_summary %>%
  pivot_longer(cols = c(DegreeCorrelation, BetweennessCorrelation),
               names_to = "CentralityType", values_to = "Correlation") %>%
  ggplot(aes(x = Year, y = Correlation, color = CentralityType)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Centrality and change in the correlation of military expenditure /GDP",
    x = "Year",
    y = "correlation",
    color = "Centrality type"
  ) +
  theme_minimal()

######################################################
########################### brokerage ###########################
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

# 可视化总是作为中介的国家分布
consistent_brokers %>%
  top_n(10, TotalYears) %>%
  ggplot(aes(x = reorder(Country, -TotalYears), y = TotalYears, fill = AvgBetweenness)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(
    title = "Countries Always acting as an intermediary (1980-2010)",
    x = "Country",
    y = "Year",
    fill = "Average Betweenness"
  ) +
  theme_minimal()

