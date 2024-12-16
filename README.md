# **README**

## **Overview**
This project uses dynamic network analysis to study international defense cooperation networks from 1980 to 2010. The analysis focuses on identifying patterns, relationships, and key countries in the network using various centrality measures, correlation analysis, community detection, and brokerage analysis. The project also provides interactive visualizations to better understand the temporal evolution of these networks.

---

## **Features**

### **1. Dynamic Network Construction**
- **Data Input**: 
  - Bilateral defense agreements (`DCAD_endyear_cleaned.csv`) spanning 1980 to 2010.
  - Military expenditure as a percentage of GDP (`military_spending_cleaned.csv`).
- **Edge Attributes**: Agreements are represented as weighted edges, where weights reflect the number of active agreements in a given year.
- **Node Attributes**: Nodes represent countries, with attributes for military spending as a percentage of GDP.
- **Dynamic Network**: The network evolves over time, with agreements being added and removed based on their active years.

### **2. Visualization of Dynamic Networks**
- **Network Snapshots**: Visualize the network for a specific year.
- **Node Properties**:
  - Colors indicate military spending levels.
  - Sizes reflect military spending scaled between a predefined range.
- **Edge Properties**:
  - Thickness corresponds to edge weights.
  - Color changes for weights above a threshold (e.g., red for weights > 1).
- **Interactive Features**:
  - Display node and edge attributes (e.g., military spending and edge weights) on hover.
  - Include time slider and playback controls for exploring the network evolution.

### **3. Centrality Analysis**
- **Metrics**:
  - Degree Centrality
  - Betweenness Centrality
  - Eigenvector Centrality
  - Closeness Centrality (computed on the largest connected component).
- **Temporal Trends**:
  - Track centrality changes over time for specific countries (e.g., USA, China, Russia).
  - Identify patterns in centrality for countries with high military spending (> 15% of GDP).
- **Visualizations**:
  - Line plots for each centrality metric over time.

### **4. Correlation Analysis**
- **Objective**: Explore the relationship between military spending (% of GDP) and centrality metrics.
- **Method**: Compute Pearson correlation coefficients for each year (1982-2010) across four centrality metrics.
- **Visualization**: 
  - Line plots showing the temporal trends of correlations for each centrality measure.

### **5. Community Detection**
- **Algorithm**: Louvain method for modularity-based community detection.
- **Temporal Analysis**:
  - Track community assignments of countries over time.
  - Highlight countries that frequently change communities.
- **Advanced Techniques**:
  - Hungarian algorithm for aligning community labels across years.
- **Visualizations**:
  - Community changes for specific countries (e.g., Jordan, China, Iran, Turkey, DRC).

### **6. Brokerage Analysis**
- **Framework**: Gould-Fernandez method to classify broker roles (e.g., coordinator, representative).
- **Group Division**: Countries are divided into two groups based on military spending (> 2% and ≤ 2% of GDP).
- **Insights**:
  - Identify key broker countries (e.g., USA) and their sustained roles in connecting groups.
- **Visualization**:
  - Bar plots showing t-values for different brokerage roles.

---

## **How to Use**
1. **Prepare Data**: Ensure that input files (`DCAD_endyear_cleaned.csv` and `military_spending_cleaned.csv`) are correctly formatted and placed in the working directory.
2. **Run Scripts (Rmd file)**:
   - Construct the dynamic network.
   - Perform centrality, correlation, community detection, and brokerage analyses.
   - Generate visualizations for network evolution and analysis results.
3. **Explore Visualizations**:
   - Open the HTML file (`network_animation_with_labels_final.html`) to interactively explore the dynamic network.

---

## **Dependencies**
- **R Packages**:
  - `dplyr`: Data manipulation
  - `networkDynamic`: Dynamic network construction
  - `ndtv`: Dynamic network visualization
  - `sna`: Centrality computations
  - `igraph`: Graph-based computations (e.g., closeness centrality)
  - `ggplot2`: Data visualization
  - `tidyr`: Data reshaping

---

# **Acknowledgments**

Special thanks to Professor Brandon Kinne and the University of California, Davis, for making the **Defense Cooperation Agreement Dataset (DCAD)** available, which served as the cornerstone of this research. The dataset can be accessed via the following link: [Defense Cooperation Agreement Dataset](https://correlatesofwar.org/data-sets/defense-cooperation-agreement-dataset/).
