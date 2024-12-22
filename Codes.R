# Load necessary libraries
library(randomForest)
library(ggplot2)
library(doParallel)
library(dplyr)
library(packcircles)
library(gridExtra)

# 1. Data Preprocessing
# Load the data
data <- read.csv("./dataset.csv")
data <- data[, !colnames(data) %in% "X"]
summary(data)

# 确保 track_genre 是分类变量
data$track_genre <- as.factor(data$track_genre)

# 计算每个类别的计数和占比
genre_distribution <- data %>%
  group_by(track_genre) %>%
  summarise(count = n()) %>%
  mutate(percentage = (count / sum(count)) * 100) %>%
  arrange(desc(percentage))

# 查看分布数据
print(genre_distribution)

# 查看流行度 (popularity) 的分布
ggplot(data, aes(x = popularity)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "white") +
  labs(title = "Distribution of Popularity",
       x = "Popularity", y = "Frequency")

# Handle missing values
data$duration_ms[is.na(data$duration_ms)] <- median(data$duration_ms, na.rm = TRUE)
data$danceability[is.na(data$danceability)] <- mean(data$danceability, na.rm = TRUE)

# Standardize numerical features
data$loudness <- scale(data$loudness)
data$tempo <- scale(data$tempo)

# Convert categorical variables to factors
data$track_genre <- as.factor(data$track_genre)
data$explicit <- as.numeric(data$explicit == "true")
data$key <- as.factor(data$key)
#data$mode <- as.factor(data$mode)
data$mode <- as.numeric(data$mode)


# 2. Calculate and plot the highest average popularity by genre
genre_popularity <- aggregate(popularity ~ track_genre, data = data, FUN = mean)
genre_popularity <- genre_popularity[order(-genre_popularity$popularity),]
top_genres <- head(genre_popularity$track_genre, 3) # Select top 3 genres

# 调整图表尺寸和字体
p <- ggplot(genre_popularity, aes(x = reorder(track_genre, popularity), y = popularity, fill = popularity)) +
  geom_bar(stat = "identity", width = 0.8) +  # 调整条形宽度
  scale_fill_gradient(low = "lightblue", high = "darkblue") +  # 优化颜色方案
  labs(title = "Average Popularity by Genre (All Genres)",
       x = "Average Popularity",
       y = "Genre") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 8),  # 调整字体大小
    axis.text.y = element_text(size = 7),  # 调整字体大小
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "right"  # 将图例放置在右侧，节省空间
  ) +
  coord_flip()  # 使用水平条形图

# 增大绘图窗口大小
ggsave("popularity_by_genre.png", p, width = 12, height = 10)  # 保存为较大的图像

# 显示图表
print(p)

# Prepare the data for bubble chart
genre_popularity <- aggregate(popularity ~ track_genre, data = data, FUN = mean)
genre_popularity <- genre_popularity[order(-genre_popularity$popularity),]

# 使用 `packcircles` 计算圆形布局
packing <- circleProgressiveLayout(genre_popularity$popularity, sizetype = "radius")
genre_popularity <- cbind(genre_popularity, packing)

# Generate a dataframe for ggplot
dat.gg <- circleLayoutVertices(packing, npoints = 50)

# Plotting the bubble chart
p <- ggplot() +
  geom_polygon(data = dat.gg, aes(x, y, group = id, fill = as.factor(id)), color = "black", alpha = 0.6) +
  geom_text(data = genre_popularity, aes(x, y, label = track_genre), size = 3) +
  theme_void() +
  labs(title = "Average Popularity by Genre (Bubble Chart)") +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "none"
  ) +
  scale_fill_brewer(palette = "Set3")

# Save the bubble chart
ggsave("bubble_chart_genres.png", p, width = 10, height = 10)

# Display the chart
print(p)

# 3. Random Forest Analysis for Top 3 Genres
# Set random seed for reproducibility
set.seed(123)

# Register parallel processing
registerDoParallel(cores = 4)

# Initialize an empty list to store the plots
plot_list <- list()

# Analysis for each top genre
for (genre in top_genres) {
  genre_data <- subset(data, track_genre == genre)
  model <- randomForest(popularity ~ explicit + danceability + energy + key + loudness +
                          mode + speechiness + acousticness + instrumentalness +
                          liveness + valence + tempo + time_signature, 
                        data = genre_data, importance = TRUE, ntree = 300)
  
  importance_data <- importance(model)
  importance_df <- data.frame(Feature = rownames(importance_data), Importance = importance_data[,1])
  importance_df <- importance_df[order(-importance_df$Importance),]
  
  # Generate a plot for the current genre
  p <- ggplot(importance_df, aes(x = reorder(Feature, Importance), y = Importance, fill = Importance)) +
    geom_col() +
    scale_fill_gradient(low = "pink", high = "red") +
    labs(title = paste("Importance in", genre),  # Shortened title
         x = "Importance",
         y = "Features") +
    theme_minimal() +
    theme(axis.title.y = element_blank(),
          plot.title = element_text(face = "bold", hjust = 0.5, size = 10),  # Adjust title size
          legend.position = "none") +
    coord_flip()
  
  # Append the plot to the list
  plot_list[[genre]] <- p
}

# Use cowplot to plot all plots in one row, adjust plot margins
plot_grid(plotlist = plot_list, ncol = length(plot_list), align = 'v', 
          rel_widths = c(1, 1, 1)) # Adjust relative widths if needed

# Calculate and plot the highest average popularity by genre
genre_popularity <- aggregate(popularity ~ track_genre, data = data, FUN = mean)
top_genres <- head(genre_popularity[order(-genre_popularity$popularity), "track_genre"], 3)
print(top_genres)

# Initialize an empty data frame to store correlation data for all top genres
all_cor_data <- data.frame(Feature = character(), Correlation = numeric(), Genre = character())

# Analyze correlations for each top genre and combine the data
for (genre in top_genres) {
  genre_data <- subset(data, track_genre == genre)
  
  # Ensure numeric columns are selected for correlation
  numeric_columns <- genre_data[, sapply(genre_data, is.numeric)]
  cor_matrix <- cor(numeric_columns, use = "complete.obs")
  
  # Extract correlation with 'popularity'
  cor_data <- as.data.frame(cor_matrix["popularity", -which(colnames(cor_matrix) == "popularity")], stringsAsFactors = FALSE)
  names(cor_data) <- c("Correlation")
  cor_data$Feature <- rownames(cor_data)
  cor_data$Genre <- genre  # Add genre information
  
  all_cor_data <- rbind(all_cor_data, cor_data)  # Combine with previous data
}

print(all_cor_data)
# Plotting all correlation data in one plot with points
p <- ggplot(all_cor_data, aes(x = Feature, y = Correlation, color = Genre)) +
  geom_point(size = 4, alpha = 0.8) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Feature Correlation with Popularity across Top Genres",
       x = "Features", y = "Correlation",
       color = aes("Music Genre") )  # Adjust genre colors as needed

print(p)  # Display the plot

####

library(dplyr)
library(reshape2)
library(ggplot2)


#################################################
# 确认 top_genres 的正确性
print(top_genres)  # 确保这里只有 pop-film, kpop, chill

# 找到每个类别中最流行的歌手
top_artists <- data.frame()

for (genre in top_genres) {
  genre_data <- subset(data, track_genre == genre)
  
  # 找到该类别中流行度最高的歌手
  top_artist <- genre_data %>%
    group_by(artists) %>%
    summarise(avg_popularity = mean(popularity, na.rm = TRUE), song_count = n()) %>%  # 统计歌曲数量
    arrange(desc(avg_popularity)) %>%
    slice(1) # 取最高流行度的歌手
  
  top_artist$track_genre <- genre
  top_artists <- rbind(top_artists, top_artist)
}

# 打印最流行的歌手及其平均流行度和歌曲数量
print(top_artists)

# 可视化代码（保持原样）
selected_artists_data <- data %>%
  filter(artists %in% top_artists$artists & track_genre %in% top_genres) %>%
  select(artists, track_genre, explicit, danceability, energy, key, loudness, 
         mode, speechiness, acousticness, instrumentalness, liveness, valence, tempo, time_signature)

melted_data <- reshape2::melt(selected_artists_data, id.vars = c("artists", "track_genre"))

ggplot(melted_data, aes(x = variable, y = value, fill = artists)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ track_genre, nrow = 1, scales = "free_y") +
  theme_minimal() +
  labs(title = "Top Artists' Music Feature Profiles Across Genres",
       x = "Features", y = "Value",
       fill = "Artist") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(face = "bold", hjust = 0.5))
#####
# 确认 top_genres 的正确性
print(top_genres)  # 确保这里只有 pop-film, kpop, chill

# 找到每个类别中最流行的歌手
top_artists <- data.frame()

for (genre in top_genres) {
  genre_data <- subset(data, track_genre == genre)
  
  # 找到该类别中流行度最高的歌手
  top_artist <- genre_data %>%
    group_by(artists) %>%
    summarise(avg_popularity = mean(popularity, na.rm = TRUE), song_count = n()) %>%  # 统计歌曲数量
    arrange(desc(avg_popularity)) %>%
    slice(1) # 取最高流行度的歌手
  
  top_artist$track_genre <- genre
  top_artists <- rbind(top_artists, top_artist)
}

# 打印最流行的歌手及其平均流行度和歌曲数量
print(top_artists)

# 提取这些歌手的特征数据
selected_artists_data <- data %>%
  filter(artists %in% top_artists$artists & track_genre %in% top_genres) %>%
  select(artists, track_genre, explicit, danceability, energy, key, loudness, 
         mode, speechiness, acousticness, instrumentalness, liveness, valence, tempo, time_signature)

# 转换数据为长格式
melted_data <- reshape2::melt(selected_artists_data, id.vars = c("artists", "track_genre"))

# 统一 Y 轴标尺的图表
ggplot(melted_data, aes(x = variable, y = value, fill = artists)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ track_genre, nrow = 1, scales = "fixed") +  # 修改 scales 为 "fixed"
  theme_minimal() +
  labs(title = "Top Artists' Music Feature Profiles Across Genres (Unified Y-axis)",
       x = "Features", y = "Value",
       fill = "Artist") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(face = "bold", hjust = 0.5))
