library(tidyverse)
# read in data
X = read.table("q4/X.dat")


create_centroids = function(k=3) 
  data.frame(iter=1,
             label=factor(sample(1:k)),
             x=runif(k,min=min(X$V1),max=max(X$V1)),
             y=runif(k,min=min(X$V2),max=max(X$V2)))

distance = function(x1,y1,x2,y2) 
  ((x1-x2)^2 + (y1-y2)^2)^0.5

get_nearest_centroid = function(centroid_df, X) {
  cdf = centroid_df
  cdf$bool = TRUE
  xdf = X
  xdf$bool = TRUE
  df = cdf %>% 
    #cross join cdf and xdf
    inner_join(xdf, by = 'bool') %>%
    # calculate distance to from every point to every centroid
    mutate(distance = distance(x,y,V1,V2)) %>%
    # get nearest centroid to each point
    group_by(V1,V2) %>%
    mutate(rank = rank(distance)) %>%
    filter(rank == 1) %>%
    select(iter, label, x,y,V1,V2,distance)
  df
}

avg_distance = function(df) mean(df$distance)

get_new_centroids = function(df) {
  df = df %>% 
    group_by(label) %>%
    summarise(iter = min(iter) + 1,
              x = mean(V1), 
              y = mean(V2))
  df
}

centroids = create_centroids()
centroid_assignment = get_nearest_centroid(centroids, X)
avg_dist = avg_distance(centroid_assignment)
new_centroids = get_new_centroids(centroid_assignment)
centroid_assignment = get_nearest_centroid(new_centroids, X)

kmeans = function(X,k=3) {
  tolerance = 0.05
  delta = Inf
  centroids = create_centroids(k=k)
  centroid_assignment = get_nearest_centroid(centroids, X)
  df_iters = centroid_assignment
  dist0 = avg_distance(centroid_assignment)
  while (delta > tolerance ) {
    centroids = get_new_centroids(centroid_assignment)
    centroid_assignment = get_nearest_centroid(centroids, X)
    df_iters = df_iters %>% bind_rows(centroid_assignment)
    dist1 = avg_distance(centroid_assignment)
    delta = abs(dist1 - dist0)/dist0
    dist0 = dist1
  }
  df_iters
}

kmeans_iters = kmeans(X, k=4)
gg = ggplot(kmeans_iters, aes(x = V1, y = V2, color = label)) + 
  geom_point() +
  facet_wrap(~ iter) +
  scale_color_discrete(guide="none")
ggsave("clusters_with_k4.png", gg)











