library(tidyverse)
library(stats)
library(factoextra)

players <- read_csv("~/Downloads/archive (5)/players_22.csv")


# Attacking players in the EPL
EPL <- players %>%
  filter(league_name == "English Premier League") %>% 
  filter(str_detect(player_positions, 'ST|CF|LW|RW')) %>% 
  select(short_name, club_name, nationality_name,overall, potential, value_eur, wage_eur, age,
         height_cm, weight_kg, weak_foot, work_rate,
         skill_moves, pace, shooting, passing, dribbling, defending,
         physic, attacking_crossing, attacking_finishing, attacking_heading_accuracy,
         attacking_short_passing, attacking_volleys) %>% 
  mutate(across(where(is.numeric), scale))

EPL %>%
  select(everything()) %>% 
  summarise_all(funs(sum(is.na(.))))

EPL2 <- EPL %>% 
  select(where(is.numeric)) %>% 
  as.data.frame()

rownames(EPL2) <- EPL$short_name


# hclust

EPLdist <- dist(EPL2, method = "euclidean")
HCepl <- hclust(EPLdist, method = "ward.D") 
plot(HCepl, cex = 0.6)


# k-means

EPLk3 <- kmeans(EPL2, centers = 3, nstart = 25)
fviz_cluster(EPLk3, data = EPL2)  

fviz_nbclust(EPL2, kmeans, method = "wss")




# factor analysis


EPLfact <- players %>%
  filter(league_name == "English Premier League") %>% 
  filter(str_detect(player_positions, 'ST|CF|LW|RW')) %>% 
  select(short_name, club_name, nationality_name,overall, potential, value_eur, wage_eur, age,
         height_cm, weight_kg, weak_foot, work_rate,
         skill_moves, pace, shooting, passing, dribbling, defending,
         physic, attacking_crossing, attacking_finishing, attacking_heading_accuracy,
         attacking_short_passing, attacking_volleys)

EPLfact <- EPLfact %>% 
  select(where(is.numeric)) %>% 
  as.data.frame()

rownames(EPLfact) <- EPL$short_name

fa_size <- lapply(1:14, function(f) factanal(EPLfact, f, lower = 0.05))
pvals <- lapply(1:14, function(f) factanal(EPLfact, f, lower = 0.05)$PVAL)
pvals
fa_size[[9]]$loadings
est_corr5 <- fa_size[[9]]$loadings %*% t(fa_size[[9]]$loadings) + diag(fa_size[[9]]$uniquenesses)
round(est_corr5 - cor(EPL2), 2)

scores <- factanal(EPLfact, factors = 9, lower = 0.05, scores = "regression")
  
factanal(EPLfact, factors = 9, lower = 0.05, scores = "regression")$scores %>%
  bind_cols(Player = rownames(EPLfact)) %>%
  slice(1:100) %>% 
  ggplot(aes(Factor1, Factor2, label = Player)) +
  geom_text()
