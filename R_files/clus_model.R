# Initialise ----------------------------------------------------------

library(rstudioapi)
library(dplyr)
library(cluster)
library(ggplot2)

set.seed(7004)
max_k = 20
best_k = 6
main_dir<- dirname(dirname(rstudioapi::getSourceEditorContext()$path))

# Load csv ----------------------------------------------------------------

datadir<- paste0(main_dir,"/data")
clus_data_path<- paste0(datadir,"/data_c.csv")

clus_data<- read.csv(clus_data_path)


# Explore data ------------------------------------------------------------

#head(clus_data)
#print(colnames(clus_data))
#print(nrow(clus_data))

num_obs<- nrow(clus_data)

print("Number and percentage of missing values by record:")

for (col in colnames(clus_data) ){
  num_na<- sum(is.na(clus_data[col]))
  perc_na<- 100*num_na/num_obs
  print(paste(col, num_na, perc_na))
}

### remove rows with missing location
clus_data<- clus_data[!is.na(clus_data$location), ]


# Summarise mean ----------------------------------------------------------

loc_data<- clus_data %>% group_by(location) %>%
  summarise(mean_so2 = mean(so2, na.rm=TRUE),
            mean_no2 = mean(no2, na.rm=TRUE),
            mean_rspm = mean(rspm, na.rm=TRUE),
            mean_spm = mean(spm, na.rm=TRUE)
  ) %>% data.frame()

#head(loc_data)


# Find and impute missing values ------------------------------------------
num_loc = nrow(loc_data)
print("Number and percentage of missing values by location:")

for (col in colnames(loc_data) ){
  num_na<- sum(is.na(loc_data[col]))
  perc_na<- round(100*num_na/num_loc, digits=2)
  print(paste(col, num_na, perc_na))
}

### impute for so2, no2 and rspm. 
### Drop spm col as more than 30% is missing

loc_data$mean_so2[is.na(loc_data$mean_so2)]= mean(loc_data$mean_so2, na.rm = TRUE)
loc_data$mean_no2[is.na(loc_data$mean_no2)]= mean(loc_data$mean_no2, na.rm = TRUE)
loc_data$mean_rspm[is.na(loc_data$mean_rspm)]= mean(loc_data$mean_rspm, na.rm = TRUE)

#### set location as row names
rownames(loc_data)<- loc_data$location

#### remove unnecessary columns
clean_loc_data<- loc_data %>% select(-c(mean_spm, location))

#head(clean_loc_data)

# Scale values ------------------------------------------------------------
### use standard scalar

#boxplot(clean_loc_data$mean_so2, main = "Boxplot for mean_so2")
#boxplot(clean_loc_data$mean_no2, main = "Boxplot for mean_no2")
#boxplot(clean_loc_data$mean_rspm, main = "Boxplot for mean_rspm")

scaled_loc_data<- clean_loc_data
for (mean_col in c("mean_so2", "mean_no2", "mean_rspm")){
 scaled_loc_data[mean_col]<- scale(scaled_loc_data[mean_col], center=TRUE, scale=TRUE)
}

#head(scaled_loc_data)


# Do clustering -----------------------------------------------------------
ss_val<- rep(NA, max_k-1)

for (k in c(2:max_k)){
  k_clus<- kmeans(scaled_loc_data, centers = k, nstart = 10)
  ss_val[k-1]<- k_clus$tot.withinss
}

ss_val_df<- data.frame(k=c(2:max_k), ss_val = ss_val)
ss_val_fig<- ss_val_df %>% ggplot(aes(x=k, y=ss_val)) + 
  geom_line()

#print(ss_val_fig)

clus_loc<- kmeans(scaled_loc_data, centers = best_k, nstart = 10)
clus_grp<- data.frame(clus_loc$cluster) %>% setNames("cluster")
merged_res<- merge(clean_loc_data, clus_grp, by ='row.names', all=TRUE)
merged_res$cluster<- as.factor(merged_res$cluster)

### center of each cluster
center_data<- merged_res %>% group_by(cluster) %>%
  summarise(cen_mean_so2 = mean(mean_so2, na.rm=TRUE),
            cen_mean_no2 = mean(mean_no2, na.rm=TRUE),
            cen_mean_rspm = mean(mean_rspm, na.rm=TRUE),
  ) %>% data.frame()

print("Center of each cluster:")
print(center_data)
# Visualise clustering ----------------------------------------------------


clus_vis_so2<- merged_res %>% ggplot(aes(x=cluster, y=mean_so2)) +
  geom_boxplot()
#print(clus_vis_so2)

clus_vis_no2<- merged_res %>% ggplot(aes(x=cluster, y=mean_no2)) +
  geom_boxplot()
#print(clus_vis_no2)

clus_vis_rspm<- merged_res %>% ggplot(aes(x=cluster, y=mean_rspm)) +
  geom_boxplot()
#print(clus_vis_rspm)


# Save plots --------------------------------------------------------------
figdir<- paste0(main_dir,"/figures")

# save elbow ss plot
ggsave(filename = "ss_val_elbow.png", ss_val_fig, path = figdir)

# save cluster vis
ggsave(filename = "so2_clus_boxplot.png", clus_vis_so2, path = figdir)
ggsave(filename = "no2_clus_boxplot.png", clus_vis_no2, path = figdir)
ggsave(filename = "rspm_clus_boxplot.png", clus_vis_rspm, path = figdir)
