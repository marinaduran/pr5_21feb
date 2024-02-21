getwd()
nuevo_dir <- "c:/pr5_21feb"
setwd(nuevo_dir)
getwd()
dir()
list.files()

# Estadística descriptiva II

# 1. 
library(ggplot2)

n <- 200

set.seed(123)

sites <- sample(paste0("Site", 1:10), n, replace = TRUE)

artifact_types <- sample(c("Pottery","Tools","Jewelry","Weapons"), n, replace = TRUE)

num_artifacts <- sample(1:1000, n, replace = TRUE)

contexts <- sample(c("Habitacional","Funerario","Others"), n, replace = TRUE)

latitude <- runif(n, min = 0, max = 90)
longitude <- runif(n, min = -180, max= 180)

archaeology_data <- data.frame(
  site = sites,
  artifact_type = artifact_types,
  num_artifacts = num_artifacts,
  context = contexts,
  latitude = latitude,
  longitude = longitude
)
View(archaeology_data)
str(archaeology_data)

# 2. 
mean(num_artifacts)

# 3. 
hist(num_artifacts, main = "Histograma")

mean(num_artifacts)
median(num_artifacts)

# 4. 
boxplot(num_artifacts, main = "Grafico de artefactos", border = "pink", horizontal = T)

# 5.
densidad = density(num_artifacts)
plot(densidad, main = "Grafica de artefactos", border = "violet")

# 6.
ggplot(archaeology_data, aes(x = longitude, y = latitude)) + 
  geom_bin2d() +
  labs(title = "Artifact Density Heatmap", x = "Longitude", y = "Latitude")

# 7. 
total_artifacts <- sum(archaeology_data$num_artifact)
print(total_artifacts)

# 8. 
median(total_artifacts)

# 9. 
sd(x=num_artifacts)

# 10.
which.max(total_artifacts)

# 11.
summary_table <- aggregate(num_artifacts ~ site, data = archaeology_data,
                           FUN = function(x) c(mean = mean(x), median = median(x), sd = sd(x)))
print(summary_table)

# 12.
boxplot(num_artifacts ~ site, data = archaeology_data
        )






