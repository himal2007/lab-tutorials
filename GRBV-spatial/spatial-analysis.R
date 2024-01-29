# Spatial autocorrelation analysis for the GRBV data

# Load libraries
library(tidyverse) 
library(r4pde) 
library(raster)

# Import the data from a csv file
data <- read.csv("data.csv", row.names = 1)
head(data)

# Plot the data
data_matrix <- as.matrix(data)

# Convert to raster 
mapS2 <- terra::rast(matrix(data_matrix, nrow(data_matrix), ncol(data_matrix), byrow = FALSE))
# Convert to data frame
mapS3 <- terra::as.data.frame(mapS2, xy = TRUE)
mapS3 |>
  ggplot(aes(x, y, label = lyr.1, fill = factor(lyr.1))) +
  geom_tile(color = "white", linewidth = 0.5) +
  theme_void() +
  coord_fixed()+
  labs(fill = "Status") +
  scale_fill_manual(values = c("gray70", "darkred"))+
  theme(legend.position = "top")

# Joint count test statistics
join_count(data_matrix)

# Moran's I test statistics
set.seed(100)
library(spdep)

nb <- cell2nb(nrow(data_matrix), ncol(data_matrix), type = "queen", torus = FALSE)

col.W <- nb2listw(nb, style = "W")
moran(x = data_matrix, # numeric vector
      listw = col.W, # the nb list
      n = nrow(data_matrix), # number of zones
      S0 = Szero(col.W)) # global sum of weights

# Moran's I test
moran.test(x = data_matrix, 
           listw = col.W)

# Spatial correlogram
correl_I <- sp.correlogram(nb, as.vector(data_matrix), 
                           order = 10,
                           method = "I",  
                           zero.policy = TRUE)

df_correl <- data.frame(correl_I$res) |> 
  mutate(lag = c(1:10))
# Show the spatial autocorrelation for 10 distance lags
round(df_correl$X1,3)

df_correl |>
  ggplot(aes(lag, X1)) +
  geom_col(fill = "darkred") +
  theme_r4pde()+
  scale_x_continuous(n.breaks = 10) +
  labs(x = "Distance lag", y = "Spatial autocorrelation")

## Software version
sessionInfo()