---
# SNP and Genetic Variation

 
# Download data

data <- read.csv ('/Users/sanzidaakhteranee/Documents/HackBio_Contest/Phase_2/Data.csv', header=TRUE, sep =',')
print(data)
```

# General statistics

#one sample t-test for minor allele frequency  
t.test (data$EFFECT_ALLELE_FREQ, value=0.01, alternative ='greater')


#Report: general statistics

Null Hypothesis = SNP p value = 0.01
Alt Hypothesis  = height variability >0.01
so, p value less than 0.01 means we can reject null hypothesis and 
accept alternative hypothesis


#SNP significant p value over all populations

SNP = unique(data$SNPID) #split data based on SNPID

new_data <- data.frame() #create new data frame

for (i in SNP){ 
  subset_data <- data[data$SNPID == i, ] # create subset_data for looping SNPID iteration
  if (!any(is.na(subset_data$P)) && !any(is.na(subset_data$EFFECT_ALLELE_FREQ))) {
    if (any(subset_data$P < 0.01) && any(subset_data$EFFECT_ALLELE_FREQ > 0.01)){ 
      new_data <- rbind(new_data, subset_data) # combind two data together
    } }
} 
print(new_data) 
View(new_data)
```

#Report for SNP p value

From above code, it shows total 2281 observations that are significantly 
different SNP among all super populations


# Data partitions among 5 different populations

partitions <- split(data, data$ANCESTRY)
print(partitions)
```

# PCA analysis for EUROPEAN populations

population=partitions$EUROPEAN
population

my_data <- data.frame (geno=data$EFFECT_ALLELE, pop='population')
my_data
my_data <- matrix(rnorm(2500), ncol =2)

# Perform PCA
pca_result <- prcomp(my_data, scale. = TRUE)

# Summary of PCA results
summary(pca_result)

# Biplot for visualization
biplot(pca_result)
```

# PCA analysis for AFRICAN populations
```{r}
population=partitions$AFRICAN
population

my_data <- data.frame (geno=data$EFFECT_ALLELE, pop='population')
my_data
my_data <- matrix(rnorm(2500), ncol =2)

# Perform PCA
pca_result <- prcomp(my_data, scale. = TRUE)

# Summary of PCA results
summary(pca_result)

# Biplot for visualization
biplot(pca_result)
```


# PCA analysis for SOUTH ASIA populations

population=partitions$SOUTH_ASIA
population

my_data <- data.frame (geno=data$EFFECT_ALLELE, pop='population')
my_data
my_data <- matrix(rnorm(2500), ncol =2)

# Perform PCA
pca_result <- prcomp(my_data, scale. = TRUE)

# Summary of PCA results
summary(pca_result)

# Biplot for visualization
biplot(pca_result)
```


$PCA analysis for EAST ASIA populations
```{r}
population=partitions$EAST_ASIA
population

my_data <- data.frame (geno=data$EFFECT_ALLELE, pop='population')
my_data
my_data <- matrix(rnorm(2500), ncol =2)

# Perform PCA
pca_result <- prcomp(my_data, scale. = TRUE)

# Summary of PCA results
summary(pca_result)

# Biplot for visualization
biplot(pca_result)
```

#PCA analysis for HISPANIC populations
```{r}
population=partitions$HISPANIC
population

my_data <- data.frame (geno=data$EFFECT_ALLELE, pop='population')
my_data
my_data <- matrix(rnorm(2500), ncol =2)

# Perform PCA
pca_result <- prcomp(my_data, scale. = TRUE)

# Summary of PCA results
summary(pca_result)

# Biplot for visualization
biplot(pca_result)