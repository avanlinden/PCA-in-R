### Performing PCA ===========

#spread vertData df so that each measurement has its own column

wideData <- vertData %>% 
  select(-measure, -vertebra, -value) %>% 
  spread(key = measureName, value = logValue) #now each specimen has its own row with 9 vertebral measurements

wideData

vertPCA <- prcomp(wideData[,6:14]) #perform the PCA on the measurement columns of the data frame

str(vertPCA)

summary(vertPCA) #this gives the variable loadings, SD, and proportion of variance for each component

#in this case, since the variables are not corrected by size and the sample has everything from pygmy antelopes to bison, PC1 explains ~90% of the variance and is dominated by size

#within the prcomp object:

vertPCA$rotation # this is the matrix of variable loadings (eigenvectors)

vertPCA$x #this gives the PC scores for each specimen on each component (centered data multipled by the rotation matrix) (row numbers correspond to the initial data rows)

vertPCA$sdev #standard deviations of each PC, aka square roots of eigenvalues

# a quick screeplot of the variances for each PC
screeplot(vertPCA)

### Visualizing PCA results =========

# I like to rejoin my PC scores for each specimen with the original dataframe, so that I can include things like the species name, sex, etc in the visualizations

vertPCscores <- vertPCA$x %>% 
  as_tibble() %>% #convert PC scores to tibble
  add_column(catNum = wideData$catNum) %>%  #add catalog numbers from df used for PCA (unique identifiers)
  right_join(wideData, by = 'catNum') %>% #rejoin to wideData tibble
  select(catNum:fightStyle, PC1:PC9) #select name/sex columns and PC scores
  
#now you can plot your pc sores however you want

#obviously PC1 dominates
vertPCscores %>%   
  ggplot(aes(x = PC1, y = PC2)) +
  geom_point() +
  theme_classic()

#you can add colors or labels
# example here the points are colored by fighting style
vertPCscores %>%   
  ggplot(aes(x = PC1, y = PC2)) +
  geom_point(aes(colour = fightStyle)) +
  theme_classic() +
  geom_text_repel(aes(label = spp), size = 2)#the labels are messy but at this stage they can help you ID outliers, examine trends, etc


vertPCscores %>%   
  ggplot(aes(x = PC2, y = PC3)) +
  geom_point(aes(colour = fightStyle)) +
  theme_classic()
