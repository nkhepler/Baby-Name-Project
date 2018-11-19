## pca plot
## not sure what i'm doing
    ## pca is not correct. need more variables.

## renaming 'total' to repsective name
prop_n1 <- prop_N %>% magrittr::set_colnames(c('state', 'Nathan'))
prop_Nat1 <- prop_Nat %>% magrittr::set_colnames(c('state', 'Nathaniel'))

## creating new data frame with proportion data for both names
props <- full_join(prop_n1, prop_Nat1)

## removing state column bc pca doesn't like that
test <- data.frame(props[,2:3])
## adding back in states but as row names
## need this for doing PCA, otherwise will just compare Nathan v Nathaniel and not by state
row.names(test) <- props$state

## should read more about this
pca <- prcomp(test, scale. = TRUE)

## scores will be the data. pca$x is basically just the pca data
## pca is just a list, need to show as DF so can plot
scores = as.data.frame(pca$x)

## plot points with labels
  ## plotting props just instead
ggplot(data = props, aes(x = log10(Nathan), y = log10(Nathaniel), label=state)) +
  geom_point(size=3, colour='red') + 
  geom_hline(yintercept = 0, colour = "gray65") +
  geom_vline(xintercept = 0, colour = "gray65") +
  geom_label_repel(colour = "black", alpha = 0.8, size = 5, hjust=0, vjust=0) + 
  ggtitle("PCA plot of Nathan vs Nathaniel")
