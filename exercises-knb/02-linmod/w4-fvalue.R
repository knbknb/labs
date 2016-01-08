set.seed(1)
Fs <- lapply(1:10000, function(x)  {
        N <- 40
        p <- 4
        group <- factor(rep(1:p,each=N/p))
        X <- model.matrix(~ group)
        
        # We will show here how to calculate the "F-value", and then we will use random number to observe the distribution of the F-value under the null hypothesis.
        
        # The F-value is the mean sum of squares explained by the terms of interest (in our case, the 'group' terms) divided by the mean sum of squares of the residuals of a model including the terms of interest. So it is the explanatory power of the terms divided by the leftover variance.
        
        # Intuitively, if this number is large, it means that the group variable explains a lot of the variance in the data, compared to the amount of variance left in the data after using group information. We will calculate these values exactly here:
                
        #        First generate some random, null data, where the mean is the same for all groups:
                
                Y <- rnorm(N,mean=42,7)
        
        #The base model we wil compare against is simply Y-hat = mean(Y), which we will call mu0, and the initial sum of squares is the Y values minus mu0:
                
                mu0 <- mean(Y)
        initial.ss <- sum((Y - mu0)^2)
        
        #We then need to calculate the fitted values for each group, which is simply the mean of each group, and the residuals from this model, which we will call "after.group.ss" for the sum of squares after using the group information:
                
                s <- split(Y, group)
        after.group.ss <- sum(sapply(s, function(x) sum((x - mean(x))^2)))
        
        #Then the explanatory power of the group variable is the initial sum of squares minus the residual sum of squares:
                
         (group.ss <- initial.ss - after.group.ss)
        
        #We calculate the mean of these values, but we divide by terms which remove the number of fitted parameters. For the group sum of squares, this is number of parameters used to fit the groups (3, because the intercept is in the initial model). For the after group sum of squares, 
        #this is the number of samples minus the number of parameters total (So N - 4, including the intercept).
        
        group.ms <- group.ss / (p - 1)
        after.group.ms <- after.group.ss / (N - p)
        #
        #The F-value is simply the ratio of these mean sum of squares.
        
        f.value <- group.ms / after.group.ms
        f.value
})
Fs <- unlist(Fs)
mean(Fs)
hist(Fs, col="grey", border="white", breaks=50, freq=FALSE)

#Overlay the theoretical F-distribution, with parameters df1=p - 1, df2=N - p.

xs <- seq(from=0,to=6,length=100)
lines(xs, df(xs, df1 = p - 1, df2 = N - p), col="red")

#This is the distribution which is used to calculate the p-values for the ANOVA table produced by anova().