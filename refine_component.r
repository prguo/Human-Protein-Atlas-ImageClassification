#load data
```{r}
train<-read.csv(file="~/Desktop/comp755/project/den161v5_565_train_all_vec.csv",header=T,sep=",")
train_label<-read.csv(file="~/Desktop/comp755/project/class.csv",header=T,sep=",")
```

#label
```{r}
n<-nrow(train_label)
m<-matrix(rep(NA,28*n),n,28)
for (i in 1:n)
{
  for (j in 2:6)
  {
      if (is.na(train_label[i,j])==F)
      {
        x=train_label[i,j]
       m[i,x+1]=1 
      }
  }
}
label<-cbind(train_label,m)
label<-label[,-c(2:6)]
label[is.na(label)] <- 0
colnames(label) <- c("Id","L0","L1","L2","L3","L4","L5","L6","L7","L8","L9","L10",
                   "L11","L12","L13","L14","L15","L16","L17","L18","L19","L20",
                   "L21","L22","L23","L24","L25","L26","L27")
```



#ss small (8,9,10,15,27)
```{r}
#small sample class dataset
train_ss<-train[((data[,10]==1)|(data[,12]==1)|(data[,11]==1)|(data[,17]==1)|(data[,29]==1)),]
label_ss<-label[train_ss$Id,c(10,11,12,17,22,29)]
```

#full dataset
```{r}
feature<-train[, colSums(train != 0) > 0]
protein.pca <- princomp(feature[,-1])
#options(max.print=1000000)

#comp=50, cumul var=0.9815
#comp=15, cumul var=0.9023
#summary(protein.pca)

#scree plot
#plot(protein.pca$sdev^2, type="b", xlab="PC Number", ylab="Variance",main="Scree Plot")

#save 15 comp as dataset
train.comp<-data.frame(protein.pca$scores[,1:15])
write.csv(train.comp,"~/Desktop/comp755/project/pca_ff.csv")

write.csv(label,"~/Desktop/comp755/project/label_ff.csv")

```




