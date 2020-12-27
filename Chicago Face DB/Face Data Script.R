#Chicago Face Database
#Last Updated: 06/09/2020

#Upload data
#Getting Started
setwd("/Users/jonzimmerman/Desktop/Data Projects/Chicago Face Database/CFD")
#install.packages("readxl")
library(readxl)

#Load in data
CFD=as.data.frame(read_excel("CFD.xlsx",sheet=1))
dim(CFD)
#597 69
colnames(CFD)

CFD.reduced=CFD[,c(1,2,3,4,14:24,26:69)]
dim(CFD.reduced)
#597    59

CFD.f=CFD[,c(1,2,3,4,14:24,26:69)]
CFD.m=CFD.f[,c(5:59)]

CFD.m1=CFD.m[,c(3,1:2,4:15)]
CFD.m2=CFD.m[,c(3,16:55)]
CFD.m2.2=CFD.m2[,c(1,4,7:12,19,22,30,32:33,36,41)]
dim(CFD.m1)
dim(CFD.m2)
dim(CFD.m2.2)


#CFD.pcr=CFD[,c(2:4,14:)]

CFD.male   = subset(CFD,CFD$Gender=="M")
CFD.female = subset(CFD,CFD$Gender=="F")
par(mfrow=c(1,1))

reg1=lm(CFD$Attractive~CFD$Afraid+CFD$Angry+CFD$Disgusted+CFD$Dominant+CFD$Happy+
        CFD$Happy+CFD$Sad+CFD$Surprised+CFD$Threatening+CFD$Trustworthy+CFD$Unusual+
        CFD$Feminine+CFD$Masculine+CFD$NoseLength+CFD$AvgEyeHeight+CFD$BottomLipChin)

reg2=lm(CFD$Attractive~CFD$Happy,data=CFD)
plot_summs(reg1)


leveneTest(reg1)
vif(reg1)
par(mfrow = c(2, 2))
plot(reg1)

par(mfrow=c(1,1))
library(ggplot2)
ggplotRegression <- function (fit) {
  
  require(ggplot2)
  
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_point() +
    stat_smooth(method = "lm", col = "red") +
    labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                       "Intercept =",signif(fit$coef[[1]],5 ),
                       " Slope =",signif(fit$coef[[2]], 5),
                       " P =",signif(summary(fit)$coef[2,4], 5)))
}


install.packages("ERSA")
library(ERSA)



cols <- termColours(reg1,pal = RColorBrewer::brewer.pal(8, "Set2"))
plottStats(reg1,cols)

plot=ggplot(CFD.FA,aes_string(CFD.FA$Happy,CFD.FA$Attractive))+geom_point()
plot=plot+geom_smooth(method=lm)
plot=plot+stat_poly_eq(formula =CFD.FA$Attractive~CFD.FA$Trustworthy,
                       aes(label = paste(..eq.label.., ..rr.label..,..f.value.label, sep = "~~~")), 
                       parse = TRUE)
plot=plot+ggtitle("Asian Women")
plot=plot+theme(plot.title = element_text(hjust = 0.5,face="bold"))
plot=plot+xlab("")+ylab("")
plot

install.packages("ggpmisc")
library(ggpmisc)


ggplot(mtcars, aes(x = disp, y = mpg)) +
  stat_smooth(method = "lm") +
  geom_point() +
  stat_fit_glance(method = "lm",
                  label.y = "bottom",
                  method.args = list(formula = y ~ x),
                  mapping = aes(label = sprintf('r^2~"="~%.3f~~italic(P)~"="~%.2g',
                                                stat(r.squared), stat(p.value))),
                  parse = TRUE)

#install.packages("jtools")
library(jtools)
#install.packages("ggstance")
library(ggstance)
#install.packages("broom.mixed")
library(broom.mixed)
plot_summs(reg1)



# Maximum Likelihood Factor Analysis
# entering raw data and extracting 3 factors,
# with varimax rotation

fit <- factanal(mtcars, 2, rotation="varimax")
print(fit, digits=2, cutoff=.3, sort=TRUE)

scree.plot(fit$correlation,xlim(0,4))
# plot factor 1 by factor 2

plot(load,type="n") # set up plot
text(load,labels=names(to_be_factored),cex=.7) # add variable names

capture.output(print(fit, digits=4, cutoff=.3, sort=TRUE), file="temp.txt")
cat(readLines("temp.txt")[23:30], sep="\n")


plot_summs(reg1)

PropVar=colSums(fit$loading*fit$loading)/dim(fit$loading)[1]    
CumVar=cumsum(colSums(fit$loading*fit$loading)/dim(fit$loading)[1]) 
c=rbind(PropVar,CumVar)



# Principal Axis Factor Analysis
library(psych)
fit <- fa(cor(to_be_factored), nfactors=12, rotate="varimax",fm="ml")
fit # print results


library(DT)

library(DT)
data <- data.frame(
  C1 = rnorm(100),
  C2 = rgamma(100, 10, 10)
)

dtable <- datatable(mtcars, rownames=TRUE, options = list(lengthChange = FALSE, dom='t'))

colRamp <- colorRamp(c("white","blue"))
for(column in names(mtcars)){
  x <- na.omit(mtcars[[column]])
  brks <- quantile(x, probs = seq(.05, .95, .01))
  RGB <- colRamp(c(0, (brks-min(x))/(max(x)-min(x))))
  clrs <- apply(RGB, 1, function(rgb){
    sprintf("rgb(%s)", toString(round(rgb,0)))
  })
  dtable <- dtable %>% 
    formatStyle(column, backgroundColor = styleInterval(brks, clrs))
}

dtable




#--------------------------------------------------------------#
#https://www.analyticsvidhya.com/blog/2016/03/pca-practical-guide-principal-component-analysis-python/
#Principal Component Analysis (PCA)

to_be_PCAed=CFD[,c(4,14:15,18:22,24,26:29,32,37,50,58,60:61,64,69)]
CFD.pca = prcomp(to_be_PCAed, scale = TRUE)
scree.plot(CFD.pca)

library(FactoMineR)
CFD.pca=PCA(to_be_PCAed,scale.unit=TRUE,ncp=12)
head(CFD.pca$ind)
CFDCFD.pca$var$cor




library(caret)
testrun=preProcess(to_be_PCAed, method = "pca",pcaComp=5)
rots=as.data.frame(round(testrun$rotation,6))
plot(rots$PC4,rots$PC2)

tmp <- structure(list(`2019-01-01` = c("f", "f", "f", "<U+263D>", "<U+263D>", "<U+263D>"), `2019-01-02` = c("<U+270E>", "<U+270E>", "<U+270E>", "<U+270E>", "<U+270E>", "<U+270E>"), `2019-01-03` = c("t", "t", "t", "d", "d", "d"), `2019-01-04` = c("d", "d", "d", "<U+2699>", "<U+2699>", "<U+2699>"), `2019-01-05` = c("&", "&", "&", "&", "&", "&"), `2019-01-06` = c("<U+2699>", "<U+2699>", "<U+2699>", "&", "&", "&"), `2019-01-07` = c("^", "^", "^", "^", "^", "^"), `2019-01-08` = c("&", "&", "&", "<U+270E>", "<U+270E>", "<U+270E>"), `2019-01-09` = c("<U+2699>", "<U+2699>", "<U+2699>", "<U+2699>", "<U+2699>", "<U+2699>"), `2019-01-10` = c("s", "s", "s", "s", "s", "s")), row.names = c(NA, 6L), class = "data.frame")

ht <- as_hux(tmp)
ht <- map_background_color(ht, by_values("<U+270E>" = "red", "<U+2699>" = "green"))



brks <- quantile(mtcars, probs = seq(.05, .95, .01), na.rm = TRUE)
clrs <- round(seq(305, 40, length.out = length(brks) + 1), 0) %>%
{paste0("rgb(305,", ., ",", ., ")")}

datatable(data,rownames=TRUE,options = list(pageLength =20, lengthChange = FALSE, dom='t')) %>%
  formatStyle(colnames(data), backgroundColor = styleInterval(brks, clrs))

datatable(mtcars) %>% 
  formatStyle(colnames(mtcars),
    color = styleInterval(c(3.4, 3.8), c('white', 'blue', 'red')),
    backgroundColor = styleInterval(brks, c('coral', 'lightblue'))
  ) 


iris_df <- iris %>% 
  mutate(
    diff = cut(Sepal.Width - Petal.Length, breaks = c(-Inf, 0, +Inf))
  ) 

datatable(iris_df) %>% 
  formatStyle(names(iri),
              backgroundColor = styleEqual(levels(iris_df$diff), c('green', 'red'))
  ) 


library(DT)

library(DT)
testrun <- round(runif(100), 6) # some data
data <- data.frame(testrun = testrun) # better to name it
brks <- quantile(data$testrun, probs = seq(.05, .95, .01), na.rm = TRUE) # provide numeric vector
clrs_df <- colorRamp(c("white","blue"))(c(0,brks))  %>%
  as_tibble(.name_repair ="minimal") %>%
  setNames(nm=c("r","g","b")) %>%
  mutate_all(~as.character(round(.,digits=0)))  %>% mutate(mycolor=paste0("rgb(",
                                                                          paste(r,g,b,sep = ","),
                                                                          ")"))
clrs <- pull(clrs_df,mycolor)
DT::datatable(data,rownames=TRUE,options = list(lengthChange = FALSE, dom='t')) %>%
  formatStyle(colnames(data), backgroundColor = styleInterval(brks, clrs))
















std_dev <- CFD.pca$sdev
pr_var <- std_dev^2
prop_varex <- pr_var/sum(pr_var)

plot(prop_varex, xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     type = "b")

plot(cumsum(prop_varex), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")


#-------------PCR-------------#
library(caret)
library(e1071)
#CFD=CFD[4:dim(CFD)[2]]
#CFD=CFD[-13]


library(caTools)
set.seed(123)
split = sample.split(CFD$Attractive, SplitRatio = 0.8)
training_set = subset(CFD, split == TRUE)
test_set = subset(CFD, split == FALSE)

# Feature Scaling
training_set[-4] = scale(training_set[-4])
test_set[-4] = scale(test_set[-4])



pca = preProcess(x = training_set[-4], method = 'pca', pcaComp = 2)
training_set = predict(pca, training_set)



library(e1071)
classifier = glm(formula = Attractive ~ .,
                 data = training_set)

# Predicting the Test set results
y_pred = predict(classifier, newdata = test_set[-4])

# Making the Confusion Matrix
cm = table(test_set[, 4], y_pred)
