library(tree)
f<-read.csv("C:\\Users\\matiaae\\Desktop\\DS\\Black Friday Problem\\train.csv")
test<- read.csv("C:\\Users\\matiaae\\Desktop\\DS\\Black Friday Problem\\test.csv")

#drop user_id and product id
fit.tree<- f[,c(-1,-2)]

attach(fit.tree)
purchase.tree = tree(Purchase~.,fit.tree)

tree.pred = predict(purchase.tree, fit.tree,type = "vector")

f$predicted_purchase_tree <- tree.pred

test.pred = predict(purchase.tree,test[,!c(-1,-2)],type="vector")

test$tree_pred = test.pred

out<- test[,c('User_ID','Product_ID','tree_pred')]
names(out)<- c('User_ID','Product_ID','Purchase')
write.csv(out,"C:\\Users\\matiaae\\Desktop\\DS\\Black Friday Problem\\submission1.csv")

attach(f)
#glm approach

f$Product_Category_1 <- as.numeric(f$Product_Category_1)
f$Product_Category_2 <- as.numeric(f$Product_Category_2)
f$Product_Category_3 <- as.numeric(f$Product_Category_3)

f[is.na(f$Product_Category_1),'Product_Category_1']<-'unk'
f[is.na(f$Product_Category_2),'Product_Category_2']<-'unk'
f[is.na(f$Product_Category_3),'Product_Category_3']<-'unk'

f$Product_Category_1 <- as.factor(f$Product_Category_1)
f$Product_Category_2 <- as.factor(f$Product_Category_2)
f$Product_Category_3 <- as.factor(f$Product_Category_3)


linear.model = lm(Purchase~.,data=f[,-which(names(f) %in% c('User_ID','Product_ID','predicted_purchase_tree') )] )
summary(linear.model)

linear.prediction <- predict(linear.model, f[,-which(names(f) %in% c('User_ID','Product_ID','predicted_purchase_tree') )])
f$Linear_prediction = linear.prediction

detach(f)

test$Product_Category_1 <- as.numeric(test$Product_Category_1)
test$Product_Category_2 <- as.numeric(test$Product_Category_2)
test$Product_Category_3 <- as.numeric(test$Product_Category_3)

test[is.na(test$Product_Category_1),'Product_Category_1']<-'unk'
test[is.na(test$Product_Category_2),'Product_Category_2']<-'unk'
test[is.na(test$Product_Category_3),'Product_Category_3']<-'unk'

test$Product_Category_1 <- as.factor(test$Product_Category_1)
test$Product_Category_2 <- as.factor(test$Product_Category_2)
test$Product_Category_3 <- as.factor(test$Product_Category_3)

test[test$Product_Category_2=="18",'Product_Category_2'] = 'unk'
test$Product_Category_2<- factor(test$Product_Category_2)


test.pred.linear = predict(linear.model,test[,!c(-1,-2)])


test_metric <- function(A,B){
  len_A = length(A)
  len_B = length(B)
  if(len_A != len_B) { return( -1) }
  return( sqrt( sum( (A-B)^2 )/len_A ) ) 
}

