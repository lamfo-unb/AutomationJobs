library(keras)
library(reticulate)

#Read the data
dataWords<-readRDS("Data/CBOwords.rds")

real.mat <- as.matrix(dataWords[,c(-1)])

# Set `dimnames` to `NULL`
dimnames(real.mat) <- NULL

#Normalize
real.mat<-apply(real.mat,2,as.numeric)
#real.mat<-apply(real.mat,2,function(x) (x-min(x))/(max(x)-min(x)))

#Begin the model
model <- keras_model_sequential() 

#Architeture
model %>% 
  layer_dense(units = 4000, activation = 'sigmoid', input_shape = 8606) %>% 
  layer_dropout(rate = 0.6) %>% 
  layer_dense(units = 2000, activation = 'sigmoid') %>%
  layer_dropout(rate = 0.5) %>%
  layer_dense(units = 1000, activation = 'sigmoid') %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 300, activation = 'sigmoid') %>%
  layer_dropout(rate = 0.1) %>%
  layer_dense(units = 1, activation = 'sigmoid')

# Compile the model
model %>% compile(optimizer='adam',
                  loss='mse')

#Simulate design
target.sim<-rbeta(nrow(dataWords),1,1)

#Sample
iSel <- sample(1:nrow(dataWords),1000,F)
#Training
design<-real.mat[iSel,]
target<-target.sim[iSel]

#Validation
p.design<-real.mat[-iSel,]
p.target<-target.sim[-iSel]

# Fit the model 
history <- model %>% fit(
  design, 
  target, 
  epochs = 200, 
  batch_size = 10, 
  validation_split = 0.2
)

plot(history)


y <- predict(model, design.full)