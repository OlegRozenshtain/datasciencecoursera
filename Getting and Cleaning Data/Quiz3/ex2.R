# Using the jpeg package read in the following picture. Use the parameter 
# native=TRUE. What are the 30th and 80th quantiles of the resulting data?
ex2<-function()
{
    library(jpeg)
    pic<-readJPEG(source = "./Getting and Cleaning Data/Quiz3/ex2.jpg", 
                  native = TRUE)
    
    quantile(pic, probs = c(0.3, 0.8), na.rm = TRUE)
}