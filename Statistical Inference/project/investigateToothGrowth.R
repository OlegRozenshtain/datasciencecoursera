investigateToothGrowth<-function()
{
    library(datasets)
    data(ToothGrowth)
    
    print("Data description:")
    cat("\n")
    print(str(ToothGrowth))
    # count number of observations for each (supp,dose) combination
    print(table(ToothGrowth$supp, ToothGrowth$dose))
    cat("\n")
    
    print("Data summary:")
    cat("\n")
    # get summary of len for each (supp,dose) combination
    print(aggregate(len ~ supp + dose, data = ToothGrowth, summary))
    cat("\n")
    
    library(lattice)
    png("lengths_of_odontoblast_cells.png", width = 720)
    print(xyplot(len ~ dose | supp, data = ToothGrowth,
                 main = "lengths of odontoblast cells in guinea pigs for different vitamin C dosage,
                 given by supplement method" , 
                 xlab = "dosage [milligrams]", ylab = "length [microns]"))
    dev.off()
    
    library(ggplot2)
    png("means_of_lengths.png")
    print(ggplot(aggregate(len ~ supp + dose, data = ToothGrowth, mean), 
                 aes(x = dose, y = len, colour = supp, group = supp)) + 
          geom_point(size = 4) + geom_line() + 
          theme(plot.title = element_text(face = "bold", vjust = 2)) +
          labs(title = "mean lengths of odontoblast cells in guinea pigs 
               for different vitamin C dosage",
               x = "dosage [milligrams]", y = "length [microns]"))
    dev.off()
    
    png("lengths_histogram.png", width = 960)
    print(histogram(~len | factor(dose)*supp, data = ToothGrowth, 
                    layout = c(3,2), breaks = seq(2,36,2), type = "count", 
                    xlab = "length [microns]", 
                    main = "histogram per supplement regime"))
    dev.off()
    
    # perform t-test to compare OJ mean with VC mean over all dosage leveles.
    # extract first the confidence intervals, and second the p-value.
    
    cat("\n")
    print("95 percent confidence interval:")
    print(t(sapply(c(dose0.5=0.5, dose1.0=1, dose2.0=2), 
                   function(d) with(ToothGrowth, 
                                    t.test(len[(supp == "OJ") & (dose == d)], 
                                           len[(supp == "VC") & (dose == d)], 
                                           var.equal = TRUE))$conf)))
    cat("\n")
    print("p-values:")
    print(sapply(c(dose0.5=0.5, dose1.0=1, dose2.0=2), 
                 function(d) with(ToothGrowth, 
                                  t.test(len[(supp == "OJ") & (dose == d)],
                                         len[(supp == "VC") & (dose == d)], 
                                         var.equal = TRUE))$p.value))
}