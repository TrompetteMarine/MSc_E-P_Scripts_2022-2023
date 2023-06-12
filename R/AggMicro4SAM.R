#  Use HC and HCagg datasets From Consumption.R
#--------------------------------

x1= vector("numeric",13)
for(i in 1:13){x1[i]=colSums(HCagg[i,])}

