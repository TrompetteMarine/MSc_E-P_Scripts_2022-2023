
library(LaplacesDemon)
library(MDBED)



joint.density.plot(MapData2$tPolicy,MapData2$tpPolicy, Title="Joint Trace Plot",
                   contour=TRUE, color=TRUE,Trace=c(1,10))

joint.density.plot(MapData2$hPolicy,MapData2$hpPolicy, Title="Joint Trace Plot",
                   contour=TRUE, color=TRUE,Trace=c(1,10))

joint.density.plot(MapData2$dPolicy,MapData2$dpPolicy, Title="Joint Trace Plot",
                   contour=TRUE, color=TRUE,Trace=c(1,10))


joint.density.plot(MapData2$oPolicy,MapData2$opPolicy, Title="Joint Trace Plot",
                   contour=TRUE, color=TRUE,Trace=c(1,10))


