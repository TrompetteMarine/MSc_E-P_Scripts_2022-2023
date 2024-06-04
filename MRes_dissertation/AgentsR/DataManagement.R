# Distributions

g1FS1v = (df1$fs*df1$g1)/(0.5*length(df1$fs))
g1FS2v = (df2$fs*df2$g1)/(0.5*length(df1$fs))
g1FS3v = (df3$fs*df3$g1)/(0.5*length(df1$fs))
g1FS4v = (df4$fs*df4$g1)/(0.5*length(df1$fs))
g1FS5v = (df5$fs*df5$g1)/(0.5*length(df1$fs))

g1CP1v = (df1$cp*df1$g1)/(0.5*length(df1$fs))
g1CP2v = (df1$cp*df1$g1)/(0.5*length(df1$fs))
g1CP3v = (df1$cp*df1$g1)/(0.5*length(df1$fs))
g1CP4v = (df1$cp*df1$g1)/(0.5*length(df1$fs))
g1CP5v = (df1$cp*df1$g1)/(0.5*length(df1$fs))

#group 2

g2FS1v = (df1$fs*df1$g2)/(0.5*length(df1$fs))
g2FS2v = (df2$fs*df2$g2)/(0.5*length(df1$fs))
g2FS3v = (df3$fs*df3$g2)/(0.5*length(df1$fs))
g2FS4v = (df4$fs*df4$g2)/(0.5*length(df1$fs))
g2FS5v = (df5$fs*df5$g2)/(0.5*length(df1$fs))

g2CP1v = (df1$cp*df1$g2)/(0.5*length(df1$cp))
g2CP2v = (df1$cp*df1$g2)/(0.5*length(df1$cp))
g2CP3v = (df1$cp*df1$g2)/(0.5*length(df1$cp))
g2CP4v = (df1$cp*df1$g2)/(0.5*length(df1$cp))
g2CP5v = (df1$cp*df1$g2)/(0.5*length(df1$cp))


#FS and CP by rounds
FS1 = sum(df1$fs)/length(df1$fs)
FS2 = sum(df2$fs)/length(df2$fs)
FS3 = sum(df3$fs)/length(df3$fs)
FS4 = sum(df4$fs)/length(df4$fs)
FS5 = sum(df5$fs)/length(df5$fs)

CP1 = sum(df1$cp)/length(df1$cp)
CP2 = sum(df2$cp)/length(df2$cp)
CP3 = sum(df3$cp)/length(df3$cp)
CP4 = sum(df4$cp)/length(df4$cp)
CP5 = sum(df5$cp)/length(df5$cp)

## FS and CP by rounds for each group

#group 1
g1FS1 = sum(df1$fs*df1$g1)/(0.5*length(df1$fs))
g1FS2 = sum(df2$fs*df2$g1)/(0.5*length(df1$fs))
g1FS3 = sum(df3$fs*df3$g1)/(0.5*length(df1$fs))
g1FS4 = sum(df4$fs*df4$g1)/(0.5*length(df1$fs))
g1FS5 = sum(df5$fs*df5$g1)/(0.5*length(df1$fs))

g1CP1 = sum(df1$cp*df1$g1)/(0.5*length(df1$fs))
g1CP2 = sum(df1$cp*df1$g1)/(0.5*length(df1$fs))
g1CP3 = sum(df1$cp*df1$g1)/(0.5*length(df1$fs))
g1CP4 = sum(df1$cp*df1$g1)/(0.5*length(df1$fs))
g1CP5 = sum(df1$cp*df1$g1)/(0.5*length(df1$fs))

#group 2

g2FS1 = sum(df1$fs*df1$g2)/(0.5*length(df1$fs))
g2FS2 = sum(df2$fs*df2$g2)/(0.5*length(df1$fs))
g2FS3 = sum(df3$fs*df3$g2)/(0.5*length(df1$fs))
g2FS4 = sum(df4$fs*df4$g2)/(0.5*length(df1$fs))
g2FS5 = sum(df5$fs*df5$g2)/(0.5*length(df1$fs))

g2CP1 = sum(df1$cp*df1$g2)/(0.5*length(df1$cp))
g2CP2 = sum(df1$cp*df1$g2)/(0.5*length(df1$cp))
g2CP3 = sum(df1$cp*df1$g2)/(0.5*length(df1$cp))
g2CP4 = sum(df1$cp*df1$g2)/(0.5*length(df1$cp))
g2CP5 = sum(df1$cp*df1$g2)/(0.5*length(df1$cp))


y1=c(g1FS1,g1FS2,g1FS3,g1FS4,g1FS5)
y2=c(g2FS1,g2FS2,g2FS3,g2FS4,g2FS5)
x= 1:5



## CP| FS t-1 by group per round each round
g1CPFS1_2 = sum((df2$cpfs*df2$g1)[df1$fs*df1$g1==1])/sum(df1$fs*df1$g1)
g1CPFS1_3 = sum((df3$cpfs*df3$g1)[df2$fs*df2$g1==1])/sum(df2$fs*df2$g1)
g1CPFS1_4 = sum((df4$cpfs*df4$g1)[df3$fs*df3$g1==1])/sum(df3$fs*df3$g1)
g1CPFS1_5 = sum((df5$cpfs*df5$g1)[df4$fs*df4$g1==1])/sum(df4$fs*df4$g1)

g2CPFS1_2 = sum((df2$cpfs*df2$g2)[df1$fs*df1$g2==1])/sum(df1$fs*df1$g2)
g2CPFS1_3 = sum((df3$cpfs*df3$g2)[df2$fs*df2$g2==1])/sum(df2$fs*df2$g2)
g2CPFS1_4 = sum((df4$cpfs*df4$g2)[df3$fs*df3$g2==1])/sum(df3$fs*df2$g2)
g2CPFS1_5 = sum((df5$cpfs*df5$g2)[df4$fs*df4$g2==1])/sum(df4$fs*df4$g2)