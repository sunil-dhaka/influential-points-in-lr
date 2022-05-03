infectionrisk <- read.table("../data/hospital_infct_03.txt", header=T)
attach(infectionrisk)
model <- lm(InfctRsk ~ Stay)
summary(model)
plot(x=Stay, y=InfctRsk,
panel.last = lines(sort(Stay), fitted(model)[order(Stay)]))
summary(influence.measures(model))

detach(infectionrisk)