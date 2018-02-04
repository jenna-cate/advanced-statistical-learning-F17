
library(ISLR)
#-----------------------------------------------------------------------------
#-- Organize Data
#-----------------------------------------------------------------------------
Data = Default

class.default = ifelse(Data$default=="Yes",1,0)  # 0/1 labels

balance.1 = Data$balance[class.default==1] # separate data into classes
balance.0 = Data$balance[class.default==0]

#-----------------------------------------------------------------------------
#-- (a) Gaussian Model
#-----------------------------------------------------------------------------
xbar.1 = mean(balance.1) # estimate parameters
xbar.0 = mean(balance.0)
v.1 = var(balance.1)*((n-1)/n)
v.0 = var(balance.0)*((n-1)/n)

f.1 <- function(x){(1/sqrt(2*pi*v.1))*exp(-((x-xbar.1)^2/(2*v.1)))}
plot(f.1, xlim = c(xbar.1-6*sqrt(v.1), xbar.1+6*sqrt(v.1)))

f.0 <- function(x){(1/sqrt(2*pi*v.0))*exp(-(x-xbar.0)^2/(2*v.0))}
plot(f.0, xlim = c(xbar.0-6*sqrt(v.0), xbar.0+6*sqrt(v.0)))

ldr = function(x){(1/sqrt(2*pi*v.1))-((x-xbar.1)^2/(2*v.1))-(1/sqrt(2*pi*v.0))+((x-xbar.0)^2/(2*v.0))}
plot(ldr, xlim = c(0, 30000), main = "Log Density Ratio")

#-----------------------------------------------------------------------------
#-- (b) KDE
#-----------------------------------------------------------------------------

plot(density(balance.1))
plot(density(balance.0))
f.1.kde = density(balance.1, from = 0, to = max(Data$balance))
f.0.kde = density(balance.0, from = 0, to = max(Data$balance))
ldr.kde = log(f.1.kde$y/f.0.kde$y)
plot(ldr.kde, type = 'l')

#-----------------------------------------------------------------------------
#-- (c) Categorical
#-----------------------------------------------------------------------------

student.1 = Data$student[class.default==1] # separate data into classes
student.0 = Data$student[class.default==0]

student.1 = ifelse(student.1=="Yes",1,0)  # 0/1 labels
student.0 = ifelse(student.0=="Yes",1,0)

n.1 = length(student.1) # number of obs (stud/not) in class 1
nstud.1 = sum(student.1) # num students in class 1
nnot.1 = n.1-nstud.1 # num not students in class 1
n.0 = length(student.0)
nstud.0 = sum(student.0)
nnot.0 = n.0-nstud.0

ps.1 = (nstud.1+1)/(n.1+2)
pns.1 = (nnot.1+1)/(n.1+2)
f.1.cat = function(x){ps.1*(x==1)+pns.1*(x==0)}
plot(f.1.cat)

ps.0 = (nstud.0+1)/(n.0+2)
pns.0 = (nnot.0+1)/(n.0+2)
f.0.cat = function(x){ps.0*(x==1)+pns.0*(x==0)}
plot(f.0.cat)

ldr.cat = function(x){log(ps.1/ps.0)*(x==1)+log(pns.1/pns.0)*(x==0)}
plot(ldr.cat)

#-----------------------------------------------------------------------------
#-- (d) Bayesian
#-----------------------------------------------------------------------------

pi.1 = mean(class.default==1)
pi.0 = 1-pi.1

phat = function(x){(f.1(x)*pi.1)/(f.1(x)*pi.1+f.0(x)*pi.0)}
plot(phat(Data$balance)~Data$balance)
