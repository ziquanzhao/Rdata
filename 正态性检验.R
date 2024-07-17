yield <- c(6830,6311,6011,5061,3571,2729,2451)


#SW检验
#SW检验对于小样本量更好（少于5000个），统计效能更好
#只能用于正态性检验
#一般都用这个
shapiro.test(x = yield)

#Shapiro-Wilk normality test
#
#data:  yield
#W = 0.90071, p-value = 0.3353

#W = 0.90071: W 值接近 1，表示数据接近正态分布。
#p-value = 0.3353: p 值大于 0.05，不能拒绝原假设，数据可能服从正态分布。




#KS检验
#用于大样本情况（大于5000个）,小样本时容易错误,需要有数据的平均值和标准差才行
#不仅可以用于正态性检验，还可以用于均值分布检验和指数分布检验
#一般不用这个
ks.test(x = yield,y = "pnorm",mean = mean(yield),sd = sd(yield))
#Exact one-sample Kolmogorov-Smirnov test
#
#data:  yield
#D = 0.1952, p-value = 0.9091
#alternative hypothesis: two-sided

#D = 0.1952: D 值表示样本分布与理论分布之间的最大差异。
#p-value = 0.9091: p 值大于 0.05，不能拒绝原假设，数据可能服从正态分布。