
# ������������� � ��������� ����� PortfolioAnalytics
#install.packages("PortfolioAnalytics")
library(PortfolioAnalytics)

# ������������� � ��������� ����� DEoptim
#install.packages("DEoptim")
library(DEoptim)

# ������������� � ��������� ����� FactorAnalytics
#install.packages("githubinstall")
#install.packages("pcaPP")
#library(githubinstall)
#githubinstall("R-Finance/FactorAnalytics")
library(factorAnalytics)

# ������������� � ��������� ����� quantmod
#install.packages("quantmod")
library(quantmod)

# ������� ����� 
rm(list=ls())

# ������������� random.seed ��� ����������������� �����������
set.seed(1234)

# ��������� ������ ������� ����
benchmark <- read.csv(file = "micex_ind_structure.csv", header = T, stringsAsFactors = F, sep = ";")

# ������� ������ ��������� �������
head(benchmark)

# ��������� ��������� �� csv � ����������� �� � ������ xts (eXtended Time Series)
returns <- read.csv(file = "quotes_indx.csv", header = T, stringsAsFactors = F, sep = ",")
returns$Date <- as.Date(returns$Date)
returns <- xts(returns[,-1], order.by=returns[,1])

# ��������� ������� log-���������� �� ������ ��� � ������� ������� ROC
returns <- ROC(returns, type = "discrete")
returns <- returns[-1,]

# ������� ������� �������� � �����������

# ������� ������ ���� �������� � �������� � ���� ��������� �����, � ������� �� ����� �������������, � ������ ����� ������
portfolio.spec <- portfolio.spec(assets=benchmark$ticker, weight_seq = generatesequence(min=1e-5, max=0.15, by=0.0001, rounding = 4))

# �����������: ����� ����� ���� ������� � �������� ����� 1, ������������� ������� ������� ���������
portfolio.spec <- add.constraint(portfolio=portfolio.spec, type="weight_sum", min_sum=0.99, max_sum=1.01)

# �����������: ���-�� ��������� ������������ � �������� �� ��������� 15
portfolio.spec <- add.constraint(portfolio=portfolio.spec, type="position_limit", max_pos=15)

# �����������: ���� ��������� ����� � �������� �� ��������� 15%, �������� ������� ���������
portfolio.spec <- add.constraint(portfolio=portfolio.spec, type="box", min=0.00, max=0.15)

# �������� ������ �� ���������� � ����� ���������� ������� (CHEMICAL)
portfolio.spec <- add.constraint(portfolio=portfolio.spec, type="group",
                                 groups=list(groupRestricted=which(benchmark$industry == "CHEMICAL"),
                                             grouAllowed=which(benchmark$industry != "CHEMICAL")),
                                 group_min=c(0.0, 0.99),
                                 group_max=c(0.0, 1.01))

# ��������� �������� ������� � ���� �������������� �������� 0.20%
portfolio.spec <- add.constraint(portfolio=portfolio.spec, type="transaction_cost", ptc=0.002)



# ������� ������� ������� ����������� �������� �������� ��������
activeRiskObj = function(weights, sigma =0 , b.weights=0)
{
  # ��������� �������� ���� ��������� ��������
  active.weights = weights - b.weights

  # ��������� �������� ���� � ���������� ��� ��������  
  active.var = t(active.weights) %*% sigma %*% active.weights
  print(sqrt(active.var)*sqrt(252)*10000)
  return(active.var)
}


sigma.model <- function(R){
  # ������ �������������� �������������� ������
  s.model <- fitStatisticalFactorModel(data = R, check = FALSE, na.rm = F, refine = T, max.k = 5, k = 3)

  # ��������� �������������� ������� �� ������
  sigma <- factorModelCovariance(t(s.model$loadings),
                                   var(s.model$factors),s.model$resid.variance)
  # ���������� ���������
  out <- list()
  out$sigma <- sigma
  return(out)
}


# ������ ������� �������
portfolio.spec <- add.objective(portfolio=portfolio.spec, type="risk", name="activeRiskObj",
                            arguments=list(b.weights = benchmark$weight))

# ��������� �������� ����������� ��������
.storage <<- new.env()
opt.pasd <- optimize.portfolio(R = returns['2017-02-11/'], portfolio = portfolio.spec,
                               optimize_method="DEoptim",
                               momentFUN = "sigma.model",
                               search_size = 200)
# ������� ���������� �����������
print(sqrt(opt.pasd$objective_measures$activeRiskObj * 252) * 10000)

optimal.weights <- opt.pasd$weights
names(optimal.weights) <- benchmark$ticker
barplot(optimal.weights[optimal.weights > 0])


# �������������� �������������� ������: ������ ������� ��������� (press 2)
s.model <- fitStatisticalFactorModel(data = returns['2017-02-11/'], check = FALSE, na.rm = T, refine = T, max.k = 5, k = 3)
plot(s.model)
