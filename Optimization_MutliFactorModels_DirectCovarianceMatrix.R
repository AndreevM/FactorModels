
# Устанавливаем и загружаем пакет PortfolioAnalytics
#install.packages("PortfolioAnalytics")
library(PortfolioAnalytics)

# Устанавливаем и загружаем пакет DEoptim
#install.packages("DEoptim")
library(DEoptim)

# устанавливаем и загружаем пакет FactorAnalytics
#install.packages("githubinstall")
#install.packages("pcaPP")
#library(githubinstall)
#githubinstall("R-Finance/FactorAnalytics")
library(factorAnalytics)

# Устанавливаем и загружаем пакет quantmod
#install.packages("quantmod")
library(quantmod)

# очистка среды 
rm(list=ls())

# устанавливаем random.seed для воспроизводимости результатов
set.seed(1234)

# загружаем состав индекса ММВБ
benchmark <- read.csv(file = "micex_ind_structure.csv", header = T, stringsAsFactors = F, sep = ";")

# выводим первые несколько записей
head(benchmark)

# загружаем котировки из csv и преобразуем их в объект xts (eXtended Time Series)
returns <- read.csv(file = "quotes_indx.csv", header = T, stringsAsFactors = F, sep = ",")
returns$Date <- as.Date(returns$Date)
returns <- xts(returns[,-1], order.by=returns[,1])

# Вычисляем дневные log-доходности на основе цен с помощью функции ROC
returns <- ROC(returns, type = "discrete")
returns <- returns[-1,]

# создаем целевой портфель и ограничения

# создаем объект типа портфель и передаем в него множество акций, в которые мы будем инвестировать, и задаем сетку поиска
portfolio.spec <- portfolio.spec(assets=benchmark$ticker, weight_seq = generatesequence(min=1e-5, max=0.15, by=0.0001, rounding = 4))

# ограничение: сумма весов всех активов в портфеле равна 1, использование заемных средств запрещено
portfolio.spec <- add.constraint(portfolio=portfolio.spec, type="weight_sum", min_sum=0.99, max_sum=1.01)

# ограничение: кол-во различных инструментов в портфеле не превышает 15
portfolio.spec <- add.constraint(portfolio=portfolio.spec, type="position_limit", max_pos=15)

# ограничение: доля отдельной акции в портфеле не превышает 15%, короткие продажи запрещены
portfolio.spec <- add.constraint(portfolio=portfolio.spec, type="box", min=0.00, max=0.15)

# добавлям запрет на инвестиции в акции химической отрасли (CHEMICAL)
portfolio.spec <- add.constraint(portfolio=portfolio.spec, type="group",
                                 groups=list(groupRestricted=which(benchmark$industry == "CHEMICAL"),
                                             grouAllowed=which(benchmark$industry != "CHEMICAL")),
                                 group_min=c(0.0, 0.99),
                                 group_max=c(0.0, 1.01))

# добавляем штрафную функцию в виде транзакционных расходов 0.20%
portfolio.spec <- add.constraint(portfolio=portfolio.spec, type="transaction_cost", ptc=0.002)


# создаем целевую функцию минимизации активной вариации портфеля
activeRiskObj = function(weights, sigma =0 , b.weights=0)
{
  # вычисляем активные веса тестового портфеля
  active.weights = weights - b.weights

  # вычисляем активный риск и возвращаем его значение  
  active.var = t(active.weights) %*% sigma %*% active.weights
  print(sqrt(active.var)*sqrt(252)*10000)
  return(active.var)
}

# задаем функцию прямого вычисления ковариационной матрицы
sigma.model <- function(R){

  sigma <- cov(returns['2017-02-11/'])

  # возвращаем результат
  out <- list()
  out$sigma <- sigma
  return(out)
}


# задаем целевую функцию
portfolio.spec <- add.objective(portfolio=portfolio.spec, type="risk", name="activeRiskObj",
                            arguments=list(b.weights = benchmark$weight))

# запускаем алгоритм оптимизации портфеля
.storage <<- new.env()
opt.pasd <- optimize.portfolio(R = returns['2017-02-11/'], portfolio = portfolio.spec,
                               optimize_method="DEoptim",
                               momentFUN = "sigma.model",
                               search_size = 200)

# выводим результаты оптимизации
print(sqrt(opt.pasd$objective_measures$activeRiskObj * 252) * 10000)

optimal.weights <- opt.pasd$weights
names(optimal.weights) <- benchmark$ticker
barplot(optimal.weights[optimal.weights > 0])