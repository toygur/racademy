
library(tseries)
library(PerformanceAnalytics)

start <- "2014-01-01"
end <- "2016-12-31"
provider <- "yahoo"
compression <- "d"
quote <- "AdjClose"

msftPrices <- as.xts(get.hist.quote(instrument="msft",
                                    start=start, end=end,
                                    quote=quote,
                                    provider=provider,
                                    compression=compression,
                                    quiet = TRUE))

sp500Prices <- as.xts(get.hist.quote(instrument="^gspc",
                                    start=start, end=end,
                                    quote=quote,
                                    provider=provider,
                                    compression=compression,
                                    quiet = TRUE))


dowPrices <- as.xts(get.hist.quote(instrument="^dji",
                                     start=start, end=end,
                                     quote=quote,
                                     provider=provider,
                                     compression=compression,
                                     quiet = TRUE))


daxPrices <- as.xts(get.hist.quote(instrument="^gdaxi",
                                     start=start, end=end,
                                     quote=quote,
                                     provider=provider,
                                     compression=compression,
                                     quiet = TRUE))

nikkeiPrices <- as.xts(get.hist.quote(instrument="^n225",
                                   start=start, end=end,
                                   quote=quote,
                                   provider=provider,
                                   compression=compression,
                                   quiet = TRUE))

aaplPrices <- as.xts(get.hist.quote(instrument="aapl",
                                   start=start, end=end,
                                   quote=quote,
                                   provider=provider,
                                   compression=compression,
                                   quiet = TRUE))

Prices <- merge(msftPrices,sp500Prices,
                dowPrices,daxPrices,
                nikkeiPrices,aaplPrices)

colnames(Prices) <- c("msft","sp500","dow","dax",
                      "nikkei225","aapl")

Prices <- na.exclude(Prices)
chart.TimeSeries(Prices,
                 legend.loc = "bottomright",
                 main="Prices Chart 01.2013 - 12.2016")


