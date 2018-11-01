
cryptor
=======

Overview
--------

**cryptor** is an R package that wraps around the API's of Kraken, Binance and Nomics. The package can be used to access cryptocurrency-related data. To some extent, it can be used to manage a portfolio of cryptocurrencies on a centralized exchange.

Installation
------------

``` r
# The easiest way to obtain the package is to install the development version from GitHub:
# install.packages("devtools")
devtools::install_github("minornode/cryptor")
```

Dependencies
------------

The library comes with quite some dependencies. Installing the cryptor package should also install the following packages:

-   dplyr (&gt;= 0.7.5),
-   tidyr (&gt;= 0.8.1),
-   plyr (&gt;= 1.8.4),
-   magrittr (&gt;= 1.5),
-   httr (&gt;= 1.3.1),
-   jsonlite (&gt;= 1.5),
-   digest (&gt;= 0.6.15),
-   base64enc (&gt;= 0.1-3),
-   assertthat (&gt;= 0.2.0.9000),
-   lubridate (&gt;= 1.7.4),
-   data.table (&gt;= 1.11.4)

Usage
-----

To use the functionality of the API's you can either call a public query (e.g. \[kraken\_servertime()\]) or alternatively to call a private query you must first set the credentials using the appropriate function (e.g. \[credentials\_kraken()\]).

``` r
# All nomics endpoints require a signature
credentials_nomics(key = "2018-09-demo-dont-deploy-b69315e440beb145") # demo API key can be outdated
nomics_markets()

# Public Kraken API calls can be called without credentials
kraken_assetpairs()

# Private Kraken API calls require credentials to be set first
credentials_kraken(key = 'abcd', secret = '123')
kraken_ledger()
```

``` r
library(tidyverse)
library(cryptor)
kraken_candles(pairs = "XBTEUR", interval = 1440) %>% 
  ggplot(aes(x = time, y = close)) + 
  geom_line() + 
  theme_classic()
```

![](man/figures/README-plot.bitcoin-1.png)

Contribution guideline
----------------------

All contributions are welcome. Feel free to add issues / PR's.

Who do I talk to?
-----------------

-   You can contact me at *<thibaut.vanweehaeghe@gmail.com>*
-   ETH adress: 0x05Aa67ce7D9a9A4Cb115d3CC5c7dbaE250eB1539
