---
title: 'SAX: Symbolic Aggregate Approximation'
author: "Ariel Lev, 27. February 2016"
output: 
  html_document:
    keep_md: true
    css: style.css
    pandoc_args: [
      "+RTS", "-K64m",
      "-RTS"
    ]    
---



### Getting and cleaning the data
Loading the Apple Inc (APPL) data set into R and performing an initial exploration.

```r
apple <- read.table("../apple.csv", stringsAsFactors = F)
apple$Date <- as.Date(apple$Date)
apple <- apple %>% arrange(Date)
kable(head(apple,10), caption = "Table 1.|Apple Inc (AAPL) Prices, Dividends, Splits and Trading Volume 2015")
```



|Date       |   Open|   High|     Low|  Close|   Volume| Ex.Dividend| Split.Ratio| Adj..Open| Adj..High| Adj..Low| Adj..Close| Adj..Volume|
|:----------|------:|------:|-------:|------:|--------:|-----------:|-----------:|---------:|---------:|--------:|----------:|-----------:|
|2015-01-02 | 111.39| 111.44| 107.350| 109.33| 53204626|           0|           1|  108.9424|  108.9913| 104.9912|   106.9277|    53204626|
|2015-01-05 | 108.29| 108.65| 105.410| 106.25| 64285491|           0|           1|  105.9105|  106.2626| 103.0938|   103.9153|    64285491|
|2015-01-06 | 106.54| 107.43| 104.630| 106.26| 65797116|           0|           1|  104.1990|  105.0694| 102.3309|   103.9251|    65797116|
|2015-01-07 | 107.20| 108.20| 106.695| 107.75| 40105934|           0|           1|  104.8445|  105.8225| 104.3506|   105.3824|    40105934|
|2015-01-08 | 109.23| 112.15| 108.700| 111.89| 59364547|           0|           1|  106.8298|  109.6857| 106.3115|   109.4314|    59364547|
|2015-01-09 | 112.67| 113.25| 110.210| 112.01| 53315099|           0|           1|  110.1943|  110.7615| 107.7883|   109.5488|    53315099|
|2015-01-12 | 112.60| 112.63| 108.800| 109.25| 49650790|           0|           1|  110.1258|  110.1551| 106.4093|   106.8494|    49650790|
|2015-01-13 | 111.43| 112.80| 108.910| 110.22| 67091928|           0|           1|  108.9815|  110.3214| 106.5169|   107.7981|    67091928|
|2015-01-14 | 109.04| 110.49| 108.500| 109.80| 48956588|           0|           1|  106.6440|  108.0622| 106.1159|   107.3873|    48956588|
|2015-01-15 | 110.00| 110.06| 106.660| 106.82| 60013996|           0|           1|  107.5829|  107.6416| 104.3163|   104.4728|    60013996|

### Dimension reduction using SAX
Translating the time series into its symbolic representation using SAX, and computing the frequency of each SAX symbol in this representation.

```r
# amount of equal sized frames
W <- 50 

# The size of the alphabet
alpha <- 10

data <- apple$Open


# assumes that the distribution of the numeric series x follows a Gaussian distribution
# therefore normalizing
normalize <- (data - mean(data)) / sd(data)

# convert to SAX representation
data_paa <- PAA(normalize, W)
data_sax <- convert.to.SAX.symbol(data_paa, alpha)

# converting to letters
data_sax <- sapply(data_sax, function(x) LETTERS[x])
data_sax
```

```
##  [1] "A" "B" "A" "C" "E" "G" "I" "J" "I" "H" "I" "H" "H" "I" "H" "J" "I"
## [18] "H" "I" "J" "J" "I" "I" "I" "H" "H" "I" "I" "G" "D" "C" "B" "A" "B"
## [35] "B" "C" "C" "A" "B" "B" "C" "E" "F" "C" "D" "E" "D" "B" "A" "A"
```

```r
# frequency
table(data_sax)
```

```
## data_sax
##  A  B  C  D  E  F  G  H  I  J 
##  6  7  6  3  3  1  2  7 11  4
```

### Alternative dimensionality reduction method

```r
# discrete wavelet transform
wt <- dwt(data,  n.levels=7, boundary="periodic", fast=FALSE)

# discrete Fourier transform
# data_dft <- FFT(data)
```

### Parallelize your implementation of SAX

```r
# Can be implemented via a classic MapR job:
# 1. Map tasks iterate over sequential chunks of data for feature selection. 
# 2. A combiner averages features and yields PAA values (Piecewise Aggregate Approximation).
# 3. Reducers sort PAAs and apply SAX.
```

### A graphical interface

```r
## data wrangling for plot

# utility function to convert numeric vectors into data frames containing original dates
makeDF <- function(x, label, dates, type) {
  df <- as.data.frame(dates)
  names(df) <- c("Date")  
  df[label] <- x
  df$type <- type
  df
}

apple_df <- apple[,1:2]
apple_df$type <- "Stock"

# calculating PAA over an unnormalized data for the graphical representation
data_paa <- PAA(data, W)

data_paa <- rep(data_paa, each=length(data)/W)
data_sax <- rep(data_sax, each=length(data)/W)

# fixing length of p to align it with number of raw observations
data_paa <- append(data_paa, rep(data_paa[length(data_paa)], length(data) - length(data_paa)))
data_sax <- append(data_sax, rep(data_sax[length(data_sax)], length(data) - length(data_sax)))

paa_df <- makeDF(data_paa, "Open", apple$Date, "PAA")
#dwt_df <- makeDF(data_dwt, "Open", apple$Date, "dwt")
#dft_df <- makeDF(data_dft, "Open", apple$Date, "dft")

all <- rbind(apple_df, paa_df)

## plotting
p <- nPlot(Open ~ Date, type = "lineChart", data = all, group = "type")
p$xAxis(
    tickFormat =   "#!
      function(d) {return d3.time.format('%B')(new Date(d*1000*3600*24));}
    !#"
  )

p$chart(tooltipContent = "#! function(key, x, y){
        return '<h3>' + key + '</h3>' + 
        '<p>' + y + ' in ' + x + '</p>'
        } !#")

p$addParams(width = 1000, pointSize = 0, lineWidth = 1)
p$xAxis(axisLabel = "Month")
p$yAxis(axisLabel = "Rate")
#p$chart(stacked = TRUE)
#p$chart(reduceXTicks = FALSE)
#p$chart(color = c('#db1847', '#169998'))
p$print('iframesrc', include_assets = TRUE)
```

<link rel='stylesheet' href=/Library/Frameworks/R.framework/Versions/3.1/Resources/library/rCharts/libraries/nvd3/css/nv.d3.css>
<link rel='stylesheet' href=/Library/Frameworks/R.framework/Versions/3.1/Resources/library/rCharts/libraries/nvd3/css/rNVD3.css>
<script type='text/javascript' src=/Library/Frameworks/R.framework/Versions/3.1/Resources/library/rCharts/libraries/nvd3/js/jquery-1.8.2.min.js></script>
<script type='text/javascript' src=/Library/Frameworks/R.framework/Versions/3.1/Resources/library/rCharts/libraries/nvd3/js/d3.v3.min.js></script>
<script type='text/javascript' src=/Library/Frameworks/R.framework/Versions/3.1/Resources/library/rCharts/libraries/nvd3/js/nv.d3.min-new.js></script>
<script type='text/javascript' src=/Library/Frameworks/R.framework/Versions/3.1/Resources/library/rCharts/libraries/nvd3/js/fisheye.js></script> 
 <style>
  .rChart {
    display: block;
    margin-left: auto; 
    margin-right: auto;
    width: 1000px;
    height: 400px;
  }  
  </style>
<div id = 'iframesrc' class = 'rChart nvd3'></div>
<script type='text/javascript'>
 $(document).ready(function(){
      drawiframesrc()
    });
    function drawiframesrc(){  
      var opts = {
 "dom": "iframesrc",
"width":   1000,
"height":    400,
"x": "Date",
"y": "Open",
"type": "lineChart",
"group": "type",
"pointSize":      0,
"lineWidth":      1,
"id": "iframesrc" 
},
        data = [
 {
 "Date":          16437,
"Open":         111.39,
"type": "Stock" 
},
{
 "Date":          16440,
"Open":         108.29,
"type": "Stock" 
},
{
 "Date":          16441,
"Open":         106.54,
"type": "Stock" 
},
{
 "Date":          16442,
"Open":          107.2,
"type": "Stock" 
},
{
 "Date":          16443,
"Open":         109.23,
"type": "Stock" 
},
{
 "Date":          16444,
"Open":         112.67,
"type": "Stock" 
},
{
 "Date":          16447,
"Open":          112.6,
"type": "Stock" 
},
{
 "Date":          16448,
"Open":         111.43,
"type": "Stock" 
},
{
 "Date":          16449,
"Open":         109.04,
"type": "Stock" 
},
{
 "Date":          16450,
"Open":            110,
"type": "Stock" 
},
{
 "Date":          16451,
"Open":         107.03,
"type": "Stock" 
},
{
 "Date":          16455,
"Open":         107.84,
"type": "Stock" 
},
{
 "Date":          16456,
"Open":         108.95,
"type": "Stock" 
},
{
 "Date":          16457,
"Open":         110.26,
"type": "Stock" 
},
{
 "Date":          16458,
"Open":          112.3,
"type": "Stock" 
},
{
 "Date":          16461,
"Open":         113.74,
"type": "Stock" 
},
{
 "Date":          16462,
"Open":         112.42,
"type": "Stock" 
},
{
 "Date":          16463,
"Open":        117.625,
"type": "Stock" 
},
{
 "Date":          16464,
"Open":         116.32,
"type": "Stock" 
},
{
 "Date":          16465,
"Open":          118.4,
"type": "Stock" 
},
{
 "Date":          16468,
"Open":         118.05,
"type": "Stock" 
},
{
 "Date":          16469,
"Open":          118.5,
"type": "Stock" 
},
{
 "Date":          16470,
"Open":          118.5,
"type": "Stock" 
},
{
 "Date":          16471,
"Open":         120.02,
"type": "Stock" 
},
{
 "Date":          16472,
"Open":         120.02,
"type": "Stock" 
},
{
 "Date":          16475,
"Open":         118.55,
"type": "Stock" 
},
{
 "Date":          16476,
"Open":         120.17,
"type": "Stock" 
},
{
 "Date":          16477,
"Open":         122.77,
"type": "Stock" 
},
{
 "Date":          16478,
"Open":         126.06,
"type": "Stock" 
},
{
 "Date":          16479,
"Open":         127.28,
"type": "Stock" 
},
{
 "Date":          16483,
"Open":         127.49,
"type": "Stock" 
},
{
 "Date":          16484,
"Open":        127.625,
"type": "Stock" 
},
{
 "Date":          16485,
"Open":         128.48,
"type": "Stock" 
},
{
 "Date":          16486,
"Open":         128.62,
"type": "Stock" 
},
{
 "Date":          16489,
"Open":         130.02,
"type": "Stock" 
},
{
 "Date":          16490,
"Open":         132.94,
"type": "Stock" 
},
{
 "Date":          16491,
"Open":         131.56,
"type": "Stock" 
},
{
 "Date":          16492,
"Open":        128.785,
"type": "Stock" 
},
{
 "Date":          16493,
"Open":            130,
"type": "Stock" 
},
{
 "Date":          16496,
"Open":         129.25,
"type": "Stock" 
},
{
 "Date":          16497,
"Open":         128.96,
"type": "Stock" 
},
{
 "Date":          16498,
"Open":          129.1,
"type": "Stock" 
},
{
 "Date":          16499,
"Open":         128.58,
"type": "Stock" 
},
{
 "Date":          16500,
"Open":          128.4,
"type": "Stock" 
},
{
 "Date":          16503,
"Open":         127.96,
"type": "Stock" 
},
{
 "Date":          16504,
"Open":         126.41,
"type": "Stock" 
},
{
 "Date":          16505,
"Open":         124.75,
"type": "Stock" 
},
{
 "Date":          16506,
"Open":         122.31,
"type": "Stock" 
},
{
 "Date":          16507,
"Open":          124.4,
"type": "Stock" 
},
{
 "Date":          16510,
"Open":         123.88,
"type": "Stock" 
},
{
 "Date":          16511,
"Open":          125.9,
"type": "Stock" 
},
{
 "Date":          16512,
"Open":            127,
"type": "Stock" 
},
{
 "Date":          16513,
"Open":         128.75,
"type": "Stock" 
},
{
 "Date":          16514,
"Open":         128.25,
"type": "Stock" 
},
{
 "Date":          16517,
"Open":         127.12,
"type": "Stock" 
},
{
 "Date":          16518,
"Open":         127.23,
"type": "Stock" 
},
{
 "Date":          16519,
"Open":         126.54,
"type": "Stock" 
},
{
 "Date":          16520,
"Open":         122.76,
"type": "Stock" 
},
{
 "Date":          16521,
"Open":         124.57,
"type": "Stock" 
},
{
 "Date":          16524,
"Open":         124.05,
"type": "Stock" 
},
{
 "Date":          16525,
"Open":         126.09,
"type": "Stock" 
},
{
 "Date":          16526,
"Open":         124.82,
"type": "Stock" 
},
{
 "Date":          16527,
"Open":         125.03,
"type": "Stock" 
},
{
 "Date":          16531,
"Open":         124.47,
"type": "Stock" 
},
{
 "Date":          16532,
"Open":         127.64,
"type": "Stock" 
},
{
 "Date":          16533,
"Open":         125.85,
"type": "Stock" 
},
{
 "Date":          16534,
"Open":         125.85,
"type": "Stock" 
},
{
 "Date":          16535,
"Open":         125.95,
"type": "Stock" 
},
{
 "Date":          16538,
"Open":         128.37,
"type": "Stock" 
},
{
 "Date":          16539,
"Open":            127,
"type": "Stock" 
},
{
 "Date":          16540,
"Open":         126.41,
"type": "Stock" 
},
{
 "Date":          16541,
"Open":         126.28,
"type": "Stock" 
},
{
 "Date":          16542,
"Open":         125.55,
"type": "Stock" 
},
{
 "Date":          16545,
"Open":         125.57,
"type": "Stock" 
},
{
 "Date":          16546,
"Open":          128.1,
"type": "Stock" 
},
{
 "Date":          16547,
"Open":         126.99,
"type": "Stock" 
},
{
 "Date":          16548,
"Open":          128.3,
"type": "Stock" 
},
{
 "Date":          16549,
"Open":         130.49,
"type": "Stock" 
},
{
 "Date":          16552,
"Open":         132.31,
"type": "Stock" 
},
{
 "Date":          16553,
"Open":        134.455,
"type": "Stock" 
},
{
 "Date":          16554,
"Open":         130.16,
"type": "Stock" 
},
{
 "Date":          16555,
"Open":          127.5,
"type": "Stock" 
},
{
 "Date":          16556,
"Open":          126.1,
"type": "Stock" 
},
{
 "Date":          16559,
"Open":          129.5,
"type": "Stock" 
},
{
 "Date":          16560,
"Open":         128.15,
"type": "Stock" 
},
{
 "Date":          16561,
"Open":         126.56,
"type": "Stock" 
},
{
 "Date":          16562,
"Open":         124.77,
"type": "Stock" 
},
{
 "Date":          16563,
"Open":         126.68,
"type": "Stock" 
},
{
 "Date":          16566,
"Open":         127.39,
"type": "Stock" 
},
{
 "Date":          16567,
"Open":          125.6,
"type": "Stock" 
},
{
 "Date":          16568,
"Open":         126.15,
"type": "Stock" 
},
{
 "Date":          16569,
"Open":         127.41,
"type": "Stock" 
},
{
 "Date":          16570,
"Open":         129.07,
"type": "Stock" 
},
{
 "Date":          16573,
"Open":         128.38,
"type": "Stock" 
},
{
 "Date":          16574,
"Open":         130.69,
"type": "Stock" 
},
{
 "Date":          16575,
"Open":            130,
"type": "Stock" 
},
{
 "Date":          16576,
"Open":         130.07,
"type": "Stock" 
},
{
 "Date":          16577,
"Open":          131.6,
"type": "Stock" 
},
{
 "Date":          16581,
"Open":          132.6,
"type": "Stock" 
},
{
 "Date":          16582,
"Open":         130.34,
"type": "Stock" 
},
{
 "Date":          16583,
"Open":         131.86,
"type": "Stock" 
},
{
 "Date":          16584,
"Open":         131.23,
"type": "Stock" 
},
{
 "Date":          16587,
"Open":          131.2,
"type": "Stock" 
},
{
 "Date":          16588,
"Open":         129.86,
"type": "Stock" 
},
{
 "Date":          16589,
"Open":         130.66,
"type": "Stock" 
},
{
 "Date":          16590,
"Open":         129.58,
"type": "Stock" 
},
{
 "Date":          16591,
"Open":          129.5,
"type": "Stock" 
},
{
 "Date":          16594,
"Open":          128.9,
"type": "Stock" 
},
{
 "Date":          16595,
"Open":          126.7,
"type": "Stock" 
},
{
 "Date":          16596,
"Open":         127.92,
"type": "Stock" 
},
{
 "Date":          16597,
"Open":         129.18,
"type": "Stock" 
},
{
 "Date":          16598,
"Open":        128.185,
"type": "Stock" 
},
{
 "Date":          16601,
"Open":          126.1,
"type": "Stock" 
},
{
 "Date":          16602,
"Open":         127.03,
"type": "Stock" 
},
{
 "Date":          16603,
"Open":         127.72,
"type": "Stock" 
},
{
 "Date":          16604,
"Open":         127.23,
"type": "Stock" 
},
{
 "Date":          16605,
"Open":         127.71,
"type": "Stock" 
},
{
 "Date":          16608,
"Open":         127.49,
"type": "Stock" 
},
{
 "Date":          16609,
"Open":         127.48,
"type": "Stock" 
},
{
 "Date":          16610,
"Open":         127.21,
"type": "Stock" 
},
{
 "Date":          16611,
"Open":         128.86,
"type": "Stock" 
},
{
 "Date":          16612,
"Open":         127.67,
"type": "Stock" 
},
{
 "Date":          16615,
"Open":         125.46,
"type": "Stock" 
},
{
 "Date":          16616,
"Open":         125.57,
"type": "Stock" 
},
{
 "Date":          16617,
"Open":          126.9,
"type": "Stock" 
},
{
 "Date":          16618,
"Open":         126.43,
"type": "Stock" 
},
{
 "Date":          16622,
"Open":         124.94,
"type": "Stock" 
},
{
 "Date":          16623,
"Open":         125.89,
"type": "Stock" 
},
{
 "Date":          16624,
"Open":         124.48,
"type": "Stock" 
},
{
 "Date":          16625,
"Open":         123.85,
"type": "Stock" 
},
{
 "Date":          16626,
"Open":         121.94,
"type": "Stock" 
},
{
 "Date":          16629,
"Open":         125.03,
"type": "Stock" 
},
{
 "Date":          16630,
"Open":         126.04,
"type": "Stock" 
},
{
 "Date":          16631,
"Open":         125.72,
"type": "Stock" 
},
{
 "Date":          16632,
"Open":         127.74,
"type": "Stock" 
},
{
 "Date":          16633,
"Open":         129.08,
"type": "Stock" 
},
{
 "Date":          16636,
"Open":         130.97,
"type": "Stock" 
},
{
 "Date":          16637,
"Open":         132.85,
"type": "Stock" 
},
{
 "Date":          16638,
"Open":         121.99,
"type": "Stock" 
},
{
 "Date":          16639,
"Open":          126.2,
"type": "Stock" 
},
{
 "Date":          16640,
"Open":         125.32,
"type": "Stock" 
},
{
 "Date":          16643,
"Open":         123.09,
"type": "Stock" 
},
{
 "Date":          16644,
"Open":         123.38,
"type": "Stock" 
},
{
 "Date":          16645,
"Open":         123.15,
"type": "Stock" 
},
{
 "Date":          16646,
"Open":         122.32,
"type": "Stock" 
},
{
 "Date":          16647,
"Open":          122.6,
"type": "Stock" 
},
{
 "Date":          16650,
"Open":          121.5,
"type": "Stock" 
},
{
 "Date":          16651,
"Open":         117.42,
"type": "Stock" 
},
{
 "Date":          16652,
"Open":         112.95,
"type": "Stock" 
},
{
 "Date":          16653,
"Open":         115.97,
"type": "Stock" 
},
{
 "Date":          16654,
"Open":         114.58,
"type": "Stock" 
},
{
 "Date":          16657,
"Open":         116.53,
"type": "Stock" 
},
{
 "Date":          16658,
"Open":         117.81,
"type": "Stock" 
},
{
 "Date":          16659,
"Open":         112.53,
"type": "Stock" 
},
{
 "Date":          16660,
"Open":         116.04,
"type": "Stock" 
},
{
 "Date":          16661,
"Open":         114.32,
"type": "Stock" 
},
{
 "Date":          16664,
"Open":         116.04,
"type": "Stock" 
},
{
 "Date":          16665,
"Open":         116.43,
"type": "Stock" 
},
{
 "Date":          16666,
"Open":          116.1,
"type": "Stock" 
},
{
 "Date":          16667,
"Open":         114.08,
"type": "Stock" 
},
{
 "Date":          16668,
"Open":         110.43,
"type": "Stock" 
},
{
 "Date":          16671,
"Open":          94.87,
"type": "Stock" 
},
{
 "Date":          16672,
"Open":         111.11,
"type": "Stock" 
},
{
 "Date":          16673,
"Open":        107.085,
"type": "Stock" 
},
{
 "Date":          16674,
"Open":         112.25,
"type": "Stock" 
},
{
 "Date":          16675,
"Open":         112.17,
"type": "Stock" 
},
{
 "Date":          16678,
"Open":         112.13,
"type": "Stock" 
},
{
 "Date":          16679,
"Open":         110.18,
"type": "Stock" 
},
{
 "Date":          16680,
"Open":            110,
"type": "Stock" 
},
{
 "Date":          16681,
"Open":         112.49,
"type": "Stock" 
},
{
 "Date":          16682,
"Open":         108.97,
"type": "Stock" 
},
{
 "Date":          16686,
"Open":         111.65,
"type": "Stock" 
},
{
 "Date":          16687,
"Open":         113.76,
"type": "Stock" 
},
{
 "Date":          16688,
"Open":         110.27,
"type": "Stock" 
},
{
 "Date":          16689,
"Open":         111.79,
"type": "Stock" 
},
{
 "Date":          16692,
"Open":         116.58,
"type": "Stock" 
},
{
 "Date":          16693,
"Open":         115.93,
"type": "Stock" 
},
{
 "Date":          16694,
"Open":         116.25,
"type": "Stock" 
},
{
 "Date":          16695,
"Open":         115.66,
"type": "Stock" 
},
{
 "Date":          16696,
"Open":         112.21,
"type": "Stock" 
},
{
 "Date":          16699,
"Open":         113.67,
"type": "Stock" 
},
{
 "Date":          16700,
"Open":         113.38,
"type": "Stock" 
},
{
 "Date":          16701,
"Open":         113.63,
"type": "Stock" 
},
{
 "Date":          16702,
"Open":         113.25,
"type": "Stock" 
},
{
 "Date":          16703,
"Open":         116.44,
"type": "Stock" 
},
{
 "Date":          16706,
"Open":         113.85,
"type": "Stock" 
},
{
 "Date":          16707,
"Open":         112.83,
"type": "Stock" 
},
{
 "Date":          16708,
"Open":         110.17,
"type": "Stock" 
},
{
 "Date":          16709,
"Open":         109.07,
"type": "Stock" 
},
{
 "Date":          16710,
"Open":         108.01,
"type": "Stock" 
},
{
 "Date":          16713,
"Open":         109.88,
"type": "Stock" 
},
{
 "Date":          16714,
"Open":         110.63,
"type": "Stock" 
},
{
 "Date":          16715,
"Open":         111.74,
"type": "Stock" 
},
{
 "Date":          16716,
"Open":         110.19,
"type": "Stock" 
},
{
 "Date":          16717,
"Open":            110,
"type": "Stock" 
},
{
 "Date":          16720,
"Open":         112.73,
"type": "Stock" 
},
{
 "Date":          16721,
"Open":         110.82,
"type": "Stock" 
},
{
 "Date":          16722,
"Open":         111.29,
"type": "Stock" 
},
{
 "Date":          16723,
"Open":         110.93,
"type": "Stock" 
},
{
 "Date":          16724,
"Open":         111.78,
"type": "Stock" 
},
{
 "Date":          16727,
"Open":          110.8,
"type": "Stock" 
},
{
 "Date":          16728,
"Open":         111.34,
"type": "Stock" 
},
{
 "Date":          16729,
"Open":            114,
"type": "Stock" 
},
{
 "Date":          16730,
"Open":         114.33,
"type": "Stock" 
},
{
 "Date":          16731,
"Open":          116.7,
"type": "Stock" 
},
{
 "Date":          16734,
"Open":         118.08,
"type": "Stock" 
},
{
 "Date":          16735,
"Open":          115.4,
"type": "Stock" 
},
{
 "Date":          16736,
"Open":         116.93,
"type": "Stock" 
},
{
 "Date":          16737,
"Open":          118.7,
"type": "Stock" 
},
{
 "Date":          16738,
"Open":         120.99,
"type": "Stock" 
},
{
 "Date":          16741,
"Open":         119.87,
"type": "Stock" 
},
{
 "Date":          16742,
"Open":         120.79,
"type": "Stock" 
},
{
 "Date":          16743,
"Open":         123.13,
"type": "Stock" 
},
{
 "Date":          16744,
"Open":         121.85,
"type": "Stock" 
},
{
 "Date":          16745,
"Open":         121.11,
"type": "Stock" 
},
{
 "Date":          16748,
"Open":         120.96,
"type": "Stock" 
},
{
 "Date":          16749,
"Open":          116.9,
"type": "Stock" 
},
{
 "Date":          16750,
"Open":         116.37,
"type": "Stock" 
},
{
 "Date":          16751,
"Open":         116.26,
"type": "Stock" 
},
{
 "Date":          16752,
"Open":          115.2,
"type": "Stock" 
},
{
 "Date":          16755,
"Open":         111.38,
"type": "Stock" 
},
{
 "Date":          16756,
"Open":         114.92,
"type": "Stock" 
},
{
 "Date":          16757,
"Open":         115.76,
"type": "Stock" 
},
{
 "Date":          16758,
"Open":         117.64,
"type": "Stock" 
},
{
 "Date":          16759,
"Open":          119.2,
"type": "Stock" 
},
{
 "Date":          16762,
"Open":         119.27,
"type": "Stock" 
},
{
 "Date":          16763,
"Open":         117.33,
"type": "Stock" 
},
{
 "Date":          16764,
"Open":         119.21,
"type": "Stock" 
},
{
 "Date":          16766,
"Open":         118.29,
"type": "Stock" 
},
{
 "Date":          16769,
"Open":         117.99,
"type": "Stock" 
},
{
 "Date":          16770,
"Open":         118.75,
"type": "Stock" 
},
{
 "Date":          16771,
"Open":         117.05,
"type": "Stock" 
},
{
 "Date":          16772,
"Open":         116.55,
"type": "Stock" 
},
{
 "Date":          16773,
"Open":         115.29,
"type": "Stock" 
},
{
 "Date":          16776,
"Open":         118.98,
"type": "Stock" 
},
{
 "Date":          16777,
"Open":         117.52,
"type": "Stock" 
},
{
 "Date":          16778,
"Open":         117.64,
"type": "Stock" 
},
{
 "Date":          16779,
"Open":         116.04,
"type": "Stock" 
},
{
 "Date":          16780,
"Open":         115.19,
"type": "Stock" 
},
{
 "Date":          16783,
"Open":         112.18,
"type": "Stock" 
},
{
 "Date":          16784,
"Open":         111.94,
"type": "Stock" 
},
{
 "Date":          16785,
"Open":         111.07,
"type": "Stock" 
},
{
 "Date":          16786,
"Open":         112.02,
"type": "Stock" 
},
{
 "Date":          16787,
"Open":         108.91,
"type": "Stock" 
},
{
 "Date":          16790,
"Open":         107.28,
"type": "Stock" 
},
{
 "Date":          16791,
"Open":          107.4,
"type": "Stock" 
},
{
 "Date":          16792,
"Open":         107.27,
"type": "Stock" 
},
{
 "Date":          16793,
"Open":            109,
"type": "Stock" 
},
{
 "Date":          16797,
"Open":         107.59,
"type": "Stock" 
},
{
 "Date":          16798,
"Open":         106.96,
"type": "Stock" 
},
{
 "Date":          16799,
"Open":         108.58,
"type": "Stock" 
},
{
 "Date":          16800,
"Open":         107.01,
"type": "Stock" 
},
{
 "Date":          16437,
"Open": 108.5628571429,
"type": "PAA" 
},
{
 "Date":          16440,
"Open": 108.5628571429,
"type": "PAA" 
},
{
 "Date":          16441,
"Open": 108.5628571429,
"type": "PAA" 
},
{
 "Date":          16442,
"Open": 108.5628571429,
"type": "PAA" 
},
{
 "Date":          16443,
"Open": 108.5628571429,
"type": "PAA" 
},
{
 "Date":          16444,
"Open": 111.0705555556,
"type": "PAA" 
},
{
 "Date":          16447,
"Open": 111.0705555556,
"type": "PAA" 
},
{
 "Date":          16448,
"Open": 111.0705555556,
"type": "PAA" 
},
{
 "Date":          16449,
"Open": 111.0705555556,
"type": "PAA" 
},
{
 "Date":          16450,
"Open": 111.0705555556,
"type": "PAA" 
},
{
 "Date":          16451,
"Open": 109.4179365079,
"type": "PAA" 
},
{
 "Date":          16455,
"Open": 109.4179365079,
"type": "PAA" 
},
{
 "Date":          16456,
"Open": 109.4179365079,
"type": "PAA" 
},
{
 "Date":          16457,
"Open": 109.4179365079,
"type": "PAA" 
},
{
 "Date":          16458,
"Open": 109.4179365079,
"type": "PAA" 
},
{
 "Date":          16461,
"Open": 115.8222619048,
"type": "PAA" 
},
{
 "Date":          16462,
"Open": 115.8222619048,
"type": "PAA" 
},
{
 "Date":          16463,
"Open": 115.8222619048,
"type": "PAA" 
},
{
 "Date":          16464,
"Open": 115.8222619048,
"type": "PAA" 
},
{
 "Date":          16465,
"Open": 115.8222619048,
"type": "PAA" 
},
{
 "Date":          16468,
"Open": 119.0301587302,
"type": "PAA" 
},
{
 "Date":          16469,
"Open": 119.0301587302,
"type": "PAA" 
},
{
 "Date":          16470,
"Open": 119.0301587302,
"type": "PAA" 
},
{
 "Date":          16471,
"Open": 119.0301587302,
"type": "PAA" 
},
{
 "Date":          16472,
"Open": 119.0301587302,
"type": "PAA" 
},
{
 "Date":          16475,
"Open": 123.3566666667,
"type": "PAA" 
},
{
 "Date":          16476,
"Open": 123.3566666667,
"type": "PAA" 
},
{
 "Date":          16477,
"Open": 123.3566666667,
"type": "PAA" 
},
{
 "Date":          16478,
"Open": 123.3566666667,
"type": "PAA" 
},
{
 "Date":          16479,
"Open": 123.3566666667,
"type": "PAA" 
},
{
 "Date":          16483,
"Open": 128.7421825397,
"type": "PAA" 
},
{
 "Date":          16484,
"Open": 128.7421825397,
"type": "PAA" 
},
{
 "Date":          16485,
"Open": 128.7421825397,
"type": "PAA" 
},
{
 "Date":          16486,
"Open": 128.7421825397,
"type": "PAA" 
},
{
 "Date":          16489,
"Open": 128.7421825397,
"type": "PAA" 
},
{
 "Date":          16490,
"Open": 130.2736111111,
"type": "PAA" 
},
{
 "Date":          16491,
"Open": 130.2736111111,
"type": "PAA" 
},
{
 "Date":          16492,
"Open": 130.2736111111,
"type": "PAA" 
},
{
 "Date":          16493,
"Open": 130.2736111111,
"type": "PAA" 
},
{
 "Date":          16496,
"Open": 130.2736111111,
"type": "PAA" 
},
{
 "Date":          16497,
"Open": 128.4207142857,
"type": "PAA" 
},
{
 "Date":          16498,
"Open": 128.4207142857,
"type": "PAA" 
},
{
 "Date":          16499,
"Open": 128.4207142857,
"type": "PAA" 
},
{
 "Date":          16500,
"Open": 128.4207142857,
"type": "PAA" 
},
{
 "Date":          16503,
"Open": 128.4207142857,
"type": "PAA" 
},
{
 "Date":          16504,
"Open": 124.3258730159,
"type": "PAA" 
},
{
 "Date":          16505,
"Open": 124.3258730159,
"type": "PAA" 
},
{
 "Date":          16506,
"Open": 124.3258730159,
"type": "PAA" 
},
{
 "Date":          16507,
"Open": 124.3258730159,
"type": "PAA" 
},
{
 "Date":          16510,
"Open": 124.3258730159,
"type": "PAA" 
},
{
 "Date":          16511,
"Open": 127.5081746032,
"type": "PAA" 
},
{
 "Date":          16512,
"Open": 127.5081746032,
"type": "PAA" 
},
{
 "Date":          16513,
"Open": 127.5081746032,
"type": "PAA" 
},
{
 "Date":          16514,
"Open": 127.5081746032,
"type": "PAA" 
},
{
 "Date":          16517,
"Open": 127.5081746032,
"type": "PAA" 
},
{
 "Date":          16518,
"Open": 124.9388888889,
"type": "PAA" 
},
{
 "Date":          16519,
"Open": 124.9388888889,
"type": "PAA" 
},
{
 "Date":          16520,
"Open": 124.9388888889,
"type": "PAA" 
},
{
 "Date":          16521,
"Open": 124.9388888889,
"type": "PAA" 
},
{
 "Date":          16524,
"Open": 124.9388888889,
"type": "PAA" 
},
{
 "Date":          16525,
"Open":  125.589047619,
"type": "PAA" 
},
{
 "Date":          16526,
"Open":  125.589047619,
"type": "PAA" 
},
{
 "Date":          16527,
"Open":  125.589047619,
"type": "PAA" 
},
{
 "Date":          16531,
"Open":  125.589047619,
"type": "PAA" 
},
{
 "Date":          16532,
"Open":  125.589047619,
"type": "PAA" 
},
{
 "Date":          16533,
"Open": 126.6602380952,
"type": "PAA" 
},
{
 "Date":          16534,
"Open": 126.6602380952,
"type": "PAA" 
},
{
 "Date":          16535,
"Open": 126.6602380952,
"type": "PAA" 
},
{
 "Date":          16538,
"Open": 126.6602380952,
"type": "PAA" 
},
{
 "Date":          16539,
"Open": 126.6602380952,
"type": "PAA" 
},
{
 "Date":          16540,
"Open": 126.4512698413,
"type": "PAA" 
},
{
 "Date":          16541,
"Open": 126.4512698413,
"type": "PAA" 
},
{
 "Date":          16542,
"Open": 126.4512698413,
"type": "PAA" 
},
{
 "Date":          16545,
"Open": 126.4512698413,
"type": "PAA" 
},
{
 "Date":          16546,
"Open": 126.4512698413,
"type": "PAA" 
},
{
 "Date":          16547,
"Open": 130.8836111111,
"type": "PAA" 
},
{
 "Date":          16548,
"Open": 130.8836111111,
"type": "PAA" 
},
{
 "Date":          16549,
"Open": 130.8836111111,
"type": "PAA" 
},
{
 "Date":          16552,
"Open": 130.8836111111,
"type": "PAA" 
},
{
 "Date":          16553,
"Open": 130.8836111111,
"type": "PAA" 
},
{
 "Date":          16554,
"Open": 127.8111904762,
"type": "PAA" 
},
{
 "Date":          16555,
"Open": 127.8111904762,
"type": "PAA" 
},
{
 "Date":          16556,
"Open": 127.8111904762,
"type": "PAA" 
},
{
 "Date":          16559,
"Open": 127.8111904762,
"type": "PAA" 
},
{
 "Date":          16560,
"Open": 127.8111904762,
"type": "PAA" 
},
{
 "Date":          16561,
"Open": 126.1442857143,
"type": "PAA" 
},
{
 "Date":          16562,
"Open": 126.1442857143,
"type": "PAA" 
},
{
 "Date":          16563,
"Open": 126.1442857143,
"type": "PAA" 
},
{
 "Date":          16566,
"Open": 126.1442857143,
"type": "PAA" 
},
{
 "Date":          16567,
"Open": 126.1442857143,
"type": "PAA" 
},
{
 "Date":          16568,
"Open": 128.9031746032,
"type": "PAA" 
},
{
 "Date":          16569,
"Open": 128.9031746032,
"type": "PAA" 
},
{
 "Date":          16570,
"Open": 128.9031746032,
"type": "PAA" 
},
{
 "Date":          16573,
"Open": 128.9031746032,
"type": "PAA" 
},
{
 "Date":          16574,
"Open": 128.9031746032,
"type": "PAA" 
},
{
 "Date":          16575,
"Open": 131.2099206349,
"type": "PAA" 
},
{
 "Date":          16576,
"Open": 131.2099206349,
"type": "PAA" 
},
{
 "Date":          16577,
"Open": 131.2099206349,
"type": "PAA" 
},
{
 "Date":          16581,
"Open": 131.2099206349,
"type": "PAA" 
},
{
 "Date":          16582,
"Open": 131.2099206349,
"type": "PAA" 
},
{
 "Date":          16583,
"Open": 130.5891269841,
"type": "PAA" 
},
{
 "Date":          16584,
"Open": 130.5891269841,
"type": "PAA" 
},
{
 "Date":          16587,
"Open": 130.5891269841,
"type": "PAA" 
},
{
 "Date":          16588,
"Open": 130.5891269841,
"type": "PAA" 
},
{
 "Date":          16589,
"Open": 130.5891269841,
"type": "PAA" 
},
{
 "Date":          16590,
"Open": 128.4585714286,
"type": "PAA" 
},
{
 "Date":          16591,
"Open": 128.4585714286,
"type": "PAA" 
},
{
 "Date":          16594,
"Open": 128.4585714286,
"type": "PAA" 
},
{
 "Date":          16595,
"Open": 128.4585714286,
"type": "PAA" 
},
{
 "Date":          16596,
"Open": 128.4585714286,
"type": "PAA" 
},
{
 "Date":          16597,
"Open": 127.2992460317,
"type": "PAA" 
},
{
 "Date":          16598,
"Open": 127.2992460317,
"type": "PAA" 
},
{
 "Date":          16601,
"Open": 127.2992460317,
"type": "PAA" 
},
{
 "Date":          16602,
"Open": 127.2992460317,
"type": "PAA" 
},
{
 "Date":          16603,
"Open": 127.2992460317,
"type": "PAA" 
},
{
 "Date":          16604,
"Open": 127.7329365079,
"type": "PAA" 
},
{
 "Date":          16605,
"Open": 127.7329365079,
"type": "PAA" 
},
{
 "Date":          16608,
"Open": 127.7329365079,
"type": "PAA" 
},
{
 "Date":          16609,
"Open": 127.7329365079,
"type": "PAA" 
},
{
 "Date":          16610,
"Open": 127.7329365079,
"type": "PAA" 
},
{
 "Date":          16611,
"Open": 126.4254761905,
"type": "PAA" 
},
{
 "Date":          16612,
"Open": 126.4254761905,
"type": "PAA" 
},
{
 "Date":          16615,
"Open": 126.4254761905,
"type": "PAA" 
},
{
 "Date":          16616,
"Open": 126.4254761905,
"type": "PAA" 
},
{
 "Date":          16617,
"Open": 126.4254761905,
"type": "PAA" 
},
{
 "Date":          16618,
"Open": 124.2264285714,
"type": "PAA" 
},
{
 "Date":          16622,
"Open": 124.2264285714,
"type": "PAA" 
},
{
 "Date":          16623,
"Open": 124.2264285714,
"type": "PAA" 
},
{
 "Date":          16624,
"Open": 124.2264285714,
"type": "PAA" 
},
{
 "Date":          16625,
"Open": 124.2264285714,
"type": "PAA" 
},
{
 "Date":          16626,
"Open": 126.8028571429,
"type": "PAA" 
},
{
 "Date":          16629,
"Open": 126.8028571429,
"type": "PAA" 
},
{
 "Date":          16630,
"Open": 126.8028571429,
"type": "PAA" 
},
{
 "Date":          16631,
"Open": 126.8028571429,
"type": "PAA" 
},
{
 "Date":          16632,
"Open": 126.8028571429,
"type": "PAA" 
},
{
 "Date":          16633,
"Open": 127.3061904762,
"type": "PAA" 
},
{
 "Date":          16636,
"Open": 127.3061904762,
"type": "PAA" 
},
{
 "Date":          16637,
"Open": 127.3061904762,
"type": "PAA" 
},
{
 "Date":          16638,
"Open": 127.3061904762,
"type": "PAA" 
},
{
 "Date":          16639,
"Open": 127.3061904762,
"type": "PAA" 
},
{
 "Date":          16640,
"Open":  122.858968254,
"type": "PAA" 
},
{
 "Date":          16643,
"Open":  122.858968254,
"type": "PAA" 
},
{
 "Date":          16644,
"Open":  122.858968254,
"type": "PAA" 
},
{
 "Date":          16645,
"Open":  122.858968254,
"type": "PAA" 
},
{
 "Date":          16646,
"Open":  122.858968254,
"type": "PAA" 
},
{
 "Date":          16647,
"Open": 116.3265873016,
"type": "PAA" 
},
{
 "Date":          16650,
"Open": 116.3265873016,
"type": "PAA" 
},
{
 "Date":          16651,
"Open": 116.3265873016,
"type": "PAA" 
},
{
 "Date":          16652,
"Open": 116.3265873016,
"type": "PAA" 
},
{
 "Date":          16653,
"Open": 116.3265873016,
"type": "PAA" 
},
{
 "Date":          16654,
"Open": 115.4312698413,
"type": "PAA" 
},
{
 "Date":          16657,
"Open": 115.4312698413,
"type": "PAA" 
},
{
 "Date":          16658,
"Open": 115.4312698413,
"type": "PAA" 
},
{
 "Date":          16659,
"Open": 115.4312698413,
"type": "PAA" 
},
{
 "Date":          16660,
"Open": 115.4312698413,
"type": "PAA" 
},
{
 "Date":          16661,
"Open": 113.4511904762,
"type": "PAA" 
},
{
 "Date":          16664,
"Open": 113.4511904762,
"type": "PAA" 
},
{
 "Date":          16665,
"Open": 113.4511904762,
"type": "PAA" 
},
{
 "Date":          16666,
"Open": 113.4511904762,
"type": "PAA" 
},
{
 "Date":          16667,
"Open": 113.4511904762,
"type": "PAA" 
},
{
 "Date":          16668,
"Open": 108.4926587302,
"type": "PAA" 
},
{
 "Date":          16671,
"Open": 108.4926587302,
"type": "PAA" 
},
{
 "Date":          16672,
"Open": 108.4926587302,
"type": "PAA" 
},
{
 "Date":          16673,
"Open": 108.4926587302,
"type": "PAA" 
},
{
 "Date":          16674,
"Open": 108.4926587302,
"type": "PAA" 
},
{
 "Date":          16675,
"Open": 110.7306349206,
"type": "PAA" 
},
{
 "Date":          16678,
"Open": 110.7306349206,
"type": "PAA" 
},
{
 "Date":          16679,
"Open": 110.7306349206,
"type": "PAA" 
},
{
 "Date":          16680,
"Open": 110.7306349206,
"type": "PAA" 
},
{
 "Date":          16681,
"Open": 110.7306349206,
"type": "PAA" 
},
{
 "Date":          16682,
"Open": 113.1404761905,
"type": "PAA" 
},
{
 "Date":          16686,
"Open": 113.1404761905,
"type": "PAA" 
},
{
 "Date":          16687,
"Open": 113.1404761905,
"type": "PAA" 
},
{
 "Date":          16688,
"Open": 113.1404761905,
"type": "PAA" 
},
{
 "Date":          16689,
"Open": 113.1404761905,
"type": "PAA" 
},
{
 "Date":          16692,
"Open": 114.5307936508,
"type": "PAA" 
},
{
 "Date":          16693,
"Open": 114.5307936508,
"type": "PAA" 
},
{
 "Date":          16694,
"Open": 114.5307936508,
"type": "PAA" 
},
{
 "Date":          16695,
"Open": 114.5307936508,
"type": "PAA" 
},
{
 "Date":          16696,
"Open": 114.5307936508,
"type": "PAA" 
},
{
 "Date":          16699,
"Open": 114.0518253968,
"type": "PAA" 
},
{
 "Date":          16700,
"Open": 114.0518253968,
"type": "PAA" 
},
{
 "Date":          16701,
"Open": 114.0518253968,
"type": "PAA" 
},
{
 "Date":          16702,
"Open": 114.0518253968,
"type": "PAA" 
},
{
 "Date":          16703,
"Open": 114.0518253968,
"type": "PAA" 
},
{
 "Date":          16706,
"Open": 109.7875396825,
"type": "PAA" 
},
{
 "Date":          16707,
"Open": 109.7875396825,
"type": "PAA" 
},
{
 "Date":          16708,
"Open": 109.7875396825,
"type": "PAA" 
},
{
 "Date":          16709,
"Open": 109.7875396825,
"type": "PAA" 
},
{
 "Date":          16710,
"Open": 109.7875396825,
"type": "PAA" 
},
{
 "Date":          16713,
"Open": 111.0757142857,
"type": "PAA" 
},
{
 "Date":          16714,
"Open": 111.0757142857,
"type": "PAA" 
},
{
 "Date":          16715,
"Open": 111.0757142857,
"type": "PAA" 
},
{
 "Date":          16716,
"Open": 111.0757142857,
"type": "PAA" 
},
{
 "Date":          16717,
"Open": 111.0757142857,
"type": "PAA" 
},
{
 "Date":          16720,
"Open": 111.1834920635,
"type": "PAA" 
},
{
 "Date":          16721,
"Open": 111.1834920635,
"type": "PAA" 
},
{
 "Date":          16722,
"Open": 111.1834920635,
"type": "PAA" 
},
{
 "Date":          16723,
"Open": 111.1834920635,
"type": "PAA" 
},
{
 "Date":          16724,
"Open": 111.1834920635,
"type": "PAA" 
},
{
 "Date":          16727,
"Open": 115.3773809524,
"type": "PAA" 
},
{
 "Date":          16728,
"Open": 115.3773809524,
"type": "PAA" 
},
{
 "Date":          16729,
"Open": 115.3773809524,
"type": "PAA" 
},
{
 "Date":          16730,
"Open": 115.3773809524,
"type": "PAA" 
},
{
 "Date":          16731,
"Open": 115.3773809524,
"type": "PAA" 
},
{
 "Date":          16734,
"Open": 119.0815873016,
"type": "PAA" 
},
{
 "Date":          16735,
"Open": 119.0815873016,
"type": "PAA" 
},
{
 "Date":          16736,
"Open": 119.0815873016,
"type": "PAA" 
},
{
 "Date":          16737,
"Open": 119.0815873016,
"type": "PAA" 
},
{
 "Date":          16738,
"Open": 119.0815873016,
"type": "PAA" 
},
{
 "Date":          16741,
"Open": 121.0061111111,
"type": "PAA" 
},
{
 "Date":          16742,
"Open": 121.0061111111,
"type": "PAA" 
},
{
 "Date":          16743,
"Open": 121.0061111111,
"type": "PAA" 
},
{
 "Date":          16744,
"Open": 121.0061111111,
"type": "PAA" 
},
{
 "Date":          16745,
"Open": 121.0061111111,
"type": "PAA" 
},
{
 "Date":          16748,
"Open": 114.9367460317,
"type": "PAA" 
},
{
 "Date":          16749,
"Open": 114.9367460317,
"type": "PAA" 
},
{
 "Date":          16750,
"Open": 114.9367460317,
"type": "PAA" 
},
{
 "Date":          16751,
"Open": 114.9367460317,
"type": "PAA" 
},
{
 "Date":          16752,
"Open": 114.9367460317,
"type": "PAA" 
},
{
 "Date":          16755,
"Open": 117.7211904762,
"type": "PAA" 
},
{
 "Date":          16756,
"Open": 117.7211904762,
"type": "PAA" 
},
{
 "Date":          16757,
"Open": 117.7211904762,
"type": "PAA" 
},
{
 "Date":          16758,
"Open": 117.7211904762,
"type": "PAA" 
},
{
 "Date":          16759,
"Open": 117.7211904762,
"type": "PAA" 
},
{
 "Date":          16762,
"Open": 118.2595238095,
"type": "PAA" 
},
{
 "Date":          16763,
"Open": 118.2595238095,
"type": "PAA" 
},
{
 "Date":          16764,
"Open": 118.2595238095,
"type": "PAA" 
},
{
 "Date":          16766,
"Open": 118.2595238095,
"type": "PAA" 
},
{
 "Date":          16769,
"Open": 118.2595238095,
"type": "PAA" 
},
{
 "Date":          16770,
"Open": 117.1807936508,
"type": "PAA" 
},
{
 "Date":          16771,
"Open": 117.1807936508,
"type": "PAA" 
},
{
 "Date":          16772,
"Open": 117.1807936508,
"type": "PAA" 
},
{
 "Date":          16773,
"Open": 117.1807936508,
"type": "PAA" 
},
{
 "Date":          16776,
"Open": 117.1807936508,
"type": "PAA" 
},
{
 "Date":          16777,
"Open": 113.4228571429,
"type": "PAA" 
},
{
 "Date":          16778,
"Open": 113.4228571429,
"type": "PAA" 
},
{
 "Date":          16779,
"Open": 113.4228571429,
"type": "PAA" 
},
{
 "Date":          16780,
"Open": 113.4228571429,
"type": "PAA" 
},
{
 "Date":          16783,
"Open": 113.4228571429,
"type": "PAA" 
},
{
 "Date":          16784,
"Open":  108.625952381,
"type": "PAA" 
},
{
 "Date":          16785,
"Open":  108.625952381,
"type": "PAA" 
},
{
 "Date":          16786,
"Open":  108.625952381,
"type": "PAA" 
},
{
 "Date":          16787,
"Open":  108.625952381,
"type": "PAA" 
},
{
 "Date":          16790,
"Open":  108.625952381,
"type": "PAA" 
},
{
 "Date":          16791,
"Open": 107.8235714286,
"type": "PAA" 
},
{
 "Date":          16792,
"Open": 107.8235714286,
"type": "PAA" 
},
{
 "Date":          16793,
"Open": 107.8235714286,
"type": "PAA" 
},
{
 "Date":          16797,
"Open": 107.8235714286,
"type": "PAA" 
},
{
 "Date":          16798,
"Open": 107.8235714286,
"type": "PAA" 
},
{
 "Date":          16799,
"Open": 107.8235714286,
"type": "PAA" 
},
{
 "Date":          16800,
"Open": 107.8235714286,
"type": "PAA" 
} 
]
  
      if(!(opts.type==="pieChart" || opts.type==="sparklinePlus" || opts.type==="bulletChart")) {
        var data = d3.nest()
          .key(function(d){
            //return opts.group === undefined ? 'main' : d[opts.group]
            //instead of main would think a better default is opts.x
            return opts.group === undefined ? opts.y : d[opts.group];
          })
          .entries(data);
      }
      
      if (opts.disabled != undefined){
        data.map(function(d, i){
          d.disabled = opts.disabled[i]
        })
      }
      
      nv.addGraph(function() {
        var chart = nv.models[opts.type]()
          .width(opts.width)
          .height(opts.height)
          
        if (opts.type != "bulletChart"){
          chart
            .x(function(d) { return d[opts.x] })
            .y(function(d) { return d[opts.y] })
        }
          
         
        chart
  .tooltipContent( function(key, x, y){
        return '<h3>' + key + '</h3>' + 
        '<p>' + y + ' in ' + x + '</p>'
        } )
          
        chart.xAxis
  .tickFormat(
      function(d) {return d3.time.format('%B')(new Date(d*1000*3600*24));}
    )
  .axisLabel("Month")

        
        
        chart.yAxis
  .axisLabel("Rate")
      
       d3.select("#" + opts.id)
        .append('svg')
        .datum(data)
        .transition().duration(500)
        .call(chart);

       nv.utils.windowResize(chart.update);
       return chart;
      });
    };
</script>
