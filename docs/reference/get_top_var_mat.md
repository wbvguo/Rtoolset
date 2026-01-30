# get the the var_gene_list and topN (or top percent) most variable genes

get the the var_gene_list and topN (or top percent) most variable genes

## Usage

``` r
get_top_var_mat(count_df, prop = 0.2, topN = NULL)
```

## Arguments

- count_df:

  a data.frame of count data, rows are genes and columns are samples

- prop:

  proportion of top genes to return, default is 0.2

- topN:

  number of top genes to return, default is NULL

## Value

a list of variance (`var_genes_list`) and the expression matrix of
most-variable genes (`topN_mat`)

## Examples

``` r
get_top_var_mat(count_df)
#> $var_genes
#>    gene_1    gene_2    gene_3    gene_4    gene_5    gene_6    gene_7    gene_8 
#> 103.67368 191.25000 135.83947  75.83947  72.48421 147.93684 119.08158  62.83158 
#>    gene_9   gene_10   gene_11   gene_12   gene_13   gene_14   gene_15   gene_16 
#> 134.35789 109.41842  84.34474 101.35789  99.50263  74.46316  97.52368  97.92368 
#>   gene_17   gene_18   gene_19   gene_20   gene_21   gene_22   gene_23   gene_24 
#>  79.62895  87.58947 154.80000  56.43158 106.04211 117.71316 107.18684  77.85263 
#>   gene_25   gene_26   gene_27   gene_28   gene_29   gene_30   gene_31   gene_32 
#> 111.67368  41.94474 101.06316  70.13421 121.72632  89.46053  86.47368 121.73421 
#>   gene_33   gene_34   gene_35   gene_36   gene_37   gene_38   gene_39   gene_40 
#>  48.66053 101.43158  78.21053  93.25000 116.68158  91.88421 173.88421  76.25263 
#>   gene_41   gene_42   gene_43   gene_44   gene_45   gene_46   gene_47   gene_48 
#>  86.82895 104.83158 158.02895 145.31316  91.16842  64.97632  68.90526  68.25263 
#>   gene_49   gene_50   gene_51   gene_52   gene_53   gene_54   gene_55   gene_56 
#> 137.62895  85.60789 125.85263 124.69474  72.51316 118.72632  92.62105 196.40789 
#>   gene_57   gene_58   gene_59   gene_60   gene_61   gene_62   gene_63   gene_64 
#>  62.45000  74.93684 125.81842 118.23947 115.60789  75.67105  56.58947  63.41842 
#>   gene_65   gene_66   gene_67   gene_68   gene_69   gene_70   gene_71   gene_72 
#>  89.22105 140.97632  88.58947 108.47105  75.31316  68.73684 103.83947  98.78684 
#>   gene_73   gene_74   gene_75   gene_76   gene_77   gene_78   gene_79   gene_80 
#> 190.05000 115.14737 101.83947  94.57632  82.99737  63.18684  32.89211  69.98684 
#>   gene_81   gene_82   gene_83   gene_84   gene_85   gene_86   gene_87   gene_88 
#>  86.15789 110.77895 124.25263 152.05000  70.76579 117.29211 145.29211 100.13421 
#>   gene_89   gene_90   gene_91   gene_92   gene_93   gene_94   gene_95   gene_96 
#> 131.57895  76.55526 170.67368  82.19737 103.77895 105.05263  63.21053 103.06316 
#>   gene_97   gene_98   gene_99  gene_100 
#>  77.85263  92.66053  57.72632 193.18684 
#> 
#> $topN_mat
#>          sample_1 sample_2 sample_3 sample_4 sample_5 sample_6 sample_7
#> gene_56       115      100      111       81       90       84      103
#> gene_100       88      108       89      128       88      121       91
#> gene_2        104      111       98      109      109      122      121
#> gene_73       101      100      130       99       95      115      118
#> gene_39       123       95      129       97       91       93       91
#> gene_91        93      106      109       73       76      126       90
#> gene_43        75      109      102       85       97       93       94
#> gene_19       123       94       93       93      109      117       82
#> gene_84       124      101      110      106       92      122       94
#> gene_6         93       87      115       90      122       96       92
#> gene_44        96       87       98       89       99      109       92
#> gene_87        88       99       93       97       92      102       97
#> gene_66       101      123      124      113      105      103       85
#> gene_49       102      112       99       96      130      101       95
#> gene_3        102      111       90      109      118      114      101
#> gene_9         84       95      103       91      100      102       83
#> gene_89       101       96      102       79       88       87      103
#> gene_51        93       94      106      101       97       89      101
#> gene_59        99      111      117       93      102       93      104
#> gene_52       110      114       99      102       96       88      118
#>          sample_8 sample_9 sample_10 sample_11 sample_12 sample_13 sample_14
#> gene_56        86      107       106       125        90       117       121
#> gene_100      100      105        69        96        85        99        87
#> gene_2         71      108       108        93        83       125        97
#> gene_73        99       91       110        94       108        95       120
#> gene_39       103       98        97       112        97       108       116
#> gene_91       117       91        99        92       100        96        95
#> gene_43        91       90       102       125        88       120        79
#> gene_19        96      108       117       109       116       106       120
#> gene_84        95      107       114        77        94       105       109
#> gene_6        104      111        99       100       104       126        95
#> gene_44        98      117       116       101        96       105       125
#> gene_87       117       99       105        93       102        91       106
#> gene_66       111       95       100        80       119       115       116
#> gene_49        99      124        93        87        89       103        84
#> gene_3         92      100        92       106       111        83       115
#> gene_9         99      103       111       112       129        93        97
#> gene_89       113      102       112       103       114        97       112
#> gene_51       103      101        90       108       131       107        74
#> gene_59        99      119       116       102       107       101        97
#> gene_52        88      101       103        80       105        87       110
#>          sample_15 sample_16 sample_17 sample_18 sample_19 sample_20
#> gene_56        105       111       114        79       116        94
#> gene_100       101       111        89       111       108       109
#> gene_2         116       104       102       106        83       115
#> gene_73        100        97       118        72       114        83
#> gene_39        123        97        76        94       104       114
#> gene_91         83        98       108       103        87       110
#> gene_43         90       103        92       111        97       104
#> gene_19         99        95        78       100       111       110
#> gene_84         96        98       102        90        80       113
#> gene_6         122       107       109        85        89       102
#> gene_44        107       109       109        78       100        80
#> gene_87        111        87        77       102       134        95
#> gene_66         98       100       101        93       108       113
#> gene_49         95       101       109       113        93        94
#> gene_3         103        80       102        98        85       119
#> gene_9         108       103       124        95        92       104
#> gene_89         99        77        99       112       105       119
#> gene_51         89       101       106        94       109       102
#> gene_59         69        90       100        98        90        96
#> gene_52         85       103       103       121        98        93
#> 
```
