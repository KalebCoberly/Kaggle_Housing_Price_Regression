# library(tidyverse)
# # library(dict) #Still not found after installation
# library(container) # For Dict class
# library(comprehenr)
# library(GGally)
# library(reshape2)
# library(gridExtra)
# library(gplots)
# library(DescTools)

# source('tools/wrangle.R')

###==========================================================================###

find_best_normalizer = function(x, funcs_lst) {
  # Find the transformation (funcs_lst$func; e.g. log10) that best normalizes a
  # feature (x), applying Shapiro-Wilk normality test.
  # H0 = Population is normally distributed.
    # Greater p-value is better. Greater W when comparing trans of same vector?
  
  # Parameters:
    # x : (c(num)) Continuous variable to normalize.
    # funcs_lst: (list(chr: fun)) Dictionary of functions to try.
  
  # Return:
    # all_funcs_results: list(
    #   'results': list(
    #     <name of function> (chr): list( ## shapiro.test return object.
    #       'statistic': (Named num) w
    #       'p.value': (num)
    #       'method': (chr)
    #       'data.name': (chr)
    #     )
    #   ),
    #   'best_func': <name of function> (chr)
    # )
  
  all_funcs_results = list(
    'results' = vector(mode = 'list', length = length(names(funcs_lst))),
    'best_func' = list('name' = NULL, 'func' = NULL)
  )
  names(all_funcs_results$results) = names(funcs_lst)
  best_p = 0
  
  for (func in names(funcs_lst)) {
    all_funcs_results$results[[func]] = shapiro.test(x = funcs_lst[[func]](x))
    if (
      !is.na(all_funcs_results$results[[func]]$p.value) &
      all_funcs_results$results[[func]]$p.value > best_p
    ) {
      all_funcs_results$best_func$name = func
      all_funcs_results$best_func$func = funcs_lst[[func]]
      best_p = all_funcs_results$results[[func]]$p.value
    }
  }
  
  return(all_funcs_results)
}


###==========================================================================###


find_best_normalizer_per_feat =  function (
  df, feats_lst, funcs_lst, exclude_vals
) {
  # For each feature, find the transformation (funcs_lst$func; e.g. log10)
  # that best normalizes a feature (x), applying Shapiro-Wilk normality test.
  # H0 = Population is normally distributed.
    # Greater p-value is better. Greater W when comparing trans of same vector?
    
  # Parameters:
    # df: (data.frame)
    # feat_lst: (list(chr)) Names of columns to check. Must be continuous
      # variables.
    # funcs_lst: (list(chr: fun)) Dictionary of functions to try.
    # exclude_vals: (list()) Values to exclude from test to avoid NAs.
      # e.g. 0 if applying a log transformation, though would be best to add 1.
    
  # Return:
    # all_feats_results: list(
    #   <name of feature>: list(
    #     'results': list(
    #       <name of function> (chr): list( ## shapiro.test return object.
    #         'statistic': (Named num) w
    #         'p.value': (num)
    #         'method': (chr)
    #         'data.name': (chr)
    #       )
    #     ),
    #     'best_func': <name of function> (chr)
    #   )
    # )

  all_feats_results = vector(mode = 'list', length = length(feats_lst))
  names(all_feats_results) = feats_lst
  
  for (feat in feats_lst) {
    if (feat %in% colnames(df)) {
      x = setdiff(df[!is.na(df[feat]), feat], exclude_vals)
      n = length(x)
      if (n > 3 & n < 5000) { # Necessary for shapiro.test
        all_feats_results[[feat]] = find_best_normalizer(
          x = x,
          funcs_lst = funcs_lst
        )
      } else {
        # print(paste('Number of unique values too small or big, n =', n, '. Skipping', feat, '.'))
      }
    }
  }
  
  return(all_feats_results)
}


###==========================================================================###


engineer_y = function (df) {
  # Transform and Winsorize SalePrice.
  # This is hard coded for reuse on test set.
  # Not passed by reference. Passed by copy. I think. R is opaque at times.
  
  # Parameter:
    # df: (data.frame) Must contain a numeric variable SalePrice. No NAs.
  
  # Return:
    # df: (data.frame) W/ 2 new columns `log(SalePrice)`, `Win(log(SalePrice))`.
  
  df = df %>%
    mutate('log(SalePrice)' = ifelse(SalePrice == 0, 0, log(SalePrice))) %>%
    mutate(
      'Win(log(SalePrice))' = Winsorize(
        .data[['log(SalePrice)']],
        # probs = c(0.005, 0.995),
        minval = 10.9579480025541,
        maxval = 13.2279465702719
      )
    )
  
  return(df)
}


###==========================================================================###


engineer_X = function (df) {
  # Feature engineering pipeline. Transform, add, drop variables.
  # This is hard coded for reuse on test set.
  # Not passed by reference. Passed by copy. I think. R is opaque at times.
  
  # Parameter:
  # df: (data.frame) Must contain a numeric variable SalePrice. No NAs.
  
  # Return:
  # df: (data.frame) W/ 2 new columns `log(SalePrice)`, `Win(log(SalePrice))`.
  
  df = df %>%
    select( # Drop immediately "useless" features.
      -c('Street', 'Alley', 'LandContour', 'Utilities', 'LandSlope',
         'Condition2', 'RoofMatl', 'Heating', 'CentralAir', 'Electrical',
         'X1stFlrSF', 'LowQualFinSF', 'KitchenAbvGr', 'Functional',
         'GarageYrBlt', 'GarageQual', 'GarageCond', 'PavedDrive',
         'X3SsnPorch', 'PoolArea', 'PoolQC')
    ) %>%
    mutate('log10(LotFrontage)' = log10(LotFrontage + 1)) %>%
    mutate(
      'Win(LotFrontage)' = Winsorize(
        LotFrontage,
        # probs = (0.05, 0.95),
        # na.rm = T
        minval = 34.05,
        maxval = 108
      )
    ) %>%
    mutate('log10(log10(LotArea))' = log10(log10(LotArea + 1))) %>%
    mutate(
      'Win(LotArea)' = Winsorize(
        LotArea,
        # probs = c(0.01, 0.95),
        # na.rm = T
        minval = 1975.96,
        maxval = 16946.4
      )
    ) %>%
    # I believe ordered factors will be treated as equally spaced numbers?
    # Will they scale?
    # mutate(OverallQual_int = as.integer(OverallQual)) %>%
    # mutate(OverallCond_int = as.integer(OverallCond)) %>%
    mutate(
      YearBuilt_fact = cut(
        x = YearBuilt,
        breaks = c(-Inf, 1944, 1986, Inf),
        ordered_result = T,
        labels = c('Before_1945', '1945_1986', 'After_1986')
      )
    ) %>%
    mutate('sqrt(Age)' = sqrt(YrSold - YearBuilt)) %>%
    # Could probably refactor the next two mutates since dropping the first.
    mutate(
      'YearRemodAdd.uncode' = ifelse(
        YearRemodAdd == 1950, YearBuilt, YearRemodAdd
      )
    ) %>%
    mutate(
      YearRemodAdd_fact = factor(
        cut(
          ifelse(
            YearRemodAdd.uncode == YearBuilt | is.na(YearRemodAdd.uncode),
            1949,
            YearRemodAdd.uncode
          ),
          breaks = c(-Inf, 1950, 1960, 1970, 1980, 1990, 2000, Inf),
          labels = c('None', '50s', '60s', '70s', '80s', '90s', '00s')
        )
      )
    ) %>%
    mutate('cbrt(MasVnrArea)' = MasVnrArea^(1/3)) %>%
    mutate(
      'Win(cbrt(MasVnrArea))' = ifelse(
        MasVnrArea == 0,
        0, # Only Winsorize the non-zero set.
        Winsorize(
          x = `cbrt(MasVnrArea`,
          # probs = c(0.005, 0.995),
          # na.rm = T
          minval = 1.52019153849196,
          maxval = 10.278035603362
        )
      )
    ) %>%
    # mutate('sqrt(BsmtFinSF1)' = sqrt(BsmtFinSF1)) %>%
    # mutate('sqrt(BsmtFinSF2)' = sqrt(BsmtFinSF2)) %>%
    mutate('cbrt(BsmtUnfSF)' = BsmtUnfSF^(1/3)) %>%
    mutate(
      'square(log(TotalBsmtSF))' = ifelse(TotalBsmt == 0, 0, log(TotalBsmt)^2)
    ) %>%
    mutate(
      'Win(square(log(TotalBsmtSF)))' = ifelse(
        TotalBsmtSF == 0,
        0, # Only Winsorize the non-zero set.
        Winsorize(
          `square(log(TotalBsmtSF))`,
          # probs = c(0.005, 0.995),
          # na.rm = T
          minval = 32.9890450797526,
          maxval = 60.3486636755867
        )
      )
    ) %>%
    mutate('TotalBsmtFinSF' = TotalBsmtSF - BsmtUnfSF) %>%
    mutate('sqrt(TotalBsmtFinSF)' = sqrt(TotalBsmtFinSF)) %>%
    mutate(
      'Win(sqrt(TotalBsmtFinSF))' = ifelse(
        TotalBsmtFinSF == 0,
        0, # Only Winsorize the non-zero set.
        Winsorize(
          x = `sqrt(TotalBsmtFinSF)`,
          # probs = c(0.01, 0.995),
          # na.rm = T
          minval = 4.47213595499958,
          maxval = 46.3884140769296
        )
      )
    ) %>%
    # mutate('log2(X1stFlrSF)' = log2(X1stFlrSF)) %>%
    mutate('X2ndFlrBin' = ifelse(X2ndFlrSF <= 0, 0, 1)) %>%
    mutate(
      'square(log2(GrLivArea))' = ifelse(GrLivArea <= 0, 0, log2(GrLivArea)^2)
    ) %>%
    mutate(
      'Win(square(log2(GrLivArea)))' = Winsorize(
        `square(log2(GrLivArea))`,
        # probs = c(0.002, 0.998),
        # na.rm = T
        minval = 78.8827556705776,
        maxval = 143.236548514549
      )
    ) %>%
    mutate(
      'Win(TotBaths)' = Winsorize(
        FullBath + BsmtFullBath + 0.5*HalfBath + 0.5*BsmtHalfBath,
        # probs = c(0.002, 0.998),
        # na.rm = T
        minval = 1,
        maxval = 4.786
      )
    ) %>%
    mutate(
      'Win(BedroomAbvGr)' = Winsorize(
        BedroomAbvGr,
        # probs = c(0.002, 0.998),
        # na.rm = T
        minval = 1,
        maxval = 5.42999999999995
      )
    ) %>%
    mutate(
      'Win(TotRmsAbvGrd)' = Winsorize(
        TotRmsAbvGrd,
        # probs = c(0, 0.975),
        # na.rm = T
        minval = 2,
        maxval = 10.15
      )
    ) %>%
    # Encoded in FirePlaceQu.
    # mutate('Fireplaces.bin' = ifelse(Fireplaces == 0, 0, 1)) %>%
    mutate('sqrt(sqrt(WoodDeckSF))' = sqrt(sqrt(WoodDeckSF))) %>%
    mutate('cbrt(WoodDeckSF)' = WoodDeckSF^(1/3)) %>%
    mutate(
      'Win(cbrt(WoodDeckSF))' = ifelse(
        WoodDeckSF == 0,
        0,
        Winsorize(
          `cbrt(WoodDeckSF)`,
          # probs = c(0.01, 0.99),
          # na.rm = T
          minval = 2.99287415882937,
          maxval = 8.48252791006478
        )
      )
    ) %>%
    mutate('OpenPorch.bin' = ifelse(OpenPorchSF == 0, 0, 1)) %>%
    mutate('ScreenPorch.bin' = ifelse(ScreenPorch == 0, 0, 1)) %>%
    mutate('cbrt(ScreenPorch)' = ScreenPorch^(1/3)) %>%
    mutate(
      'log(MiscVal)' = ifelse(
        MiscVal == 0,
        0,
        log(MiscVal)
      )
    ) %>% # Drop after use.
    select(-c('YearRemodAdd.uncode', 'FullBath', 'BsmtFullBath', 'HalfBath',
              'BsmtHalfBath'))

    
    # None of these matter on their own in this set.
    # mutate(MoSold = factor(MoSold)) %>%
    # mutate(YrSold = factor(YrSold, ordered = T)) %>%
    # mutate(
    #   SoldDate = as.Date(
    #     paste(
    #       as.character(YrSold),
    #       as.character(MoSold),
    #       '15',
    #       sep = '/'
    #     ),
  #     format = '%Y/%m/%d'
  #   )
  # )
  
  return(df)
}


###==========================================================================###


# engineer_Xy = function (X, y) {
#   return(list(engineer_X(df = X), engineer_y(df = y)))
# }

engineer_all = function (X, y) {
  # Depends on source('tools/wrangle.R')
  X = engineer_X(df = wrangle(df = X)$df)
  y = engineer_y(df = y)
  
  return(list(X, y))
}