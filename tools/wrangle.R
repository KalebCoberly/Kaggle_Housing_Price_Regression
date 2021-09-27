# library(tidyverse)
# # library(dict) #Still not found after installation
# library(container) # For Dict class
# library(comprehenr)
# library(GGally)
# library(reshape2)
# library(gridExtra)
# library(gplots)
# library(DescTools)

###==========================================================================###

wrangle = function(df) {
  
  ###========================================================================###
  
  # Create dictionary of factors to their original levels.
  # Probably could write script to read in from data dict,
  # but I want to put eyes on it anyway, check against actual table.
  # It's nice to have it visible in the code, too.
  # A set with many more features would require a script.
  
  make_factor_levels_dct = function(){
    dict(x = list(
      'MSSubClass' = c('20', '30', '40', '45', '50', '60', '70', '75', '80',
                       '85', '90', '120', '150', '160', '180', '190'),
      'MSZoning' = c('A', 'C', 'FV', 'I', 'RH', 'RL', 'RP', 'RM'),
      'Street' = c('None', 'Grvl', 'Pave'),
      'Alley' = c('None', 'Grvl', 'Pave'),
      'LotShape' = c('Reg', 'IR1', 'IR2', 'IR3'),
      'LandContour' = c('Lvl', 'Bnk', 'HLS', 'Low'),
      'Utilities' = c('None', 'ELO', 'NoSeWa', 'NoSewr', 'AllPub'),
      'LotConfig' = c('Inside', 'Corner', 'CulDSac', 'FR2', 'FR3'),
      'LandSlope' = c('None', 'Gtl', 'Mod', 'Sev'),
      'Neighborhood' = c('Blmngtn', 'Blueste', 'BrDale', 'BrkSide', 'ClearCr',
                         'CollgCr', 'Crawfor', 'Edwards', 'Gilbert', 'IDOTRR',
                         'MeadowV', 'Mitchel', 'Names', 'NoRidge', 'NPkVill',
                         'NridgHt', 'NWAmes', 'OldTown', 'SWISU', 'Sawyer',
                         'SawyerW', 'Somerst', 'StoneBr', 'Timber', 'Veenker'),
      'Condition1' = c('Artery', 'Feedr', 'Norm', 'RRNn', 'RRAn', 'PosN', 'PosA',
                       'RRNe', 'RRAe'),
      'Condition2' = c('Artery', 'Feedr', 'Norm', 'RRNn', 'RRAn', 'PosN', 'PosA',
                       'RRNe', 'RRAe'),
      'BldgType' = c('1Fam', '2FmCon', 'Duplx', 'TwnhsE', 'TwnhsI'),
      'HouseStyle' = c('1Story', '1.5Fin', '1.5Unf', '2Story', '2.5Fin',
                       '2.5Unf', 'SFoyer', 'SLvl'),
      'OverallQual' = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
      'OverallCond' = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
      'RoofStyle' = c('Flat', 'Gable', 'Gambrel', 'Hip', 'Mansard', 'Shed'),
      'RoofMatl' = c('ClyTile', 'CompShg', 'Membran', 'Metal', 'Roll', 'Tar&Grv',
                     'WdShake', 'WdShngl'),
      'Exterior1st' = c('AsbShng', 'AsphShn', 'BrkComm', 'BrkFace', 'CBlock',
                        'CemntBd', 'HdBoard', 'ImStucc', 'MetalSd', 'Other',
                        'Plywood', 'PreCast', 'Stone', 'Stucco', 'VinylSd',
                        'Wd Sdng', 'WdShing'),
      'Exterior2nd' = c('AsbShng', 'AsphShn', 'BrkComm', 'BrkFace', 'CBlock',
                        'CemntBd', 'HdBoard', 'ImStucc', 'MetalSd', 'Other',
                        'Plywood', 'PreCast', 'Stone', 'Stucco', 'VinylSd',
                        'Wd Sdng', 'WdShing', 'None'),
      'MasVnrType' = c('BrkCmn', 'BrkFace', 'CBlock', 'None','Stone'),
      'ExterQual' = c('Po', 'Fa', 'TA', 'Gd', 'Ex'),
      'ExterCond' = c('Po', 'Fa', 'TA', 'Gd', 'Ex'),
      'Foundation' = c('BrkTil', 'CBlock', 'PConc', 'Slab', 'Stone', 'Wood'),
      'BsmtQual' = c('None', 'Po', 'Fa', 'TA', 'Gd', 'Ex'),
      'BsmtCond' = c('None', 'Po', 'Fa', 'TA', 'Gd', 'Ex'),
      'BsmtExposure' = c('None', 'No', 'Mn', 'Av', 'Gd'),
      'BsmtFinType1' = c('None', 'Unf', 'LwQ', 'Rec', 'BLQ', 'ALQ', 'GLQ'),
      'BsmtFinType2' = c('None', 'Unf', 'LwQ', 'Rec', 'BLQ', 'ALQ', 'GLQ'),
      'Heating' = c('None', 'Floor', 'GasA', 'GasW', 'Grav', 'OthW', 'Wall'),
      'HeatingQC' = c('None', 'Po', 'Fa', 'TA', 'Gd', 'Ex'),
      'CentralAir' = c('N', 'Y'),
      'Electrical' = c('None', 'SBrkr', 'FuseA', 'FuseF', 'FuseP', 'Mix'),
      'KitchenQual' = c('Po', 'Fa', 'TA', 'Gd', 'Ex'),
      'Functional' = c('Sal', 'Sev', 'Maj2', 'Maj1', 'Mod', 'Min2', 'Min1',
                       'Typ'),
      'FireplaceQu' = c('None', 'Po', 'Fa', 'TA', 'Gd', 'Ex'),
      'GarageType' = c('2Types', 'Attchd', 'Basment', 'BuiltIn', 'CarPort',
                       'Detchd', 'None'),
      'GarageFinish' = c('None', 'Unf', 'RFn', 'Fin'),
      'GarageQual' = c('None', 'Po', 'Fa', 'TA', 'Gd', 'Ex'),
      'GarageCond' = c('None', 'Po', 'Fa', 'TA', 'Gd', 'Ex'),
      'PavedDrive' = c('None', 'N', 'P', 'Y'),
      'PoolQC' = c('None', 'Po', 'Fa', 'TA', 'Gd', 'Ex'),
      'Fence' = c('GdPrv', 'MnPrv', 'GdWo', 'MnWw', 'None'),
      'MiscFeature' = c('Elev', 'Gar2', 'Othr', 'Shed', 'TenC', 'None'),
      'SaleType' = c('WD', 'CWD', 'VWD', 'New', 'COD', 'Con', 'ConLw', 'ConLI',
                     'ConLD', 'Oth'),
      'SaleCondition' = c('Normal', 'Abnorml', 'AdjLand', 'Alloca', 'Family',
                          'Partial')
    ))
  }
  
  ###========================================================================###
  
  make_level_maps_dct = function(factor_levels_dct){
    # Audit factor levels.
    # Handle miscasing. Map uppercase to canonical value.
    # Handle typos.
    
    # Parameters:
      # factor_levels_dct (dict): created by make_factor_levels_dct above.
    
    # Return:
      # factor_levels_dct (dict)...
    
    level_maps_dct = dict()
    
    for (col_name in keys(factor_levels_dct)) {
      col_dct = dict()
      for (level in factor_levels_dct[col_name]) {
        col_dct[toupper(level)] = level
      }
      level_maps_dct[col_name] = col_dct
    }
    
    # Manually adding found typos.
    level_maps_dct[['MSZoning']][toupper('C (all)')] = 'C'
    level_maps_dct[['BldgType']][toupper('Duplex')] = 'Duplx'
    level_maps_dct[['BldgType']][toupper('Twnhs')] = 'Twnhs' #New value
    level_maps_dct[['Exterior2nd']][toupper('CmentBd')] = 'CemntBd'
    level_maps_dct[['Exterior2nd']][toupper('Brk Cmn')] = 'BrkComm'
    level_maps_dct[['Exterior2nd']][toupper('Wd Shng')] = 'WdShing'
    
    return(level_maps_dct)
  }
  
  ###========================================================================###
  
  map_levels = function(x, level_map, col_name) {
    # Maps factor values to appropriate values. (Called by sapply().)
    
    # Parameters:
      # x (char): value in column.
      # level_map (dict(char: char)): anticipated level values mapped to
        # accepted level values.
      # col_name (char): name of column.
    
    # Return:
      # x (char): input level value mapped to accepted level value.
    
    # Fix mis-cased values.
    caps_x = toupper(x)
    if (caps_x %in% keys(level_map)) {
      x = level_map[caps_x]
    } else if (!is.na(x)) {
      print('x not in map.')
      print(paste('x:', x))
      print(paste('col_name:', col_name))
    }
    
    return(x)
  }
  
  ###========================================================================###
  
  handle_factors = function(x, level_map, col_name) {
    # Applies vectorized operations to x (a column).
    # Namely, maps level values to accepted level values. (Called by sapply().)
    
    # Parameters:
      # x: (vector(character)?) Column from data.frame, a factor.
      # level_map: (dict(character:character)) Anticipated level values mapped to
        # accepted level values.
      # col_name: (character) Name of column.
    
    # Return:
      # x: (vector(character)?) Modified values.
    
    x = sapply(
      X = x,
      FUN = map_levels,
      level_map = level_map,
      col_name = col_name
    )
    
    return(x)
  }
  
  ###========================================================================###
  
  map_new_levels = function(df, factor_levels_dct, FUN, level_maps_dct) {
    # Just add remaining unknown levels to the canon.
    
    # Parameters:
      # df: (data.frame) HousePriceRegression set with identical features.
        # (Excludes 'SalePrice'.)
      # factor_levels_dct: (dict(character:c(character)))
        # Key: column name, Value: accepted level values.
      # FUN: (function) To pass to sapply().
      # level_maps_dct: (dict(character:dict(character:character)))
        # Column names mapped to dictionary of anticipated level values mapped to
        # accepted level values.
    
    # Return:
      # list(data.frame, dict): modified dataframe and dictionary of levels
        # mapped to their factors.
    
    for (col_name in keys(factor_levels_dct)) {
      
      # Map known typos.
      df[col_name] = sapply(
        X = df[col_name],
        FUN = FUN,
        level_map = level_maps_dct[col_name],
        col_name = col_name
      )
      
      # Add all other values to the factor's level dictionary/map.
      levels = unique(df[col_name])
      for (level_name in levels[[col_name]]) {
        if (!is.na(level_name) &
            !(toupper(level_name) %in% keys(level_maps_dct[col_name]))) {
          print('New level:')
          print(level_name)
          print('Found in:')
          print(col_name)
          factor_levels_dct[[col_name]] = c(factor_levels_dct[col_name],
                                            level_name)
          level_maps_dct[[col_name]][toupper(level_name)] = level_name
          print('Added.')
        }
      }
    }
    
    return(list(df, level_maps_dct))
  }
  
  ###========================================================================###
  
  set_types = function(df, factor_levels_dct) {
    # Sets data types of features in df with these hard-coded features.
    # Sets factor levels using factor_levels_dct.
    
    # Parameters:
      # df: (data.frame) HousePriceRegression set with identical features.
        # (Excludes 'SalePrice'.)
      # factor_levels_dct: (dict(character:c(character)))
        # Key: column name, Value: accepted level values.
    
    # Return:
      # (default output; no return statement): (data.frame) df with types set.
    
    df %>%
      mutate(Id = as.character(Id)) %>%
      mutate(
        MSSubClass = factor(
          MSSubClass,
          levels = factor_levels_dct['MSSubClass']
        )
      ) %>%
      mutate(
        MSZoning = factor(
          MSZoning,
          levels = factor_levels_dct['MSZoning']
        )
      ) %>%
      mutate(LotFrontage = as.numeric(LotFrontage)) %>%
      mutate(LotArea = as.numeric(LotArea)) %>% ## NAs = 0?
      mutate(
        Street = factor(
          Street, #ifelse(is.na(Street) | 'NA', 'None', Street),
          ordered = TRUE,
          levels = factor_levels_dct['Street']
        )
      ) %>%
      mutate(
        Alley = factor(
          Alley, #ifelse(is.na(Alley) | 'NA', 'None', Alley),
          ordered = TRUE,
          levels = factor_levels_dct['Alley']
        )
      ) %>%
      mutate(
        LotShape = factor(
          LotShape,
          ordered = TRUE,
          levels = factor_levels_dct['LotShape']  
        )
      ) %>%
      mutate(
        LandContour = factor(
          LandContour,
          levels = factor_levels_dct['LandContour']
        )
      ) %>%
      mutate(
        Utilities = factor(
          Utilities, #ifelse(is.na(Utilities) | 'NA', 'None', Utilities),
          ordered = TRUE,
          levels = factor_levels_dct['Utilities']
        )
      ) %>%
      mutate(
        LotConfig = factor(
          LotConfig,
          levels = factor_levels_dct['LotConfig']
        )
      ) %>%
      mutate(
        LandSlope = factor(
          LandSlope,
          ordered = TRUE,
          levels = factor_levels_dct['LandSlope']
        )
      ) %>%
      mutate(
        Neighborhood = factor(
          Neighborhood,
          levels = factor_levels_dct['Neighborhood']
        )
      ) %>%
      mutate(
        Condition1 = factor(
          Condition1,
          levels = factor_levels_dct['Condition1']
        )
      ) %>%
      mutate(
        Condition2 = factor(
          Condition2,
          factor_levels_dct['Condition2']
        )
      ) %>%
      mutate(
        BldgType = factor(
          BldgType,
          levels = factor_levels_dct['BldgType']
        )
      ) %>%
      mutate(
        HouseStyle = factor(
          HouseStyle,
          factor_levels_dct['HouseStyle']
        )
      ) %>%
      mutate(
        OverallQual = factor(
          OverallQual,
          ordered = TRUE,
          levels = factor_levels_dct['OverallQual']
        )
      ) %>%
      mutate(
        OverallCond = factor(
          OverallCond,
          ordered=TRUE,
          levels = factor_levels_dct['OverallCond']
        )
      ) %>%
      mutate(YearBuilt = as.integer(YearBuilt)) %>%
      mutate(YearRemodAdd = as.integer(YearRemodAdd)) %>%
      mutate(
        RoofStyle = factor(
          RoofStyle,
          levels = factor_levels_dct['RoofStyle']
        )
      ) %>%
      mutate(
        RoofMatl = factor(
          RoofMatl,
          levels = factor_levels_dct['RoofMatl']
        )
      ) %>%
      mutate(
        Exterior1st = factor(
          Exterior1st,
          levels = factor_levels_dct['Exterior1st']
        )
      ) %>%
      mutate(
        Exterior2nd = factor(
          Exterior2nd,
          levels = factor_levels_dct['Exterior2nd']
        )
      ) %>%
      mutate(
        MasVnrType = factor(
          MasVnrType,
          levels = factor_levels_dct['MasVnrType']
        )
      ) %>%
      mutate(MasVnrArea = as.numeric(MasVnrArea)) %>%
      mutate(
        ExterQual = factor(
          ExterQual,
          ordered = TRUE,
          levels = factor_levels_dct['ExterQual']
        )
      ) %>%
      mutate(
        ExterCond = factor(
          ExterCond,
          ordered = TRUE,
          levels = factor_levels_dct['ExterCond']
        )
      ) %>%
      mutate(Foundation = factor(Foundation)) %>%
      mutate(
        BsmtQual = factor(
          BsmtQual, #ifelse(is.na(BsmtQual) | 'NA', 'None', BsmtQual),
          ordered = TRUE,
          levels = factor_levels_dct['BsmtQual']
        )
      ) %>%
      mutate(
        BsmtCond = factor(
          BsmtCond, #ifelse(is.na(BsmtCond) | 'NA', 'None', BsmtCond),
          ordered = TRUE,
          levels = factor_levels_dct['BsmtCond']
        )
      ) %>%
      mutate(
        BsmtExposure = factor(
          BsmtExposure, #ifelse(is.na(BsmtExposure) | 'NA', 'None', BsmtExposure),
          ordered = TRUE,
          levels = factor_levels_dct['BsmtExposure']
        )
      ) %>%
      mutate(
        BsmtFinType1 = factor(
          BsmtFinType1, #ifelse(is.na(BsmtFinType1) | 'NA', 'None', BsmtFinType1),
          ordered =TRUE,
          levels = factor_levels_dct['BsmtFinType1']
        )
      ) %>%
      mutate(BsmtFinSF1 = as.numeric(BsmtFinSF1)) %>%
      mutate(
        BsmtFinType2 = factor(
          BsmtFinType2, #ifelse(is.na(BsmtFinType1) | 'NA', 'None', BsmtFinType2),
          ordered = TRUE,
          levels = factor_levels_dct['BsmtFinType2']
        )
      ) %>%
      mutate(BsmtFinSF2 = as.numeric(BsmtFinSF2)) %>%
      mutate(BsmtUnfSF = as.numeric(BsmtUnfSF)) %>%
      mutate(TotalBsmtSF = as.numeric(TotalBsmtSF)) %>%
      mutate(
        Heating = factor(
          Heating,
          levels = factor_levels_dct['Heating']
        )
      ) %>%
      mutate(
        HeatingQC = factor(
          HeatingQC,
          ordered = TRUE,
          levels = factor_levels_dct['HeatingQC']
        )
      ) %>%
      mutate(
        CentralAir = factor(
          CentralAir,
          ordered = TRUE,
          levels = factor_levels_dct['CentralAir']
        )
      ) %>%
      mutate(
        Electrical = factor(
          Electrical,
          levels = factor_levels_dct['Electrical']
        )
      ) %>%
      mutate(X1stFlrSF = as.numeric(X1stFlrSF)) %>%
      mutate(X2ndFlrSF = as.numeric(X2ndFlrSF)) %>%
      mutate(LowQualFinSF = as.numeric(LowQualFinSF)) %>%
      mutate(GrLivArea = as.numeric(GrLivArea)) %>%
      mutate(BsmtFullBath = as.integer(BsmtFullBath)) %>%
      mutate(BsmtHalfBath = as.integer(BsmtHalfBath)) %>%
      mutate(FullBath = as.integer(FullBath)) %>%
      mutate(HalfBath = as.integer(HalfBath)) %>%
      mutate(BedroomAbvGr = as.integer(BedroomAbvGr)) %>%
      mutate(KitchenAbvGr = as.integer(KitchenAbvGr)) %>%
      mutate(
        KitchenQual = factor(
          KitchenQual,
          ordered = TRUE,
          levels = factor_levels_dct['KitchenQual']
        )
      ) %>%
      mutate(TotRmsAbvGrd = as.integer(TotRmsAbvGrd)) %>%
      mutate(
        Functional = factor(
          Functional,
          ordered = TRUE,
          levels = factor_levels_dct['Functional']
        )
      ) %>%
      mutate(Fireplaces = as.integer(Fireplaces)) %>%
      mutate(
        FireplaceQu = factor(
          FireplaceQu, #ifelse(is.na(FireplaceQu) | 'NA', 'None', FireplaceQu),
          ordered = TRUE,
          levels = factor_levels_dct['FireplaceQu']
        )
      ) %>%
      mutate(
        GarageType = factor(
          GarageType, #ifelse(is.na(GarageType) | 'NA', 'None', GarageType),
          levels = factor_levels_dct['GarageType']
        )
      )%>%
      mutate(GarageYrBlt = as.integer(GarageYrBlt)) %>%
      mutate(
        GarageFinish = factor(
          GarageFinish, #ifelse(is.na(GarageFinish) | 'NA', 'None', GarageFinish),
          ordered = TRUE,
          levels = factor_levels_dct['GarageFinish']
        )
      ) %>%
      mutate(GarageCars = as.integer(GarageCars)) %>%
      mutate(GarageArea = as.numeric(GarageArea)) %>%
      mutate(
        GarageQual = factor(
          GarageQual, #ifelse(is.na(GarageQual) | 'NA', 'None', GarageQual),
          ordered = TRUE,
          levels = factor_levels_dct['GarageQual']
        )
      ) %>%
      mutate(
        GarageCond = factor(
          GarageCond, #ifelse(is.na(GarageCond) | 'NA', 'None', GarageCond),
          ordered = TRUE,
          levels = factor_levels_dct['GarageCond']
        )
      ) %>%
      mutate(
        PavedDrive = factor(
          PavedDrive,
          ordered = TRUE,
          levels = factor_levels_dct['PavedDrive']
        )
      ) %>%
      mutate(WoodDeckSF = as.numeric(WoodDeckSF)) %>%
      mutate(OpenPorchSF = as.numeric(OpenPorchSF)) %>%
      mutate(EnclosedPorch = as.numeric(EnclosedPorch)) %>%
      mutate(X3SsnPorch = as.numeric(X3SsnPorch)) %>%
      mutate(ScreenPorch = as.numeric(ScreenPorch)) %>%
      mutate(PoolArea = as.numeric(PoolArea)) %>%
      mutate(
        PoolQC = factor(
          PoolQC, #ifelse(is.na(PoolQC) | 'NA', 'None', PoolQC),
          ordered = TRUE,
          levels = factor_levels_dct['PoolQC']
        )
      ) %>%
      mutate(
        Fence = factor(
          Fence, #ifelse(is.na(Fence) | 'NA', 'None', Fence),
          levels = factor_levels_dct['Fence']
        )
      ) %>%
      mutate(
        MiscFeature = factor(
          MiscFeature, #ifelse(is.na(MiscFeature) | 'NA', 'None', MiscFeature),
          levels = factor_levels_dct['MiscFeature']
        )
      ) %>%
      mutate(MiscVal = as.numeric(MiscVal)) %>%
      mutate(MoSold = as.integer(MoSold)) %>% # To create compound date col later
      mutate(YrSold = as.integer(YrSold)) %>%
      mutate(
        SaleType = factor(
          SaleType,
          levels = factor_levels_dct['SaleType']
        )
      ) %>%
      mutate(
        SaleCondition = factor(
          SaleCondition,
          levels = factor_levels_dct['SaleCondition']
        )
      )#%>%
      # mutate(SalePrice = as.numeric(SalePrice))
  }
  
  ###========================================================================###
  
  impute_vals = function(df, feature_vec, value = 'None') {
    # Imputes val to NAs in each feature in list.
    # There's probably a better way to do it, like with replace(), but didn't
    # get it to work.
    
    # Parameters:
      # df: (data.frame)
      # feature_vec: (c(character)) Features to impute.
      # value: (same type as feature(s)) Value to impute with.
    
    # Return:
      # df (data.frame): passed dataframe with imputed values.
    
    for (feature in feature_vec) {
      df[is.na(df[feature]), feature] = value
    }
    
    return(df)
  }
  
  ###========================================================================###
  
  make_known_nones_vec = function() {
    # Given with data set, NAs mean none.
    
    c('Alley', 'BsmtQual', 'BsmtCond', 'BsmtExposure', 'BsmtFinType1',
      'BsmtFinType2', 'FireplaceQu', 'GarageType', 'GarageFinish', 'GarageQual',
      'GarageCond', 'PoolQC', 'Fence', 'MiscFeature')
  }
    
  ###========================================================================###
  
  make_assumed_nones_vec = function(){
    # Assuming some missing values are absence of feature, not absence of data.
    # But, for ease of project, also assuming some features won't be missing in
    # this residential subset (e.g. Kitchen).
    # Be sure to add "None" to factor_level_dct.
    
    c('Street', 'LandSlope', 'Utilities', 'Exterior2nd', 'MasVnrType',
      'Heating', 'HeatingQC', 'Electrical', 'PavedDrive')
  }
  
  ###========================================================================###
  
  make_assumed_zeros_vec = function() {
    # Assuming some numeric/integer NAs are absence of feature, not absent data.
    # LotFrontage has too many missing to assume just yet.
    # LotArea not missing any, but future set could include mobiles on park.
    # GarageYrBuilt doesn't make sense to set to 0, since it will scale weird.
    
    c('LotArea', 'MasVnrArea', 'X2ndFlrSF', 'LowQualFinSF', 'BsmtFullBath',
      'BsmtHalfBath', 'FullBath', 'HalfBath', 'Fireplaces', 'GarageCars',
      'GarageArea', 'WoodDeckSF', 'OpenPorchSF', 'EnclosedPorch', 'X3SsnPorch',
      'ScreenPorch', 'PoolArea', 'MiscVal')
  }
  
  ###========================================================================###
  
  make_unimputed_vec = function(df_colnames) {
    # Get variables to not preemptively impute given what we know and assume.
    
    # Parameters:
      # df_colnames (c(char)): vector of column names.
    
    # Returns:
      # c(char): vector of column names not found in list of columns to impute.
    
    imputed_vec = c(
      make_known_nones_vec(),
      make_assumed_nones_vec(),
      make_assumed_zeros_vec()
    )
    
    return(df_colnames[!(df_colnames %in% imputed_vec)])
  }
  
  ###========================================================================###
  
  check_feature_logic = function(df) {
    # Impute where missing related features imply None/0.
    # And, generally check logic, but not too fine-toothed for this project, Kaleb.
    
    # Parameters:
      # df (data.frame): dataframe to audit.
    
    # Return:
      # df (data.frame): audited dataframe.
    
    # Heating, HeatingQC
    # None missing in this set.
    # Rules, if implemented:
    # Impute HeatingQC to None where Heating is None.
    # df[df['Heating'] == 'None', 'HeatingQC'] = 'None'
    # On second thought, I'll not enforce this, won't necessarily improve info.
    
    # Utilities, Heating, HeatingQC, CentralAir, Electrical
    # Only Electrical missing one in the set, and AllPub Utilities are recorded.
    # So, impute to mode (SBrkr), since mode is strong.
    # Could wait and do this in preprocessing of train, but I'll do it
    # explicitly now since it is a logical part of the wrangle here.
    df[df['Electrical'] == 'None', 'Electrical'] =
      'SBrkr'
    
    # LandContour, LandSlope
    # Check where (LandSlope == 'None' | LandSlope == 'Gtl') &
    # (LandContour != 'Lvl' & LandContour != 'Low')
    # Found 79 "gentle" "hillsides" and "banks".
    # Doesn't quite add up, but I'm not going to create a policy.
    # Check where (LandSlope != 'None' & LandSlope != 'Gtl') &
    # (LandContour == 'Lvl' | LandContour != 'Low')
    # Found 44 "moderate" or "severe" slopes that are also "low" or "level."
    # Toss out "Low" since it's merely a depression without grade qualified.
    # Found 16 "moderate" yet "level" plots, one "severe" yet "level".
    # If those features seem important, I'll drop severe&level from training.
    
    # Condition1, Condition2
    # If Condition1 is Norm, then Condition2 should be as well, else swap values.
    # No such cases exist in this set, but I'll set a policy anyway.
    # View(filter(kaggle_train_set, Condition1 == 'Norm' & Condition2 != 'Norm'))
    df[(df$Condition1 == 'Norm') &
                       (df$Condition2 != 'Norm'),
                     'Condition1'] =
      df[(df$Condition1 == 'Norm') &
                         (df$Condition2 != 'Norm'),
                       'Condition2']
    
    df[(df$Condition1 == 'Norm') &
                       (df$Condition2 != 'Norm'),
                     'Condition2'] =
      'Norm'
    
    # HouseStyle, 2ndFlrSF
    # If HouseStyle is less than 2 stories, X2ndFlrSF should be zero/NA, else it should be >0.
    # View(select(filter(kaggle_train_set, (HouseStyle %in% c('1Story', '1.5Fin', '1.5Unf')) & (!is.na(X2ndFlrSF) & X2ndFlrSF > 0)), HouseStyle, X2ndFlrSF))
    # 148 matches, but only two are straight 1Story with X2ndFlrSF values.
    # I'm confident that these will be important features, but I'm not confident 
    # about which values to adjust. I'll go ahead and drop these records.
    df =
      subset(
        df,
        !((HouseStyle == '1Story') & (!is.na(X2ndFlrSF) & X2ndFlrSF > 0))
      )
    
    # Inversely, if HouseStyle is more than 1 story, it should have X2ndFlrSF.
    # View(subset(kaggle_train_set, select = c(HouseStyle, X2ndFlrSF), subset = ((HouseStyle != '1Story') & (is.na(X2ndFlrSF) | X2ndFlrSF <= 0))))
    # This returns 108 records, but none are 2Story. There's clearly confusion
    # about what constitutes 2nd-floor SF. I'm not going to fix this.
    
    # YearBuilt, YearRemodAdd
    # YearRemodAdd >= YearBuilt
    # View(subset(kaggle_train_set, subset = (YearBuilt > YearRemodAdd), select = c(YearBuilt, YearRemodAdd)))
    # None found, but I'll set a policy to swap values. Couldn't hurt.
    built_yrs = df[
      (df$YearBuilt > df$YearRemodAdd),
      'YearBuilt'
    ]
    
    df[
      (df$YearBuilt > df$YearRemodAdd),
      'YearBuilt'
    ] = 
      df[
        (df$YearBuilt > df$YearRemodAdd),
        'YearRemodAdd'
      ]
    
    df[
      (df$YearBuilt > df$YearRemodAdd),
      'YearRemodAdd'
    ] = built_yrs
    
    # Exterior1st, Exterior2nd
    # None missing. I'll ignore.
    # That said, it's odd that there's only one "Other" point. Even if "Other"
    # means none, that means that virtually all houses in Ames have more than one
    # type of Exterior??
    
    # MasVnrType, MasVnrArea
    # If there's masonry, it should have SF. If SF, should be classified.
    # View(subset(kaggle_train_set, subset = (MasVnrType != 'None' & (MasVnrArea <= 0 | is.na(MasVnrArea)))))
    # Found 2 records with type but no SF. If features important, drop records,
    # or impute SF.
    # View(subset(kaggle_train_set, subset = (MasVnrType == 'None' & (MasVnrArea > 0 & !is.na(MasVnrArea)))))
    # Found 5 records with SF but no type. If features important, drop records,
    # or impute SF..
    # Decided to add to assumed 'None'/0 vectors above, automatically imputing.
    
    # BsmtQual, BsmtCond, BsmtExposure, BsmtFinType1, BsmtFinSF1, BsmtFinType2,
    # BsmtFinSF2, BsmtUnfSF, TotalBsmtSF
    # There should be agrement on whether a basement exists, basically.
    # View(
    #   subset(
    #     kaggle_train_set,
    #     subset = (
    #       (
    #         BsmtQual != 'None' |
    #         BsmtCond != 'None' |
    #         BsmtExposure != 'None' |
    #         # BsmtFinType1 != 'None' |
    #         # BsmtFinSF1 != 0 |
    #         # BsmtFinType2 != 'None' |
    #         # BsmtFinSF2 != 0 |
    #         TotalBsmtSF != 0
    #       ) &
    #       (
    #         BsmtQual == 'None' |
    #         BsmtCond == 'None' |
    #         BsmtExposure == 'None' |
    #         # BsmtFinType1 == 'None' |
    #         # BsmtFinSF1 == 0 |
    #         # BsmtFinType2 == 'None' |
    #         # BsmtFinSF2 == 0 |
    #         TotalBsmtSF == 0
    #       )
    #     )
    #   )
    # )
    # One record found in which at least one feature claims a basement yet
    # either Qual, Cond, Exposure, or TotalSF claim no basement. I'm not going
    # to make a policy to set TotalSF in such a case, but the Exposure can be
    # set to "No" (no exposure) instead of "None" (NA).
    # I could intelligently impute the other two (missing a "No" option), but
    # that would have to wait for after the split to avoid leakage. I'll just
    # ignore it for this project.
    df[
      (
        (df$BsmtExposure == 'None') &
          (df$BsmtCond != 'None' |
             df$BsmtQual != 'None')
      ),
      'BsmtExposure'
    ] = 'No'
    # Could continue with finer-toothed comb re: FinType/SF, but won't.
    
    # 1stFlrSF, 2ndFlrSF, GrLivArea
    # There probably aren't any completely underground houses in Ames.
    # View(subset(kaggle_train_set, (GrLivArea <= 0) | is.na(GrLivArea)))
    # View(subset(kaggle_train_set, GrLivArea < X2ndFlrSF))
    
    # TotRmsAbvGrd >= (Bedroom + Kitchen)
    # View(subset(kaggle_train_set, TotRmsAbvGrd < (BedroomAbvGr + KitchenAbvGr)))
    # None found. Could set policy to make TotRms at least Bed+Kitch(+2?), but nah.
    
    # Fireplaces, FireplaceQu
    # These should agree on the existence of 1 or more.
    # View(
    #   subset(
    #     kaggle_train_set,
    #     subset =
    #       ((FireplaceQu != 'None') & (is.na(Fireplaces) | Fireplaces <= 0)) |
    #       ((FireplaceQu == 'None') & (!is.na(Fireplaces) & Fireplaces > 0)),
    #     select = c(FireplaceQu, Fireplaces)
    #   )
    # )
    # No violations found, and not going to set a policy.
    
    # GarageType, GarageYrBlt, GarageFinish, GarageCars, GarageArea, GarageQual,
    # GarageCond
    # These should agree.
    # View(
    #   subset(
    #     kaggle_train_set,
    #     subset = 
    #       (
    #         (
    #           GarageType == 'None' |
    #           is.na(GarageYrBlt) |
    #           GarageFinish == 'None' |
    #           GarageCars == 0 |
    #           GarageArea == 0 |
    #           GarageQual == 'None' |
    #           GarageCond == 'None'
    #         ) &
    #         (
    #           GarageType != 'None' |
    #           !is.na(GarageYrBlt) |
    #           GarageFinish != 'None' |
    #           GarageCars != 0 |
    #           GarageArea != 0 |
    #           GarageQual != 'None' |
    #           GarageCond != 'None'
    #         )
    #       )
    #   )
    # )
    # No matches found. No sensible policy to set. Multivariate impute train set.
    
    # PoolArea, PoolQC
    # These should agree.
    # View(
    #   subset(
    #     kaggle_train_set,
    #     subset = 
    #       (
    #         ((PoolArea <= 0 | is.na(PoolArea)) & PoolQC != 'None') |
    #         ((PoolArea != 0 & !is.na(PoolArea)) & PoolQC == 'None')
    #       )
    #   )
    # )
    # No matches found. No sensible policy to set. Multivariate impute train set.
    
    # MiscFeature, MiscVal
    # These should agree.
    # View(
    #   subset(
    #     kaggle_train_set,
    #     subset =
    #       (
    #         ((MiscVal <= 0 | is.na(MiscVal)) & MiscFeature != 'None') |
    #         ((MiscVal != 0 & !is.na(MiscVal)) & MiscFeature == 'None')
    #       ),
    #     select = c(MiscFeature, MiscVal)
    #   )
    # )
    # Two matches, but they're "Other" and "Shed" valued at $0, believable.
    
    return(df)
  }
  
  ###========================================================================###
  
  run_all = function(df) {
    # Run all above. Generally clean up before splitting and exploring.
    # Set data types and factor levels.
    # Impute NAs where known/assumed/implied.
    # Impute where related features imply None/0, and generally check logic.
    
    # Parameters:
      # df (data.frame): dataframe to wrangle.
    
    # Return:
      # list(
      #   'df' = df,
      #   'factor_levels_dct' = factor_levels_dct,
      #   'level_maps_dct' = level_maps_dct,
      #   'known_nones_vec' = known_nones_vec,
      #   'assumed_nones_vec' = assumed_nones_vec,
      #   'assumed_zeros_vec' = assumed_zeros_vec,
      #   'unimputed_vec' = unimputed_vec
      # )
    
  # Remove dupes.
    df = unique(df)
    
  # Set data types and factor levels.
    # Map non-canonical level values to canonical level values.
    # Handle miscasing, typos. Add unexpected values.
    # Create dictionary of factors to their original levels.
    # Probably could write script to read in from data dict,
    # but I want to put eyes on it anyway, check against actual table.
    # It's nice to have it visible in the code, too.
    # A set with many more features would require a script.
    
    # Map factor column names to levels.
    factor_levels_dct = make_factor_levels_dct()
    # Map level miscasing/typos to accepted values. Add new values.
    level_maps_dct = make_level_maps_dct(factor_levels_dct = factor_levels_dct)
    
    mapped_lst = map_new_levels(
      df = df,
      factor_levels_dct = factor_levels_dct,
      FUN = handle_factors,
      level_maps_dct = level_maps_dct
    )
    
    df = mapped_lst[[1]]
    level_maps_dct = mapped_lst[[2]]
    # Added unanticipated levels:
    # 'MasVnrType' : 'Masonry' BUT COUNT IS 0???
    
    # # Explicitly set data types and levels of all features.
    # df = set_types(
    #   df = df,
    #   factor_levels_dct = factor_levels_dct
    # )
    
    
  # Impute NAs where known/assumed/implied.
    # Given with data dictionary, NAs mean none with these features.
    known_nones_vec = make_known_nones_vec()
    
    # Assuming some missing values are absence of feature, not absence of data.
    # But, for ease of project, also assuming some features won't be missing in
    # this residential subset (e.g. Kitchen).
    ## Be sure to add "None" to factor_level_dct.##
    assumed_nones_vec = make_assumed_nones_vec()
    
    df = impute_vals(
      df = df,
      feature_vec = c(known_nones_vec, assumed_nones_vec),
      value = 'None'
    )
    
    # Assuming some numeric/integer NAs are absence of feature, not absent data.
    # LotFrontage has too many missing to assume just yet.
    # LotArea not missing any, but future set could include mobiles on park.
    # GarageYrBuilt doesn't make sense to set to 0, since it will scale weird.
    assumed_zeros_vec = make_assumed_zeros_vec()
    
    df = impute_vals(
      df = df,
      feature_vec = assumed_zeros_vec,
      value = 0
    )
    
    # Some of these will be imputed next where implied by related features.
    unimputed_vec = make_unimputed_vec(colnames(df))
    
    
  # Impute where related features imply None/0, and generally check logic.
  # But, not too fine-toothed for this project, Kaleb.
    df = check_feature_logic(df = df)
    
    
    # Explicitly set data types and levels of all features.
    df = set_types(
      df = df,
      factor_levels_dct = factor_levels_dct
    )
    
    return(
      list(
        'df' = df,
        'factor_levels_dct' = factor_levels_dct,
        'level_maps_dct' = level_maps_dct,
        'known_nones_vec' = known_nones_vec,
        'assumed_nones_vec' = assumed_nones_vec,
        'assumed_zeros_vec' = assumed_zeros_vec,
        'unimputed_vec' = unimputed_vec
      )
    )
  }
  
  ###========================================================================###
  
  return (run_all(df = df))
}