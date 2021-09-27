# Treating Kaggle train set as the full set,
# split the final test set out,
# and split the validation set out from that.
# library(comprehenr)

###==========================================================================###

train_test_split = function(
  df, y_cols, id_cols, feats_lst, test_size = .3, alpha = .5, target_alpha = .9,
  validate = T
) {
  # Splits df into train/test sets and input/target (X/y) sets.
  # (Must have id_col, but can be "dummy" since it's discarded for index.)
  
  # Optionally uses Wilcoxon rank-sum test on each variable to validate split.
  # This is unabashed p-hacking (or data dredging), but I justify it by the
  # fact that we're not concerned with the probability of two sets
  # coincidentally representing the same population; we just want them to seem
  # similarly representative.
  # Note: since we're looking for a split that confirms the null hypothesis,
  # we're looking for high p-values.
  
  # Parameters:
    # df: (data.frame) Full data set, including target variable(s).
    # y_cols: (c(character)) Target column(s).
    # id_cols: (c(character)) Id column(s) to drop, because df maintains index.
    # test_size: (numeric) Proportion of rows to use for test set.
      # (Does not validate.)
    # alpha: (numeric) Max acceptable probability of incorrectly rejecting the
      # null hypothesis.
        # H0 = feature n of train and of test do not represent different sets.
          # (i.e. representative split)
        # H1 = feature n of train and of test represent different supersets.
    # target_alpha: (numeric) Alpha to use if feature is target feature (i.e.
      # if feature is in y_cols).
    # validate: (bool) Should set split be validated?
  
  # Return:
    # split_lst: (list(data.frame)) (train_X, train_y, test_X, test_y)
      # train_X (data.frame) Input features in training subset.
      # train_y (data.frame) Target variable in training subset.
      # test_X (data.frame) Input features in testing subset.
      # test_y (data.frame) Target variable in testing subset.
  
  ###
  ### TO DO: Add a parameter and logic to choose whether to track best split.
  ###   Maybe store best split index globally to preserve in case of early
  ###   termination.
  ###
  
  split_lst = list(
    'train_X' = data.frame(),
    'train_y' = data.frame(),
    'test_X' = data.frame(),
    'test_y' = data.frame()
  )
  
  full_set_len = nrow(df)
  test_set_len = as.integer(test_size * full_set_len)
  
  # To track average p-values of features:
  feats_p_av_lst = vector(mode = 'list', length = length(feats_lst))
  names(feats_p_av_lst) = feats_lst
  
  
  # Split and validate until valid.
  valid_split = FALSE
  while (!valid_split) {
    # Split randomly.
    test_idx = sample(x = full_set_len, size = test_set_len)
    split_lst$train_X = select(df[-test_idx, ], -all_of(y_cols))
    split_lst$train_y = select(df[-test_idx, ], all_of(y_cols))
    split_lst$train_y[id_cols] = split_lst$train_X[id_cols]
    split_lst$test_X = select(df[test_idx, ], -all_of(y_cols))
    split_lst$test_y = select(df[test_idx, ], all_of(y_cols))
    split_lst$test_y[id_cols] = split_lst$test_X[id_cols]
    
    # Validate the split.
    if (validate) {
      # Randomize test order to "cost-average" compute.
      feats_lst = sample(feats_lst)
      
      # Test X and y separately to avoid the join compute and data copies.
      X_validation_results = validate_split(
        train = split_lst$train_X,
        test = split_lst$test_X,
        feats_lst = feats_lst,
        y_cols = y_cols,
        feats_p_val_lst = feats_p_av_lst,
        alpha = alpha,
        target_alpha = target_alpha
      )
      feats_p_av_lst = X_validation_results$p_vals
      
      if (X_validation_results$valid){
        
        y_validation_results = validate_split(
          train = split_lst$train_y,
          test = split_lst$test_y,
          feats_lst = feats_lst,
          y_cols = y_cols,
          feats_p_val_lst = feats_p_av_lst,
          alpha = alpha,
          target_alpha = target_alpha
        )
        feats_p_av_lst = y_validation_results$p_vals
        
        if (y_validation_results$valid) {
          valid_split = TRUE
        } # else { print("Invalid y split. Resampling.") }
      } # else { print("Invalid X split. Resampling.") }
    } else {valid_split = TRUE}
  }
  
  if (validate) {
    for(feat in names(feats_p_av_lst)) {
      feats_p_av_lst[[feat]] = mean(feats_p_av_lst[[feat]])
    }
    print('Average p-values:')
    print(feats_p_av_lst)
  }
  
  return(split_lst)
}

###==========================================================================###

validate_split = function(
  train, test, feats_lst, y_cols, feats_p_val_lst = NULL, alpha = .5,
  target_alpha = .9
) {
  # Conducts Wilcoxon ranks sum test column by column to test if train and test
  # represent a similar superset. (i.e., is the split stratified on every
  # feature?) Both train and test should have the same features. There should
  # be at least one numeric (i.e. continuous) feature, as the test will only
  # be performed on these columns -- this does limit the overall test to
  # continuous variables.
  
  # Parameters:
    # train: (data.frame) A subset of original set to compare to the other
      # subset, test.
    # test: (data.frame) A subset of original set to compare to the other
      # subset, train.
    # feats_lst: (list(character)) List of features to test.
    # y_cols: (c(character)) Vector of target features.
    # feats_p_val_lst: (list(character:list(double)) Dictionary of p-values to
      # to track which features are hardest to stratify.
    # alpha: (numeric) Probability of incorrectly rejecting the null hypothesis.
      # H0 = feature n of train and test does not represent different sets.
        # (i.e. representative split)
      # H1 = feature n of train and test represent different supersets.
    # target_alpha: (numeric) Alpha to use if feature is target feature (i.e.
      # if feature is in y_cols).
  
  # Return:
    # list(
      # valid: (bool) Are the sets representative of the same superset?
      # p_vals: (list(character:list(double)) feats_p_val_lst updated
    # )
  
  valid_split = T
  
  for (feat in feats_lst) {
    if (valid_split & feat %in% colnames(train) & feat %in% colnames(test)) {
      this_alpha = alpha
      
      if (feat %in% y_cols) {
        this_alpha = target_alpha
      }
      
      results = wilcox.test(
        x = as.double(train[[feat]]),
        y = as.double(test[[feat]])
      )
      
      if (!(results$p.value > this_alpha)) {
        # print("Reject null hypothesis that split is not unrepresentative:")
        valid_split = FALSE
      }
      
      if (is.null(feats_p_val_lst)) {
        feats_p_val_lst = vector(mode = 'list', length = length(feats_lst))
        names(feats_p_val_lst) = feats_lst
        for (feat_name in feats_p_val_lst) {
          feats_p_val_lst[feat_name] = list()
        }
      }
      feats_p_val_lst[[feat]] = c(feats_p_val_lst[[feat]], results$p.value)
    }
  }
  
  return(list('valid' = valid_split, 'p_vals' = feats_p_val_lst))
}

###==========================================================================###

write_sets = function(set_lst, prefix, file_path, row.names = FALSE) {
  # Writes dataframes to CSV files. (Split sets in this application.)
  
  # Parameters:
    # set_lst (list(char: data.frame)): list of dataframes to write.
    # prefix (char): prefix to give filenames.
    # file_path (char): file path to directory to write files to.
    # row.names (bool): whether dataframes have row names.
  
  # Return:
    # None. Writes files.
  
  for (set_name in names(set_lst)) {
    write.csv(
      set_lst[[set_name]],
      paste(file_path, prefix, set_name, '.csv', sep = ''),
      row.names = row.names
    )
  }
}

###==========================================================================###

adversarial_validation = function (train, test) {
  # Performs adversarial validation with caret's xgbTree. If XGBoost can't
  # differentiate between the train and the test set well (classify them), then
  # the split is good.
  
  # Parameters:
    # train (data.frame): train set to validate.
    # test (data.frame): test set to validate.
  
  # Return:
    # trainer (object): model object returned by caret's train with xgbTree.
  
  # Label and shuffle.
  X_test = test %>%
    # select(where(is.numeric | is.ordered)) %>%
    mutate(AV_label = 0)
  X_train = train %>%
    # select(where(is.numeric | is.ordered)) %>%
    mutate(AV_label = 1)
  
  all_df = rbind(X_test, X_train)
  all_df$AV_label = factor(all_df$AV_label)
  
  shuffled_idx = sample(x = 1:nrow(all_df), size = nrow(all_df))
  half_shuff_length = length(shuffled_idx) / 2
  test_idx = shuffled_idx[1:half_shuff_length]
  train_idx = shuffled_idx[(half_shuff_length + 1) : length(shuffled_idx)]
  
  X_test = all_df[test_idx, ]
  X_train = all_df[train_idx, ]
  test_lbls = as.matrix(X_test[ , 'AV_label']) # as.matrix for some preProcesses
  train_lbls = as.matrix(X_train[ , 'AV_label'])
  colnames(test_lbls) = c('AV_label')
  colnames(train_lbls) = c('AV_label')
  X_test = select(X_test, -c('AV_label'))
  X_train = select(X_train, -c('AV_label'))
  
  # # Unnecessary if passing to XGBoost.
  # Train and predict
  # imper = preProcess(x = X_train, method = 'knnImpute')
  # print('imper:')
  # imper
  # X_train = predict(imper, newdata = X_train)
  # X_test = predict(imper, newdata = X_test)
  
  # # Ruins everything. Resets data types and changes to matrix.
  # # Try subsetting first, then adding back.
  # dummer = dummyVars(formula = ~ ., data = X_train)
  # print('dummer:')
  # dummer
  # X_train = predict(dummer, newdata = X_train)
  # X_test = predict(dummer, newdata = X_test)
  
  ranger = preProcess(X_train, method = 'range')
  X_train = predict(ranger, newdata = X_train)
  X_test = predict(ranger, newdata = X_test)
  
  X_test = cbind(X_test, test_lbls)
  X_train = cbind(X_train, train_lbls)
  
  # X_test = as.data.frame(X_test)
  # X_train = as.data.frame(X_train)
  
  trainer = train(
    AV_label ~ .,
    data = X_train,
    method = 'xgbTree',
    na.action = na.pass
  )
  print("summary(trainer$results$Accuracy):")
  print(summary(trainer$results$Accuracy))
  
  return(trainer)
}