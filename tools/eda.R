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

transformed_hist = function (data, x, func, binwidth, x_lab, filter_vals) {
  # Creates ggplot2 histogram of variable with tranformation function applied.
  
  # Parameters:
    # data (data.frame): data.frame with variable to be transformed.
    # x (char): name of variable column in data to transform.
    # func (function): function to transform the variable.
    # binwidth (numeric): argument for geom_histogram, to set binwidth.
    # x_lab (char): x label.
    # filter_vals (bool): whether to filter out 0, 1, and NAs.
  
  # Return:
    # plt (ggplot plot): histogram.
  
  if (filter_vals) {
    data = data[data[x] != 0 & data[x] != 1 & !is.na(data[x]), ]
  }
  
  plt = ggplot(
    data = data,
    mapping = aes(x = func(.data[[x]]))
  ) +
    geom_histogram(binwidth = binwidth) +
    xlab(label = x_lab)
  
  return(plt)
}

###==========================================================================###

transformed_boxplot = function (data, x, func, x_lab, filter_vals) {
  # Creates ggplot2 notched boxplot of variable transformed by func.
  
  # Parameters:
    # data (data.frame): data.frame with variable to be transformed.
    # x (char): name of variable column in data to transform.
    # func (function): function to transform the variable.
    # x_lab (char): x label.
    # filter_vals (bool): whether to filter out 0, 1, and NAs.
  
  # Return:
    # plt (ggplot plot): notched boxplot.
  
  if (filter_vals) {
    data = data[data[x] != 0 & data[x] != 1 & !is.na(data[x]), ]
  }
  
  plt = ggplot(
    data = data,
    mapping = aes(x = func(.data[[x]]))
  ) +
    geom_boxplot(notch = T) +
    xlab(label = x_lab)
  
  return(plt)
}

###==========================================================================###

get_cors = function (data, x_lst, feats) {
  # Creates a dataframe of correlations between variables.
  
  # Parameters:
    # data (data.frame): dataframe with variables to correlate.
    # x_lst (c(char)): vector of names of variables to make columns.
      # Variables must be numeric.
    # feats (c(char)): vector of names fo variables to make rows.
      # Variables must be numeric.
  
  # Return:
    # df (data.frame): dataframe of correlations between variables named in
      # x_lst and feats.
  
  df = data.frame(
    row.names = feats[!(feats %in% x_lst)]
  )
  for (x in x_lst) {
    df[x] = NA
    for (feat in row.names(df)) {
      cor_x = data[!is.na(data[feat]), x]
      cor_y = data[!is.na(data[feat]), feat]
      if (length(cor_x) > 0 & length(cor_y) > 0) {
        df[feat, x] = cor(
          x = cor_x,
          y = cor_y
        )
      }
    }
  }
  
  return(df)
}

###==========================================================================###

sum_and_trans_cont = function (
  data, x, func, func_name, t_binw = 30, x_binw = 30, filter_vals = F
) {
  # Generates a 2X2 plot of histogram and boxplot of variable next to those of
  # the variable transformed.
  
  # Parameters:
    # data (data.frame): dataframe with variable to plot.
    # x (char): name of variable to plot.
    # func (function): transformation function.
    # func_name (char): name of function to use for labeling.
    # t_binw (numeric): binwidth of transformed histogram. (default: 30)
    # x_binw (numeric): binwidth of raw histogram. (default: 30)
    # filter_vals (bool): whether to filter out values hard coded in functions
      # called to generate transformed plots (currently 0, 1, NA). (default: F)
  
  # Return:
    # void
  
  # Output:
    # (grid.arrange(ggplot, ggplot, ggplot, ggplot)): 2X2 plot of four
      # ggplots, two histograms and two boxplots.
  
  
  gg = ggplot(data = select(data, all_of(x)), mapping = aes(x = .data[[x]]))
  p1 = gg + geom_histogram(binwidth = x_binw)
  p2 = gg + geom_boxplot(notch = T)
  
  p3 = transformed_hist(
    data = select(data, all_of(x)),
    x = x,
    func = func,
    binwidth = t_binw,
    x_lab = paste(func_name, "(", x, ")"),
    filter_vals = filter_vals
  )
  
  p4 = transformed_boxplot(
    data = select(data, all_of(x)),
    x = x,
    func = func,
    x_lab = paste(func_name, "(", x, ")"),
    filter_vals = filter_vals
  )
  
  grid.arrange(p1, p2, p3, p4)
  
  return()
}

###==========================================================================###

sum_and_trans_fact = function (data, x, y) {
  # Generates side-by-side plots of horizontal bar graph with counts and
  # horizontal fenced jitterbox violin of a factor against a numeric variable.
  
  # Parameters:
    # data (data.frame): dataframe with factor and numeric variables to plot.
    # x (char): name of factor.
    # y (char): name of numeric.
  
  # Return:
    # void
  
  # Output:
    # (grid.arrange(ggplot, ggplot)): side-by-side plots of horizontal bar
      # graph with counts and horizontal fenced jitterbox violin of a factor
      # against a numeric variable.
  
###  
### TO DO: optionally convert integer to factor. ###
###
  
  p1 = basic_bar(data = data, x = x) +
    coord_flip()
  p2 = fenced_jbv(data = data, x = x, y = y, jit_h = 0) +
    coord_flip()
  
  grid.arrange(p1, p2, ncol = 2)
  
  return()
}

###==========================================================================###

basic_bar = function (data, x, hjust = 'right') {
  # Generates a horizontal bar graph of a factor with counts.
  
  # Parameters:
    # data (data.frame): dataframe with variable to plot.
    # x (char): name of variable to plot.
    # hjust (char): parameter to pass geom_text. Sets where to print the count.
  
  # Return:
    # plt (ggplot): a horizontal bar graph of a factor with counts.
  
  plt = ggplot(data = data, mapping = aes(x = .data[[x]])) +
    geom_bar() +
    geom_text(
      stat = 'count',
      mapping = aes(label = ..count..),
      hjust = hjust
    )
  
  return(plt)
}

###==========================================================================###

fenced_jbv = function (
  data, x, y, pt_col = 'red', jit_col = NULL, box_color = 'blue',
  jit_alpha = 0.2, leg_lbl = NULL, jit_w = 0.4, jit_h = 0.4
) {
  # Generates overlaid plots of numeric variable against another (best as
  # factor). Violin over notched boxplot, over jittered scatter plot, with lines
  # through each factor level's mean and outer percentiles (the fence). The
  # boxplot width encodes proportion of set represented in each factor level.
  # Can optionally encode a variable in the point colors.
  
  # Parameters:
    # data (data.frame): dataframe with variables to plot.
    # x (char): name of x variable. Best if it's a factor, but can be numeric.
    # y (char): name of y variable. Must be numeric.
    # pt_col (char): name of color to set all points to uniform color.
    # jit_col (char): name of variable to encode point color by.
    # box_color (char): name of color to set box lines.
    # jit_alpha (numeric): to set point transparency.
    # leg_lbl (char): label to give legend for points.
    # jit_w (numeric): degree of jitter along x axis.
    # jit_h (numeric): degree of jitter along y axis.
  
  # Return:
    # plt (ggplot): Violin over notched boxplot, over jittered scatter plot,
      # with lines through each factor level's mean and outer percentiles (the
      # fence). The boxplot width encodes proportion of set represented in each
      # factor level. Can optionally encode a variable in the point colors.
  
  # plt = NULL
  
  gg = ggplot(data = data, mapping = aes(x = .data[[x]], y = .data[[y]]))
  jit = NULL
  if (!is.null(jit_col)) {
    # gg = ggplot(data = data, mapping = aes(x = .data[[x]], y = .data[[y]]))
    jit = geom_jitter(
      alpha = jit_alpha,
      aes(color = .data[[jit_col]]),
      position = position_jitter(w = jit_w, h = jit_h)
    )
  } else {
    # gg = ggplot(data = data, mapping = aes(x = .data[[x]], y = .data[[y]]))
    jit = geom_jitter(
      alpha = jit_alpha,
      aes(color = pt_col),
      position = position_jitter(w = jit_w, h = jit_h)
      )
  }
  
  plt = gg + jit +
    geom_boxplot(
      notch = T,
      notchwidth = .1,
      varwidth = T,
      alpha = 0,
      color = box_color
    ) +
    geom_violin(alpha = 0) +
    geom_line(
      stat = 'summary',
      fun = quantile,
      fun.args = list(probs = .9),
      linetype = 2, aes(group = 1)
    ) +
    geom_line(stat = 'summary', fun = mean, mapping = aes(group = 1)) +
    geom_line(
      stat = 'summary',
      fun = quantile,
      fun.args = list(probs = .1),
      linetype = 2, aes(group = 1)
    ) +
    xlab(label = x) + ylab(label = y)
  
  if (!is.null(leg_lbl)) {
    plt = plt + labs(color = leg_lbl)
  } else { plt = plt + theme(legend.position = 'none')}
  
  return(plt)
}

###==========================================================================###

summarize_by = function(data, x, y) {
  # Summarize a variable grouped by another in a reader-friendly table. Summary
  # includes count, min, Q1, median, mean, Q3, and max.
  
  # Parameters:
    # data (data.frame): dataframe that contains the x and y variables.
    # x (char): name of variable to group by.
    # y (char): name of variable to summarize.
  
  # Return:
    # tbl (data.frame): summary table.
  
  min_y = paste('min_', y)
  Q1_y = paste('Q1_', y)
  med_y = paste('med_', y)
  mean_y = paste('mean_', y)
  Q3_y = paste('Q3_', y)
  max_y = paste('max_', y)
  
  tbl = data %>%
    group_by(.data[[x]]) %>%
    summarize(
      n = n(),
      min_y = min(.data[[y]]),
      Q1_y = quantile(.data[[y]], .25),
      med_y = median(.data[[y]]),
      mean_y = mean(.data[[y]]),
      Q3_y = quantile(.data[[y]], .75),
      max_y = max(.data[[y]])
    )
  
  return(tbl)
}

###==========================================================================###

get_level_pvals = function (data, x, z, min_n = 30) {
  # Creates dataframe of t-test p-values for comparisons of a continuous
  # variable (z) between a factor's (x) levels.
    # For instance:
    # H0: z values of level A are not from a different population than z values
      # of level B.
  
  # Parameters:
    # data (data.frame): dataframe containing the variables to use.
    # x (char): name of factor with levels to group by and compare z values of.
    # z (char): name of continuous variable to group by x levels and compare.
    # min_n (integer): minimum observation count for a level to be tested.
  
  # Return:
    # df (data.frame): grid of p-values between factor levels. (rows = cols)
  
  df = data.frame(row.names = unique(data[[x]]))
  
  for (x_level in row.names(df)) {
    for (y_level in row.names(df)) {
      df[x_level, y_level] = NA
      if (
        nrow(data[data[x] == x_level, ]) >= min_n &
        nrow(data[data[x] == y_level, ]) >= min_n
      ) {
        results = t.test(
          x = data[data[x] == x_level, z],
          y = data[data[x] == y_level, z]
        )
        df[x_level, y_level] = results$p.value
      }
    }
  }
  
  return(df)
}

###==========================================================================###

get_signif_levels = function (data, x, z, min_n = 30, alpha = 0.1) {
  # Get grid of p-values comparing z grouped by x, and list levels with
  # significant p-values.
  
  # Parameters:
    # data (data.frame): dataframe containing x and z variables to use.
    # x (char): name of factor to group z by and t-test between levels.
    # z (char): name of continuous variable to group by x.
    # min_n (integer): minimum observation count for a level to be tested.
    # alpha (numeric): test criteria, risk threshold to compare p-values to.
  
  # Return:
    # results (list(data.frame, data.frame)):
      # First dataframe in list is full grid of p-values between levels.
      # Second dataframe is dictionary of levels with boolean indicating t-test
        # results.
  
  results = NULL
  
  df = get_level_pvals(
    data = data[!is.na(data[x]), ],
    x = x,
    z = z,
    min_n = min_n
  )
  
  signif_levels = as.data.frame(apply(df, 1, function (df) { any(df < alpha) }))
  signif_levels = filter(signif_levels, signif_levels == T)
  signif_levels = row.names(signif_levels)
  
  results = list(
    'pval_df' = df,
    'signif_levels' = signif_levels
  )
  
  return(results)
}

###==========================================================================###

plot_scat_pairs = function (df, x, y_lst) {
  # Generates side-by-side of transposed scatter plots between x and each y in
  # y_lst. Overlays linear regression and smoothed regression on each, with
  # confidence ranges.
  
  # Parameters:
    # df (dataframe): dataframe that contains the variables to plot.
    # x (char): name of variable to plot.
    # y (c(char)): list of variables to plot against x.
  
  # Return:
    # void
  
  #Output:
    # (grid.arrange(ggplot, ggplot)): side-by-side of transposed scatter
      # plots between x and each y in y_lst.  Overlays linear regression and
      # smoothed regression on each, with confidence ranges. First plot plots x
      # on x axis; second plots x on y axis.

  
  for (y in y_lst) {
    if (x != y) {
      p1 = ggplot(data = df, aes(x = .data[[x]], y = .data[[y]])) +
        geom_jitter() +
        geom_smooth() +
        geom_smooth(method = 'lm', color = 'yellow') +
        labs(x = x, y = y)
      p2 = ggplot(data = df, aes(y = .data[[x]], x = .data[[y]])) +
        geom_jitter() +
        geom_smooth() +
        geom_smooth(method = 'lm', color = 'yellow') +
        labs(y = x, x = y)
      plt = grid.arrange(p1, p2, ncol = 2)
    }
  }
  
  return()
}
