# Kaggle_Housing_Price_Regression

Wrangling, EDA, feature engineering, ML and linear regression, and hypothesis testing in R notebooks.

<a id = "top"></a>

# Overview

Thus far in this project, I wrangle, explore, and engineer data in preparation for statistical and machine learning (ML) regression of county assessor data to predict residential sales prices. To complete the project, I will use a few regression models to test whether I added value with my treatment of the data. If the selected ML algorithms build models that perform significantly and meaningfully better with my engineered data than with selected "raw" data, then I can say that I have added value to the data by making it more amenable to ML.

The exploratory data analysis (EDA) and feature engineering process that I have completed thus far are outlined and explained in a fair amount of detail in this overview section. I stopped short of feature selection and preprocessing for ML as I will do that next in a new notebook to be added to this project upon completion. The narrative, code, stats, and visualizations in the <a href="https://github.com/KalebCoberly/Kaggle_Housing_Price_Regression/wrangle_and_split.html" target="_blank">wrangle and split R notebook</a>, the EDA R notebooks <a href="https://github.com/KalebCoberly/Kaggle_Housing_Price_Regression/EDA_pt1.html" target="_blank">part 1</a> and <a href="https://github.com/KalebCoberly/Kaggle_Housing_Price_Regression/EDA_pt2.html" target="_blank">part 2</a>, and associated <a href="https://github.com/KalebCoberly/Kaggle_Housing_Price_Regression/tree/main/tools" target="_blank">scripts</a> should be viewed as content that can be referred to or used for other polished documents and production processes, not as polished products themselves. You can also view the <a href="https://github.com/KalebCoberly/Kaggle_Housing_Price_Regression/blob/main/EDA_full.Rmd" target="_blank">full EDA .Rmd document here</a>.

## Modeling and Hypothesis Test

[Back to top.](#top)

I will build the models and conduct hypothesis tests in a notebook to be added upon completion. I will compare the prediction errors of the models trained on the "raw" control data to the prediction errors of the models trained on the experimental engineered data.

*Null hypothesis:* Feature engineering did not lower mean errors.

*Alternative hypothesis:* Feature engineering lowered mean errors.

I will pool the errors within each group rather than grouping by ML algorithm and running a separate hypothesis test for each algorithm. This will avoid "p-hacking" with multiple tests.

If the errors warrant it (e.g., they are normally distributed or can be made to be normally distributed), I will use a simple one-tailed <a href="https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/t.test" target="_blank">Student's t-test</a>. If not, I will use a <a href="https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/wilcox.test" target="_blank">Wilcoxon rank-sum test</a> (if warranted), which is a stochastic hypothesis test that is valid for data that is not normally distributed. Pooling the errors of multiple models will likely make the distribution polymodal, so the rank-sum test will probably be necessary.

### Scaling Errors for Training vs. Comparison

I could apply a log10 transformation to sales prices in order to scale errors proportionally to the sales prices, which would avoid overweighting errors on pricier houses. This would have the added benefit of making the target variable more normally distributed and thus better fit to linear regression and ML algorithms that perform better with normally distributed variables.

However, I found that a natural logarithm better normalized the target variable in the training set, according to the results of a <a href="https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/shapiro.test" target ="_blank">Shapiro-Wilk normality test</a>. So, I will need to divide the errors by the values (error/log(SalePrice)), making errors a percentage. Rather than go with the sensible log10 scale anyway, I will use this opportunity to get "under the hood" of the ML algorithms a little bit and write my own summary function to adjust the errors.

I will use the Root Mean Squared Error (RMSE) of these adjusted errors in model training and evaluation with both the control data and the experimental data. Taking the squared errors penalizes greater errors (though not pricier errors, since the errors are measured as a percentage). Taking the square root of the mean re-standardizes the distance between model mean errors for a "flatter" comparison of models in training.

To compare the experimental errors to the control errors for the hypothesis test, I will reverse the log transformation of the experimental predictions. I will then divide the errors by the true target value for both groups.

Though I will pool errors to conduct a single test of my overall hypothesis, I may also use a few hypothesis tests to explore the results. In that case, I will only conduct a hypothesis test between like ML models. For instance, I will test how the results of a Random Forest (RF) model trained on the control data compare to the results of a RF model trained on the experimental data. But, I will not test an RF model against a K-Nearest-Neighbors (KNN) model. I will, of course, generally compare models created with different algorithms, just not as a hypothesis test.

### Model and Feature Selection

In preparation for this project, I interviewed a home appraiser. I wanted to find out which features appraisers typically consider and get a sense of their analytical methods. This will help me construct a control data set and select test ML algorithms.

Based on that discussion, I made a list of features to use to train the control models. Other than basic auditing/wrangling and scaling, I will use these features as is. The results of the exploration and feature engineering in this document will feed into selection of the experimental data set.

The point of this exercise is to measure how well I prepare data for ML and statistical use, not to attempt to recreate what a human appraiser does. That said, I identified three ML algorithms that sort of emulate what an appraiser does.

I selected RF because a decision tree groups like observations as an appraiser might group like houses to generate comparisons ("comps"). Also, a decision tree doesn't rely on standardized data, so it puts the control data and the engineered data on more level ground.

KNN also clusters observations into data "neighborhoods" in a similar way that appraisers run comps. While I may min-max scale the control data for KNN to give it sensible distance measures, I do not see the lack of normalization to necessarily be a disadvantage to the control data in KNN. Outliers may aid intelligent clustering in a multivariate problems. For this reason, I may choose to use some less-transformed features in the experimental set for KNN in an additional informal test to explore this idea.

Lastly, Lasso regression intelligently avoids overfit by penalizing each additional feature. This more closely emulates what a human appraiser does by adding and subtracting value with each feature. The engineered data will be at a distinct advantage, however, as I have engineered features with an eye to linear regression (mainly normalizing/Winsorizing continuous variables and identifying insignificant factor levels).

I both transformed and Winsorized the scale of the target variable, SalePrice. I will apply the transformation to the target variable in the test set, but not Winsorization as it is not a process you can reverse in a vectorized fashion. I will train the Lasso regression with the Winsorized target to achieve an optimal fit, but test without Winsorizing the target.

I will use <a href="https://cran.r-project.org/web/packages/caret/index.html" target="_blank">caret</a> for ML, preprocessing (min-max scaling, imputation), and feature selection. I will keep preprocessing minimal (e.g., forgo decomposition methods like principal component analysis) as I want to test the efficacy of the "manual" engineering I have done before such steps.

I may also construct a multilinear regression "by hand" with the R <a href="https://stat.ethz.ch/R-manual/R-devel/library/stats/html/00Index.html" target="_blank">stats</a> package, which will allow me to explicitly create variable interactions.

## EDA and Feature Engineering

[Back to top.](#top)

Thus far in the process, I <a href="https://github.com/KalebCoberly/Kaggle_Housing_Price_Regression/wrangle_and_split.html" target="_blank">wrangle and split in this R notebook</a>, and I explore the data and engineer features in EDA R notebooks <a href="https://github.com/KalebCoberly/Kaggle_Housing_Price_Regression/EDA_pt1.html" target="_blank">part 1</a> and <a href="https://github.com/KalebCoberly/Kaggle_Housing_Price_Regression/EDA_pt2.html" target="_blank">part 2</a>, and use <a href="https://github.com/KalebCoberly/Kaggle_Housing_Price_Regression/tree/main/tools" target="_blank">these associated scripts</a>

### Auditing and Wrangling

Though I do not document the auditing process, the wrangling script ("tools/wrangle.R") I built as a result documents the treatments I made to the data before splitting and exploring it. This treatment mainly consists of setting data types, standardizing factor levels, checking for internal consistencies, and imputing missing values that can logically be deduced. I will impute remaining missing values using caret in the ML phase.

The script should work on future data (e.g. the test set), though some issues may emerge that are idiosyncratic to the new set. I will test it out when I move on to ML. That said, I did use the wrangling script to create summary objects that aided the auditing process. As such, the script constructs and returns a lot of unnecessary data; it can certainly be trimmed and refactored.

### Train-Test Split

I split the data into training, validation, and testing sets. Even though I will use cross-validation while training the models, I set aside a validation set so that I would have ample space to explore and try out ideas before testing. I used a 70-30 split in two rounds so that the testing set is 30% of the set I have to work with, the validation set is 21% (0.3 \* 0.7), and the training set is 49% (0.7 \* 0.7).

I was especially concerned with representation across all variables. A random sample of the index did not ensure a representative sample. The more features you have, the more confident you can be that one or more of them are *not* representative with a random split. I found methods to stratify, but only by a single discrete variable at a time, which also did not produce representative splits.

To test this, I used the <a href="https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/wilcox.test" target="_blank">Wilcoxon rank-sum test</a> to compare the split subsets for each continuous feature. This as a non-parametric test and is thus suited to testing data that is not guaranteed to be normalized, as was the case before exploration and engineering. It simply finds the distance between each point in one set and each point in the other set. Then it uses a parametric test to determine if the mean distance differs significantly from 0. Because the objective is to ensure representation and the null hypothesis of this test is that the sets do not differ signficantly, we want to fail to reject the null hypothesis. The criterion, then, is a p-value greater than alpha, which ought to be set high to have higher confidence in the representativity of the split.

From this, I built a naive data set splitter (in "tools/split.R") that randomly samples observations then applies the Wilcoxon rank-sum test to each continuous feature. Should one feature fail to meet the criterion for representativity, the script resamples and tries again.

This is unabashedly aggressive "p-hacking" or "data dredging," but that is not actually a problem here. We already know the subsets come from the same superset, and we are not concerned with being confident one way or another about this hypothesis. The objective is simply to create subsets that *appear* to come from the same superset, subsets that are *representative*.

That said, this process can run for a very long time, infinitely in some cases. As with the wrangling script, I built in some extraneous processes that helped monitor the process, and it certainly can be refactored for better performance. Refactoring it will not really mitigate the underlying issue of brute force -- worse than brute force as random samples can repeat. You could modify it to return the "best" split after a given amount of time running.

I sought another method and found the maximum dissimilarity method in caret, <a href="https://www.rdocumentation.org/packages/caret/versions/6.0-88/topics/maxDissim" target="_blank">maxDissim</a>. This elegant solution iteratively selects points that are most dissimilar to the sample set, running in much less time.

It did, however, produce worse results than my method, according to a Wilcoxon rank-sum test. See <a href="https://github.com/KalebCoberly/Kaggle_Housing_Price_Regression/wrangle_and_split.html" target="_blank">results</a>. But, that is a "rigged" scenario because it applies the same test that I p-hacked to create my subset.

I instead implemented an adversarial validation using <a href="http://topepo.github.io/caret/available-models.html" target="_blank">caret's xgbTree</a>. In adversarial validation, you label each set in the split and see if the algorithm can tell the difference. If accuracy is 0.5, the model cannot tell the difference between one half of the split and the other, which is a good indication that your split is representative.

My split method produced a much more representative split than maxDissim. Splitting again for the validation set produced results close to maxDissim, though I did not split again with maxDissim to compare. The Kaggle split far outperformed my method and maxDissim. That said, Kaggle used a 50-50 split, whereas I used a 70-30 split, which may account somewhat for the difference in accuracy variance and mean. Though they have kept their method hidden as far as I can tell, I am guessing they incorporated multifactor stratification to do so well with a classification tree. See <a href="https://github.com/KalebCoberly/Kaggle_Housing_Price_Regression/wrangle_and_split.html" target="_blank">results</a>

Both maxDissim and my method only use the continuous variables. Thus, representation is undoubtedly still inaccurate in at least some factors. An alternative method may incorporate recursive stratification.

### Exploratory Data Analysis (EDA) and Feature Engineering

I mostly focused my exploration on predicting the target variable, SalePrice. I developed a standard process for handling different kinds of variables, both to engineer them and to visualize them.

I duplicated the engineering process in a well-documented engineering script ("tools/engineer.R") to apply to future data. I also developed a well-documented source file ("tools/eda.R") of the EDA tool kit that I used in this document. The engineering script could definitely be merged and refactored with the wrangling script for optimal computation. I will also need to verify that the engineering script produces the same output as the EDA notebook, which I will do in the next notebook.

While I focused mostly on basic regression prep, I did explore some interactions between predictor variables, and I did tell some "side stories."

#### Continuous Variables

For continuous variables, I sought the best normalizing scale transformation and Winsorization. In doing so, I prioritized correlations to SalePrice. I skipped multivariate Winsorization, which would have aided straightforward linear regression but may have hindered clustering and interactions between variables. I may choose to do targeted multivariate Winsorization in the next phase as I select and preprocess the variables for each different ML algorithm.

I iterated through a list of potential transformation functions and used <a href="https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/shapiro.test" target="_blank">Shapiro-Wilk normality test</a> to determine which function best normalized the variable. Again, this was a valid form of p-hacking in that I was unconcerned about the normality of ground truth and only needed the quality of normality in the variables. However, I naively excluded 0 values to avoid throwing an error logarithmic transformations, but that still transforms 1s into 0s, rendering logs useless for variables with a large number of 1s. Fortunately, there were other transformation functions. I did not implement more dynamic transformation functions like Box-Cox and Yeo-Johnson.

I also manually estimated the best Winsorization using the same normality test and a QQ plot. I hardcoded the upper and lower limit values into the engineering script, rather than using the percentiles, to avoid leakage into the test set.

I could have automated this process more, but I wanted to open it up and inspect it to convey a better sense for what I was doing and whether it was a sensible thing to do. I may also have developed a more robust measure of normality, including more metrics like kurtosis.

For variables in which a 0 indicates a missing feature, I normalized only the non-zero set. The idea was that it might aid regression when the variable is put into interaction with its missingness.

I also determined whether simply binarizing the presence of a feature would better correlate to sale price than a linear regression of the full variable.

To verify that my transformations and modifications produced sensible outcomes, I visualized at each step.

As mentioned above, I applied a natural log to SalePrice because it "best" normalized the variable, whereas a log10 transformation would have been more sensible. I will mitigate this when I write a custom summary function to train with. But, similarly, for other variables a more sensible transformation might have wiser to use than the one I used. For instance, measures of area may most sensibly be transformed to their square-root, but a different function have "best" normalized the data in the training set. I went with the function that my algorithm spit out here, but I may return to the question in the ML phase and compare sensible transformations to "optimal" transformations.

In some cases, my treatment of the data may have constituted overfitting (e.g. log(sqrt(variable))), but I often went with it as I am not concerned as much with the best outcome as with seeing what happens when conventional wisdom is ignored. I will pay attention in the next phase of modeling to whether those variables are more duds than their training set correlations implied they might be.

#### Factors

As with continuous variables, I developed a standard approach to exploring factors. It was decidedly less involved exploration and engineering.

For each factor, I summarized sale price by factor levels, both in table form and in a plot. I also automated iterative hypothesis testing for significant differences in sale prices between levels, identifying levels that differed significantly than another level. This was only for exploration purposes and should not be used to model, as it is an invalid form of p-hacking. I chose not to calculate effect size for this reason, and because I visualized it to some extent with notched boxplots.

One-hot encoding the factors and then intelligently selecting features in the ML phase will suss this out anyway. At that point, I will also determine if simply binarizing the presence of a feature better predicts price.

#### Integers and Ordered Factors

I treated integers and ordered factors as I did with both continuous variables and factors.

#### A Note on the Data Dictionary

I have only uploaded the data dictionary as received from Kaggle. I will need to update it to include entries for the new features I have engineered.

## Background

[Back to top.](#top)

I downloaded the data from <a href="https://www.kaggle.com/c/house-prices-advanced-regression-techniques" target="_blank">this Kaggle competition</a>. Kaggle acquired the set from a university professor who discussed the set and his treatment of it in <a href="http://jse.amstat.org/v19n3/decock.pdf" target="_blank">this article</a>. He cleaned up the set quite a bit. For instance, he removed the city's modeling variables, restricted records to residential sales, and removed previous sales of the same home within the given period.

## Technical Choices

[Back to top.](#top)

I wanted to do this project completely in R because I want to try out the caret package for the ML. Also, it is a forgiving enough data set that I do not need to do too much wrangling, so there is no need to use Python when R is so fit for analysis and visualization. The light amount of wrangling is a good opportunity to get more comfortable with R beyond ggplot2.

I am also choosing to do it completely on my PC in R-Studio because I like the robust features of R-Studio and because I want to get a sense of my computer's performance. Some of the compute, like ML training, may be best done on a cloud server, but that will be another project.

### A Note on the Code

Much of the code in the EDA document is repeated and could be wrapped into functions, but I chose to leave it exposed and paste it in for each variable so I could easily play with it with each use. Likewise, many of the functions in the scripts wrap a lot of now-extraneous processes and objects that were useful as I inspected the processes while I built them. These could be stripped down. The "tools/engineer.R" script is pretty streamlined, though it could be combined with the "tools/wrangle.R" script and refactored.

## References

Dean De Cock: <a href="http://jse.amstat.org/v19n3/decock.pdf" target="_blank">http://jse.amstat.org/v19n3/decock.pdf]</a>

Kaggle: <a href="https://www.kaggle.com/c/house-prices-advanced-regression-techniques" target="_blank">https://www.kaggle.com/c/house-prices-advanced-regression-techniques]</a>

Shapiro-Wilk normality test: <a href="https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/shapiro.test" target="_blank">https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/shapiro.test]</a>

Student's t-test: <a href="https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/t.test" target="_blank">https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/t.test</a>

Wilcoxon rank-sum test: <a href="https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/wilcox.test" target="_blank">https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/wilcox.test]</a>

<a href="https://github.com/KalebCoberly/Kaggle_Housing_Price_Regression/wrangle_and_split.html" target="_blank">See loaded packages.</a>
