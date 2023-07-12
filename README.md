# Statistical-learning
A supervised and unsupervised analysis for the Statistical learning exam. A total of 143 countries are analysed, each characterised
by 11 socio-economic and demographic variables. The analysis is divided into two parts. The first part conducts
a supervised analysis with the fertility rate as the dependent variable. The first proposed method is a multiple
linear regression and its diagnostics. The model is tested using k-fold cross-validation. Since there seems to be
multicollinearity in the diagnostics, partial least square regression is proposed as an alternative solution. This
is followed by regression using decision trees, both simple and random forest. Among all the supervised models,
multiple linear regression shows better performance in terms of accuracy. The second part of the analysis focuses on
unsupervised algorithms, particularly clustering. First, the k-means algorithm is performed solely on the numerical
variables. Then, the grower distance is used to cluster the variables including the categorical variable of religion.
In performing the clustering, the fertility rate is not used, not because it is the outcome, but rather to see if
the naturally created clusters reflect different fertility rates. In the first method, 3 clusters are chosen, with one
grouping developed countries and the other two comprising developing countries. In the second method, 2 clusters
emerge, with one predominantly consisting of Islamic countries and the other predominantly consisting of Christian
countries, including the more developed ones.
