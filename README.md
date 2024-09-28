# Assessing Interference with Regression Analysis Techniques

[Jonathan Swindell*](https://github.com/JonathanSwindell/JonathanSwindell), [Carson Slater](https://github.com/carsonslater), Samuel Hussey, Charles Baylis, Robert J. Marks II

### Overview

[Paper](https://doi.org/10.1109/WMCS62019.2024.10619025)

This work proposes using regression analysis to predict aggregate interference in next-generation Dynamic Spectrum Access (DSA) systems, which must operate in real-time. The generalized additive model performs comparably to other machine learning methods but is much faster to train, making it suitable for dynamic spectrum environments. 

***
### Motivation



***
### Methodology

Previously, other methods employed to model aggregate interference in Dynamic Spectrum Access (DSA) systems featured machine learning models, of which are not the most parsimoious. This approach simply considers employing a generalized linear model to predict this interference. Machine learning generally exploits correlations within data to develop accurate predictions, but can be hard to interpret. This is why many ML models are referred to as `black box models.’ Regression models, although rudimentary, could prove useful as they are interpretable and relatively computationally inexpensive.

