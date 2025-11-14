# Social Network Analysis of the Bondora Peer-to-Peer Lending Platform

The following project was undertaken as part of the course Social Network Analysis for Data Scientists at the Jheronimus Academy of Data Science (JADS). The objective of the Group was to utilise network statistical models to uncover patterns and structural properties of the dataset.

The project contributors are

| Name    | Surname     |
|:--------|:------------|
| Floris  | Vermeulen   |
| Martijn | van Iterson |
| Niek    | Fleerakkers |
| Patryk  | Grodek      |
| Samir   | Sabitli     |

## Project Abstract

> TBD

## Research Problem

### Study 1

The first study aims to uncover how similarities across borrowers' loan uses are explained by their personal and loan characteristics. To understand this, Linear Quadratic Assignment Problem (QAP) regression models were utilised for their ability to explain networks as a linear function of other related networks. In the following table we describe the construction and use of each variable within the Linear QAP framework.

Under `Variable Name`, we list the original attribute used from the raw dataset, however, its transformation into the appropriate data structure is described under the `Construction` column

| Variable Name | Variable Type | Construction |
|--------------|--------------|--------------------------------------------|
| `UseOfLoan` | Dependent | $n\times n$ adjacency matrix of $n$ borrowers given a common reported `UseofLoan`. Obtained by transforming the $n\times m$ incidence matrix of $m$ loan types by $\mathbf{X} \cdot \mathbf{X}^T$ weighted by unique loan count. |
| `Rating` | Main Predictor | $n \times n$ adjacency matrix of $n$ borrowers given a commonly reported credit rating across their portfolio of loans. Obtained by transforming the $n \times m$ incidence matrix of $m$ credit ratings by $\mathbf{X\cdot X^T}$, weighted by unique loan count. |
| `OccupationArea` | Main Predictor | Binary $n\times n$ adjacency matrix of $n$ borrowers given a common occupation area reported across their portfolio of loans. Obtained by transforming the $n\times m$ incidence matrix of occupation areas by $\mathbf{X\cdot X^T}$, replacing weighted values by ones. |
| `Gender` | Control Variable | Binary $n\times n$ adjacency matrix of $n$ borrowers given a common gender reported across all loans. Obtained by transforming the $n\times m$ incidence matrix of genders by $\mathbf{X\cdot X^T}$, replacing weighted values by ones. |
| `Age` | Control Variable | $n\times n$ adjacency matrix of differences across borrowers' ages constructed by transforming the $n \times m$ incidence matrix of $m$ ages by $\mathbf{D}=\left[ |x_i-x_j| \right]^n_{i,j=1}$. |
| `LoanDuration` | Control Variable | $n\times m$ adjacency matrix of differences across borrowers' average loan duration by transforming the $n\times m$ matrix of $m$ average loan durations by $\mathbf{D}=\left[ |x_i-x_j| \right]^n_{i,j=1}$. |
| `Amount` | Control Variable | $n\times m$ adjacency matrix of differences across average loan amounts per borrower by transforming the $n\times m$ matrix of $m$ average loan amounts by $\mathbf{D}=\left[ |x_i-x_j| \right]^n_{i,j=1}$. |

## Methodology

### Data Pre-processing

-   Attributes were condensed to:

```         
c("LoanId", "UserName","Age", "Gender", 
               "Country", "Amount", "Interest","LoanDuration",
              "UseOfLoan", "Rating", "Restructured", "MonthlyPayment",
              "OccupationArea", "BiddingStartedOn")
```

-   Any rows with `NA` values across the above attributes were removed to preserve data completeness
-   Data was filtered to `BiddingStartedOn` date between 2014 and 2016 because other date ranges did not have complete information on `UseofLoan`
-   To ensure that the resulting network is computationally manageable, we randomly sampled 500 individuals to be used as the training set

### Quadratic Analytic Procedure

> ***Hypothesis 1:** Borrowers with the same credit ratings choose similar loan uses*

> ***Hypothesis 2:** Borrowers sharing the same occupation area tend to share loan uses*

### Exponential Random Graph Model

> ***Hypothesis 3:** Borrowers who share one loan purpose are likely to share another*
>
> -   ERGM term: `cycle(4)`
>
> -   Endogenous, dyad-dependent Markovian term

> ***Hypothesis 4:** Borrowers exhibit preferences for minimal debt distinctiveness, selecting one loan use*
>
> -   ERGM term: `b1degree(1)`
>
> -   Endogenous, dyad-dependent Markovian term

> ***Hypothesis 5:** Younger borrowers have different loan use mixes than older borrowers*
>
> -   ERGM term: `b1cov("age")`
>
> -   Exogenous, dyad-independent vertex-covariate term

> ***Hypothesis 6:** Gender differences lead to different loan use mixes*
>
> -   ERGM term: `b1nodematch("gender")`
>
> -   Exogenous, dyad-independent homophily term

## Dataset

The dataset contains over 100,000 loan grants between February 2009 and July 2021 across several countries in Europe. Across the users, 112 different attributes are available. These mainly describe the demographic, technical, financial, and geographic characteristics of the users. The data was formerly published on Bondora's website, however, now it can be found on [kaggle](https://www.kaggle.com/datasets/sid321axn/bondora-peer-to-peer-lending-loan-data?resource=download).

## Project Structure

``` bash
    ├── _output
    │   └── SNA_Group7.pdf
    ├── dataset
    │   └── LoanData_Bondora.csv
    ├── resources
    │   ├── images
    │   │   └── draft_1/
    │   ├── objects
    │   │   ├── bondora_sample.RDS
    │   │   └── p2p_network.RDS
    │   ├── apa-6th-edition.csl
    │   └── r-references.bib
    ├── scripts
    │   ├── data_processing_bondora.R
    │   └── network_analysis.R
    ├── tex_tables/
    ├── _quarto.yml
    ├── .gitignore
    ├── README.md
    ├── SNA_Group7.qmd
    └── SNA_Group7.Rproj
```

Due to GitHub's size constraints, the dataset `LoanData_Bondora.csv` is unavailable in this repository. The project has been constructed as such:

-   The main report was written and compiled by Quarto.
-   The .pdf of the main report can be found under `_output/`
-   The R scripts utilised to process the dataset and conduct network analyses can be found under `scripts/`
    -   `bondora_preprocessing.R` outlines the steps needed to transform the raw dataset into a usable network
    -   `qap_network_analysis.R` outlines the analysis using the QAP Linear Regression
    -   `ergm_network_analysis.R` outlines the analysis using ERGM models

## R Package Requirements

-   The `snafun` package that can be remotely downloaded from the [SNAfun GitHub Repo](https://github.com/SNAnalyst/SNAfun)
-   `here` package to locate relevant directories using the local `.Rpoj` file
-   `igraph`, `sna`, and `network` to undertake network modelling
-   `viridis` for the colour palette
-   `Rglpk` as an alternative ERGM convergence algorithm
