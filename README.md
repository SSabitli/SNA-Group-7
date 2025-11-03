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

The first study aims to understand the effect of borrowers' credit ratings on their stated loan use purpose. To understand this, Linear Quadratic Assignment Problem (QAP) regression models were utilised for their ability to explain networks as a linear function of other related networks. In the following table we describe the construction and use of each variable within the Linear QAP framework.

Under `Variable Name`, we list the original attribute used from the raw dataset, however, its transformation into the appropriate data structure is described under the `Construction` column

| Variable Name | Variable Type | Construction | Purpose |
|----------------|-------------------|-------------------------------|-----------------------------------------|
| `UseOfLoan` | Dependent | $n\times n$ adjacency matrix of $n$ borrowers given a common reported `UseofLoan`. Obtained by transforming the $n\times m$  incidence matrix of $m$ loan types by $\mathbf{X} \cdot \mathbf{X}^T$ weighted by unique loan count. |  |
| `Rating` | Main Predictor |  |  |
| `Gender` | Control Variable |  |  |
| `Age` | Control Variable |  |  |
| `LoanDuration` | Control Variable |  |  |
| `Amount` | Control Variable |  |  |

## Methodology

### Data Pre-processing

-   s

### Exponential Random Graph Model

-   Quantify and test hypotheses related to the structures that exist around pairs of similar borrowers
-   Quantify and test hypotheses related to how borrowers' exogenous attributes affect the formation of similarity between them

### Quadratic Analytic Procedure

-   Used to test whether certain network attributes are random

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
-   The .pdf of the report can be found under `_output/`
-   The R scripts utilised to process the dataset and conduct network analyses can be found under `scripts/`

## R Package Requirements

-   The `snafun` package that can be remotely downloaded from the [SNAfun GitHub Repo](https://github.com/SNAnalyst/SNAfun)
-   `here` package to locate relevant directories within the scripts
-   `igraph`, `sna`, and `network`
