# Cluster Analysis Readme

## Introduction
Welcome to the Cluster example using kmeans! In this guide, you will learn how to implement clusters to segment different groups based on shared features and measure the robustness of the solution. Rather than including screenshots of plain code and describing functions, we will walk through the overall process, how to interpret results, and test the solution.

## Requirements
Before proceeding, ensure you have the following libraries installed in your R environment:
- `readxl`
- `psych`
- `ggplot2`
- `gmodels`
- `vcd`
- `varhandle`
- `factoextra`
- `dplyr`
- `fpc`

You can install these libraries by running the following code:
```R
packs <- c('readxl', 'psych', 'ggplot2', 'gmodels', 'vcd', 'varhandle', 'factoextra', 'dplyr', 'fpc')
if (!require(packs)) 
  install.packages(packs)
```

## Data
The data used in this project is stored in `lite_dataS.xlsx`.

## Getting Started
To begin, follow these steps:
1. Clone this repository (optional).
2. Install the required libraries as mentioned above.
3. Load the dataset `lite_dataS.xlsx` into your R environment.
4. Run the code. In this site you will find a more detailed discussion: https://medium.com/@dannfe97/implementing-kmeans-in-a-diverse-dataset-about-credit-cards-636363f9508b

## Structure
- `cluster_analysis.R`: Main script file containing the implementation of cluster analysis.
- `lite_dataS.xlsx`: Dataset used for clustering.

## Contribution
If you find any issues or have suggestions for improvement, feel free to open an issue or create a pull request on GitHub.

## License
This project is licensed under the [MIT License](LICENSE).

Happy clustering! 📊🔍
