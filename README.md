# College Education Analysis Dashboard (with AI Chatbot)

> **This is a fork of the [original College-Data-Shiny-App-Dashboard](https://github.com/jsmarkertjs/College-Data-Shiny-App-Dashboard)** — enhanced with an AI-powered chat assistant built with [ellmer](https://ellmer.tidyverse.org/) and [shinychat](https://posit-dev.github.io/shinychat/). See original for contributions. Found out today you cant fork your own repo....

## AI Chatbot — Quick Start

1.  **Set your Gemini API key** in `.Renviron`:

    ```         
    GEMINI_API_KEY=your_key_here
    ```

2.  **Install dependencies** (original packages + chatbot packages):

    ``` r
    install.packages(c("shiny", "readr", "dplyr", "leaflet", "DT", "tidyverse",
                       "gridExtra", "bslib", "thematic",
                       "ellmer", "shinychat", "duckdb", "DBI"))
    ```

3.  **Run the app:** Open `app.R` in RStudio and click **Run App**.

4.  **Use the chatbot:** Navigate to the **Chat Assistant** tab, type a question, and press Enter. The chatbot can:

    - Answer questions about the college dataset (writes SQL queries against a DuckDB database)
    - Search the web for external information (weather, events, general knowledge) with source citations

Refer to the [original README](#original-readme) below for the full dashboard overview and features.

------------------------------------------------------------------------

# Original README {#original-readme}

# College Education Analysis Dashboard

## Overview

The College Education Analysis Dashboard is an interactive Shiny application designed to address the complexity of higher education decision-making by serving two distinct user groups.

For students and families, the app serves as a prescriptive tool to determine "Best Value." It helps users navigate the trade-offs between tuition, debt, and future earnings through a custom Best Value Index (BVI), allowing for easier comparison of schools based on return on investment.

For institutional researchers and administrators, the app functions as an analytical platform. It provides tools to model how various institutional levers—such as faculty salaries, admission rates, and endowments—statistically correlate with student outcomes like graduation rates.

Ultimately, this project bridges the gap between simple college lookup tools and complex statistical analysis, empowering users to make data-informed decisions.

## Data

The dataset consolidates the U.S. Department of Education's College Scorecard (2022-23) and IPEDS data. It contains 1,865 observations filtered for currently operating, 4-year, public or private non-profit institutions with over 100 undergraduates.

## Quick Start

1.  **Install Dependencies:**

`install.packages(c("shiny", "readr", "dplyr", "leaflet", "DT", "tidyverse", "gridExtra", "bslib", "thematic"))`

1.  **Run App:** Open `app.R` in RStudio and click "Run App".

## Features

- **Student Tab:** Filter schools by demographics and cost to calculate a Best Value Index (BVI) (Earnings ÷ (Net Cost + Debt)) and view results on an interactive map.

- **Researcher Tab:** Perform single and multi-variable exploration (T-tests, ANOVA) and build custom regression models to predict graduation rates.

- **Data Table:** View, sort, and download the full cleaned dataset as a CSV.

## Contributors

- **Jon Cote (DATA-613):** [cotejon-033](https://github.com/cotejon-033)

- **John Dye (DATA-413):** [ellis-di](https://github.com/ellis-di)

- **Camden Egan (DATA-413):** [ce8304a](https://github.com/ce8304a)

- **Jack Markert (DATA-413):** [jsmarkertjs](https://github.com/jsmarkertjs)

- **Shae Ramberg (DATA-413):** [shaeramberg](https://github.com/shaeramberg)

## License

College Education Analysis © 2025 by Jon Cote, John Dye, Camden Egan, Jack Markert, Shae Ramberg is licensed under [CC BY-NC 4.0](http://creativecommons.org/licenses/by-nc/4.0/).
