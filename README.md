# Landmark Reliability

**Landmark Reliability** is an advanced web application designed for medical image landmarking, calibration, imputation, and graphical analysis. Developed using R and Shiny, this project facilitates precise point marking on medical images, accurate measurement of distances, and comprehensive data analysis.

## Tools & Technologies

Landmark Reliability utilizes the following tools and technologies:

- **R**: The core programming language for statistical analysis and application development.
- **Shiny**: A framework for building interactive web applications in R.
- **JavaScript**: For enhancing interactivity and functionality within the web application.
- **HTML/CSS**: For structuring and styling the web application's user interface.
- **ggplot2**: A package for creating static, high-quality graphics.
- **shinyWidgets**: Provides additional UI components and functionality for Shiny applications.
- **plotly**: Enables interactive graphical representations and plots.
- **dplyr**: Facilitates data manipulation and transformation.
- **mice**: Implements methods for handling missing data through imputation.

## Features

The application offers the following features:

- **Interactive Landmarking**: Users can precisely mark multiple points on medical images.
- **Calibration**: Includes a reference scale embedded in the images to convert pixel distances into real-world units, enhancing measurement accuracy.
- **Data Management**: Robust system for storing, managing, and exporting marked points and measurements, ensuring data integrity and user flexibility.
- **Imputation**: Advanced techniques to predict and fill in missing landmarks by analyzing and comparing user markings.
- **Graphical Outputs**: Comprehensive visualizations of marked points, measured distances, and other analytical results to support detailed data interpretation and reporting.

## Installation and Setup

To set up the project on a UNIX-based system, follow these steps:

### 1. Clone the Repository

Clone the project repository to your local machine using the following command:

```bash
git clone https://github.com/MustafaCanInce/shiny
```
## Run the Application

Rscript runShinyApp.R

The runShinyApp.R script performs the following tasks:

- Checks and installs any missing R packages required for the application.
- Sets the working directory to the directory containing runShinyApp.R.
- Sources the ui.R and server.R files.
- Launches the Shiny application.

## Usage
Once the application is running, you can access it through your web browser. Use the interface to upload images, mark landmarks, calibrate measurements, and view graphical outputs.

## Contributing
Contributions are welcome. To contribute:

Fork the repository.
Create a new branch for your feature or bug fix.
Commit your changes and push them to your branch.
Open a pull request with a description of your changes.
For any issues or inquiries, please use the GitHub Issues page.
