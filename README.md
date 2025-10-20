# Beyond Correlation: SASP 2025 Workshop

This Quarto project supports the stand-alone SASP 2025 pre-conference workshop **Beyond Correlation: A Practical Introduction to Causal Inference in Observational Social Psychology**.

## Repository structure

- `index.qmd` — landing page with logistics, objectives, schedule, and setup instructions.  
- `content/` — lecture notes covering causal inference concepts and workflows.  
- `pdfs/` — handouts, case studies, and slide decks.  
- `workshop-scripts/` — R scripts installed via the `causalworkshop` package.  
- `quiz/` — workshop quiz materials and answer key.

## Getting started

1. Install R and RStudio.  
2. Install the workshop package:

   ```r
   if (!requireNamespace("devtools", quietly = TRUE)) {
     install.packages("devtools")
   }

   devtools::install_github("go-bayes/causalworkshop")
   ```

3. Run `check_workshop_prerequisites()` and `get_workshop_scripts()` from the `causalworkshop` package to pull the reproducible exercises into a local folder.  
4. Render the site locally with `quarto preview`.

Student presentation materials will be posted to this repository after the workshop.
