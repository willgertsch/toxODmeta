# toxODmeta
This is an R package for finding optimal designs for toxicology models using metaheuristics. Designs can be found either by using the toxODmeta() function or by using the included Shiny app. 

## How to add new models.
This software is designed to make it easy to add new models. To add a new model, I currently follow this checklist (will hopefully streamline in future):

1. add new information matrix in info_matrices.R.
2. add new sensitivity functions for all objectives in sens_funs.R.
3. add model and objectives to if-else selector in plot_sens.R.
4. add model information matrix function to selector in toxODmeta function.
5. update global list of available models in Shiny app.
6. add new parameters to interface if necessary and expand input reading for theta on server-side.
7. make sure server passes correct number of parameters to main function.
8. update documentation for model, making sure the parameterization matches what is in the code.
