# Perspective-taking to Reduce Affective Polarization on Social Media
[Martin Saveski*](http://martinsaveski.com),
[Nabeel Gillani*](https://www.nabeelgillani.com/), 
[Ann Yuan](https://www.media.mit.edu/people/annyuan/overview/), 
[Prashanth Vijayaraghavan](https://www.mit.edu/~pralav/), and 
[Deb Roy](https://www.media.mit.edu/people/dkroy/overview/).

This repository contains supplementary material and code for replicating the analysis in 
the paper "Perspective-taking to Reduce Affective Polarization on Social Media" 
published in the proceedings of International AAAI Conference on Web and Social Media (ICWSM '22).

## Supplementary material
The supplementary materials can be found in [`supplementary-material.pdf`](https://github.com/msaveski/twitter_perspective_taking/blob/main/supplementary-material.pdf).

It contains the following:
- A description and justification of the mixed-effects linear models used for the main analyses,
- Checks for the robustness of results across different model specifications,
- Checks for covariate balance to ensure randomized treatment assignment worked as intended,
- An analysis of results accounting for selective attrition in post-survey completions.

## Code guide
- `models_simple.R`: Fits `lmer` and `brms` models, using the two treatments (`feed=opposite` and `prompt=empathic`) with and without an interaction term.
- `models_w_covs.R`: Similar as above but the models also include interactions with each of the treatments and the covariates (`days_active`, `num_statuses`, `num_favorites`, `num_followers`, `num_friends`).
- `plots.R`: Generates Figures 2 (main effects) and 3 (interaction effects) of the paper based on the `brms` models with covariates as fitted in `models_w_covs.R`.
- `robustness_plot.R`: Generates a robustness plot showing the coefficients of the treatments across different models specifications (shown in Appendix).
- `robustness_tables.R`: Generates tables for the different models (shown in Appendix).
- `balance_reg.R`: Runs covariate balance checks for both treatments, regressing the treatment on the covariates (detailed results shown in Appendix).
- `balance_ri.R`: Runs covariate balance checks using randomization inference (detailed results shown in Appendix).
- `attrition.R`: Tests the effects of attrition using monotonicity bounds for users who were shown the survey but did not complete some questions (detailed results shown in Appendix).
- `load_data.R`: Loads and joins the key data files. Used in most of the other analyses scripts.
- `utils.R`: Contains a function for generating tables from `brms` models.

## Data
Feel free to email us (msaveski [AT] mit.edu / ngillani [AT] mit.edu) to request an anonymized version of the data.

## Citation
```
@inproceedings{saveski2022perspective,
  title={Perspective-taking to Reduce Affective Polarization on Social Media},
  author={Saveski, Martin and Gillani, Nabeel and Yuan, Ann and Vijayaraghavan, Prashanth and Roy, Deb},
  booktitle={Proceedings of the International AAAI Conference on Web and Social Media},
  year={2022}
}
```

## License
This code is licensed under the MIT license found in the LICENSE file.
