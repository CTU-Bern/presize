presize 0.2.0
-----------------------------------------

* various changes to ensure that all functions support vectors for different scenarios


presize 0.1.4
-----------------------------------------

* minor changes to readme and shinyapp (corrections of references/typos)

presize 0.1.3
-----------------------------------------

* more minor changes for CRAN 

presize 0.1.2
-----------------------------------------

* minor changes requested by CRAN 

presize 0.1.1
-----------------------------------------

* first version for CRAN
* minor clarifications to options/descriptions
* minor changes to shiny app

presize 0.1.0
-----------------------------------------

* initial 'final' version

presize 0.0.1.9007
-----------------------------------------

* addition of wrappers for `prec_lr` (`prec_pos_lr`, `prec_neg_lr`) to simplify positive/negative LRs


presize 0.0.1.9006
-----------------------------------------

* addition of method for likelihood ratios `prec_lr`

presize 0.0.1.9005
-----------------------------------------

* addition of function for Cohen's kappa

* update shiny app to include kappa

* POSSIBLE BREAKING CHANGE: arguments in `prec_rateratio` renamed from `*_exp` and `*_control` to `*1` and `*2` for consistency with other functions

presize 0.0.1.9004
-----------------------------------------

* addition of shiny app and pkgdown

presize 0.0.1.9003
-----------------------------------------

* `prec_sens` and `prec_spec` allow prev and conf.width

* multiple notes allowed in print method

* add contributing guidelines

* `prec_sens_spec` removed. Confidence intervals and sample sizes are quite different to other methods. A two step approach using `prec_sens` and `prec_spec` is instead recommended


presize 0.0.1.9002
-----------------------------------------

* addition of various tests

* addition of rate ratio method
