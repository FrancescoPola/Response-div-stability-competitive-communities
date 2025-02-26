---
title: "Analysis of the experiment looking at the effect of response diversity on community stability in fluctuating environments"
author: "Til Hämmig, Francesco Polazzo"
date: "01 November, 2024"
output:
  bookdown::html_document2:
    toc: true
    toc_float:
      collapsed: true
      smooth_scroll: true
    code_folding: hide
    keep_md: yes
    fig_caption: true  
editor_options: 
  markdown: 
    wrap: 72
---






# Introduction

The purpose of this document is to provide a reproducible record of all analyses and figures in the main article. The main article is focused on the effect of response diversity on community stability in fluctuating environments. We are going to look at the effect of response diversity, richness, temperature and nutrients on community temporal stability. Specifically, we are going to look at the effect of fundamental balance (our measurement of stability) on temporal stability. Then, as response diversity is thought to stabilize temporal stability of aggregate community properties via asynchrony, we are going to look at the relationship between response diversity and asynchrony. 
Finally, as multiple evidence suggests that compensatory dynamics and temporal stability are determine by species interactions, we are going to analyse the effect of species interactions on stability to understand if they are more important than response diversity in driving temporal stability of community biomass.

This document is produced by an Rmarkdown file that includes code to reproduce from data all results presented in the main article.



# Load datasets


## Data wrangling and balance calculation



``` r
# needs to have id_new variable
ciliate_traits <- ciliate_traits %>%
  dplyr::mutate(
    # Remove dots from the date
    cleaned_date = gsub("\\.", "", date),
    # Extract the part of id after the underscore
    id_suffix = sub(".*_(.*)", "\\1", id),
    # Combine cleaned_date, id_suffix, and species_initial into a new variable
    id_new = paste0(cleaned_date, id_suffix, composition)
  ) %>%
  # Optionally, remove the intermediate columns to clean up
  dplyr::select(-cleaned_date, -id_suffix,-new_id)

uniqueN(ciliate_traits$id_new)==nrow(ciliate_traits) # all unique  ;)

id_dd<-full_join(dd_all_pred,dplyr::select(ciliate_traits,id_new,biomass),join_by("id_new"))


## add day variable

#create a day variable from the date variable

id_dd<-dplyr::mutate(id_dd,date=as.Date(date,format = "%d.%m.%y"))

earliest_date<-min(id_dd$date)
days_since_earliest<-as.numeric(id_dd$date-earliest_date)+1
id_dd<-id_dd%>%dplyr::mutate(day=days_since_earliest)

#create a summarised df on microcosm level with each species seperate
# Make sure, that we have n_frames and not N_frames
names(id_dd)[names(id_dd) == "N_frames"] <- "n_frames"

#extrapolation_factor <- 9.301902  # for 16 x magnification 
extrapolation_factor <- 9.828125  # for 25 x magnification 
video_biomass_species <- c( "C", "P", "S","D","L","T")

biomasses <- id_dd %>%
  dplyr::group_by( day,temperature,nutrients,sample_ID,composition,predict_spec) %>% # group  by xxx
  dplyr::summarize(
    biomass = sum(biomass * n_frames, na.rm = TRUE) / (1 * 125) # if not 3 videos corrections is done below with dens_factor
  ) %>%
  dplyr::mutate(
    biomass = biomass * extrapolation_factor,
    )

biomasses<-biomasses%>%dplyr::mutate(biomass=biomass*1000)

dd_ts_id<-biomasses

#fill up missing dates with biomass<-0

fill_dd<-expand.grid(sample_ID=unique(dd_ts_id$sample_ID),day=unique(dd_ts_id$day),predict_spec=unique(dd_ts_id$predict_spec))
complete_ts<-full_join(fill_dd,dd_ts_id,join_by(sample_ID,day,predict_spec))

complete_ts$biomass[is.na(complete_ts$biomass)]<-0
complete_ts<-complete_ts%>%dplyr::mutate(composition=sub("_.*", "", sample_ID))
complete_ts<-complete_ts %>%
  dplyr::mutate(temperature = sapply(strsplit(as.character(sample_ID), "_"), function(x) paste(x[3], x[4], sep = "-")))
complete_ts<- dplyr::mutate(complete_ts,nutrients = gsub(".*Nut(.*?)_.*", "\\1", sample_ID))

# Now remove wrong combinations of composition and predict_spec / predict_spec

complete_ts<- complete_ts %>%
  rowwise() %>%
  dplyr::filter(predict_spec %in% unlist(strsplit(composition, ""))) %>%
  ungroup()  
complete_ts<-dplyr::mutate(complete_ts,temperature=as.character(temperature),
                    nutrients=as.character(nutrients),
                    richness=nchar(composition))

complete_ts<-complete_ts%>%group_by(sample_ID,composition,day)%>%dplyr::mutate(tot_biomass=sum(biomass))
complete_ts<-complete_ts%>%dplyr::mutate(biom_contribution=biomass/tot_biomass)

df_biomass_mod <- complete_ts

complete_ts<-complete_ts%>%dplyr::mutate(temperature=paste0(temperature," °C"),
                                      nutrients=paste0(nutrients," g/L"))


# introduce slopes of 
names(df_slopes)[names(df_slopes)=="species_initial"]<-"predict_spec"

slope_ts<-full_join(dplyr::select(df_slopes,nutrients,predict_spec,temperature,slope),complete_ts)
slope_ts<-slope_ts%>%dplyr::mutate(w_slope=biom_contribution*slope,
                            sign=sign(slope))

slope_ts<-slope_ts%>%group_by(sample_ID,temperature,nutrients,richness,composition,day,tot_biomass)%>%dplyr::summarize(
  sum_w_slopes=abs(sum(w_slope)),
                   mean_abs_slope=mean(abs(slope)),
  sum_abs_slope=sum(abs(slope)),
  abs_sum_slope=abs(sum(slope)),
  symmetry=abs(sum(sign)))


slope_ts<-slope_ts%>%dplyr::mutate(richness=as.factor(richness))


##create new variable where it checks, where the last observation =0 is; with complete_ts
aggr_ts <- slope_ts %>%
  group_by( sample_ID) %>%
  arrange(day) %>%
  mutate(
    # Create a flag for non-zero tot_biomass
    non_zero_biomass = tot_biomass != 0,
    # Find the last non-zero day
    last_non_zero_day = ifelse(any(non_zero_biomass), max(day[non_zero_biomass], na.rm = TRUE), NA),
    # Find the first zero day after the last non-zero day
    first_zero_day = ifelse(
      !is.na(last_non_zero_day),
      min(day[!non_zero_biomass & day > last_non_zero_day], na.rm = TRUE),
      NA
    ),
    # Flag for days after the first zero day
    is_after_first_zero_day = ifelse(!is.na(first_zero_day), day > first_zero_day, FALSE)
  ) %>%
  ungroup()

aggr_ts<-aggr_ts%>%mutate(rep_var=sub("_[^_]+$", "", sample_ID))

biomass_ts<-aggr_ts%>%group_by(day,temperature,nutrients,richness)%>%summarize(tot_biom=mean(tot_biomass),se_tot_biom=sd(tot_biomass)/sqrt(as.numeric(length(tot_biomass))))
```


# Biomass

Let's have a look at the biomass dynamics in the different environmental treatments.

### tot biomass plot

<img src="Results_RD_mod_files/figure-html/plot_biomass-1.png" style="display: block; margin: auto;" />
 **Figure 1** : Community total biomass during the experiment in different environmental treatments. Different color represent richness levels.



# Main Results 

We now look at the main results of the experiment. We are going to look first at the effect of richness, temperature and nutrients on community temporal stability. Then, we are going to look at the relationship between divergence (original response diversity metric) and temporal stability. Finally, we are going to look at the relationship between response diversity and temporal stability.

In the whole analysis, we calculated the temporal stability of total community biomass as the inverse of the coefficient of variation (ICV) (i.e. $\frac{\sigma}{\mu}$). 


### Effect of T, N and R
<img src="Results_RD_mod_files/figure-html/boxplots_TNR-1.png" style="display: block; margin: auto;" />
**Figure 2**: Effects of richness (a), temperature (b), and nutrients (c) on community total biomass temporal stability.



We can see that richness does not have a clear effect on community temporal stability, while stability was higher at lower temperature, and nutrients increased community temporal stability.

### Effect of Divergence

We look at the relationship between divergence (our original response diversity metric) and stability

<img src="Results_RD_mod_files/figure-html/divergence_CV-1.png" style="display: block; margin: auto;" />
**Figure 3**: Relationship between Divergence and temporal stability of total community biomass.



Divergence is positively related to temporal stability, suggesting that response diversity promotes stability. However, the relationship between divergence and stability becomes weaker as richness increases. We think that this is due to divergence considering only the responses of the 2 most "responding" species. Thus, when species richness increases, disregarding the responses of the other species in the community except the 2 responding the most makes the relationship between response diversity and stability weaker. 

This is why, after running the experiment, we developed another metric to measure response diversity, which we called **balance**, and that is presented in the main text of the publication. 
Balance has several desirable features that makes it a more suitable metric than divergence: Independence of richness, higher predictive power, and accounts for the responses of all species in the community (as opposed to divergence that accounts for only the 2 most "responding" species).

Here, we provide extensive evidence of why balance is a better metric to measure response diversity than divergence, and thus justifying focusing the analysis around balance.

# Comparing Divergence and Balance

We first compare how well divergence and balance predict stability (predictive power). 



## Balance


Model assumptions

``` r
# 

mod1 <- lm(data=complete_aggr,log10(stability)~log10(balance_f))
check_model(mod1)
```

<img src="Results_RD_mod_files/figure-html/model_check1-1.png" style="display: block; margin: auto;" />



## Divergence

``` r
mod2 <- lm(data=complete_aggr,log10(stability)~(divergence))
check_model(mod2)
```

<img src="Results_RD_mod_files/figure-html/model_check2-1.png" style="display: block; margin: auto;" />



**Table 1**: Comparision of model performance of divergence and balance as predictors of stability. Model 1 has balance as predictor and model 2 has divergence as predictor.
<table class="table" style="color: black; width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> model </th>
   <th style="text-align:right;"> AIC </th>
   <th style="text-align:right;"> AICc </th>
   <th style="text-align:right;"> BIC </th>
   <th style="text-align:right;"> R2 </th>
   <th style="text-align:right;"> R2_adjusted </th>
   <th style="text-align:right;"> RMSE </th>
   <th style="text-align:right;"> Sigma </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:right;"> -89.27328 </td>
   <td style="text-align:right;"> -89.17286 </td>
   <td style="text-align:right;"> -78.79409 </td>
   <td style="text-align:right;"> 0.1917679 </td>
   <td style="text-align:right;"> 0.1884142 </td>
   <td style="text-align:right;"> 0.1510344 </td>
   <td style="text-align:right;"> 0.1516599 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:right;"> -55.71579 </td>
   <td style="text-align:right;"> -55.61538 </td>
   <td style="text-align:right;"> -45.23661 </td>
   <td style="text-align:right;"> 0.0720796 </td>
   <td style="text-align:right;"> 0.0682293 </td>
   <td style="text-align:right;"> 0.1618316 </td>
   <td style="text-align:right;"> 0.1625017 </td>
  </tr>
</tbody>
</table>

A model with Balance as predictor performs better than one with divergence as predictor, and it explains more of the variance in stability than divergence.



Moreover, from **Figure 3**, it looks like divergence declines in performance as richness increases. Let's test this analytically.
To do than we build a linear model having stability as response variable and either log10(balance) or divergence as predictor for each richness level. We then extract the R squared of the models and their *standardised* estimates. (standardised estimates were calculated centering divergence and balance using the function scale()).


``` r
# getting model estimates for each richness level
lm_divergence_richness_E <- complete_aggr %>%
  nest(data = -richness) %>%
  mutate(
    model = map(data, ~ lm(log10(stability) ~ scale(divergence), data = .x)),
    results = map(model, broom::tidy)
  ) %>%
  unnest(results) %>% dplyr::filter(term=="scale(divergence)") 


# getting model R squared for each richness level

lm_divergence_richness_R <- complete_aggr %>%
  nest(data = -richness) %>%
  mutate(
    model = map(data, ~ lm(log10(stability) ~ scale(divergence), data = .x)),
    results = map(model, broom::glance)
  ) %>%
  unnest(results) 

lm_divergence_richness_R
```

```
## # A tibble: 3 × 15
##   richness data     model  r.squared adj.r.squared sigma statistic p.value    df
##   <fct>    <list>   <list>     <dbl>         <dbl> <dbl>     <dbl>   <dbl> <dbl>
## 1 2        <tibble> <lm>      0.201         0.191  0.209     19.9  2.66e-5     1
## 2 3        <tibble> <lm>      0.172         0.161  0.108     16.4  1.19e-4     1
## 3 4        <tibble> <lm>      0.0337        0.0215 0.129      2.76 1.01e-1     1
## # ℹ 6 more variables: logLik <dbl>, AIC <dbl>, BIC <dbl>, deviance <dbl>,
## #   df.residual <int>, nobs <int>
```



``` r
# getting model estimatesf or each richness level
lm_balance_richness_E <- complete_aggr %>%
  nest(data = -richness) %>%
  mutate(
    model = map(data, ~ lm(log10(stability) ~ scale(log10(balance_f)), data = .x)),
    results = map(model, broom::tidy)
  ) %>%
  unnest(results) %>% dplyr::filter(term=="scale(log10(balance_f))") 



# getting model R squared for each richness level
lm_balance_richness_R <- complete_aggr %>%
  nest(data = -richness) %>%
  mutate(
    model = map(data, ~ lm(log10(stability) ~ scale(log10(balance_f)), data = .x)),
    results = map(model, broom::glance)
  ) %>%
  unnest(results) 

lm_balance_richness_R
```

```
## # A tibble: 3 × 15
##   richness data     model  r.squared adj.r.squared sigma statistic p.value    df
##   <fct>    <list>   <list>     <dbl>         <dbl> <dbl>     <dbl>   <dbl> <dbl>
## 1 2        <tibble> <lm>       0.188         0.177 0.210      18.2 5.36e-5     1
## 2 3        <tibble> <lm>       0.232         0.222 0.104      23.8 5.44e-6     1
## 3 4        <tibble> <lm>       0.272         0.263 0.112      29.6 5.84e-7     1
## # ℹ 6 more variables: logLik <dbl>, AIC <dbl>, BIC <dbl>, deviance <dbl>,
## #   df.residual <int>, nobs <int>
```


<img src="Results_RD_mod_files/figure-html/R_squared-1.png" style="display: block; margin: auto;" />
**Figure 4**: Performance comparison of divergence vs balance. In (a), the R squared of linear models for divergence and balance are shown for each richness level. In (b), the estimates of the linear models for divergence and balance are shown for each richness level.




We can see that the R squared of divergence as predictor of stability becomes smaller as richness increases, while the R squared of balance as predictor of stability does not (actually increases slightly). 



Now we build a linear model were stability is modeled as a function of balance and divergence. 
Then, we compared the variance explained by the full model compared to a model containing either only balance or only divergence.


``` r
lm_div_balance <- lm(data=complete_aggr,log10(stability)~log10(balance_f)+divergence)
check_model(lm_div_balance)
```

<img src="Results_RD_mod_files/figure-html/model_check3-1.png" style="display: block; margin: auto;" />



**model with only divergence**

``` r
lm_div <- lm(data=complete_aggr,log10(stability)~divergence)
check_model(lm_div)
```

<img src="Results_RD_mod_files/figure-html/model_check4-1.png" style="display: block; margin: auto;" />




**model with only balance**

``` r
lm_balance <- lm(data=complete_aggr,log10(stability)~log10(balance_f))
check_model(lm_balance)
```

<img src="Results_RD_mod_files/figure-html/model_check5-1.png" style="display: block; margin: auto;" />




**Table 2**: Comparison of model performance of divergence, balance and both as predictors of stability. Model 1 has both balance and divergence as predictors, model 2 has divergence as predictor, and model 3 has balance as predictor.
<table class="table" style="color: black; width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> model </th>
   <th style="text-align:right;"> AIC </th>
   <th style="text-align:right;"> AICc </th>
   <th style="text-align:right;"> BIC </th>
   <th style="text-align:right;"> R2 </th>
   <th style="text-align:right;"> R2_adjusted </th>
   <th style="text-align:right;"> RMSE </th>
   <th style="text-align:right;"> Sigma </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:right;"> -88.97683 </td>
   <td style="text-align:right;"> -88.80876 </td>
   <td style="text-align:right;"> -75.00458 </td>
   <td style="text-align:right;"> 0.1974141 </td>
   <td style="text-align:right;"> 0.1907259 </td>
   <td style="text-align:right;"> 0.1505060 </td>
   <td style="text-align:right;"> 0.1514437 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:right;"> -55.71579 </td>
   <td style="text-align:right;"> -55.61538 </td>
   <td style="text-align:right;"> -45.23661 </td>
   <td style="text-align:right;"> 0.0720796 </td>
   <td style="text-align:right;"> 0.0682293 </td>
   <td style="text-align:right;"> 0.1618316 </td>
   <td style="text-align:right;"> 0.1625017 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:right;"> -89.27328 </td>
   <td style="text-align:right;"> -89.17286 </td>
   <td style="text-align:right;"> -78.79409 </td>
   <td style="text-align:right;"> 0.1917679 </td>
   <td style="text-align:right;"> 0.1884142 </td>
   <td style="text-align:right;"> 0.1510344 </td>
   <td style="text-align:right;"> 0.1516599 </td>
  </tr>
</tbody>
</table>





Type III anova table: a model with both balance and divergence as predictors is not significantly different from a model with only balance as predictor.

``` r
anova(lm_div_balance,  lm_balance)
```

```
## Analysis of Variance Table
## 
## Model 1: log10(stability) ~ log10(balance_f) + divergence
## Model 2: log10(stability) ~ log10(balance_f)
##   Res.Df    RSS Df Sum of Sq      F Pr(>F)
## 1    240 5.5044                           
## 2    241 5.5432 -1 -0.038724 1.6884 0.1951
```




Type III anova table: a model with both balance and divergence as predictors is significantly better from a model with only divergence as predictor.

``` r
anova(lm_div_balance,  lm_div)
```

```
## Analysis of Variance Table
## 
## Model 1: log10(stability) ~ log10(balance_f) + divergence
## Model 2: log10(stability) ~ divergence
##   Res.Df    RSS Df Sum of Sq      F    Pr(>F)    
## 1    240 5.5044                                  
## 2    241 6.3640 -1  -0.85959 37.479 3.741e-09 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

Overall, balance explains more of the variance in stability than divergence, and there is virtually no difference between a model containing only balance and the full model.




**Interaction divergence and richness**

Richness had to transformed to numeric and to be centered to avoid collinearity with divergence


``` r
lm_rich_div <- lm(data=complete_aggr,log10(stability)~divergence*scale(as.numeric(richness)))
check_model(lm_rich_div)
```

<img src="Results_RD_mod_files/figure-html/model_check6-1.png" style="display: block; margin: auto;" />




Type III anova table

``` r
anova(lm_rich_div)
```

```
## Analysis of Variance Table
## 
## Response: log10(stability)
##                                         Df Sum Sq Mean Sq F value    Pr(>F)    
## divergence                               1 0.4943 0.49435 19.8282 1.301e-05 ***
## scale(as.numeric(richness))              1 0.1558 0.15579  6.2487  0.013100 *  
## divergence:scale(as.numeric(richness))   1 0.2496 0.24958 10.0106  0.001758 ** 
## Residuals                              239 5.9587 0.02493                      
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```


Divergence significantly interact with richness, suggesting that the relationship between divergence and stability changes with richness. 
While an ideal metric of response diversity should be independent of richness.



We repeat the same model using balance instead of divergence.

``` r
lm_rich_balance <- lm(data=complete_aggr,log10(stability)~log10(balance_f)*scale(as.numeric(richness)))
check_model(lm_rich_balance)
```

<img src="Results_RD_mod_files/figure-html/model_check7-1.png" style="display: block; margin: auto;" />





Type III anova table

``` r
anova(lm_rich_balance)
```

```
## Analysis of Variance Table
## 
## Response: log10(stability)
##                                               Df Sum Sq Mean Sq F value
## log10(balance_f)                               1 1.3152 1.31522 57.1274
## scale(as.numeric(richness))                    1 0.0003 0.00028  0.0122
## log10(balance_f):scale(as.numeric(richness))   1 0.0405 0.04050  1.7589
## Residuals                                    239 5.5024 0.02302        
##                                                 Pr(>F)    
## log10(balance_f)                             8.694e-13 ***
## scale(as.numeric(richness))                     0.9123    
## log10(balance_f):scale(as.numeric(richness))    0.1860    
## Residuals                                                 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

Balance does not significantly interact with richness, suggesting that the relationship between balance and stability is stable across richness levels.



Finally, we assess variable importance using the relative importance of predictors in the full model.
We use the package vip (https://cran.r-project.org/web/packages/vip/vignettes/vip.html) to calculate the relative importance of predictors in the full model.
The function vip::vip for multiple linear regression, or linear models (LMs), uses the absolute value of the -statistic  as a measure of VI.
Motivation for the use of the associated 𝑡-statistic is given in Bring (1994) [https://www.tandfonline.com/doi/abs/10.1080/00031305.1994.10476059].


``` r
vip::vip(lm_div_balance)
```

<img src="Results_RD_mod_files/figure-html/vip1-1.png" style="display: block; margin: auto;" />
**Figure 5**: Variable importance in the model including both balance and divergence as predictors of stability.


We believe that the extensive evidence here provided justifies focusing the analysis around balance, and not divergence, as a metric of response diversity.
We will thus only look at balance for the rest of the analysis. 



# Effect RD

We are now going to look at how response diversity (balance) affected temporal stability of total community biomass. We are going to look at the relationship between fundamental balance (so based only on species response surfaces measured in monoculture), an realised balance (measured accounting for species contribution to balance).

This is fundamentally testing our most important hypothesis.

<img src="Results_RD_mod_files/figure-html/effect_RD-1.png" style="display: block; margin: auto;" />
**Figure 6**: Effects of fundamental and realised response diversity (measured as balance) on total community biomass temporal stability.


We can see that balance is always negatively related to temporal stability, which means that response diversity promotes stability across richness levels. Interestingly, we see that there is little difference between fundamental and realised balance. Yet, as the richness increases, the relationship between realised balance and stability becomes steeper compared to fundamental balance. 





# Linear models


## Model: Fundamental balance

First we analyze the effect of fundamental balance, temperature, nutrients and richness on biomass temporal stability using a linear model. 
balance was modelled as continuous variables, while richness, temperature and nutrients were modelled as categorical variables. balance and stability were log-transformed to meet the assumptions of linear models.



``` r
lm_full<-lm(data=complete_aggr,log10(stability)~log10(balance_f)+(richness)+nutrients+temperature)
```



Check model's assumptions
<div class="figure" style="text-align: center">
<img src="Results_RD_mod_files/figure-html/model_check8-1.png" alt="model check 1."  />
<p class="caption">(\#fig:model_check8)model check 1.</p>
</div>




``` r
summary(lm_full)
```

```
## 
## Call:
## lm(formula = log10(stability) ~ log10(balance_f) + (richness) + 
##     nutrients + temperature, data = complete_aggr)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.29981 -0.07245 -0.00896  0.04696  0.41550 
## 
## Coefficients:
##                     Estimate Std. Error t value Pr(>|t|)    
## (Intercept)         -0.34893    0.02822 -12.367  < 2e-16 ***
## log10(balance_f)    -0.05352    0.01596  -3.354 0.000929 ***
## richness3           -0.04166    0.01849  -2.253 0.025193 *  
## richness4           -0.01244    0.01871  -0.665 0.506925    
## nutrients0.35 g/L    0.17958    0.01860   9.656  < 2e-16 ***
## nutrients0.75 g/L    0.21218    0.01932  10.982  < 2e-16 ***
## temperature22-25 °C -0.07781    0.01853  -4.199 3.81e-05 ***
## temperature25-28 °C -0.09830    0.02457  -4.002 8.44e-05 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.1177 on 235 degrees of freedom
## Multiple R-squared:  0.5256,	Adjusted R-squared:  0.5115 
## F-statistic:  37.2 on 7 and 235 DF,  p-value: < 2.2e-16
```


``` r
anova(lm_full)
```

```
## Analysis of Variance Table
## 
## Response: log10(stability)
##                   Df Sum Sq Mean Sq F value    Pr(>F)    
## log10(balance_f)   1 1.3152 1.31522 94.9984 < 2.2e-16 ***
## richness           2 0.0850 0.04251  3.0706   0.04826 *  
## nutrients          2 1.8765 0.93823 67.7684 < 2.2e-16 ***
## temperature        2 0.3282 0.16410 11.8530 1.246e-05 ***
## Residuals        235 3.2535 0.01384                      
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

A linear model was fitted to examine the effects of resource balance, richness, nutrients, and temperature on community stability (measured as log₁₀(stability)). The model explained a significant portion of the variance (Adjusted R² = 0.5115, F(7, 235) = 37.2, p < 2.2e-16).

The intercept of the model was estimated at -0.349 (SE = 0.028, p < 2e-16), indicating the baseline log₁₀(stability) when all predictor variables are at their reference levels.

Among the predictors, log₁₀(balance) showed a significant negative effect on stability (Estimate = -0.054, SE = 0.016, p = 0.0009). This suggests that as balance increases (more balance), stability tends to decrease.

In terms of richness, communities with three species (richness3) showed a significant decrease in stability compared to the reference level (richness2; 𝛽=−0.042±0.018β=−0.042±0.018; t=−2.25, p=0.025). In contrast, communities with four species (richness4) did not differ significantly from the reference level (𝛽=−0.012±0.019β=−0.012±0.019; t=−0.67, p=0.51).
In contrast, communities with four species (richness4) did not differ significantly from the reference level (𝛽=−0.012±0.019β=−0.012±0.019; t=−0.67, p=0.51).


Nutrient concentration also had a significant positive effect on stability, with estimates for 0.35 g/L (Estimate = 0.180, SE = 0.019, p < 2e-16) and 0.75 g/L (Estimate = 0.212, SE = 0.019, p < 2e-16) indicating increased stability with higher nutrient levels.

Finally, temperature regimes showed a significant effect on stability. Both 22–25 °C (Estimate = -0.078, SE = 0.019, p = 3.81e-05) and 25–28 °C (Estimate = -0.098, SE = 0.025, p = 8.44e-05) significantly reduced stability when compared to the baseline (18–21 °C).


In summary, our findings show that temporal stability is significantly influenced by response diversity (balance), nutrient concentration, and temperature, with higher nutrient concentrations enhancing stability and higher temperatures reducing it. However, species richness was not a significant determinant of stability within the conditions of this study.

Prepare publication-ready table 

Summary table




**Table 2**: Linear model results for the effects of balance, richness, nutrients, and temperature on community stability. Estimates are presented with 95% confidence intervals and p-values. Significant results are highlighted in bold.
<table class="table table-striped table-hover table-condensed" style="color: black; width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:center;"> Term </th>
   <th style="text-align:center;"> Estimate </th>
   <th style="text-align:center;"> Lower 95% CI </th>
   <th style="text-align:center;"> Upper 95% CI </th>
   <th style="text-align:center;"> t value </th>
   <th style="text-align:center;"> p-value </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:center;font-weight: bold;"> (Intercept) </td>
   <td style="text-align:center;font-weight: bold;"> -0.349 </td>
   <td style="text-align:center;font-weight: bold;"> -0.405 </td>
   <td style="text-align:center;font-weight: bold;"> -0.293 </td>
   <td style="text-align:center;font-weight: bold;"> -12.367 </td>
   <td style="text-align:center;font-weight: bold;"> 2.17e-27 </td>
  </tr>
  <tr>
   <td style="text-align:center;font-weight: bold;"> log10(balance_f) </td>
   <td style="text-align:center;font-weight: bold;"> -0.054 </td>
   <td style="text-align:center;font-weight: bold;"> -0.085 </td>
   <td style="text-align:center;font-weight: bold;"> -0.022 </td>
   <td style="text-align:center;font-weight: bold;"> -3.354 </td>
   <td style="text-align:center;font-weight: bold;"> 9.29e-04 </td>
  </tr>
  <tr>
   <td style="text-align:center;font-weight: bold;"> richness3 </td>
   <td style="text-align:center;font-weight: bold;"> -0.042 </td>
   <td style="text-align:center;font-weight: bold;"> -0.078 </td>
   <td style="text-align:center;font-weight: bold;"> -0.005 </td>
   <td style="text-align:center;font-weight: bold;"> -2.253 </td>
   <td style="text-align:center;font-weight: bold;"> 2.52e-02 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> richness4 </td>
   <td style="text-align:center;"> -0.012 </td>
   <td style="text-align:center;"> -0.049 </td>
   <td style="text-align:center;"> 0.024 </td>
   <td style="text-align:center;"> -0.665 </td>
   <td style="text-align:center;"> 5.07e-01 </td>
  </tr>
  <tr>
   <td style="text-align:center;font-weight: bold;"> nutrients0.35 g/L </td>
   <td style="text-align:center;font-weight: bold;"> 0.180 </td>
   <td style="text-align:center;font-weight: bold;"> 0.143 </td>
   <td style="text-align:center;font-weight: bold;"> 0.216 </td>
   <td style="text-align:center;font-weight: bold;"> 9.656 </td>
   <td style="text-align:center;font-weight: bold;"> 8.60e-19 </td>
  </tr>
  <tr>
   <td style="text-align:center;font-weight: bold;"> nutrients0.75 g/L </td>
   <td style="text-align:center;font-weight: bold;"> 0.212 </td>
   <td style="text-align:center;font-weight: bold;"> 0.174 </td>
   <td style="text-align:center;font-weight: bold;"> 0.250 </td>
   <td style="text-align:center;font-weight: bold;"> 10.982 </td>
   <td style="text-align:center;font-weight: bold;"> 6.43e-23 </td>
  </tr>
  <tr>
   <td style="text-align:center;font-weight: bold;"> temperature22-25 °C </td>
   <td style="text-align:center;font-weight: bold;"> -0.078 </td>
   <td style="text-align:center;font-weight: bold;"> -0.114 </td>
   <td style="text-align:center;font-weight: bold;"> -0.041 </td>
   <td style="text-align:center;font-weight: bold;"> -4.199 </td>
   <td style="text-align:center;font-weight: bold;"> 3.81e-05 </td>
  </tr>
  <tr>
   <td style="text-align:center;font-weight: bold;"> temperature25-28 °C </td>
   <td style="text-align:center;font-weight: bold;"> -0.098 </td>
   <td style="text-align:center;font-weight: bold;"> -0.147 </td>
   <td style="text-align:center;font-weight: bold;"> -0.050 </td>
   <td style="text-align:center;font-weight: bold;"> -4.002 </td>
   <td style="text-align:center;font-weight: bold;"> 8.44e-05 </td>
  </tr>
</tbody>
</table>


**Table 3**:Pairwise contrasts

```{=html}
<div id="mhyiqclkzt" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#mhyiqclkzt table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#mhyiqclkzt thead, #mhyiqclkzt tbody, #mhyiqclkzt tfoot, #mhyiqclkzt tr, #mhyiqclkzt td, #mhyiqclkzt th {
  border-style: none;
}

#mhyiqclkzt p {
  margin: 0;
  padding: 0;
}

#mhyiqclkzt .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#mhyiqclkzt .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#mhyiqclkzt .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#mhyiqclkzt .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#mhyiqclkzt .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#mhyiqclkzt .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#mhyiqclkzt .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#mhyiqclkzt .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#mhyiqclkzt .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#mhyiqclkzt .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#mhyiqclkzt .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#mhyiqclkzt .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#mhyiqclkzt .gt_spanner_row {
  border-bottom-style: hidden;
}

#mhyiqclkzt .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#mhyiqclkzt .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#mhyiqclkzt .gt_from_md > :first-child {
  margin-top: 0;
}

#mhyiqclkzt .gt_from_md > :last-child {
  margin-bottom: 0;
}

#mhyiqclkzt .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#mhyiqclkzt .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#mhyiqclkzt .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#mhyiqclkzt .gt_row_group_first td {
  border-top-width: 2px;
}

#mhyiqclkzt .gt_row_group_first th {
  border-top-width: 2px;
}

#mhyiqclkzt .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#mhyiqclkzt .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#mhyiqclkzt .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#mhyiqclkzt .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#mhyiqclkzt .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#mhyiqclkzt .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#mhyiqclkzt .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#mhyiqclkzt .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#mhyiqclkzt .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#mhyiqclkzt .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#mhyiqclkzt .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#mhyiqclkzt .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#mhyiqclkzt .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#mhyiqclkzt .gt_left {
  text-align: left;
}

#mhyiqclkzt .gt_center {
  text-align: center;
}

#mhyiqclkzt .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#mhyiqclkzt .gt_font_normal {
  font-weight: normal;
}

#mhyiqclkzt .gt_font_bold {
  font-weight: bold;
}

#mhyiqclkzt .gt_font_italic {
  font-style: italic;
}

#mhyiqclkzt .gt_super {
  font-size: 65%;
}

#mhyiqclkzt .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#mhyiqclkzt .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#mhyiqclkzt .gt_indent_1 {
  text-indent: 5px;
}

#mhyiqclkzt .gt_indent_2 {
  text-indent: 10px;
}

#mhyiqclkzt .gt_indent_3 {
  text-indent: 15px;
}

#mhyiqclkzt .gt_indent_4 {
  text-indent: 20px;
}

#mhyiqclkzt .gt_indent_5 {
  text-indent: 25px;
}

#mhyiqclkzt .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#mhyiqclkzt div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_col_headings gt_spanner_row">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="2" colspan="1" scope="col" id="label"><span class='gt_from_md'><strong>Predictor</strong></span></th>
      <th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="1" scope="col" id="**Linear Regression Results**">
        <div class="gt_column_spanner"><span class='gt_from_md'><strong>Linear Regression Results</strong></span></div>
      </th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="2" colspan="1" scope="col" id="conf.low"><span class='gt_from_md'><strong>95% CI</strong></span><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span></th>
      <th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="1" scope="col" id="**Linear Regression Results**">
        <div class="gt_column_spanner"><span class='gt_from_md'><strong>Linear Regression Results</strong></span></div>
      </th>
    </tr>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="estimate"><span class='gt_from_md'><strong>Estimate</strong></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="p.value"><span class='gt_from_md'><strong>p-value</strong></span></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">log10(balance_f)</td>
<td headers="estimate" class="gt_row gt_center">-0.05</td>
<td headers="conf.low" class="gt_row gt_center">-0.08, -0.02</td>
<td headers="p.value" class="gt_row gt_center"><0.001</td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">richness</td>
<td headers="estimate" class="gt_row gt_center"><br /></td>
<td headers="conf.low" class="gt_row gt_center"><br /></td>
<td headers="p.value" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    richness3 - richness2</td>
<td headers="estimate" class="gt_row gt_center">-0.04</td>
<td headers="conf.low" class="gt_row gt_center">-0.09, 0.00</td>
<td headers="p.value" class="gt_row gt_center">0.065</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    richness4 - richness2</td>
<td headers="estimate" class="gt_row gt_center">-0.01</td>
<td headers="conf.low" class="gt_row gt_center">-0.06, 0.03</td>
<td headers="p.value" class="gt_row gt_center">0.8</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    richness4 - richness3</td>
<td headers="estimate" class="gt_row gt_center">0.03</td>
<td headers="conf.low" class="gt_row gt_center">-0.01, 0.07</td>
<td headers="p.value" class="gt_row gt_center">0.3</td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">nutrients</td>
<td headers="estimate" class="gt_row gt_center"><br /></td>
<td headers="conf.low" class="gt_row gt_center"><br /></td>
<td headers="p.value" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    (0.35 g/L) - (0.01 g/L)</td>
<td headers="estimate" class="gt_row gt_center">0.18</td>
<td headers="conf.low" class="gt_row gt_center">0.14, 0.22</td>
<td headers="p.value" class="gt_row gt_center"><0.001</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    (0.75 g/L) - (0.01 g/L)</td>
<td headers="estimate" class="gt_row gt_center">0.21</td>
<td headers="conf.low" class="gt_row gt_center">0.17, 0.26</td>
<td headers="p.value" class="gt_row gt_center"><0.001</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    (0.75 g/L) - (0.35 g/L)</td>
<td headers="estimate" class="gt_row gt_center">0.03</td>
<td headers="conf.low" class="gt_row gt_center">-0.01, 0.08</td>
<td headers="p.value" class="gt_row gt_center">0.2</td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">temperature</td>
<td headers="estimate" class="gt_row gt_center"><br /></td>
<td headers="conf.low" class="gt_row gt_center"><br /></td>
<td headers="p.value" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    (22-25 °C) - (18-21 °C)</td>
<td headers="estimate" class="gt_row gt_center">-0.08</td>
<td headers="conf.low" class="gt_row gt_center">-0.12, -0.03</td>
<td headers="p.value" class="gt_row gt_center"><0.001</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    (25-28 °C) - (18-21 °C)</td>
<td headers="estimate" class="gt_row gt_center">-0.10</td>
<td headers="conf.low" class="gt_row gt_center">-0.16, -0.04</td>
<td headers="p.value" class="gt_row gt_center"><0.001</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    (25-28 °C) - (22-25 °C)</td>
<td headers="estimate" class="gt_row gt_center">-0.02</td>
<td headers="conf.low" class="gt_row gt_center">-0.08, 0.04</td>
<td headers="p.value" class="gt_row gt_center">0.7</td></tr>
  </tbody>
  
  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="4"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span> <span class='gt_from_md'>CI = Confidence Interval</span></td>
    </tr>
  </tfoot>
</table>
</div>
```


### sum vs. weighted_sum

We now look at how fundamental and realised balance are related to each other. 

<img src="Results_RD_mod_files/figure-html/Cor_sum_weighted_sum-1.png" style="display: block; margin: auto;" />
**Figure 7**: Relationship between fundamental and realised balance. The black line represents the 1:1 relationship between fundamental and realised balance.



# Asynchrony
Response diversity (aka balance) has been suggested as a mechanism that promotes temporal stability of community biomass by promoting species asynchrony.

We thus calculated the asynchrony index suggested by Gross et al. (2014)[https://www.journals.uchicago.edu/doi/epdf/10.1086/673915] to calculate the effect of asynchrony on temporal stability and to see how reponse diversity relate to asynchrony.
The index ranges between -1 and 1, with -1 indicating perfect asyncrony and 1 being perfectly synchronous, and 0 indicating random variation.





### Plot stability vs. Asynchrony Gross
<img src="Results_RD_mod_files/figure-html/async_plots-1.png" style="display: block; margin: auto;" />
**Figure 8**: Relationship between temporal stability and asynchrony (Gross) divided by nutrient level.

### Plot Asynchrony Gross vs fundamental balance

<img src="Results_RD_mod_files/figure-html/async-1.png" style="display: block; margin: auto;" />
**Figure 9**: Relationship between asynchrony (Gross) and fundamental balance divided by nutrient level.

# SEM 


Now, we use a structural equation model (SEM) to explore how stability is influenced by asynchrony, temperature, nutrient levels, balance, and richness, with asynchrony also modeled as dependent on balance, nutrients, and richness.




``` r
model1B <- '
  stability ~ asynchrony_Gross
  +temperature
  +nutrients
  +log_balance_f+
  richness

  
  asynchrony_Gross ~ log_balance_f
  +nutrients
  +richness
'


# Fit the model

fit1B <- sem(model1B, estimator="MLM",meanstructure = TRUE,data = sem_aggr%>%dplyr::filter(!is.na(synchrony_Gross)))


# Summarize the results
summary(fit1B, standardized = TRUE,rsquare=T, fit.measures = TRUE)
```

```
## lavaan 0.6-19 ended normally after 1 iteration
## 
##   Estimator                                         ML
##   Optimization method                           NLMINB
##   Number of model parameters                        12
## 
##   Number of observations                           241
## 
## Model Test User Model:
##                                               Standard      Scaled
##   Test Statistic                                 1.777       1.537
##   Degrees of freedom                                 1           1
##   P-value (Chi-square)                           0.183       0.215
##   Scaling correction factor                                  1.156
##     Satorra-Bentler correction                                    
## 
## Model Test Baseline Model:
## 
##   Test statistic                               295.357     353.081
##   Degrees of freedom                                 9           9
##   P-value                                        0.000       0.000
##   Scaling correction factor                                  0.837
## 
## User Model versus Baseline Model:
## 
##   Comparative Fit Index (CFI)                    0.997       0.998
##   Tucker-Lewis Index (TLI)                       0.976       0.986
##                                                                   
##   Robust Comparative Fit Index (CFI)                         0.998
##   Robust Tucker-Lewis Index (TLI)                            0.981
## 
## Loglikelihood and Information Criteria:
## 
##   Loglikelihood user model (H0)                152.706     152.706
##   Loglikelihood unrestricted model (H1)             NA          NA
##                                                                   
##   Akaike (AIC)                                -281.411    -281.411
##   Bayesian (BIC)                              -239.594    -239.594
##   Sample-size adjusted Bayesian (SABIC)       -277.631    -277.631
## 
## Root Mean Square Error of Approximation:
## 
##   RMSEA                                          0.057       0.047
##   90 Percent confidence interval - lower         0.000       0.000
##   90 Percent confidence interval - upper         0.192       0.176
##   P-value H_0: RMSEA <= 0.050                    0.306       0.356
##   P-value H_0: RMSEA >= 0.080                    0.531       0.469
##                                                                   
##   Robust RMSEA                                               0.051
##   90 Percent confidence interval - lower                     0.000
##   90 Percent confidence interval - upper                     0.200
##   P-value H_0: Robust RMSEA <= 0.050                         0.327
##   P-value H_0: Robust RMSEA >= 0.080                         0.525
## 
## Standardized Root Mean Square Residual:
## 
##   SRMR                                           0.012       0.012
## 
## Parameter Estimates:
## 
##   Standard errors                           Robust.sem
##   Information                                 Expected
##   Information saturated (h1) model          Structured
## 
## Regressions:
##                      Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
##   stability ~                                                             
##     asynchrny_Grss      0.195    0.033    5.947    0.000    0.195    0.399
##     temperature        -0.049    0.011   -4.454    0.000   -0.049   -0.241
##     nutrients           0.147    0.011   13.216    0.000    0.147    0.719
##     log_balance_f      -0.029    0.012   -2.452    0.014   -0.029   -0.120
##     richness            0.011    0.010    1.181    0.238    0.011    0.055
##   asynchrony_Gross ~                                                      
##     log_balance_f      -0.080    0.029   -2.719    0.007   -0.080   -0.159
##     nutrients          -0.210    0.022   -9.373    0.000   -0.210   -0.501
##     richness           -0.102    0.023   -4.404    0.000   -0.102   -0.243
## 
## Intercepts:
##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
##    .stability        -0.407    0.040  -10.122    0.000   -0.407   -2.441
##    .asynchrny_Grss    0.125    0.065    1.921    0.055    0.125    0.367
## 
## Variances:
##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
##    .stability         0.012    0.001   10.670    0.000    0.012    0.430
##    .asynchrny_Grss    0.081    0.009    8.917    0.000    0.081    0.694
## 
## R-Square:
##                    Estimate
##     stability         0.570
##     asynchrny_Grss    0.306
```
**Model Fit**

The model fit indices suggest a good fit:

Comparative Fit Index (CFI) = 0.998 and Tucker-Lewis Index (TLI) = 0.986, both indicating a good fit as values close to 1 are considered strong.
Root Mean Square Error of Approximation (RMSEA) = 0.047 (with robust RMSEA at 0.051) and Standardized Root Mean Square Residual (SRMR) = 0.012. These values indicate a good fit, with RMSEA and SRMR values below 0.05 generally preferred.


Interpretation of Pathways


**Stability**:

*Asynchrony*: Positive and highly significant effect on stability, suggesting that asynchrony (indicating lack of synchrony or compensatory dynamics) is associated with greater stability.

*Temperature*: Negative effect, where higher temperature values correlate with lower stability, potentially due to physiological stress or disruption in community dynamics at higher temperatures.

*Nutrients*: Positive and highly significant, suggesting that greater nutrient availability enhances stability, possibly through support for higher productivity or resource buffering.

*balance*: Negative and significant effect, where greater balance reduces stability.
Richness: Not significant, indicating that within this model, richness does not have a notable effect on stability.


**Asynchrony**:

*balance*: Negative and significant, suggesting that greater balance reduces asynchrony.

*Nutrients*: Negative and highly significant effect, indicating that higher nutrient concentrations are associated with lower asynchrony, possibly due to homogenizing effects of nutrient availability.

*Richness*: Negative and significant, where increased richness is associated with reduced asynchrony, possibly indicating increased interactions or overlap in resource use among species.

**Explained Variance**

*Stability*: The model explains 57% of the variance in stability, suggesting a substantial amount of stability is accounted for by these factors.

*Asynchrony*: The model explains 30.6% of the variance in asynchrony, indicating that while balance, nutrients, and richness contribute, other factors may also play a role in driving asynchrony.

**Summary**

This SEM model demonstrates that stability in the ecosystem is positively associated with asynchrony and nutrient levels, but negatively associated with temperature and balance. Interestingly, species richness has no direct impact on stability but does reduce asynchrony, indicating indirect complexity in the stability-dynamics relationship. This highlights the role of environmental and community factors in ecosystem stability, with asynchrony serving as a crucial intermediary in maintaining stability in fluctuating conditions.

<div class="figure" style="text-align: center">
<img src="SEM.png" alt="SEM." width="718" />
<p class="caption">(\#fig:SEM)SEM.</p>
</div>

**Figure 10**: Structural equation model (SEM) of the relationship between fundamental balance, asynchrony, richness, nutrients, temperature and temporal stability. The model shows that fundamental balance has a negative effect on asynchrony, which in turn has a positive effect on temporal stability. The model also shows that fundamental balance has a direct negative effect on temporal stability. Temperature has a direct negative effect on temporal stability, while nutrients have a direct positive effect on temporal stability. Richness has a direct negative effect on asynchrony, but no direct effect on temporal stability. 




# Species Interactions




``` r
lm_M_int<-lm(data=int_aggr%>%dplyr::filter(theta%in%c("none","var")),log10(stability)~log10(balance_f)+nutrients+temperature+(richness)+mean_interaction)
```


check model assumptions
<div class="figure" style="text-align: center">
<img src="Results_RD_mod_files/figure-html/model_check_mixed-1.png" alt="model check 1."  />
<p class="caption">(\#fig:model_check_mixed)model check 1.</p>
</div>



``` r
summary(lm_M_int)
```

```
## 
## Call:
## lm(formula = log10(stability) ~ log10(balance_f) + nutrients + 
##     temperature + (richness) + mean_interaction, data = int_aggr %>% 
##     dplyr::filter(theta %in% c("none", "var")))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.29920 -0.07316 -0.00719  0.05333  0.41059 
## 
## Coefficients:
##                      Estimate Std. Error t value Pr(>|t|)    
## (Intercept)         -0.355922   0.028377 -12.543  < 2e-16 ***
## log10(balance_f)    -0.054555   0.015899  -3.431 0.000710 ***
## nutrients0.35 g/L    0.192131   0.019863   9.673  < 2e-16 ***
## nutrients0.75 g/L    0.222239   0.020080  11.068  < 2e-16 ***
## temperature22-25 °C -0.072852   0.018669  -3.902 0.000125 ***
## temperature25-28 °C -0.092956   0.024651  -3.771 0.000206 ***
## richness3           -0.039305   0.018459  -2.129 0.034277 *  
## richness4           -0.003528   0.019315  -0.183 0.855244    
## mean_interaction    -0.049311   0.028230  -1.747 0.081987 .  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.1172 on 234 degrees of freedom
## Multiple R-squared:  0.5317,	Adjusted R-squared:  0.5157 
## F-statistic: 33.21 on 8 and 234 DF,  p-value: < 2.2e-16
```



``` r
anova(lm_M_int)
```

```
## Analysis of Variance Table
## 
## Response: log10(stability)
##                   Df Sum Sq Mean Sq F value    Pr(>F)    
## log10(balance_f)   1 1.3152 1.31522 95.8276 < 2.2e-16 ***
## nutrients          2 1.8824 0.94120 68.5761 < 2.2e-16 ***
## temperature        2 0.3333 0.16663 12.1409 9.623e-06 ***
## richness           2 0.0740 0.03701  2.6968   0.06952 .  
## mean_interaction   1 0.0419 0.04188  3.0512   0.08199 .  
## Residuals        234 3.2116 0.01372                      
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```


<img src="Results_RD_mod_files/figure-html/interactions-1.png" style="display: block; margin: auto;" />
**Figure 11**: Relationship between mean interaction strength and asynchrony (Gross) divided by nutrient level.

<img src="Results_RD_mod_files/figure-html/interactions_density-1.png" style="display: block; margin: auto;" />
**Figure 12**: Distribution of mean interaction coefficient faceted by temperature, nutrients, and richness. 

# SEM2 

``` r
sem_aggr2 <- int_aggr %>%
  ungroup() %>%  # Ensure there is no grouping
  mutate(
    log_balance_f = log10(balance_f),
    stability = log10(1 / CV),
    richness = as.numeric(richness),
    temperature=temperature,
    asynchrony_Gross= (-synchrony_Gross)
    #Keep it as an ordered factor
  )


model1B <- '
  stability ~ asynchrony_Gross
  +nutrients
  +log_balance_f+
  richness +
  mean_interaction+
  temperature

  
  asynchrony_Gross ~ log_balance_f
  +nutrients
  +richness+
  mean_interaction
'


# Fit the model

fit1B <- sem(model1B, estimator="MLM",meanstructure = TRUE,data = sem_aggr2%>%dplyr::filter(!is.na(asynchrony_Gross), theta%in%c("none","var")))


# Summarize the results
summary(fit1B, standardized = TRUE,rsquare=T, fit.measures = TRUE)
```

```
## lavaan 0.6-19 ended normally after 1 iteration
## 
##   Estimator                                         ML
##   Optimization method                           NLMINB
##   Number of model parameters                        14
## 
##   Number of observations                           241
## 
## Model Test User Model:
##                                               Standard      Scaled
##   Test Statistic                                 0.661       0.534
##   Degrees of freedom                                 1           1
##   P-value (Chi-square)                           0.416       0.465
##   Scaling correction factor                                  1.238
##     Satorra-Bentler correction                                    
## 
## Model Test Baseline Model:
## 
##   Test statistic                               315.059     427.408
##   Degrees of freedom                                11          11
##   P-value                                        0.000       0.000
##   Scaling correction factor                                  0.737
## 
## User Model versus Baseline Model:
## 
##   Comparative Fit Index (CFI)                    1.000       1.000
##   Tucker-Lewis Index (TLI)                       1.012       1.012
##                                                                   
##   Robust Comparative Fit Index (CFI)                         1.000
##   Robust Tucker-Lewis Index (TLI)                            1.021
## 
## Loglikelihood and Information Criteria:
## 
##   Loglikelihood user model (H0)                163.115     163.115
##   Loglikelihood unrestricted model (H1)             NA          NA
##                                                                   
##   Akaike (AIC)                                -298.229    -298.229
##   Bayesian (BIC)                              -249.442    -249.442
##   Sample-size adjusted Bayesian (SABIC)       -293.819    -293.819
## 
## Root Mean Square Error of Approximation:
## 
##   RMSEA                                          0.000       0.000
##   90 Percent confidence interval - lower         0.000       0.000
##   90 Percent confidence interval - upper         0.158       0.137
##   P-value H_0: RMSEA <= 0.050                    0.541       0.632
##   P-value H_0: RMSEA >= 0.080                    0.314       0.228
##                                                                   
##   Robust RMSEA                                               0.000
##   90 Percent confidence interval - lower                     0.000
##   90 Percent confidence interval - upper                     0.170
##   P-value H_0: Robust RMSEA <= 0.050                         0.563
##   P-value H_0: Robust RMSEA >= 0.080                         0.318
## 
## Standardized Root Mean Square Residual:
## 
##   SRMR                                           0.006       0.006
## 
## Parameter Estimates:
## 
##   Standard errors                           Robust.sem
##   Information                                 Expected
##   Information saturated (h1) model          Structured
## 
## Regressions:
##                      Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
##   stability ~                                                             
##     asynchrny_Grss      0.206    0.034    6.096    0.000    0.206    0.421
##     nutrients           0.145    0.011   13.381    0.000    0.145    0.707
##     log_balance_f      -0.027    0.012   -2.355    0.019   -0.027   -0.112
##     richness            0.009    0.010    0.892    0.373    0.009    0.042
##     mean_interactn      0.043    0.022    1.907    0.056    0.043    0.077
##     temperature        -0.051    0.011   -4.655    0.000   -0.051   -0.250
##   asynchrony_Gross ~                                                      
##     log_balance_f      -0.075    0.028   -2.701    0.007   -0.075   -0.150
##     nutrients          -0.180    0.024   -7.441    0.000   -0.180   -0.430
##     richness           -0.078    0.021   -3.675    0.000   -0.078   -0.186
##     mean_interactn     -0.272    0.054   -5.034    0.000   -0.272   -0.242
## 
## Intercepts:
##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
##    .stability        -0.394    0.041   -9.713    0.000   -0.394   -2.361
##    .asynchrny_Grss    0.071    0.066    1.082    0.279    0.071    0.208
## 
## Variances:
##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
##    .stability         0.012    0.001   10.476    0.000    0.012    0.424
##    .asynchrny_Grss    0.075    0.008    8.996    0.000    0.075    0.644
## 
## R-Square:
##                    Estimate
##     stability         0.576
##     asynchrny_Grss    0.356
```

<div class="figure" style="text-align: center">
<img src="SEM2.png" alt="SEM." width="8000" />
<p class="caption">(\#fig:SEM2)SEM.</p>
</div>

**Figure 13**: Structural equation model (SEM) of the relationship between fundamental balance, asynchrony, richness, species interactions, nutrients, temperature and temporal stability.  

**Model Fit **

Chi-Square Test: The model test statistic for the user model (𝜒2=0.661,df=1, p=0.416) suggests that the model fits the data well, as the p-value is not significant. This indicates no significant difference between the observed and predicted covariance matrices.

Comparative Fit Index (CFI) and Tucker-Lewis Index (TLI): Both indices are very close to 1 (CFI = 1.000, TLI = 1.012), indicating excellent model fit.

RMSEA: The RMSEA is 0, with a 90% confidence interval from 0 to 0.158. The low RMSEA, along with p-values for tests of close fit (RMSEA ≤ 0.05), further supports a well-fitting model.

SRMR: The SRMR value of 0.006 indicates a good fit, as values below 0.08 are generally acceptable.


**Regression Paths**

**Predictors of Stability**

*Asynchrony_Gross*: Positively associated with stability (β=0.206, p<0.001, standardized = 0.421), suggesting that greater asynchrony among species contributes to higher stability.

*Nutrients*: Significant positive association with stability (β=0.145,p<0.001, standardized = 0.707), indicating that increased nutrient levels promote stability.

*Log(Balance)*: A small but significant negative association (β=−0.027, p=0.019, standardized = -0.112), suggesting that greater balance in response diversity slightly decreases stability.

*Richness*: Not a significant predictor (β=0.009, p=0.373).

*Mean Interaction*: Approaches significance (β=0.043, p=0.056), suggesting a potential weak positive influence on stability.

*Temperature*: Shows a significant negative association (β=−0.051, p<0.001, standardized = -0.250), indicating that higher temperatures tend to reduce stability.


**Predictors of Asynchrony_Gross**

*Log(Balance)*: Shows a negative relationship with asynchrony (β=−0.075, p=0.007), suggesting that higher balance is associated with lower asynchrony among species.

*Nutrients*: Significant negative effect (β=−0.180, p<0.001, standardized = -0.430), indicating that higher nutrient levels reduce species asynchrony.

*Richness*: Negatively associated with asynchrony (β=−0.078, p<0.001), implying that greater species richness leads to less asynchrony.

*Mean Interaction*: A strong negative association with asynchrony (β=−0.272, p<0.001), suggesting that higher mean interaction values reduce species asynchrony.

**Variance and R-Squared**


Overall, the model suggests that stability is positively influenced by asynchrony and nutrient levels, while it decreases with higher temperatures and increased balance in species responses. Asynchrony itself is reduced by increased nutrients, richness, balance, and mean interaction levels, highlighting complex interactions between community structure and environmental factors that influence ecosystem stability.
