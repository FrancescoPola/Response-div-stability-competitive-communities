---
title: "Multifarious response diversity experiments"
author: "Francesco Polazzo, Til Hämmig, Owen L. Petchey, Frank Pennekamp"
date: "31 October, 2024"
output:
  bookdown::html_document2:
    toc: true
    toc_float:
      collapsed: true
      smooth_scroll: true
    code_folding: hide
    keep_md: yes
editor_options: 
  markdown: 
    wrap: 72
---




# Intention 

The goal of this document is to calculate the growth rate (r) and the carrying capacity (K) of 6 species of ciliates 
that we grew individually in a replicated (n = 3) factorial experiment with 5 temperatures (18, 21, 24, 26, and 28), 5 nutrients levels and their combinations (= 25 treatments) for 3 weeks. 
The calculated r and K are then going to be used as the traits to calculate the response diversity of communities composed of 2, 3, and 4 species to all possible changes in temperature and nutrients. The calculated response diversity will inform us on which communities will be used in the following experimental step.
We are going to calculate the growth rate as the slope of the regression of ln(Nt) where: ln is natural log and Nt is the population density at time t, during the period of exponential growth. 
We are going to set initially the period of exponential growth as the first 6 days, but we are going to visually check whether this choice is correct. 
K will be calculated as the highest population biomass for each population during the experiment.


Let's start loading the data set and creating a subset for calculating r
The data set loaded here is a reanalysis of monoculture data, where different bemovi settings have been used to stay consistent between poly and monoculture






<div class="figure" style="text-align: center">
<img src="Analysis_multifarious_RD_final_V2_new_files/figure-html/time_series_spp-1.png" alt="Time series of species densities across the treatments."  />
<p class="caption">(\#fig:time_series_spp)Time series of species densities across the treatments.</p>
</div>


Calculate K and r. For some species in some environmental conditions, different lengths of the time series have been used to calculate intrinsic rate of growth



Visual inspection of exponential growth phase 



<div class="figure" style="text-align: center">
<img src="Analysis_multifarious_RD_final_V2_new_files/figure-html/reg_all-1.png" alt="Species densities across the treatments with the regression lines used to calculate the intrinsic rate of growth (r)."  />
<p class="caption">(\#fig:reg_all)Species densities across the treatments with the regression lines used to calculate the intrinsic rate of growth (r).</p>
</div>

## GAMs to fit responde surface 

We focus on the intrinsic rate of growth (r). This is because the intrisic rate of growth has been shown to be a better predictor of community temporal stability than carrying capacity (K) (Ross et al 2023) [https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/2041-210X.14087]. 
We are going to fit response surfaces using the calculated r for all species using GAMs. Then we are also going to use r as the species' trait to calculate potential response diversity, as well as response diversity when the trajecotry of the environmental change is known. The rationale is that r is likely of more relevance if the environmental change of interest occurs rapidly, since r provides information on a population's ability to rapidly bounce back after disturbance. In the upcoming experiment, we will have temperature fluctuating relatively fast, and so we decide now to focus on r. 

Use GAMs to fit response surface of r and K 




Create surface plots



We now check how the GAMs surfaces (r) look compared to the measured densities.

Checking predictions
<div class="figure" style="text-align: center">
<img src="Analysis_multifarious_RD_final_V2_new_files/figure-html/surface_Dexi-1.png" alt="Measured density values of Dexiostoma in the different treatments (a) vs fitted surface of growth rate (b)."  />
<p class="caption">(\#fig:surface_Dexi)Measured density values of Dexiostoma in the different treatments (a) vs fitted surface of growth rate (b).</p>
</div>


<div class="figure" style="text-align: center">
<img src="Analysis_multifarious_RD_final_V2_new_files/figure-html/surface_colp-1.png" alt="Measured density values of Colpidium in the different treatments (a) vs fitted surface of growth rate (b)."  />
<p class="caption">(\#fig:surface_colp)Measured density values of Colpidium in the different treatments (a) vs fitted surface of growth rate (b).</p>
</div>



<div class="figure" style="text-align: center">
<img src="Analysis_multifarious_RD_final_V2_new_files/figure-html/surface_loxo-1.png" alt="Measured density values of Loxocephalus in the different treatments (a) vs fitted surface of growth rate (b)."  />
<p class="caption">(\#fig:surface_loxo)Measured density values of Loxocephalus in the different treatments (a) vs fitted surface of growth rate (b).</p>
</div>


<div class="figure" style="text-align: center">
<img src="Analysis_multifarious_RD_final_V2_new_files/figure-html/surface_paramecium-1.png" alt="Measured density values of Paramecium in the different treatments (a) vs fitted surface of growth rate (b)."  />
<p class="caption">(\#fig:surface_paramecium)Measured density values of Paramecium in the different treatments (a) vs fitted surface of growth rate (b).</p>
</div>

<div class="figure" style="text-align: center">
<img src="Analysis_multifarious_RD_final_V2_new_files/figure-html/surface_spiro-1.png" alt="Measured density values of Spirostotum in the different treatments (a) vs fitted surface of growth rate (b)."  />
<p class="caption">(\#fig:surface_spiro)Measured density values of Spirostotum in the different treatments (a) vs fitted surface of growth rate (b).</p>
</div>


<div class="figure" style="text-align: center">
<img src="Analysis_multifarious_RD_final_V2_new_files/figure-html/surface_tetra-1.png" alt="Measured density values of Tetrahymena in the different treatments (a) vs fitted surface of growth rate (b)."  />
<p class="caption">(\#fig:surface_tetra)Measured density values of Tetrahymena in the different treatments (a) vs fitted surface of growth rate (b).</p>
</div>



# Response diversity with known direction of environmental change

Now we calculate RD with known trajecory of environmental change, i.e. the one we are going to apply in the experiment. 

- Create a data set with environmental conditions that we may use in the experiment and calculate RD knowing the trajecotry of the environmental change.

- fluctuating temperature x 3 fixed nutrients = 9 treatments

## Temperature fluctuations

For the experiment we used three different temperature regimes, all with same magnitude but different mean temperatures: 18-21, 22-25, 25-28. Temperature stayed at lower end of the range for three days, and then transitioned over 24 h to the higher end, where it stayed for another three days before transitioning back. The experiment lasted for 60 days.

<div class="figure" style="text-align: center">
<img src="Analysis_multifarious_RD_final_V2_new_files/figure-html/experiment-1.png" alt="Visual representation of the experimental design."  />
<p class="caption">(\#fig:experiment)Visual representation of the experimental design.</p>
</div>


## Get the slopes (change in species performance from Temperature 1 to Temperature 2 and vice versa)




## Calculate diveregence for all possible compositions


<div class="figure" style="text-align: center">
<img src="Analysis_multifarious_RD_final_V2_new_files/figure-html/communities_all-1.png" alt="Community compositions that were used in the experiment to create, for each richness level, a gradient of response diversity. Letter represent different species: C = Colpidium, S = Spirostomum, D = Dexiostoma, P = Paramecium, L = Loxocephalus"  />
<p class="caption">(\#fig:communities_all)Community compositions that were used in the experiment to create, for each richness level, a gradient of response diversity. Letter represent different species: C = Colpidium, S = Spirostomum, D = Dexiostoma, P = Paramecium, L = Loxocephalus</p>
</div>


## Calculate diveregence

Communities employed in each environmental treatment were selected prior to the community experiment based on mono culture response experiments. While analyzing Community data using the BEMOVI R package, settings had to be changed (threshold and minimal particle size) to reduce processing time to a manageable level. To stay consistent between mono culture and community experiment (and to ensure correct species identification), we reanalyzed mono culture data. This resulted in slightly different response surfaces, causing some values of divergence to change. Therefore, the gradient of divergence has changed for some environmental treatments. 
In the plot below you can see the compositions used in the experiment and their response diversity for each enviromental treatment and richness level.

<div class="figure" style="text-align: center">
<img src="Analysis_multifarious_RD_final_V2_new_files/figure-html/communities-1.png" alt="Community compositions that were used in the experiment to create, for each richness level, a gradient of response diversity. Letter represent different species: C = Colpidium, S = Spirostomum, D = Dexiostoma, P = Paramecium, L = Loxocephalus"  />
<p class="caption">(\#fig:communities)Community compositions that were used in the experiment to create, for each richness level, a gradient of response diversity. Letter represent different species: C = Colpidium, S = Spirostomum, D = Dexiostoma, P = Paramecium, L = Loxocephalus</p>
</div>






<!-- ##Check divergence -->

<!-- ###Get surface slices -->

<!-- ```{r } -->




<!-- new_data <- expand_grid(temperature = seq(18, 28, by= 0.02), -->
<!--                         nutrients = rep(2)) -->

<!-- get_check_plot<-function(nutrient_level){ -->

<!--   new_data <- expand_grid(temperature = seq(18, 28, by= 0.02), -->
<!--                         nutrients = rep(nutrient_level)) -->
<!--   num_rows <- nrow(new_data) -->

<!-- pred_slice <- data.frame(matrix(nrow = num_rows, ncol = 0)) -->

<!-- for (j in 1:length(new_nested_gams$species)) { -->
<!--   predictions <- unlist(map(nested_gams$gams[j], ~ predict(.x, newdata = new_data))) -->
<!--   pred_slice <- bind_rows(pred_slice, data.frame(prediction = predictions, species = new_nested_gams$species[j])) -->
<!-- } -->

<!-- pred_slice <- na.omit(pred_slice) -->

<!-- pred_slice<-cbind(pred_slice,new_data) -->



<!-- plot<-ggplot(data=pred_slice,aes(x=temperature,y=prediction,colour=species))+ -->
<!--   #annotate("rect", xmin = 25, xmax = 28, ymin = -0.5, ymax = 1, -->
<!--            #alpha = .4,fill = "grey")+ -->
<!--   geom_line(linewidth=1.5)+ -->
<!--   scale_color_brewer(palette = "Dark2",guide=NULL)+ -->
<!--   ylab(expression("Growth rate ["* day^{-1} *  "]"))+ -->
<!--   xlab("Temperature [°C]")+ -->
<!--   scale_x_continuous(breaks = seq(18, 28, by = 1))+ -->
<!--   theme(axis.title.x=element_text(size=16), -->
<!--         axis.title.y = element_text(size = 16))+ -->
<!--   ylim(-1.5,1)+ -->
<!--   #geom_vline(xintercept = c(18, 21), linetype = "dashed", color = "black")+ -->
<!--   geom_vline(xintercept = c(22, 25), linetype = "dashed", color = "black") -->
<!-- #geom_vline(xintercept = c(25.05, 28), color = "black")+ -->
<!--   #labs(title = paste("nutrient level",nutrient_level)) -->


<!--   return(plot) -->
<!-- } -->

<!-- n_1<-get_check_plot(0.1) -->
<!-- n_2<-get_check_plot(0.35) -->
<!-- n_5<-get_check_plot(0.75) -->

<!-- n_1+n_2+n_5 -->







<!-- ``` -->

<!-- # Figures Poster -->


<!-- ```{r} -->

<!-- df_int <- gather(df_slopes%>%dplyr::filter(nutrients=="0.35 g/L",temperature=="22-25 °C"), key = "source", value = "values", maxT_y, minT_y) -->
<!-- df_int$source[df_int$source=="maxT_y"]<-25 -->
<!-- df_int$source[df_int$source=="minT_y"]<-22 -->


<!-- simple_slopes<-ggplot(data=df_int,aes(y=values,x=as.numeric(source),color=species,group=species))+ -->
<!--   geom_vline(xintercept = c(22, 25), linetype = "dashed", color = "black")+ -->
<!--   geom_point(size=3)+geom_line(linewidth=1.5)+ -->
<!--   ylim(-1.5,1)+xlim(21.5,25.5)+ -->
<!--   scale_color_brewer(palette = "Dark2")+ -->
<!--   ylab("")+xlab("Temperature [°C]")+theme(axis.title.x=element_text(size=16), -->
<!--         axis.title.y = element_text(size = 16)) -->
<!--         #legend.direction = "horizontal" ) -->

<!-- slopes_plot<-n_2+simple_slopes +plot_layout(widths = c(2, 1)) -->



<!-- legend <- cowplot::get_legend(simple_slopes) -->

<!-- # Arrange the legend horizontally using plot_grid -->
<!-- horizontal_legend <- cowplot::plot_grid(legend) -->

<!-- # Save the plot as a PDF -->
<!-- ggsave("slopes_poster.pdf", plot = slopes_plot, width = 10, height = 6) -->

<!-- ggsave("legend.pdf", plot = horizontal_legend, width = 10, height = 6) -->

<!-- ``` -->


<!-- ###stability mock plot -->


<!-- ```{r} -->
<!-- days_ts<-1:60 -->
<!-- sp1<-sin(days_ts*0.075)+rnorm(60,0,0.2)+2 -->
<!-- sp2<-sin(0.075*days_ts+3.14)+rnorm(60,0,0.2)+2 -->
<!-- mock_ts<-data.frame(sp1,sp2) -->
<!-- mock_ts<-gather(mock_ts,key="species",value="biomass",sp1,sp2) -->
<!-- mock_ts<-cbind(mock_ts,days_ts) -->

<!-- tot_biom<-mock_ts%>%group_by(days_ts)%>%summarize(biomass=sum(biomass)) -->
<!-- tot_biom<-cbind(tot_biom,species=rep("tot_biom")) -->

<!-- mock_ts<-rbind(mock_ts,tot_biom) -->

<!-- mock_ts_plot<-ggplot(data=mock_ts,aes(x=days_ts,y=biomass,color=species))+ -->
<!--   geom_line(linewidth=c(1.5))+ -->
<!--   scale_color_manual(values=c("grey70","grey80","black"), -->
<!--                      labels=c("species 1", "species 2", "total biomass"))+ -->
<!--   ylab("Biomass")+xlab("Time")+ -->
<!--   theme( -->
<!--     axis.text.y=element_blank(), -->
<!--     axis.ticks.y=element_blank(), -->
<!--     axis.title.y=element_text(size=16), -->
<!--     axis.title.x=element_text(size=16), -->
<!--     legend.title = element_blank(),        -->
<!--     legend.text = element_text(size=14), -->
<!--     legend.position ="top" -->
<!--   ) -->

<!-- mock_CV<-mock_ts%>%group_by(species)%>%summarize(CV=sd(biomass)/mean(biomass)) -->

<!-- mock_CV_plot<-ggplot(data=mock_CV,aes(y=1/CV,x=species,fill=species))+geom_col()+ -->
<!--   scale_fill_manual(values=c("grey70","grey80","black"), -->
<!--                      labels=c("species 1", "species 2", "total biomass"))+ -->
<!--   ylab(expression("Temporal stability ["* CV^{-1} *  "]"))+xlab("Species")+ -->
<!--   scale_x_discrete(labels=c("species 1", "species 2", "total biomass")) +   -->
<!--   theme( -->
<!--     axis.text.y=element_blank(), -->
<!--     axis.ticks.y=element_blank(), -->
<!--     axis.title.y=element_text(size=16), -->
<!--     axis.title.x=element_blank(), -->
<!--     legend.position="none", -->
<!--   ) -->

<!-- mock_plot_1<-mock_ts_plot+mock_CV_plot+plot_layout(widths = c(2, 1)) -->
<!-- ggsave("mock_plot.pdf", plot = mock_plot_1, width = 10, height = 6) -->

<!-- ## Mock plot low magnitude -->

<!-- days_ts<-1:60 -->
<!-- sp1<-0.5*sin(days_ts*0.075)+rnorm(60,0,0.2)+2 -->
<!-- sp2<-0.5*sin(0.075*days_ts+1)+rnorm(60,0,0.2)+2 -->
<!-- mock_ts<-data.frame(sp1,sp2) -->
<!-- mock_ts<-gather(mock_ts,key="species",value="biomass",sp1,sp2) -->
<!-- mock_ts<-cbind(mock_ts,days_ts) -->

<!-- tot_biom<-mock_ts%>%group_by(days_ts)%>%summarize(biomass=sum(biomass)) -->
<!-- tot_biom<-cbind(tot_biom,species=rep("tot_biom")) -->

<!-- mock_ts<-rbind(mock_ts,tot_biom) -->

<!-- mock_ts_plot<-ggplot(data=mock_ts,aes(x=days_ts,y=biomass,color=species))+ -->
<!--   geom_line(linewidth=c(1.5))+ -->
<!--   scale_color_manual(values=c("grey70","grey80","black"), -->
<!--                      labels=c("species 1", "species 2", "total biomass"))+ -->
<!--   ylab("Biomass")+xlab("Time")+ -->
<!--   theme( -->
<!--     axis.text.y=element_blank(), -->
<!--     axis.ticks.y=element_blank(), -->
<!--     axis.title.y=element_text(size=16), -->
<!--     axis.title.x=element_text(size=16), -->
<!--     legend.title = element_blank(),        -->
<!--     legend.text = element_text(size=14), -->
<!--     legend.position ="top" -->
<!--   ) -->

<!-- mock_CV<-mock_ts%>%group_by(species)%>%summarize(CV=sd(biomass)/mean(biomass)) -->



<!-- mock_CV_plot<-ggplot(data=mock_CV,aes(y=1/CV,x=species,fill=species))+geom_col()+ -->
<!--   scale_fill_manual(values=c("grey70","grey80","black"), -->
<!--                      labels=c("species 1", "species 2", "total biomass"))+ -->
<!--   ylab(expression("Temporal stability ["* CV^{-1} *  "]"))+xlab("Species")+ -->
<!--   scale_x_discrete(labels=c("species 1", "species 2", "total biomass")) +   -->
<!--   theme( -->
<!--     axis.text.y=element_blank(), -->
<!--     axis.ticks.y=element_blank(), -->
<!--     axis.title.y=element_text(size=16), -->
<!--     axis.title.x=element_blank(), -->
<!--     legend.position="none", -->
<!--   ) -->

<!-- mock_plot_2<-mock_ts_plot+mock_CV_plot+plot_layout(widths = c(2, 1)) -->
<!-- ``` -->





<!-- ##surface plot -->
<!-- ```{r} -->
<!-- library(RColorBrewer) -->
<!-- library(grid) -->
<!-- library(gridExtra)  -->
<!-- color_sp<-brewer.pal(5, "Dark2") -->
<!-- s2d_plots <- list() -->

<!-- for (i in seq_along(new_nested_gams$gams)) { -->
<!--  species_name <- new_nested_gams$species[i] -->
<!--  df<-dplyr::filter(rates,species==species_name) -->
<!--  col<-color_sp[i] -->

<!-- surface_2d<-ggplot(data=df,aes(x=temperature,y=nutrients,fill=predicted))+ -->
<!--   geom_raster()+ -->
<!--   scale_fill_gradient(high=col, low="white",guide = "none")+ -->
<!--   #guides(fill = guide_colorbar(title = "", title.position = "top",  -->
<!--     #                           direction = "vertical", barwidth =                                    2,barheight = 5,frame.colour = "black"))+ -->
<!--   geom_contour(data=df,aes(x=temperature,y=nutrients,z=predicted),color = "grey40", linetype = "solid", bins=11)+ -->
<!--   scale_x_continuous(breaks = seq(18, 28, by = 2))+ -->
<!--    theme(strip.background = element_blank(),   -->
<!--         strip.text = element_text(size = 10, color = "black"), -->
<!--         strip.text.x = element_text(margin = margin(b = 10)), -->
<!--         strip.text.y = element_text(margin = margin(l = 10)), -->
<!--         axis.title.y = element_blank(), -->
<!--         axis.title.x = element_blank(), -->
<!--         legend.position = "right",   -->
<!--         legend.direction = "vertical",aspect.ratio = 1, -->
<!--         legend.key.height = unit(0.3, "cm"), -->
<!--         legend.key.width = unit(0.5, "cm"), -->
<!--         legend.key.size = unit(0.5,"cm"))+ -->
<!--   labs(title=species_name,fill="")+ -->
<!--   geom_segment(data = data.frame(y = c(0.01, 0.01), x = c(25, 25),  -->
<!--                                  xend = c(28, 28),  yend = c(0.01, 0.01)),  -->
<!--                aes(x, y, xend = xend, yend = yend), -->
<!--                inherit.aes = FALSE, linetype = 1)+ -->
<!--   geom_point(data=data.frame(x=c(25,28),y=c(0.01,0.01)),aes(x=x,y=y),inherit.aes = FALSE) -->




<!--  # Save the plot in the list -->
<!--   s2d_plots[[species_name]] <- surface_2d -->

<!-- } -->


<!-- s_dexiostoma <- s2d_plots[["Dexiostoma"]] -->
<!-- s_colpidium <- s2d_plots[["Colpidium"]] -->
<!-- s_loxocephalus <- s2d_plots[["Loxocephalus"]] -->
<!-- s_paramecium <- s2d_plots[["Paramecium"]] -->
<!-- s_spirostotum <- s2d_plots[["Spirostomum"]] -->



<!-- #first change #guides() above -->

<!-- legend_spir <- cowplot::get_legend(s_spirostotum) -->
<!-- legend_dexi <- cowplot::get_legend(s_dexiostoma) -->
<!-- legend_colp <- cowplot::get_legend(s_colpidium) -->
<!-- legend_loxo <- cowplot::get_legend(s_loxocephalus) -->
<!-- legend_para <- cowplot::get_legend(s_paramecium) -->

<!-- legends<-ggarrange(legend_spir,legend_dexi,legend_colp,legend_loxo,legend_para,nrow=2,ncol=3) -->

<!-- legends<-annotate_figure(legends, -->
<!--                          top=text_grob("growth rate",size=15,vjust=1,hjust=1.3)) -->


<!-- #run first surface plots without the guides on (don't rerun legends) -->

<!-- fig1A<-ggarrange(s_dexiostoma,s_loxocephalus,s_paramecium,s_spirostotum,s_colpidium,legends,common.legend=F,nrow=2,ncol=3,align="hv")+ -->
<!--   theme(plot.margin = margin(0.1,0.1,0.1,1.5, "cm"))  -->



<!-- fig1A<-annotate_figure(fig1A,bottom=text_grob("temperature [°C]",size=15), -->
<!--                        left=text_grob("nutrients [g/L]",size=15,vjust=2,rot=90)) -->
<!-- ``` -->

