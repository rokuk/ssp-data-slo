Klimatski podatki
================
Rok Kuk
2023-06-21

## Uvoz knjižnic

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.2     ✔ readr     2.1.4
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.0
    ## ✔ ggplot2   3.4.2     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.2     ✔ tidyr     1.3.0
    ## ✔ purrr     1.0.1     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(cowplot)
```

    ## 
    ## Attaching package: 'cowplot'
    ## 
    ## The following object is masked from 'package:lubridate':
    ## 
    ##     stamp

``` r
library(here)
```

    ## here() starts at /Users/rokuk/Documents/Work/BFUL/SSP-data-slo

``` r
library(ggokabeito)
library(colorspace)
library(ggplot2)
library(scales)
```

    ## 
    ## Attaching package: 'scales'
    ## 
    ## The following object is masked from 'package:purrr':
    ## 
    ##     discard
    ## 
    ## The following object is masked from 'package:readr':
    ## 
    ##     col_factor

## Uvoz podatkov

``` r
iam <- read_csv(here("data", "IAM", "SSP_IAM_V2_201811.csv")) %>%
    filter(REGION == "World") %>%
    mutate(SSP = str_sub(SCENARIO, 1, 4),
           MITIGATION = str_sub(SCENARIO, 6))
```

    ## Rows: 84353 Columns: 16
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (5): MODEL, SCENARIO, REGION, VARIABLE, UNIT
    ## dbl (11): 2005, 2010, 2020, 2030, 2040, 2050, 2060, 2070, 2080, 2090, 2100
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

## Cleanup

``` r
ssp1 <- filter(iam, SSP == "SSP1" & MODEL == "IMAGE")
ssp2 <- filter(iam, SSP == "SSP2" & MODEL == "MESSAGE-GLOBIOM")
ssp3 <- filter(iam, SSP == "SSP3" & MODEL == "AIM/CGE")
ssp4 <- filter(iam, SSP == "SSP4" & MODEL == "GCAM4")
ssp5 <- filter(iam, SSP == "SSP5" & MODEL == "REMIND-MAGPIE")

ssp1_notmarker <- filter(iam, SSP == "SSP1" & MODEL != "IMAGE") 
ssp2_notmarker <- filter(iam, SSP == "SSP2" & MODEL != "MESSAGE-GLOBIOM") 
ssp3_notmarker <- filter(iam, SSP == "SSP3" & MODEL != "AIM/CGE") 
ssp4_notmarker <- filter(iam, SSP == "SSP4" & MODEL != "GCAM4") 
ssp5_notmarker <- filter(iam, SSP == "SSP5" & MODEL != "REMIND-MAGPIE") 

marker <- rbind(ssp1, ssp2, ssp3, ssp4, ssp5)
marker_baseline <- filter(marker, MITIGATION == "Baseline") %>%
    mutate(MITIGATION = "izhodišče")
marker_notbaseline <- filter(marker, MITIGATION != "Baseline") %>%
    mutate(MITIGATION = paste(as.integer(MITIGATION) / 10, "W/m^2")) %>%
    mutate(MITIGATION = replace(MITIGATION, MITIGATION == "6 W/m^2", "6.0 W/m^2"))
marker <- rbind(marker_baseline, marker_notbaseline)

marker$SSP <- factor(marker$SSP, levels = c("SSP1", "SSP2", "SSP3", "SSP4", "SSP5")) %>%
    fct_recode("SSP1\nTrajnostni razvoj"="SSP1", "SSP2\nSrednja pot"="SSP2", "SSP3\nTekmovanje regij"="SSP3", "SSP4\nNeenakost"="SSP4", "SSP5\nFosilni razvoj"="SSP5")

notmarker <- rbind(ssp1_notmarker, ssp2_notmarker, ssp3_notmarker, ssp4_notmarker, ssp5_notmarker)

co2emissions_notmarker <- filter(notmarker, VARIABLE == "Emissions|CO2") %>%
    pivot_longer(cols=starts_with("2"), names_to="leto", values_to="izpusti") %>%
    mutate(leto = as.integer(leto), izpusti = as.double(izpusti)) %>%
    drop_na()

co2emissions <- filter(marker, VARIABLE == "Emissions|CO2") %>%
    pivot_longer(cols=starts_with("2"), names_to="leto", values_to="izpusti") %>%
    mutate(leto = as.integer(leto), izpusti = as.double(izpusti)) %>%
    drop_na()

energy <- filter(marker, VARIABLE == "Primary Energy|Biomass" | VARIABLE == "Primary Energy|Coal" | VARIABLE == "Primary Energy|Gas" | VARIABLE == "Primary Energy|Non-Biomass Renewables" | VARIABLE == "Primary Energy|Nuclear" | VARIABLE == "Primary Energy|Oil") %>%
    pivot_longer(cols=starts_with("2"), names_to="leto", values_to="energija") %>%
    mutate(leto = as.integer(leto), energija = as.double(energija)) %>%
    drop_na()

energylevels <- filter(energy, MITIGATION == "izhodišče" & SSP == "SSP5\nFosilni razvoj" & leto == 2100) %>%
  arrange(energija)
energy$VARIABLE <- factor(energy$VARIABLE, levels=energylevels$VARIABLE) %>%
    fct_recode("Premog"="Primary Energy|Coal", "Plin"="Primary Energy|Gas", "Nafta"="Primary Energy|Oil", "Obnovljivi viri\n(brez biomase)"="Primary Energy|Non-Biomass Renewables", "Biomasa"="Primary Energy|Biomass", "Jedrska"="Primary Energy|Nuclear")
energybaseline <- filter(energy, MITIGATION == "izhodišče")

temps_notmarker <- filter(notmarker, VARIABLE == "Diagnostics|MAGICC6|Temperature|Global Mean") %>%
    pivot_longer(cols=starts_with("2"), names_to="leto", values_to="temps") %>%
    mutate(leto = as.integer(leto), temps = as.double(temps)) %>%
    drop_na()

temps <- filter(marker, VARIABLE == "Diagnostics|MAGICC6|Temperature|Global Mean") %>%
    pivot_longer(cols=starts_with("2"), names_to="leto", values_to="temps") %>%
    mutate(leto = as.integer(leto), temps = as.double(temps)) %>%
    drop_na()

baseline_scenarios <- c("SSP1-Baseline", "SSP2-Baseline", "SSP3-Baseline", "SSP4-Baseline", "SSP5-Baseline")

mitigation_labels <- c("izhodišče", expression(paste("6.0 W/", m^2)), expression(paste("4.5 W/", m^2)), expression(paste("3.4 W/", m^2)), expression(paste("2.6 W/", m^2)), expression(paste("1.9 W/", m^2)))

labels_emissions <- full_join(filter(co2emissions, leto == 2100 & MITIGATION == "izhodišče"),
                       tibble(
                           SCENARIO = baseline_scenarios,
                           nudge_y = c(0, -4, 4, 0, 0)))
```

    ## Joining with `by = join_by(SCENARIO)`

``` r
labels_emissions$SCENARIO <- factor(labels_emissions$SCENARIO, levels=baseline_scenarios) %>%
    fct_recode("SSP1"="SSP1-Baseline", 
               "SSP2"="SSP2-Baseline", 
               "SSP3"="SSP3-Baseline", 
               "SSP4"="SSP4-Baseline", 
               "SSP5"="SSP5-Baseline")

labels_temps <- full_join(filter(temps, leto == 2100 & MITIGATION == "izhodišče"),
                       tibble(
                           SCENARIO = baseline_scenarios,
                           nudge_y = c(0, -0.15, 0.2, 0.17, 0)))
```

    ## Joining with `by = join_by(SCENARIO)`

``` r
labels_temps$SCENARIO <- factor(labels_temps$SCENARIO, levels=baseline_scenarios) %>%
    fct_recode("SSP1"="SSP1-Baseline", 
               "SSP2"="SSP2-Baseline", 
               "SSP3"="SSP3-Baseline", 
               "SSP4"="SSP4-Baseline", 
               "SSP5"="SSP5-Baseline")
```

## Grafi

### Emisije CO2

``` r
labelsize <- 7

ggplot(data = filter(co2emissions, MITIGATION == "izhodišče"), mapping = aes(x = leto, y = izpusti / 1000, color = SSP)) +
    geom_line(linewidth=0.8) + 
    geom_text(
        data = labels_emissions,
        mapping = aes(x = leto + 7, y = izpusti/1000 + nudge_y, label = SCENARIO),
        size = unit(labelsize*0.3, "pt")) +
    geom_line(data = filter(co2emissions_notmarker, MITIGATION == "Baseline"), mapping = aes(x = leto, y = izpusti / 1000, color = SSP, group = interaction(SCENARIO, MODEL)), linewidth=0.3) +
    theme_half_open() +
    background_grid(major = "y", size.major = 0.2) + 
    scale_color_manual(values=c(darken("#F0E442", 0.03), darken("#F0E442", 0.03), "#56B4E9", "#56B4E9",  "#009E73", "#009E73", "#E69F00", "#E69F00", "#0072B2","#0072B2")) +
    xlab("leto") +
    ylab(expression(paste("Gt C", O[2], " / leto", sep=""))) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 150)) +
    scale_x_continuous(limits = c(1995, 2110), breaks = c(2000, 2025, 2050, 2075, 2100)) +
    expand_limits(y=c(0, 0)) +
    labs(title = expression(bold(paste("Neto emisije C", O[2]), sep="")), caption="GRAF: Rok Kuk\nPODATKI: SSP Public Database (Version 2.0)\n VIR: Riahi et al. (2017), van Vuuren et al. (2017), Fricko et al. (2017),\nFujimori et al. (2017), Calvin et al. (2017), Kriegler et al. (2017)", subtitle = "Izhodiščni scenariji - vsi") +
    theme(plot.background = element_rect(fill="#FFFFFF"),
          legend.title = element_blank(),
          legend.margin = margin(0, 0, 0, 0),
          text = element_text(size=labelsize),
          axis.text = element_text(size=labelsize),
          axis.title = element_text(size=labelsize),
          title = element_text(size=labelsize*1.5),
          plot.caption = element_text(size=labelsize*0.6, color = "#666666", margin=margin(3,0,0,0)),
          plot.caption.position = "plot") +
    annotate("rect", xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=0, alpha=0.1) +
    guides(color="none")
```

<img src="klima_files/figure-gfm/unnamed-chunk-4-1.png" style="display: block; margin: auto;" />

``` r
ggsave2(here("output", "pdf", "izpustico2-baseline_all.pdf"), width=8, height=7, units="cm")
ggsave2(here("output", "png", "izpustico2-baseline_all.png"), width=8, height=7, units="cm", dpi=400)
```

``` r
labelsize <- 7

ggplot(data = filter(co2emissions, MITIGATION == "izhodišče"), mapping = aes(x = leto, y = izpusti / 1000, color = SSP)) +
    geom_line() + 
    geom_text(
        data = labels_emissions,
        mapping = aes(x = leto + 7, y = izpusti/1000 + nudge_y, label = SCENARIO),
        size = unit(labelsize*0.3, "pt")) +
    theme_half_open() +
    background_grid(major = "y", size.major = 0.2) + 
    scale_color_manual(values=c(darken("#F0E442", 0.03), "#56B4E9", "#009E73", "#E69F00", "#0072B2")) +
    xlab("leto") +
    ylab(expression(paste("Gt C", O[2], " / leto", sep=""))) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 150)) +
    scale_x_continuous(limits = c(1995, 2110), breaks = c(2000, 2025, 2050, 2075, 2100)) +
    expand_limits(y=c(0, 0)) +
    labs(title = expression(bold(paste("Neto emisije C", O[2]), sep="")), caption="GRAF: Rok Kuk\nPODATKI: SSP Public Database (Version 2.0) / marker scenarios - baseline\n VIR: Riahi et al. (2017), van Vuuren et al. (2017), Fricko et al. (2017),\nFujimori et al. (2017), Calvin et al. (2017), Kriegler et al. (2017)", subtitle = "Izhodiščni scenariji - izbrani") +
    theme(plot.background = element_rect(fill="#FFFFFF"),
          legend.title = element_blank(),
          legend.margin = margin(0, 0, 0, 0),
          text = element_text(size=labelsize),
          axis.text = element_text(size=labelsize),
          axis.title = element_text(size=labelsize),
          title = element_text(size=labelsize*1.5),
          plot.caption = element_text(size=labelsize*0.6, color = "#666666", margin=margin(3,0,0,0)),
          plot.caption.position = "plot") +
    annotate("rect", xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=0, alpha=0.1) +
    guides(color="none")
```

<img src="klima_files/figure-gfm/unnamed-chunk-6-1.png" style="display: block; margin: auto;" />

``` r
ggsave2(here("output", "pdf", "izpustico2-baseline.pdf"), width=8, height=7, units="cm")
ggsave2(here("output", "png", "izpustico2-baseline.png"), width=8, height=7, units="cm", dpi=400)
```

``` r
ggplot(data = co2emissions, mapping = aes(x = leto, y = izpusti / 1000, color = MITIGATION)) +
    geom_line() +
    facet_grid(cols = vars(SSP)) +
    theme_half_open() +
    background_grid(major = "y", size.major = 0.2) +
    scale_color_okabe_ito(breaks = c("izhodišče", "6.0 W/m^2", "4.5 W/m^2", "3.4 W/m^2", "2.6 W/m^2", "1.9 W/m^2"), labels = mitigation_labels) +
    panel_border() +
    xlab("leto") + 
    ylab(expression(paste("Gt C", O[2], " / leto", sep=""))) +
    scale_x_continuous(n.breaks = 4, limits = c(1990, 2110)) +
    labs(color = "Scenarij blaženja", title = expression(bold(paste("Neto emisije C", O[2]), sep="")), caption="GRAF: Rok Kuk\nPODATKI: SSP Public Database (Version 2.0) / marker scenarios\n VIR: Riahi et al. (2017), van Vuuren et al. (2017), Fricko et al. (2017), Fujimori et al. (2017), Calvin et al. (2017), Kriegler et al. (2017)") +
    theme(plot.background = element_rect(fill="#FFFFFF"),
          legend.title = element_text(size=labelsize*1.1),
          legend.margin = margin(0, 0, 0, 0),
          text = element_text(size=labelsize),
          axis.text = element_text(size=labelsize),
          axis.title = element_text(size=labelsize),
          title = element_text(size=labelsize*1.5),
          plot.caption = element_text(size=labelsize*0.6, color = "#666666", margin=margin(0,0,0,0))) +
    annotate("rect", xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=0, alpha=0.1) +
    guides(color = guide_legend(keyheight = 0.5)) 
```

<img src="klima_files/figure-gfm/unnamed-chunk-8-1.png" style="display: block; margin: auto;" />

``` r
ggsave2(here("output", "pdf", "izpustico2.pdf"), width=figwidth, height=figheight, units="cm")
ggsave2(here("output", "png", "izpustico2.png"), width=figwidth, height=figheight, units="cm", dpi=400)
```

``` r
ggplot(data = co2emissions, mapping = aes(x = leto, y = izpusti / 1000, color = SSP)) +
    geom_line() +
    facet_grid(cols = vars(fct_rev(MITIGATION))) +
    theme_half_open() +
    background_grid(major = "y", size.major = 0.2) +
    scale_color_okabe_ito(guide = guide_legend(reverse = TRUE)) +
    panel_border() +
    xlab("leto") + 
    ylab(expression(paste("Gt C", O[2], " / leto", sep=""))) +
    scale_x_continuous(n.breaks = 3, limits = c(1990, 2110), minor_breaks = c(2025, 2075)) +
    labs(color = "Scenarij razvoja", title = expression(bold(paste("Scenariji neto emisij C", O[2]), sep="")), caption="GRAF: Rok Kuk\nPODATKI: SSP Public Database (Version 2.0) / marker scenarios\n VIR: Riahi et al. (2017), van Vuuren et al. (2017), Fricko et al. (2017), Fujimori et al. (2017), Calvin et al. (2017), Kriegler et al. (2017)") +
    theme(plot.background = element_rect(fill="#FFFFFF"),
          legend.title = element_text(size=labelsize*1.1),
          legend.margin = margin(0, 0, 0, 0),
          text = element_text(size=labelsize),
          axis.text = element_text(size=labelsize),
          axis.title = element_text(size=labelsize),
          title = element_text(size=labelsize*1.5),
          plot.caption = element_text(size=labelsize*0.6, color = "#666666", margin=margin(0,0,0,0))) +
    annotate("rect", xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=0, alpha=0.1) +
    guides(color = guide_legend(keyheight = 1.2))
```

<img src="klima_files/figure-gfm/unnamed-chunk-10-1.png" style="display: block; margin: auto;" />

``` r
ggsave2(here("output", "pdf", "izpustico2_2.pdf"), width=figwidth, height=figheight, units="cm")
ggsave2(here("output", "png", "izpustico2_2.png"), width=figwidth, height=figheight, units="cm", dpi=400)
```

### Temperatura

``` r
ggplot(data = filter(temps, MITIGATION=="izhodišče"), mapping = aes(x = leto, y = temps, color = SSP)) +
    geom_line() + 
    geom_text(
        data = labels_temps,
        mapping = aes(x = leto + 7, y = temps + nudge_y, label = SCENARIO),
        size = unit(labelsize*0.3, "pt")) +
    geom_line(data = filter(temps_notmarker, MITIGATION == "Baseline"), mapping = aes(x = leto, y = temps, color = SSP, group = interaction(SCENARIO, MODEL)), linewidth=0.3) +
    theme_half_open() +
    background_grid(major = "y", size.major = 0.2) + 
    scale_color_manual(values=c(darken("#F0E442", 0.03), darken("#F0E442", 0.03), "#56B4E9", "#56B4E9",  "#009E73", "#009E73", "#E69F00", "#E69F00", "#0072B2","#0072B2")) +
    xlab("leto") +
    ylab("\u00B0C nad predindustrijsko ravnjo") +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 6)) +
    scale_x_continuous(limits = c(1995, 2110), breaks = c(2000, 2025, 2050, 2075, 2100)) +
    expand_limits(y=c(0, 0)) +
    labs(title = "Globalna povprečna temperatura", caption="GRAF: Rok Kuk\nPODATKI: SSP Public Database (Version 2.0)\n VIR: Riahi et al. (2017), van Vuuren et al. (2017), Fricko et al. (2017),\nFujimori et al. (2017), Calvin et al. (2017), Kriegler et al. (2017)", subtitle = "Izhodiščni scenariji - vsi") +
    theme(plot.background = element_rect(fill="#FFFFFF"),
          legend.title = element_blank(),
          legend.margin = margin(0, 0, 0, 0),
          text = element_text(size=labelsize),
          axis.text = element_text(size=labelsize),
          axis.title = element_text(size=labelsize),
          title = element_text(size=labelsize*1.5),
          plot.caption = element_text(size=labelsize*0.6, color = "#666666", margin=margin(3,0,0,0)),
          plot.caption.position = "plot") +
    annotate("rect", xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=0, alpha=0.1) +
    guides(color = "none") 
```

<img src="klima_files/figure-gfm/unnamed-chunk-12-1.png" style="display: block; margin: auto;" />

``` r
ggsave2(here("output", "pdf", "temp-baseline_all.pdf"), width=8, height=7, units="cm")
ggsave2(here("output", "png", "temp-baseline_all.png"), width=8, height=7, units="cm", dpi=400)
```

``` r
ggplot(data = filter(temps, MITIGATION=="izhodišče"), mapping = aes(x = leto, y = temps, color = SSP)) +
    geom_line() + 
    geom_text(
        data = labels_temps,
        mapping = aes(x = leto + 7, y = temps + nudge_y, label = SCENARIO),
        size = unit(labelsize*0.3, "pt")) +
    theme_half_open() +
    background_grid(major = "y", size.major = 0.2) + 
    scale_color_manual(values=c(darken("#F0E442", 0.03), "#56B4E9", "#009E73", "#E69F00", "#0072B2")) +
    xlab("leto") +
    ylab("\u00B0C nad predindustrijsko ravnjo") +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 6)) +
    scale_x_continuous(limits = c(1995, 2110), breaks = c(2000, 2025, 2050, 2075, 2100)) +
    expand_limits(y=c(0, 0)) +
    labs(title = "Globalna povprečna temperatura", caption="GRAF: Rok Kuk\nPODATKI: SSP Public Database (Version 2.0) / marker scenarios - baseline\n VIR: Riahi et al. (2017), van Vuuren et al. (2017), Fricko et al. (2017),\nFujimori et al. (2017), Calvin et al. (2017), Kriegler et al. (2017)", subtitle = "Izhodiščni scenariji - izbrani") +
    theme(plot.background = element_rect(fill="#FFFFFF"),
          legend.title = element_blank(),
          legend.margin = margin(0, 0, 0, 0),
          text = element_text(size=labelsize),
          axis.text = element_text(size=labelsize),
          axis.title = element_text(size=labelsize),
          title = element_text(size=labelsize*1.5),
          plot.caption = element_text(size=labelsize*0.6, color = "#666666", margin=margin(3,0,0,0)),
          plot.caption.position = "plot") +
    annotate("rect", xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=0, alpha=0.1) +
    guides(color = "none") 
```

<img src="klima_files/figure-gfm/unnamed-chunk-14-1.png" style="display: block; margin: auto;" />

``` r
ggsave2(here("output", "pdf", "temp-baseline.pdf"), width=8, height=7, units="cm")
ggsave2(here("output", "png", "temp-baseline.png"), width=8, height=7, units="cm", dpi=400)
```

``` r
ggplot(data = temps, mapping = aes(x = leto, y = temps, color = MITIGATION)) +
    geom_line() +
    facet_grid(cols = vars(SSP)) +
    theme_half_open() +
    background_grid(major = "y", size.major = 0.2) +
    scale_color_okabe_ito(breaks = c("izhodišče", "6.0 W/m^2", "4.5 W/m^2", "3.4 W/m^2", "2.6 W/m^2", "1.9 W/m^2"), labels = mitigation_labels) +
    panel_border() +
    xlab("leto") + 
    ylab("\u00B0C nad predindustrijsko ravnjo") +
    scale_x_continuous(n.breaks = 3, limits = c(1990, 2110)) +
    scale_y_continuous(expand = c(0, 0)) +
    expand_limits(y=c(0, 5.3)) +
    labs(color = "Scenarij blaženja", title = "Globalna povprečna temperatura", caption="GRAF: Rok Kuk\nPODATKI: SSP Public Database (Version 2.0) / marker scenarios\n VIR: Riahi et al. (2017), van Vuuren et al. (2017), Fricko et al. (2017), Fujimori et al. (2017), Calvin et al. (2017), Kriegler et al. (2017)") +
    theme(plot.background = element_rect(fill="#FFFFFF"),
          legend.title = element_text(size=labelsize*1.1),
          legend.margin = margin(0, 0, 0, 0),
          text = element_text(size=labelsize),
          axis.text = element_text(size=labelsize),
          axis.title = element_text(size=labelsize),
          title = element_text(size=labelsize*1.5),
          plot.caption = element_text(size=labelsize*0.6, color = "#666666", margin=margin(0,0,0,0))) +
    guides(color = guide_legend(keyheight = 0.5))
```

<img src="klima_files/figure-gfm/unnamed-chunk-16-1.png" style="display: block; margin: auto;" />

``` r
ggsave2(here("output", "pdf", "temp.pdf"), width=figwidth, height=figheight, units="cm")
ggsave2(here("output", "png", "temp.png"), width=figwidth, height=figheight, units="cm", dpi=400)
```

``` r
ggplot(data = temps, mapping = aes(x = leto, y = temps, color = SSP)) +
    geom_line() +
    facet_grid(cols = vars(fct_rev(MITIGATION))) +
    theme_half_open() +
    background_grid(major = "y", size.major = 0.2) +
    scale_color_okabe_ito() +
    panel_border() +
    xlab("leto") + 
    ylab("\u00B0C nad predindustrijsko ravnjo") +
    scale_x_continuous(n.breaks = 3, limits = c(1990, 2110)) +
    scale_y_continuous(expand = c(0, 0)) +
    expand_limits(y=c(0, 5.3)) +
    labs(color = "Scenarij razvoja", title = "Projekcije globalne povprečne temperature", caption="GRAF: Rok Kuk\nPODATKI: SSP Public Database (Version 2.0) / marker scenarios\n VIR: Riahi et al. (2017), van Vuuren et al. (2017), Fricko et al. (2017), Fujimori et al. (2017), Calvin et al. (2017), Kriegler et al. (2017)") +
    theme(plot.background = element_rect(fill="#FFFFFF"),
          legend.title = element_text(size=labelsize*1.1),
          legend.margin = margin(0, 0, 0, 0),
          text = element_text(size=labelsize),
          axis.text = element_text(size=labelsize),
          axis.title = element_text(size=labelsize),
          title = element_text(size=labelsize*1.5),
          plot.caption = element_text(size=labelsize*0.6, color = "#666666", margin=margin(0,0,0,0))) +
    guides(color = guide_legend(keyheight = 1.2))
```

<img src="klima_files/figure-gfm/unnamed-chunk-18-1.png" style="display: block; margin: auto;" />

``` r
ggsave2(here("output", "pdf", "temp_2.pdf"), width=figwidth, height=figheight, units="cm")
ggsave2(here("output", "png", "temp_2.png"), width=figwidth, height=figheight, units="cm", dpi=400)
```

## Energija

``` r
ggplot(data = energybaseline, mapping = aes(x = leto, y = energija, fill = fct_rev(VARIABLE))) +
    geom_area() +
    facet_grid(cols = vars(SSP)) +
    theme_half_open() +
    background_grid(major = "y", size.major = 0.2) +
    panel_border() +
    xlab("leto") +
    ylab("EJ") +
    labs(title = "Primarna energija", caption="GRAF: Rok Kuk\nPODATKI: SSP Public Database (Version 2.0) / marker scenarios\n VIR: Riahi et al. (2017), van Vuuren et al. (2017), Fricko et al. (2017), Fujimori et al. (2017), Calvin et al. (2017), Kriegler et al. (2017)", subtitle = "Izhodiščni scenariji") +
    scale_fill_okabe_ito() +
    scale_x_continuous(n.breaks = 3, limits = c(1990, 2110)) +
    scale_y_continuous(expand = c(0, 0)) +
    ylim(0, 1830) +
    expand_limits(y=c(0, 0)) +
    theme(plot.background = element_rect(fill="#FFFFFF"),
          legend.title = element_blank(),
          legend.margin = margin(0, 0, 0, 0),
          text = element_text(size=labelsize),
          axis.text = element_text(size=labelsize),
          axis.title = element_text(size=labelsize),
          title = element_text(size=labelsize*1.5),
          plot.caption = element_text(size=labelsize*0.6, color = "#666666", margin=margin(0,0,0,0))) +
    guides(color = guide_legend(keyheight = 0.5))
```

    ## Scale for y is already present.
    ## Adding another scale for y, which will replace the existing scale.

<img src="klima_files/figure-gfm/unnamed-chunk-20-1.png" style="display: block; margin: auto;" />

``` r
ggsave2(here("output", "pdf", "energija-baseline.pdf"), width=16, height=6, units="cm")
ggsave2(here("output", "png", "energija-baseline.png"), width=16, height=6, units="cm", dpi=400)
```

``` r
ggplot(data = energy, mapping = aes(x = leto, y = energija, fill = fct_rev(VARIABLE))) +
    geom_area() +
    facet_grid(cols = vars(SSP), rows = vars(MITIGATION)) +
    theme_half_open() +
    background_grid(major = "y", size.major = 0.2) +
    panel_border() +
    xlab("leto") +
    ylab("EJ") +
    labs(title = "Primarna energija", caption="GRAF: Rok Kuk\nPODATKI: SSP Public Database (Version 2.0) / marker scenarios\n VIR: Riahi et al. (2017), van Vuuren et al. (2017), Fricko et al. (2017), Fujimori et al. (2017), Calvin et al. (2017), Kriegler et al. (2017)") +
    scale_fill_okabe_ito() +
    scale_x_continuous(n.breaks = 3, limits = c(1990, 2110)) +
    scale_y_continuous(expand = c(0, 0)) +
    ylim(0, 1830) +
    expand_limits(y=c(0, 0)) +
    theme(plot.background = element_rect(fill="#FFFFFF"),
          legend.title = element_blank(),
          legend.margin = margin(0, 0, 0, 0),
          text = element_text(size=labelsize),
          axis.text = element_text(size=labelsize),
          axis.title = element_text(size=labelsize),
          title = element_text(size=labelsize*1.5),
          plot.caption = element_text(size=labelsize*0.6, color = "#666666", margin=margin(0,0,0,0))) +
    guides(color = guide_legend(keyheight = 0.5))
```

    ## Scale for y is already present.
    ## Adding another scale for y, which will replace the existing scale.

<img src="klima_files/figure-gfm/unnamed-chunk-22-1.png" style="display: block; margin: auto;" />

``` r
ggsave2(here("output", "pdf", "energija-mitigation.pdf"), width=16, height=25, units="cm")
ggsave2(here("output", "png", "energija-mitigation.png"), width=16, height=25, units="cm", dpi=400)
```
