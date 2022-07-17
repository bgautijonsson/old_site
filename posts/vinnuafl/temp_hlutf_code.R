model_dat <- d |>  
    filter(rekstrarform != "Alls starfandi", bakgrunnur == "Alls") |>  
    count(dags, tegund, wt = starfandi) |>  
    pivot_wider(names_from = tegund, values_from = n) |>  
    mutate(Heild = Opinbert + Annad) |> 
    filter(year(dags) >= 2008) |>  
    mutate(manudur = month(dags),
           timi = as.numeric(as.factor(dags)),
           hlutf = Opinbert / (Annad + Opinbert)) |> 
    ungroup()

m <- gam(Opinbert ~ s(manudur, bs = "cc") + s(timi, bs = "ad"), 
         data = model_dat, offset = log(Heild), family = nb(), method = "REML")

plot_dat <- emmeans(m, ~ timi + manudur, at = list(timi = model_dat$timi,
                                                   manudur = 1:12)) |> 
    tidy(type = "response") |> 
    group_by(timi) |> 
    mutate(wt = 1 / std.error,
           wt = wt / sum(wt)) |> 
    summarise(response = sum(response * wt)) |> 
    inner_join(
        model_dat,
        by = "timi"
    )


opinbert_dat <- gam(Opinbert ~ s(manudur, bs = "cc") + s(timi, bs = "ad"), data = model_dat,
    family = nb(), method = "REML") |> 
    emmeans(~ timi, at = list(timi = unique(model_dat$timi))) |> 
    tidy() |> 
    inner_join(
        model_dat |> 
            distinct(timi, dags)
    ) |> 
    mutate(change = c(0, diff(estimate)),
           perc_change = exp(change))


alm_dat <- gam(Annad ~ s(manudur, bs = "cc") + s(timi, bs = "ad"), data = model_dat,
                 family = nb(), method = "REML") |> 
    emmeans(~ timi, at = list(timi = unique(model_dat$timi))) |> 
    tidy() |> 
    inner_join(
        model_dat |> 
            distinct(timi, dags)
    ) |> 
    mutate(change = c(0, diff(estimate)),
           perc_change = exp(change))


heild_dat <- gam(Heild ~ s(manudur, bs = "cc") + s(timi, bs = "ad"), data = model_dat,
                    family = nb(), method = "REML") |> 
    emmeans(~ timi, at = list(timi = unique(model_dat$timi))) |> 
    tidy() |> 
    inner_join(
        model_dat |> 
            distinct(timi, dags)
    ) |> 
    mutate(change = c(0, diff(estimate)),
           perc_change = exp(change))

plot_dat2 <- heild_dat |> 
    mutate(tegund = "Heild") |> 
    bind_rows(
        opinbert_dat |> 
            mutate(tegund = "Opinber")
    )

plot_dat2 |> 
    select(dags, tegund,perc_change) |> 
    mutate(perc_change = round(perc_change - 1, 3)) |> 
    pivot_wider(names_from= tegund, values_from = perc_change) |> 
    View()

dates <- c("2009-08-01", "2018-01-01", "2021-01-01")

p <- plot_dat |> 
    ggplot(aes(dags, hlutf)) +  
    geom_vline(xintercept = ymd(dates), lty = 2, alpha = 0.5) +
    geom_line(aes(y = hlutf), alpha = 0.1) +
    geom_line(aes(y = response)) +  
    geom_rangeframe() +  
    scale_x_date(breaks = seq.Date(from = min(plot_dat$dags), to = ymd("2022-01-01"), by = "year"), 
                 date_labels = "%Y",  
                 guide = guide_axis(n.dodge = 1),  
                 expand = expansion(),
                 limits = c(min(plot_dat$dags), ymd("2022-04-01"))) +
    scale_y_continuous(breaks = c(range(plot_dat$hlutf),
                                  0.3, 0.325),
                       labels = label_percent(accuracy = 0.1),
                       limits = c(0.25, 0.37)) +  
    labs(x = NULL, y = NULL,  
         title = "Hlutfall opinbers starfsfólks af vinnumarkaði (2008 - 2022)",
         subtitle = "Leiðrétt fyrir mánaðarlegum sveiflum. Raungögn teiknuð í gráu") +  
    theme_tufte() +  
    theme(plot.margin = margin(t = 5, r = 60, b = 5, l = 3),
          plot.title = element_text(face = "bold"))

p1 <- plot_dat2 |> 
    ggplot(aes(dags, perc_change)) +
    geom_vline(xintercept = ymd(dates), lty = 2, alpha = 0.5) +
    geom_hline(yintercept = 1, lty = 2) +
    geom_line(aes(col = tegund)) +
    geom_text(data = plot_dat2 |> filter(dags == max(dags)),
              aes(label = tegund, col = tegund),
              hjust = 0, nudge_x = 20) +
    geom_rangeframe() +
    scale_x_date(date_breaks = "year", date_labels = "%Y", 
                 expand = expansion()) +
    scale_y_log10(labels = function(x) percent(x - 1),
                  breaks = c(1, range(plot_dat2$perc_change), 1.005, 0.995)) +
    scale_colour_brewer(type = "qual", palette = "Set1") +
    coord_cartesian(clip = "off") +
    theme_tufte() +
    theme(plot.margin = margin(t = 5, r = 60, b = 5, l = 0),
          legend.position = "none",
          plot.title = element_text(face = "bold")) +
    labs(x = NULL,
         y = NULL,
         title = "Mánaðarleg hlutfallsleg breyting í fjölda starfsfólks",
         subtitle = "Y-ás er á lograkvarða svo að -50% og +100% eru jafnlangt frá 0% breytingu")


plot_grid(p, p1, ncol = 1)

