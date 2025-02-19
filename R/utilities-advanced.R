################################################################################
######                         Génération d'output                        ######
################################################################################

# Calcul le BQ d'une série selon ses outputs
compute_BQ <- function(series_name, date_vect, y_brute, mod_x11, m7, frequency) {

    date_deb <- get_date_deb(series = y_brute, date_vec = date_vect)
    date_fin <- get_date_fin(series = y_brute, date_vec = date_vect)
    nb_NA <- count_NA(y_brute, simplify = TRUE)

    mode <- NA_character_
    valide <- saisonniere <- FALSE
    if (!is.na(mod_x11) && (is.na(m7) || m7 < 1.3)) {
        mode <- ifelse(mod_x11 == 1, "MULTIPLICATIVE", "ADDITIVE")
        valide <- saisonniere <- TRUE
    } else if (!is.na(mod_x11) && m7 >= 1.3) {
        valide <- TRUE
    }

    BQ <- data.frame(
        series = series_name,
        start = date_deb,
        end = date_fin,
        frequency = frequency,
        nb_NA = nb_NA,
        valide = valide,
        saisonniere = saisonniere,
        mode.x11 = mode
    )

    return(BQ)
}

# Lecture des fichiers de donnée
get_data <- function(path, erase_zero = TRUE, sep = ";", dec = ".", ...) {
    path <- normalizePath(path, mustWork = TRUE)
    data <- read.csv(
        file = path,
        sep = sep,
        dec = dec,
        header = TRUE,
        encoding = "UTF-8",
        ...
    )
    if (erase_zero) {
        data[data == 0] <- NA_real_
    }
    return(data)
}

# Lecture des fichiers de donnée
write_data <- function(data, path) {
    path <- normalizePath(path, mustWork = FALSE)
    write.table(
        x = data,
        file = path,
        quote = FALSE,
        sep = ";",
        row.names = FALSE,
        dec = ".",
        na = ""
    )
    return(invisible(path))
}

# Regroupe toutes les fréquences dans un tableau résumé
# L'objet list_frequencies a été créé par une réunion (list) d'objet créé avec get_data_frequency
summarise_frequencies <- function(list_frequencies) {
    frequencies <- lapply(
        X = list_frequencies,
        FUN = base::`[[`,
        "Frequency"
    )
    table_frequencies <- sapply(
        X = names(frequencies),
        FUN = \(x) format_table(frequencies[[x]], x)
    )
    return(
        Reduce(
            f = function(x, y) merge(x, y, by = "Frequency", all = TRUE),
            x = table_frequencies
        )
    )
}

# créer une table vide avec la même structure (colnames et vecteur date)
prepare_empty_table <- function(data, add_prev = TRUE) {
    data[, -1] <- NA_real_
    if (add_prev) {
        data <- Reduce(rbind, x = rep(NA_real_, 12L), init = data)
    }
    data$date <- seq.Date(
        from = as.Date(data$date[1L]),
        by = "month",
        length.out = nrow(data)
    )
    return(data)
}

# créer un bilan qualité vide
prepare_empty_BQ <- function(data) {

    series_name <- colnames(data)[which(colnames(data) != "date")]
    nb_series <- sum(colnames(data) != "date")

    BQ <- data.frame(
        series = series_name,
        start = rep(NA_character_, nb_series),
        end = rep(NA_character_, nb_series),
        frequency = rep(NA_integer_, nb_series),
        nb_NA = rep(NA_integer_, nb_series),
        valide = rep(NA, nb_series),
        saisonniere = rep(NA, nb_series),
        mode.x11 = rep(NA_character_, nb_series)
    )
    return(BQ)
}

# récupére les fichiers output du workspace pour les analyser
extract_workspace_output <- function(datasets_name, data, frequency, structure_WS) {
    if (!frequency %in% c(2L, 4L, 12L)) {
        stop("La fréquence choisie n'est pas définie pour les WS.")
    }

    id_SAP <- structure_WS[structure_WS$frequencies == frequency, datasets_name]
    if (is.na(id_SAP)) return(NULL)

    data_SA <- get_data(
        path = paste0("./ws_automatique/IPPAP_2024/Output/SAProcessing-", id_SAP, "/series_sa.csv"),
        erase_zero = FALSE,
        dec = ","
    )
    data_SA_prev <- get_data(
        path = paste0("./ws_automatique/IPPAP_2024/Output/SAProcessing-", id_SAP, "/series_sa_f.csv"),
        erase_zero = FALSE,
        dec = ","
    )
    data_S <- get_data(
        path = paste0("./ws_automatique/IPPAP_2024/Output/SAProcessing-", id_SAP, "/series_s.csv"),
        erase_zero = FALSE,
        dec = ","
    )
    data_S_prev <- get_data(
        path = paste0("./ws_automatique/IPPAP_2024/Output/SAProcessing-", id_SAP, "/series_s_f.csv"),
        erase_zero = FALSE,
        dec = ","
    )
    colnames(data_SA)[1] <- colnames(data_SA_prev)[1] <- "date"
    colnames(data_S)[1] <- colnames(data_S_prev)[1] <- "date"

    data_BQ <- get_data(
        path = paste0("./ws_automatique/IPPAP_2024/Output/SAProcessing-", id_SAP, "/demetra_m.csv"),
        erase_zero = FALSE,
        dec = ","
    )

    all_names <- colnames(data_SA)[which(colnames(data_SA) != "date")]

    BQ <- prepare_empty_BQ(data = data[, all_names, drop = FALSE])
    S <- prepare_empty_table(data = data[, c("date", all_names), drop = FALSE], add_prev = TRUE)
    SA <- prepare_empty_table(data = data[, c("date", all_names), drop = FALSE], add_prev = TRUE)

    for (id_series in seq_along(all_names)) {

        series_name <- all_names[id_series]
        y_brute <- data[, series_name]

        BQ_series <- compute_BQ(
            series_name = series_name,
            date_vect = data$date,
            y_brute = y_brute,
            mod_x11 = data_BQ$log[data_BQ$X == series_name],
            m7 = data_BQ$m7[data_BQ$X == series_name],
            frequency = frequency
        )
        BQ[id_series, ] <- BQ_series

        if (BQ_series$saisonniere) {
            if (any(c(data_SA[, series_name], data_SA_prev[, series_name]) < 0, na.rm = TRUE)) {
                stop("La série ", series_name, "contient une ou des valeur(s) négative(s) !")
            }

            if (frequency == 12) {
                SA_tot <- merge(
                    x = data_SA[, c("date", series_name)],
                    y = data_SA_prev[, c("date", series_name)],
                    by = "date", all = TRUE
                )
                SA[, series_name] <- pmax(SA_tot[, 2], SA_tot[, 3], na.rm = TRUE)

                S_tot <- merge(
                    x = data_S[, c("date", series_name)],
                    y = data_S_prev[, c("date", series_name)],
                    by = "date", all = TRUE
                )
                S[, series_name] <- pmax(S_tot[, 2], S_tot[, 3], na.rm = TRUE)
            } else {
                S[, series_name] <- recast_series(
                    series_initial = y_brute,
                    out = data_S[, series_name],
                    prev = data_S_prev[, series_name]
                )
                SA[, series_name] <- recast_series(
                    series_initial = y_brute,
                    out = data_SA[, series_name],
                    prev = data_SA_prev[, series_name]
                )
            }
        }
    }

    return(list(BQ = BQ, SA = SA, S = S))
}

extract_rjd3x13_output <- function(data, frequency, freq_data) {

    all_names <- freq_data$series[which(freq_data$Frequency == frequency)]
    if (length(all_names) == 0) {
        return(NULL)
    }

    BQ <- prepare_empty_BQ(data = data[, all_names, drop = FALSE])
    S <- prepare_empty_table(data = data[, c("date", all_names), drop = FALSE], add_prev = TRUE)
    SA <- prepare_empty_table(data = data[, c("date", all_names), drop = FALSE], add_prev = TRUE)

    for (id_series in seq_along(all_names)) {

        series_name <- all_names[id_series]
        y_brute <- data[, series_name]

        mod <- NULL
        try(
            mod <- y_brute |>
                simplify_series(na_trim = TRUE) |>
                ts(start = 2019L, frequency = frequency) |>
                rjd3x13::x13(spec = "rsa3")
        )
        mod_x11 <- ifelse(
            test = is.null(mod$result$final$d11final),
            yes = NA,
            no = mod$result_spec$regarima$transform$fn == "LOG"
        )
        m7 <- ifelse(
            test = is.null(mod$result$final$d11final),
            yes = NA,
            no = mod$result$mstats$m7
        )
        BQ_series <- compute_BQ(
            series_name = series_name,
            date_vect = data$date,
            y_brute = y_brute,
            mod_x11 = mod_x11,
            m7 = m7,
            frequency = frequency
        )
        BQ[id_series, ] <- BQ_series

        if (BQ_series$saisonniere) {
            if (any(mod$result$final$d11final < 0, na.rm = TRUE)) {
                stop("La série ", series_name, "contient une ou des valeur(s) négative(s) !")
            }
            S[, series_name] <- recast_series(
                series_initial = y_brute,
                out = mod$result$final$d16,
                prev = mod$result$final$d16a
            )
            SA[, series_name] <- recast_series(
                series_initial = y_brute,
                out = mod$result$final$d11final,
                prev = mod$result$final$d11a
            )
        }
    }

    return(list(BQ = BQ, SA = SA, S = S))
}

extract_rjd3x11plus_output <- function(datasets_name, data, frequency, freq_data) {

    filter_seas <- ifelse(datasets_name %in% c("IPPAP", "FL_2020"), "S3X1", "S3X3")

    all_names <- freq_data$series[which(freq_data$Frequency == frequency)]
    if (length(all_names) == 0) {
        return(NULL)
    }

    BQ <- prepare_empty_BQ(data = data[, all_names, drop = FALSE])
    S <- prepare_empty_table(data = data[, c("date", all_names), drop = FALSE], add_prev = TRUE)
    SA <- prepare_empty_table(data = data[, c("date", all_names), drop = FALSE], add_prev = TRUE)

    for (id_series in seq_along(all_names)) {

        series_name <- all_names[id_series]
        y_brute <- data[, series_name]

        mod <- NULL
        # ici faire un pre-processing
        if (count_NA(y_brute) == 0) {

            try(
                mod <- y_brute |>
                    simplify_series(na_trim = TRUE) |>
                    rjd3x11plus::x11plus(
                        y = _,
                        period = as.double(frequency),
                        mul = TRUE,
                        trend.horizon = as.double(frequency) + 2,  # 1/2 Filter length : not too long vs p
                        trend.degree = 3,                         # Polynomial degree
                        trend.kernel = "Henderson",               # Kernel function
                        trend.asymmetric = "CutAndNormalize",     # Truncation method
                        seas.s0 = filter_seas, seas.s1 = filter_seas,       # Seasonal filters
                        extreme.lsig = 1.5, extreme.usig = 2.5   # Sigma-limits
                    )
            )
        }

        if (is.null(mod$parameters$multiplicative)) {
            mode_x11 <- NA
        } else {

            mode_x11 <- mod$parameters$multiplicative

            if (any(mod$result$final$d11final < 0, na.rm = TRUE)) {
                stop("La série ", series_name, "contient une ou des valeur(s) négative(s) !")
            }
            S[, series_name] <- recast_series(
                series_initial = y_brute,
                out = mod$decomposition$s,
                prev = NULL
            )
            SA[, series_name] <- recast_series(
                series_initial = y_brute,
                out = mod$decomposition$sa,
                prev = NULL
            )
        }

        BQ_series <- compute_BQ(
            series_name = series_name,
            date_vect = data$date,
            y_brute = y_brute,
            mod_x11 = mode_x11,
            m7 = NA_real_,
            frequency = frequency
        )
        BQ[id_series, ] <- BQ_series
    }

    return(list(BQ = BQ, SA = SA, S = S))
}

extract_freq_0 <- function(data, freq_data) {

    all_names <- freq_data$series[which(freq_data$Frequency == 0)]
    if (length(all_names) == 0) {
        return(NULL)
    }

    BQ <- data.frame(
        series = all_names,
        start = NA_character_,
        end = NA_character_,
        frequency = 0L,
        nb_NA = 0L,
        valide = FALSE,
        saisonniere = FALSE,
        mode.x11 = NA_character_
    )

    SA <- prepare_empty_table(data[, c("date", all_names), drop = FALSE])
    S <- prepare_empty_table(data[, c("date", all_names), drop = FALSE])

    return(list(BQ = BQ, SA = SA, S = S))
}

extract_freq_1 <- function(data, freq_data) {

    all_names <- freq_data$series[which(freq_data$Frequency == 1)]
    if (length(all_names) == 0) {
        return(NULL)
    }

    BQ <- data.frame(
        series = all_names,
        start = NA_character_,
        end = NA_character_,
        frequency = 0L,
        nb_NA = 0L,
        valide = FALSE,
        saisonniere = FALSE,
        mode.x11 = NA_character_
    )

    SA <- prepare_empty_table(data[, c("date", all_names), drop = FALSE])
    S <- prepare_empty_table(data[, c("date", all_names), drop = FALSE])

    for (id_series in seq_along(all_names)) {
        series_name <- all_names[id_series]

        y_brute <- data[, series_name]
        nb_NA <- count_NA(y_brute, simplify = TRUE)
        date_deb <- get_date_deb(series = y_brute, date_vec = data$date)
        date_fin <- get_date_fin(series = y_brute, date_vec = data$date)

        BQ_series <- data.frame(
            series = series_name,
            start = date_deb,
            end = date_fin,
            frequency = 1L,
            nb_NA = nb_NA,
            valide = FALSE,
            saisonniere = FALSE,
            mode.x11 = NA_character_
        )
        BQ[id_series, ] <- BQ_series
    }

    return(list(BQ = BQ, SA = SA, S = S))
}

extract_output <- function(datasets_name, data, frequency, freq_data, structure_WS) {
    if (frequency %in% c(2L, 4L, 12L)) {
        return(extract_workspace_output(
            datasets_name = datasets_name,
            data = data,
            frequency = frequency,
            structure_WS = structure_WS
        ))
    } else if (frequency %in% c(3L, 6L)) {
        return(extract_rjd3x13_output(data, frequency, freq_data))
    } else if (frequency %in% c(5L, 7:11)) {
        return(extract_rjd3x11plus_output(datasets_name = datasets_name, data, frequency, freq_data))
    } else if (frequency == 0) {
        return(extract_freq_0(data, freq_data))
    } else if (frequency == 1) {
        return(extract_freq_1(data, freq_data))
    } else {
        stop("Fréquence inconnue")
    }
}

merge_output <- function(list_output) {
    BQ <- list_output |>
        lapply(FUN = `[[`, "BQ") |>
        do.call(what = rbind)
    SA <- list_output |>
        lapply(FUN = `[[`, "SA") |>
        Reduce(
            f = function(x, y) {
                if (is.null(x)) return(y)
                if (is.null(y)) return(x)
                merge(x, y, by = "date", all = TRUE)
            }
        )
    S <- list_output |>
        lapply(FUN = `[[`, "S") |>
        Reduce(
            f = function(x, y) {
                if (is.null(x)) return(y)
                if (is.null(y)) return(x)
                merge(x, y, by = "date", all = TRUE)
            }
        )

    return(list(BQ = BQ, SA = SA, S = S))
}

rename_output <- function(output, nomenclature) {
    BQ <- output$BQ
    SA <- output$SA
    S <- output$S

    rownames(BQ) <- BQ$series
    BQ <- BQ[nomenclature$ID, ]
    BQ$series <- nomenclature$Nomenclature

    SA <- SA[, c("date", nomenclature$ID)]
    colnames(SA) <- c("date", nomenclature$Nomenclature)

    S <- S[, c("date", nomenclature$ID)]
    colnames(S) <- c("date", nomenclature$Nomenclature)

    return(list(BQ = BQ, SA = SA, S = S))
}
