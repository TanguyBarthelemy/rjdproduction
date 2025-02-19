################################################################################
######             Ensemble de fonctions utiles pour le projet            ######
################################################################################

# Compte le nombre de NA finissant la série
nb_trailing_na <- function(series) {
    series <- rev(series)
    return(sum(!cumsum(!is.na(series))))
}

# Compte le nombre de NA commençant la série
nb_leading_na <- function(series) {
    return(sum(!cumsum(!is.na(series))))
}

get_date_deb <- function(series, date_vec) {
    if (all(is.na(series))) return(character(0L))
    return(date_vec[nb_leading_na(series) + 1L])
}

get_date_fin <- function(series, date_vec) {
    if (all(is.na(series))) return(character(0L))
    return(date_vec[length(series) - nb_trailing_na(series)])
}

# supprime les NA leading ou trailing
clean_na <- function(series, before = TRUE, after = TRUE) {
    if (all(is.na(series))) {
        return(numeric(0L))
    }

    output <- series
    if (before) {
        nb_NA <- nb_leading_na(series)
        output <- output[seq.int(from = nb_NA + 1L, to = length(output))]
    }
    if (after) {
        nb_NA <- nb_trailing_na(series)
        output <- output[seq.int(from = 1L, to = length(output) - nb_NA)]
    }

    return(output)
}

# Retourne le nombre de valeurs manquantes pour compléter une année
missing_values <- function(series) {
    return(11L - ((length(series) - 1L) %% 12L))
}

# Formatte un vecteur en matrix de 12 colonnes pour chaque mois
transpose_month <- function(series) {
    nb_NA_to_complete <- missing_values(series)
    m <- matrix(
        data = c(series, rep(NA_real_, nb_NA_to_complete)),
        nrow = 12L
    )
    return(m)
}

# Retourne un vecteur de taille 12 indiquant si le mois contient des données
get_empty_month <- function(series) {
    m <- transpose_month(series)
    empty_row <- apply(
        X = m,
        MARGIN = 1L,
        FUN = \(.x) all(is.na(.x))
    )
    return(empty_row)
}

# Retourne la fréquence d'un vecteur (sans prendre en compte les NA)
get_frequency <- function(series) {
    empty_months <- get_empty_month(series)
    return(12L - sum(empty_months))
}

# Calcul des fréquences d'un jeu de donnée
get_data_frequency <- function(data) {
    freq_data <- vapply(
        X = data,
        FUN = get_frequency,
        FUN.VALUE = integer(1)
    )
    return(
        cbind(
            series = names(freq_data),
            Frequency = freq_data
        ) |> as.data.frame()
    )
}

# Formatte une table de contingence
format_table <- function(x, name) {
    x |>
        table() |>
        (\(.x) cbind(as.integer(rownames(.x)), .x))() |>
        `colnames<-`(c("Frequency", name))
}

# Simplifie les séries en supprimant les périodes inutilisées
simplify_series <- function(series, na_trim = TRUE) {
    not_empty_months <- seq_len(12)[!get_empty_month(series)]
    output <- series[((seq_along(series) - 1) %% 12 + 1) %in% not_empty_months]

    if (na_trim) {
        return(clean_na(output))
    } else {
        return(output)
    }
}

# Compte le nombre de NA dans une série (avec ou sans simplification des colonnes absentes)
count_NA <- function(series, simplify = TRUE) {
    if (simplify) {
        series <- simplify_series(series)
    }
    return(sum(is.na(series)))
}

# Remplace les valeurs d'une séries par d'autres valeurs (par exemple SA ou S)
recast_series <- function(series_initial, out, prev) {

    y1 <- simplify_series(series_initial, na_trim = FALSE)

    m <- transpose_month(series_initial)
    not_empty_months <- !get_empty_month(series_initial)

    # Ajout d'une année pour les prévisions
    m <- cbind(m, NA_real_)

    # préparation du vecteur de remplacement
    length_tot <- length(m[not_empty_months, ])

    nb_NA1 <- nb_leading_na(y1)
    nb_NA2 <- nb_trailing_na(y1)

    replacing_series <- c(
        rep(NA_real_, nb_NA1),
        c(clean_na(out), clean_na(prev)),
        rep(NA_real_, nb_NA2)
    )
    # On complète l'année en cours (pour la matrice...)
    # Si il n'y a pas de prévisions, les NA en pus viennent de là
    replacing_series <- c(replacing_series,
                          rep(NA_real_, length_tot - length(replacing_series)))

    # Remplacement des valeurs
    m[not_empty_months, ] <- replacing_series

    # récupération sous la forme d'un vecteur
    output <- as.numeric(m)

    # On retire à la fin les qqs NA utilisés pour compléter la série brute (et former une année)
    nb_NA_to_remove <- missing_values(series_initial)
    return(output[seq_len(length(output) - nb_NA_to_remove)])
}
