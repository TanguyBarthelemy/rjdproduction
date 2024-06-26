################################################################################
#                                                                              #
#                                   Utilités                                   #
#                                                                              #
################################################################################


# Fonction d'affichage ----------------------------------------------------

printt <- function(obj) {
    obj_name <- deparse(substitute(obj))
    cat(obj_name, ":", obj, "\n")
    return(invisible(NULL))
}


# Indicateurs statistiques -----------------------------------------------------

# Calcul d'un taux de croissance
tx_cr <- function(v) {
    if ("ts" %in% class(v)) {
        w <- (v - stats::lag(v, -1)) / stats::lag(v, -1) * 100
    } else {
        w <- (v[-1] - dplyr::lag(v)[-1]) / dplyr::lag(v)[-1] * 100
    }
    return(w)
}

# Calcul d'un glissement annuel
glissement_annuel <- function(v) {
    if ("ts" %in% class(v)) {
        w <- v - stats::lag(v, -1)
    } else {
        w <- v[-1] - dplyr::lag(v)[-1]
    }
    return(w)
}


# Fonctions de complément à rjdworkspace ---------------------------------------

# récupérer le jeu de régresseur associé à un SA-ITEM

get_cjo_regressor_sai <- function(specification) {
    regressors <- specification$regarima$regression.coefficients |> rownames()
    regressors <- regressors[!substr(regressors, 1, 2) %in% c("AO", "TC", "LS", "SO", "Me")]

    regs_cjo <- "Pas_CJO"
    if (any(grepl("REG1", regressors))) regs_cjo <- "REG1"
    if (any(grepl("REG2", regressors))) regs_cjo <- "REG2"
    if (any(grepl("REG3", regressors))) regs_cjo <- "REG3"
    if (any(grepl("REG5", regressors))) regs_cjo <- "REG5"
    if (any(grepl("REG6", regressors))) regs_cjo <- "REG6"
    if (any(grepl("LeapYear", regressors)) || any(grepl("LY", regressors))) regs_cjo <- paste0(regs_cjo, "_LY")

    if (regs_cjo == "Pas_CJO_LY") regs_cjo <- "LY"

    return(regs_cjo)
}


get_cjo_regressor_ws <- function(ws_xml_path) {
    ws_ref <- load_workspace(normalizePath(ws_xml_path, mustWork = TRUE))
    compute(ws_ref)

    sap <- ws_ref |> get_object()
    regs_cjo_tab <- data.frame(series = get_all_names(sap), reg_cjo = character(count(sap)))

    for (k in seq_len(count(sap))) {
        cat(paste0("Série ", k, "/", count(sap), "\n"))

        regs_cjo <- sap |>
            get_object(k) |>
            get_model(workspace = ws_ref) |>
            get_cjo_regressor_sai()

        regs_cjo_tab[k, "reg_cjo"] <- regs_cjo
    }

    return(regs_cjo_tab)
}

# récupérer les outliers associé à un SA-ITEM

get_outliers_sai <- function(specification, after = TRUE, sorted = TRUE, date_min) {

    regressors <- specification$regarima$specification$regression$userdef$outliers

    if (nrow(regressors) > 0) {
        outliers_type <- regressors$type
        outliers_date <- regressors$date |>
            as.Date(format = "%Y-%m-%d")

        if (missing(date_min)) {
            after <- FALSE
        }

        if (after) {
            outliers_type <- outliers_type[outliers_date >= date_min]
            outliers_date <- outliers_date[outliers_date >= date_min]

            if (length(outliers_date) == 0) {
                return(NULL)
            }
        }

        if (sorted) {
            outliers_type <- outliers_type[order(outliers_date)]
            outliers_date <- outliers_date[order(outliers_date)]
        }

        return(list(
            type = outliers_type,
            date = outliers_date
        ))
    }

    return(NULL)
}

# Changer le domainSpec

set_domain_spec <- function(sa_item, spec) {
    sa_def <- .jcall(
        obj = sa_item,
        returnSig = "Ljd2/datatypes/sa/SaItemType;",
        method = "getSaDefinition"
    )

    sa_item <- rjdworkspace:::builder_from_sa(
        sa_def = sa_def,
        domainSpec = RJDemetra::get_jspec(spec)
    )
    new_sa_item <- .jnew("ec/tstoolkit/jdr/ws/SaItem", sa_item)
    new_sa_item
}


# Select regressor --------------------------------------------------------


create_reg_cjo_sets <- function(regs_cjo) {

    REG1 <- regs_cjo[, "REG1_Semaine", drop = FALSE]
    attr(REG1, "class") <- c("mts", "ts", "matrix", "array")

    LY <- regs_cjo[, "LY", drop = FALSE]
    attr(LY, "class") <- c("mts", "ts", "matrix", "array")

    sets <- list(
        Pas_CJO = NULL,
        REG1 = REG1,
        REG2 = regs_cjo[, c("REG2_Semaine", "REG2_Samedi")],
        REG3 = regs_cjo[, c("REG3_Lundi", "REG3_Semaine", "REG3_Samedi")],
        REG5 = regs_cjo[, c(
            "REG5_Lundi", "REG5_Mardi", "REG5_Mercredi",
            "REG5_Jeudi", "REG5_Vendredi"
        )],
        REG6 = regs_cjo[, c(
            "REG6_Lundi", "REG6_Mardi", "REG6_Mercredi",
            "REG6_Jeudi", "REG6_Vendredi", "REG6_Samedi"
        )],
        LY = LY,
        REG1_LY = regs_cjo[, c("REG1_Semaine", "LY")],
        REG2_LY = regs_cjo[, c("REG2_Semaine", "REG2_Samedi", "LY")],
        REG3_LY = regs_cjo[, c("REG3_Lundi", "REG3_Semaine", "REG3_Samedi", "LY")],
        REG5_LY = regs_cjo[, c(
            "REG5_Lundi", "REG5_Mardi", "REG5_Mercredi",
            "REG5_Jeudi", "REG5_Vendredi", "LY"
        )],
        REG6_LY = regs_cjo[, c(
            "REG6_Lundi", "REG6_Mardi", "REG6_Mercredi",
            "REG6_Jeudi", "REG6_Vendredi", "REG6_Samedi", "LY"
        )]
    )

    return(sets)
}

create_spec_sets <- function(regs_cjo) {
    regs_cjo_sets <- create_reg_cjo_sets(regs_cjo)

    spec_0 <- RJDemetra::x13_spec(
        spec = "RSA3"#,
        # estimate.from = "2012-01-01"
    )

    spec_sets <- lapply(X = regs_cjo_sets, FUN = function(regs_set) {
        spec <- spec_0
        if (!is.null(regs_set)) {
            nb_regs <- ifelse(is.null(ncol(regs_set)), 1, ncol(regs_set))
            spec <- RJDemetra::x13_spec(
                spec = spec,
                tradingdays.option = "UserDefined",
                usrdef.varEnabled = TRUE,
                usrdef.var = regs_set,
                usrdef.varType = rep("Calendar", nb_regs)
            )
        }
        return(spec)
    })

    return(spec_sets)
}

one_diagnostic <- function(serie, spec) {

    mod <- NULL
    try(mod <- RJDemetra::x13(series = serie, spec = spec))

    # L'ajustement a échoué
    if (is.null(mod)) {
        return(c(note = -Inf, aicc = Inf))
    }

    res_td <- mod$diagnostics$residuals_test[c(
        "f-test on sa (td)",
        "f-test on i (td)"
    ), "P.value"]

    note <- sum((res_td < .05) * 2:1)
    aicc <- mod$regarima$loglik["aicc", ]

    return(c(note = note, aicc = aicc))
}

all_diagnostics <- function(serie, spec_sets, outliers = NULL) {
    if (missing(spec_sets)) {
        spec_sets <- create_spec_sets()
    }

    output <- lapply(X = seq_along(spec_sets), FUN = function(k) {
        spec_out <- spec_sets[[k]]
        if (!is.null(outliers)) {
            spec_out <- x13_spec(
                spec = spec_out,
                usrdef.outliersEnabled = TRUE,
                usrdef.outliersType = outliers$type,
                usrdef.outliersDate = outliers$date |> as.character()
            )
        }
        cat("Computing spec", names(spec_sets)[k], "...")
        output <- one_diagnostic(spec = spec_out, serie = serie)
        cat("Done !\n")
        return(output)
    })

    output <- output |> do.call(what = rbind)
    output <- cbind(
        regs = names(spec_sets),
        data.frame(output)
    )

    return(output)
}

select_reg_one_serie <- function(serie, name = "", outliers = NULL, spec_sets, regs_cjo) {
    if (missing(spec_sets)) {
        spec_sets <- create_spec_sets(regs_cjo)
    }

    diag <- all_diagnostics(serie, outliers = outliers, spec_sets = spec_sets)
    diag_wo_na <- diag |>
        subset(!is.na(note) & !is.na(aicc) & !is.infinite(note) & !is.infinite(aicc))

    if (nrow(diag_wo_na) == 0) {
        stop(
            "Erreur lors du calcul de l'aicc et des p-value.
             Aucun jeu de regresseur n'a pu être sélectionné. ",
            ifelse(name == "", "", paste0("(Série ", name, ")"))
        )
    } else if (all(diag_wo_na$note == 0)) {
        warning(
            "Aucun jeu de regresseur n'est significatif. ",
            ifelse(name == "", "", paste0("(Série ", name, ")"))
        )
    }

    best_regs <- diag_wo_na |>
        subset(note == max(note, na.rm = TRUE)) |>
        subset(aicc == min(aicc, na.rm = TRUE))

    return(best_regs[1, 1])
}

# Si on veut utiliser les outliers, il faut forcémment fournir un workspace ref (qu contient 1 seul SAP avec toutes les séries pour récupérer les outliers)
select_regs <- function(series, ws_ref, with_outliers = FALSE, regs_cjo) {
    if (is.null(ncol(series))) {
        return(select_reg_one_serie(series, regs_cjo = regs_cjo))
    }

    if (missing(ws_ref)) {
        with_outliers <- FALSE
    }

    if (with_outliers) {
        sap_ref <- ws_ref |> get_object()
        series_name_ref <- get_all_names(sap_ref)
    }

    output <- sapply(X = seq_len(ncol(series)), FUN = function(k) {
        series_name <- colnames(series)[k]
        outliers <- NULL

        if (with_outliers) {
            # On récupère les outliers
            sai_ref <- sap_ref |> get_object(which(series_name_ref == series_name))
            sai_mod <- sai_ref |> get_model(workspace = ws_ref)
            outliers <- sai_mod |> get_outliers_sai(after = FALSE, sorted = TRUE)
        }

        cat(paste0("\nSérie ", series_name, " en cours... ", k, "/", ncol(series)), "\n")
        return(select_reg_one_serie(serie = series[, k], name = series_name, outliers = outliers, regs_cjo = regs_cjo))
    })

    output <- cbind(series = colnames(series), reg_selected = output)
    return(output)
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

create_ws_with_cjo <- function(ws_template_xml, choix_cjo, ws_destination_xml) {

    # Création WS_auto
    ws_auto <- new_workspace()
    sap1 <- new_multiprocessing(ws_auto, "SAProcessing-1")

    # Chargement ws_template_cjo
    # le WS template est un WS qui contient :
    #   - toutes les séries de notre dataset
    #   - autant de SAP que de jeux de régresseurs
    #   - des utilities remplies et à jour

    ws_template_cjo <- load_workspace(file = ws_template_xml)
    compute(ws_template_cjo)

    series_name <- ws_template_cjo |>
        get_object() |>
        get_all_names()

    # Chargement fichier de choix de regresseurs
    # le fichier choix cjo doit contenir au moins 2 colonnes :
    #   - une colonne series avec le nom des séries
    #   - une colonne cjo_N qui contient les regresseurs cjo sélectionnés
    choix_regs_cjo <- read.csv(choix_cjo, sep = ";")

    # Transfert des séries du ws_template_cjo au WS_auto
    for (k in seq_along(series_name)) {
        name_serie <- series_name[k]
        cat(paste0("Série ", name_serie, " en cours... ", k, "/", length(series_name)), "\n")

        regs_cjo <- choix_regs_cjo$cjo_N[choix_regs_cjo$series == name_serie]
        rjdworkspace::transfer_series(
            ws_from = ws_template_cjo, ws_to = ws_auto,
            selected_series = name_serie,
            pos_sap_to = 1, name_sap_from = regs_cjo
        )
    }


    # Sauvegarde du WS_auto
    save_workspace(ws_auto, file = ws_destination_xml)

    message("Il faut maintenant ajouter à la main les variables externes au WS auto sous la GUI")

    return(invisible(TRUE))
}

affect_outliers <- function(ws_ref_xml, ws_auto_xml) {
    ws_auto <- load_workspace(file = ws_auto_xml)
    ws_ref <- load_workspace(file = ws_ref_xml)

    compute(ws_auto)
    compute(ws_ref)

    sap_auto <- ws_auto |> get_object()
    sap_ref <- ws_ref |> get_object()

    sap_auto2 <- new_multiprocessing(ws_auto, name = "SAProcessing-2")

    # sap_auto3 <- new_multiprocessing(ws_auto, name = "SAProcessing-3")

    series_name_ref <- sap_ref |> get_all_names()
    series_name_auto <- sap_auto |> get_all_names()


    # Ajouter les outliers du WS ref sur le WS auto --------------------------------

    for (k in seq_len(sap_auto |> count())) {
        series_name <- series_name_auto[k]
        cat(paste0("Série ", series_name, " en cours... ", k, "/", sap_auto |> count()), "\n")

        sai_auto <- sap_auto |> get_object(which(series_name_auto == series_name))
        sai_ref <- sap_ref |> get_object(which(series_name_ref == series_name))

        outliers <- get_outliers_sai(sai_ref |> get_model(workspace = ws_ref))

        if (length(outliers$date) > 0) {
            spec_init <- sai_auto |> get_model(ws_auto)

            # lors de la création de la new_spec, RJDemetra récupère les variables de régression sans leur nom de groupe
            new_spec <- x13_spec(
                spec = spec_init,
                # estimate.from = "2012-01-01",
                usrdef.outliersEnabled = TRUE,
                usrdef.outliersType = outliers$type,
                usrdef.outliersDate = outliers$date |> as.character()
            )

            # Lors de la création du sa-item, rjdworkspace ecrit les variables de régression dans un seul et même groupe appelé r
            new_sai <- x13(series = spec_init$final$series[, "y"], spec = new_spec)

            # new_sai2 <- rjdworkspace::set_spec(sai_auto, new_spec)
            # SAP 1
            add_sa_item(workspace = ws_auto, multiprocessing = "SAProcessing-2", sa_obj = new_sai, name = series_name)

            # SAP 2
            # replace_sa_item(mp = sap_auto2, pos = k, sa_item = new_sai2)
            # SAP 3
            # add_new_sa_item(mp = sap_auto3, sa_item = new_sai2)
        } else {
            add_new_sa_item(sap = sap_auto2, sa_item = sai_auto)
        }
    }

    save_workspace(ws_auto, file = ws_auto_xml)
}

update_metadata <- function(ws_from, ws_to, pos_sap_from, pos_sap_to)
{
    all_sap_ws_from <- RJDemetra::get_all_objects(ws_from)
    all_sap_ws_from_names <- names(all_sap_ws_from)

    if (pos_sap_from > count(ws_from)) {
        stop("Wrong index")
    }

    if (pos_sap_to > count(ws_to)) {
        stop("Wrong index")
    }

    sap_ws_to <- RJDemetra::get_object(ws_to, pos_sap_to)
    sap_ws_from <- RJDemetra::get_object(ws_from, pos_sap_from)

    for (pos_sa in seq_len(RJDemetra::count(sap_ws_to))) {

        sa_ws_to <- RJDemetra::get_object(sap_ws_to, pos_sa)
        sa_name <- RJDemetra::get_name(sa_ws_to)

        sa_ws_from_i <- which(RJDemetra::get_all_names(sap_ws_from) %in% sa_name)
        if (length(sa_ws_from_i) != 1) {
            stop("erreur sur les noms")
        }

        sa_ws_from <- RJDemetra::get_object(x = sap_ws_from,
                                            pos = sa_ws_from_i[1])
        new_sa_item <- set_metadata(sa_to = sa_ws_to,
                                    sa_from = sa_ws_from)
        replace_sa_item(sap = sap_ws_to, sa_item = new_sa_item,
                        pos = pos_sa)
    }

    return(ws_to)
}

