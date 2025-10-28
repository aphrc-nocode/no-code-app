# R/utils_logging.R
save_best_model_log <- function(leaderboard,
                                session_name,
                                session_id = NA_character_,
                                dataset_id = NA_character_,
                                outcome    = NA_character_,
                                framework  = "PyCaret") {

  # ---------- Emplacement des logs ----------
  logs_path <- file.path(getwd(), "logs", "models", "index.csv")
  dir.create(dirname(logs_path), recursive = TRUE, showWarnings = FALSE)

  # ---------- Helpers robustes ----------
  `%||%` <- function(a, b) if (is.null(a)) b else a

  .extract_scalar_chr <- function(el) {
    if (is.null(el)) return(NA_character_)
    if ((is.atomic(el) || is.factor(el)) && length(el) >= 1) return(as.character(el[1]))
    if (is.data.frame(el)) {
      if (ncol(el) >= 1 && nrow(el) >= 1) return(as.character(el[[1]][1]))
      return(NA_character_)
    }
    if (is.list(el) && length(el) >= 1) {
      sub <- el[[1]]
      if ((is.atomic(sub) || is.factor(sub)) && length(sub) >= 1) return(as.character(sub[1]))
      if (is.data.frame(sub) && ncol(sub) >= 1 && nrow(sub) >= 1) return(as.character(sub[[1]][1]))
    }
    NA_character_
  }

  .to_num <- function(v) {
    v <- as.character(v)
    v <- gsub("\\u00A0", " ", v, useBytes = TRUE)
    v <- trimws(v)
    # garde le 1er nombre rencontré (gère "0.91 ± 0.03", "91,2%", etc.)
    v <- sub("^.*?([-+]?\\d+(?:[\\.,]\\d+)?(?:[eE][-+]?\\d+)?).*?$", "\\1", v, perl = TRUE)
    v <- gsub(",", ".", v)
    v <- gsub("%", "", v)
    suppressWarnings(as.numeric(v))
  }

  .flatten_df <- function(df) {
    if (is.null(df) || !is.data.frame(df)) return(data.frame())

    # 1) aplatir colonnes data.frame et list-columns
    for (n in names(df)) {
      x <- df[[n]]
      if (is.data.frame(x)) {
        if (ncol(x) == 0) { df[[n]] <- rep(NA_character_, nrow(df)); next }
        x <- x[[1]]
      }
      if (is.list(x)) {
        df[[n]] <- vapply(x, .extract_scalar_chr, character(1), USE.NAMES = FALSE)
      }
    }

    # 2) convertir les métriques connues en numérique si possible
    known <- c("Accuracy","AUC","ROC AUC","ROC_AUC","F1","F1 Score","Recall",
               "Sensitivity","Prec.","Precision","Specificity","Kappa","MCC",
               "R2","RMSE","MAE","MAPE","RMSLE")
    for (nm in intersect(known, names(df))) {
      if (!is.numeric(df[[nm]])) {
        num <- .to_num(df[[nm]])
        if (any(!is.na(num))) df[[nm]] <- num
      }
    }

    # 3) convertir aussi toute colonne "numeric-like" (>=60% convertissable) sauf identifiants/temps
    id_drop <- c("Model","model","model_name","model_id","id","Rank","rank","Fold","fold",
                 "Time","TT","TT (Sec)","TT(Sec)","TT Sec","n_test","n test")
    for (nm in setdiff(names(df), id_drop)) {
      if (!is.numeric(df[[nm]]) && is.character(df[[nm]])) {
        num <- .to_num(df[[nm]])
        if (mean(!is.na(num)) >= 0.6) df[[nm]] <- num
      }
    }
    df
  }

  # ---------- Normalisation du leaderboard ----------
  lb <- .flatten_df(as.data.frame(leaderboard, stringsAsFactors = FALSE, check.names = FALSE))
  if (!NROW(lb)) return(invisible(FALSE))

  # Harmoniser quelques noms usuels
  if ("model_name" %in% names(lb) && !"Model" %in% names(lb)) names(lb)[names(lb) == "model_name"] <- "Model"
  if ("ROC_AUC" %in% names(lb) && !"ROC AUC" %in% names(lb)) names(lb)[names(lb) == "ROC_AUC"] <- "ROC AUC"

  # Choisir le "meilleur" : par Rank si présent/numerique, sinon 1re ligne
  if ("Rank" %in% names(lb) && is.numeric(lb$Rank)) {
    lb <- lb[order(lb$Rank, decreasing = FALSE), , drop = FALSE]
  }
  best <- lb[1, , drop = FALSE]

  # ---------- Métadonnées ----------
  now_iso <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ")
  meta <- data.frame(
    date_trained = now_iso,
    session_name = as.character(session_name %||% NA_character_),
    session_id   = as.character(session_id   %||% NA_character_),
    dataset_id   = as.character(dataset_id   %||% NA_character_),
    outcome      = as.character(outcome      %||% NA_character_),
    framework    = as.character(framework    %||% "PyCaret"),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  # ---------- Modèle (lisible + code) ----------
  model_readable <- if ("Model" %in% names(best)) as.character(best$Model[1]) else NA_character_
  model_code <- if ("id" %in% names(best)) {
    as.character(best$id[1])
  } else if ("model_id" %in% names(best)) {
    as.character(best$model_id[1])
  } else {
    NA_character_
  }
  model_df <- data.frame(
    model        = model_readable,
    model_code   = model_code,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  # ---------- Mètriques : on garde toutes les colonnes numériques ----------
  drop_cols <- c("Model","id","model_id","Rank","Fold","Time","TT","TT (Sec)","TT(Sec)","TT Sec","n_test","n test")
  metric_cols <- setdiff(names(best)[vapply(best, is.numeric, logical(1))], drop_cols)
  metrics <- if (length(metric_cols)) best[, metric_cols, drop = FALSE] else data.frame(check.names = FALSE)

  # ---------- Ligne finale (UTILISER data.frame, pas cbind) ----------
  row <- data.frame(meta, model_df, metrics, stringsAsFactors = FALSE, check.names = FALSE)

  # ---------- Append / écriture (en large) ----------
  if (!file.exists(logs_path)) {
     #utils::write.csv(row, logs_path, row.names = FALSE, na = "", fileEncoding = "UTF-8", qmethod = "double")
    utils::write.table(out, logs_path,
                   sep = ",", row.names = FALSE,
                   col.names = TRUE, qmethod = "escape")

  } else {
    old <- tryCatch(utils::read.csv(logs_path, stringsAsFactors = FALSE, check.names = FALSE),
                    error = function(e) data.frame())
    # Harmoniser colonnes (union)
    all_cols <- union(names(old), names(row))
    for (nm in setdiff(all_cols, names(old))) old[[nm]] <- NA
    for (nm in setdiff(all_cols, names(row))) row[[nm]] <- NA
    old <- old[, all_cols, drop = FALSE]
    row <- row[, all_cols, drop = FALSE]

    out <- rbind(old, row)
    #utils::write.csv(out, logs_path, row.names = FALSE, na = "", fileEncoding = "UTF-8", qmethod = "double")
    utils::write.table(out, logs_path,
                   sep = ",", row.names = FALSE,
                   col.names = TRUE, qmethod = "escape")

  }

  invisible(TRUE)
}
