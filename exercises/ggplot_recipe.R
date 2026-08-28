ggplot_recipe <- function(p) {
  
  # Validate input: only accept a ggplot object
  stopifnot(inherits(p, "ggplot"))
  
  # --- Step 1: Print the ggplot() call and whether default data is attached ---
  if (!is.null(p$data) && !inherits(p$data, "waiver") && nrow(p$data) > 0) {
    # Default data exists and has rows
    cat(sprintf("ggplot(data = <%d rows>)\n", nrow(p$data)))
  } else {
    # No default data (common for ggplot() + annotation_custom)
    cat("ggplot()\n")
  }
  
  # --- Step 2: Print global aesthetics (mapping at plot level) ---
  if (length(p$mapping) > 0) {
    # Convert each aes quosure to a readable label, e.g. "variable"
    aes_chr <- vapply(p$mapping, rlang::as_label, character(1))
    # Combine into "x = 0, y = variable" style text
    aes_part <- paste(names(p$mapping), aes_chr, sep = " = ", collapse = ", ")
    cat("  aes(", aes_part, ")\n", sep = "")
  }
  
  # --- Step 2b: Print facet specification (facet_grid / facet_wrap) ---
  if (!inherits(p$facet, "FacetNull")) {
    fp <- p$facet$params
    facet_cls <- class(p$facet)[1]
    facet_fn <- switch(
      facet_cls,
      FacetGrid = "facet_grid",
      FacetWrap = "facet_wrap",
      paste0("facet_", tolower(sub("^Facet", "", facet_cls)))
    )

    # ggplot2 may store facet sides as quosures lists, single quosures, symbols, or strings
    facet_side_info <- function(x) {
      empty <- list(label = ".", names = character(0))
      if (is.null(x) || identical(x, ".")) return(empty)

      # ggplot2 >= 3.5: rows/cols are named lists with class "quosures"
      if (inherits(x, "quosures") || (is.list(x) && !rlang::is_quosure(x))) {
        nm <- names(x)
        if (length(x) == 0L || is.null(nm) || !any(nzchar(nm))) return(empty)
        nm <- nm[nzchar(nm)]
        return(list(label = paste(nm, collapse = " + "), names = nm))
      }

      if (rlang::is_quosure(x)) {
        if (rlang::quo_is_null(x)) return(empty)
        nm <- names(x)
        if (!is.null(nm) && length(nm) == 1L && nzchar(nm)) {
          return(list(label = nm, names = nm))
        }
        expr <- rlang::quo_get_expr(x)
        if (is.name(expr)) {
          nm <- as.character(expr)
          return(list(label = nm, names = nm))
        }
        if (identical(expr, quote(.))) return(empty)
        lab <- paste(deparse(expr, width.cutoff = 500L), collapse = "")
        if (identical(lab, "<quos>") || !nzchar(lab)) lab <- "."
        nm <- tryCatch(rlang::as_name(expr), error = function(e) character(0))
        return(list(label = lab, names = nm))
      }

      if (is.character(x) && length(x) == 1L) {
        if (identical(x, ".")) return(empty)
        return(list(label = x, names = x))
      }
      if (is.name(x)) {
        nm <- as.character(x)
        return(list(label = nm, names = nm))
      }
      if (identical(x, quote(.))) return(empty)

      lab <- paste(deparse(x, width.cutoff = 500L), collapse = "")
      if (identical(lab, "<quos>") || !nzchar(lab)) lab <- "."
      nm <- tryCatch(rlang::as_name(x), error = function(e) character(0))
      list(label = lab, names = nm)
    }

    facet_formula_str <- function(f) {
      lhs <- if (length(f) >= 2L) facet_side_info(f[[2L]])$label else "."
      rhs <- if (length(f) >= 3L) facet_side_info(f[[3L]])$label else "."
      paste(lhs, "~", rhs)
    }

    collect_facet_vars <- function(...) {
      unique(unlist(lapply(list(...), function(x) facet_side_info(x)$names)))
    }
    
    formula_str <- if (!is.null(fp$formula)) {
      facet_formula_str(fp$formula)
    } else if (identical(facet_cls, "FacetWrap") && !is.null(fp$facets)) {
      facet_side_info(fp$facets)$label
    } else {
      row_info <- facet_side_info(fp$rows)
      col_info <- facet_side_info(fp$cols)
      paste(row_info$label, "~", col_info$label)
    }
    
    facet_parts <- c(formula_str)
    if (!is.null(fp$scales) && !identical(fp$scales, "fixed")) {
      facet_parts <- c(facet_parts, paste0('scales = "', fp$scales, '"'))
    }
    if (!is.null(fp$switch) && nzchar(as.character(fp$switch))) {
      facet_parts <- c(facet_parts, paste0('switch = "', fp$switch, '"'))
    }
    if (!is.null(fp$nrow)) facet_parts <- c(facet_parts, paste0("nrow = ", fp$nrow))
    if (!is.null(fp$ncol)) facet_parts <- c(facet_parts, paste0("ncol = ", fp$ncol))
    
    cat("  + ", facet_fn, "(", paste(facet_parts, collapse = ", "), ")\n", sep = "")
    
    # Summarise facet variables from plot data when available
    facet_vars <- if (!is.null(fp$formula)) {
      f <- fp$formula
      collect_facet_vars(
        if (length(f) >= 2L) f[[2L]],
        if (length(f) >= 3L) f[[3L]]
      )
    } else {
      collect_facet_vars(
        fp$rows,
        fp$cols,
        if (identical(facet_cls, "FacetWrap")) fp$facets
      )
    }
    facet_vars <- facet_vars[facet_vars != "."]
    
    if (length(facet_vars) > 0 &&
        !is.null(p$data) && !inherits(p$data, "waiver") && nrow(p$data) > 0) {
      for (fv in facet_vars) {
        if (!fv %in% names(p$data)) next
        fv_vals <- p$data[[fv]]
        n_levels <- length(unique(fv_vals))
        cat("    # facet var ", fv, ": ", n_levels, " level(s)\n", sep = "")
        lvl <- utils::head(levels(as.factor(fv_vals)), 5)
        if (length(lvl) > 0) {
          suffix <- if (n_levels > length(lvl)) ", ..." else ""
          cat("    #   e.g. ", paste(lvl, collapse = ", "), suffix, "\n", sep = "")
        }
      }
    }
  }
  
  # --- Step 3: Loop over each layer (geom/stat/coord additions) ---
  for (i in seq_along(p$layers)) {
    l <- p$layers[[i]]
    
    # Infer geom function name from internal class, e.g. GeomText -> geom_text
    geom <- class(l$geom)[1]
    geom_fn <- sub("^Geom", "", geom)
    geom_fn <- tolower(gsub("([a-z])([A-Z])", "\\1_\\2", geom_fn))
    cat("  + geom_", geom_fn, "(", sep = "")
    
    # Collect layer arguments: aes() and fixed params (hjust, size, etc.)
    parts <- character(0)
    
    # Layer-specific aesthetics, e.g. aes(x = 0, y = variable, label = value)
    if (length(l$mapping) > 0) {
      aes_chr <- vapply(l$mapping, rlang::as_label, character(1))
      aes_inner <- paste(names(l$mapping), aes_chr, sep = " = ", collapse = ", ")
      parts <- c(parts, paste0("aes(", aes_inner, ")"))
    }
    
    # Non-aesthetic parameters passed to the geom, e.g. hjust = 0, size = rel(3)
    if (length(l$aes_params) > 0) {
      params <- vapply(l$aes_params, function(x) {
        txt <- paste(deparse(x), collapse = "")
        if (nchar(txt) > 40) paste0(substr(txt, 1, 37), "...") else txt
      }, character(1))
      param_str <- paste(names(l$aes_params), params, sep = " = ", collapse = ", ")
      parts <- c(parts, param_str)
    }
    
    # Print one layer line, e.g. + geom_text(aes(...), hjust = 0, size = rel(3))
    cat(paste(parts, collapse = ", "), ")\n", sep = "")
    
    # --- Step 3b: Show the data actually used by this layer after ggplot processing ---
    ld <- tryCatch(ggplot2::layer_data(p, i), error = function(e) NULL)
    if (!is.null(ld) && nrow(ld) > 0) {
      # Prefer columns that explain text plots; fall back to all columns
      show_cols <- intersect(c("x", "y", "label", "variable", "value"), names(ld))
      if (length(show_cols) == 0) show_cols <- names(ld)
      cat("    # layer data:", nrow(ld), "rows; cols:",
          paste(show_cols, collapse = ", "), "\n")
      print(utils::head(ld[, show_cols, drop = FALSE], 3))
    }
  }
  
  # --- Step 4: Print labels added via labs(), ggtitle(), xlab(), ylab() ---
  if (length(p$labels) > 0) {
    labs <- p$labels[nzchar(unlist(p$labels))]
    if (length(labs) > 0) {
      lab_str <- paste(names(labs), paste0('"', labs, '"'), sep = " = ", collapse = ", ")
      cat("  + labs(", lab_str, ")\n", sep = "")
    }
  }
  
  # --- Step 5: Print explicit scale limits, e.g. xlim(0, 1) ---
  for (sc in p$scales$scales) {
    if (!is.null(sc$limits) && length(sc$limits) > 0) {
      lim <- paste(sc$limits, collapse = ", ")
      cat("  + ", sc$aesthetics[1], "lim(", lim, ")\n", sep = "")
    }
  }
  
  # --- Step 6: Print custom attributes from non-ggplot code (e.g. patientProfilesVis) ---
  md <- attr(p, "metaData")
  if (!is.null(md)) {
    cat("\n# metaData:", paste(names(md), collapse = ", "), "\n")
    print(md)
  }
  
  # Return plot invisibly so the function can be piped/chained
  invisible(p)
}