# df: data frame
# vars: variables to tabulate
# by: variable to crosstab with (it will be on the column)
# strata: A startifcation variable
# add.p: add p-vale
# add.ci: Add CI 
# report_numeric: which statisitics to use for numeric variables
# report_numeric: how to summarize numeric variables
# drop_na: whether to include missing values in the table
# caption: caption for the table


tablefun <- function(df, vars
                     , by=NULL
                     , strata=NULL
                     , add.p=TRUE
                     , add.ci=FALSE
                     , report_numeric = c("mean", "median")
                     , numeric_summary = c("sd", "min-max")
                     , drop_na=FALSE
                     , caption=NULL) {
  
  vars_col <-  c(vars, strata, by)
  existing_vars <- c()
  for(var in vars_col) {
    if(var %in% names(df)) {
      existing_vars <- c(existing_vars, var)
    }
  }
  
  df <- na.omit(df[, existing_vars])
  report_numeric = match.arg(report_numeric)
  numeric_summary = match.arg(numeric_summary)
  if (add.ci) {
    if (report_numeric=="mean") {
      statistic = list(all_continuous() ~ "{mean}", all_categorical() ~ "{p}%")
    } else {
      statistic = list(all_continuous() ~ "{median}", all_categorical() ~ "{p}%")
    }
  } else {
    if (report_numeric=="mean") {
      if (numeric_summary=="sd") {
        statistic = list(all_continuous() ~ "{mean} ({sd})", all_categorical() ~ "{n} ({p}%)")
      } else {
        statistic = list(all_continuous() ~ "{mean} [{min, max}]", all_categorical() ~ "{n} ({p}%)")
      }
    } else {
      if (numeric_summary=="sd") {
        statistic = list(all_continuous() ~ "{median} ({sd})", all_categorical() ~ "{n} ({p}%)")
      } else {
        statistic = list(all_continuous() ~ "{median} [{min, max}]", all_categorical() ~ "{n} ({p}%)")
      }
    }
  }
  
  if (!is.null(strata) && strata != "") {
    allvars <- c(strata, by, vars)
  } else {
    allvars <- c(by, vars)
    
  }
  #print(allvars)
  if (is.null(caption)) {
    caption <- paste0(names(df[vars[1]]), " by ", names(df[by]))
  }
  tab <- (df
          %>% ungroup()
          %>% select(all_of(allvars))
  )
  if (drop_na) {
    tab <- (tab
            %>% filter(., if_any(all_of(vars), function(x)!is.na(x)))
    )
  }
  tab <- (tab
          %>% sjmisc::to_label(drop.levels=TRUE)
  )
  
  ttfun <- function(tab) {
    tab <- (tab	
            %>% tbl_summary(
              by = all_of(by)
              , statistic = statistic
            )
    )
    if (add.ci) {
      tab <- (tab
              %>% add_ci(pattern = "{stat} ({ci})")
      )
    }
    if (add.p==TRUE && !is.null(by) && by != "") {
      tab <- (tab 
              %>% add_p()
      )
    }
    if (!is.null(by) && by !="") {
      tab <- (tab
              %>% add_overall()
      )
    }
    tab <- (tab
            %>% modify_header(label = "**Variable**")
            %>% modify_caption(paste0("**", caption, "**"))
    )
    return(tab)
  }
  
  if (!is.null(strata) && strata!="") {
    tab <- (tab
            %>% tbl_strata(
              strata=all_of(strata),
              .tbl_fun=
                ~ .x
              %>% ttfun()
              , .combine_with = "tbl_stack"
            )
    )
  } else {
    tab <- (tab
            %>% ttfun()
    )
  }
  return(tab)
}


