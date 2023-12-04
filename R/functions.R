cog_abil_pca_5 <- function(data, derived_data){
    # Joining derived dataset (containing BD2READ) into dataset F and deleting the former
    data %<>% full_join(., derived_data, by = "bcsid")

    # Detecting missing values in rows
    missing <- rowSums(is.na(data)) != 0
    data <- data.frame(data, missing)

    # Performing principal component analysis
    pca <- prcomp(data[data$missing == FALSE, c("BD2READ", "f100", "f118", "f121", "f122")], center = TRUE, scale. = TRUE)

    # Initialising derived cognitive ability score variable
    data$dv_cog_abil_5 <- rep(NA, nrow(data))

    # Storing PCA first component as derived cognitive ability score
    data[data$missing == FALSE,]$dv_cog_abil_5 <- pca[["x"]][, 1]

    # Scaling the derived score to have mean 0 and SD 1
    data$dv_cog_abil_5 %<>% scale() %>% as.numeric()

    # Removing cognitive ability other than the derived score
    data %<>% select(-BD2READ, -f100, -f118, -f121, -f122, -missing)

    return(data)
}

cog_abil_pca_10 <- function(data){
    data %<>% mutate(
        # Word Definitions
        dv_bas_worddef = case_when(
            tests10 == 1 ~ rowSums(across(i3504:i3540, `==`, 1)),
            tests10 == 0 ~ NA_real_
        ),  

        #   Recall of Digits
        dv_bas_recall = case_when(
            tests10 == 1 ~ rowSums(across(i3541:i3574, `==`, 1)),
            tests10 == 0 ~ NA_real_
        ),  

        # Matrices
        dv_bas_matrices = case_when(
            tests10 == 1 ~ rowSums(across(i3617:i3644, `==`, 1)),
            tests10 == 0 ~ NA_real_
        ),  
        
        # Word Similarities
        dv_bas_wordsim = case_when(
            tests10 == 1 ~ rowSums(across(i4201:i4221, `==`, 1)),
            tests10 == 0 ~ NA_real_
        )
    )

    # Performing principal component analysis
    pca <- prcomp(data[data$tests10 == 1,] %>% select(contains("dv_bas")), center = TRUE, scale. = TRUE) 

    # Initialising derived cognitive ability score variable
    data$dv_bas_g <- rep(NA, nrow(data))

    # Storing PCA first component as derived cognitive ability score
    data[data$tests10 == 1,]$dv_bas_g <- -pca[["x"]][, 1]

    # Scaling the derived score to have mean 0 and SD 1
    data$dv_bas_g %<>% scale() %>% as.numeric()

    # Removing cognitive ability other than the derived score
    data %<>% select(
        -(contains("dv_bas") & !dv_bas_g),
        -(i3504:i4221),
        -tests10
    )

    return(data)
}

merge_datasets <- function(...){
    # Merging datasets
    data <- list(...) %>% reduce(full_join, by = "bcsid")

    return(data)
}

remove_dead_emigrants <- function(data){
    data %<>% filter(
        # Removing participants deceased by the age 46 sweep
        OUTCME10 != 8,

        # Removing participants emigrated by the age 16 sweep
        OUTCME01 != 7,
        OUTCME02 != 7,
        OUTCME03 != 7,
        OUTCME04 != 7
    )
}

derive_vars <- function(data){
    data %<>%
        # Deriving total CDS score at age 10
        mutate(
            j178a = 48 - j178a,
            j178b = 48 - j178b
        ) %>%
        mutate(
            dv_cds_10 = rowSums(across(j127:j178b))
        ) %>%
        select(-(j127:j178b)) %>%

        # Deriving abnormal sleep for each measure and removing raw variables
        mutate(
            dv_sr_sleep_abn_cat = case_when(
                B10HSLEEP >= 6 & B10HSLEEP <= 9 ~ "Normal",
                B10HSLEEP < 6 ~ "Short",
                B10HSLEEP > 9 ~ "Long"
            ),
            dv_sd_sleep_abn_cat = case_when(
                md_sd_sleepdur >= 360 & md_sd_sleepdur <= 540 ~ "Normal",
                md_sd_sleepdur < 360 ~ "Short",
                md_sd_sleepdur > 540 ~ "Long"
            ),
            dv_pal_sleep_abn_cat = case_when(
                md_pal_sleepdur >= 21600 & md_pal_sleepdur <= 32400 ~ "Normal",
                md_pal_sleepdur < 21600 ~ "Short",
                md_pal_sleepdur > 32400 ~ "Long"
            ),
            dv_vdb_sleep_abn_cat = case_when(
                md_vdb_sleepdur >= 21600 & md_vdb_sleepdur <= 32400 ~ "Normal",
                md_vdb_sleepdur < 21600 ~ "Short",
                md_vdb_sleepdur > 32400 ~ "Long"
            ),
            dv_winkler_sleep_abn_cat = case_when(
                md_winkler_sleepdur >= 21600 & md_winkler_sleepdur <= 32400 ~ "Normal",
                md_winkler_sleepdur < 21600 ~ "Short",
                md_winkler_sleepdur > 32400 ~ "Long"
            )
        ) %>% 
        select(
            -B10HSLEEP,
            -(contains("md_") & !contains("md_sleeprate"))
        ) %>%

        # Recoding father's education as a binary factor variable for use as an auxiliary variable
        mutate(dv_father_schl = case_when(
            e192 >= 16 ~ 1,
            e192 < 15 ~ 0
        )) %>%
        select(-e192) %>%

        # Combining age 5 medical condition variables into a single binary variable and removing individual variables
        mutate(dv_med_5 = case_when(
            if_any(e067:e076, ~. == 1) ~ 1,
            if_all(e067:e076, ~. != 1) ~ 0
        )) %>%
        select(-(e067:e076)) %>%

        # Combining age 10 medical condition variables into a single binary variable and removing individual variables
        mutate(dv_med_10 = case_when(
            if_any(meb4_1:meb4_51, ~. == 1) ~ 1,
            if_all(meb4_1:meb4_51, ~ is.na(.)) & if_any(meb4_2:meb4_54, ~. == 1) ~ 0
        )) %>%
        select(-(meb4_1:meb4_51), -(meb4_2:meb4_54)) %>%

        # Combining age 16 income source variables into a single variable (employment & investments vs other) and removing individual variables
        mutate(dv_incsource = case_when(
            if_any(oe1_1:oe1_5, ~. == 1) ~ "Employment/investments",
            if_all(oe1_1:oe1_5, ~. == 2) & if_any(oe1_6:oe1_19, ~. == 1) ~ "Other"
        )) %>%
        select(-contains("oe1_")) %>%

        # Combining positive activities outside school into a single variable and removing individual variables
        mutate(dv_pos_act = rowSums(across(m84:m101))) %>%
        select(-(m84:m101)) %>%

        # Combining alcohol consumption in the last week into a single variable and removing individual variables
        mutate(dv_alcohol = case_when(
            if_any(hd5_1:hd5_7, ~. == 1) ~ 1,
            hd5_8 == 1 ~ 0
        )) %>%
        select(-contains("hd5_")) %>%

        # Combining accidents and assaults into a single variable and removing individual variables
        mutate(dv_accidents = case_when(
            b960539 == 1 ~ 0L,
            b960540 > 0 ~ as.integer(b960540)
        )) %>%
        select(-b960539, -b960540) %>%

        # Combining hospital admissions into a single variable and removing individual variables
        mutate(dv_hospital = case_when(
            hospital == 1 ~ numadmn,
            hospital == 2 ~ 0
        )) %>%
        select(-hospital, -numadmn) %>%

        # Combining apprenticeships and work-related training into a single variable and removing individual variables
        mutate(dv_training = case_when(
            if_any(c("aptrain", "wrktrain"), ~. == 1) ~ 1,
            if_all(c("aptrain", "wrktrain"), ~. == 2) ~ 0
        )) %>%
        select(-aptrain, -wrktrain) %>%

        # Recoding car ownership as a binary factor variable
        mutate(B9SCQ19 = case_when(
            B9SCQ19 > 1 ~ 1,
            B9SCQ19 == 1 ~ 0
        )) %>%

        # Combining response outcome variables into a single binary variable and removing individual variables
        mutate(dv_participation = case_when(
            if_all(contains("OUTCME"), ~. == 1) ~ 1,
            if_any(contains("OUTCME"), ~. != 1) ~ 0
        )) %>%
        select(-contains("OUTCME")) %>%

        # Combining organisation membership into a single variable and removing individual variables
        mutate(dv_organisations = case_when(
            if_any(B9SCQ8A:B9SCQ8O, ~. == 1) ~ 1,
            B9SCQ8P == 1 ~ 0
        )) %>%
        select(-(B9SCQ8A:B9SCQ8P)) %>%

        # Combining marital status items into a single variable and removing individual variables
        mutate(dv_marital = case_when(
            B9MARCHK == 1 | B9HMS %in% c(2, 5) ~ "Married/CP",
            B9DIVCHK == 1 | B9HMS %in% c(1, 3, 4, 6, 7) ~ "Divorced/Separated/Widowed",
            B9HMS == 8 ~ "Single"
        )) %>%
        select(-B9DIVCHK, -B9HMS, -B9MARCHK) %>%

        # Recoding housing tenure at age 42
        mutate(B9TEN = case_when(
            B9TEN %in% c(1, 2) ~ "Owned",
            B9TEN %in% c(3, 4, 5, 7) ~ "Not owned"
        )) %>%

        rowwise() %>%

        # Deriving maximum parental education at birth and removing individual variables
        mutate(dv_par_edu_birth = max(a0009, a0010)) %>%
        select(-a0009, -a0010) %>%

        # Deriving maximum parental social class at age 5 and removing individual variables
        mutate(dv_sc_age_5 = min(e197, e206)) %>%
        select(-e197, -e206) %>%

        # Deriving maximum parental social class at age 16 and removing individual variables
        mutate(dv_sc_age_16 = min(t11_2, t11_9)) %>%
        select(-t11_2, -t11_9)

    # Recoding labelled variables as factors, taking the values of the data labels
    data[c("a0014", "a0043b", "a0255", "e216a", "BDSTYPE", "B9TEN", "ameni", "brfed", "crowd", "resmove", "tenure", "fclrg90", "dv_sc_age_5", "dv_sc_age_16", "dv_incsource", "dv_marital")] %<>% as_factor(., only_labelled = FALSE)

    # Removing Stata-format data labels which are incompatible with further cleaning and analysis steps
    data %<>% zap_labels()

    # Merging sparse categories to aid multiple imputation model convergence
    data %<>% mutate(
        # Binarising e216a (mother's regular job since child's birth) to job vs no job
        e216a = case_when(
            e216a %in% c("F+Pt Job", "Ft Job", "Pt Job") ~ "Job",
            e216a == "None" ~ "None"
        ),

        # Combining classes I and II, and sending "other" categories to NA, in parental social class measures
        fclrg90 = if_else(
            fclrg90 %in% c("I Professional", "II Managerial and technical"),
            "I/II Professional/Managerial",
            as.character(fclrg90)
        ),
        a0014 = case_when(
            a0014 %in% c("SC 1", "SC 2") ~ "SC 1/2",
            a0014 == "SC 3 NM" ~ "SC 3 NM",
            a0014 == "SC 3 M" ~ "SC 3 M",
            a0014 == "SC 4" ~ "SC 4",
            a0014 == "SC 5" ~ "SC 5"
        ),
        dv_sc_age_5 = case_when(
            dv_sc_age_5 %in% c("I", "II") ~ "I/II",
            dv_sc_age_5 == "III NM" ~ "III NM",
            dv_sc_age_5 == "III M" ~ "III M",
            dv_sc_age_5 == "IV" ~ "IV",
            dv_sc_age_5 == "V" ~ "V"
        ),
        dv_sc_age_16 = case_when(
            dv_sc_age_16 %in% c("I", "II") ~ "I/II",
            dv_sc_age_16 == "III non-manual" ~ "III non-manual",
            dv_sc_age_16 == "III manual" ~ "III manual",
            dv_sc_age_16 == "IV" ~ "IV",
            dv_sc_age_16 == "V" ~ "V"
        ),

        # Binarising school type to comprehensive vs non-comprehensive
        BDSTYPE = case_when(
            BDSTYPE %in% c("Comprehensive", "Secondary Modern/Technical", "Scottish LEA") ~ "Comprehensive",
            BDSTYPE %in% c("Grammar", "Independent Private", "Independent Special", "LEA Special") ~ "Non-comprehensive"
        ),

        # Binarising ameni (lack of amenities) to any lack of amenities vs none
        ameni = case_when(
            ameni %in% c("1 occasion", "2 occasions") ~ "At least 1 occasion",
            ameni == "No occasions" ~ "No occasions"
        ),

        # Binarising crowd (household overcrowding) to up to 1 person/room vs more than 1
        crowd = case_when(
            crowd %in% c("Over 1 to 1.5", "Over 1.5 to 2", "Over 2") ~ "Over 1",
            crowd == "Up to 1" ~ "Up to 1"
        )
    )

    # Re-converting the newly binarised variables to factors
    for(var in c("e216a", "BDSTYPE", "ameni", "crowd")){
        data[[var]] %<>% as.factor()
    }

    # Converting other binary independent/auxiliary variables to factors
    for(var in c("a0230", "d016a", "emosup", "vote97", "B9SCQ17", "B9SCQ19", "divorce", "prmnh", "sepmumbcs", "dv_father_schl", "dv_med_5", "dv_med_10", "dv_alcohol", "dv_training", "dv_participation", "dv_organisations")){
        data[[var]] %<>% as.factor()
    }

    # Converting unordered categorical independent/auxiliary variables to factors
    for(var in c("a0014", "dv_sc_age_5", "dv_sc_age_16", "dv_marital")){
        data[[var]] %<>% as.factor()
    }

    # Converting ordered categorical independent/auxiliary variables to ordered factors
    data$a0014 %<>% factor(
        ordered = TRUE,
        levels = c("SC 1/2", "SC 3 NM", "SC 3 M", "SC 4", "SC 5")
    )

    data$a0043b %<>% factor(
        ordered = TRUE,
        levels = c("Non Smoker", "Stopped Pre-Preg", "Stopped Dur-Preg", "Ctl Smokers 1 - 4", "Ctl Smokers 5 - 14", "Ctl Smokers >= 15")
    )

    data$brfed %<>% factor(
        ordered = TRUE,
        levels = c("Breastfed for over 1 month", "Breastfed for under 1 month", "Not breastfed")
    )

    data$resmove %<>% factor(
        ordered = TRUE,
        levels = c("None", "1-3", "4 or more")
    )

    data$tenure %<>% factor(
        ordered = TRUE,
        levels = c("Owned at both time points", "Owned at one time point", "Rented at both time points")
    )

    data$fclrg90 %<>% factor(
        ordered = TRUE,
        levels = c("I/II Professional/Managerial", "IIINM Skilled non-manual", "IIIM Skilled manual", "IV Partly skilled", "V Unskilled")
    )

    data$dv_sc_age_5 %<>% factor(
        ordered = TRUE,
        levels = c("I/II", "IIINM", "IIIM", "IV", "V")
    )

    data$dv_sc_age_16 %<>% factor(
        ordered = TRUE,
        levels = c("I/II", "III non-manual", "III manual", "IV", "V")
    )

    # Converting to factors, and making "normal" the reference category for, the sleep duration category variables
    for(var in c("dv_sr_sleep_abn_cat", "dv_sd_sleep_abn_cat", "dv_pal_sleep_abn_cat", "dv_vdb_sleep_abn_cat", "dv_winkler_sleep_abn_cat")){
        data[[var]] %<>% as.factor() %>% relevel("Normal")
    }

    # Recoding "yes/no" variables as binary variables taking values 0 (for "no") and 1 (for "yes")
    data %<>% 
        mutate_at(
            c("emosup", "vote97", "B9SCQ17"),
            ~ recode(., `2` = 0L, `1` = 1L)
        ) %>%
        mutate_at(
            c("d016a", "rd6m_1"),
            ~ recode(., `1` = 0L, `2` = 1L)
        )

    # Drop unused levels from categorical variables
    data %<>% droplevels()

    return(data)
}

# Binarising sleep variables for use in Poisson regression, and storing these in a new version of the dataset
binary_sleep <- function(data){
    data %<>% 
        mutate(
            dv_sr_sleep_abn = case_when(
                dv_sr_sleep_abn_cat == "Normal" ~ 0,
                dv_sr_sleep_abn_cat %in% c("Short", "Long") ~ 1,
            ),
            dv_sd_sleep_abn = case_when(
                dv_sd_sleep_abn_cat == "Normal" ~ 0,
                dv_sd_sleep_abn_cat %in% c("Short", "Long") ~ 1,
            ),
            dv_pal_sleep_abn = case_when(
                dv_pal_sleep_abn_cat == "Normal" ~ 0,
                dv_pal_sleep_abn_cat %in% c("Short", "Long") ~ 1,
            ),
            dv_vdb_sleep_abn = case_when(
                dv_vdb_sleep_abn_cat == "Normal" ~ 0,
                dv_vdb_sleep_abn_cat %in% c("Short", "Long") ~ 1,
            ),
            dv_winkler_sleep_abn = case_when(
                dv_winkler_sleep_abn_cat == "Normal" ~ 0,
                dv_winkler_sleep_abn_cat %in% c("Short", "Long") ~ 1,
            )
        ) %>%
        select(-contains("_abn_cat"))

    return(data)
}

multiple_imputation <- function(data){
    # Linearly rescaling all numeric variables to between 0 and 1 to aid regression model convergence
    for(var in names(data)){
        if(is.numeric(data[[var]])){
            data[[var]] <- (data[[var]] - min(data[[var]], na.rm =TRUE))/(max(data[[var]], na.rm = TRUE) - min(data[[var]], na.rm = TRUE))
        }
    }

    # Creating vector of multiple imputation methods and setting method to pmm for all variables
    method <- make.method(data)
    method[] <- "pmm"

    # Setting BCSID to not be imputed and removing it from the predictor set
    method["bcsid"] <- ""
    pred <- make.predictorMatrix(data)
    pred[, "bcsid"] <- 0

    # Setting number of cores to be used for parallel computation
    noCores <- 18

    # Establishing a cluster for parallel computation to save time
    cl <- makeCluster(noCores)

    # Setting random seed
    clusterSetRNGStream(cl, 1858)

    # Making the data frame, methods and predictor matrix visible in the cluster environment
    clusterExport(cl, c("data", "method", "pred"), envir = environment())

    # Loading the mice package in the cluster environment
    clusterEvalQ(cl, library(mice))

    # Running multiple imputation with 10 imputations per core used and a maximum of 15 iterations
    ## This code was run with 18 cores and therefore there are 180 imputations in total
    imp_pars <-
        parLapply(cl = cl, X = 1:noCores, fun = function(no){
            mice(data, vis = "monotone", method = method, predictorMatrix = pred, m = 10, maxit = 15, printFlag = FALSE)
        })

    # Stopping the cluster
    stopCluster(cl)

    # Merging imputations into a single mids object
    imp_merged <- reduce(imp_pars, ibind)

    return(imp_merged)
}

postprocess <- function(data){
    data_list <- complete(data, "long", include = TRUE)

    for(i in unique(data_list$.imp)){
        for(j in c("d119", "BD3MRUTT", "dv_cds_10", "BD4MAL", "rd6m_1", "intbcsz", "extbcsz")){
            data_list[data_list$.imp == i, j] <- scale(data_list[data_list$.imp == i, j])
        }
    }

    data <- as.mids(data_list)

    return(data)
}

# Fits an individual modified Poisson regression model
pois_regr_indiv <- function(dep_var, ind_vars, data){
    indiv_model_fit <- coeftest(glm(as.formula(paste(dep_var, "~", ind_vars)), data, family = poisson(link = "log")), vcov. = sandwich)
    
    return(indiv_model_fit)
}

poisson_regression <- function(dep_var, ind_vars, var_of_interest, data, results){
    # Storing each imputed dataset as a separate dataframe
    for(i in 1:data$m){
        assign(
            paste("data", i, sep = "_"), 
            complete(data, i)
        )
    }

    # Setting number of cores for parallel computation
    noCores <- 18

    # Establishing a cluster for parallel computation to save time
    cl = makeCluster(noCores)

    # Setting random seed
    clusterSetRNGStream(cl, 1858)
    
    # Making the variables and function visible in the cluster environment
    clusterExport(cl, c("dep_var", "ind_vars", "var_of_interest", "pois_regr_indiv"), envir = environment())

    # Listing datasets for each core and making them visible in the cluster environment
    for(i in 1:noCores){
        datalist <- paste("data", seq((i-1)*(data$m/noCores) + 1, i*(data$m/noCores)), sep = "_")
        clusterExport(cl[i], c(datalist, "datalist"), envir = environment())
    }

    # Loading dependencies in the cluster environment
    clusterEvalQ(cl, library(lmtest))
    clusterEvalQ(cl, library(sandwich))

    # Producing MIRA of results from each imputed dataset
    model_fit <- as.mira(c(unlist(parLapply(cl = cl, X = 1:data$m, fun = function(no){
        lapply(1:length(datalist), function(i){
            pois_regr_indiv(dep_var, ind_vars, get(datalist[i]))
        })
    }), recursive = FALSE)))
    
    # Stopping the clusters
    stopCluster(cl)

    # Pooling results
    results[[dep_var]] <- summary(pool(model_fit))

    return(results)
}

pois_regr <- function(ind_vars, var_of_interest, data){
    # Listing dependent variables
    pois_list <- c("dv_sr_sleep_abn", "dv_sd_sleep_abn", "dv_pal_sleep_abn", "dv_vdb_sleep_abn", "dv_winkler_sleep_abn")
    
    # Initialising list of results tables
    results <- list()

    # Running Poisson regression for each dependent variable
    suppressWarnings({
        for(dep_var in pois_list){
            results <- poisson_regression(dep_var, ind_vars, var_of_interest, data, results)
        }
    })

    return(results)
}

pois_results_summarise <- function(results_list, var_of_interest){
    results_frames_list <- list()
    
    # Producing tidy results table
    for(i in seq_along(results_list)){
        results <- results_list[[i]]

        dep_var <- names(results_list)[i]
        ind_var <- var_of_interest
        RR <- exp(results[results$term == var_of_interest,]$estimate)
        RR.LCI <- exp(results[results$term == var_of_interest,]$estimate + qnorm(0.05/2)*results[results$term == var_of_interest,]$std.error)
        RR.UCI <- exp(results[results$term == var_of_interest,]$estimate - qnorm(0.05/2)*results[results$term == var_of_interest,]$std.error)
        p <- results[results$term == var_of_interest,]$p.value
        Evalue <- if(RR.LCI < 1 && RR.UCI > 1) 1 else if(RR.UCI < 1) (1/RR.UCI) + sqrt((1/RR.UCI)*((1/RR.UCI) - 1)) else if(RR.LCI > 1) RR.LCI + sqrt(RR.LCI*(RR.LCI - 1))

        results_frames_list[[i]] <- data.frame(dep_var, ind_var, RR, RR.LCI, RR.UCI, p, Evalue)
    }

    results_frame <- reduce(results_frames_list, rbind)

    return(results_frame)
}

# Fits an individual multinomial regression model
multinom_regr_indiv <- function(dep_var, ind_vars, data){
    indiv_model_fit <- multinom(as.formula(paste(dep_var, "~", ind_vars)), data, maxit = 200, trace = FALSE)

    return(indiv_model_fit)
}

multinomial_regression <- function(dep_var, ind_vars, var_of_interest, data, results){
    # Storing each imputed dataset as a separate dataframe
    for(i in 1:data$m){
        assign(
            paste("data", i, sep = "_"),
            complete(data, i)
        )
    }

    # Setting number of cores for parallel computation
    noCores <- 18

    # Establishing a cluster for parallel computation to save time
    cl = makeCluster(noCores)

    # Setting random seed
    clusterSetRNGStream(cl, 1858)

    # Making the variables and function visible in the cluster environment
    clusterExport(cl, c("dep_var", "ind_vars", "var_of_interest", "multinom_regr_indiv"), envir = environment())

    # Listing datasets for each core and making them visible in the cluster environment
    for(i in 1:noCores){
        datalist <- paste("data", seq((i-1)*(data$m/noCores) + 1, i*(data$m/noCores)), sep = "_")
        clusterExport(cl[i], c(datalist, "datalist"), envir = environment())
    }

    # Loading dependency in the cluster environment
    clusterEvalQ(cl, library(nnet))

    # Producing MIRA of results from each imputed dataset
    model_fit <- as.mira(c(unlist(parLapply(cl = cl, X = 1:noCores, fun = function(no){      
        lapply(1:length(datalist), function(i){
            multinom_regr_indiv(dep_var, ind_vars, get(datalist[i]))
        })
    }), recursive = FALSE)))

    # Stopping the cluster
    stopCluster(cl)

    # Pooling results
    results[[dep_var]] <- summary(pool(model_fit))

    return(results)
}

multinom_regr <- function(ind_vars, var_of_interest, data){
    # Listing dependent variables
    multinom_list <- c("dv_sr_sleep_abn_cat", "dv_sd_sleep_abn_cat", "dv_pal_sleep_abn_cat", "dv_vdb_sleep_abn_cat", "dv_winkler_sleep_abn_cat")

    # Initialising list of results tables
    results <- list()

    # Running multinomial regression for each dependent variable
    suppressWarnings({
        for(dep_var in multinom_list){
            results <- multinomial_regression(dep_var, ind_vars, var_of_interest, data, results)
        }
    })

    return(results)
}

multinom_results_summarise <- function(results_list, var_of_interest){
    results_frames_list <- list()

    # Producing tidy results table
    for(i in seq_along(results_list)){
        results <- results_list[[i]]

        dep_var <- names(results_list)[i]
        ind_var <- var_of_interest
        group <- results[results$term == var_of_interest,]$y.level
        RRR <- exp(results[results$term == var_of_interest,]$estimate)
        RRR.LCI <- exp(results[results$term == var_of_interest,]$estimate + qnorm(0.05/2)*results[results$term == var_of_interest,]$std.error)
        RRR.UCI <- exp(results[results$term == var_of_interest,]$estimate - qnorm(0.05/2)*results[results$term == var_of_interest,]$std.error)
        p <- results[results$term == var_of_interest,]$p.value
        Evalue <- sapply(1:2, function(i){if(RRR.LCI[i] < 1 && RRR.UCI[i] > 1) 1 else if(RRR.UCI[i] < 1) (1/RRR.UCI[i]) + sqrt((1/RRR.UCI[i])*((1/RRR.UCI[i]) - 1)) else if(RRR.LCI[i] > 1) RRR.LCI[i] + sqrt(RRR.LCI[i]*(RRR.LCI[i] - 1))})

        results_frames_list[[i]] <- data.frame(dep_var, ind_var, group, RRR, RRR.LCI, RRR.UCI, p, Evalue)
    }

    results_frame <- reduce(results_frames_list, rbind)

    return(results_frame)
}

# Runs Poisson regression models with age-42 wellbeing measures as dependent variables
med_pois_regr <- function(ind_vars, var_of_interest, data){
    results <- list()
    
    suppressWarnings({
        for(dep_var in c("BD9MAL", "BD9WEMWB")){
            results <- poisson_regression(dep_var, ind_vars, var_of_interest, data, results)
        }
    })

    return(results)
}

tetra_cor <- function(data){
    pooled_correlations <- data.frame(dv_sr_sleep_abn = rep(NA, 5), dv_sd_sleep_abn = rep(NA, 5), dv_pal_sleep_abn = rep(NA, 5), dv_vdb_sleep_abn = rep(NA, 5), dv_winkler_sleep_abn = rep(NA, 5))
    row.names(pooled_correlations) <- c("dv_sr_sleep_abn", "dv_sd_sleep_abn", "dv_pal_sleep_abn", "dv_vdb_sleep_abn", "dv_winkler_sleep_abn")

    for(i in row.names(pooled_correlations)){
        for(j in row.names(pooled_correlations)){
            if(i != j){
                temp_correlations <- rep(NA, data$m)
                temp_correlations_tables <- list()
                for(k in 1:data$m){
                    temp_correlations_tables[[k]] <- tetrachoric(complete(data, k)[, c(i, j)])
                    temp_correlations[k] <- temp_correlations_tables[[k]]$rho[2]
                }
                pooled_correlations[i, j] <- mean(temp_correlations)
                pooled_correlations[i, j] <- (exp(2 * pooled_correlations[i, j]) - 1) / (exp(2 * pooled_correlations[i, j]) + 1)
            } else {
                pooled_correlations[i, j] <- 1
            }
        }
    }

    return(pooled_correlations)
}

poly_cor <- function(data){
    pooled_correlations_multi <- data.frame(dv_sr_sleep_abn_cat = rep(NA, 5), dv_sd_sleep_abn_cat = rep(NA, 5), dv_pal_sleep_abn_cat = rep(NA, 5), dv_vdb_sleep_abn_cat = rep(NA, 5), dv_winkler_sleep_abn_cat = rep(NA, 5))
    row.names(pooled_correlations_multi) <- c("dv_sr_sleep_abn_cat", "dv_sd_sleep_abn_cat", "dv_pal_sleep_abn_cat", "dv_vdb_sleep_abn_cat", "dv_winkler_sleep_abn_cat")

    for(i in row.names(pooled_correlations_multi)){
        for(j in row.names(pooled_correlations_multi)){
            if(i != j){
                temp_correlations <- rep(NA, data$m)
                temp_correlations_tables <- list()
                for(k in 1:data$m){
                    these_data <- complete(data, k)[, c(i, j)]
                    for(l in 1:ncol(these_data)){
                        these_data[, l] <- as.integer(these_data[, l])
                    }
                    temp_correlations_tables[[k]] <- polychoric(these_data)
                    temp_correlations[k] <- temp_correlations_tables[[k]]$rho[2]
                }
                pooled_correlations_multi[i, j] <- mean(temp_correlations)
                pooled_correlations_multi[i, j] <- (exp(2 * pooled_correlations_multi[i, j]) - 1) / (exp(2 * pooled_correlations_multi[i, j]) + 1)
            } else {
                pooled_correlations_multi[i, j] <- 1
            }
        }
    }

    return(pooled_correlations_multi)
}

summarise_missingness <- function(binary_data, multi_data){
    # Initialising data frame of missingness amounts
    missingness <- data.frame(missingness = rep(NA, 66))

    row.names(missingness)[1:60] <- names(binary_data)
    row.names(missingness)[61:65] <- c("dv_sr_sleep_abn_cat", "dv_sd_sleep_abn_cat", "dv_pal_sleep_abn_cat", "dv_vdb_sleep_abn_cat", "dv_winkler_sleep_abn_cat")
    row.names(missingness)[66] <- "TOTAL"

    for(i in 1:60){
        missingness$missingness[i] <- sum(is.na(binary_data[, row.names(missingness)[i]])) / nrow(binary_data) * 100
    }
    for(i in 61:65){
        missingness$missingness[i] <- sum(is.na(multi_data[, row.names(missingness)[i]])) / nrow(multi_data) * 100
    }

    missingness$missingness[66] <- sum(is.na(binary_data)) / nrow(binary_data) / ncol(binary_data) * 100

    return(missingness)
}

summarise_sleep <- function(binary_data, multi_data){
    sleep_summary <- data.frame(normal_binary = rep(NA, 5), abnormal = rep(NA, 5), short = rep(NA, 5), normal_multi = rep(NA, 5), long = rep(NA, 5))

    row.names(sleep_summary) <- c("Self-report", "Sleep diary", "activPAL", "van der Berg et al.", "Winkler et al.")

    vars <- c("sr", "sd", "pal", "vdb", "winkler")

    binary_long <- complete(binary_data, "long")
    multi_long <- complete(multi_data, "long")

    for(i in seq_along(vars)){
        sleep_summary[i, "normal_binary"] <- sum(binary_long[, paste("dv", vars[i], "sleep_abn", sep = "_")] == 0) / nrow(binary_long) * 100
        sleep_summary[i, "abnormal"] <- sum(binary_long[, paste("dv", vars[i], "sleep_abn", sep = "_")] == 1) / nrow(binary_long) * 100
        sleep_summary[i, "short"] <- sum(multi_long[, paste("dv", vars[i], "sleep_abn_cat", sep = "_")] == "Short") / nrow(multi_long) * 100
        sleep_summary[i, "normal_multi"] <- sum(multi_long[, paste("dv", vars[i], "sleep_abn_cat", sep = "_")] == "Normal") / nrow(multi_long) * 100
        sleep_summary[i, "long"] <- sum(multi_long[, paste("dv", vars[i], "sleep_abn_cat", sep = "_")] == "Long") / nrow(multi_long) * 100
    }

    return(sleep_summary)
}