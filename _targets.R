# Dependencies
library(targets)
library(tarchetypes)
library(data.table)
library(dplyr)
library(forestplot)
library(ggplot2)
library(grid)
library(gridExtra)
library(haven)
library(lmtest)
library(magrittr)
library(mice)
library(nnet)
library(parallel)
library(purrr)
library(quarto)
library(sandwich)

# Including auxiliary functions
source("R/functions.R")

# Setting random seed
set.seed(7917)

list(
    ## Loading individual datasets, selecting variables of interest and defining missing values
    # Birth sweep
    tar_target(
        birth,
        {
            birth <- read_dta("data/raw/1a_Birth_&_22_month/UKDA-2666-stata9/stata9/bcs7072a.dta") %>% select(
                bcsid, # BCS70 serial number
                a0005a, # Maternal age at birth
                a0009, # Mother's age at completion of education
                a0010, # Father's age at completion of education
                a0014, # Social class of father in 1970
                a0043b, # Smoking during pregnancy
                a0195a, # Gestational age in days
                a0230, # Occurrence of genital tract bleeding*
                a0255, # Sex of the baby*
                a0278 # Birthweight
            )

            is.na(birth[,]) <- birth[,] < 0

            return(birth)
        }
    ),

    # Age 5 sweep (D)
    tar_target(
        age_5d,
        {
            age_5d <- read_dta("data/raw/2_age_5/UKDA-2699-stata/stata/stata11/f699a.dta") %>% select(
                bcsid, # BCS70 serial number
                d016a, # Bedwetting
                d119 # Rutter score
            )

            is.na(age_5d[,]) <- age_5d[,] < 0

            return(age_5d)
        }
    ),

    # Age 5 sweep (E)
    tar_target(
        age_5e,
        {
            age_5e <- read_dta("data/raw/2_age_5/UKDA-2699-stata/stata/stata11/f699b.dta") %>% select(
                bcsid, # BCS70 serial number
                e067:e076, # Medical conditions
                e192, # Age father left school*
                e197, # Father's social class
                e206, # Mother's social class
                e216a # Mother's regular job since child's birth
            )

            is.na(age_5e[,]) <- age_5e[,] < 0

            return(age_5e)
        }
    ),

    # Age 5 sweep (F)
    tar_target(
        age_5f_prepca,
        {
            age_5f_prepca <- read_dta("data/raw/2_age_5/UKDA-2699-stata/stata/stata11/f699c.dta") %>% select(
                bcsid, # BCS70 serial number
                f100, # Schonell Reading Test score
                f118, # Complete a Profile Test score
                f121, # Standardised Human Figure Drawing score
                f122 # Standardised Copying Designs score
            )

            is.na(age_5f_prepca[c("f100", "f118")]) <- age_5f_prepca[c("f100", "f118")] < 0

            is.na(age_5f_prepca[c("f121", "f122")]) <- age_5f_prepca[c("f121", "f122")] == -5

            return(age_5f_prepca)
        }
    ),

    # Age 5 sweep (derived variables)
    tar_target(
        age_5dv,
        {
            age_5dv <- read_dta("data/raw/2_age_5/UKDA-2699-stata/stata/stata11/bcs2derived.dta") %>% rename(
                bcsid = BCSID
            ) %>% select(
                bcsid, # BCS70 serial number
                BD2READ # Standardised English Picture Vocabulary Test score
            )

            is.na(age_5dv[,]) <- age_5dv[,] == -1

            return(age_5dv)
        }
    ),

    # Age 10 sweep
    tar_target(
        age_10_prepca,
        {
            age_10_prepca <- read_dta("data/raw/3_age_10/UKDA-3723-stata/stata/stata11_se/sn3723.dta") %>% select(
                bcsid, # BCS70 serial number
                i3504:i3540, # BAS word definitions
                i3541:i3574, # BAS recall of digits
                i3617:i3644, # BAS matrices
                i4201:i4221, # BAS word similarities (derived)
                j111, # Total days missed schooling*
                j127:j178b, # Child Development Scale
                m84:m101, # Positive activities outside school*
                meb4_1, meb4_6, meb4_16, meb4_21, meb4_26, meb4_46, meb4_51, # Medical conditions (present)
                meb4_2:meb4_4, meb4_7:meb4_9, meb4_17:meb4_19, meb4_22:meb4_24, meb4_27:meb4_29, meb4_47:meb4_49, meb4_52:meb4_54, # Medical conditions (absent)
                tests10 # Maths test, reading test, BAS test completed
            )

            is.na(age_10_prepca[, grep("j127", names(age_10_prepca)):grep("j178b", names(age_10_prepca))]) <- age_10_prepca[, grep("j127", names(age_10_prepca)):grep("j178b", names(age_10_prepca))] < 0

            is.na(age_10_prepca[, grep("m84", names(age_10_prepca)):grep("m101", names(age_10_prepca))]) <- age_10_prepca[, grep("m84", names(age_10_prepca)):grep("m101", names(age_10_prepca))] < 0

            return(age_10_prepca)
        }
    ),

    # Age 10 sweep (derived variables)
    tar_target(
        age_10dv,
        {
            age_10dv <- read_dta("data/raw/3_age_10/UKDA-3723-stata/stata/stata11_se/bcs3derived.dta") %>% rename(
                bcsid = BCSID
            ) %>%
            select(
                bcsid, # BCS70 serial number
                BD3MRUTT # Rutter score
            )

            is.na(age_10dv[,]) <- age_10dv[,] < 0

            return(age_10dv)
        }
    ),

    # Age 16 sweep
    tar_target(
        age_16,
        {
            age_16 <- read_dta("data/raw/4_age_16/UKDA-3535-stata/stata/stata13_se/bcs7016x.dta") %>% select(
                bcsid, # BCS70 serial number
                contains("hd5_"), # Alcohol in last 7 days*
                contains("oe1_"), # Sources of income*
                rd6m_1, # Behavioural or emotional problems
                t11_2, # Father's social class
                t11_9 # Mother's social class
            )

            is.na(age_16[,]) <- age_16[,] < 0

            return(age_16)
        }
    ),

    # Age 16 sweep (arithmetic test scores)
    tar_target(
        age_16arith,
        read_dta("data/raw/4a_age_16_arithmetic/UKDA-6095-stata/stata/stata11/bcs70_16-year_arithmetic_data.dta") %>% select(
            bcsid, # BCS70 serial number
            mathscore # Arithmetic test score*
        )
    ),

    # Age 16 sweep (derived variables)
    tar_target(
        age_16dv,
        {
            age_16dv <- read_dta("data/raw/4_age_16/UKDA-3535-stata/stata/stata13_se/bcs4derived.dta") %>% rename(
                bcsid = BCSID
            ) %>% select(
                bcsid, # BCS70 serial number
                BD4MAL, # Total Malaise score
            )

            is.na(age_16dv[,]) <- age_16dv[,] < 0

            return(age_16dv)
        }
    ),

    # Age 16 sweep (school type)
    tar_target(
        age_16school,
        {
            age_16school <- read_dta("data/raw/4_age_16/UKDA-3535-stata/stata/stata13_se/bcs70_age16_school_type.dta") %>% rename(
                bcsid = BCSID
            )

            is.na(age_16school[,]) <- age_16school[,] < 0

            return(age_16school)
        }
    ),

    # Age 26 sweep
    tar_target(
        age_26,
        {
            age_26 <- read_dta("data/raw/5_age_26/UKDA-3833-stata/stata/stata11/bcs96x.dta") %>% select(
                bcsid, # BCS70 serial number
                b960539, b960540 # Accidents and assaults*
            )

            is.na(age_26[,]) <- age_26[,] < 0

            return(age_26)
        }
    ),

    # Age 30 sweep
    tar_target(
        age_30,
        {
            age_30 <- read_dta("data/raw/6_age_30/UKDA-5558-stata/stata/stata11_se/bcs2000.dta") %>% select(
                bcsid, # BCS70 serial number
                aptrain, # Apprenticeships*
                emosup, # People for advice/support*
                hospital, numadmn, # Hospital admissions*
                wrktrain, # Work-related training*
                vote97 # Voted in 1997 General Election*
            )

            is.na(age_30[, "numadmn"]) <- age_30[, "numadmn"] < 0

            is.na(age_30[c("aptrain", "wrktrain", "emosup", "hospital", "vote97")]) <- age_30[c("aptrain", "wrktrain", "emosup", "hospital", "vote97")] > 7

            return(age_30)
        }
    ),

    # Age 42 sweep
    tar_target(
        age_42,
        {
            age_42 <- read_dta("data/raw/9_age_42/UKDA-7473-stata/stata/stata13/bcs70_2012_flatfile.dta") %>% rename(
                bcsid = BCSID
            ) %>% select(
                bcsid, # BCS70 serial number
                B9DIVCHK, B9HMS, B9MARCHK, # Marital status*
                contains("B9SCQ8"), # Organisational membership*
                B9SCQ17, # Computer ownership*
                B9SCQ19, # Number of cars or vans owned*
                B9TEN, # Housing tenure*
            )

            is.na(age_42[,]) <- age_42[,] < 0

            return(age_42)
        }
    ),

    # Age 42 sweep (derived variables)
    tar_target(
        age_42dv,
        {
            age_42dv <- read_dta("data/raw/9_age_42/UKDA-7473-stata/stata/stata13/bcs70_2012_derived.dta") %>% rename(
                bcsid = BCSID   
            ) %>% select(   
                bcsid, # BCS70 serial number    
                BD9MAL, # Malaise score 
                BD9WEMWB # Warwick Edinburgh Mental Well-Being Scale    
            )

            is.na(age_42dv[,]) <- age_42dv[,] < 0

            return(age_42dv)
        }
    ),

    # Age 46 sweep
    tar_target(
        age_46,
        {
            age_46 <- read_dta("data/raw/10_age_46/UKDA-8547-stata/stata/stata11/bcs_age46_main.dta") %>% rename(
                bcsid = BCSID
            ) %>% select(
                bcsid, # BCS70 serial number
                B10HSLEEP # Average number of hours of sleep per night
            )

            is.na(age_46[,]) <- age_46[,] < 0

            return(age_46)
        }
    ),

    # CLOSER WP9 (childhood environment and adult wellbeing)
    tar_target(
        CLOSER_env,
        {
            CLOSER_env <- read_dta("data/raw/CLOSER_childhood_environment_adult_wellbeing/UKDA-8553-stata/stata/stata13/bcs70_closer_wp9.dta") %>% select(
                bcsid, # BCS70 serial number
                ameni, # Lacking sole use of amenities
                brfed, # Whether breastfed
                crowd, # Crowding in childhood
                divorce, # Parents divorced during study member's childhood
                extbcsz, # Externalising behaviour in childhood, standardised
                intbcsz, # Internalising behaviour in childhood, standardised
                prmnh, # Poor maternal / familial mental health
                resmove, # Number of residential moves in childhood
                sepmumbcs, # Ever separated from mother
                tenure # Accommodation owned or rented in childhood
            )

                is.na(CLOSER_env[c("ameni", "brfed", "crowd", "divorce", "prmnh", "resmove", "sepmumbcs", "tenure")]) <- CLOSER_env[c("ameni", "brfed", "crowd", "divorce", "prmnh", "resmove", "sepmumbcs", "tenure")] < 0

                is.na(CLOSER_env[c("intbcsz", "extbcsz")]) <- CLOSER_env[c("intbcsz", "extbcsz")] < -100

                return(CLOSER_env)
        }
    ),

    # CLOSER WP1 (height, weight and BMI)
    tar_target(
        CLOSER_BMI,
        read_dta("data/raw/CLOSER_height_weight_BMI/UKDA-8551-stata/stata/stata13/bcs70_closer_wp1.dta") %>% filter(
            visitage == 10
        ) %>% select(
            bcsid, # BCS70 serial number
            bmi # Body mass index
        )
    ),

    # CLOSER WP2 (socioeconomic measures)
    tar_target(
        CLOSER_SES,
        {
            CLOSER_SES <- read_dta("data/raw/CLOSER_socioeconomic_measures/UKDA-8305-stata/stata/stata13/bcs70_closer_wp2ses.dta") %>% select(
                bcsid, # BCS70 serial number
                fclrg90 # Father's social class (RG 1990 version) at age 10 sweep
            )

            is.na(CLOSER_SES[,]) <- CLOSER_SES[,] < 0

            return(CLOSER_SES)
        }
    ),

    # Response dataset
    tar_target(
        response,
        read_dta("data/raw/Response_dataset/UKDA-5641-stata/stata/stata13/bcs70_response_1970-2016.dta") %>% rename(
            bcsid = BCSID
        ) %>% select(
            bcsid, # BCS70 serial number
            contains("OUTCME") # Outcome variables
        )
    ),

    # activPAL algorithm sleep estimates
    tar_target(
        activpal,
        fread("data/raw/10b_age_46_sleep_data/activpal_sleep_bcsid.csv") %>% rename(
            bcsid = BCSID
        )
    ),

    # Sleep diary data
    tar_target(
        diary,
        read_sav("data/raw/10b_age_46_sleep_data/diary_cleaned_bcsid.sav") %>% rename(
            bcsid = BCSID
        ) %>% select(
            bcsid,
            md_sd_sleepdur, # Median sleep duration
            md_sleeprate # Median sleep rating
        )
    ),

    # van der Berg et al. algorithm sleep estimates
    tar_target(
        vdb,
        fread("data/raw/10b_age_46_sleep_data/vanderberg_sleep_bcsid.csv") %>% rename(
            bcsid = BCSID
        )
    ),

    # Winkler et al. algorithm sleep estimates
    tar_target(
        winkler,
        fread("data/raw/10b_age_46_sleep_data/winkler_sleep_bcsid.csv") %>% rename(
            bcsid = BCSID
        )
    ),

    # Performing principal component analysis on cognitive ability measures at ages 5 and 10
    tar_target(
        age_5f,
        cog_abil_pca_5(
            age_5f_prepca, 
            age_5dv
        )
    ),

    tar_target(
        age_10,
        cog_abil_pca_10(
            age_10_prepca
        )
    ),

    # Combining individual datasets into a single data frame
    tar_target(
        data_merged,
        merge_datasets(
            birth,
            age_5d,
            age_5e,
            age_5f,
            age_10,
            age_10dv,
            age_16,
            age_16arith,
            age_16dv,
            age_16school,
            age_26,
            age_30,
            age_42,
            age_42dv,
            age_46,
            CLOSER_env,
            CLOSER_BMI,
            CLOSER_SES,
            response,
            activpal,
            diary,
            vdb,
            winkler
        )
    ),

    # Removing dead/emigrants
    tar_target(
        data_rm_dead,
        remove_dead_emigrants(
            data_merged
        )
    ),

    # Deriving variables
    tar_target(
        data_clean_multi,
        derive_vars(
            data_rm_dead
        )
    ),

    # Producing version with binary sleep variables
    tar_target(
        data_clean_binary,
        binary_sleep(
            data_clean_multi
        )
    ),

    # Performing multiple imputation
    tar_target(
        data_imputed_binary,
        multiple_imputation(
            data_clean_binary
        )
    ),

    tar_target(
        data_imputed_multi,
        multiple_imputation(
            data_clean_multi
        )
    ),
    
    # Producing report with trace plots
    tar_quarto(
        mice_report,
        "mice_report.Qmd"
    ),

    # Performing Poisson regression analyses
    tar_target(
        rutter_5,
        pois_regr(
            "d119 + a0005a + a0043b + a0195a + a0278 + a0014 + dv_par_edu_birth + tenure + prmnh + crowd + ameni + brfed",
            "d119",
            data_imputed_binary
        )
    ),

    tar_target(
        rutter_5_summary,
        pois_results_summarise(
            rutter_5,
            "d119"
        )
    ),

    tar_target(
        rutter_10,
        pois_regr(
            "BD3MRUTT + a0005a + a0043b + a0195a + a0278 + a0014 + dv_par_edu_birth + dv_sc_age_5 + e216a + dv_cog_abil_5 + dv_med_5 + d016a + bmi + fclrg90 + tenure + sepmumbcs + prmnh + crowd + ameni + brfed + resmove",
            "BD3MRUTT",
            data_imputed_binary
        )
    ),

    tar_target(
        rutter_10_summary,
        pois_results_summarise(
            rutter_10,
            "BD3MRUTT"
        )
    ),

    tar_target(
        cds,
        pois_regr(
            "dv_cds_10 + a0005a + a0043b + a0195a + a0278 + a0014 + dv_par_edu_birth + dv_sc_age_5 + e216a + dv_cog_abil_5 + dv_med_5 + d016a + bmi + fclrg90 + tenure + sepmumbcs + prmnh + crowd + ameni + brfed + resmove",
            "dv_cds_10",
            data_imputed_binary
        )
    ),

    tar_target(
        cds_summary,
        pois_results_summarise(
            cds,
            "dv_cds_10"
        )
    ),

    tar_target(
        malaise,
        pois_regr(
            "BD4MAL + a0005a + a0043b + a0195a + a0278 + a0014 + dv_par_edu_birth + dv_sc_age_5 + e216a + dv_cog_abil_5 + dv_med_5 + d016a + dv_bas_g + dv_med_10 + dv_sc_age_16 + bmi + fclrg90 + tenure + divorce + sepmumbcs + prmnh + crowd + ameni + brfed + resmove",
            "BD4MAL",
            data_imputed_binary
        )
    ),

    tar_target(
        malaise_summary,
        pois_results_summarise(
            malaise,
            "BD4MAL"
        )
    ),

    tar_target(
        beh_emo,
        pois_regr(
            "rd6m_1 + a0005a + a0043b + a0195a + a0278 + a0014 + dv_par_edu_birth + dv_sc_age_5 + e216a + dv_cog_abil_5 + dv_med_5 + d016a + dv_bas_g + dv_med_10 + dv_sc_age_16 + bmi + fclrg90 + tenure + divorce + sepmumbcs + prmnh + crowd + ameni + brfed + resmove",
            "rd6m_1",
            data_imputed_binary
        )
    ),

    tar_target(
        beh_emo_summary,
        pois_results_summarise(
            beh_emo,
            "rd6m_1"
        )
    ),

    tar_target(
        int_beh,
        pois_regr(
            "intbcsz + a0005a + a0043b + a0195a + a0278 + a0014 + dv_par_edu_birth + dv_sc_age_5 + e216a + dv_cog_abil_5 + dv_med_5 + d016a + dv_bas_g + dv_med_10 + dv_sc_age_16 + bmi + fclrg90 + tenure + divorce + sepmumbcs + prmnh + crowd + ameni + brfed + resmove",
            "intbcsz",
            data_imputed_binary
        )
    ),

    tar_target(
        int_beh_summary,
        pois_results_summarise(
            int_beh,
            "intbcsz"
        )
    ),

    tar_target(
        ext_beh,
        pois_regr(
            "extbcsz + a0005a + a0043b + a0195a + a0278 + a0014 + dv_par_edu_birth + dv_sc_age_5 + e216a + dv_cog_abil_5 + dv_med_5 + d016a + dv_bas_g + dv_med_10 + dv_sc_age_16 + bmi + fclrg90 + tenure + divorce + sepmumbcs + prmnh + crowd + ameni + brfed + resmove",
            "extbcsz",
            data_imputed_binary
        )
    ),

    tar_target(
        ext_beh_summary,
        pois_results_summarise(
            ext_beh,
            "extbcsz"
        )
    ),

    # Performing multinomial regression analyses
    tar_target(
        rutter_5_multi,
        multinom_regr(
            "d119 + a0005a + a0043b + a0195a + a0278 + a0014 + dv_par_edu_birth + tenure + prmnh + crowd + ameni + brfed",
            "d119",
            data_imputed_multi
        )
    ),

    tar_target(
        rutter_5_multi_summary,
        multinom_results_summarise(
            rutter_5_multi,
            "d119"
        )
    ),

    tar_target(
        rutter_10_multi,
        multinom_regr(
            "BD3MRUTT + a0005a + a0043b + a0195a + a0278 + a0014 + dv_par_edu_birth + dv_sc_age_5 + e216a + dv_cog_abil_5 + dv_med_5 + d016a + bmi + fclrg90 + tenure + sepmumbcs + prmnh + crowd + ameni + brfed + resmove", 
            "BD3MRUTT", 
            data_imputed_multi
        )
    ),

    tar_target(
        rutter_10_multi_summary,
        multinom_results_summarise(
            rutter_10_multi,
            "BD3MRUTT"
        )
    ),

    tar_target(
        cds_multi,
        multinom_regr(
            "dv_cds_10 + a0005a + a0043b + a0195a + a0278 + a0014 + dv_par_edu_birth + dv_sc_age_5 + e216a + dv_cog_abil_5 + dv_med_5 + d016a + bmi + fclrg90 + tenure + sepmumbcs + prmnh + crowd + ameni + brfed + resmove",
            "dv_cds_10",
            data_imputed_multi
        )
    ),

    tar_target(
        cds_multi_summary,
        multinom_results_summarise(
            cds_multi,
            "dv_cds_10"
        )
    ),

    tar_target(
        malaise_multi,
        multinom_regr(
            "BD4MAL + a0005a + a0043b + a0195a + a0278 + a0014 + dv_par_edu_birth + dv_sc_age_5 + e216a + dv_cog_abil_5 + dv_med_5 + d016a + dv_bas_g + dv_med_10 + dv_sc_age_16 + bmi + fclrg90 + tenure + divorce + sepmumbcs + prmnh + crowd + ameni + brfed + resmove",
            "BD4MAL",
            data_imputed_multi
        )
    ),

    tar_target(
        malaise_multi_summary,
        multinom_results_summarise(
            malaise_multi,
            "BD4MAL"
        )
    ),

    tar_target(
        beh_emo_multi,
        multinom_regr(
            "rd6m_1 + a0005a + a0043b + a0195a + a0278 + a0014 + dv_par_edu_birth + dv_sc_age_5 + e216a + dv_cog_abil_5 + dv_med_5 + d016a + dv_bas_g + dv_med_10 + dv_sc_age_16 + bmi + fclrg90 + tenure + divorce + sepmumbcs + prmnh + crowd + ameni + brfed + resmove",
            "rd6m_1",
            data_imputed_multi
        )
    ),

    tar_target(
        beh_emo_multi_summary,
        multinom_results_summarise(
            beh_emo_multi,
            "rd6m_1"
        )
    ),

    tar_target(
        int_beh_multi,
        multinom_regr(
            "intbcsz + a0005a + a0043b + a0195a + a0278 + a0014 + dv_par_edu_birth + dv_sc_age_5 + e216a + dv_cog_abil_5 + dv_med_5 + d016a + dv_bas_g + dv_med_10 + dv_sc_age_16 + bmi + fclrg90 + tenure + divorce + sepmumbcs + prmnh + crowd + ameni + brfed + resmove",
            "intbcsz",
            data_imputed_multi
        )
    ),

    tar_target(
        int_beh_multi_summary,
        multinom_results_summarise(
            int_beh_multi,
            "intbcsz"
        )
    ),

    tar_target(
        ext_beh_multi,
        multinom_regr(
            "extbcsz + a0005a + a0043b + a0195a + a0278 + a0014 + dv_par_edu_birth + dv_sc_age_5 + e216a + dv_cog_abil_5 + dv_med_5 + d016a + dv_bas_g + dv_med_10 + dv_sc_age_16 + bmi + fclrg90 + tenure + divorce + sepmumbcs + prmnh + crowd + ameni + brfed + resmove",
            "extbcsz",
            data_imputed_multi
        )
    ),

    tar_target(
        ext_beh_multi_summary,
        multinom_results_summarise(
            ext_beh_multi,
            "extbcsz"
        )
    ),

    # Performing mediation analyses
    tar_target(
        rutter_5_med1,
        med_pois_regr(
            "d119 + a0005a + a0043b + a0195a + a0278 + a0014 + dv_par_edu_birth + tenure + prmnh + crowd + ameni + brfed",
            "d119",
            data_imputed_binary
        )
    ),

    tar_target(
        rutter_5_med1_summary,
        pois_results_summarise(
            rutter_5_med1,
            "d119"
        )
    ),

    tar_target(
        rutter_5_med2,
        pois_regr(
            "d119 + BD9MAL + BD9WEMWB + a0005a + a0043b + a0195a + a0278 + a0014 + dv_par_edu_birth + tenure + prmnh + crowd + ameni + brfed",
            "d119",
            data_imputed_binary
        )
    ),

    tar_target(
        rutter_5_med2_summary,
        pois_results_summarise(
            rutter_5_med2,
            "d119"
        )
    ),
    
    tar_target(
        rutter_10_med1,
        med_pois_regr(
            "BD3MRUTT + a0005a + a0043b + a0195a + a0278 + a0014 + dv_par_edu_birth + dv_sc_age_5 + e216a + dv_cog_abil_5 + dv_med_5 + d016a + bmi + fclrg90 + tenure + sepmumbcs + prmnh + crowd + ameni + brfed + resmove",
            "BD3MRUTT",
            data_imputed_binary
        )
    ),

    tar_target(
        rutter_10_med1_summary,
        pois_results_summarise(
            rutter_10_med1,
            "BD3MRUTT"
        )
    ),

    tar_target(
        rutter_10_med2,
        pois_regr(
            "BD3MRUTT + BD9MAL + BD9WEMWB + a0005a + a0043b + a0195a + a0278 + a0014 + dv_par_edu_birth + dv_sc_age_5 + e216a + dv_cog_abil_5 + dv_med_5 + d016a + bmi + fclrg90 + tenure + sepmumbcs + prmnh + crowd + ameni + brfed + resmove",
            "BD3MRUTT",
            data_imputed_binary
        )
    ),

    tar_target(
        rutter_10_med2_summary,
        pois_results_summarise(
            rutter_10_med2,
            "BD3MRUTT"
        )
    ),

    tar_target(
        cds_med1,
        med_pois_regr(
            "dv_cds_10 + a0005a + a0043b + a0195a + a0278 + a0014 + dv_par_edu_birth + dv_sc_age_5 + e216a + dv_cog_abil_5 + dv_med_5 + d016a + bmi + fclrg90 + tenure + sepmumbcs + prmnh + crowd + ameni + brfed + resmove",
            "dv_cds_10",
            data_imputed_binary
        )
    ),

    tar_target(
        cds_med1_summary,
        pois_results_summarise(
            cds_med1,
            "dv_cds_10"
        )
    ),

    tar_target(
        cds_med2,
        pois_regr(
            "dv_cds_10 + BD9MAL + BD9WEMWB + a0005a + a0043b + a0195a + a0278 + a0014 + dv_par_edu_birth + dv_sc_age_5 + e216a + dv_cog_abil_5 + dv_med_5 + d016a + bmi + fclrg90 + tenure + sepmumbcs + prmnh + crowd + ameni + brfed + resmove",
            "dv_cds_10",
            data_imputed_binary
        )
    ),

    tar_target(
        cds_med2_summary,
        pois_results_summarise(
            cds_med2,
            "dv_cds_10"
        )
    ),

    tar_target(
        malaise_med1,
        med_pois_regr(
            "BD4MAL + a0005a + a0043b + a0195a + a0278 + a0014 + dv_par_edu_birth + dv_sc_age_5 + e216a + dv_cog_abil_5 + dv_med_5 + d016a + dv_bas_g + dv_med_10 + dv_sc_age_16 + bmi + fclrg90 + tenure + divorce + sepmumbcs + prmnh + crowd + ameni + brfed + resmove",
            "BD4MAL",
            data_imputed_binary
        )
    ),

    tar_target(
        malaise_med1_summary,
        pois_results_summarise(
            malaise_med1,
            "BD4MAL"
        )
    ),

    tar_target(
        malaise_med2,
        pois_regr(
            "BD4MAL + BD9MAL + BD9WEMWB + a0005a + a0043b + a0195a + a0278 + a0014 + dv_par_edu_birth + dv_sc_age_5 + e216a + dv_cog_abil_5 + dv_med_5 + d016a + dv_bas_g + dv_med_10 + dv_sc_age_16 + bmi + fclrg90 + tenure + divorce + sepmumbcs + prmnh + crowd + ameni + brfed + resmove",
            "BD4MAL",
            data_imputed_binary
        )
    ),

    tar_target(
        malaise_med2_summary,
        pois_results_summarise(
            malaise_med2,
            "BD4MAL"
        )
    ),

    tar_target(
        beh_emo_med1,
        med_pois_regr(
            "rd6m_1 + a0005a + a0043b + a0195a + a0278 + a0014 + dv_par_edu_birth + dv_sc_age_5 + e216a + dv_cog_abil_5 + dv_med_5 + d016a + dv_bas_g + dv_med_10 + dv_sc_age_16 + bmi + fclrg90 + tenure + divorce + sepmumbcs + prmnh + crowd + ameni + brfed + resmove",
            "rd6m_1",
            data_imputed_binary
        )
    ),

    tar_target(
        beh_emo_med1_summary,
        pois_results_summarise(
            beh_emo_med1,
            "rd6m_1"
        )
    ),

    tar_target(
        beh_emo_med2,
        pois_regr(
            "rd6m_1 + BD9MAL + BD9WEMWB + a0005a + a0043b + a0195a + a0278 + a0014 + dv_par_edu_birth + dv_sc_age_5 + e216a + dv_cog_abil_5 + dv_med_5 + d016a + dv_bas_g + dv_med_10 + dv_sc_age_16 + bmi + fclrg90 + tenure + divorce + sepmumbcs + prmnh + crowd + ameni + brfed + resmove",
            "rd6m_1",
            data_imputed_binary
        )
    ),

    tar_target(
        beh_emo_med2_summary,
        pois_results_summarise(
            beh_emo_med2,
            "rd6m_1"
        )
    ),
    
    tar_target(
        int_beh_med1,
        med_pois_regr(
            "intbcsz + a0005a + a0043b + a0195a + a0278 + a0014 + dv_par_edu_birth + dv_sc_age_5 + e216a + dv_cog_abil_5 + dv_med_5 + d016a + dv_bas_g + dv_med_10 + dv_sc_age_16 + bmi + fclrg90 + tenure + divorce + sepmumbcs + prmnh + crowd + ameni + brfed + resmove",
            "intbcsz",
            data_imputed_binary
        )
    ),

    tar_target(
        int_beh_med1_summary,
        pois_results_summarise(
            int_beh_med1,
            "intbcsz"
        )
    ),

    tar_target(
        int_beh_med2,
        pois_regr(
            "intbcsz + BD9MAL + BD9WEMWB + a0005a + a0043b + a0195a + a0278 + a0014 + dv_par_edu_birth + dv_sc_age_5 + e216a + dv_cog_abil_5 + dv_med_5 + d016a + dv_bas_g + dv_med_10 + dv_sc_age_16 + bmi + fclrg90 + tenure + divorce + sepmumbcs + prmnh + crowd + ameni + brfed + resmove",
            "intbcsz",
            data_imputed_binary
        )
    ),

    tar_target(
        int_beh_med2_summary,
        pois_results_summarise(
            int_beh_med2,
            "intbcsz"
        )
    ),

    tar_target(
        ext_beh_med1,
        med_pois_regr(
            "extbcsz + a0005a + a0043b + a0195a + a0278 + a0014 + dv_par_edu_birth + dv_sc_age_5 + e216a + dv_cog_abil_5 + dv_med_5 + d016a + dv_bas_g + dv_med_10 + dv_sc_age_16 + bmi + fclrg90 + tenure + divorce + sepmumbcs + prmnh + crowd + ameni + brfed + resmove",
            "extbcsz",
            data_imputed_binary
        )
    ),

    tar_target(
        ext_beh_med1_summary,
        pois_results_summarise(
            ext_beh_med1,
            "extbcsz"
        )
    ),

    tar_target(
        ext_beh_med2,
        pois_regr(
            "extbcsz + BD9MAL + BD9WEMWB + a0005a + a0043b + a0195a + a0278 + a0014 + dv_par_edu_birth + dv_sc_age_5 + e216a + dv_cog_abil_5 + dv_med_5 + d016a + dv_bas_g + dv_med_10 + dv_sc_age_16 + bmi + fclrg90 + tenure + divorce + sepmumbcs + prmnh + crowd + ameni + brfed + resmove",
            "extbcsz",
            data_imputed_binary
        )
    ),

    tar_target(
        ext_beh_med2_summary,
        pois_results_summarise(
            ext_beh_med2,
            "extbcsz"
        )
    ),

    # Producing male-only and female-only datasets
    tar_target(
        male_data_binary,
        filter(data_imputed_binary, a0255 == "Male")
    ),

    tar_target(
        female_data_binary,
        filter(data_imputed_binary, a0255 == "Female")
    ),

    tar_target(
        male_data_multi,
        filter(data_imputed_multi, a0255 == "Male")
    ),

    tar_target(
        female_data_multi,
        filter(data_imputed_multi, a0255 == "Female")
    ),

    # Repeating all analyses disaggregated by sex
    tar_target(
        rutter_5_male,
        pois_regr(
            "d119 + a0005a + a0043b + a0195a + a0278 + a0014 + dv_par_edu_birth + tenure + prmnh + crowd + ameni + brfed", 
            "d119",
            male_data_binary
        )
    ),

    tar_target(
        rutter_5_male_summary,
        pois_results_summarise(
            rutter_5_male,
            "d119"
        )
    ),

    tar_target(
        rutter_5_multi_male,
        multinom_regr(
            "d119 + a0005a + a0043b + a0195a + a0278 + a0014 + dv_par_edu_birth + tenure + prmnh + crowd + ameni + brfed",
            "d119",
            male_data_multi
        )
    ),

    tar_target(
        rutter_5_multi_male_summary,
        multinom_results_summarise(
            rutter_5_multi_male,
            "d119"
        )
    ),

    tar_target(
        rutter_5_female,
        pois_regr(
            "d119 + a0005a + a0043b + a0195a + a0278 + a0014 + dv_par_edu_birth + tenure + prmnh + crowd + ameni + brfed", 
            "d119",
            female_data_binary
        )
    ),

    tar_target(
        rutter_5_female_summary,
        pois_results_summarise(
            rutter_5_female,
            "d119"
        )
    ),

    tar_target(
        rutter_5_multi_female,
        multinom_regr(
            "d119 + a0005a + a0043b + a0195a + a0278 + a0014 + dv_par_edu_birth + tenure + prmnh + crowd + ameni + brfed",
            "d119",
            female_data_multi
        )
    ),

    tar_target(
        rutter_5_multi_female_summary,
        multinom_results_summarise(
            rutter_5_multi_female,
            "d119"
        )
    ),

    tar_target(
        rutter_10_male,
        pois_regr(
            "BD3MRUTT + a0005a + a0043b + a0195a + a0278 + a0014 + dv_par_edu_birth + dv_sc_age_5 + e216a + dv_cog_abil_5 + dv_med_5 + d016a + bmi + fclrg90 + tenure + sepmumbcs + prmnh + crowd + ameni + brfed + resmove",
            "BD3MRUTT",
            male_data_binary
        )
    ),

    tar_target(
        rutter_10_male_summary,
        pois_results_summarise(
            rutter_10_male,
            "BD3MRUTT"
        )
    ),

    tar_target(
        rutter_10_multi_male,
        multinom_regr(
            "BD3MRUTT + a0005a + a0043b + a0195a + a0278 + a0014 + dv_par_edu_birth + dv_sc_age_5 + e216a + dv_cog_abil_5 + dv_med_5 + d016a + bmi + fclrg90 + tenure + sepmumbcs + prmnh + crowd + ameni + brfed + resmove",
            "BD3MRUTT",
            male_data_multi
        )
    ),

    tar_target(
        rutter_10_multi_male_summary,
        multinom_results_summarise(
            rutter_10_multi_male,
            "BD3MRUTT"
        )
    ),

    tar_target(
        rutter_10_female,
        pois_regr(
            "BD3MRUTT + a0005a + a0043b + a0195a + a0278 + a0014 + dv_par_edu_birth + dv_sc_age_5 + e216a + dv_cog_abil_5 + dv_med_5 + d016a + bmi + fclrg90 + tenure + sepmumbcs + prmnh + crowd + ameni + brfed + resmove",
            "BD3MRUTT",
            female_data_binary
        )
    ),

    tar_target(
        rutter_10_female_summary,
        pois_results_summarise(
            rutter_10_female,
            "BD3MRUTT"
        )
    ),

    tar_target(
        rutter_10_multi_female,
        multinom_regr(
            "BD3MRUTT + a0005a + a0043b + a0195a + a0278 + a0014 + dv_par_edu_birth + dv_sc_age_5 + e216a + dv_cog_abil_5 + dv_med_5 + d016a + bmi + fclrg90 + tenure + sepmumbcs + prmnh + crowd + ameni + brfed + resmove",
            "BD3MRUTT",
            female_data_multi
        )
    ),

    tar_target(
        rutter_10_multi_female_summary,
        multinom_results_summarise(
            rutter_10_multi_female,
            "BD3MRUTT"
        )
    ),

    tar_target(
        cds_male,
        pois_regr(
            "dv_cds_10 + a0005a + a0043b + a0195a + a0278 + a0014 + dv_par_edu_birth + dv_sc_age_5 + e216a + dv_cog_abil_5 + dv_med_5 + d016a + bmi + fclrg90 + tenure + sepmumbcs + prmnh + crowd + ameni + brfed + resmove",
            "dv_cds_10",
            male_data_binary
        )
    ),

    tar_target(
        cds_male_summary,
        pois_results_summarise(
            cds_male,
            "dv_cds_10"
        )
    ),

    tar_target(
        cds_multi_male,
        multinom_regr(
            "dv_cds_10 + a0005a + a0043b + a0195a + a0278 + a0014 + dv_par_edu_birth + dv_sc_age_5 + e216a + dv_cog_abil_5 + dv_med_5 + d016a + bmi + fclrg90 + tenure + sepmumbcs + prmnh + crowd + ameni + brfed + resmove",
            "dv_cds_10",
            male_data_multi
        )
    ),

    tar_target(
        cds_multi_male_summary,
        multinom_results_summarise(
            cds_multi_male,
            "dv_cds_10"
        )
    ),

    tar_target(
        cds_female,
        pois_regr(
            "dv_cds_10 + a0005a + a0043b + a0195a + a0278 + a0014 + dv_par_edu_birth + dv_sc_age_5 + e216a + dv_cog_abil_5 + dv_med_5 + d016a + bmi + fclrg90 + tenure + sepmumbcs + prmnh + crowd + ameni + brfed + resmove",
            "dv_cds_10",
            female_data_binary
        )
    ),

    tar_target(
        cds_female_summary,
        pois_results_summarise(
            cds_female,
            "dv_cds_10"
        )
    ),

    tar_target(
        cds_multi_female,
        multinom_regr(
            "dv_cds_10 + a0005a + a0043b + a0195a + a0278 + a0014 + dv_par_edu_birth + dv_sc_age_5 + e216a + dv_cog_abil_5 + dv_med_5 + d016a + bmi + fclrg90 + tenure + sepmumbcs + prmnh + crowd + ameni + brfed + resmove",
            "dv_cds_10",
            female_data_multi
        )
    ),

    tar_target(
        cds_multi_female_summary,
        multinom_results_summarise(
            cds_multi_female,
            "dv_cds_10"
        )
    ),

    tar_target(
        malaise_male,
        pois_regr(
            "BD4MAL + a0005a + a0043b + a0195a + a0278 + a0014 + dv_par_edu_birth + dv_sc_age_5 + e216a + dv_cog_abil_5 + dv_med_5 + d016a + dv_bas_g + dv_med_10 + dv_sc_age_16 + bmi + fclrg90 + tenure + divorce + sepmumbcs + prmnh + crowd + ameni + brfed + resmove",
            "BD4MAL",
            male_data_binary
        )
    ),

    tar_target(
        malaise_male_summary,
        pois_results_summarise(
            malaise_male,
            "BD4MAL"
        )
    ),

    tar_target(
        malaise_multi_male,
        multinom_regr(
            "BD4MAL + a0005a + a0043b + a0195a + a0278 + a0014 + dv_par_edu_birth + dv_sc_age_5 + e216a + dv_cog_abil_5 + dv_med_5 + d016a + dv_bas_g + dv_med_10 + dv_sc_age_16 + bmi + fclrg90 + tenure + divorce + sepmumbcs + prmnh + crowd + ameni + brfed + resmove",
            "BD4MAL",
            male_data_multi
        )
    ),

    tar_target(
        malaise_multi_male_summary,
        multinom_results_summarise(
            malaise_multi_male,
            "BD4MAL"
        )
    ),

    tar_target(
        malaise_female,
        pois_regr(
            "BD4MAL + a0005a + a0043b + a0195a + a0278 + a0014 + dv_par_edu_birth + dv_sc_age_5 + e216a + dv_cog_abil_5 + dv_med_5 + d016a + dv_bas_g + dv_med_10 + dv_sc_age_16 + bmi + fclrg90 + tenure + divorce + sepmumbcs + prmnh + crowd + ameni + brfed + resmove",
            "BD4MAL",
            female_data_binary
        )
    ),

    tar_target(
        malaise_female_summary,
        pois_results_summarise(
            malaise_female,
            "BD4MAL"
        )
    ),

    tar_target(
        malaise_multi_female,
        multinom_regr(
            "BD4MAL + a0005a + a0043b + a0195a + a0278 + a0014 + dv_par_edu_birth + dv_sc_age_5 + e216a + dv_cog_abil_5 + dv_med_5 + d016a + dv_bas_g + dv_med_10 + dv_sc_age_16 + bmi + fclrg90 + tenure + divorce + sepmumbcs + prmnh + crowd + ameni + brfed + resmove",
            "BD4MAL",
            female_data_multi
        )
    ),

    tar_target(
        malaise_multi_female_summary,
        multinom_results_summarise(
            malaise_multi_female,
            "BD4MAL"
        )
    ),

    tar_target(
        beh_emo_male,
        pois_regr(
            "rd6m_1 + a0005a + a0043b + a0195a + a0278 + a0014 + dv_par_edu_birth + dv_sc_age_5 + e216a + dv_cog_abil_5 + dv_med_5 + d016a + dv_bas_g + dv_med_10 + dv_sc_age_16 + bmi + fclrg90 + tenure + divorce + sepmumbcs + prmnh + crowd + ameni + brfed + resmove",
            "rd6m_1",
            male_data_binary
        )
    ),

    tar_target(
        beh_emo_male_summary,
        pois_results_summarise(
            beh_emo_male,
            "rd6m_1"
        )
    ),

    tar_target(
        beh_emo_multi_male,
        multinom_regr(
            "rd6m_1 + a0005a + a0043b + a0195a + a0278 + a0014 + dv_par_edu_birth + dv_sc_age_5 + e216a + dv_cog_abil_5 + dv_med_5 + d016a + dv_bas_g + dv_med_10 + dv_sc_age_16 + bmi + fclrg90 + tenure + divorce + sepmumbcs + prmnh + crowd + ameni + brfed + resmove",
            "rd6m_1",
            male_data_multi
        )
    ),

    tar_target(
        beh_emo_multi_male_summary,
        multinom_results_summarise(
            beh_emo_multi_male,
            "rd6m_1"
        )
    ),

    tar_target(
        beh_emo_female,
        pois_regr(
            "rd6m_1 + a0005a + a0043b + a0195a + a0278 + a0014 + dv_par_edu_birth + dv_sc_age_5 + e216a + dv_cog_abil_5 + dv_med_5 + d016a + dv_bas_g + dv_med_10 + dv_sc_age_16 + bmi + fclrg90 + tenure + divorce + sepmumbcs + prmnh + crowd + ameni + brfed + resmove",
            "rd6m_1",
            female_data_binary
        )
    ),

    tar_target(
        beh_emo_female_summary,
        pois_results_summarise(
            beh_emo_female,
            "rd6m_1"
        )
    ),

    tar_target(
        beh_emo_multi_female,
        multinom_regr(
            "rd6m_1 + a0005a + a0043b + a0195a + a0278 + a0014 + dv_par_edu_birth + dv_sc_age_5 + e216a + dv_cog_abil_5 + dv_med_5 + d016a + dv_bas_g + dv_med_10 + dv_sc_age_16 + bmi + fclrg90 + tenure + divorce + sepmumbcs + prmnh + crowd + ameni + brfed + resmove",
            "rd6m_1",
            female_data_multi
        )
    ),

    tar_target(
        beh_emo_multi_female_summary,
        multinom_results_summarise(
            beh_emo_multi_female,
            "rd6m_1"
        )
    ),

    tar_target(
        int_beh_male,
        pois_regr(
            "intbcsz + a0005a + a0043b + a0195a + a0278 + a0014 + dv_par_edu_birth + dv_sc_age_5 + e216a + dv_cog_abil_5 + dv_med_5 + d016a + dv_bas_g + dv_med_10 + dv_sc_age_16 + bmi + fclrg90 + tenure + divorce + sepmumbcs + prmnh + crowd + ameni + brfed + resmove",
            "intbcsz",
            male_data_binary
        )
    ),

    tar_target(
        int_beh_male_summary,
        pois_results_summarise(
            int_beh_male,
            "intbcsz"
        )
    ),

    tar_target(
        int_beh_multi_male,
        multinom_regr(
            "intbcsz + a0005a + a0043b + a0195a + a0278 + a0014 + dv_par_edu_birth + dv_sc_age_5 + e216a + dv_cog_abil_5 + dv_med_5 + d016a + dv_bas_g + dv_med_10 + dv_sc_age_16 + bmi + fclrg90 + tenure + divorce + sepmumbcs + prmnh + crowd + ameni + brfed + resmove",
            "intbcsz",
            male_data_multi
        )
    ),

    tar_target(
        int_beh_multi_male_summary,
        multinom_results_summarise(
            int_beh_multi_male,
            "intbcsz"
        )
    ),

    tar_target(
        int_beh_female,
        pois_regr(
            "intbcsz + a0005a + a0043b + a0195a + a0278 + a0014 + dv_par_edu_birth + dv_sc_age_5 + e216a + dv_cog_abil_5 + dv_med_5 + d016a + dv_bas_g + dv_med_10 + dv_sc_age_16 + bmi + fclrg90 + tenure + divorce + sepmumbcs + prmnh + crowd + ameni + brfed + resmove",
            "intbcsz",
            female_data_binary
        )
    ),

    tar_target(
        int_beh_female_summary,
        pois_results_summarise(
            int_beh_female,
            "intbcsz"
        )
    ),

    tar_target(
        int_beh_multi_female,
        multinom_regr(
            "intbcsz + a0005a + a0043b + a0195a + a0278 + a0014 + dv_par_edu_birth + dv_sc_age_5 + e216a + dv_cog_abil_5 + dv_med_5 + d016a + dv_bas_g + dv_med_10 + dv_sc_age_16 + bmi + fclrg90 + tenure + divorce + sepmumbcs + prmnh + crowd + ameni + brfed + resmove",
            "intbcsz",
            female_data_multi
        )
    ),

    tar_target(
        int_beh_multi_female_summary,
        multinom_results_summarise(
            int_beh_multi_female,
            "intbcsz"
        )
    ),

    tar_target(
        ext_beh_male,
        pois_regr(
            "extbcsz + a0005a + a0043b + a0195a + a0278 + a0014 + dv_par_edu_birth + dv_sc_age_5 + e216a + dv_cog_abil_5 + dv_med_5 + d016a + dv_bas_g + dv_med_10 + dv_sc_age_16 + bmi + fclrg90 + tenure + divorce + sepmumbcs + prmnh + crowd + ameni + brfed + resmove",
            "extbcsz",
            male_data_binary
        )
    ),

    tar_target(
        ext_beh_male_summary,
        pois_results_summarise(
            ext_beh_male,
            "extbcsz"
        )
    ),

    tar_target(
        ext_beh_multi_male,
        multinom_regr(
            "extbcsz + a0005a + a0043b + a0195a + a0278 + a0014 + dv_par_edu_birth + dv_sc_age_5 + e216a + dv_cog_abil_5 + dv_med_5 + d016a + dv_bas_g + dv_med_10 + dv_sc_age_16 + bmi + fclrg90 + tenure + divorce + sepmumbcs + prmnh + crowd + ameni + brfed + resmove",
            "extbcsz",
            male_data_multi
        )
    ),

    tar_target(
        ext_beh_multi_male_summary,
        multinom_results_summarise(
            ext_beh_multi_male,
            "extbcsz"
        )
    ),

    tar_target(
        ext_beh_female,
        pois_regr(
            "extbcsz + a0005a + a0043b + a0195a + a0278 + a0014 + dv_par_edu_birth + dv_sc_age_5 + e216a + dv_cog_abil_5 + dv_med_5 + d016a + dv_bas_g + dv_med_10 + dv_sc_age_16 + bmi + fclrg90 + tenure + divorce + sepmumbcs + prmnh + crowd + ameni + brfed + resmove",
            "extbcsz",
            female_data_binary
        )
    ),

    tar_target(
        ext_beh_female_summary,
        pois_results_summarise(
            ext_beh_female,
            "extbcsz"
        )
    ),

    tar_target(
        ext_beh_multi_female,
        multinom_regr(
            "extbcsz + a0005a + a0043b + a0195a + a0278 + a0014 + dv_par_edu_birth + dv_sc_age_5 + e216a + dv_cog_abil_5 + dv_med_5 + d016a + dv_bas_g + dv_med_10 + dv_sc_age_16 + bmi + fclrg90 + tenure + divorce + sepmumbcs + prmnh + crowd + ameni + brfed + resmove",
            "extbcsz",
            female_data_multi
        )
    ),

    tar_target(
        ext_beh_multi_female_summary,
        multinom_results_summarise(
            ext_beh_multi_female,
            "extbcsz"
        )
    ),

    tar_target(
        rutter_5_med1_male,
        med_pois_regr(
            "d119 + a0005a + a0043b + a0195a + a0278 + a0014 + dv_par_edu_birth + tenure + prmnh + crowd + ameni + brfed",
            "d119",
            male_data_binary
        )
    ),

    tar_target(
        rutter_5_med1_male_summary,
        pois_results_summarise(
            rutter_5_med1_male,
            "d119"
        )
    ),

    tar_target(
        rutter_5_med2_male,
        pois_regr(
            "d119 + BD9MAL + BD9WEMWB + a0005a + a0043b + a0195a + a0278 + a0014 + dv_par_edu_birth + tenure + prmnh + crowd + ameni + brfed",
            "d119",
            male_data_binary
        )
    ),

    tar_target(
        rutter_5_med2_male_summary,
        pois_results_summarise(
            rutter_5_med2_male,
            "d119"
        )
    ),

    tar_target(
        rutter_5_med1_female,
        med_pois_regr(
            "d119 + a0005a + a0043b + a0195a + a0278 + a0014 + dv_par_edu_birth + tenure + prmnh + crowd + ameni + brfed",
            "d119",
            female_data_binary
        )
    ),

    tar_target(
        rutter_5_med1_female_summary,
        pois_results_summarise(
            rutter_5_med1_female,
            "d119"
        )
    ),

    tar_target(
        rutter_5_med2_female,
        pois_regr(
            "d119 + BD9MAL + BD9WEMWB + a0005a + a0043b + a0195a + a0278 + a0014 + dv_par_edu_birth + tenure + prmnh + crowd + ameni + brfed",
            "d119",
            female_data_binary
        )
    ),

    tar_target(
        rutter_5_med2_female_summary,
        pois_results_summarise(
            rutter_5_med2_female,
            "d119"
        )
    ),
    
    tar_target(
        rutter_10_med1_male,
        med_pois_regr(
            "BD3MRUTT + a0005a + a0043b + a0195a + a0278 + a0014 + dv_par_edu_birth + dv_sc_age_5 + e216a + dv_cog_abil_5 + dv_med_5 + d016a + bmi + fclrg90 + tenure + sepmumbcs + prmnh + crowd + ameni + brfed + resmove",
            "BD3MRUTT",
            male_data_binary
        )
    ),

    tar_target(
        rutter_10_med1_male_summary,
        pois_results_summarise(
            rutter_10_med1_male,
            "BD3MRUTT"
        )
    ),

    tar_target(
        rutter_10_med2_male,
        pois_regr(
            "BD3MRUTT + BD9MAL + BD9WEMWB + a0005a + a0043b + a0195a + a0278 + a0014 + dv_par_edu_birth + dv_sc_age_5 + e216a + dv_cog_abil_5 + dv_med_5 + d016a + bmi + fclrg90 + tenure + sepmumbcs + prmnh + crowd + ameni + brfed + resmove",
            "BD3MRUTT",
            male_data_binary
        )
    ),

    tar_target(
        rutter_10_med2_male_summary,
        pois_results_summarise(
            rutter_10_med2_male,
            "BD3MRUTT"
        )
    ),

    tar_target(
        rutter_10_med1_female,
        med_pois_regr(
            "BD3MRUTT + a0005a + a0043b + a0195a + a0278 + a0014 + dv_par_edu_birth + dv_sc_age_5 + e216a + dv_cog_abil_5 + dv_med_5 + d016a + bmi + fclrg90 + tenure + sepmumbcs + prmnh + crowd + ameni + brfed + resmove",
            "BD3MRUTT",
            female_data_binary
        )
    ),

    tar_target(
        rutter_10_med1_female_summary,
        pois_results_summarise(
            rutter_10_med1_female,
            "BD3MRUTT"
        )
    ),

    tar_target(
        rutter_10_med2_female,
        pois_regr(
            "BD3MRUTT + BD9MAL + BD9WEMWB + a0005a + a0043b + a0195a + a0278 + a0014 + dv_par_edu_birth + dv_sc_age_5 + e216a + dv_cog_abil_5 + dv_med_5 + d016a + bmi + fclrg90 + tenure + sepmumbcs + prmnh + crowd + ameni + brfed + resmove",
            "BD3MRUTT",
            female_data_binary
        )
    ),

    tar_target(
        rutter_10_med2_female_summary,
        pois_results_summarise(
            rutter_10_med2_female,
            "BD3MRUTT"
        )
    ),

    tar_target(
        cds_med1_male,
        med_pois_regr(
            "dv_cds_10 + a0005a + a0043b + a0195a + a0278 + a0014 + dv_par_edu_birth + dv_sc_age_5 + e216a + dv_cog_abil_5 + dv_med_5 + d016a + bmi + fclrg90 + tenure + sepmumbcs + prmnh + crowd + ameni + brfed + resmove",
            "dv_cds_10",
            male_data_binary
        )
    ),

    tar_target(
        cds_med1_male_summary,
        pois_results_summarise(
            cds_med1_male,
            "dv_cds_10"
        )
    ),

    tar_target(
        cds_med2_male,
        pois_regr(
            "dv_cds_10 + BD9MAL + BD9WEMWB + a0005a + a0043b + a0195a + a0278 + a0014 + dv_par_edu_birth + dv_sc_age_5 + e216a + dv_cog_abil_5 + dv_med_5 + d016a + bmi + fclrg90 + tenure + sepmumbcs + prmnh + crowd + ameni + brfed + resmove",
            "dv_cds_10",
            male_data_binary
        )
    ),

    tar_target(
        cds_med2_male_summary,
        pois_results_summarise(
            cds_med2_male,
            "dv_cds_10"
        )
    ),

    tar_target(
        cds_med1_female,
        med_pois_regr(
            "dv_cds_10 + a0005a + a0043b + a0195a + a0278 + a0014 + dv_par_edu_birth + dv_sc_age_5 + e216a + dv_cog_abil_5 + dv_med_5 + d016a + bmi + fclrg90 + tenure + sepmumbcs + prmnh + crowd + ameni + brfed + resmove",
            "dv_cds_10",
            female_data_binary
        )
    ),

    tar_target(
        cds_med1_female_summary,
        pois_results_summarise(
            cds_med1_female,
            "dv_cds_10"
        )
    ),

    tar_target(
        cds_med2_female,
        pois_regr(
            "dv_cds_10 + BD9MAL + BD9WEMWB + a0005a + a0043b + a0195a + a0278 + a0014 + dv_par_edu_birth + dv_sc_age_5 + e216a + dv_cog_abil_5 + dv_med_5 + d016a + bmi + fclrg90 + tenure + sepmumbcs + prmnh + crowd + ameni + brfed + resmove",
            "dv_cds_10",
            female_data_binary
        )
    ),
    
    tar_target(
        cds_med2_female_summary,
        pois_results_summarise(
            cds_med2_female,
            "dv_cds_10"
        )
    ),

    tar_target(
        malaise_med1_male,
        med_pois_regr(
            "BD4MAL + a0005a + a0043b + a0195a + a0278 + a0014 + dv_par_edu_birth + dv_sc_age_5 + e216a + dv_cog_abil_5 + dv_med_5 + d016a + dv_bas_g + dv_med_10 + dv_sc_age_16 + bmi + fclrg90 + tenure + divorce + sepmumbcs + prmnh + crowd + ameni + brfed + resmove",
            "BD4MAL",
            male_data_binary
        )
    ),

    tar_target(
        malaise_med1_male_summary,
        pois_results_summarise(
            malaise_med1_male,
            "BD4MAL"
        )
    ),

    tar_target(
        malaise_med2_male,
        pois_regr(
            "BD4MAL + BD9MAL + BD9WEMWB + a0005a + a0043b + a0195a + a0278 + a0014 + dv_par_edu_birth + dv_sc_age_5 + e216a + dv_cog_abil_5 + dv_med_5 + d016a + dv_bas_g + dv_med_10 + dv_sc_age_16 + bmi + fclrg90 + tenure + divorce + sepmumbcs + prmnh + crowd + ameni + brfed + resmove",
            "BD4MAL",
            male_data_binary
        )
    ),

    tar_target(
        malaise_med2_male_summary,
        pois_results_summarise(
            malaise_med2_male,
            "BD4MAL"
        )
    ),

    tar_target(
        malaise_med1_female,
        med_pois_regr(
            "BD4MAL + a0005a + a0043b + a0195a + a0278 + a0014 + dv_par_edu_birth + dv_sc_age_5 + e216a + dv_cog_abil_5 + dv_med_5 + d016a + dv_bas_g + dv_med_10 + dv_sc_age_16 + bmi + fclrg90 + tenure + divorce + sepmumbcs + prmnh + crowd + ameni + brfed + resmove",
            "BD4MAL",
            female_data_binary
        )
    ),

    tar_target(
        malaise_med1_female_summary,
        pois_results_summarise(
            malaise_med1_female,
            "BD4MAL"
        )
    ),

    tar_target(
        malaise_med2_female,
        pois_regr(
            "BD4MAL + BD9MAL + BD9WEMWB + a0005a + a0043b + a0195a + a0278 + a0014 + dv_par_edu_birth + dv_sc_age_5 + e216a + dv_cog_abil_5 + dv_med_5 + d016a + dv_bas_g + dv_med_10 + dv_sc_age_16 + bmi + fclrg90 + tenure + divorce + sepmumbcs + prmnh + crowd + ameni + brfed + resmove",
            "BD4MAL",
            female_data_binary
        )
    ),

    tar_target(
        malaise_med2_female_summary,
        pois_results_summarise(
            malaise_med2_female,
            "BD4MAL"
        )
    ),

    tar_target(
        beh_emo_med1_male,
        med_pois_regr(
            "rd6m_1 + a0005a + a0043b + a0195a + a0278 + a0014 + dv_par_edu_birth + dv_sc_age_5 + e216a + dv_cog_abil_5 + dv_med_5 + d016a + dv_bas_g + dv_med_10 + dv_sc_age_16 + bmi + fclrg90 + tenure + divorce + sepmumbcs + prmnh + crowd + ameni + brfed + resmove",
            "rd6m_1",
            male_data_binary
        )
    ),

    tar_target(
        beh_emo_med1_male_summary,
        pois_results_summarise(
            beh_emo_med1_male,
            "rd6m_1"
        )
    ),

    tar_target(
        beh_emo_med2_male,
        pois_regr(
            "rd6m_1 + BD9MAL + BD9WEMWB + a0005a + a0043b + a0195a + a0278 + a0014 + dv_par_edu_birth + dv_sc_age_5 + e216a + dv_cog_abil_5 + dv_med_5 + d016a + dv_bas_g + dv_med_10 + dv_sc_age_16 + bmi + fclrg90 + tenure + divorce + sepmumbcs + prmnh + crowd + ameni + brfed + resmove",
            "rd6m_1",
            male_data_binary
        )
    ),

    tar_target(
        beh_emo_med2_male_summary,
        pois_results_summarise(
            beh_emo_med2_male,
            "rd6m_1"
        )
    ),

    tar_target(
        beh_emo_med1_female,
        med_pois_regr(
            "rd6m_1 + a0005a + a0043b + a0195a + a0278 + a0014 + dv_par_edu_birth + dv_sc_age_5 + e216a + dv_cog_abil_5 + dv_med_5 + d016a + dv_bas_g + dv_med_10 + dv_sc_age_16 + bmi + fclrg90 + tenure + divorce + sepmumbcs + prmnh + crowd + ameni + brfed + resmove",
            "rd6m_1",
            female_data_binary
        )
    ),

    tar_target(
        beh_emo_med1_female_summary,
        pois_results_summarise(
            beh_emo_med1_female,
            "rd6m_1"
        )
    ),

    tar_target(
        beh_emo_med2_female,
        pois_regr(
            "rd6m_1 + BD9MAL + BD9WEMWB + a0005a + a0043b + a0195a + a0278 + a0014 + dv_par_edu_birth + dv_sc_age_5 + e216a + dv_cog_abil_5 + dv_med_5 + d016a + dv_bas_g + dv_med_10 + dv_sc_age_16 + bmi + fclrg90 + tenure + divorce + sepmumbcs + prmnh + crowd + ameni + brfed + resmove",
            "rd6m_1",
            female_data_binary
        )
    ),

    tar_target(
        beh_emo_med2_female_summary,
        pois_results_summarise(
            beh_emo_med2_female,
            "rd6m_1"
        )
    ),
    
    tar_target(
        int_beh_med1_male,
        med_pois_regr(
            "intbcsz + a0005a + a0043b + a0195a + a0278 + a0014 + dv_par_edu_birth + dv_sc_age_5 + e216a + dv_cog_abil_5 + dv_med_5 + d016a + dv_bas_g + dv_med_10 + dv_sc_age_16 + bmi + fclrg90 + tenure + divorce + sepmumbcs + prmnh + crowd + ameni + brfed + resmove",
            "intbcsz",
            male_data_binary
        )
    ),

    tar_target(
        int_beh_med1_male_summary,
        pois_results_summarise(
            int_beh_med1_male,
            "intbcsz"
        )
    ),

    tar_target(
        int_beh_med2_male,
        pois_regr(
            "intbcsz + BD9MAL + BD9WEMWB + a0005a + a0043b + a0195a + a0278 + a0014 + dv_par_edu_birth + dv_sc_age_5 + e216a + dv_cog_abil_5 + dv_med_5 + d016a + dv_bas_g + dv_med_10 + dv_sc_age_16 + bmi + fclrg90 + tenure + divorce + sepmumbcs + prmnh + crowd + ameni + brfed + resmove",
            "intbcsz",
            male_data_binary
        )
    ),

    tar_target(
        int_beh_med2_male_summary,
        pois_results_summarise(
            int_beh_med2_male,
            "intbcsz"
        )
    ),

    tar_target(
        int_beh_med1_female,
        med_pois_regr(
            "intbcsz + a0005a + a0043b + a0195a + a0278 + a0014 + dv_par_edu_birth + dv_sc_age_5 + e216a + dv_cog_abil_5 + dv_med_5 + d016a + dv_bas_g + dv_med_10 + dv_sc_age_16 + bmi + fclrg90 + tenure + divorce + sepmumbcs + prmnh + crowd + ameni + brfed + resmove",
            "intbcsz",
            female_data_binary
        )
    ),

    tar_target(
        int_beh_med1_female_summary,
        pois_results_summarise(
            int_beh_med1_female,
            "intbcsz"
        )
    ),

    tar_target(
        int_beh_med2_female,
        pois_regr(
            "intbcsz + BD9MAL + BD9WEMWB + a0005a + a0043b + a0195a + a0278 + a0014 + dv_par_edu_birth + dv_sc_age_5 + e216a + dv_cog_abil_5 + dv_med_5 + d016a + dv_bas_g + dv_med_10 + dv_sc_age_16 + bmi + fclrg90 + tenure + divorce + sepmumbcs + prmnh + crowd + ameni + brfed + resmove",
            "intbcsz",
            female_data_binary
        )
    ),

    tar_target(
        int_beh_med2_female_summary,
        pois_results_summarise(
            int_beh_med2_female,
            "intbcsz"
        )
    ),

    tar_target(
        ext_beh_med1_male,
        med_pois_regr(
            "extbcsz + a0005a + a0043b + a0195a + a0278 + a0014 + dv_par_edu_birth + dv_sc_age_5 + e216a + dv_cog_abil_5 + dv_med_5 + d016a + dv_bas_g + dv_med_10 + dv_sc_age_16 + bmi + fclrg90 + tenure + divorce + sepmumbcs + prmnh + crowd + ameni + brfed + resmove",
            "extbcsz",
            male_data_binary
        )
    ),

    tar_target(
        ext_beh_med1_male_summary,
        pois_results_summarise(
            ext_beh_med1_male,
            "extbcsz"
        )
    ),

    tar_target(
        ext_beh_med2_male,
        pois_regr(
            "extbcsz + BD9MAL + BD9WEMWB + a0005a + a0043b + a0195a + a0278 + a0014 + dv_par_edu_birth + dv_sc_age_5 + e216a + dv_cog_abil_5 + dv_med_5 + d016a + dv_bas_g + dv_med_10 + dv_sc_age_16 + bmi + fclrg90 + tenure + divorce + sepmumbcs + prmnh + crowd + ameni + brfed + resmove",
            "extbcsz",
            male_data_binary
        )
    ),

    tar_target(
        ext_beh_med2_male_summary,
        pois_results_summarise(
            ext_beh_med2_male,
            "extbcsz"
        )
    ),

    tar_target(
        ext_beh_med1_female,
        med_pois_regr(
            "extbcsz + a0005a + a0043b + a0195a + a0278 + a0014 + dv_par_edu_birth + dv_sc_age_5 + e216a + dv_cog_abil_5 + dv_med_5 + d016a + dv_bas_g + dv_med_10 + dv_sc_age_16 + bmi + fclrg90 + tenure + divorce + sepmumbcs + prmnh + crowd + ameni + brfed + resmove",
            "extbcsz",
            female_data_binary
        )
    ),

    tar_target(
        ext_beh_med1_female_summary,
        pois_results_summarise(
            ext_beh_med1_female,
            "extbcsz"
        )
    ),

    tar_target(
        ext_beh_med2_female,
        pois_regr(
            "extbcsz + BD9MAL + BD9WEMWB + a0005a + a0043b + a0195a + a0278 + a0014 + dv_par_edu_birth + dv_sc_age_5 + e216a + dv_cog_abil_5 + dv_med_5 + d016a + dv_bas_g + dv_med_10 + dv_sc_age_16 + bmi + fclrg90 + tenure + divorce + sepmumbcs + prmnh + crowd + ameni + brfed + resmove",
            "extbcsz",
            female_data_binary
        )
    ),

    tar_target(
        ext_beh_med2_female_summary,
        pois_results_summarise(
            ext_beh_med2_female,
            "extbcsz"
        )
    ),

    tar_quarto(
        models_report,
        "models_report.Qmd"
    )
)
