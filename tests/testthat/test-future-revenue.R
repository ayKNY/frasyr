
context("check future_vpa with maximizing revenue scenario")

options(warn=-1)
data(res_vpa_org)
data(res_sr_HSL2)

# normal lognormal ----
data_future_test <- make_future_data(res_vpa_org, # VPAの結果
                                     nsim = 100, # シミュレーション回数
                                     nyear = 20, # 将来予測の年数
                                     future_initial_year_name = 2017,
                                     start_F_year_name = 2018,
                                     start_biopar_year_name=2018,
                                     start_random_rec_year_name = 2018,
                                     # biopar setting
                                     waa_year=2015:2017, waa=NULL,
                                     waa_catch_year=2015:2017, waa_catch=NULL,
                                     maa_year=2015:2017, maa=NULL,
                                     M_year=2015:2017, M=NULL,
                                     # faa setting
                                     faa_year=2015:2017,
                                     currentF=NULL,futureF=NULL,
                                     # HCR setting (not work when using TMB)
                                     start_ABC_year_name=2019, # HCRを適用する最初の年
                                     HCR_beta=1, # HCRのbeta
                                     HCR_Blimit=-1, # HCRのBlimit
                                     HCR_Bban=-1, # HCRのBban
                                     HCR_year_lag=0, # HCRで何年遅れにするか
                                     HCR_function_name = "HCR_default",
                                     # SR setting
                                     res_SR=res_sr_HSL2,
                                     seed_number=1,
                                     resid_type="lognormal",
                                     resample_year_range=0, # リサンプリングの場合、残差をリサンプリングする年の範囲
                                     bias_correction=TRUE, # バイアス補正をするかどうか
                                     recruit_intercept=0, # 移入や放流などで一定の加入がある場合に足す加入尾数
                                     # Other
                                     Pope=res_vpa_org$input$Pope
)

test_that("estimate multi to maximize the future revenue",{

          data_future_rev <- add_paa_mat(data_future = data_future_test,
                                         paa = c(400, 1200, 1400, 1000))
          #変動なしで計算
          data_future_rev$data$SR_mat[,,"deviance"] <- 0

          expect_error(res_future_tesrev <- future_vpa(tmb_data=data_future_test$data,
                                                       objective = "Revenue",
                                                       optim_method = "R",
                                                       obj_value = 3))

          res_future_tesrev <- future_vpa(tmb_data=data_future_rev$data,
                                            objective = "Revenue",
                                          optim_method = "R",
                                          obj_value = 3)

          expect_equal(res_future_tesrev$multi, 0.474, tol = 0.001)

        res_MSY_rev <- est_MSYRP(data_future = data_future_rev, objective = "Revenue")

        #summary_tableに漁獲金額の結果が反映されているか
        expect_true(any(names(res_MSY_rev$all.stat) %in% "rev.mean"))
        expect_equal(res_MSY_rev$summary$"Fref/Fcur"[1], res_future_tesrev$multi, tol=0.00001)
        }
                  )

#デフォルトのMSY計算のシナリオに影響が出ないように設計する

res_MSY_y <- est_MSYRP(data_future = data_future_rev, objective = "MSY")

future_vpa(data_future_rev$data,
           objective = "Revenue",
           optim_method = "R",
           obj_value = 3,
           do_MSE = TRUE,
           MSE_input_data = res_MSY_y$data_future_MSY$data,
           MSE_sd = 0,
           MSE_catch_exact_TAC = TRUE)
