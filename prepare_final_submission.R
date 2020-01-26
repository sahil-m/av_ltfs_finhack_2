segment_1_preds <- read_csv("data/pred_test_s1_withProphet_v2.csv")
segment_2_preds <- read_csv("data/pred_test_s2_withProphet_v2.csv")


final_submission <- combine_segment_1_and_segment_2_predictions(segment_1_preds, segment_2_preds)


write_csv(final_submission, "data/submission_withProphet_v2.csv")
