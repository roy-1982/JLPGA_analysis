pacman::p_load(ggrepel, googlesheets4, dplyr, tidyr, showtext, corrplot, tibble, fmsb, psych, knitr)

font_add_google("Dela Gothic One", "dela")
font_add_google("M PLUS 1p", "mplus")
showtext_auto() 

df <- read_sheet("https://docs.google.com/spreadsheets/d/1N12bZEAg2jGqjAQxva_ifXY5gRyNF05vE9aQhnJT-Jg/edit?gid=0#gid=0") 

# 数値データ && 標準偏差が0より大きいことを条件に絞り込み
df_num <- df %>%
  select(where(is.numeric)) %>%
  drop_na() %>%
  select(where(~ sd(.x) > 0)) 

res_cor <- cor(df_num) 
corrplot(res_cor, 
         method = "number",
         type = "upper",       
         tl.col = "black",  
         tl.cex = 0.6,
         tl.srt = 45,         
         addCoef.col = "black" ,
         number.cex = 0.7
)

df_input <- df %>%
  select(
    選手名,           
    平均ストローク,
    平均バーディー,
    # パーセーブ率,
    パーオン率,
    平均パット数,
    ディスタンス,
    # Ｆキープ率,
    リカバリー率,
    Ｓセーブ率,
    # イーグル, 
    パー,
    ボギー,
    # ダブルボギー
  ) %>%
  column_to_rownames(var = "選手名") %>% 
  as.data.frame()

data <- apply(df_input %>% select(-平均ストローク), 2, scale) # 平均ストロークは回帰分析でのみ使用するので除外

VSS.scree(data)
model <- fa(data, nfactors = 4, rotate = "varimax", fm="ml") 
fa.diagram(model, cex=0.1)
print(model, cut = 0.3, sort = TRUE)
kable(round(unclass(model$loadings), 3), caption = "Factor Loadings")

df_scores <- as.data.frame(predict(model, data))
df_combined <- cbind(df_input, df_scores)
df_combined$ML2 <- df_combined$ML2 * -1


set.seed(123)
km_res <- kmeans(df_scores[, c("ML1", "ML2", "ML3")], centers = 4)
df_combined$Cluster <- as.factor(km_res$cluster)
levels(df_combined$Cluster) <- c("エリート・オールラウンダー", "ショットメーカー", "ショートゲーム巧者", "安定・粘りタイプ")

ggplot(df_combined, aes(x = ML1, y = ML2, label = rownames(df_combined))) +
  geom_hline(yintercept = 0, color = "grey80", linetype = "solid", size = 0.3) +
  geom_vline(xintercept = 0, color = "grey80", linetype = "solid", size = 0.3) +
  geom_point(aes(color = Cluster, size = ML3), alpha = 0.4, stroke = 0.5) + 
  geom_text_repel(
    family = "mplus", 
    size = 2.5, 
    color = "grey10", 
    max.overlaps = 30,
    box.padding = 0.5,   
    point.padding = 0.3,  
    segment.color = "grey50", 
    segment.size = 0.2
  ) + 
  scale_size_continuous(
    range = c(1, 14), 
    breaks = c(-1, 0, 1.5),
    labels = c("低い", "平均", "高い")
  ) +
  scale_color_manual(values = c(
    "エリート・オールラウンダー" = "#E41A1C", 
    "ショットメーカー"           = "#377EB8", 
    "ショートゲーム巧者"         = "#4DAF4A", 
    "安定・粘りタイプ"           = "#984EA3"
  )) +
  labs(
    title = "JLPGA 3大能力因子のバランスマップ",
    subtitle = "右上ほど「ショットで乗せて、外しても寄せる」完成度の高い選手（円が大きいほど攻撃的）",
    x = "←低い　ショット精度 (パーオンのしやすさ)　高い→",
    y = "←低い　リカバリー能力 (ボギーを打たない粘り)　高い→",
    color = "プレースタイル分類",
    size = "攻撃力 (バーディー奪取)"
  ) + 
  guides(
    color = guide_legend(title.position = "top", title.hjust = 0, nrow = 2, order = 1),
    size = guide_legend(title.position = "top", title.hjust = 0, nrow = 1, order = 2)
  ) +
  theme_minimal(base_family = "mplus") +
  theme(
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.box.just = "top", 
    legend.spacing.x = unit(1.0, "cm"),
    legend.box.margin = margin(t = 20),
    plot.title = element_text(family = "dela", size = 22, hjust = 0.5, margin = margin(b = 10)),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "grey30", margin = margin(b = 20)),
    axis.title = element_text(face = "bold", size = 10),
    legend.title = element_text(face = "bold", size = 9),
    legend.text = element_text(size = 8),
    panel.grid.minor = element_blank(),
    plot.margin = margin(20, 20, 20, 20)
  ) +
  coord_cartesian(xlim = c(-3, 3), ylim = c(-3.5, 2.5))

# ggsave("~/Desktop/JLPGA因子分析.png", width = 950, height = 950, units = "px", dpi = 96)

#--------------------------------------

res_lm <- lm(平均ストローク ~ ML1 + ML2 + ML3, data = df_combined)
summary(res_lm)

coef_df_score <- as.data.frame(summary(res_lm)$coefficients[-1, ]) 
coef_df_score$Factor <- c("ショット精度(ML1)", "リカバリー能力(ML2)", "攻撃力/パット(ML3)")
coef_df_score$Estimate <- abs(coef_df_score$Estimate) 


ggplot(coef_df_score, aes(x = reorder(Factor, Estimate), y = Estimate)) +
  geom_bar(stat = "identity", fill = "#E41A1C", alpha = 0.6) +
  coord_flip() +
  labs(title = "平均ストロークに対する寄与度比較",
       subtitle = "スコアを縮めるために最もインパクトが大きいのは「リカバリー能力（ML2）」",
       x = "能力因子",
       y = "影響力の大きさ（係数の絶対値）") +
  theme_minimal(base_family = "mplus") +
  theme(
    plot.title = element_text(family = "dela", size = 22, hjust = 0, margin = margin(b = 10)),
    plot.subtitle = element_text(face = "bold", size = 11, hjust = 0, color = "grey30", margin = margin(b = 20)),
    axis.title = element_text(face = "bold", size = 10),
    panel.grid.minor = element_blank(),
    plot.margin = margin(20, 20, 20, 20)
  )

# ggsave("~/Desktop/JLPGA寄与度.png", width = 950, height = 650, units = "px", dpi = 96)







