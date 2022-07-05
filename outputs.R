# save tables --------------------------------------------------------------
dir.create(file.path(getwd(), "outputs"))
dir.create(file.path(getwd(), "outputs/tables"))

# save desc_data
write.table(desc_data,
  sep = ",",
  file = "./outputs/tables/TabS1.csv",
  quote = FALSE, col.names = FALSE
)

# save desc_stat
write.table(desc_stat,
  sep = ",",
  file = "./outputs/tables/TabS2.csv",
  quote = FALSE, row.names = FALSE
)

# save desc_cor
write.table(desc_cor,
  sep = ",",
  file = "./outputs/tables/TabS3.csv",
  quote = FALSE, row.names = TRUE
)

# save auc
write.table(auc,
  sep = ",",
  file = "./outputs/tables/Tab4.csv",
  quote = FALSE, col.names = FALSE
)

# save redux_sorted_load
if (redux_method != "none") {
  write.table(redux_sorted_load,
    sep = ",",
    file = "./outputs/tables/TabS5.csv",
    quote = FALSE, row.names = TRUE
  )
}

# save plot p_redux
if (redux_method != "none") {
  ggsave(
    filename = paste0("./outputs/figures/pdf/", "FigS5", ".pdf"),
    plot = p_redux,
    width = width_factor * 105, height = height_factor * 74.25, units = "mm"
  )
  ggsave(
    filename = paste0("./outputs/figures/png/", "FigS5", ".png"),
    plot = p_redux,
    width = width_factor * 105, height = height_factor * 74.25, units = "mm"
  )
  ggsave(
    filename = paste0("./outputs/figures/jpg_low-quality/", "FigS5", ".jpg"),
    plot = p_redux,
    width = width_factor * 105, height = height_factor * 74.25,
    units = "mm", dpi = 100
  )
}