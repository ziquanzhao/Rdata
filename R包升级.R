# 更新CRAN包
update.packages(ask = FALSE)

# 更新Bioconductor包
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install(update = TRUE, ask = FALSE)

# 更新通过GitHub安装的包
if (!requireNamespace("remotes", quietly = TRUE))
  install.packages("remotes")
remotes::update_packages()

# 检查并解决依赖问题
update.packages(checkBuilt = TRUE, ask = FALSE)

#升级R
library(installr)
updateR()
