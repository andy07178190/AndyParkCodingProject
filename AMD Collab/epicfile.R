if (!requireNamespace("BiocManager", quietly = TRUE)) {
  install.packages("BiocManager")
}
BiocManager::install(c(
  "Biobase",
  "minfi",
  "sva",
  "IlluminaHumanMethylation450kmanifest",
  "IlluminaHumanMethylation450kanno.ilmn12.hg19",
  "IlluminaHumanMethylationEPICmanifest",
  "IlluminaHumanMethylationEPICanno.ilm10b4.hg19"
), force = TRUE)

library(Biobase)
library(minfi)
library(sva)


idat_path <- "~/Desktop/GSE188894_RAW"

idat_files <- c(
  "~/Desktop/GSE188894_RAW/GPL13534_450K_Manifest_header_Descriptions-1.xlsx",
  "~/Desktop/GSE188894_RAW/GPL13534_450K_Manifest_header_Descriptions.xlsx",
  "~/Desktop/GSE188894_RAW/GPL13534_HumanMethylation450_15017482_v.1.1.bpm.txt",
  "~/Desktop/GSE188894_RAW/GPL13534_HumanMethylation450_15017482_v.1.1.csv",
  "~/Desktop/GSE188894_RAW/GPL13534_HumanMethylation450_15017482_v.1.2.bpm",
  "~/Desktop/GSE188894_RAW/GSM5691570_LFS_MB_P_model_BGB_Clariom_S_Human.CEL",
  "~/Desktop/GSE188894_RAW/GSM5691571_LFS_MB_P_model_Romi_Clariom_S_Human.CEL",
  "~/Desktop/GSE188894_RAW/GSM5691572_LFS_MB_P_model_BGB-Romi_Clariom_S_Human.CEL",
  "~/Desktop/GSE188894_RAW/GSM5691573_LFS_MB_P_model_DMSO_Clariom_S_Human.CEL",
  "~/Desktop/GSE188894_RAW/GSM5691574_LFS_MB_1R_model_BGB_Clariom_S_Human.CEL",
  "~/Desktop/GSE188894_RAW/GSM5691575_LFS_MB_1R_model_Romi_Clariom_S_Human.CEL",
  "~/Desktop/GSE188894_RAW/GSM5691576_LFS_MB_1R_model_BGB-Romi_Clariom_S_Human.CEL",
  "~/Desktop/GSE188894_RAW/GSM5691577_LFS_MB_1R_model_DMSO_Clariom_S_Human.CEL",
  "~/Desktop/GSE188894_RAW/GSM5691578_RCMB18_BGB_Clariom_S_Human.CEL",
  "~/Desktop/GSE188894_RAW/GSM5691579_RCMB18_Romi_Clariom_S_Human.CEL",
  "~/Desktop/GSE188894_RAW/GSM5691580_RCMB18_BGB-Romi_Clariom_S_Human.CEL",
  "~/Desktop/GSE188894_RAW/GSM5691581_RCMB18_DMSO_Clariom_S_Human.CEL",
  "~/Desktop/GSE188894_RAW/GSM5692918_200889820015_R08C01_Grn.idat",
  "~/Desktop/GSE188894_RAW/GSM5692918_200889820015_R08C01_Red.idat",
  "~/Desktop/GSE188894_RAW/GSM5692919_203048410044_R04C01_Grn.idat",
  "~/Desktop/GSE188894_RAW/GSM5692919_203048410044_R04C01_Red.idat",
  "~/Desktop/GSE188894_RAW/GSM5692920_203048410044_R05C01_Grn.idat",
  "~/Desktop/GSE188894_RAW/GSM5692920_203048410044_R05C01_Red.idat",
  "~/Desktop/GSE188894_RAW/GSM5692921_203048410044_R06C01_Grn.idat",
  "~/Desktop/GSE188894_RAW/GSM5692921_203048410044_R06C01_Red.idat",
  "~/Desktop/GSE188894_RAW/GSM5692922_203048410044_R07C01_Grn.idat",
  "~/Desktop/GSE188894_RAW/GSM5692922_203048410044_R07C01_Red.idat",
  "~/Desktop/GSE188894_RAW/GSM5692923_203048410044_R08C01_Grn.idat",
  "~/Desktop/GSE188894_RAW/GSM5692923_203048410044_R08C01_Red.idat",
  "~/Desktop/GSE188894_RAW/GSM5692924_203049640078_R08C01_Grn.idat",
  "~/Desktop/GSE188894_RAW/GSM5692924_203049640078_R08C01_Red.idat",
  "~/Desktop/GSE188894_RAW/GSM5692925_10006823133_R01C01_Grn.idat",
  "~/Desktop/GSE188894_RAW/GSM5692925_10006823133_R01C01_Red.idat",
  "~/Desktop/GSE188894_RAW/GSM5692926_3998523031_R02C01_Grn.idat",
  "~/Desktop/GSE188894_RAW/GSM5692926_3998523031_R02C01_Red.idat",
  "~/Desktop/GSE188894_RAW/GSM5692927_3999543052_R01C02_Grn.idat",
  "~/Desktop/GSE188894_RAW/GSM5692927_3999543052_R01C02_Red.idat",
  "~/Desktop/GSE188894_RAW/GSM5692928_8622007132_R03C01_Grn.idat",
  "~/Desktop/GSE188894_RAW/GSM5692928_8622007132_R03C01_Red.idat")

basenames <- unique(gsub("_Grn\\.idat$|_Red\\.idat$", "", idat_files))

grn_files <- paste0(basenames, "_Grn.idat")
red_files <- paste0(basenames, "_Red.idat")

missing_grn <- grn_files[!file.exists(grn_files)]
missing_red <- red_files[!file.exists(red_files)]

if (length(missing_grn) > 0 || length(missing_red) > 0) {
  stop("Some paired IDAT files are missing:\n",
       "Missing _Grn.idat files: ", paste(missing_grn, collapse = ", "), "\n",
       "Missing _Red.idat files: ", paste(missing_red, collapse = ", "))
}

detect_platform <- function(base) {
  files <- grep(base, idat_files, value = TRUE)
  platforms <- sapply(files, function(file) {
    tryCatch({
      readIDAT(file)$platform
    }, error = function(e) NA)
  })
  unique(platforms[!is.na(platforms)])
}

platforms <- sapply(basenames, detect_platform)

epic_basenames <- basenames[platforms == "IlluminaHumanMethylationEPIC"]
k450_basenames <- basenames[platforms == "IlluminaHumanMethylation450k"]

RGset_epic <- tryCatch({
  if (length(epic_basenames) > 0) {
    read.metharray(basenames = epic_basenames, force = TRUE)
  } else {
    message("No EPIC files found.")
    NULL
  }
}, error = function(e) {
  message("Error reading EPIC IDAT files: ", e$message)
  NULL
})

RGset_450k <- tryCatch({
  if (length(k450_basenames) > 0) {
    read.metharray(basenames = k450_basenames, force = TRUE)
  } else {
    message("No 450k files found.")
    NULL
  }
}, error = function(e) {
  message("Error reading 450k IDAT files: ", e$message)
  NULL
})

normalize_and_get_beta <- function(RGset) {
  normalized <- tryCatch({
    preprocessQuantile(RGset)
  }, error = function(e) {
    message("Error during normalization: ", e$message)
    NULL
  })
  
  if (is.null(normalized)) return(NULL)
  
  beta_values <- tryCatch({
    getBeta(normalized)
  }, error = function(e) {
    message("Error extracting beta values: ", e$message)
    NULL
  })
  
  beta_values
}

beta_epic <- normalize_and_get_beta(RGset_epic)
beta_450k <- normalize_and_get_beta(RGset_450k)

if (!is.null(beta_epic)) {
  message("EPIC Beta values extracted with dimensions: ", paste(dim(beta_epic), collapse = " x "))
}

if (!is.null(beta_450k)) {
  message("450k Beta values extracted with dimensions: ", paste(dim(beta_450k), collapse = " x "))
}

