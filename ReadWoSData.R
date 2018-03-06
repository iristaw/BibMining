#使用data.table将wos文件读入(require data.table)
#要求data.table版本升级到1.10.4 (require updating your data.table's version to 1.10.4)
require(data.table)
#files是单个文件名/多个文件名 (files can be a single file or multi-files)
#若files是多个文件，则先执行以下步骤 (please firstly run the step below if object(files) are multifiles)
#files <- list.files("files", "*.txt", full.names = TRUE)
wos.read <- function(files, ...) {
    items = data.table()
    if (!is.vector(files)) {
        files <- c(files)
    }
    for (file in files) {
        items <- rbind(items, fread(file, sep = "\t", sep2 = "\t", na.strings = "", encoding = "UTF-8", quote = "", fill = TRUE, blank.lines.skip = TRUE, ...))
    }
    names <- colnames(items)
    name <- names[length(names)]
    items[, (name) := NULL]
    return(items)
}
