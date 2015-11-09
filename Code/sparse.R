# GET MATRICES ------------------------------------------------
vars <- names(DT3)[grepl("logkwh|d_.*|temp.*|rhum.*", names(DT3))]
yvars <- vars[grep("logkwh", vars)]
xvars <- vars[grep("^d_*", vars)]
wvars <- vars[grep("^temp|^rhum", vars)]
xvars <- c(xvars, wvars)
Z <- sparse.model.matrix(DT3$id~as.factor(DT3$id)-1)
zvars <- str_replace_all(attr(Z, "Dimnames")[[2]], "^as.factor\\(DT3\\$id\\)", "id_")
attr(Z, "Dimnames")[[2]] <- zvars
XYT <- Matrix(cBind(as.matrix(DT3[, c(yvars, xvars), with = F]), Z))
# update xvars
nms <- attr(XYT, "Dimnames")[[2]]
y_indx <- grep("logkwh", nms)
x_indx <- grep("^d_*|^temp|^rhum", nms)
z_indx <- grep("^id_", nms)
