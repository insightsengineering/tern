
if (FALSE) {


library(grid)
library(haven)
library(dplyr)

#Use data from mo28072 for code development
asl <- read_sas("Z:/cdpt8148/s28072f/libraries/asl.sas7bdat")
ars <- read_sas("Z:/cdpt8148/s28072f/libraries/ars.sas7bdat")


asl <- asl %>%
  select(USUBJID, STUDYID, ARM, ARMCD) %>%
  mutate(PLID = substr(USUBJID, 9, length(USUBJID)))

apchg <- ars %>%
  filter(PARAMCD == "SUMLDIAM", ADY > 0) %>% select(USUBJID, STUDYID, ADY, AVAL) %>% filter(!is.na(AVAL))
abs <- ars %>%
  filter(PARAMCD == "CBORINVR", ADY > 0) %>% select(USUBJID, STUDYID, AVALC)

if(nrow(apchg) > length(unique(apchg$USUBJID))) stop("Duplicated Maximum SLD percentage change records!")
if(nrow(abs) > length(unique(abs$USUBJID))) stop("Duplicated Confirmed Resoponse records!")

#Merge pchange and besr response
asld <- right_join(abs %>% select(STUDYID, USUBJID, AVALC), apchg)

ylim <- c(-100, max(100, asld$AVAL))

ANL_waterfall <- function(ASL, ARS) {

    ## list into data into patient bnased nested lists
    patients <- Map(function(usubjid, studyid) {
    asl <- subset(ASL, USUBJID == usubjid & STUDYID == studyid)
    if (nrow(asl) != 1) stop("one row per patient expected in asl")
    ars <- subset(ARS, USUBJID == usubjid & STUDYID == studyid)
    #  if (nrow(ars) == 0) warning(paste("patient", usubjid, "in study", studyid, "has no ARS data"))
    list(ASL = as.list(asl), ARS = ars)
  }, unique(ARS$USUBJID), unique(ARS$STUDYID))


  # p <- patients[[1]]
  # p <- patients[["BP29428-274992-2014"]]
  plot_data <- Map(function(p) {

    ## change from baseline
    d.chng <- p$ARS
    i <- which.min(d.chng$AVAL)

    lchng <- if (is.na(i) || length(i) != 1 || !is.numeric(i)) {
      # cat(paste('patient', p$ASL$USUBJID, " has no maximum change due to missing values\n"))
      list(
        CHNG = NA_real_,
        ADY = "-"
      )
    } else {
      list(
        CHNG = d.chng$AVAL[i],
        ADY = d.chng$ADY[i],
        RESP = d.chng$AVALC[i]
      )
    }

    c(
      p$ASL,
      lchng
    )
  }, patients)

  plot_data
}

X <- ANL_waterfall(asl, asld)

X <- X[1:20]

col_var <- "RESP"
col_x <- sapply(X, `[[`, col_var, USE.NAMES = FALSE)


chng <- vapply(X, `[[`, numeric(1), "CHNG")
max_change <- max(chng, na.rm = TRUE)
ylim <- c(-100, max(100, max_change))
ord <- order(chng, decreasing = TRUE, na.last = TRUE)

#Define Response Colours
color_resp <- list(PD = "red", SD = "yellow", PR = "blue", CR = "green", NE = "grey")
grid.newpage()

pushViewport(viewport(gp = gpar(fontsize = 11)))
d_usubjid <- convertWidth(stringWidth("MO28072-xxxxxx-xx"), "lines", valueOnly = TRUE)
pushViewport(plotViewport(margins = c(d_usubjid + 2, 5, 2, 2)))

grid.text("Maximum Percentage\nChange SLD from baseline", x = unit(-3.5, "lines"), rot = 90)
pushViewport(viewport(gp = gpar(fontsize = 8)))
pushViewport(dataViewport(yData = ylim, xData = c(0,1)))
grid.rect()
grid.yaxis()
grid.lines(y = unit(c(0,0), "native"))


n <- length(X)

bar_width <- unit(0.8/n, "npc")
# i <- 1
for(i in seq_along(ord)) {
  k <- ord[i] # patient index
  x <- unit(i/(n+1), "npc")

  chng_k <- X[[k]]$CHNG

  ## bar
  col_value <- X[[k]][[col_var]]
  grid.rect(
    x = x, y = unit(0, "native"),
    height = unit(chng_k, "native"), width = bar_width,
    just = "bottom", gp = gpar(fill = color_resp[[col_value]], col = color_resp[[col_value]])
  )

  if (!is.na(chng_k)) {
    isUp <- chng_k > 0
    # ## day
    # grid.text(paste0("D", X[[k]]$ADY), x = x,
    #           y = unit(0, "native") + unit(0.4* ifelse(isUp, -1, 1), "lines"),
    #           just = if (isUp) "top" else "bottom")
    #
    # ## % change
    grid.text(paste(round(chng_k, 0), "%"), x = x,
              y = unit(chng_k, "native") + unit(0.4* ifelse(isUp, 1, -1), "lines"),
              just = if (isUp) "bottom" else "top")
  }

  ## USUBJID and Dose
  grid.text(
    paste0(X[[k]]$PLID),
    x = x, y = unit(0, "npc") - unit(d_usubjid, "lines"), just = "left", rot = 90,
    gp = gpar(fontsize = 11)
  )
}

## Add limits for progressive disease (+20%) and PR (-30%).
grid.segments(x0 = unit(c(0, 0), "npc"),
              x1 = unit(c(1, 1), "npc"),
              y0 = unit(c(20, -30), "native"),
              y1 = unit(c(20, -30), "native"),
              gp = gpar(lty = 2, col = "gray60"))
# 
# grid.rect(x = unit(0.85, "npc"), y = unit(0.85, "npc"),
#           width = unit(0.2, "npc"), height = unit(0.2, "npc"))


vp_lgnd <- viewport(x = unit(0.85, "npc"), y = unit(0.85, "npc"),
                    width = unit(0.2, "npc"), height = unit(0.2, "npc"))

pushViewport(vp_lgnd)
grid.rect()
grid.rect(x = unit(0.1, "npc"), y = unit(0.1 + 0.8/length(color_resp), "npc"),
          width = unit(0.08, "npc"), height = unit(0.08, "npc"), gp = gpar(col = "red", fill = "red"))


g_wf <- function(id, y, col, xlab, ylab, legend, add_text = TRUE, add_days)
  
ggplot(cabbage_exp, aes(x = interaction(Date, Cultivar), y = Weight)) + geom_col() + geom_text(aes(label = Weight), vjust = -0.2)

}