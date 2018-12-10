translateNT2PQ <- function(mnt, progress=NULL) {
  m <- mnt$M
  n <- mnt$N
  t <- mnt$T
  size <- nrow(mnt)

  mpq <- matrix(0, size, 3)
  for (i in 1:size) {
    mpq[i, ] <- unlist(nt2pq_continuous(m[i], n[i], t[i]))
    
    if (is.function(progress)) {
      progress(i, detail = paste0(round(i/size * 100), "%"))
    }
    
  }
  colnames(mpq) <- c("M", "P", "Q")
  data.frame(mpq)
}


simulateCurves <- function(mpq, t_max=20) {
  size <- nrow(mpq)
  
  res <- array(0, c(t_max+1, 3, size))
  for (i in 1:size) {
    curve <- generate_diffusion_continuous(m=mpq[i, 1], p=mpq[i, 2], q=mpq[i, 3], t_min=0, t_max=t_max)
    res[,,i] <- as.matrix(curve)
  }
  dimnames(res)[[2]] <- c("Time", "N", "dN")
  res
}
