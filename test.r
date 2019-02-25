source("src/elicitationIO.r")


sol <- pq2nt_discrete(m=140, p=0.031, q=0.472)

cud1 <- generate_diffusion_discrete(m=100, p=0.058, q=0.543, t_min=0, t_max=20, dt= 1)
cud <- generate_diffusion_discrete(m=100, p=0.058, q=0.543, t_min=0, t_max=20, dt= 0.001)
cuc <- generate_diffusion_continuous(m=100, p=0.058, q=0.543, t_min=0, t_max=20)


system.time({
sol.pq <<-sapply(1:100, function(x) nt2pq_discrete(140, 4.99, 6.26))
})

system.time({
  sol.pq <<-sapply(1:100, function(x) nt2pq_continuous(140, 4.99, 6.26))
})


library(DCGen)

dat <- getDefaultInputData()
show <- showInputData(dat)

ExpA = new_expert("A", "Triangle", c(54.2, 10, 150), "Triangle", c(2.3, 0, 5), "Triangle", c(5.1, 3, 8))
ExpB = new_expert("B", "Triangle", c(158.8, 30, 230), "Triangle", c(5.7, 2, 15), "Triangle", c(9.9, 7, 13))
ExpC = new_expert("C", "Triangle", c(204.4, 30, 410), "Triangle", c(7.1, 2, 10), "Triangle", c(3.5, 2, 6))

experts <- aggregate_experts(list(ExpA, ExpB, ExpC))


pars <- rand_parameters(experts, 300, method='mixture', type='continuous')

cvs <- generate_diffusion_curves(pars, t_max=20)

inp <- list(
  dnShow = T,
  centVal = 70,
  curveType = "stats",
  avgType = "mean"
)

mcvs <- apply(cvs, c(1,2), mean)
mcvs <- round(mcvs, 2)
mcvs <- mcvs[mcvs[, 1] == round(mcvs[, 1]),]
mcvs <- mcvs[seq.int(1, nrow(mcvs), by=max(round(nrow(mcvs)/10), 1)), ]
rownames(mcvs) <- rep("", nrow(mcvs))

library(rmarkdown)
out <- render(input = "report_template.Rmd", #pdf_document()
              #output_format = word_document(),
              output_format=format,
              output_dir="temp", 
              output_file=filename, 
              envir = list(dat=dat, pars=pars, curves=cvs, mean_curves=mcvs,
                           input=inp))
