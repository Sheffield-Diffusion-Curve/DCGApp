source("src/elicitationIO.r")


sol <- pq2nt_discrete(m=140, p=0.031, q=0.472)

cud1 <- generate_diffusion_discrete(m=100, p=0.058, q=0.543, t_min=0, t_max=20, dt= 1)
cud <- generate_diffusion_discrete(m=100, p=0.058, q=0.543, t_min=0, t_max=20, dt= 0.001)
cuc <- generate_diffusion_continuous(m=100, p=0.058, q=0.543, t_min=0, t_max=20)


system.time({
sol.pq <<-sapply(1:1000, function(x) nt2pq_discrete(140, 4.99, 6.26))
})

system.time({
  sol.pq <<-sapply(1:1000, function(x) nt2pq_continuous(140, 4.99, 6.26))
})





Data <- getDefaultInputData(n=2)

Elcs <- inputELCs(Data)

SimuNT <- randMNT(Elcs, 1000, "mixture")
  
  
SimuPQ <- translateNT2PQ(SimuNT)





