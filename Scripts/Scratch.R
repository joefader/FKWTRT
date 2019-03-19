
M <- matrix(1:6, nrow=3, byrow=TRUE)
col <- matrix(c(0,0,0), nrow=3, byrow=TRUE)
M <- cbind(M,col)

M[,3] = apply(M, 1, function(x) 2*x[1]+x[2])




sets_deep_all %>% 
  filter(YEAR == 2015) %>% 
  filter(rdist.earth.vec(matrix(c(HAUL_BEGIN_LON[2], HAUL_BEGIN_LAT[2]), ncol=2), 
                         matrix(c(HAUL_BEGIN_LON, HAUL_BEGIN_LAT), ncol=2),
                  miles = F, R = 6371) < 100) %>% 
  summarise(mean(CPUE, na.rm=T)) %>% as_vector()


#sets_deep_all %>% 
rdist.earth.vec(coordinates, 
                matrix(c(sets_deep_all$HAUL_BEGIN_LON[2], sets_deep_all$HAUL_BEGIN_LAT[2]), ncol=2),
                miles = F, R = 6371)


coordinates <- matrix(c(sets_deep_all$HAUL_BEGIN_LON[1], sets_deep_all$HAUL_BEGIN_LAT[1]), ncol=2)
  
  matrix(c(sets_deep_all[1,"HAUL_BEGIN_LON"], sets_deep_all[1,"HAUL_BEGIN_LAT"]), ncol=2)
