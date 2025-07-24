cjsoma <- sum (count (bio.cj, Lvl.D) [2])
Itsoma <- sum(count (bio.It, Lvl.D)[2])
bcsoma <- sum (count (bio.bc, Lvl.D)[2])
bpsoma <- sum (count (bio.bp, Lvl.D)[2])
fsfsoma <- sum (count (bio.Fsf, Lvl.D)[2])
fbarsoma <- sum (count (bio.Fbar, Lvl.D)[2])

MF3=count (bio.cj, Lvl.D) [2]/cjsoma * 100
MF2=count (bio.It, Lvl.D) [2]/Itsoma * 100
MF1=count (bio.bc, Lvl.D) [2]/bcsoma * 100
MF6=count (bio.bp, Lvl.D) [2]/bpsoma * 100
MF4=count (bio.Fsf, Lvl.D) [2]/fsfsoma * 100
MF5=count (bio.Fbar, Lvl.D) [2]/fbarsoma * 100

MF3_level_dens=count (bio.cj, Lvl.D) [,1]
MF2_level_dens=count (bio.It, Lvl.D) [,1]
MF1_level_dens=count (bio.bc, Lvl.D) [,1]
MF6_level_dens=count (bio.bp, Lvl.D) [,1]
MF4_level_dens=count (bio.Fsf, Lvl.D) [,1]
MF5_level_dens=count (bio.Fbar, Lvl.D) [,1]

MF3_absolute=count (bio.cj, Lvl.D) [,2]
MF2_absolute=count (bio.It, Lvl.D) [,2]
MF1_absolute=count (bio.bc, Lvl.D) [,2]
MF6_absolute=count (bio.bp, Lvl.D) [,2]
MF4_absolute=count (bio.Fsf, Lvl.D) [,2]
MF5_absolute=count (bio.Fbar, Lvl.D) [,2]
