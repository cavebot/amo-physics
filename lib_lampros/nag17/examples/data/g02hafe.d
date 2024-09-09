G02HAF Example Program Data
 
 8  3            : N   M
 
 1. -1. -1. 2.1  : X1 X2 X3 Y
 1. -1.  1. 3.6
 1.  1. -1. 4.5
 1.  1.  1. 6.1
 1. -2.  0. 1.3
 1.  0. -2. 1.9
 1.  2.  0. 6.7
 1.  0.  2. 5.5  : End of X1 X2 X3 and Y values
 
 1  2  1         : INDW  IPSI  ISIGMA
 3.0  0          : CUCV  INDC (Only if INDW.ne.0)
 1.5 3.0 4.5     : H1  H2  H3 (Only if IPSI.eq.2)
 1.5             : DCHI (Only if IPSI.gt.0 .and. ISIGMA.gt.1)
