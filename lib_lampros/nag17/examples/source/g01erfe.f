*     G01ERF Example Program Text
*     Mark 16 Release. NAG Copyright 1992.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION P, T, VK
      INTEGER          I, IFAIL, N
*     .. External Functions ..
      DOUBLE PRECISION G01ERF
      EXTERNAL         G01ERF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G01ERF Example Program Results'
      WRITE (NOUT,*)
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N
      DO 20 I = 1, N
         READ (NIN,*) T, VK
         IFAIL = 1
         P = G01ERF(T,VK,IFAIL)
         WRITE (NOUT,99999) P
   20 CONTINUE
      STOP
*
99999 FORMAT (' P = ',F10.4)
      END
