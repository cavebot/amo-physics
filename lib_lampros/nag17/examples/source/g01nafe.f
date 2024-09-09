*     G01NAF Example Program Text
*     Mark 16 Release. NAG Copyright 1992.
*     .. Parameters ..
      INTEGER          NDIM
      PARAMETER        (NDIM=10)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION BETA, CON
      INTEGER          I, IFAIL, J, L, N
*     .. Local Arrays ..
      DOUBLE PRECISION A(NDIM,NDIM), EMU(NDIM), RKUM(12), RMOM(12),
     +                 SIGMA(NDIM,NDIM), WK(3*NDIM*(NDIM+1)/2+NDIM)
*     .. External Subroutines ..
      EXTERNAL         G01NAF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G01NAF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) BETA, CON
      READ (NIN,*) N, L
      IF (N.LE.NDIM .AND. L.LE.12) THEN
*
*        Compute A, EMU, and SIGMA for simple autoregression
*
         DO 40 I = 1, N
            DO 20 J = I, N
               A(J,I) = 0.0D0
   20       CONTINUE
   40    CONTINUE
         DO 60 I = 1, N - 1
            A(I+1,I) = 0.5D0
   60    CONTINUE
         EMU(1) = CON*BETA
         DO 80 I = 1, N - 1
            EMU(I+1) = BETA*EMU(I)
   80    CONTINUE
         SIGMA(1,1) = 1.0D0
         DO 100 I = 2, N
            SIGMA(I,I) = BETA*BETA*SIGMA(I-1,I-1) + 1.0D0
  100    CONTINUE
         DO 140 I = 1, N
            DO 120 J = I + 1, N
               SIGMA(J,I) = BETA*SIGMA(J-1,I)
  120       CONTINUE
  140    CONTINUE
         IFAIL = 0
*
         CALL G01NAF('M','M',N,A,NDIM,EMU,SIGMA,NDIM,L,RKUM,RMOM,WK,
     +               IFAIL)
*
         WRITE (NOUT,*)
         WRITE (NOUT,99999) ' N = ', N, ' BETA = ', BETA, ' CON = ',
     +     CON
         WRITE (NOUT,*)
         WRITE (NOUT,*) '    Cumulants       Moments'
         WRITE (NOUT,*)
         DO 160 I = 1, L
            WRITE (NOUT,99998) I, RKUM(I), RMOM(I)
  160    CONTINUE
      END IF
      STOP
*
99999 FORMAT (A,I3,2(A,F6.3))
99998 FORMAT (I3,D12.4,4X,D12.4)
      END
