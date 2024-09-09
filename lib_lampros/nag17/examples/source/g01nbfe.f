*     G01NBF Example Program Text
*     Mark 16 Release. NAG Copyright 1992.
*     .. Parameters ..
      INTEGER          NDIM
      PARAMETER        (NDIM=10)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION ABSERR, BETA, Y0
      INTEGER          I, IFAIL, J, L1, L2, LMAX, N
*     .. Local Arrays ..
      DOUBLE PRECISION A(NDIM,NDIM), B(NDIM,NDIM), C(NDIM,NDIM),
     +                 ELA(NDIM), EMU(NDIM), RMOM(12), SIGMA(NDIM,NDIM),
     +                 WK(3*NDIM*NDIM+20*NDIM)
*     .. External Subroutines ..
      EXTERNAL         G01NBF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G01NBF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) BETA, Y0
      READ (NIN,*) N, L1, L2
      IF (N.LE.NDIM .AND. L2.LE.12) THEN
*
*        Compute A, EMU, and SIGMA for simple autoregression
*
         DO 40 I = 1, N
            DO 20 J = I, N
               A(J,I) = 0.0D0
               B(J,I) = 0.0D0
   20       CONTINUE
   40    CONTINUE
         DO 60 I = 1, N - 1
            A(I+1,I) = 0.5D0
            B(I,I) = 1.0D0
   60    CONTINUE
         EMU(1) = Y0*BETA
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
         IFAIL = -1
*
         CALL G01NBF('Ratio','Mean',N,A,NDIM,B,NDIM,C,NDIM,ELA,EMU,
     +               SIGMA,NDIM,L1,L2,LMAX,RMOM,ABSERR,0.0D0,WK,IFAIL)
*
         IF (IFAIL.EQ.0 .OR. IFAIL.GE.6) THEN
            WRITE (NOUT,*)
            WRITE (NOUT,99999) ' N = ', N, ' BETA = ', BETA,
     +        ' Y0 = ', Y0
            WRITE (NOUT,*)
            WRITE (NOUT,*) '      Moments'
            WRITE (NOUT,*)
            J = 0
            DO 160 I = L1, LMAX
               J = J + 1
               WRITE (NOUT,99998) I, RMOM(J)
  160       CONTINUE
         END IF
      END IF
      STOP
*
99999 FORMAT (A,I3,2(A,F6.3))
99998 FORMAT (I3,D12.3)
      END
