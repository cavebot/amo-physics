*     F04YAF Example Program Text.
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          PMAX, NRX, LWORK
      PARAMETER        (PMAX=10,NRX=PMAX,LWORK=4*PMAX)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION SIGMA, TOL
      INTEGER          I, IFAIL, IRANK, J, JOB, N, P
      LOGICAL          SVD
*     .. Local Arrays ..
      DOUBLE PRECISION CJ(PMAX), WORK(LWORK), X(NRX,PMAX), Y(PMAX)
*     .. External Subroutines ..
      EXTERNAL         F04JGF, F04YAF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'F04YAF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N, P
      TOL = 5.0D-4
      IFAIL = 0
      IF (N.GT.0 .AND. N.LE.NRX .AND. P.GT.0 .AND. P.LE.PMAX) THEN
         READ (NIN,*) ((X(I,J),J=1,P),I=1,N)
         READ (NIN,*) (Y(I),I=1,N)
*
         CALL F04JGF(N,P,X,NRX,Y,TOL,SVD,SIGMA,IRANK,WORK,LWORK,IFAIL)
*
         WRITE (NOUT,*)
         WRITE (NOUT,99999) 'SIGMA =', SIGMA, '  Rank =', IRANK,
     +     '  SVD =', SVD
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'Solution vector'
         WRITE (NOUT,99998) (Y(I),I=1,P)
         JOB = 0
*
         CALL F04YAF(JOB,P,SIGMA,X,NRX,SVD,IRANK,WORK,CJ,WORK(P+1),
     +               IFAIL)
*
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'Estimated variances of regression coefficients'
         WRITE (NOUT,99998) (CJ(J),J=1,P)
      END IF
      STOP
*
99999 FORMAT (1X,A,F9.4,A,I3,A,L3)
99998 FORMAT (1X,7F9.4)
      END
