*     F02APF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NMAX, IH
      PARAMETER        (NMAX=8,IH=NMAX)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      INTEGER          I, IFAIL, J, K, L, N
*     .. Local Arrays ..
      DOUBLE PRECISION H(IH,NMAX), WI(NMAX), WR(NMAX)
      INTEGER          ICNT(NMAX)
*     .. External Functions ..
      DOUBLE PRECISION X02AJF
      EXTERNAL         X02AJF
*     .. External Subroutines ..
      EXTERNAL         F01AKF, F02APF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'F02APF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N
      WRITE (NOUT,*)
      IF (N.LT.1 .OR. N.GT.NMAX) THEN
         WRITE (NOUT,99999) 'N is out of range: N = ', N
         STOP
      END IF
      READ (NIN,*) ((H(I,J),J=1,N),I=1,N)
      K = 1
      L = N
*
*     Reduce to upper Hessenberg form
      CALL F01AKF(N,K,L,H,IH,ICNT)
*
      IFAIL = 1
*     Eigenvalues of Hessenberg form
      CALL F02APF(N,X02AJF(),H,IH,WR,WI,ICNT,IFAIL)
*
      IF (IFAIL.NE.0) THEN
         WRITE (NOUT,99999) 'Error in F02APF. IFAIL =', IFAIL
      ELSE
         WRITE (NOUT,*)
     +     'Eigenvalues       Iterations (machine dependent)'
         WRITE (NOUT,99998) ('(',WR(I),',',WI(I),')     ',ICNT(I),I=1,N)
      END IF
      STOP
*
99999 FORMAT (1X,A,I5)
99998 FORMAT (1X,A,F7.3,A,F7.3,A,I2)
      END
