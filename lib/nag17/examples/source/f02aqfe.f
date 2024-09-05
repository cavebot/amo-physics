*     F02AQF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NMAX, IH, IV
      PARAMETER        (NMAX=8,IH=NMAX,IV=NMAX)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      INTEGER          I, IFAIL, J, K, L, N
*     .. Local Arrays ..
      DOUBLE PRECISION H(IH,NMAX), V(IV,NMAX), WI(NMAX), WR(NMAX)
      INTEGER          INTGER(NMAX)
*     .. External Functions ..
      DOUBLE PRECISION X02AJF
      EXTERNAL         X02AJF
*     .. External Subroutines ..
      EXTERNAL         F01AKF, F01APF, F02AQF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'F02AQF Example Program Results'
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
      CALL F01AKF(N,K,L,H,IH,INTGER)
*     Form matrix of accumulated transformations
      CALL F01APF(N,K,L,INTGER,H,IH,V,IV)
*
      IFAIL = 1
*     Eigenvalues and eigenvectors of original matrix
      CALL F02AQF(N,K,L,X02AJF(),H,IH,V,IV,WR,WI,INTGER,IFAIL)
*
      IF (IFAIL.NE.0) THEN
         WRITE (NOUT,99999) 'Error in F02AQF. IFAIL =', IFAIL
      ELSE
         WRITE (NOUT,*)
     +     'Eigenvalues        Iterations (machine dependent)'
         WRITE (NOUT,99997) ('(',WR(I),',',WI(I),')      ',INTGER(I),
     +     I=1,N)
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'Contents of array V'
         DO 20 I = 1, N
            WRITE (NOUT,99998) (V(I,J),J=1,N)
   20    CONTINUE
      END IF
      STOP
*
99999 FORMAT (1X,A,I5)
99998 FORMAT (1X,8F9.4)
99997 FORMAT (1X,A,F7.3,A,F7.3,A,I2)
      END
