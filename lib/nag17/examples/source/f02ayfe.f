*     F02AYF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NMAX, IA, IB
      PARAMETER        (NMAX=4,IA=NMAX,IB=NMAX)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION EPS
      INTEGER          I, IFAIL, J, N
*     .. Local Arrays ..
      DOUBLE PRECISION A(IA,NMAX), B(IB,NMAX), D(NMAX), E(NMAX),
     +                 WK1(NMAX), WK2(NMAX)
*     .. External Functions ..
      DOUBLE PRECISION X02AJF
      EXTERNAL         X02AJF
*     .. External Subroutines ..
      EXTERNAL         F01BCF, F02AYF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'F02AYF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N
      WRITE (NOUT,*)
      IF (N.LT.1 .OR. N.GT.NMAX) THEN
         WRITE (NOUT,99999) 'N is out of range: N = ', N
         STOP
      END IF
      READ (NIN,*) ((A(I,J),B(I,J),J=1,N),I=1,N)
*
*     Reduce to tridiagonal form
      CALL F01BCF(N,0.0D0,A,IA,B,IB,D,E,WK1,WK2)
*
      EPS = X02AJF()
      IFAIL = 1
*
*     Eigenvalues and eigenvectors of original matrix
      CALL F02AYF(N,EPS,D,E,A,IA,B,IB,IFAIL)
*
      IF (IFAIL.NE.0) THEN
         WRITE (NOUT,99999) 'Error in F02AYF. IFAIL =', IFAIL
      ELSE
         WRITE (NOUT,*) 'Eigenvalues'
         WRITE (NOUT,99998) (D(I),I=1,N)
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'Eigenvectors'
         DO 20 I = 1, N
            WRITE (NOUT,99997) (' (',A(I,J),',',B(I,J),')',J=1,N)
   20    CONTINUE
      END IF
      STOP
*
99999 FORMAT (1X,A,I5)
99998 FORMAT (1X,4(F13.4,5X))
99997 FORMAT (1X,4(A,F7.3,A,F7.3,A))
      END
