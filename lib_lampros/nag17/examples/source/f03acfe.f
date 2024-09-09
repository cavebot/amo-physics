*     F03ACF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NMAX, MMAX, IA, IL, M1MAX
      PARAMETER        (NMAX=10,MMAX=4,IA=NMAX,IL=NMAX,M1MAX=MMAX+1)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION DETERM
      INTEGER          I, IFAIL, J, M, N
*     .. Local Arrays ..
      DOUBLE PRECISION A(IA,M1MAX), RL(IL,M1MAX)
*     .. External Subroutines ..
      EXTERNAL         F03ACF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'F03ACF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N, M
      WRITE (NOUT,*)
      IF (N.LT.1 .OR. N.GT.NMAX .OR. M.LT.1 .OR. M.GT.MMAX) THEN
         WRITE (NOUT,99999) 'N or M out of range: N = ', N, '  M = ', M
      ELSE
         READ (NIN,*) ((A(I,J),J=1,M+1),I=1,N)
         IFAIL = 1
*
         CALL F03ACF(A,IA,N,M,DETERM,RL,IL,M+1,IFAIL)
*
         IF (IFAIL.NE.0) THEN
            WRITE (NOUT,99999) 'Error in F03ACF. IFAIL =', IFAIL
         ELSE
            WRITE (NOUT,99998) 'Value of determinant = ', DETERM
         END IF
      END IF
      STOP
*
99999 FORMAT (1X,A,I5,A,I5)
99998 FORMAT (1X,A,F9.4)
      END
