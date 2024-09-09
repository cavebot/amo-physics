*     F05AAF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          MMAX, IA, N2MAX
      PARAMETER        (MMAX=5,IA=MMAX,N2MAX=5)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION CC
      INTEGER          I, ICOL, IFAIL, J, M, N1, N2
*     .. Local Arrays ..
      DOUBLE PRECISION A(IA,N2MAX), S(N2MAX)
*     .. External Subroutines ..
      EXTERNAL         F05AAF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'F05AAF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) M, N1, N2
      WRITE (NOUT,*)
      WRITE (NOUT,99999) 'N1 = ', N1, '  N2 = ', N2
      IF (M.GT.0 .AND. M.LE.MMAX .AND. N2.GT.0 .AND. N2.LE.N2MAX) THEN
         READ (NIN,*) ((A(I,J),J=1,M),I=1,M)
         IFAIL = 1
*
         CALL F05AAF(A,IA,M,N1,N2,S,CC,ICOL,IFAIL)
*
         WRITE (NOUT,*)
         IF (IFAIL.NE.0) THEN
            WRITE (NOUT,99999) 'Error in F05AAF. IFAIL =', IFAIL
         ELSE
            WRITE (NOUT,99998) 'CC = ', CC, ' ICOL = ', ICOL
            WRITE (NOUT,*)
            WRITE (NOUT,*) 'Final matrix'
            WRITE (NOUT,99997) ((A(I,J),J=1,M),I=1,M)
         END IF
      ELSE
         WRITE (NOUT,*) 'M or N2 is out of range'
         WRITE (NOUT,99996) 'M = ', M, ' N2 = ', N2
      END IF
      STOP
*
99999 FORMAT (1X,A,I2,A,I2)
99998 FORMAT (1X,A,F7.4,A,I2)
99997 FORMAT (1X,4F9.4)
99996 FORMAT (1X,A,I5,A,I5)
      END
