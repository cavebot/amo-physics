*     F03ADF Example Program Text
*     Mark 15 Revised.  NAG Copyright 1991.
*     .. Parameters ..
      INTEGER          NMAX, IA
      PARAMETER        (NMAX=8,IA=NMAX)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION DETI, DETR
      INTEGER          I, IFAIL, J, N
*     .. Local Arrays ..
      COMPLEX*16       A(IA,NMAX)
      DOUBLE PRECISION WKSPCE(NMAX)
*     .. External Subroutines ..
      EXTERNAL         F03ADF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'F03ADF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N
      WRITE (NOUT,*)
      IF (N.GE.0 .AND. N.LE.NMAX) THEN
         READ (NIN,*) ((A(I,J),J=1,N),I=1,N)
         IFAIL = 0
*
         CALL F03ADF(A,IA,N,DETR,DETI,WKSPCE,IFAIL)
*
         WRITE (NOUT,99998) 'Value of determinant = (', DETR, ',',
     *     DETI, ')'
      ELSE
         WRITE (NOUT,99999) 'N is out of range: N = ', N
      END IF
      STOP
*
99999 FORMAT (1X,A,I5)
99998 FORMAT (1X,A,F9.4,A,F9.4,A)
      END
