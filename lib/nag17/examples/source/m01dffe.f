*     M01DFF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          MMAX, NMAX
      PARAMETER        (MMAX=20,NMAX=20)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      INTEGER          I, IFAIL, J, M, N
*     .. Local Arrays ..
      INTEGER          IM(MMAX,NMAX), IRANK(MMAX)
*     .. External Subroutines ..
      EXTERNAL         M01DFF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'M01DFF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) M, N
      IF (M.GE.1 .AND. M.LE.MMAX .AND. N.GE.1 .AND. N.LE.NMAX) THEN
         DO 20 I = 1, M
            READ (NIN,*) (IM(I,J),J=1,N)
   20    CONTINUE
         IFAIL = 0
*
         CALL M01DFF(IM,MMAX,1,M,1,N,'Descending',IRANK,IFAIL)
*
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'Data                        Ranks'
         WRITE (NOUT,*)
         DO 40 I = 1, M
            WRITE (NOUT,99999) (IM(I,J),J=1,N), IRANK(I)
   40    CONTINUE
      END IF
      STOP
*
99999 FORMAT (1X,3I7,I11)
      END
