*     M01EAF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          MMAX, NMAX
      PARAMETER        (MMAX=20,NMAX=20)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      INTEGER          I, IFAIL, J, K, M, N
*     .. Local Arrays ..
      DOUBLE PRECISION RM(MMAX,NMAX)
      INTEGER          IRANK(MMAX)
*     .. External Subroutines ..
      EXTERNAL         M01DAF, M01EAF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'M01EAF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) M, N, K
      IF (M.GE.1 .AND. M.LE.MMAX .AND. N.GE.1 .AND. N.LE.NMAX .AND.
     +    K.GE.1 .AND. K.LE.N) THEN
         DO 20 I = 1, M
            READ (NIN,*) (RM(I,J),J=1,N)
   20    CONTINUE
         IFAIL = 0
*
         CALL M01DAF(RM(1,K),1,M,'Ascending',IRANK,IFAIL)
*
         DO 40 J = 1, N
*
            CALL M01EAF(RM(1,J),1,M,IRANK,IFAIL)
*
   40    CONTINUE
         WRITE (NOUT,*)
         WRITE (NOUT,99999) 'Matrix sorted on column', K
         WRITE (NOUT,*)
         DO 60 I = 1, M
            WRITE (NOUT,99998) (RM(I,J),J=1,N)
   60    CONTINUE
      END IF
      STOP
*
99999 FORMAT (1X,A,I3)
99998 FORMAT (1X,3F7.1)
      END
