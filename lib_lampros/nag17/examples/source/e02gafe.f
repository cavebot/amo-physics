*     E02GAF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          MMAX, LA, NPLUS2
      PARAMETER        (MMAX=5,LA=MMAX+2,NPLUS2=5)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION RESID, T, TOL
      INTEGER          I, IFAIL, ITER, M, RANK
*     .. Local Arrays ..
      DOUBLE PRECISION A(LA,NPLUS2), B(MMAX), X(NPLUS2)
      INTEGER          IWORK(MMAX)
*     .. External Subroutines ..
      EXTERNAL         E02GAF
*     .. Intrinsic Functions ..
      INTRINSIC        EXP
*     .. Executable Statements ..
      WRITE (NOUT,*) 'E02GAF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) M
      IF (M.GT.0 .AND. M.LE.MMAX) THEN
         DO 20 I = 1, M
            READ (NIN,*) T, B(I)
            A(I,1) = EXP(T)
            A(I,2) = EXP(-T)
            A(I,3) = 1.0D0
   20    CONTINUE
         TOL = 0.0D0
         IFAIL = 1
*
         CALL E02GAF(M,A,LA,B,NPLUS2,TOL,X,RESID,RANK,ITER,IWORK,IFAIL)
*
         IF (IFAIL.LE.1) THEN
            WRITE (NOUT,*)
            WRITE (NOUT,99999) 'Resid = ', RESID, '  Rank = ', RANK,
     +        '  Iterations = ', ITER, '  IFAIL =', IFAIL
            WRITE (NOUT,*)
            WRITE (NOUT,*) 'Solution'
            WRITE (NOUT,99998) (X(I),I=1,NPLUS2-2)
         ELSE
            WRITE (NOUT,99997) 'E02GAF fails with error', IFAIL
         END IF
      END IF
      STOP
*
99999 FORMAT (1X,A,D10.2,A,I5,A,I5,A,I5)
99998 FORMAT (1X,6F10.4)
99997 FORMAT (1X,A,I2)
      END
