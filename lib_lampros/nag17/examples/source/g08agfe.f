*     G08AGF Example Program Text
*     Mark 14 Release.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          MAXN
      PARAMETER        (MAXN=10)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      DOUBLE PRECISION XME
      PARAMETER        (XME=0.0D0)
*     .. Local Scalars ..
      DOUBLE PRECISION P, RS, RSNOR
      INTEGER          I, IFAIL, N, NZ1
*     .. Local Arrays ..
      DOUBLE PRECISION WRK(3*MAXN), X(MAXN), Y(MAXN), Z(MAXN)
*     .. External Subroutines ..
      EXTERNAL         G08AGF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G08AGF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N
      IF (N.LE.MAXN) THEN
         READ (NIN,*) (X(I),I=1,N), (Y(I),I=1,N)
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'Wilcoxon one sample signed ranks test'
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'Data values'
         WRITE (NOUT,99999) (X(I),I=1,N), (Y(I),I=1,N)
         DO 20 I = 1, N
            Z(I) = X(I) - Y(I)
   20    CONTINUE
         IFAIL = 0
*
         CALL G08AGF(N,Z,XME,'Two-tail','Nozeros',RS,RSNOR,P,NZ1,WRK,
     +               IFAIL)
*
         WRITE (NOUT,*)
         WRITE (NOUT,99998) 'Test statistic            = ', RS
         WRITE (NOUT,99998) 'Normalized test statistic = ', RSNOR
         WRITE (NOUT,99997) 'Degrees of freedom        = ', NZ1
         WRITE (NOUT,99998) 'Two tail probability      = ', P
      ELSE
         WRITE (NOUT,99996) 'N is too large : N = ', N
      END IF
      STOP
*
99999 FORMAT (4X,8F5.1)
99998 FORMAT (1X,A,F8.4)
99997 FORMAT (1X,A,I8)
99996 FORMAT (1X,A,I16)
      END
