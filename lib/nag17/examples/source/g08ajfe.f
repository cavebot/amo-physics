*     G08AJF Example Program Text
*     Mark 14 Release.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          MAXN1, MAXN2, MAXL
      PARAMETER        (MAXN1=25,MAXN2=25,MAXL=200)
*     .. Local Scalars ..
      DOUBLE PRECISION P, PEXACT, U, UNOR
      INTEGER          I, IFAIL, LWRK, N1, N2
      LOGICAL          TIES
*     .. Local Arrays ..
      DOUBLE PRECISION RANKS(MAXN1+MAXN2), WRK(MAXL), X(MAXN1), Y(MAXN2)
*     .. External Subroutines ..
      EXTERNAL         G08AHF, G08AJF
*     .. Intrinsic Functions ..
      INTRINSIC        INT
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G08AJF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N1, N2
      WRITE (NOUT,*)
      IF (N1.LE.MAXN1 .AND. N2.LE.MAXN2) THEN
         WRITE (NOUT,99999) 'Sample size of group 1 = ', N1
         WRITE (NOUT,99999) 'Sample size of group 2 = ', N2
         WRITE (NOUT,*)
         READ (NIN,*) (X(I),I=1,N1)
         WRITE (NOUT,*) 'Mann-Whitney U test'
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'Data values'
         WRITE (NOUT,*)
         WRITE (NOUT,99998) '    Group 1  ', (X(I),I=1,N1)
         READ (NIN,*) (Y(I),I=1,N2)
         WRITE (NOUT,*)
         WRITE (NOUT,99998) '    Group 2  ', (Y(I),I=1,N2)
         IFAIL = 0
*
         CALL G08AHF(N1,X,N2,Y,'Lower-tail',U,UNOR,P,TIES,RANKS,WRK,
     +               IFAIL)
*
         WRITE (NOUT,*)
         WRITE (NOUT,99997) 'Test statistic     = ', U
         WRITE (NOUT,99997) 'Normal statistic   = ', UNOR
         WRITE (NOUT,99997) 'Tail probability   = ', P
         WRITE (NOUT,*)
         IF ( .NOT. TIES) THEN
            LWRK = INT(N1*N2/2) + 1
            WRITE (NOUT,99996)
     +        'The length of the workspace is calculated as ', LWRK
            IFAIL = 0
*
            CALL G08AJF(N1,N2,'Lower-tail',U,PEXACT,WRK,LWRK,IFAIL)
*
            WRITE (NOUT,*)
            WRITE (NOUT,99997) 'Exact tail probability = ', PEXACT
         ELSE
            WRITE (NOUT,*)
     +   'There are ties in the pooled sample so G08AJF was not called.'
         END IF
      ELSE
         WRITE (NOUT,*) 'Either N1 or N2 is out of range :'
         WRITE (NOUT,99995) 'N1 = ', N1, ' and N2 = ', N2
      END IF
      STOP
*
99999 FORMAT (1X,A,I5)
99998 FORMAT (1X,A,8F5.1,2(/14X,8F5.1))
99997 FORMAT (1X,A,F10.4)
99996 FORMAT (1X,A,I10)
99995 FORMAT (1X,A,I16,A,I16)
      END
