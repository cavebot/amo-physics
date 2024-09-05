*     G08AKF Example Program Text
*     Mark 14 Release.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          MAXN1, MAXN2, MAXL, MAXIW
      PARAMETER        (MAXN1=25,MAXN2=25,MAXL=8000,MAXIW=100)
*     .. Local Scalars ..
      DOUBLE PRECISION P, PEXACT, U, UNOR
      INTEGER          I, IFAIL, LWRK, N, N1, N2, NSUM
      LOGICAL          TIES
*     .. Local Arrays ..
      DOUBLE PRECISION RANKS(MAXN1+MAXN2), WRK(MAXL), X(MAXN1), Y(MAXN2)
      INTEGER          IWRK(MAXIW)
*     .. External Subroutines ..
      EXTERNAL         G08AHF, G08AKF
*     .. Intrinsic Functions ..
      INTRINSIC        MIN
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G08AKF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N1, N2
      WRITE (NOUT,*)
      IF ((N1.LE.MAXN1) .AND. (N2.LE.MAXN2)) THEN
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
         IF (TIES) THEN
            NSUM = N1 + N2
            WRITE (NOUT,*)
            WRITE (NOUT,*) 'Ranks'
            WRITE (NOUT,*)
            WRITE (NOUT,99998) '    Group 1  ', (RANKS(I),I=1,N1)
            WRITE (NOUT,*)
            WRITE (NOUT,99998) '    Group 2  ', (RANKS(I),I=N1+1,NSUM)
            N = MIN(N1,N2)
            LWRK = N + N*(N+1)*NSUM - N*(N+1)*(2*N+1)/3 + 1
            WRITE (NOUT,*)
            WRITE (NOUT,99996)
     +        'The length of the workspace is calculated as ', LWRK
            IFAIL = 0
*
            CALL G08AKF(N1,N2,'Lower-tail',RANKS,U,PEXACT,WRK,LWRK,IWRK,
     +                  IFAIL)
*
            WRITE (NOUT,*)
            WRITE (NOUT,99997) 'Exact tail probability = ', PEXACT
         ELSE
            WRITE (NOUT,*)
     +'There are no ties in the pooled sample so G08AKF was not called.'
         END IF
      ELSE
         WRITE (NOUT,*) 'Either N or M is out of range :'
         WRITE (NOUT,99995) 'N1 = ', N1, ' AND N2 = ', N2
      END IF
      STOP
*
99999 FORMAT (1X,A,I5)
99998 FORMAT (1X,A,8F5.1,2(/14X,8F5.1))
99997 FORMAT (1X,A,F10.4)
99996 FORMAT (1X,A,I10)
99995 FORMAT (1X,A,I16,A,I16)
      END
