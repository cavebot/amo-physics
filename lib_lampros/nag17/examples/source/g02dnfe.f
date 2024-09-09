*     G02DNF Example Program Text
*     Mark 14 Release.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          MMAX, NMAX
      PARAMETER        (MMAX=5,NMAX=12)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION RSS, SESTAT, STAT, T, TOL
      INTEGER          I, IDF, IFAIL, IP, IRANK, J, M, N, NESTFN
      LOGICAL          EST, SVD
      CHARACTER        MEAN, WEIGHT
*     .. Local Arrays ..
      DOUBLE PRECISION B(MMAX), COV((MMAX*MMAX+MMAX)/2), F(MMAX),
     +                 H(NMAX), P(MMAX*(MMAX+2)), Q(NMAX,MMAX+1),
     +                 RES(NMAX), SE(MMAX), WK(MMAX*MMAX+5*(MMAX-1)),
     +                 WT(NMAX), X(NMAX,MMAX), Y(NMAX)
      INTEGER          ISX(MMAX)
*     .. External Subroutines ..
      EXTERNAL         G02DAF, G02DNF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G02DNF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N, M, WEIGHT, MEAN
      IF (N.LE.NMAX .AND. M.LT.MMAX) THEN
         IF (WEIGHT.EQ.'W' .OR. WEIGHT.EQ.'w') THEN
            DO 20 I = 1, N
               READ (NIN,*) (X(I,J),J=1,M), Y(I), WT(I)
   20       CONTINUE
         ELSE
            DO 40 I = 1, N
               READ (NIN,*) (X(I,J),J=1,M), Y(I)
   40       CONTINUE
         END IF
         READ (NIN,*) (ISX(J),J=1,M), IP
*        Set tolerance
         TOL = 0.00001D0
         IFAIL = 0
*
*        Find initial estimates using G02DAF
         CALL G02DAF(MEAN,WEIGHT,N,X,NMAX,M,ISX,IP,Y,WT,RSS,IDF,B,SE,
     +               COV,RES,H,Q,NMAX,SVD,IRANK,P,TOL,WK,IFAIL)
*
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'Estimates from G02DAF'
         WRITE (NOUT,*)
         WRITE (NOUT,99999) 'Residual sum of squares = ', RSS
         WRITE (NOUT,99998) 'Degrees of freedom = ', IDF
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'Variable   Parameter estimate   Standard error'
         WRITE (NOUT,*)
         DO 60 J = 1, IP
            WRITE (NOUT,99997) J, B(J), SE(J)
   60    CONTINUE
         READ (NIN,*) NESTFN
         DO 80 I = 1, NESTFN
            READ (NIN,*) (F(J),J=1,IP)
            IFAIL = -1
*
            CALL G02DNF(IP,IRANK,B,COV,P,F,EST,STAT,SESTAT,T,TOL,WK,
     +                  IFAIL)
*
            IF (IFAIL.EQ.0 .OR. IFAIL.EQ.2) THEN
               WRITE (NOUT,*)
               WRITE (NOUT,99996) 'Function ', I
               WRITE (NOUT,*)
               WRITE (NOUT,99995) (F(J),J=1,IP)
               WRITE (NOUT,*)
               IF (EST) THEN
                  WRITE (NOUT,99994) 'STAT = ', STAT, ' SE = ', SESTAT,
     +              ' T = ', T
               ELSE
                  WRITE (NOUT,*) 'Function not estimable'
               END IF
            END IF
   80    CONTINUE
      END IF
      STOP
*
99999 FORMAT (1X,A,D12.4)
99998 FORMAT (1X,A,I4)
99997 FORMAT (1X,I6,2D20.4)
99996 FORMAT (1X,A,I4)
99995 FORMAT (1X,5F8.2)
99994 FORMAT (1X,A,F10.4,A,F10.4,A,F10.4)
      END
