*     G02GNF Example Program Text
*     Mark 14 Release.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NMAX, MMAX
      PARAMETER        (NMAX=15,MMAX=9)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION A, DEV, EPS, SESTAT, STAT, TOL, Z
      INTEGER          I, IDF, IFAIL, IP, IPRINT, IRANK, J, M, MAXIT, N,
     +                 NESTFN
      LOGICAL          EST
*     .. Local Arrays ..
      DOUBLE PRECISION B(MMAX), COV((MMAX*MMAX+MMAX)/2), F(MMAX),
     +                 SE(MMAX), V(NMAX,7+MMAX),
     +                 WK((MMAX*MMAX+3*MMAX+22)/2), WT(NMAX),
     +                 X(NMAX,MMAX), Y(NMAX)
      INTEGER          ISX(MMAX)
*     .. External Subroutines ..
      EXTERNAL         G02GCF, G02GNF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G02GNF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N, M, IPRINT
      IF (N.LE.NMAX .AND. M.LT.MMAX) THEN
         DO 20 I = 1, N
            READ (NIN,*) (X(I,J),J=1,M), Y(I)
   20    CONTINUE
         READ (NIN,*) (ISX(J),J=1,M), IP
*        Set control parameters
         EPS = 0.000001D0
         TOL = 0.00005D0
         MAXIT = 10
         IFAIL = -1
*
*        Fit Log-linear model using G02GCF
         CALL G02GCF('L','M','N','U',N,X,NMAX,M,ISX,IP,Y,WT,A,DEV,IDF,B,
     +               IRANK,SE,COV,V,NMAX,TOL,MAXIT,IPRINT,EPS,WK,IFAIL)
*
         IF (IFAIL.EQ.0 .OR. IFAIL.GE.7) THEN
            WRITE (NOUT,*)
            WRITE (NOUT,99999) 'Deviance = ', DEV
            WRITE (NOUT,99998) 'Degrees of freedom = ', IDF
            WRITE (NOUT,*)
            WRITE (NOUT,*) '      Estimate     Standard error'
            WRITE (NOUT,*)
            DO 40 I = 1, IP
               WRITE (NOUT,99997) B(I), SE(I)
   40       CONTINUE
            READ (NIN,*) NESTFN
            DO 60 I = 1, NESTFN
               READ (NIN,*) (F(J),J=1,IP)
               IFAIL = -1
*
               CALL G02GNF(IP,IRANK,B,COV,V,NMAX,F,EST,STAT,SESTAT,Z,
     +                     TOL,WK,IFAIL)
*
               IF (IFAIL.EQ.0 .OR. IFAIL.EQ.2) THEN
                  WRITE (NOUT,*)
                  WRITE (NOUT,99996) 'Function ', I
                  WRITE (NOUT,99995) (F(J),J=1,IP)
                  WRITE (NOUT,*)
                  IF (EST) THEN
                     WRITE (NOUT,99994) 'STAT = ', STAT, ' SE = ',
     +                 SESTAT, ' Z = ', Z
                  ELSE
                     WRITE (NOUT,*) 'Function not estimable'
                  END IF
               END IF
   60       CONTINUE
         END IF
      END IF
      STOP
*
99999 FORMAT (1X,A,D12.4)
99998 FORMAT (1X,A,I2)
99997 FORMAT (1X,2F14.4)
99996 FORMAT (1X,A,I4)
99995 FORMAT (1X,5F8.2)
99994 FORMAT (1X,A,F10.4,A,F10.4,A,F10.4)
      END
