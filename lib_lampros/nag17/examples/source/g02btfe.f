*     G02BTF Example Program Text
*     Mark 14 Release.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          INCX, MMAX, MP
      PARAMETER        (INCX=1,MMAX=18,MP=(MMAX*(MMAX+1))/2)
      DOUBLE PRECISION ONE
      PARAMETER        (ONE=1.0D0)
*     .. Local Scalars ..
      DOUBLE PRECISION ALPHA, SW, WT
      INTEGER          I, IFAIL, J, M, MM, N, NPRINT
      CHARACTER        MEAN
*     .. Local Arrays ..
      DOUBLE PRECISION C(MP), V(MP), X(MMAX*INCX), XBAR(MMAX)
*     .. External Subroutines ..
      EXTERNAL         F06FDF, G02BTF, X04CCF
*     .. Intrinsic Functions ..
      INTRINSIC        MOD
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G02BTF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*,END=40) MEAN, M, N, NPRINT
      SW = 0.0D0
      IF (M.LT.MMAX) THEN
         DO 20 I = 1, N
            READ (NIN,*) WT, (X(J),J=1,M)
            IFAIL = 0
*
*           Calculate the sums of squares and cross-products matrix
            CALL G02BTF(MEAN,M,WT,X,INCX,SW,XBAR,C,IFAIL)
*
            IF (MOD(I,NPRINT).EQ.0 .OR. I.EQ.N) THEN
               WRITE (NOUT,*)
               WRITE (NOUT,*)
     +           '---------------------------------------------'
               WRITE (NOUT,99999) 'Observation: ', I, '      Weight = ',
     +           WT
               WRITE (NOUT,*)
     +           '---------------------------------------------'
               WRITE (NOUT,*)
               WRITE (NOUT,*) 'Means'
               WRITE (NOUT,99998) (XBAR(J),J=1,M)
               WRITE (NOUT,*)
*              Print the sums of squares and cross products matrix
               CALL X04CCF('Upper','Non-unit',M,C,
     +                     'Sums of squares and cross-products',IFAIL)
               IF (SW.GT.ONE) THEN
*                 Calculate the variance matrix
                  ALPHA = ONE/(SW-ONE)
                  MM = (M*(M+1))/2
                  CALL F06FDF(MM,ALPHA,C,1,V,1)
*                 Print the variance matrix
                  WRITE (NOUT,*)
                  CALL X04CCF('Upper','Non-unit',M,V,'Variance matrix',
     +                        IFAIL)
               END IF
            END IF
   20    CONTINUE
      ELSE
         WRITE (NOUT,99997) 'M is too large. M =', M
      END IF
   40 STOP
*
99999 FORMAT (1X,A,I4,A,F13.4)
99998 FORMAT (1X,4F14.4)
99997 FORMAT (1X,A,I5)
      END
