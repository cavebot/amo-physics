*     D01PAF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NDIM, IV1, IV2, MXORD
      PARAMETER        (NDIM=3,IV1=NDIM+1,IV2=2*(NDIM+1),MXORD=5)
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION ESTERR
      INTEGER          IFAIL, J, K, MAXORD, MINORD, NEVALS
*     .. Local Arrays ..
      DOUBLE PRECISION FINVLS(MXORD), VERTEX(IV1,IV2)
*     .. External Functions ..
      DOUBLE PRECISION FUNCTN
      EXTERNAL         FUNCTN
*     .. External Subroutines ..
      EXTERNAL         D01PAF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'D01PAF Example Program Results'
      DO 40 J = 1, IV1
         DO 20 K = 1, NDIM
            VERTEX(J,K) = 0.0D0
   20    CONTINUE
         IF (J.GT.1) VERTEX(J,J-1) = 1.0D0
   40 CONTINUE
      MINORD = 0
      NEVALS = 1
      WRITE (NOUT,*)
      WRITE (NOUT,*)
     +  'MAXORD   Estimated      Estimated         Integrand'
      WRITE (NOUT,*)
     +  '           value         accuracy        evaluations'
      DO 60 MAXORD = 1, MXORD
         IFAIL = 0
*
         CALL D01PAF(NDIM,VERTEX,IV1,IV2,FUNCTN,MINORD,MAXORD,FINVLS,
     +               ESTERR,IFAIL)
*
         WRITE (NOUT,99999) MAXORD, FINVLS(MAXORD), ESTERR, NEVALS
         NEVALS = (NEVALS*(MAXORD+NDIM+1))/MAXORD
   60 CONTINUE
      STOP
*
99999 FORMAT (1X,I4,F13.5,D16.3,I15)
      END
*
      DOUBLE PRECISION FUNCTION FUNCTN(NDIM,X)
*     .. Scalar Arguments ..
      INTEGER                          NDIM
*     .. Array Arguments ..
      DOUBLE PRECISION                 X(NDIM)
*     .. Intrinsic Functions ..
      INTRINSIC                        COS, EXP
*     .. Executable Statements ..
      FUNCTN = EXP(X(1)+X(2)+X(3))*COS(X(1)+X(2)+X(3))
      RETURN
      END
