*     D05BDF Example Program Text
*     Mark 16 Release. NAG Copyright 1992.
*     .. Parameters ..
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
      INTEGER          IORDER, NMESH, LCT, LWK
      PARAMETER        (IORDER=4,NMESH=2**6+2*IORDER-1,LCT=NMESH/32+1,
     +                 LWK=(2*IORDER+6)*NMESH+8*IORDER*IORDER-
     +                 16*IORDER+1)
*     .. Local Scalars ..
      DOUBLE PRECISION ERR, ERRMAX, H, HI1, SOLN, T, TLIM, TOLNL
      INTEGER          I, IFAIL
*     .. Local Arrays ..
      DOUBLE PRECISION WORK(LWK), YN(NMESH)
      INTEGER          NCT(LCT)
*     .. External Functions ..
      DOUBLE PRECISION CF1, CF2, CG1, CG2, CK1, CK2, X02AJF
      EXTERNAL         CF1, CF2, CG1, CG2, CK1, CK2, X02AJF
*     .. External Subroutines ..
      EXTERNAL         D05BDF
*     .. Intrinsic Functions ..
      INTRINSIC        ABS, DBLE, MOD, SQRT
*     .. Executable Statements ..
      WRITE (NOUT,*) 'D05BDF Example Program Results'
      WRITE (NOUT,*)
      IFAIL = 0
      TLIM = 7.0D0
      TOLNL = SQRT(X02AJF())
      H = TLIM/(NMESH-1)
*
      CALL D05BDF(CK1,CF1,CG1,'Initial',IORDER,TLIM,TOLNL,NMESH,YN,WORK,
     +            LWK,NCT,IFAIL)
*
      WRITE (NOUT,*) 'Example 1'
      WRITE (NOUT,*)
      WRITE (NOUT,99997) H
      WRITE (NOUT,*)
      WRITE (NOUT,*) '     T        Approximate'
      WRITE (NOUT,*) '                Solution '
      WRITE (NOUT,*)
*
      ERRMAX = 0.0D0
      DO 20 I = 1, NMESH
         HI1 = DBLE(I-1)*H
         ERR = ABS(YN(I)-SQRT(HI1))
         IF (ERR.GT.ERRMAX) THEN
            ERRMAX = ERR
            T = HI1
            SOLN = YN(I)
         END IF
         IF (MOD(I,5).EQ.1) WRITE (NOUT,99998) HI1, YN(I)
   20 CONTINUE
      WRITE (NOUT,*)
      WRITE (NOUT,99999) ERRMAX, T, SOLN
*
*
      TLIM = 5.0D0
      H = TLIM/(NMESH-1)
*
      CALL D05BDF(CK2,CF2,CG2,'Subsequent',IORDER,TLIM,TOLNL,NMESH,YN,
     +            WORK,LWK,NCT,IFAIL)
*
      WRITE (NOUT,*) 'Example 2'
      WRITE (NOUT,*)
      WRITE (NOUT,99997) H
      WRITE (NOUT,*)
      WRITE (NOUT,*) '     T        Approximate'
      WRITE (NOUT,*) '                Solution '
      WRITE (NOUT,*)
*
      ERRMAX = 0.0D0
      DO 40 I = 1, NMESH
         HI1 = DBLE(I-1)*H
         ERR = ABS(YN(I)-(1.0D0-HI1)*SQRT(HI1))
         IF (ERR.GT.ERRMAX) THEN
            ERRMAX = ERR
            T = HI1
            SOLN = YN(I)
         END IF
         IF (MOD(I,7).EQ.1) WRITE (NOUT,99998) HI1, YN(I)
   40 CONTINUE
      WRITE (NOUT,*)
      WRITE (NOUT,99999) ERRMAX, T, SOLN
*
      STOP
*
99999 FORMAT (' The maximum absolute error, ',E10.2,', occurred at T =',
     +       F8.4,/' with solution ',F8.4,/)
99998 FORMAT (1X,F8.4,F15.4)
99997 FORMAT (' The stepsize h = ',F8.4)
      END
*
*
      DOUBLE PRECISION FUNCTION CK1(T)
*     .. Scalar Arguments ..
      DOUBLE PRECISION              T
*     .. Local Scalars ..
      DOUBLE PRECISION              PI
*     .. External Functions ..
      DOUBLE PRECISION              X01AAF
      EXTERNAL                      X01AAF
*     .. Intrinsic Functions ..
      INTRINSIC                     SQRT
*     .. Executable Statements ..
      CK1 = -SQRT(X01AAF(PI))
      RETURN
      END
*
*
      DOUBLE PRECISION FUNCTION CF1(T)
*     .. Scalar Arguments ..
      DOUBLE PRECISION              T
*     .. Local Scalars ..
      DOUBLE PRECISION              PI
*     .. External Functions ..
      DOUBLE PRECISION              X01AAF
      EXTERNAL                      X01AAF
*     .. Intrinsic Functions ..
      INTRINSIC                     SQRT
*     .. Executable Statements ..
      CF1 = SQRT(T) + (3.0D0/8.0D0)*T*T*X01AAF(PI)
      RETURN
      END
*
*
      DOUBLE PRECISION FUNCTION CG1(S,Y)
*     .. Scalar Arguments ..
      DOUBLE PRECISION              S, Y
*     .. Executable Statements ..
      CG1 = Y*Y*Y
      RETURN
      END
*
*
      DOUBLE PRECISION FUNCTION CK2(T)
*     .. Scalar Arguments ..
      DOUBLE PRECISION              T
*     .. Local Scalars ..
      DOUBLE PRECISION              PI
*     .. External Functions ..
      DOUBLE PRECISION              X01AAF
      EXTERNAL                      X01AAF
*     .. Intrinsic Functions ..
      INTRINSIC                     SQRT
*     .. Executable Statements ..
      CK2 = -SQRT(X01AAF(PI))
      RETURN
      END
*
*
      DOUBLE PRECISION FUNCTION CF2(T)
*     .. Scalar Arguments ..
      DOUBLE PRECISION              T
*     .. Intrinsic Functions ..
      INTRINSIC                     SQRT
*     .. Executable Statements ..
*
      CF2 = (3.0D0-T)*SQRT(T)
      RETURN
      END
*
*
      DOUBLE PRECISION FUNCTION CG2(S,Y)
*     .. Scalar Arguments ..
      DOUBLE PRECISION              S, Y
*     .. Intrinsic Functions ..
      INTRINSIC                     EXP
*     .. Executable Statements ..
      CG2 = EXP(S*(1.0D0-S)*(1.0D0-S)-Y*Y)
      RETURN
      END
