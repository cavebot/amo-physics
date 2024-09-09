*     D05BAF Example Program Text
*     Mark 14 Release.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
      INTEGER          LWK, NMESH
      PARAMETER        (LWK=1000,NMESH=6)
*     .. Local Scalars ..
      DOUBLE PRECISION ALIM, H, THRESH, TLIM, TOL
      INTEGER          I, IFAIL, IORDER
      CHARACTER        METHOD
*     .. Local Arrays ..
      DOUBLE PRECISION ERRST(NMESH), WORK(LWK), YN(NMESH)
*     .. External Functions ..
      DOUBLE PRECISION CF, CG, CK, SOL, X02AJF
      EXTERNAL         CF, CG, CK, SOL, X02AJF
*     .. External Subroutines ..
      EXTERNAL         D05BAF
*     .. Intrinsic Functions ..
      INTRINSIC        ABS
*     .. Executable Statements ..
      WRITE (NOUT,*) 'D05BAF Example Program Results'
      METHOD = 'A'
      IORDER = 6
      ALIM = 0.D0
      TLIM = 20.D0
      H = (TLIM-ALIM)/NMESH
      TOL = 1.D-3
      THRESH = X02AJF()
*
      WRITE (NOUT,*)
      WRITE (NOUT,99999) 'Size of workspace =', LWK
      WRITE (NOUT,99998) 'Tolerance         =', TOL
      WRITE (NOUT,*)
      IFAIL = 0
*
      CALL D05BAF(CK,CG,CF,METHOD,IORDER,ALIM,TLIM,YN,ERRST,NMESH,TOL,
     +            THRESH,WORK,LWK,IFAIL)
*
      IF (IFAIL.EQ.0) THEN
         WRITE (NOUT,*)
     + '   T        Approx. Sol.  True Sol.    Est. Error    Actual Erro
     +r'
         WRITE (NOUT,99997) (ALIM+I*H,YN(I),SOL(I*H),ERRST(I),ABS((YN(I)
     +     -SOL(I*H))/SOL(I*H)),I=1,NMESH)
      END IF
      STOP
*
99999 FORMAT (1X,A,I12)
99998 FORMAT (1X,A,D12.4)
99997 FORMAT (F7.2,2F14.5,2D15.5)
      END
*
      DOUBLE PRECISION FUNCTION SOL(T)
*     .. Scalar Arguments ..
      DOUBLE PRECISION              T
*     .. Intrinsic Functions ..
      INTRINSIC                     EXP, LOG
*     .. Executable Statements ..
      SOL = LOG(T+EXP(1.D0))
      RETURN
      END
*
      DOUBLE PRECISION FUNCTION CF(T)
*     .. Scalar Arguments ..
      DOUBLE PRECISION             T
*     .. Intrinsic Functions ..
      INTRINSIC                    EXP
*     .. Executable Statements ..
      CF = EXP(-T)
      RETURN
      END
*
      DOUBLE PRECISION FUNCTION CK(T)
*     .. Scalar Arguments ..
      DOUBLE PRECISION             T
*     .. Intrinsic Functions ..
      INTRINSIC                    EXP
*     .. Executable Statements ..
      CK = EXP(-T)
      RETURN
      END
*
      DOUBLE PRECISION FUNCTION CG(S,Y)
*     .. Scalar Arguments ..
      DOUBLE PRECISION             S, Y
*     .. Intrinsic Functions ..
      INTRINSIC                    EXP
*     .. Executable Statements ..
      CG = Y + EXP(-Y)
      RETURN
      END
