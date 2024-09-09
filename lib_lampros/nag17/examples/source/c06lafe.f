*     C06LAF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NMAX, MXTERM
      PARAMETER        (NMAX=20,MXTERM=200)
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION AHIGH, ALOW, ALPHAB, RELERR, TFAC
      INTEGER          I, IFAIL, N, NA, NFEVAL, NTERMS
*     .. Local Arrays ..
      DOUBLE PRECISION ERREST(NMAX), T(NMAX), TRUREL(NMAX),
     +                 TRURES(NMAX), VALINV(NMAX), WORK(4*MXTERM+2)
*     .. External Subroutines ..
      EXTERNAL         C06LAF, FUN
*     .. Intrinsic Functions ..
      INTRINSIC        ABS, EXP, DBLE
*     .. Executable Statements ..
      WRITE (NOUT,*) 'C06LAF Example Program Results'
      WRITE (NOUT,*)
      WRITE (NOUT,*) '(results may be machine-dependent)'
      ALPHAB = -0.5D0
      T(1) = 1.0D0
*
*     Test for values of a close to ALPHAB.
*
      RELERR = 0.01D0
      TFAC = 7.5D0
      WRITE (NOUT,*)
      WRITE (NOUT,99997) 'Test with T(1) =', T(1)
      WRITE (NOUT,*)
      WRITE (NOUT,99999) '  MXTERM =', MXTERM, '  TFAC =', TFAC,
     +  '  ALPHAB =', ALPHAB, '  RELERR =', RELERR
      IFAIL = -1
*
      CALL C06LAF(FUN,1,T,VALINV,ERREST,RELERR,ALPHAB,TFAC,MXTERM,
     +            NTERMS,NA,ALOW,AHIGH,NFEVAL,WORK,IFAIL)
*
      IF (IFAIL.GT.0 .AND. IFAIL.LT.5) GO TO 60
      WRITE (NOUT,*)
      WRITE (NOUT,*) '   T        Result        exp(-T/2)   ',
     +  'Relative error  Error estimate'
      TRURES(1) = EXP(-T(1)/2.0D0)
      TRUREL(1) = ABS((VALINV(1)-TRURES(1))/TRURES(1))
      WRITE (NOUT,99998) T(1), VALINV(1), TRURES(1), TRUREL(1),
     +  ERREST(1)
      WRITE (NOUT,*)
      WRITE (NOUT,99996) ' NTERMS =', NTERMS, '  NFEVAL =', NFEVAL,
     +  '  ALOW =', ALOW, '  AHIGH =', AHIGH, '  IFAIL =', IFAIL
*
*     Test for larger values of a.
*
      RELERR = 1.0D-3
      TFAC = 0.8D0
      WRITE (NOUT,*)
      WRITE (NOUT,99997) 'Test with T(1) =', T(1)
      WRITE (NOUT,*)
      WRITE (NOUT,99999) '  MXTERM =', MXTERM, '  TFAC =', TFAC,
     +  '  ALPHAB =', ALPHAB, '  RELERR =', RELERR
      IFAIL = -1
*
      CALL C06LAF(FUN,1,T,VALINV,ERREST,RELERR,ALPHAB,TFAC,MXTERM,
     +            NTERMS,NA,ALOW,AHIGH,NFEVAL,WORK,IFAIL)
*
      IF (IFAIL.GT.0 .AND. IFAIL.LT.5) GO TO 60
      WRITE (NOUT,*)
      WRITE (NOUT,*) '   T        Result        exp(-T/2)   ',
     +  'Relative error  Error estimate'
      TRURES(1) = EXP(-T(1)/2.0D0)
      TRUREL(1) = ABS((VALINV(1)-TRURES(1))/TRURES(1))
      WRITE (NOUT,99998) T(1), VALINV(1), TRURES(1), TRUREL(1),
     +  ERREST(1)
      WRITE (NOUT,*)
      WRITE (NOUT,99996) ' NTERMS =', NTERMS, '  NFEVAL =', NFEVAL,
     +  '  ALOW =', ALOW, '  AHIGH =', AHIGH, '  IFAIL =', IFAIL
*
      WRITE (NOUT,*)
      WRITE (NOUT,*) 'Compute inverse'
      WRITE (NOUT,*)
      WRITE (NOUT,99999) '  MXTERM =', MXTERM, '  TFAC =', TFAC,
     +  '  ALPHAB =', ALPHAB, '  RELERR =', RELERR
      WRITE (NOUT,*)
      WRITE (NOUT,*) '   T        Result        exp(-T/2)   ',
     +  'Relative error  Error estimate'
      N = 5
      DO 20 I = 1, N
         T(I) = DBLE(I)
   20 CONTINUE
      IFAIL = -1
*
      CALL C06LAF(FUN,N,T,VALINV,ERREST,RELERR,ALPHAB,TFAC,MXTERM,
     +            NTERMS,NA,ALOW,AHIGH,NFEVAL,WORK,IFAIL)
*
      IF (IFAIL.GT.0 .AND. IFAIL.LT.5) GO TO 60
      DO 40 I = 1, N
         TRURES(I) = EXP(-T(I)/2.0D0)
         TRUREL(I) = ABS((VALINV(I)-TRURES(I))/TRURES(I))
   40 CONTINUE
      WRITE (NOUT,99998) (T(I),VALINV(I),TRURES(I),TRUREL(I),ERREST(I),
     +  I=1,N)
   60 WRITE (NOUT,*)
      WRITE (NOUT,99996) ' NTERMS =', NTERMS, '  NFEVAL =', NFEVAL,
     +  '  ALOW =', ALOW, '  AHIGH =', AHIGH, '  IFAIL =', IFAIL
*
99999 FORMAT (1X,A,I4,A,F6.2,A,F6.2,A,1P,D8.1)
99998 FORMAT (1X,F4.1,7X,F6.3,9X,F6.3,8X,D8.1,8X,D8.1)
99997 FORMAT (1X,A,F4.1)
99996 FORMAT (1X,A,I4,A,I4,A,F7.2,A,F7.2,A,I2)
      END
*
      SUBROUTINE FUN(PR,PI,FR,FI)
*     Function to be inverted
*     .. Scalar Arguments ..
      DOUBLE PRECISION FI, FR, PI, PR
*     .. External Subroutines ..
      EXTERNAL       A02ACF
*     .. Executable Statements ..
      CALL A02ACF(1.0D0,0.0D0,PR+0.5D0,PI,FR,FI)
*
      RETURN
      END
