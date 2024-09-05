*     F01BLF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          MMAX, NMAX, IA, IU
      PARAMETER        (MMAX=6,NMAX=MMAX,IA=MMAX,IU=NMAX)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION CXIX, T
      INTEGER          I, IFAIL, IRANK, J, M, N
*     .. Local Arrays ..
      DOUBLE PRECISION A(IA,NMAX), AIJMAX(NMAX), D(MMAX), DU(NMAX),
     +                 U(IU,NMAX)
      INTEGER          INC(NMAX)
*     .. External Functions ..
      DOUBLE PRECISION X02AJF
      EXTERNAL         X02AJF
*     .. External Subroutines ..
      EXTERNAL         F01BLF
*     .. Intrinsic Functions ..
      INTRINSIC        MIN, SQRT
*     .. Executable Statements ..
      WRITE (NOUT,*) 'F01BLF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) M, N
      WRITE (NOUT,*)
      IF (M.GT.0 .AND. M.LE.MMAX .AND. N.GT.0 .AND. N.LE.M) THEN
         READ (NIN,*) ((A(I,J),J=1,N),I=1,M)
*        Set T to N times norm of A.
         CXIX = 0.0D0
         DO 40 I = 1, M
            DO 20 J = 1, N
               CXIX = CXIX + A(I,J)**2
   20       CONTINUE
   40    CONTINUE
         T = SQRT(CXIX)*X02AJF()
         IFAIL = 1
*
         CALL F01BLF(M,N,T,A,IA,AIJMAX,IRANK,INC,D,U,IU,DU,IFAIL)
*
         IF (IFAIL.NE.0) THEN
            WRITE (NOUT,99998) 'Error in F01BLF. IFAIL =', IFAIL
         ELSE
            WRITE (NOUT,*)
     +        'Maximum element in A(K) for I.GE.K and J.GE.K'
            WRITE (NOUT,*)
            WRITE (NOUT,*) '   K    Modulus'
            WRITE (NOUT,99997) (I,AIJMAX(I),I=1,MIN(N,IRANK+1))
            WRITE (NOUT,*)
            WRITE (NOUT,99998) 'Rank = ', IRANK
            WRITE (NOUT,*)
            WRITE (NOUT,99995) 'T = ', T, '  (machine dependent)'
            WRITE (NOUT,*)
            WRITE (NOUT,*) 'Transpose of pseudo-inverse'
            DO 60 I = 1, M
               WRITE (NOUT,99996) (A(I,J),J=1,N)
   60       CONTINUE
         END IF
      ELSE
         WRITE (NOUT,99999) 'M or N out of range: M = ', M, '   N = ', N
      END IF
      STOP
*
99999 FORMAT (1X,A,I5,A,I5)
99998 FORMAT (1X,A,I2)
99997 FORMAT (1X,I4,2X,1P,D12.4)
99996 FORMAT (1X,1P,6D12.4)
99995 FORMAT (1X,A,1P,D11.4,A)
      END
