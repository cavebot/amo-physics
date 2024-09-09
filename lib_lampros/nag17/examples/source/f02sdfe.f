*     F02SDF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NMAX, MAMAX, MBMAX, IA, IB, LWORK
      PARAMETER        (NMAX=10,MAMAX=5,MBMAX=5,IA=2*MAMAX+1,
     +                 IB=2*MBMAX+1,LWORK=NMAX*(MAMAX+2))
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION RMU
      INTEGER          I, IFAIL, J, K, K1, K2, MA, MB, N
*     .. Local Arrays ..
      DOUBLE PRECISION A(IA,NMAX), B(IB,NMAX), D(30), WORK(LWORK),
     +                 X(NMAX)
      INTEGER          IWORK(NMAX)
*     .. External Subroutines ..
      EXTERNAL         F02SDF
*     .. Intrinsic Functions ..
      INTRINSIC        MIN
*     .. Executable Statements ..
      WRITE (NOUT,*) 'F02SDF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N, MA, MB
      IF (N.GT.0 .AND. N.LE.NMAX .AND. MA.GE.0 .AND. MA.LE.MAMAX .AND.
     +    MB.LE.MBMAX) THEN
         DO 20 I = 1, N
            K1 = MA + 1 - MIN(MA,I-1)
            K2 = MA + 1 + MIN(MA,N-I)
            READ (NIN,*) (A(K,I),K=K1,K2)
   20    CONTINUE
         DO 40 I = 1, N
            K1 = MB + 1 - MIN(MB,I-1)
            K2 = MB + 1 + MIN(MB,N-I)
            READ (NIN,*) (B(K,I),K=K1,K2)
   40    CONTINUE
         READ (NIN,*) RMU, D(1)
         IFAIL = 1
*
         CALL F02SDF(N,MA+1,MB+1,A,IA,B,IB,.FALSE.,0.0D0,RMU,X,D,IWORK,
     +               WORK,LWORK,IFAIL)
*
         WRITE (NOUT,*)
         IF (IFAIL.NE.0) THEN
            WRITE (NOUT,99999) 'Error in F02SDF. IFAIL =', IFAIL
            IF (IFAIL.EQ.7 .OR. IFAIL.EQ.9) THEN
               WRITE (NOUT,*)
               WRITE (NOUT,*) 'Successive corrections to RMU were'
               WRITE (NOUT,*)
               DO 60 J = 1, 29
                  IF (D(J).EQ.0.0D0) STOP
                  WRITE (NOUT,99996) D(J)
   60          CONTINUE
            END IF
         ELSE
            WRITE (NOUT,99998) 'Corrected eigenvalue = ', RMU + D(30)
            WRITE (NOUT,*)
            WRITE (NOUT,*) 'Eigenvector is'
            WRITE (NOUT,99997) (X(I),I=1,N)
         END IF
      END IF
      STOP
*
99999 FORMAT (1X,A,I2)
99998 FORMAT (1X,A,F8.4)
99997 FORMAT (1X,5F9.4)
99996 FORMAT (1X,D20.4)
      END
