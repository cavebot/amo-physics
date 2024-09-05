*     F02BJF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NMAX, IA, IB, IZ
      PARAMETER        (NMAX=8,IA=NMAX,IB=NMAX,IZ=NMAX)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION EPS1
      INTEGER          I, IFAIL, IP, J, K, N
      LOGICAL          MATZ
*     .. Local Arrays ..
      DOUBLE PRECISION A(IA,NMAX), ALFI(NMAX), ALFR(NMAX), B(IB,NMAX),
     +                 BETA(NMAX), Z(IZ,NMAX)
      INTEGER          ITER(NMAX)
*     .. External Functions ..
      DOUBLE PRECISION X02AJF
      EXTERNAL         X02AJF
*     .. External Subroutines ..
      EXTERNAL         F02BJF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'F02BJF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N
      IF (N.GT.0 .AND. N.LE.NMAX) THEN
         READ (NIN,*) ((A(I,J),J=1,N),I=1,N)
         READ (NIN,*) ((B(I,J),J=1,N),I=1,N)
         MATZ = .TRUE.
         EPS1 = X02AJF()
         IFAIL = 1
*
         CALL F02BJF(N,A,IA,B,IB,EPS1,ALFR,ALFI,BETA,MATZ,Z,IZ,ITER,
     +               IFAIL)
*
         IF (IFAIL.NE.0) THEN
            WRITE (NOUT,*)
            WRITE (NOUT,99999) 'Error in F02BJF. IFAIL =', IFAIL
         ELSE
            IP = 0
            DO 40 I = 1, N
               WRITE (NOUT,*)
               WRITE (NOUT,99999) 'Eigensolution', I
               WRITE (NOUT,*)
               WRITE (NOUT,99998) 'ALFR(', I, ')', ALFR(I), '   ALFI(',
     +           I, ')', ALFI(I), '  BETA(', I, ')', BETA(I)
               IF (BETA(I).EQ.0.0D0) THEN
                  WRITE (NOUT,*) 'LAMBDA is infinite'
               ELSE
                  IF (ALFI(I).EQ.0.0D0) THEN
                     WRITE (NOUT,*)
                     WRITE (NOUT,99997) 'LAMBDA    ', ALFR(I)/BETA(I)
                     WRITE (NOUT,*)
                     WRITE (NOUT,*) 'Eigenvector'
                     WRITE (NOUT,99996) (Z(J,I),J=1,N)
                  ELSE
                     WRITE (NOUT,*)
                     WRITE (NOUT,99997) 'LAMBDA    ', ALFR(I)/BETA(I),
     +                 ALFI(I)/BETA(I)
                     WRITE (NOUT,*)
                     WRITE (NOUT,*) 'Eigenvector'
                     K = (-1)**(IP+2)
                     DO 20 J = 1, N
                        WRITE (NOUT,99995) Z(J,I-IP), K*Z(J,I-IP+1)
   20                CONTINUE
                     IP = 1 - IP
                  END IF
               END IF
   40       CONTINUE
            WRITE (NOUT,*)
            WRITE (NOUT,*) 'Number of iterations (machine-dependent)'
            WRITE (NOUT,99994) (ITER(I),I=1,N)
         END IF
      ELSE
         WRITE (NOUT,*)
         WRITE (NOUT,99999) 'N is out of range: N = ', N
      END IF
      STOP
*
99999 FORMAT (1X,A,I4)
99998 FORMAT (1X,A,I1,A,F7.3,A,I1,A,F7.3,A,I1,A,F7.3)
99997 FORMAT (1X,A,2F7.3)
99996 FORMAT (1X,F7.3)
99995 FORMAT (1X,2F7.3)
99994 FORMAT (1X,8I4)
      END
