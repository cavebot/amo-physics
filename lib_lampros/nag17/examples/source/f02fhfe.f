*     F02FHF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NMAX, MAMAX, MBMAX, NRA, NRB, LWORK
      PARAMETER        (NMAX=20,MAMAX=5,MBMAX=5,NRA=MAMAX+1,NRB=MBMAX+1,
     +                 LWORK=NMAX+(3*MAMAX+MBMAX)*(MAMAX+MBMAX+2))
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      INTEGER          I, IFAIL, J, MA, MB, N
*     .. Local Arrays ..
      DOUBLE PRECISION A(NRA,NMAX), B(NRB,NMAX), D(NMAX), WORK(LWORK)
*     .. External Subroutines ..
      EXTERNAL         F02FHF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'F02FHF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N, MA, MB
      WRITE (NOUT,*)
      IF (N.LT.1 .OR. N.GT.NMAX .OR. MA.LT.0 .OR. MA.GT.MAMAX .OR.
     +    MB.LT.0 .OR. MB.GT.MBMAX) THEN
         WRITE (NOUT,*) 'N or MA or MB is out of range.'
         WRITE (NOUT,99999) 'N = ', N, '   MA = ', MA, '   MB = ', MB
      ELSE
         DO 20 I = 1, MA + 1
            READ (NIN,*) (A(I,J),J=1,N)
   20    CONTINUE
         DO 40 I = 1, MB + 1
            READ (NIN,*) (B(I,J),J=1,N)
   40    CONTINUE
*
         IFAIL = 1
*
         CALL F02FHF(N,MA,A,NRA,MB,B,NRB,D,WORK,LWORK,IFAIL)
*
         IF (IFAIL.NE.0) THEN
            WRITE (NOUT,*)
            WRITE (NOUT,99999) 'F02FHF fails. IFAIL =', IFAIL
         ELSE
            WRITE (NOUT,*) 'Eigenvalues'
            WRITE (NOUT,99998) (D(J),J=1,N)
         END IF
      END IF
      STOP
*
99999 FORMAT (1X,A,I5,A,I5,A,I5)
99998 FORMAT (1X,7F9.4)
      END
