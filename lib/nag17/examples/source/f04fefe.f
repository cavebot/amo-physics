*     F04FEF Example Program Text
*     Mark 15 Release. NAG Copyright 1991.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          NMAX
      PARAMETER        (NMAX=100)
*     .. Local Scalars ..
      DOUBLE PRECISION VLAST
      INTEGER          I, IFAIL, N
      LOGICAL          WANTP, WANTV
*     .. Local Arrays ..
      DOUBLE PRECISION P(NMAX), T(0:NMAX), V(NMAX), WORK(NMAX-1),
     +                 X(NMAX)
*     .. External Subroutines ..
      EXTERNAL         F04FEF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'F04FEF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N
      WRITE (NOUT,*)
      IF ((N.LT.0) .OR. (N.GT.NMAX)) THEN
         WRITE (NOUT,99999) 'N is out of range. N = ', N
      ELSE
         READ (NIN,*) (T(I),I=0,N)
         WANTP = .TRUE.
         WANTV = .TRUE.
*
         IFAIL = -1
*
         CALL F04FEF(N,T,X,WANTP,P,WANTV,V,VLAST,WORK,IFAIL)
*
         IF (IFAIL.EQ.0) THEN
            WRITE (NOUT,*)
            WRITE (NOUT,*) 'Solution vector'
            WRITE (NOUT,99998) (X(I),I=1,N)
            IF (WANTP) THEN
               WRITE (NOUT,*)
               WRITE (NOUT,*) 'Reflection coefficients'
               WRITE (NOUT,99998) (P(I),I=1,N)
            END IF
            IF (WANTV) THEN
               WRITE (NOUT,*)
               WRITE (NOUT,*) 'Mean square prediction errors'
               WRITE (NOUT,99998) (V(I),I=1,N)
            END IF
         ELSE IF (IFAIL.GT.0) THEN
            WRITE (NOUT,*)
            WRITE (NOUT,99999) 'Solution for system of order', IFAIL
            WRITE (NOUT,99998) (X(I),I=1,IFAIL)
            IF (WANTP) THEN
               WRITE (NOUT,*)
               WRITE (NOUT,*) 'Reflection coefficients'
               WRITE (NOUT,99998) (P(I),I=1,IFAIL)
            END IF
            IF (WANTV) THEN
               WRITE (NOUT,*)
               WRITE (NOUT,*) 'Mean square prediction errors'
               WRITE (NOUT,99998) (V(I),I=1,IFAIL)
            END IF
         END IF
      END IF
      STOP
*
99999 FORMAT (1X,A,I5)
99998 FORMAT (1X,5F9.4)
      END
