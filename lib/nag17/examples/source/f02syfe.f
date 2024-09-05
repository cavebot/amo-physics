*     F02SYF Example Program Text
*     Mark 14 Release.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          NMAX, LDQ, LDPT
      PARAMETER        (NMAX=10,LDQ=NMAX,LDPT=NMAX)
*     .. Local Scalars ..
      INTEGER          I, IFAIL, J, N, NCOLB
*     .. Local Arrays ..
      DOUBLE PRECISION D(NMAX), DUMMY(1), E(NMAX), PT(LDPT,NMAX),
     +                 Q(LDQ,NMAX), WORK(4*NMAX)
*     .. External Subroutines ..
      EXTERNAL         F02SYF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'F02SYF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N
      WRITE (NOUT,*)
      IF (N.GT.NMAX) THEN
         WRITE (NOUT,*) 'N is out of range.'
         WRITE (NOUT,99999) 'N = ', N
      ELSE
         READ (NIN,*) (D(I),I=1,N)
         IF (N.GT.1) READ (NIN,*) (E(I),I=1,N-1)
*        Set Q and PT to the unit matrix.
         DO 40 J = 1, N
            DO 20 I = 1, N
               Q(I,J) = 0.0D0
               PT(I,J) = 0.0D0
   20       CONTINUE
            Q(J,J) = 1.0D0
            PT(J,J) = 1.0D0
   40    CONTINUE
         NCOLB = 0
         IFAIL = 0
*
*        Find the singular value decomposition of A
         CALL F02SYF(N,D,E,NCOLB,DUMMY,1,N,Q,LDQ,N,PT,LDPT,WORK,IFAIL)
*
         WRITE (NOUT,*) 'Singular value decomposition of A'
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'Singular values'
         WRITE (NOUT,99998) (D(I),I=1,N)
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'Left-hand singular vectors, by column'
         DO 60 I = 1, N
            WRITE (NOUT,99998) (Q(I,J),J=1,N)
   60    CONTINUE
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'Right-hand singular vectors, by column'
         DO 80 I = 1, N
            WRITE (NOUT,99998) (PT(J,I),J=1,N)
   80    CONTINUE
      END IF
      STOP
*
99999 FORMAT (1X,A,I5)
99998 FORMAT (3(1X,F8.4))
      END
