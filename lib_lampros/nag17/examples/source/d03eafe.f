*     D03EAF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          N, M, NP1, IC, NP4, N1, N1P1
      PARAMETER        (N=6,M=2,NP1=N+1,IC=N+1,NP4=N+4,N1=2*(N+M)-2,
     +                 N1P1=N1+1)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION ALPHA, C, P, Q
      INTEGER          I, IFAIL, J, NPTS
      LOGICAL          DORM, EXT, STGONE
*     .. Local Arrays ..
      DOUBLE PRECISION C1(IC,NP4), PHI(N), PHID(N), X(N1P1), Y(N1P1)
      INTEGER          ICINT(NP1)
*     .. External Subroutines ..
      EXTERNAL         D03EAF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'D03EAF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) EXT, DORM
      STGONE = .TRUE.
      WRITE (NOUT,*)
      IF ( .NOT. EXT .AND. .NOT. DORM) THEN
         READ (NIN,*) ALPHA
         WRITE (NOUT,*) 'Interior Neumann problem'
         WRITE (NOUT,*)
         WRITE (NOUT,99999) 'Total integral =', ALPHA
      ELSE
         IF (EXT .AND. .NOT. DORM) THEN
            READ (NIN,*) ALPHA
            WRITE (NOUT,*) 'Exterior Neumann problem'
            WRITE (NOUT,*)
            WRITE (NOUT,99998) 'C=', ALPHA
         END IF
      END IF
      DO 20 I = 1, N1 + 1
         READ (NIN,*) X(I), Y(I)
   20 CONTINUE
      DO 40 I = 1, N
         READ (NIN,*) PHI(I), PHID(I)
   40 CONTINUE
      IFAIL = 1
*
      CALL D03EAF(STGONE,EXT,DORM,N,P,Q,X,Y,N1P1,PHI,PHID,ALPHA,C1,IC,
     +            NP4,ICINT,NP1,IFAIL)
*
      IF (IFAIL.NE.0) THEN
         WRITE (NOUT,*)
         WRITE (NOUT,99996) 'Error in D03EAF IFAIL = ', IFAIL
         WRITE (NOUT,*)
         WRITE (NOUT,99996) 'The value of RANK is ', ICINT(1)
         STOP
      END IF
      C = ALPHA
      IF (EXT .AND. DORM) THEN
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'Exterior problem'
         WRITE (NOUT,*)
         WRITE (NOUT,99999) 'Computed C =', C
      END IF
      J = 2
      WRITE (NOUT,*)
      WRITE (NOUT,*) 'Nodes'
      WRITE (NOUT,*)
     +  '           X              Y              PHI            PHID'
      DO 60 I = 1, N
         IF (X(J).EQ.X(J-1) .AND. Y(J).EQ.Y(J-1)) J = J + 2
         WRITE (NOUT,99997) X(J), Y(J), PHI(I), PHID(I)
         J = J + 2
   60 CONTINUE
      STGONE = .FALSE.
      WRITE (NOUT,*)
      WRITE (NOUT,*) 'Selected points'
      WRITE (NOUT,*) '           X              Y              PHI'
      READ (NIN,*) NPTS
      DO 80 I = 1, NPTS
         READ (NIN,*) P, Q
         ALPHA = C
*
         CALL D03EAF(STGONE,EXT,DORM,N,P,Q,X,Y,N1P1,PHI,PHID,ALPHA,C1,
     +               IC,NP4,ICINT,NP1,IFAIL)
*
         WRITE (NOUT,99997) P, Q, ALPHA
   80 CONTINUE
      STOP
*
99999 FORMAT (1X,A,F15.2)
99998 FORMAT (1X,A,D15.4)
99997 FORMAT (1X,4F15.2)
99996 FORMAT (1X,A,I2)
      END
