*     G10CAF Example Program Text
*     Mark 16 Release. NAG Copyright 1992.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          NMAX
      PARAMETER        (NMAX=100)
*     .. Local Scalars ..
      INTEGER          I, IFAIL, ITYPE, N
*     .. Local Arrays ..
      DOUBLE PRECISION ROUGH(NMAX), ROUGH1(NMAX), SMOOT1(NMAX),
     +                 SMOOTH(NMAX), Y(NMAX)
*     .. External Subroutines ..
      EXTERNAL         G10CAF
*     .. Executable Statements ..
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N
      IF (N.GT.0 .AND. N.LE.NMAX) THEN
         READ (NIN,*) (Y(I),I=1,N)
*
         ITYPE = 1
         IFAIL = 0
         CALL G10CAF(ITYPE,N,Y,SMOOTH,ROUGH,IFAIL)
         ITYPE = 0
         IFAIL = 0
         CALL G10CAF(ITYPE,N,Y,SMOOT1,ROUGH1,IFAIL)
*
         WRITE (NOUT,*) ' G10CAF Example Program Results'
         WRITE (NOUT,*)
         WRITE (NOUT,99999)
         WRITE (NOUT,99998)
         DO 20 I = 1, N
            WRITE (NOUT,99997) I, Y(I), SMOOTH(I), ROUGH(I),
     +        SMOOT1(I), ROUGH1(I)
   20    CONTINUE
      ELSE
         WRITE (NOUT,*) ' N is out of range'
      END IF
      STOP
*
99999 FORMAT ('                      Using 3RSSH,twice         Using 4',
     +       '253H,twice')
99998 FORMAT (' Index     Data      Smooth        Rough       Smooth  ',
     +       '      Rough')
99997 FORMAT (1X,I4,F11.1,4F13.4)
      END
