      SUBROUTINE F06QXF( SIDE, PIVOT, DIRECT, M, N, K1, K2, C, S, A,
     $                   LDA )
C     MARK 13 RELEASE. NAG COPYRIGHT 1988.
C     .. Scalar Arguments ..
      INTEGER            K1, K2, LDA, M, N
      CHARACTER*1        DIRECT, PIVOT, SIDE
C     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), C( * ), S( * )
C     ..
C
C  F06QXF  performs the transformation
C
C     A := P*A,   when   SIDE = 'L' or 'l'  (  Left-hand side )
C
C     A := A*P',  when   SIDE = 'R' or 'r'  ( Right-hand side )
C
C  where A is an m by n matrix and P is an orthogonal matrix, consisting
C  of a  sequence  of  plane  rotations,  applied  in  planes  k1 to k2,
C  determined by the parameters PIVOT and DIRECT as follows:
C
C     When  PIVOT  = 'V' or 'v'  ( Variable pivot )
C     and   DIRECT = 'F' or 'f'  ( Forward sequence ) then
C
C        P is given as a sequence of plane rotation matrices
C
C           P = P( k2 - 1 )*...*P( k1 + 1 )*P( k1 ),
C
C        where  P( k )  is a plane rotation matrix for the  ( k, k + 1 )
C        plane.
C
C     When  PIVOT  = 'V' or 'v'  ( Variable pivot )
C     and   DIRECT = 'B' or 'b'  ( Backward sequence ) then
C
C        P is given as a sequence of plane rotation matrices
C
C           P = P( k1 )*P( k1 + 1 )*...*P( k2 - 1 ),
C
C        where  P( k )  is a plane rotation matrix for the  ( k, k + 1 )
C        plane.
C
C     When  PIVOT  = 'T' or 't'  ( Top pivot )
C     and   DIRECT = 'F' or 'f'  ( Forward sequence ) then
C
C        P is given as a sequence of plane rotation matrices
C
C           P = P( k2 - 1 )*P( k2 - 2 )*...*P( k1 ),
C
C        where  P( k )  is a plane rotation matrix for the ( k1, k + 1 )
C        plane.
C
C     When  PIVOT  = 'T' or 't'  ( Top pivot )
C     and   DIRECT = 'B' or 'b'  ( Backward sequence ) then
C
C        P is given as a sequence of plane rotation matrices
C
C           P = P( k1 )*P( k1 + 1 )*...*P( k2 - 1 ),
C
C        where  P( k )  is a plane rotation matrix for the ( k1, k + 1 )
C        plane.
C
C     When  PIVOT  = 'B' or 'b'  ( Bottom pivot )
C     and   DIRECT = 'F' or 'f'  ( Forward sequence ) then
C
C        P is given as a sequence of plane rotation matrices
C
C           P = P( k2 - 1 )*P( k2 - 2 )*...*P( k1 ),
C
C        where  P( k )  is a  plane rotation  matrix  for the  ( k, k2 )
C        plane.
C
C     When  PIVOT  = 'B' or 'b'  ( Bottom pivot )
C     and   DIRECT = 'B' or 'b'  ( Backward sequence ) then
C
C        P is given as a sequence of plane rotation matrices
C
C           P = P( k1 )*P( k1 + 1 )*...*P( k2 - 1 ),
C
C        where  P( k )  is a  plane rotation  matrix  for the  ( k, k2 )
C        plane.
C
C  c( k ) and s( k )  must contain the  cosine and sine  that define the
C  matrix  P( k ).  The  two by two  plane rotation  part of the  matrix
C  P( k ), R( k ), is assumed to be of the form
C
C     R( k ) = (  c( k )  s( k ) ).
C              ( -s( k )  c( k ) )
C
C  If m, n or k1 are less than unity,  or k2 is not greater than k1,  or
C  SIDE = 'L' or 'l'  and  k2  is greater than  m, or  SIDE = 'R' or 'r'
C  and  k2  is greater than  n,  then an  immediate return  is effected.
C
C
C  Nag Fortran 77 O( n**2 ) basic linear algebra routine.
C
C  -- Written on 20-November-1986.
C     Sven Hammarling and Mick Pont, Nag Central Office.
C
C     Alternative code used in this version to call the Level 1 BLAS
C     routine DROT.
C
C     .. Parameters ..
      DOUBLE PRECISION   ONE, ZERO
      PARAMETER          ( ONE = 1.0D+0, ZERO = 0.0D+0 )
C     .. Local Scalars ..
      INTEGER            J
      LOGICAL            LEFT, RIGHT
C     .. Intrinsic Functions ..
      INTRINSIC          MIN
C     .. External Subroutines ..
      EXTERNAL           DROT
C     ..
C     .. Executable Statements ..
      LEFT = ( SIDE.EQ.'L' ).OR.( SIDE.EQ.'l' )
      RIGHT = ( SIDE.EQ.'R' ).OR.( SIDE.EQ.'r' )
      IF( ( MIN( M, N, K1 ).LT.1 ).OR.( K2.LE.K1 ).OR.
     $    ( ( LEFT ).AND.( K2.GT.M ) ).OR.
     $    ( ( RIGHT ).AND.( K2.GT.N ) ) )RETURN
      IF( LEFT )THEN
         IF( ( PIVOT.EQ.'V' ).OR.( PIVOT.EQ.'v' ) )THEN
            IF( ( DIRECT.EQ.'F' ).OR.( DIRECT.EQ.'f' ) )THEN
               DO 10 J = K1, K2 - 1
                  IF( ( C( J ).NE.ONE ).OR.( S( J ).NE.ZERO ) )THEN
                     CALL DROT( N, A( J, 1 ), LDA, A( J + 1, 1 ), LDA,
     $                          C( J ), S( J ) )
                  END IF
   10          CONTINUE
            ELSE IF( ( DIRECT.EQ.'B' ).OR.( DIRECT.EQ.'b' ) )THEN
               DO 20 J = K2 - 1, K1, -1
                  IF( ( C( J ).NE.ONE ).OR.( S( J ).NE.ZERO ) )THEN
                     CALL DROT( N, A( J, 1 ), LDA, A( J + 1, 1 ), LDA,
     $                          C( J ), S( J ) )
                  END IF
   20          CONTINUE
            END IF
         ELSE IF( ( PIVOT.EQ.'T' ).OR.( PIVOT.EQ.'t' ) )THEN
            IF( ( DIRECT.EQ.'F' ).OR.( DIRECT.EQ.'f' ) )THEN
               DO 30 J = K1 + 1, K2
                  IF( ( C( J - 1 ).NE.ONE ).OR.
     $                ( S( J - 1 ).NE.ZERO ) )THEN
                     CALL DROT( N, A( K1, 1 ), LDA, A( J, 1 ), LDA,
     $                          C( J - 1 ), S( J - 1 ) )
                  END IF
   30          CONTINUE
            ELSE IF( ( DIRECT.EQ.'B' ).OR.( DIRECT.EQ.'b' ) )THEN
               DO 40 J = K2, K1 + 1, -1
                  IF( ( C( J - 1 ).NE.ONE ).OR.
     $                ( S( J - 1 ).NE.ZERO ) )THEN
                     CALL DROT( N, A( K1, 1 ), LDA, A( J, 1 ), LDA,
     $                          C( J - 1 ), S( J - 1 ) )
                  END IF
   40          CONTINUE
            END IF
         ELSE IF( ( PIVOT.EQ.'B' ).OR.( PIVOT.EQ.'b' ) )THEN
            IF( ( DIRECT.EQ.'F' ).OR.( DIRECT.EQ.'f' ) )THEN
               DO 50 J = K1, K2 - 1
                  IF( ( C( J ).NE.ONE ).OR.( S( J ).NE.ZERO ) )THEN
                     CALL DROT( N, A( J, 1 ), LDA, A( K2, 1 ), LDA,
     $                          C( J ), S( J ) )
                  END IF
   50          CONTINUE
            ELSE IF( ( DIRECT.EQ.'B' ).OR.( DIRECT.EQ.'b' ) )THEN
               DO 60 J = K2 - 1, K1, -1
                  IF( ( C( J ).NE.ONE ).OR.( S( J ).NE.ZERO ) )THEN
                     CALL DROT( N, A( J, 1 ), LDA, A( K2, 1 ), LDA,
     $                          C( J ), S( J ) )
                  END IF
   60          CONTINUE
            END IF
         END IF
      ELSE IF( RIGHT )THEN
         IF( ( PIVOT.EQ.'V' ).OR.( PIVOT.EQ.'v' ) )THEN
            IF( ( DIRECT.EQ.'F' ).OR.( DIRECT.EQ.'f' ) )THEN
               DO 70 J = K1, K2 - 1
                  IF( ( C( J ).NE.ONE ).OR.( S( J ).NE.ZERO ) )THEN
                     CALL DROT( M, A( 1, J ), 1, A( 1, J + 1 ), 1,
     $                          C( J ), S( J ) )
                  END IF
   70          CONTINUE
            ELSE IF( ( DIRECT.EQ.'B' ).OR.( DIRECT.EQ.'b' ) )THEN
               DO 80 J = K2 - 1, K1, -1
                  IF( ( C( J ).NE.ONE ).OR.( S( J ).NE.ZERO ) )THEN
                     CALL DROT( M, A( 1, J ), 1, A( 1, J + 1 ), 1,
     $                          C( J ), S( J ) )
                  END IF
   80          CONTINUE
            END IF
         ELSE IF( ( PIVOT.EQ.'T' ).OR.( PIVOT.EQ.'t' ) )THEN
            IF( ( DIRECT.EQ.'F' ).OR.( DIRECT.EQ.'f' ) )THEN
               DO 90 J = K1 + 1, K2
                  IF( ( C( J - 1 ).NE.ONE ).OR.
     $                ( S( J - 1 ).NE.ZERO ) )THEN
                     CALL DROT( M, A( 1, K1 ), 1, A( 1, J ), 1,
     $                          C( J - 1 ), S( J - 1 ) )
                  END IF
   90          CONTINUE
            ELSE IF( ( DIRECT.EQ.'B' ).OR.( DIRECT.EQ.'b' ) )THEN
               DO 100 J = K2, K1 + 1, -1
                  IF( ( C( J - 1 ).NE.ONE ).OR.
     $                ( S( J - 1 ).NE.ZERO ) )THEN
                     CALL DROT( M, A( 1, K1 ), 1, A( 1, J ), 1,
     $                          C( J - 1 ), S( J - 1 ) )
                  END IF
  100          CONTINUE
            END IF
         ELSE IF( ( PIVOT.EQ.'B' ).OR.( PIVOT.EQ.'b' ) )THEN
            IF( ( DIRECT.EQ.'F' ).OR.( DIRECT.EQ.'f' ) )THEN
               DO 110 J = K1, K2 - 1
                  IF( ( C( J ).NE.ONE ).OR.( S( J ).NE.ZERO ) )THEN
                     CALL DROT( M, A( 1, J ), 1, A( 1, K2 ), 1, C( J ),
     $                          S( J ) )
                  END IF
  110          CONTINUE
            ELSE IF( ( DIRECT.EQ.'B' ).OR.( DIRECT.EQ.'b' ) )THEN
               DO 120 J = K2 - 1, K1, -1
                  IF( ( C( J ).NE.ONE ).OR.( S( J ).NE.ZERO ) )THEN
                     CALL DROT( M, A( 1, J ), 1, A( 1, K2 ), 1, C( J ),
     $                          S( J ) )
                  END IF
  120          CONTINUE
            END IF
         END IF
      END IF
C
      RETURN
C
C     End of F06QXF. ( SGESRC )
C
      END
