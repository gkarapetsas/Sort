  Module Sort_subroutines

   InterFace QuickSort
      Module Procedure QuickSort_Cmplx
      Module Procedure QuickSort_Int
   End InterFace QuickSort


  contains

  Subroutine     Sort (InPut,First,Last,Position)
    Implicit None 
    Complex(8), Dimension(:), Intent(InOut)            :: InPut
    Integer                 , Intent(In)               :: First
    Integer                 , Intent(In)               :: Last

    Integer   , Dimension(:), Allocatable  , Optional  :: Position
    Complex(8), Dimension(:), Allocatable              :: A
    Integer                                            :: Row

    If (First /= 1) Then 
    Write(42,*) 'The Subroutine Need Some Changes '
    Write(42,*) 'Does Not Work for First /= 1'
    Write(42,*) 'Change it/ I have headache right now'
    STOP 
    End If 


    Allocate  (A, Source = InPut)

    Call QuickSort(A, First , Last )


    If (.Not. Present(Position) ) Return

    If (Allocated(Position)) DeAllocate(Position)
        Allocate (Position (Size(InPut))) ; Position = -1


    Do Row = 1, Last
    Position(Row) = MinLoc (dAbs(Dble(A(Row)-InPut)),Dim = 1)
    End Do 
    InPut = A 
    DeAllocate(A)
  End Subroutine Sort



  Recursive Subroutine QuickSort_Cmplx(A, First, Last)
    implicit none
    Complex(8), Dimension(:), Intent(InOut) :: A
    Integer                 , Intent(In)    :: First
    Integer                 , Intent(In)    :: Last


    Complex(8)                              :: X
    Complex(8)                              :: T
    Integer                                 :: i
    Integer                                 :: j

    X = A( ( First + Last ) / 2  )
    I = First
    J = Last

    Do
      Do While ( Dble( A(i) )  < Dble(X)) ; i=i+1
      End Do

      Do While (Dble(X) < Dble( A(j) ) ) ; j=j-1
      end do

      If (i >= j) Exit
    
      T = A(i);  A(i) = A(j);  A(j) = T
      i=i+1
      j=j-1
    End Do

    If (First < i-1 )  Call QuickSort(a, First, i-1 )
    If (j+1   < Last)  Call QuickSort(a, j+1  , Last)
  End Subroutine       QuickSort_Cmplx

  Recursive Subroutine QuickSort_Int(A, First, Last)
    implicit none
    Integer, Dimension(:), Intent(InOut) :: A
    Integer              , Intent(In)    :: First
    Integer              , Intent(In)    :: Last


    Integer                              :: X
    Integer                              :: T
    Integer                              :: i
    Integer                              :: j

    X = A( ( First + Last ) / 2  )
    I = First
    J = Last

    Do
      Do While ( A(i) < X ) ; i=i+1
      End Do

      Do While ( X < A(j) ) ; j=j-1
      end do

      If (i >= j) Exit
    
      T = A(i);  A(i) = A(j);  A(j) = T
      i=i+1
      j=j-1
    End Do

    If (First < i-1 )  Call QuickSort(a, First, i-1 )
    If (j+1   < Last)  Call QuickSort(a, j+1  , Last)
  End Subroutine       QuickSort_Int


  Subroutine      Unique_sort(A,N)

! This subroutine sorts an array and removes duplicates

  Implicit None

  Integer, Intent(Inout), Dimension(:) :: A

  Integer, Intent(Out) :: N

  Integer :: J, K

  Call Quicksort(A,1,Size(A))

  J = 1

  DO K = 1, Size(A) - 1
    IF ( A(K) /= A(K+1) ) THEN
      A(J) = A(K)
      J = J + 1
    ENDIF
  ENDDO

  A(J)    = A( Size(A) )
   
  A(J+1:) = Huge(0)

  N = J ! N gives the number of unique entries

  End Subroutine     Unique_sort

  End Module Sort_subroutines
