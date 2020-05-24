!>-----------------------------------------------------------------------------------------------------------
!>
!> Authors: Dionisis Pettas   (dspedi@gmail.com)                 Date: 24/05/2020
!>          George Karapetsas (gkarapetsas@gmail.com)
!>
!>-----------------------------------------------------------------------------------------------------------
!>
!> This is a collection of subroutines with sorting algorithms for arrays

  Module Sort_subroutines

   Private

   Public Sort, Unique_Sort

   InterFace Sort
!    This subroutine sorts an array
     Module Procedure Sort_Int
     Module Procedure Sort_Dble
     Module Procedure Sort_Cmplx
   End InterFace Sort

   InterFace Unique_Sort
!    This subroutine sorts an array and removes duplicates
     Module Procedure Unique_Sort_Int
     Module Procedure Unique_Sort_Dble
   End InterFace Unique_Sort


   InterFace QuickSort
     Module Procedure QuickSort_Int
     Module Procedure QuickSort_Dble
     Module Procedure QuickSort_Cmplx
   End InterFace QuickSort

  contains

!========================================================================
!        Procedure Sort
!========================================================================

  Subroutine     Sort_Int (InPut,First,Last,Position)
    Implicit None 
    Integer, Dimension(:), Intent(InOut)            :: InPut
    Integer              , Intent(In)               :: First
    Integer              , Intent(In)               :: Last

    Integer, Dimension(:), Allocatable  , Optional  :: Position
    Integer, Dimension(:), Allocatable              :: A
    Integer                                         :: Row
    Integer                                         :: J


    Allocate  (A, Source = InPut)

    Call QuickSort(A, First , Last )

    If (.Not. Present(Position) ) Then
     InPut = A 
     DeAllocate(A)
     Return
    Endif

    If (Allocated(Position)) DeAllocate(Position)
        Allocate (Position (Size(InPut))) ; Position = -1

    Do Row = 1, Last
      J = MinLoc (abs((A(Row)-InPut)),Dim = 1)
      Position(Row) = J
      Input(J) = Huge(0)
    End Do 
    InPut = A 
    DeAllocate(A)

  End Subroutine Sort_Int

  Subroutine     Sort_dble (InPut,First,Last,Position)
    Implicit None 
    Real(8), Dimension(:), Intent(InOut)            :: InPut
    Integer              , Intent(In)               :: First
    Integer              , Intent(In)               :: Last

    Integer, Dimension(:), Allocatable  , Optional  :: Position
    Real(8), Dimension(:), Allocatable              :: A
    Integer                                         :: Row
    Integer                                         :: J

    Allocate  (A, Source = InPut)

    Call QuickSort(A, First , Last )

    If (.Not. Present(Position) ) Then
     InPut = A 
     DeAllocate(A)
     Return
    Endif

    If (Allocated(Position)) DeAllocate(Position)
        Allocate (Position (Size(InPut))) ; Position = -1

    Do Row = 1, Last
      J = MinLoc (dAbs(A(Row)-InPut),Dim = 1)
      Position(Row) = J
      Input(J) = Huge(0.D0)
    End Do 
    InPut = A 
    DeAllocate(A)

  End Subroutine Sort_dble

  Subroutine     Sort_cmplx (InPut,First,Last,Position)
!   Sorts based on the real part 
    Implicit None 
    Complex(8), Dimension(:), Intent(InOut)            :: InPut
    Integer                 , Intent(In)               :: First
    Integer                 , Intent(In)               :: Last

    Integer   , Dimension(:), Allocatable  , Optional  :: Position
    Complex(8), Dimension(:), Allocatable              :: A
    Integer                                            :: Row
    Integer                                            :: J

    Allocate  (A, Source = InPut)

    Call QuickSort(A, First , Last )

    If (.Not. Present(Position) ) Then
     InPut = A 
     DeAllocate(A)
     Return
    Endif

    If (Allocated(Position)) DeAllocate(Position)
        Allocate (Position (Size(InPut))) ; Position = -1

    Do Row = 1, Last
      J = MinLoc (dAbs(Dble(A(Row)-InPut)),Dim = 1)
      Position(Row) = J
      Input(J) = CMPLX(Huge(0.d0),Huge(0.d0))
    End Do 
    InPut = A 
    DeAllocate(A)

  End Subroutine Sort_cmplx

!========================================================================
!        Procedure Unique_sort
!========================================================================

  Subroutine      Unique_sort_Int(A,N)

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

  End Subroutine     Unique_sort_Int

  Subroutine      Unique_sort_Dble(A,N,epsilon)

! This subroutine sorts an array and removes duplicates

  Implicit None

  Real(8), Intent(Inout), Dimension(:) :: A

  Real(8), Intent(In) :: epsilon

  Integer, Intent(Out) :: N

  Integer :: J, K

  Call Quicksort(A,1,Size(A))

  J = 1

  DO K = 1, Size(A) - 1
    IF ( ABS(A(K) - A(K+1)) > epsilon ) THEN
      A(J) = A(K)
      J = J + 1
    ENDIF
  ENDDO

  A(J)    = A( Size(A) )
   
  A(J+1:) = Huge(0)

  N = J ! N gives the number of unique entries

  End Subroutine     Unique_sort_Dble

!========================================================================
!        Procedure QuickSort
!========================================================================

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

  Recursive Subroutine QuickSort_Dble(A, First, Last)
    implicit none
    Real(8), Dimension(:), Intent(InOut) :: A
    Integer              , Intent(In)    :: First
    Integer              , Intent(In)    :: Last

    Real(8)                              :: X
    Real(8)                              :: T
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
  End Subroutine       QuickSort_Dble

  Recursive Subroutine QuickSort_Cmplx(A, First, Last)
!   Sorts based on the real part
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
      Do While ( Dble( A(i) ) < Dble( X ) ) ; i=i+1
      End Do

      Do While ( Dble( X ) < Dble( A(j) ) ) ; j=j-1
      end do

      If (i >= j) Exit
    
      T = A(i);  A(i) = A(j);  A(j) = T
      i=i+1
      j=j-1
    End Do

    If (First < i-1 )  Call QuickSort(a, First, i-1 )
    If (j+1   < Last)  Call QuickSort(a, j+1  , Last)
  End Subroutine       QuickSort_Cmplx

  End Module Sort_subroutines
