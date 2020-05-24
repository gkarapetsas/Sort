# include "../SortSub.f90"

Program Test_Sort

Use Sort_Subroutines

Implicit none 

Integer, Dimension(10) :: A, A_sorted

Real(8), Dimension(10) :: B, B_sorted

Complex(8), Dimension(10) :: C, C_sorted

Integer, Dimension(:), Allocatable:: Position

Integer :: N

Real(8) :: epsilon

A = (/ 1, 3, 4, 0, 7, 1, -2, 8, 99, 3 /)

B = (/ 1.5, 3.2, 4.5, 0.9, -7.2, 1.0, 2., 8., 0.999, 4.1 /)

C = (/ (1.5,1.0), (3.2,0.5), -(4.5,0.), (0.9,3.), (7.2,1.2), (1.0,0.1), (2.,0.3), (8.,12.), (0.999,0.1), (4.1,4.) /)

A_sorted = A
Call Sort (A_sorted,1,Size(A),Position)
print*, A_sorted
print*, Position
print*

A_sorted = A
Call Unique_Sort (A_sorted,N)
print*, N
print*, A_sorted
print*

B_sorted = B
Call Sort (B_sorted,1,Size(B),Position)
print*, B_sorted
print*, Position
print*

B_sorted = B
epsilon = 0.002
Call Unique_Sort (B_sorted,N,epsilon)
print*, N
print*, B_sorted
print*

C_sorted = C
Call Sort (C_sorted,1,Size(C),Position)
print*, C_sorted
print*, Position

End Program Test_Sort
