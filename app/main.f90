program main
  use day2401_mod, only: day2401
  use day2402_mod, only: day2402
  use day2403_mod, only: day2403
  implicit none

!goto 100

  print '("Advent of Code 2024 (www.adventofcode.com)")'
  call day2401('inp/01/input.txt')
  call day2402('inp/02/input.txt')
  call day2403('inp/03/input.txt')
end program main
