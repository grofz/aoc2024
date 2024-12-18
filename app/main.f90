program main
  use day2401_mod, only: day2401
  use day2402_mod, only: day2402
  use day2403_mod, only: day2403
  use day2404_mod, only: day2404
  use day2405_mod, only: day2405
  use day2406_mod, only: day2406
  use day2407_mod, only: day2407
  use day2408_mod, only: day2408
  use day2409_mod, only: day2409
  use day2410_mod, only: day2410
  use day2411_mod, only: day2411
  use day2412_mod, only: day2412
  use day2413_mod, only: day2413
  use day2414_mod, only: day2414
  use day2415_mod, only: day2415
  use day2416_mod, only: day2416
  use day2417_mod, only: day2417
  use day2418_mod, only: day2418
  implicit none

! goto 100

  print '("Advent of Code 2024 (www.adventofcode.com)")'
  call day2401('inp/01/input.txt')
  call day2402('inp/02/input.txt')
  call day2403('inp/03/input.txt')
  call day2404('inp/04/input.txt')
  call day2405('inp/05/input.txt')
  call day2406('inp/06/input.txt')
 !call day2407('inp/07/input.txt', 1) ! un-optimized
 !call day2407('inp/07/input.txt', 2) ! optimization
  call day2407('inp/07/input.txt', 3) ! better optimization
  call day2408('inp/08/input.txt')
  call day2409('inp/09/input.txt')
  call day2410('inp/10/input.txt')
  call day2411('inp/11/input.txt')
  call day2412('inp/12/input.txt')
  call day2413('inp/13/input.txt')
  call day2414('inp/14/input.txt')
  call day2415('inp/15/input.txt')
  call day2416('inp/16/input.txt')
  call day2417('inp/17/input.txt')
  call day2418('inp/18/input.txt')
  100 continue
end program main

! Notes
! Day 4
! https://rcoh.me/posts/linear-time-median-finding/
