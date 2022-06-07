program macierze

  !Kacper Jajuga, kierunek: informatyka, rok I, semestr II

  !Przykladowa macierz zostala przeze mnie podana w pliku Nazwa_we.txt i jest ona rozmiaru 3x3
  !Program czyta osiem znakow z pliku i przypisuje je do konkretnego miejsca tab(i,j), dlatego pojedyncza liczba (razem ze spacjami i kropka) musza zajmowac dokladnie tyle miejsca w notatniku

  real tab, wyznacznik, pomocniczatab
  dimension tab(15,15)
  integer wymiar, i, j, k
  character*15 Nazwa_we, Nazwa_wy

  write(*,*) 'Podaj nazwe pliku wejsciowego: '
  read(*,*) Nazwa_we
  write(*,*) 'Podaj nazwe pliku, do ktorego chcesz zapisac wyniki: '
  read(*,*) Nazwa_wy

  open(10,file=Nazwa_we)
  open(11,file=Nazwa_wy)

  write(*,*) 'Jakiego wymiaru jest przetwarzana macierz kwadratowa?: '
  read(*,*) wymiar

  do while (wymiar > 15 .or. wymiar < 1)  !Warunek sprawdza, czy wymmiar macierzy jest spoza zakresu <1, 15>, jesli tak, to prosi o ponowne podanie wymiaru, dopoki nie bedzie poprawny
    write(*,*) 'Wymiar macierzy musi byc w przedziale <1, 15>'
    write(*,*) 'Jeszcze raz podaj liczbe elementow: '
    read(*,*) wymiar
  end do
  
  write (*,*) 'Wczytywanie macierzy z pliku Nazwa_we.txt ...'

  do i=1,wymiar
    read(10,*)(tab(i,j),j=1,wymiar)
  enddo !i

  write(*,*)
  write(*,*) 'Tak prezentuje sie podana macierz'
  write(11,*) 'Tak prezentuje sie podana macierz'

  do i=1,wymiar
    write(*,'(15f8.2)')(tab(i,j),j=1,wymiar)
    write(11,'(15f8.2)')(tab(i,j),j=1,wymiar)
  enddo !i
    
  do i=1,wymiar+1
    do j=i+1,wymiar
      pomocniczatab = tab(j,i)/tab(i,i)
      do k=i, wymiar
        tab(j,k)=tab(j,k)-pomocniczatab*tab(i,k)
      end do
    end do
  end do

  write(*,*)
  write(*,*)'Macierz po obliczeniach: '
  write(11,*)'Macierz po obliczeniach: '

  do i=1,wymiar
    write(*,'(15f8.2)')(tab(i,j),j=1,wymiar)
    write(11,'(15f8.2)')(tab(i,j),j=1,wymiar)
  enddo !i

  wyznacznik = 1    !Przypisuje zmiennej wyznacznik wartosc 1, aby program poprawnie liczyl wyznacznik
  do i=1, wymiar
    wyznacznik = wyznacznik * tab(i,i)  !Obliczanie wyznacznika poprzez mnozenie liczb lezacych na glownej przekatnej macierzy
  end do
  
  write(*,'(A)', advance="no") 'Wyznacznik macierzy wynosi: '
  write(*,'(F10.2)') wyznacznik
  write(11,'(A)', advance="no") 'Wyznacznik macierzy wynosi: '
  write(11,'(F10.2)') wyznacznik
  write(*,*)
  
  close(10)
  close(11)

  write(*,*) 'Nacisnij Enter aby zakonczyc program'
  read(*,*)

end program