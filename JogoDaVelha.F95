program jogo_da_velha
  implicit none

  character(len=1), dimension(3, 3) :: tabuleiro
  character(len=1) :: jogador
  integer :: linha, coluna, i, j
  logical :: jogo_ativo
  character(len=1) :: vencedor

  ! Inicializa o tabuleiro
  tabuleiro = ' '

  jogador = 'X'
  jogo_ativo = .true.

  print *, "Bem-vindo ao Jogo da Velha!"
  
  do while (jogo_ativo)
    call mostrar_tabuleiro(tabuleiro)
    print *, "Jogador ", jogador, ", faça sua jogada (linha e coluna): "
    read *, linha, coluna

    ! Verifica se a jogada é válida
    if (linha < 1 .or. linha > 3 .or. coluna < 1 .or. coluna > 3 .or. tabuleiro(linha, coluna) /= ' ') then
      print *, "Jogada inválida. Tente novamente."
    else
      tabuleiro(linha, coluna) = jogador
      call verificar_vencedor(tabuleiro, jogador, vencedor, jogo_ativo)
      
      if (jogo_ativo) then
        if (jogador == 'X') then
          jogador = 'O'
        else
          jogador = 'X'
        endif
      endif
    endif
  end do

  call mostrar_tabuleiro(tabuleiro)
  if (vencedor == ' ') then
    print *, "O jogo terminou em empate!"
  else
    print *, "Parabéns Jogador ", vencedor, "! Você venceu!"
  endif

contains

  subroutine mostrar_tabuleiro(tabuleiro)
    character(len=1), dimension(3, 3), intent(in) :: tabuleiro
    integer :: i, j

    print *, "  1 2 3"
    do i = 1, 3
      write(*, '(A)', advance="no") i, ' '
      do j = 1, 3
        if (tabuleiro(i, j) == ' ') then
          write(*, '(A)', advance="no") '.'
        else
          write(*, '(A)', advance="no") tabuleiro(i, j)
        endif
        if (j < 3) write(*, '(A)', advance="no") '|'
      end do
      print *, ''
      if (i < 3) print *, "  -----"
    end do
  end subroutine mostrar_tabuleiro

  subroutine verificar_vencedor(tabuleiro, jogador, vencedor, jogo_ativo)
    character(len=1), dimension(3, 3), intent(in) :: tabuleiro
    character(len=1), intent(in) :: jogador
    character(len=1), intent(out) :: vencedor
    logical, intent(out) :: jogo_ativo
    integer :: i

    vencedor = ' '
    jogo_ativo = .true.

    ! Verifica linhas e colunas
    do i = 1, 3
      if (all(tabuleiro(i, :) == jogador) .or. all(tabuleiro(:, i) == jogador)) then
        vencedor = jogador
        jogo_ativo = .false.
        return
      endif
    end do

    ! Verifica diagonais
    if ((tabuleiro(1, 1) == jogador .and. tabuleiro(2, 2) == jogador .and. tabuleiro(3, 3) == jogador) .or. &
        (tabuleiro(1, 3) == jogador .and. tabuleiro(2, 2) == jogador .and. tabuleiro(3, 1) == jogador)) then
      vencedor = jogador
      jogo_ativo = .false.
      return
    endif

    ! Verifica se o tabuleiro está cheio
    if (all(tabuleiro /= ' ')) then
      vencedor = ' '
      jogo_ativo = .false.
    endif
  end subroutine verificar_vencedor

end program jogo_da_velha
