program calculadora_cientifica
    implicit none
    integer :: escolha
    real :: num1, num2, resultado
    real, dimension(2,2) :: mat1, mat2, mat_res
    integer :: i, j

    ! Menu principal
    do
        print *, 'Calculadora Científica'
        print *, '1. Seno'
        print *, '2. Cosseno'
        print *, '3. Tangente'
        print *, '4. Exponencial'
        print *, '5. Logaritmo natural'
        print *, '6. Multiplicação de Matrizes'
        print *, '7. Sair'
        print *, 'Escolha uma opção: '
        read *, escolha

        select case (escolha)
        case (1)
            ! Seno
            print *, 'Digite o ângulo em radianos:'
            read *, num1
            resultado = sin(num1)
            print *, 'Seno(', num1, ') = ', resultado

        case (2)
            ! Cosseno
            print *, 'Digite o ângulo em radianos:'
            read *, num1
            resultado = cos(num1)
            print *, 'Cosseno(', num1, ') = ', resultado

        case (3)
            ! Tangente
            print *, 'Digite o ângulo em radianos:'
            read *, num1
            resultado = tan(num1)
            print *, 'Tangente(', num1, ') = ', resultado

        case (4)
            ! Exponencial
            print *, 'Digite o número:'
            read *, num1
            resultado = exp(num1)
            print *, 'Exponencial de ', num1, ' = ', resultado

        case (5)
            ! Logaritmo natural
            print *, 'Digite o número:'
            read *, num1
            if (num1 > 0.0) then
                resultado = log(num1)
                print *, 'Logaritmo natural de ', num1, ' = ', resultado
            else
                print *, 'Número inválido! Deve ser maior que zero.'
            end if

        case (6)
            ! Multiplicação de Matrizes 2x2
            print *, 'Digite os elementos da primeira matriz 2x2:'
            do i = 1, 2
                do j = 1, 2
                    read *, mat1(i,j)
                end do
            end do

            print *, 'Digite os elementos da segunda matriz 2x2:'
            do i = 1, 2
                do j = 1, 2
                    read *, mat2(i,j)
                end do
            end do

            ! Multiplicação
            mat_res = matmul(mat1, mat2)
            print *, 'Resultado da multiplicação das matrizes:'
            do i = 1, 2
                print *, mat_res(i, :)
            end do

        case (7)
            exit

        case default
            print *, 'Opção inválida!'
        end select
    end do
end program calculadora_cientifica
