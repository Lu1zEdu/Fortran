program fractal_quaternion
  implicit none
  integer, parameter :: dp = kind(1.0d0)
  integer, parameter :: width = 800, height = 800, max_iter = 100
  real(dp) :: xmin, xmax, ymin, ymax
  real(dp), dimension(width, height) :: fractal_image
  integer :: x, y, iter_count
  real(dp) :: real_part, imag_part

  ! Definir limites do plano de visualização
  xmin = -2.0_dp
  xmax = 2.0_dp
  ymin = -2.0_dp
  ymax = 2.0_dp

  ! Loop para gerar o fractal 4D (quatérnio)
  do x = 1, width
     do y = 1, height
        ! Mapear o pixel (x, y) para o plano real-imaginário
        real_part = xmin + (x - 1) * (xmax - xmin) / (width - 1)
        imag_part = ymin + (y - 1) * (ymax - ymin) / (height - 1)
        
        ! Chamar a função que calcula o fractal e retorna o número de iterações
        iter_count = mandelbrot_quaternion(real_part, imag_part, max_iter)

        ! Armazenar o número de iterações na imagem
        fractal_image(x, y) = real(iter_count, dp)
     end do
  end do

  ! Escrever a imagem em um arquivo para posterior visualização
  call write_image_to_file('fractal_output.dat', fractal_image, width, height)

end program fractal_quaternion

! Função que calcula a sequência de Mandelbrot para quatérnios (4D)
integer function mandelbrot_quaternion(real_part, imag_part, max_iter)
  implicit none
  integer, parameter :: dp = kind(1.0d0)
  real(dp), intent(in) :: real_part, imag_part
  integer, intent(in) :: max_iter
  real(dp), dimension(4) :: z, q
  integer :: n

  ! Inicializa z e define o quatérnio (a + bi + cj + dk)
  z = 0.0_dp
  q(1) = real_part
  q(2) = imag_part
  q(3) = 0.0_dp  ! Fixando a terceira dimensão (c)
  q(4) = 0.0_dp  ! Fixando a quarta dimensão (d)

  ! Iterar a fórmula z = z^2 + q em 4D
  do n = 1, max_iter
     if (norm(z) > 2.0_dp) then
        mandelbrot_quaternion = n
        return
     endif
     z = quaternion_mult(z, z) + q
  end do

  mandelbrot_quaternion = max_iter
end function mandelbrot_quaternion

! Função para multiplicar dois quatérnios
real(dp), dimension(4) function quaternion_mult(q1, q2)
  implicit none
  integer, parameter :: dp = kind(1.0d0)
  real(dp), intent(in), dimension(4) :: q1, q2
  real(dp) :: a, b, c, d

  a = q1(1)*q2(1) - q1(2)*q2(2) - q1(3)*q2(3) - q1(4)*q2(4)
  b = q1(1)*q2(2) + q1(2)*q2(1) + q1(3)*q2(4) - q1(4)*q2(3)
  c = q1(1)*q2(3) - q1(2)*q2(4) + q1(3)*q2(1) + q1(4)*q2(2)
  d = q1(1)*q2(4) + q1(2)*q2(3) - q1(3)*q2(2) + q1(4)*q2(1)

  quaternion_mult(1) = a
  quaternion_mult(2) = b
  quaternion_mult(3) = c
  quaternion_mult(4) = d
end function quaternion_mult

! Função que calcula a norma de um quatérnio
real(dp) function norm(q)
  implicit none
  real(dp), intent(in), dimension(4) :: q
  norm = sqrt(q(1)**2 + q(2)**2 + q(3)**2 + q(4)**2)
end function norm

! Subrotina para salvar a imagem em um arquivo
subroutine write_image_to_file(filename, image, width, height)
  implicit none
  character(len=*), intent(in) :: filename
  real(dp), intent(in), dimension(width, height) :: image
  integer, intent(in) :: width, height
  integer :: x, y
  open(unit=10, file=filename, status='replace')
  do y = 1, height
     do x = 1, width
        write(10, '(F8.2)', advance='no') image(x, y)
     end do
     write(10, *)  ! Nova linha
  end do
  close(10)
end subroutine write_image_to_file
