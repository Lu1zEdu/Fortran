import numpy as np
import matplotlib.pyplot as plt


def mandelbrot_quaternion(q, max_iter):
    """
    Calcula a sequência de Mandelbrot para quatérnios (4D).
    q é um vetor de 4 números reais (representando o quatérnio a + bi + cj + dk).
    """
    z = np.array([0.0, 0.0, 0.0, 0.0])  # Inicia z no quatérnio (0, 0, 0, 0)
    for n in range(max_iter):
        if np.linalg.norm(z) > 2.0:  # Se o comprimento de z exceder 2
            return n

        # Itera a fórmula z = z^2 + q em 4D
        z = quaternion_mult(z, z) + q

    return max_iter


def quaternion_mult(q1, q2):
    """
    Multiplica dois quatérnios (q1 e q2).
    Cada quatérnio é representado como um vetor [a, b, c, d].
    """
    a1, b1, c1, d1 = q1
    a2, b2, c2, d2 = q2

    a = a1 * a2 - b1 * b2 - c1 * c2 - d1 * d2
    b = a1 * b2 + b1 * a2 + c1 * d2 - d1 * c2
    c = a1 * c2 - b1 * d2 + c1 * a2 + d1 * b2
    d = a1 * d2 + b1 * c2 - c1 * b2 + d1 * a2

    return np.array([a, b, c, d])


def generate_fractal_quaternion(width, height, max_iter=100):
    """
    Gera um fractal baseado em quatérnios e projeta-o em 2D para visualização.
    """
    xmin, xmax = -2.0, 2.0
    ymin, ymax = -2.0, 2.0

    # Inicializar imagem
    image = np.zeros((height, width))

    # Gerar fractal 4D e projetar em 2D
    for x in range(width):
        for y in range(height):
            # Mapeia o pixel (x, y) para o plano real-imaginário
            real = xmin + (x / width) * (xmax - xmin)
            imag = ymin + (y / height) * (ymax - ymin)

            # Define o quatérnio (a + bi + cj + dk) com c e d fixos
            c, d = 0.0, 0.0  # Fixando duas dimensões
            q = np.array([real, imag, c, d])

            # Chama a função que calcula o fractal
            iter_count = mandelbrot_quaternion(q, max_iter)

            # Usa o número de iterações para determinar a cor do pixel
            image[y, x] = iter_count

    return image


# Parâmetros de visualização
width, height = 800, 800

# Gera o fractal com quatérnios (4D)
fractal_image = generate_fractal_quaternion(width, height)

# Mostra a imagem resultante
plt.imshow(fractal_image, cmap="hot", extent=[-2, 2, -2, 2])
plt.colorbar()
plt.title("Fractal baseado em Quatérnios (Projeção 2D)")
plt.show()
