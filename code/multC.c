#include <stdio.h>
#include <time.h>
#include <stdlib.h>

// Função para alocar memória para um vetor
double *instantiate_vector(int vector_size)
{
  return (double *)malloc((vector_size + 1) * sizeof(double));
}

// Função para criar um vetor preenchido com zeros
double *zero_vector(int vector_size)
{
  double *vector = instantiate_vector(vector_size);
  for (int i = 0; i < vector_size; i++)
    vector[i] = 0;

  return vector;
}

// Função para gerar um vetor aleatório
double *generate_random_vector(int vector_size)
{
  double *vector = instantiate_vector(vector_size);
  for (int i = 0; i <= vector_size; i++)
    vector[i] = rand();

  return vector;
}

// Função para alocar memória para uma matriz
double **instantiate_matrix(int matrix_size)
{
  double **matrix = (double **)malloc((matrix_size + 1) * sizeof(double *));
  for (int i = 0; i <= matrix_size; i++)
    matrix[i] = instantiate_vector(matrix_size);

  return matrix;
}

// Função para preencher uma linha da matriz com valores aleatórios
void fill_matrix_line(int current_line, int matrix_size, double **matrix)
{
  for (int j = 0; j < matrix_size; j++)
  {
    matrix[current_line][j] = rand();
  }
}

// Função para gerar uma matriz aleatória
double **generate_random_matrix(int matrix_size)
{
  double **matrix = instantiate_matrix(matrix_size);
  for (int i = 0; i <= matrix_size; i++)
    fill_matrix_line(i, matrix_size, matrix);

  return matrix;
}

// Função para realizar o produto linha da matriz-vetor na ordem IJ
void line_product_ij(int size, int line_index, double *result, double **matrix, double *vector)
{
  for (int j = 0; j <= size; j++)
    result[line_index] += matrix[line_index][j] * vector[j];
}

// Função para realizar o produto matriz-vetor na ordem IJ
double *matrix_vector_product_ij(int size, double **matrix, double *vector)
{
  double *result = zero_vector(size);

  for (int i = 0; i <= size; i++)
    line_product_ij(size, i, result, matrix, vector);

  return result;
}

// Função para realizar o produto coluna da matriz-vetor na ordem JI
void line_product_ji(int size, int column_index, double *result, double **matrix, double *vector)
{
  for (int i = 0; i <= size; i++)
    result[i] += matrix[i][column_index] * vector[column_index];
}

// Função para realizar o produto matriz-vetor na ordem JI
double *matrix_vector_product_ji(int size, double **matrix, double *vector)
{
  double *result = zero_vector(size);
  for (int j = 0; j <= size; j++)
    line_product_ji(size, j, result, matrix, vector);

  return result;
}

// Função para liberar a memória alocada para a matriz
void free_matrix(double **matrix, int matrix_size)
{
  for (int i = 0; i <= matrix_size; i++)
    free(matrix[i]);

  free(matrix);
}

int main(void)
{
  srand(time(NULL));
  FILE *ij, *ji;
  clock_t startT, endT;
  double totalT;
  int size;

  ij = fopen("../files/c/IJ.csv", "w+");
  ji = fopen("../files/c/JI.csv", "w+");

  for (int i = 0; i < 11; i++)
  {
    size = 4500 * i;
    double **A = generate_random_matrix(size);
    double *x = generate_random_vector(size);

    startT = clock();
    double *b = matrix_vector_product_ij(size, A, x);
    endT = clock();

    totalT = ((double)(endT - startT)) / CLOCKS_PER_SEC;

    printf("\nIt took the computer %.6f s to compute a %d-degree matrix on IJ; i = %d\n", totalT, size, i);
    fprintf(ij, "%d,%.6f\n", size, totalT);

    free(b);

    startT = clock();
    b = matrix_vector_product_ji(size, A, x);
    endT = clock();

    totalT = ((double)(endT - startT)) / CLOCKS_PER_SEC;

    printf("\nIt took the computer %.6f s to compute a %d-degree matrix on JI; i = %d\n", totalT, size, i);
    fprintf(ji, "%d,%.6f\n", size, totalT);

    free_matrix(A, size);
    free(x);
  }

  fclose(ij);
  fclose(ji);

  return 0;
}
