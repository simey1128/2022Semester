#include <iostream>
#include <omp.h>
#include <stdlib.h>
#include <ctime>
#include <cmath>

template<typename T>
void print_vector(const char* vector_name, T* vector, int vector_size){
  std::cout<<"\n** "<<vector_name<<std::endl;
  std::cout<<"[";
  for(int i=0; i<vector_size; i++) std::cout<<vector[i]<<", ";
  std::cout<<"]"<<std::endl;
}

void print_matrix(const char* matrix_name, double** matrix, int matrix_size){
  std::cout<<"\n** "<<matrix_name<<std::endl;
  for(int i=0; i<matrix_size; i++){
    std::cout<<"[";
    for(int j=0; j<matrix_size; j++){
      std::cout<<matrix[i][j]<<", ";
    }
    std::cout<<"]"<<std::endl;
  }
}

template<typename T>
void print_elapsed(const char* name, T value){
  std::cout<<">>>>> "<<name<<" elapsed: "<<value<<std::endl;
}

// need to optimization ~ dense matrix multiplication
void check_L21norm(double**input, int*pi, double**lower, double**upper, int matrix_size){
  
  double **PA_LU = new double*[matrix_size]();
  for(int i=0; i<matrix_size; i++){
    PA_LU[i] = new double[matrix_size]();
  }

  omp_set_num_threads(4);
  #pragma omp parallel for
  for(int r=0; r<matrix_size; r++){
    for(int c=0; c<matrix_size; c++){
      double pa_sum = 0.0;
      double lu_sum = 0.0;
      for(int k=0; k<matrix_size; k++){
        if(pi[r]==k) pa_sum += input[k][c];
        lu_sum += lower[r][k] * upper[k][c];
        
      }
      PA_LU[r][c] = pa_sum - lu_sum;
    }
  }

  // L2,1 norm
  double result = 0.0;
  #pragma omp parallel for reduction(+: result)
  for(int c=0; c<matrix_size; c++){
    double sum = 0.0;
    for(int r=0; r<matrix_size; r++){
      sum += pow(PA_LU[r][c], 2);
    }
    result += pow(sum, 0.5);
  }
  
  if(result<=1.0) std::cout<<":) success! L2,1 norm is "<< result<<std::endl;
  else std::cout<<":( failed. L2,1 norm is "<< result<<std::endl;

  printf("\n");
  for(int i=0; i<matrix_size; i++){
    delete[] PA_LU[i];
  }
  delete[] PA_LU;
}

template<typename T>
void swap_element(T &a, T &b){
  T temp = a;
  a = b;
  b = temp;
}

void swap_vector(double *a, double *b, int*col_range){
  // #pragma omp parallel for
  for(int i=col_range[0]; i<col_range[1]; i++) swap_element(a[i], b[i]);
}

void transpose_matrix(double**t, double**origin, int matrix_size){
  for(int i=0; i<matrix_size; i++){
    for(int j=0; j<matrix_size; j++){

    }
  }
}
void 
usage(const char *name)
{
	std::cout << "usage: " << name
                  << " matrix-size nworkers"
                  << std::endl;
 	exit(-1);
}

main(int argc, char **argv)
{

  const char *name = argv[0];

  if (argc < 3) usage(name);

  int matrix_size = atoi(argv[1]);

  int nworkers = atoi(argv[2]);

  std::cout << name << ": " 
            << matrix_size << " " << nworkers
            << std::endl;

  omp_set_num_threads(nworkers);

  double start_program = omp_get_wtime();

  //##################################################### elapsed time
  double elapsed_program = 0.0;
  double elapsed_LU = 0.0;
  
  
  //##################################################### memory allocation 
  int *pi = new int[matrix_size]();
  double **upper = new double*[matrix_size]();
  double **lower = new double*[matrix_size]();
  double **input = new double*[matrix_size]();
  double **_input = new double*[matrix_size]();

  for(int i=0; i<matrix_size; i++){
    pi[i] = i;
    upper[i] = new double[matrix_size]();
    lower[i] = new double[matrix_size]();
    lower[i][i] = 1;

    input[i] = new double[matrix_size]();
    _input[i] = new double[matrix_size]();
  }

  //##################################################### matrix initialization
  unsigned int seed;
  seed = 256 + omp_get_thread_num();
  
  for(int i=0; i<matrix_size; i++){
    for(int j=0; j<matrix_size; j++)
      {
        input[i][j] = (double )rand_r(&seed)/(double)RAND_MAX;
        _input[i][j] = input[i][j];
      }    
  } 
  //##################################################### Start LU decomposision
  bool singular = false;
  double start_LU = omp_get_wtime();
  for(int k=0; k<matrix_size; k++){
    double max_value = 0.0;
    int max_idx = 0;
    #pragma omp parallel default(none) firstprivate(k, matrix_size, singular) shared(input, upper, lower, pi, max_idx, max_value)
    {
      #pragma omp for reduction(max:max_value) reduction(max: max_idx) 
      for(int i=k; i<matrix_size; i++){
        double abs_value = abs(input[i][k]);
        if(max_value<abs_value){
          max_value = abs_value;
          max_idx = i;
        }
      }

      #pragma omp single
      {
        if(max_value == 0){
          singular = true;
          printf("singular matrix\n");
        }

        swap_element(pi[k], pi[max_idx]);
        double *temp = input[k];
        input[k] = input[max_idx];
        input[max_idx] = temp;

        int lower_col_range[2] = {0, k};
        swap_vector(lower[k], lower[max_idx], lower_col_range);

        upper[k][k] = input[k][k];
      }

      #pragma omp for nowait
      for(int i=k+1; i<matrix_size; i++){
        lower[i][k] = input[i][k] / upper[k][k]; 
      }
      #pragma omp for schedule(static,8)
      for(int i=k+1; i<matrix_size; i++){
        upper[k][i] = input[k][i];
      }
      #pragma omp for 
      for(int i=k+1; i<matrix_size; i++){
        for(int j=k+1; j<matrix_size; j++){
          input[i][j] -= lower[i][k] * upper[k][j];
        }
      }
      
      
    }
  }
  elapsed_LU = omp_get_wtime() - start_LU;
  elapsed_program = omp_get_wtime() - start_program;

  //##################################################### log elapsed time
  print_elapsed("LU decomposition", elapsed_LU);
  print_elapsed("program", elapsed_program);

  //##################################################### check L2,1 norm value
  check_L21norm(_input, pi, lower, upper, matrix_size);
  

  //##################################################### memory deallocation
  for(int i=0; i<matrix_size; i++){
    delete[] upper[i];
    delete[] lower[i];
    delete[] input[i];
    delete[] _input[i];
  }
  delete[] upper;
  delete[] lower;
  delete[] input;
  delete[] _input;
  delete[] pi;
  
  return 0;

    
  }

  

