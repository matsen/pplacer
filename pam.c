#include <assert.h>

#include <gsl/gsl_vector.h>
#include <gsl/gsl_matrix.h>

typedef struct {
  /* Number of medoids */
  size_t k;
  /* |S| x |U| Distance matrix */
  gsl_matrix *M;

  /* Indicator whether s_i is a medoid */
  gsl_vector_short *in_set;

  /* Index of closest item in set */
  gsl_vector_uint *cl_index;
  /* distance to item referenced by cl_index */
  gsl_vector_float *cl_dist;
} pam_partition_t;

typedef pam_partition_t* pam_partition;

pam_partition init_pam_partition(const gsl_matrix_float *M, const size_t k) {
  assert(k <= M->size1);

  pam_partition p = malloc(sizeof(pam_partition_t));

  return p;
}

/*
 * Frees p
 *
 * Note that p->M is *not* freed by this function.
 */
void free_pam_partition(const pam_partition p) {
  gsl_vector_short_free(p->in_set);
  gsl_vector_uint_free(p->cl_index);
  gsl_vector_float_free(p->cl_dist);

  free(p);
}

int main() {
  gsl_matrix_float* m;
  pam_partition p;

  m = gsl_matrix_float_calloc(10, 3);
  p = init_pam_partition(m, 6);

  free_pam_partition(p);
  gsl_matrix_float_free(m);
  
  return 0;
}
