#include <float.h>
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>

#include <gsl/gsl_vector.h>
#include <gsl/gsl_matrix.h>
#include <gsl/gsl_rng.h>
#include <gsl/gsl_randist.h>
/*
 * Partitioning Around Medoids (PAM)
 *
 * Let S be a set of selectable items
 * Let U be a set of unselectable items
 *
 * This code takes an SxU distance matrix, and a value k, and attempts to find
 * the set S' which minimizes the average distance from U to the closest item
 * in S'.
 */

/* Vector / Matrix operations */



/* Types */
typedef struct {
  /* Number of medoids */
  size_t k;
  /* |S| x |U| Distance matrix */
  gsl_matrix_float *M;

  /* Indicator whether s_i is a medoid */
  gsl_vector_uchar *in_set;

  /* Index of closest item in set */
  gsl_vector_uint *cl_index;
  /* distance to item referenced by cl_index */
  gsl_vector_float *cl_dist;
} pam_partition_t;

typedef pam_partition_t* pam_partition;

/* Declarations */
static void gsl_vector_float_masked_min_index(const gsl_vector_float *v,
                                 const gsl_vector_uchar *mask,
                                 size_t *index, /*OUT*/
                                 float *value /*OUT*/);

static size_t* range(const size_t n);

pam_partition pam_init_partition(gsl_matrix_float *M, const size_t k);
void free_pam_partition(const pam_partition p);
void pam_print_partition(FILE* stream, const pam_partition p);
float pam_total_cost(const pam_partition p);

static void pam_choose_random_partition(pam_partition p);
static void pam_find_closest_medoid(pam_partition p);
static void pam_find_closest_medoid_index(pam_partition p, size_t i);

static void pam_run(pam_partition p);

/* Initialize a PAM partition given distances M, keep count k */
pam_partition pam_init_partition(gsl_matrix_float *M, const size_t k) {
  assert(k <= M->size1);
  assert(k > 0);

  pam_partition p = malloc(sizeof(pam_partition_t));
  p->M = M;
  p->k = k;

  p->in_set = gsl_vector_uchar_calloc(M->size1);

  p->cl_index = gsl_vector_uint_calloc(M->size2);
  p->cl_dist  = gsl_vector_float_calloc(M->size2);

  /* Initialize S' randomly, calculate distances between U and S' */
  pam_choose_random_partition(p);
  pam_find_closest_medoid(p);

  return p;
}

/* Return the total cost of a partition */
float pam_total_cost(const pam_partition p) {
  float s = 0.0;
  const gsl_vector_float *cl_dist = p->cl_dist;
  size_t i;

  for (i = 0; i < cl_dist->size; i++) {
    s += gsl_vector_float_get(cl_dist, i);
  }

  return s;
}

/*
 * Frees p
 *
 * Note that the distance matrix p->M is *not* freed by this function.
 */
void free_pam_partition(const pam_partition p) {
  gsl_vector_uchar_free(p->in_set);
  gsl_vector_uint_free(p->cl_index);
  gsl_vector_float_free(p->cl_dist);

  free(p);
}

/* Generate an array containing the range from 0 to n-1 */
static size_t* range(const size_t n) {
  size_t i;
  size_t* r = malloc(sizeof(size_t)*n);
  for (i = 0; i < n; i++)
    r[i] = i;
  return r;
}

/* Randomly set S' */
static void pam_choose_random_partition(pam_partition p) {
  gsl_rng *rng;
  size_t *indices, *chosen, i;

  rng = gsl_rng_alloc(gsl_rng_taus);
  indices = range(p->M->size1);
  chosen = (size_t*)calloc(p->k, sizeof(size_t));
  gsl_ran_choose(rng, (void*)chosen, p->k, indices, p->M->size1, sizeof(size_t));

  /* Reset */
  gsl_vector_uchar_set_zero(p->in_set);
  for (i = 0; i < p->k; i++) {
    gsl_vector_uchar_set(p->in_set, chosen[i], 1);
  }

  gsl_rng_free(rng);
  free(indices);
  free(chosen);
}

void pam_print_partition(FILE* stream, const pam_partition p) {
  fprintf(stream, "%lu x %lu; k= %lu\n", p->M->size1, p->M->size2, p->k);
  gsl_vector_uchar_fprintf(stream, p->in_set, "%d");
  gsl_matrix_float_fprintf(stream, p->M, "%f");
}

/* Set the closest medoid, distance to closest medoid for all i */
static void pam_find_closest_medoid(pam_partition p) {
  /*
   * For each item in U, find closest item in S', recording the distance and
   * index
   */
  size_t i;

  for (i = 0; i < p->M->size2; i++) {
    pam_find_closest_medoid_index(p, i);
  }
}

/* Set the closest medoid, distance to closest medoid for column i */
static void pam_find_closest_medoid_index(pam_partition p, size_t i) {
  size_t index;
  float min;
  gsl_vector_float_view col;

  min = FLT_MAX;
  col = gsl_matrix_float_column(p->M, i);
  gsl_vector_float_masked_min_index(&(col.vector), p->in_set, &index, &min);

  assert(min < FLT_MAX);

  gsl_vector_uint_set(p->cl_index, i, index);
  gsl_vector_float_set(p->cl_dist, i, min);
}

/*
 * v:    vector to search for index of minimum value
 * mask: boolean vector to indicate if v[i] is to be used
 */
static void gsl_vector_float_masked_min_index(const gsl_vector_float *v,
                                 const gsl_vector_uchar *mask,
                                 size_t *index, /*OUT*/
                                 float *value /*OUT*/) {
  float min = FLT_MAX, val;
  long idx = -1;
  size_t i;
  assert(v->size == mask->size);

  for (i = 0; i < v->size; i++) {
    /* Check mask */
    if (!gsl_vector_uchar_get(mask, i)) continue;

    val = gsl_vector_float_get(v, i);
    if (val < min) {
      min = val;
      idx = i;
    }
  }

  /* Set result */
  if (idx >= 0) {
    *index = idx;
    *value = min;
  }
}

/*
 * Calculate cost if states of i and j are reversed. Restores original
 * configuration prior to returning.
 *
 * p: partition
 * i, j: indices to swap. *i* must be a medoid; *j* must not.
 */
float pam_swap_update_cost(pam_partition p, size_t i, size_t j,
    gsl_vector_uint *cl_index, gsl_vector_float *cl_dist) {
  assert(gsl_vector_uchar_get(p->in_set, i) && !gsl_vector_uchar_get(p->in_set, j));
  /* Back up current values */
  gsl_vector_uint *cli = p->cl_index;
  gsl_vector_float *cld = p->cl_dist;
  float result;

  p->cl_index = cl_index;
  p->cl_dist = cl_dist;

  /* Swap */
  gsl_vector_uchar_swap_elements(p->in_set, i, j);

  /* OPTIMIZE HERE */
  pam_find_closest_medoid(p);
  result = pam_total_cost(p);

  /* Finally, restore state */
  gsl_vector_uchar_swap_elements(p->in_set, i, j);
  p->cl_index = cli;
  p->cl_dist = cld;

  return result;
}

/* Run PAM */
void pam_run(pam_partition p, size_t max_iters) {
  size_t i, j, k, m, n, trimmed_size = p->M->size1 - p->k, any_swaps = 0;
  size_t *medoids, *trimmed;
  float c, current_cost;
  gsl_vector_float *cost = gsl_vector_float_alloc(trimmed_size);
  gsl_vector_uint *cl_index = gsl_vector_uint_alloc(p->cl_index->size);
  gsl_vector_float *cl_dist = gsl_vector_float_alloc(p->cl_dist->size);

  medoids = malloc(sizeof(size_t) * p->k);
  trimmed = malloc(sizeof(size_t) * (p->M->size1 - p->k));

  j = 0;
  k = 0;
  for (i = 0; i < p->M->size1; i++) {
    if (gsl_vector_uchar_get(p->in_set, i))
      medoids[j++] = i;
    else
      trimmed[k++] = i;
  }

  assert(j == p->k);
  assert(k == p->M->size1 - p->k);

  do {
    any_swaps = 0;

    /* For every medoid, m, swap with every non-medoid, compute cost */
    for(i = 0; i < p->k; i++) {
      m = medoids[i];
      current_cost = pam_total_cost(p);
      /* Try every non-medoid */
      gsl_vector_float_set_all(cost, FLT_MAX);

      for (j = 0; j < trimmed_size; j++) {
        n = trimmed[j];
        c = pam_swap_update_cost(p, m, n, cl_index, cl_dist);
        gsl_vector_float_set(cost, j, c);
      }

      /* Find the minimum cost from all swaps */
      j = gsl_vector_float_min_index(cost);
      if (gsl_vector_float_get(cost, j) < current_cost) {
        /* Current cost beaten */
        any_swaps = 1;
        n = trimmed[j];
        gsl_vector_uchar_swap_elements(p->in_set, m, n);

        /* Move n to medoids, m to trimmed */
        trimmed[j] = m;
        medoids[i] = n;

        /* Recalculate cached values */
        pam_find_closest_medoid(p);
      }
    }
  } while (any_swaps);

  gsl_vector_float_free(cost);
  gsl_vector_uint_free(cl_index);
  gsl_vector_float_free(cl_dist);
  free(medoids);
  free(trimmed);
}

int main() {
  gsl_matrix_float* m;
  pam_partition p;

  m = gsl_matrix_float_calloc(10, 3);
  gsl_matrix_float_set_all(m, 0.5);
  p = pam_init_partition(m, 6);
  pam_run(p);

  free_pam_partition(p);
  gsl_matrix_float_free(m);

  return 0;
}
