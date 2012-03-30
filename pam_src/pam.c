#include <float.h>
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>

#include <gsl/gsl_vector.h>
#include <gsl/gsl_matrix.h>
#include <gsl/gsl_rng.h>
#include <gsl/gsl_randist.h>

#ifdef PAM_TEST
#include <time.h>
#endif

/*
 * Partitioning Around Medoids (PAM)
 *
 * Let S be a set of selectable items
 * Let U be a set of unselectable items
 *
 * This code takes an SxU distance matrix, and a value k, and attempts to find
 * the set S' which minimizes the average distance from U to the closest item
 * in S'.
 *
 * This differs from traditional k-medoids, in that instead of choosing a
 * subset of points from the original dataset, we have distances between two
 * sets of items:
 *   S - "Selectable" items to search for an optimal subset of
 *   U - "Unselectable" items used to compute cost
 */

/* Types */
typedef struct __pam_partition_t {
  /* Number of medoids */
  size_t k;
  /* |S| x |U| Distance matrix */
  gsl_matrix *M;

  /* Indicator whether s_i is a medoid */
  gsl_vector_uchar *in_set;

  /* Index of closest item in set */
  gsl_vector_ulong *cl_index;
  /* distance to item referenced by cl_index */
  gsl_vector *cl_dist;
} pam_partition_t;

typedef pam_partition_t *pam_partition;

int PAM_VERBOSE = 0;

/* Declarations */
static void gsl_vector_masked_min_index(const gsl_vector * v,
                                              const gsl_vector_uchar * mask,
                                              /*OUT*/ size_t * index,
                                              /*OUT*/ double *value);

static size_t * range(const size_t n);

pam_partition pam_partition_init(gsl_matrix * M, const size_t k);
void pam_partition_free(const pam_partition p);
void pam_partition_fprintf(FILE * stream, const pam_partition p);
double pam_total_cost(const pam_partition p);

static double pam_swap_cost(pam_partition p, size_t m, size_t n);
static double pam_swap_update_cost(pam_partition p, size_t m, size_t n,
                                  gsl_vector_ulong * cl_index,
                                  gsl_vector * cl_dist);

static void pam_choose_random_partition(pam_partition p);
static void pam_find_closest_medoid(pam_partition p);
static void pam_find_closest_medoid_index(pam_partition p, size_t i);

static void pam_run(pam_partition p, size_t max_iters);

size_t * pam(gsl_matrix * distances, size_t k, /*OUT*/ double * dist);

/* Initialize a PAM partition given distances M, keep count k */
pam_partition pam_partition_init(gsl_matrix * M, const size_t k)
{
  assert(k <= M->size1);
  assert(k > 0);

  pam_partition p = malloc(sizeof(pam_partition_t));
  p->M = M;
  p->k = k;

  p->in_set = gsl_vector_uchar_calloc(M->size1);

  p->cl_index = gsl_vector_ulong_calloc(M->size2);
  p->cl_dist = gsl_vector_calloc(M->size2);

  /* Initialize S' randomly, calculate distances between U and S' */
  pam_choose_random_partition(p);
  pam_find_closest_medoid(p);

  return p;
}

/* Return the total cost of a partition */
double pam_total_cost(const pam_partition p)
{
  double cost = 0.0;
  const gsl_vector *cl_dist = p->cl_dist;
  size_t i;

  for (i = 0; i < cl_dist->size; i++) {
    cost += gsl_vector_get(cl_dist, i);
  }

  return cost;
}

/*
 * Frees p
 *
 * Note that the distance matrix p->M is *not* freed by this function.
 */
void pam_partition_free(const pam_partition p)
{
  gsl_vector_uchar_free(p->in_set);
  gsl_vector_ulong_free(p->cl_index);
  gsl_vector_free(p->cl_dist);

  free(p);
}

/* Generate an array containing the range from 0 to n-1 */
static size_t *range(const size_t n)
{
  size_t i;
  size_t *r = malloc(sizeof(size_t) * n);
  for (i = 0; i < n; i++)
    r[i] = i;
  return r;
}

/* Randomly set S' */
static void pam_choose_random_partition(pam_partition p)
{
  gsl_rng *rng;
  size_t *indices, *chosen, i;

  rng = gsl_rng_alloc(gsl_rng_taus2);
  indices = range(p->M->size1);
  chosen = (size_t *) calloc(p->k, sizeof(size_t));
  gsl_ran_choose(rng, (void *) chosen, p->k, indices, p->M->size1,
                 sizeof(size_t));

  /* Reset */
  gsl_vector_uchar_set_zero(p->in_set);
  for (i = 0; i < p->k; i++) {
    gsl_vector_uchar_set(p->in_set, chosen[i], 1);
  }

  gsl_rng_free(rng);
  free(indices);
  free(chosen);
}

void pam_partition_fprintf(FILE * stream, const pam_partition p)
{
  size_t i, j;
  fprintf(stream, "%lu x %lu; k= %lu; total cost= %f\n", p->M->size1,
          p->M->size2, p->k, pam_total_cost(p));
  for (i = 0; i < p->M->size1; i++) {
    fprintf(stream, "%d:\t", gsl_vector_uchar_get(p->in_set, i));
    for (j = 0; j < p->M->size2; j++) {
      fprintf(stream, "%f\t", gsl_matrix_get(p->M, i, j));
    }
    fprintf(stream, "\n");
  }
  for (i = 0; i < p->cl_index->size; i++) {
    fprintf(stream, "\t%lu", gsl_vector_ulong_get(p->cl_index, i));
  }
  fprintf(stream, "\n");
  for (i = 0; i < p->cl_dist->size; i++) {
    fprintf(stream, "\t%f", gsl_vector_get(p->cl_dist, i));
  }
  fprintf(stream, "\n");
}

/* Set the closest medoid, distance to closest medoid for all i */
static void pam_find_closest_medoid(pam_partition p)
{
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
static void pam_find_closest_medoid_index(pam_partition p, size_t i)
{
  size_t index;
  double min;
  gsl_vector_view col;

  min = FLT_MAX;
  col = gsl_matrix_column(p->M, i);
  gsl_vector_masked_min_index(&(col.vector), p->in_set, &index,
                                    &min);

  assert(min < FLT_MAX);

  gsl_vector_ulong_set(p->cl_index, i, index);
  gsl_vector_set(p->cl_dist, i, min);
}

/*
 * v:    vector to search for index of minimum value
 * mask: boolean vector to indicate if v[i] is to be used
 */
static void
gsl_vector_masked_min_index(const gsl_vector * v,
                                  const gsl_vector_uchar * mask,
                                  size_t * index, /*OUT*/
                                  double * value /*OUT*/)
{
  double min = FLT_MAX, val;
  long idx = -1;
  size_t i;
  assert(v->size == mask->size);

  for (i = 0; i < v->size; i++) {
    /* Check mask */
    if (!gsl_vector_uchar_get(mask, i))
      continue;

    val = gsl_vector_get(v, i);
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
static double pam_swap_update_cost(pam_partition p, size_t m, size_t n,
                                  gsl_vector_ulong * cl_index,
                                  gsl_vector * cl_dist)
{
  assert(gsl_vector_uchar_get(p->in_set, m)
         && !gsl_vector_uchar_get(p->in_set, n));
  /* Back up current values */
  gsl_vector_ulong *cli = p->cl_index;
  gsl_vector *cld = p->cl_dist;
  double result;

  /* Copy current values */
  gsl_vector_ulong_memcpy(cl_index, cli);
  gsl_vector_memcpy(cl_dist, cld);
  p->cl_index = cl_index;
  p->cl_dist = cl_dist;

  /* Swap */
  gsl_vector_uchar_swap_elements(p->in_set, m, n);

  result = pam_swap_cost(p, m, n);
  assert(result == pam_total_cost(p));

  /* Finally, restore state */
  gsl_vector_uchar_swap_elements(p->in_set, m, n);
  p->cl_index = cli;
  p->cl_dist = cld;

  return result;
}

/*
 * Update the cost after swapping current medoid m with non-medoid n
 * Distance to closest medoid, closest medoid index are updated.
 */
static double pam_swap_cost(pam_partition p, size_t m, size_t n)
{
  double cost = 0.0;
  size_t i, cl;
  gsl_vector_view col;

  /* Update for each column */
  for (i = 0; i < p->M->size2; i++) {
    cl = gsl_vector_ulong_get(p->cl_index, i);

    /* If closest to medoid being removed, find new closest medoid */
    if (cl == m) {
      col = gsl_matrix_column(p->M, i);
      gsl_vector_masked_min_index(&(col.vector), p->in_set,
                                        &cl,
                                        gsl_vector_ptr(p->cl_dist,
                                                             i));
      gsl_vector_ulong_set(p->cl_index, i, cl);
    } else {
      /* Check if the new medoid is closer than the old */
      assert(gsl_vector_get(p->cl_dist, i) ==
             gsl_matrix_get(p->M,
                                  gsl_vector_ulong_get(p->cl_index, i), i));
      if (gsl_matrix_get(p->M, n, i) <
          gsl_vector_get(p->cl_dist, i)) {
        gsl_vector_set(p->cl_dist, i,
                             gsl_matrix_get(p->M, n, i));
        gsl_vector_ulong_set(p->cl_index, i, n);
      }
    }
    cost += gsl_vector_get(p->cl_dist, i);
  }

  return cost;
}

/* Run PAM */
static void pam_run(pam_partition p, size_t max_iters)
{
  if (p->k == p->M->size1) {
    /* Simple case */
    return;
  }

  size_t i, j, k, m, n, trimmed_size = p->M->size1 - p->k, any_swaps =
      0, iter = 0;
  size_t *medoids, *trimmed;
  double c, current_cost;
  gsl_vector *cost = gsl_vector_alloc(trimmed_size);
  gsl_vector_ulong *cl_index = gsl_vector_ulong_alloc(p->cl_index->size);
  gsl_vector *cl_dist = gsl_vector_alloc(p->cl_dist->size);

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
    if (PAM_VERBOSE)
      fprintf(stderr, "Iteration %lu\n", iter);

    any_swaps = 0;

    /* For every medoid, m, swap with every non-medoid, compute cost */
    for (i = 0; i < p->k; i++) {
      m = medoids[i];
      current_cost = pam_total_cost(p);
      /* Try every non-medoid */
      gsl_vector_set_all(cost, FLT_MAX);

      for (j = 0; j < trimmed_size; j++) {
        n = trimmed[j];
        c = pam_swap_update_cost(p, m, n, cl_index, cl_dist);
        gsl_vector_set(cost, j, c);
      }

      /* Find the minimum cost from all swaps */
      j = gsl_vector_min_index(cost);
      if (gsl_vector_get(cost, j) < current_cost) {
        /* Current cost beaten */
        any_swaps = 1;
        n = trimmed[j];
        assert(n != m);
        assert(!gsl_vector_uchar_get(p->in_set, n));
        assert(gsl_vector_uchar_get(p->in_set, m));
        if (PAM_VERBOSE)
          fprintf(stderr, "SWAP: %lu->%lu [%f -> %f]\n", m, n,
                  current_cost, gsl_vector_get(cost, j));
        gsl_vector_uchar_swap_elements(p->in_set, m, n);

        /* Move n to medoids, m to trimmed */
        trimmed[j] = m;
        medoids[i] = n;

        /* Recalculate cached values */
        pam_swap_cost(p, m, n);
      }
    }
  }
  while (any_swaps && ++iter < max_iters);

  if (PAM_VERBOSE) {
    fprintf(stderr, "Done in %lu iterations. Final config:\n", iter);
    gsl_vector_uchar_fprintf(stderr, p->in_set, "%d");
    fprintf(stderr, "Final cost: %f\n", pam_total_cost(p));
  }

  gsl_vector_free(cost);
  gsl_vector_ulong_free(cl_index);
  gsl_vector_free(cl_dist);
  free(medoids);
  free(trimmed);
}

/* Partition around medoids. Returns the indices of the medoids. */
size_t * pam(gsl_matrix * distances, size_t k, /*OUT*/ double * dist)
{
  size_t * result = malloc(sizeof(size_t) * k);
  size_t i = 0, j = 0;
  pam_partition p;

  assert(k <= distances->size1);

  p = pam_partition_init(distances, k);
  pam_run(p, 100000);

  *dist = pam_total_cost(p);

  for (i = 0; i < p->in_set->size; i++) {
    if (gsl_vector_uchar_get(p->in_set, i))
      result[j++] = i;
  }
  assert(j == k);

  pam_partition_free(p);

  return result;
}

#ifdef PAM_TEST
int main(int argc, char **argv)
{
  gsl_matrix *m;
  FILE *f;
  size_t *result;
  double work;
  int rows, cols, keep, i;
  time_t start, end;

  if (argc != 3) {
    fprintf(stderr, "USAGE: %s dist_matrix keep_rows\n\n", argv[0]);
    fprintf(stderr, "dist_matrix should have one line with 'n_rows n_cols' ");
    fprintf(stderr, "followed by the matrix.\n");
    return 1;
  }

  keep = atoi(argv[2]);

  f = fopen(argv[1], "r");
  /* Read dimensions: width then height */
  assert(fscanf(f, "%d %d", &rows, &cols) == 2);
  m = gsl_matrix_alloc(rows, cols);
  assert(!gsl_matrix_fscanf(f, m));
  fclose(f);

  time(&start);
  result = pam(m, keep, &work);
  time(&end);

  for(i = 0; i < keep; i++)
    printf("%lu ", result[i]);
  printf("\n");
  printf("Total work: %f\n", work);
  printf("Took: %.2lfs\n", difftime(end, start));

  gsl_matrix_free(m);
  free(result);

  return 0;
}
#endif
