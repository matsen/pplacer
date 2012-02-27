/* cddlib-based solution pruning for `rppr voronoi` */

#include "setoper.h"
#include "cdd.h"
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <math.h>
#include <float.h>

#define EPS 1e-7

#ifdef PPLACER_CDD_TEST
/* Debugging functions */
static void dd_PrintMatrix(dd_Amatrix m, dd_rowrange rows, dd_colrange cols) {
  dd_rowrange i;
  dd_colrange j;
  for(i = 0; i < rows; i++) {
    for(j = 0; j < cols; j++) {
       printf("%e\t", *m[i][j]);
    }
    printf("\n");
  }
}

void dd_PrintMatrixPtr(dd_MatrixPtr m) {
  dd_PrintMatrix(m->matrix, m->rowsize, m->colsize);
}
#endif

/*
 * Create a dd_MatrixPtr, filled with values from init_vals.
 * length of init_vals must be rows*cols in row-major order
 *
 * This function adds a constraint: x > 0
 *
 * Caller must free returned matrix.
 */
static dd_MatrixPtr init_ineq_doubles(const double* init_vals, int rows, int cols) {
  /* Only 3-column inputs supported */
  assert(cols == 3);

  dd_MatrixPtr m = dd_CreateMatrix(rows+1, cols);
  dd_rowrange i;
  dd_colrange j;

  /* Fill values */
  for(i = 0; i < m->rowsize-1; i++) {
    for(j = 0; j < m->colsize; j++) {
      dd_set_d(m->matrix[i][j], init_vals[m->colsize*i+j]);
    }
  }

  /* Add a row constraining solution space x>=0 */
  dd_set_d(m->matrix[rows][0], 0); /* intercept */
  dd_set_d(m->matrix[rows][1], 1); /* coef of x */
  dd_set_d(m->matrix[rows][2], 0); /* coef of y */

  /* Mark the representation as inequalities */
  m->representation = dd_Inequality;

  /* Input types = reals */
  m->numbtype = dd_Real;
  return m;
}

static int is_vertex(const mytype i) {
  /* Vertices are 1, rays 0 */
  return *i > dd_almostzero;
}

/* Count the vertices in a matrix of external points */
static size_t count_vertices(const dd_MatrixPtr generators) {
  dd_rowrange i;
  int vertex_count = 0;
  for(i = 0; i < generators->rowsize; i++) {
    /* Check if row is vertex (first column 0) */
    if(is_vertex(generators->matrix[i][0]))
      vertex_count++;
  }

  return vertex_count;
}

/* Returns first item in s. s must be non-empty. */
static long set_first(const set_type s) {
  long elem;

  for(elem = 1; elem <= s[0]; elem++) {
    if(set_member(elem, s))  {
      return elem;
    }
  }
  assert(0); /* empty set */
}

/*
 * Functions for sorting generators.
 *
 * All assume a matrix with at least 2 columns.
 */
typedef struct {
  size_t index;
  dd_Arow row;
} SortRow;

/*
 * Comparison function for qsort. Sorts by first column (desc), then second
 * (asc).
 */
static int generator_row_cmp(const void *a, const void*b) {
  const SortRow *ia = (const SortRow *)a;
  const SortRow *ib = (const SortRow *)b;

  double da, db;
  da = *(ia->row[0]);
  db = *(ib->row[0]);

  /* Compare first column - sort descending */
  if(db - da < 0.0) return -1; /* da > db */
  if(db - da > 0.0) return 1;  /* da < db */

  /* Sort by second column - ascending */
  da = *(ia->row[1]);
  db = *(ib->row[1]);
  if(da - db < 0.0) return -1;
  if(da - db > 0.0) return 1;

  /* Tie */
  return 0;
}


/*
 * Returns ordered indices of rows in m, ordered first by m col 0 (desc), then
 * col 1 (asc)
 * This gives vertices before rays, ordered by point on x axis.
 */
static size_t* sort_generators(dd_MatrixPtr m) {
  dd_rowrange i;
  /* Create SortRows for performing ordering */
  SortRow* rows = (SortRow*)malloc(sizeof(SortRow)*m->rowsize);
  for(i = 0; i < m->rowsize; i++) {
    rows[i].index = i;
    rows[i].row = m->matrix[i];
  }

  /* Sort */
  qsort(rows, m->rowsize, sizeof(SortRow), generator_row_cmp);

  /* Extract sorted indices */
  size_t* result = (size_t*)malloc(sizeof(size_t)*m->rowsize);
  for(i = 0; i < m->rowsize; i++)
    result[i] = rows[i].index;

  free(rows);

  return result;
}

/*
 * Parse the generator output. Find the generators which are vertices
 * (non-rays) add their corresponding inequalities to an output set.
 *
 * Returns a matrix where each row contains the x-coordinate of the vertex,
 * y-coordinate of the vertex, and 0-based index of the inequality which bounds
 * the polytope to the right of the vertex. output_size is set to the total
 * length of the ouptut.
 */
static double* list_extreme_vertices(const dd_MatrixPtr generators,
    const dd_SetFamilyPtr incidence, const size_t nrows, long added_index,
    /*OUT*/size_t* output_size) {
  assert(generators->rowsize > 0);
  assert(generators->colsize > 2);

  /* At most, all nrows are included in the hull */
  set_type cur_vert_set, next_vert_set, prev_vert_set, s;
  dd_rowrange i;
  long elem;
  size_t out_row = 0, r;

  /*
   * Determine output size by counting vertices (and omitting rays)
   *
   * If The polytope has only one vertex, and that vertex is the origin, no
   * vertices are returned - only rays.
   */
  size_t vertex_count = count_vertices(generators);
  *output_size = !vertex_count ? 3 : 3*vertex_count;
  double* output = (double*)malloc(sizeof(double)*(*output_size));
  size_t* indices = sort_generators(generators);

  assert(*output_size > 0);

  /*
   * One special case:
   * cddlib doesn't report solution vertices containing only the origin.
   * Solutions with the origin and additional points are reported.
   */
  if(!vertex_count) { /* Origin-only solution = no vertices */
    assert(generators->rowsize == 2);
    /* Find the first ray without added_index incident - this is the ray to the
       right of the origin. */
    for(i = 0; i < generators->rowsize; i++) {
      r = indices[i];
      cur_vert_set = incidence->set[r];

      if(set_member(added_index, cur_vert_set)) /* Ray has added_index incident */
        continue;

      assert(set_card(cur_vert_set) == 1);

      set_initialize(&s, cur_vert_set[0]);
      set_copy(s, cur_vert_set);
      set_delelem(s, added_index);
      assert(set_card(s) == 1);

      output[0] = 0.0;                        /* x */
      output[1] = 0.0;                        /* y */
      output[2] = (double)(set_first(s) - 1); /* inequality index */
      break;
    }
  } else { /* 1+ vertices */
    for(i = 0; i < generators->rowsize; i++) {
      r = indices[i];
      if(is_vertex(generators->matrix[r][0])) {
        assert(out_row < vertex_count);
        cur_vert_set = incidence->set[r];

        if(vertex_count == 1 && i == 0) {
          /* First and only vertex. Result is just: */
          /*    current_set \ {added_index} */
          /* added_index is removed below. */
          set_initialize(&s, cur_vert_set[0]);
          set_copy(s, cur_vert_set);
        } else if(i < vertex_count - 1) { /* Not the last vertex */
          next_vert_set = incidence->set[indices[i+1]];

          /* Sets should be same size */
          assert(cur_vert_set[0] == next_vert_set[0]);
          set_initialize(&s, cur_vert_set[0]);
          set_int(s, cur_vert_set, next_vert_set);
        } else { /* Last vertex */
          /* Previous set instead of next */
          assert(i);
          prev_vert_set = incidence->set[indices[i-1]];

          /* Sets should be same size */
          assert(cur_vert_set[0] == prev_vert_set[0]);
          set_initialize(&s, cur_vert_set[0]);

          /* Diff, instead of intersection */
          set_diff(s, cur_vert_set, prev_vert_set);
        }

        /* Remove added index for first item */
        if(!i) set_delelem(s, added_index);

        if(set_card(s) == 0) {
          set_write(cur_vert_set);
        }

        assert(set_card(s) == 1);

        /* Only one item in the set */
        elem = set_first(s);
        set_free(s);

        /* Fill output row */
        int base = out_row * 3;
        output[base] = *generators->matrix[r][1];   /* x */
        output[base+1] = *generators->matrix[r][2]; /* y */
        output[base+2] = (double)(elem - 1);        /* ineq index, converted to 0-base */

        out_row++;
      } else {
        assert(out_row == vertex_count);
      }
    }
  }

  free(indices);

  return output;
}


/*
 * Find the extreme vertices of the convex hull of ineqs.
 *
 * ineqs: row-major order matrix of total length row*cols
 *
 * Returns array representing 3 by (output_size/3) matrix, where each row is:
 *   x_i, y_i, l_i
 * Where x_i, y_i are the coordinates of the vertex, and l_i is the index of
 * the input inequality that constrains the solution space *to the right* of
 * vertex i.
 */
double* extreme_vertices(const double* ineqs, const size_t nrows, const size_t cols,
    /*OUT*/ size_t* output_size) {

  /* Preconditions */
  assert(ineqs != NULL);
  assert(cols == 3);
  assert(output_size != NULL);

  /*
   * Initialize library
   * TODO: Do we want to do this on every call?
   */
  dd_set_global_constants();
  dd_ErrorType err;
  dd_MatrixPtr generators;
  dd_MatrixPtr m = init_ineq_doubles(ineqs, nrows, cols);
  dd_SetFamilyPtr incidence;

  /* Outputs */
  dd_PolyhedraPtr poly = dd_DDMatrix2Poly(m, &err);
  if(err != dd_NoError) {
    return NULL;
  }

  /* Get generators */
  generators = dd_CopyGenerators(poly);

  /* Get incidence */
  incidence = dd_CopyIncidence(poly);

  double* result = list_extreme_vertices(generators, incidence, nrows, m->rowsize, output_size);

  dd_FreeMatrix(m);
  dd_FreeMatrix(generators);
  dd_FreePolyhedra(poly);
  dd_FreeSetFamily(incidence);
  dd_free_global_constants();

  return result;
}


#ifdef PPLACER_CDD_TEST
/*
 * Helper function to generate a matrix from init_vals
 */
static dd_MatrixPtr init_matrix(const double* init_vals, int rows, int cols) {
  dd_MatrixPtr m = dd_CreateMatrix(rows, cols);
  dd_rowrange i;
  dd_colrange j;

  /* Fill values */
  for(i = 0; i < m->rowsize; i++) {
    for(j = 0; j < m->colsize; j++) {
      dd_set_d(m->matrix[i][j], init_vals[m->colsize*i+j]);
    }
  }
  return m;
}

static void test_sort_generators() {
  const double input[] = {
    1.0, 1.0, 2.0,
    1.0, 0.0, 0.0,
    0.0, 0.0, -1.0,
    0.0, 0.2, 1.0,
    1.0, 2.0, 3.0 };

  const int rows = 5;
  const int cols = 3;

  dd_MatrixPtr m = init_matrix(input, rows, cols);
  size_t* sorted_indices = sort_generators(m);
  dd_FreeMatrix(m);
  size_t expected[] = {1, 0, 4, 2, 3};
  int i;
  for(i = 0; i < rows; i++) {
    assert(sorted_indices[i] == expected[i]);
  }

  free(sorted_indices);
}

/* Test data */
const double INPUT_MATRIX[] = {
  0.0, 2.0, -1.0 ,
  1.0, 1.0, -1.0 ,
  2.0, 0.5, -1.0 ,
  15.0, 1.0, -1.0,
  16.0, 1.0, -1.0,
  17.0, 1.0, -1.0 };

const int ROWS = 6;
const int COLS = 3;

/* Run a simple test */
int main(int argc, char* argv[]) {
  /* Init cdd */
  size_t result_size;
  double* result = extreme_vertices(INPUT_MATRIX, ROWS, COLS, &result_size);
  assert(result_size == 9);

  assert(abs(result[0]) < EPS);
  assert(abs(result[1]) < EPS);
  assert(abs(result[2]) < EPS);

  assert(result[3] == 1.0);
  assert(result[4] == 2.0);
  assert(result[5] == 1.0);

  assert(result[6] == 2.0);
  assert(result[7] == 3.0);
  assert(result[8] == 2.0);

  test_sort_generators();

  free(result);
  return 0;
}
#endif
