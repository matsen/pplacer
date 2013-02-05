/* http://www.gnu.org/software/gsl/manual/html_node/Nonlinear-Least_002dSquares-Fitting.html */

#include "lcfit_pair_c.h"

#include <stdlib.h>
#include <gsl/gsl_vector.h>
#include <gsl/gsl_blas.h>
#include <gsl/gsl_multifit_nlin.h>
#include <gsl/gsl_roots.h>

/** \brief Default initial conditions */
const bsm_t DEFAULT_INIT = {1500.0, 1000.0, 1.0, 0.5};

double lcfit_bsm_log_like(const double t, const bsm_t* m)
{
    double expterm = exp(-m->r * (t + m->b));
    return (m->c * log((1 + expterm) / 2) + m->m * log((1 - expterm) / 2));
}

/* The ML branch length for c, m, r, b */
double lcfit_bsm_ml_t(const bsm_t* m)
{
    double t = ((log((m->c - m->m) / (m->c + m->m))) / (-m->r)) - m->b;
    /* std::cout << "lcfit:" << ll(t, c, m, r) << "\n"; */
    return t;
}

/*
 * The scaling parameter for c and m to obtain log-likelihood value `l` at branch length `t`,
 * keeping `r` and `b` fixed.
 */
double lcfit_bsm_scale_factor(const double t, const double l, const bsm_t* m)
{
    double result = l / lcfit_bsm_log_like(t, m);
    return result;
}

/* Next, the data to fit such a log likelihood function. */
struct data_to_fit {
    const size_t n;   /* Number of observations */
    const double* t;  /* Branch lengths */
    const double* l;  /* Corresponding likelihoods */
};

/* Evaluate the likelihood curve described in data at the point x. */
int expb_f(const gsl_vector* x, void* data, gsl_vector* f)
{
    const size_t n = ((struct data_to_fit*) data)->n;
    const double* t = ((struct data_to_fit*) data)->t;
    const double* l = ((struct data_to_fit*) data)->l;

    bsm_t m = {gsl_vector_get(x, 0),
               gsl_vector_get(x, 1),
               gsl_vector_get(x, 2),
               gsl_vector_get(x, 3)};

    size_t i;

    for(i = 0; i < n; i++) {
        gsl_vector_set(f, i, lcfit_bsm_log_like(t[i], &m) - l[i]);
    }

    return GSL_SUCCESS;
}

/* The corresponding Jacobian. */
int expb_df(const gsl_vector* x, void* data, gsl_matrix* J)
{
    const size_t n = ((struct data_to_fit*) data)->n;
    const double* t = ((struct data_to_fit*) data)->t;
    /* double *l = ((struct data_to_fit *) data)->l; */

    double c = gsl_vector_get(x, 0);
    double m = gsl_vector_get(x, 1);
    double r = gsl_vector_get(x, 2);
    double b = gsl_vector_get(x, 3);

    size_t i;
    double expterm;

    for(i = 0; i < n; i++) {
        /* nx4 Jacobian matrix J(i,j) = dfi / dxj, */
        /* where fi = c*log((1+exp(-r*t[i]))/2)+m*log((1-exp(-r*t[i]))/2) - l[i] */
        /* so df/dc = log((1+exp(-r*(t+b)))/2) */
        /* so df/dm = log((1-exp(-r*(t+b)))/2) */
        /* so df/dr = c*(-(t+b))*exp(-r*(t+b))/(1+exp(-r*(t+b)))+m*(t+b)*exp(-r*(t+b))/(1-exp(-r*(t+b))) */
        /* so df/db = c*(-r)*exp(-r*(t+b))/(1+exp(-r*(t+b)))+m*r*exp(-r*(t+b))/(1-exp(-r*(t+b))) */
        /* and the xj are the parameters (c, m, r, b) */

        expterm = exp(-r * (t[i] + b));
        gsl_matrix_set(J, i, 0, log((1 + expterm) / 2)); /* df/dc */
        gsl_matrix_set(J, i, 1, log((1 - expterm) / 2)); /* df/dm */
        gsl_matrix_set(J, i, 2, (t[i] + b) * (-c * expterm / (1 + expterm) + m * expterm / (1 - expterm))); /* df/dr */
        gsl_matrix_set(J, i, 3, r * (-c * expterm / (1 + expterm) + m * expterm / (1 - expterm))); /* df/db */
    }
    return GSL_SUCCESS;
}

int expb_fdf(const gsl_vector* x, void* data, gsl_vector* f, gsl_matrix* J)
{
    expb_f(x, data, f);
    expb_df(x, data, J);

    return GSL_SUCCESS;
}

void print_state(unsigned int iter, gsl_multifit_fdfsolver* s)
{
    printf("iter: %3u x = % 15.8f % 15.8f % 15.8f % 15.8f "
           "|f(x)| = %g\n",
           iter,
           gsl_vector_get(s->x, 0),
           gsl_vector_get(s->x, 1),
           gsl_vector_get(s->x, 2),
           gsl_vector_get(s->x, 3),
           gsl_blas_dnrm2(s->f));
}

/*
     c = x[0];
     r = x[1];
     m = x[2];
     b = x[3];
*/
int lcfit_fit_bsm(const size_t n, const double* t, const double* l, bsm_t *m)
{
    const gsl_multifit_fdfsolver_type* T;
    gsl_multifit_fdfsolver* s;
    double x[4] = {m->c, m->m, m->r, m->b};

    int status;
    unsigned int iter = 0;

    struct data_to_fit d = {n, t, l};
    gsl_multifit_function_fdf fdf;

    /* Storing the contents of x on the stack.
     * http://www.gnu.org/software/gsl/manual/html_node/Vector-views.html */
    gsl_vector_const_view x_view = gsl_vector_const_view_array(x, 4);

    fdf.f = &expb_f;
    fdf.df = &expb_df;
    fdf.fdf = &expb_fdf;
    fdf.n = n;
    fdf.p = 4; /* 4 parameters */
    fdf.params = &d;

    T = gsl_multifit_fdfsolver_lmsder;
    s = gsl_multifit_fdfsolver_alloc(T, n, 4);
    gsl_multifit_fdfsolver_set(s, &fdf, &x_view.vector); /* Taking address of view.vector gives a const gsl_vector * */

    do {
        iter++;
        status = gsl_multifit_fdfsolver_iterate(s);

#ifdef VERBOSE
        printf("status = %s\n", gsl_strerror(status));
        print_state(iter, s);
#endif /* VERBOSE */

        if(status)
            break;

        status = gsl_multifit_test_delta(s->dx, s->x, 1e-4, 1e-4);
    } while(status == GSL_CONTINUE && iter < 500);

#define FIT(i) gsl_vector_get(s->x, i)
#define ERR(i) sqrt(gsl_matrix_get(covar,i,i))
#ifdef VERBOSE
    gsl_matrix* covar = gsl_matrix_alloc(4, 4);
    gsl_multifit_covar(s->J, 0.0, covar);
    gsl_matrix_fprintf(stdout, covar, "%g");
    gsl_matrix_free(covar);

    printf("c = %.5f +/- %.5f\n", FIT(0), ERR(0));
    printf("m = %.5f +/- %.5f\n", FIT(1), ERR(1));
    printf("r = %.5f +/- %.5f\n", FIT(2), ERR(2));
    printf("r = %.5f +/- %.5f\n", FIT(3), ERR(3));

    printf("status = %s\n", gsl_strerror(status));
#endif /* VERBOSE */

    // Update fit
    m->c = FIT(0);
    m->m = FIT(1);
    m->r = FIT(2);
    m->b = FIT(3);

    gsl_multifit_fdfsolver_free(s);
    return 0;
}
