#include "lcfit_tripod_c.h"
#include <assert.h>
#include <math.h>
#include <stdlib.h>
#include <string.h>

#include <gsl/gsl_blas.h>
#include <gsl/gsl_multifit_nlin.h>
#include <gsl/gsl_roots.h>
#include <gsl/gsl_vector.h>

/**
 * \file lcfit_tripod_c.c
 *
 * PPLACER SETUP:
 *   In pplacer-land, we have one fixed branch length t between y and z.
 *   dist_bl is the amount of branch length between z and the attachment of the pendant branch length x.
 *   pend_bl is x's pendant branch length.
 *   We assume that the fixed branch have a shared rate r and offset b.
 *
 * \verbatim
 *             y
 *             |
 *   t-dist_bl |  pend_bl
 *             |---------- x
 *     dist_bl |
 *             |
 *             z
 * \endverbatim
 *
 * \c y taken as the 'reference' sequence, with all sites being in state \c 0.
 * The model is them parameterized by:
 *  * \c n00     Number of sites constant in x, y, and z
 *  * \c n01     Number of sites constant in y, z, mutated in x
 *  * \c n10     Number of sites constant in y, z, mutated in x
 *  * \c n11     Number of sites different from y at both x and z
 *  * \c r       Rate
 *  * \c b       Branch length offset
 *  * \c dist_bl Distal branch length
 *  * \c t       y-z distance
 *  * \c rx      Rate on pendant branch
 *  * \c bx      Branch length offset on pendant branch
 *  * \c pend_bl Pendant branch length
 */

/** Number of parameters in the BSM */
const size_t TRIPOD_BSM_NPARAM = 9;
/** Number of *varying* parameters in the BSM - one less, since t is fixed */
const size_t TRIPOD_BSM_NVARYING = 8;

/* Some utilities */
double*
array_of_tripod_bsm(const tripod_bsm_t* m)
{
    assert(m != NULL);
    double* result = malloc(sizeof(double) * TRIPOD_BSM_NPARAM);
    result[0] = m->n00;
    result[1] = m->n01;
    result[2] = m->n10;
    result[3] = m->n11;
    result[4] = m->r;
    result[5] = m->b;
    result[6] = m->t;
    result[7] = m->rx;
    result[8] = m->bx;
    return result;
}

void
vector_of_tripod_bsm(const tripod_bsm_t* m, gsl_vector* v)
{
    assert(v != NULL);
    assert(m != NULL);
    assert(v->size == TRIPOD_BSM_NVARYING && "unexpected vector size");
    gsl_vector_set(v, 0, m->n00);
    gsl_vector_set(v, 1, m->n01);
    gsl_vector_set(v, 2, m->n10);
    gsl_vector_set(v, 3, m->n11);
    gsl_vector_set(v, 4, m->r);
    gsl_vector_set(v, 5, m->b);
    gsl_vector_set(v, 6, m->rx);
    gsl_vector_set(v, 7, m->bx);
}

void
tripod_bsm_of_vector(const gsl_vector *v, const double t, tripod_bsm_t* m)
{
    assert(v != NULL);
    assert(m != NULL);
    assert(v->size == TRIPOD_BSM_NVARYING && "unexpected vector size");
    m->n00 = gsl_vector_get(v, 0);
    m->n01 = gsl_vector_get(v, 1);
    m->n10 = gsl_vector_get(v, 2);
    m->n11 = gsl_vector_get(v, 3);
    m->r   = gsl_vector_get(v, 4);
    m->b   = gsl_vector_get(v, 5);
    m->t   = t;
    m->rx  = gsl_vector_get(v, 6);
    m->bx  = gsl_vector_get(v, 7);
}

void
lcfit_tripod_bsm_fprintf(FILE* out, const tripod_bsm_t* m)
{
    fprintf(out, "{n00=%f,n01=%f,n10=%f,n11=%f,r=%f,b=%f,t=%f,rx=%f,bx=%f}\n",
            m->n00,
            m->n01,
            m->n10,
            m->n11,
            m->r,
            m->b,
            m->t,
            m->rx,
            m->bx);
}

/**
 * Rescale model \c m to intersect with <c>(dist_bl, pend_bl, ll)</c>
 *
 * \param dist_bl distal branch length
 * \param pend_bl pendant branch length
 * \param ll Log-likelihood at (dist_bl, pend_bl)
 * \param m Current model. <b>Model is rescaled</b>
 * \return The scaling factor
 */
double
lcfit_tripod_rescale(const double dist_bl, const double pend_bl, const double ll, tripod_bsm_t* m)
{
    double scale_factor = ll / lcfit_tripod_ll(dist_bl, pend_bl, m);
    m->n00 *= scale_factor;
    m->n01 *= scale_factor;
    m->n10 *= scale_factor;
    m->n11 *= scale_factor;
    return scale_factor;
}

/**
 * Calculate the log-likelihood of a tripod
 *
 * \param dist_bl  Distal branch length
 * \param pend_bl  Pendant branch length
 * \param m  Model
 * \return Log-likelihood under \c m
 */
double
lcfit_tripod_ll(const double dist_bl, const double pend_bl, const tripod_bsm_t* m)
{
    static const int I0_0 = 1;
    static const double R0_30 = 0.5;
    static const double R0_31 = 0.125;
    double R0_0;
    double R0_1;
    double R0_2;
    double R0_3;
    double R0_4;
    double R0_5;
    double R0_6;
    double R0_7;
    double R0_8;
    double R0_9;
    double R0_10;
    double R0_11;
    double R0_12;
    double R0_13;
    double R0_14;
    double R0_15;
    double R0_16;
    double R0_17;
    double R0_18;
    double R0_19;
    double R0_20;
    double R0_21;
    double R0_22;
    double R0_23;
    double R0_24;
    double R0_25;
    double R0_26;
    double R0_27;
    double R0_28;
    double R0_29;
    double R0_32;
    double R0_33;
    double R0_34;
    double R0_35;
    double R0_36;
    R0_0 = m->n00;
    R0_1 = m->n01;
    R0_2 = m->n10;
    R0_3 = m->n11;
    R0_4 = m->r;
    R0_5 = m->b;
    R0_6 = dist_bl;
    R0_7 = m->t;
    R0_8 = m->rx;
    R0_9 = m->bx;
    R0_10 = pend_bl;
    R0_11 = R0_5 + R0_6;
    R0_12 = R0_11 * R0_4;
    R0_13 = -R0_12;
    R0_12 = exp(R0_13);
    R0_14 = -R0_6;
    R0_15 = R0_5 + R0_14 + R0_7;
    R0_16 = R0_4 * R0_15;
    R0_17 = -R0_16;
    R0_16 = exp(R0_17);
    R0_18 = R0_9 + R0_10;
    R0_19 = R0_8 * R0_18;
    R0_20 = -R0_19;
    R0_19 = exp(R0_20);
    R0_21 = -R0_12;
    R0_22 = (double) I0_0;
    R0_22 = R0_22 + R0_21;
    R0_23 = (double) I0_0;
    R0_23 = R0_23 + R0_16;
    R0_24 = -R0_19;
    R0_25 = (double) I0_0;
    R0_25 = R0_25 + R0_24;
    R0_26 = (double) I0_0;
    R0_26 = R0_26 + R0_12;
    R0_27 = -R0_16;
    R0_28 = (double) I0_0;
    R0_28 = R0_28 + R0_27;
    R0_29 = (double) I0_0;
    R0_29 = R0_29 + R0_19;
    R0_32 = R0_31 * R0_26 * R0_23 * R0_25;
    R0_33 = R0_31 * R0_22 * R0_28 * R0_29;
    R0_32 = R0_32 + R0_33;
    R0_33 = R0_30 * R0_32;
    R0_32 = log(R0_33);
    R0_33 = R0_3 * R0_32;
    R0_32 = R0_31 * R0_22 * R0_23 * R0_25;
    R0_34 = R0_31 * R0_26 * R0_28 * R0_29;
    R0_32 = R0_32 + R0_34;
    R0_34 = R0_30 * R0_32;
    R0_32 = log(R0_34);
    R0_34 = R0_1 * R0_32;
    R0_32 = R0_31 * R0_26 * R0_28 * R0_25;
    R0_35 = R0_31 * R0_22 * R0_23 * R0_29;
    R0_32 = R0_32 + R0_35;
    R0_35 = R0_30 * R0_32;
    R0_32 = log(R0_35);
    R0_35 = R0_2 * R0_32;
    R0_32 = R0_31 * R0_22 * R0_28 * R0_25;
    R0_36 = R0_31 * R0_26 * R0_23 * R0_29;
    R0_32 = R0_32 + R0_36;
    R0_36 = R0_30 * R0_32;
    R0_32 = log(R0_36);
    R0_36 = R0_0 * R0_32;
    R0_33 = R0_33 + R0_34 + R0_35 + R0_36;
    return R0_33;
}

/**
 *
 * Calculate the jacobian of a tripod
 * \param dist_bl  Distal branch length
 * \param pend_bl Pendant branch length
 * \param m  Model
 * \return The jacobian, length 8
 */
double*
lcfit_tripod_jacobian(const double dist_bl, const double pend_bl, const tripod_bsm_t* m)
{
    static const int I0_0 = -2;
    static const int I0_1 = 2;
    static const double R0_63 = 0.125;
    double R0_0;
    double R0_1;
    double R0_2;
    double R0_3;
    double R0_4;
    double R0_5;
    double R0_6;
    double R0_7;
    double R0_8;
    double R0_9;
    double R0_10;
    double R0_11;
    double R0_12;
    double R0_13;
    double R0_14;
    double R0_15;
    double R0_16;
    double R0_17;
    double R0_18;
    double R0_19;
    double R0_20;
    double R0_21;
    double R0_22;
    double R0_23;
    double R0_24;
    double R0_25;
    double R0_26;
    double R0_27;
    double R0_28;
    double R0_29;
    double R0_30;
    double R0_31;
    double R0_32;
    double R0_33;
    double R0_34;
    double R0_35;
    double R0_36;
    double R0_37;
    double R0_38;
    double R0_39;
    double R0_40;
    double R0_41;
    double R0_42;
    double R0_43;
    double R0_44;
    double R0_45;
    double R0_46;
    double R0_47;
    double R0_48;
    double R0_49;
    double R0_50;
    double R0_51;
    double R0_52;
    double R0_53;
    double R0_54;
    double R0_55;
    double R0_56;
    double R0_57;
    double R0_58;
    double R0_59;
    double R0_60;
    double R0_61;
    double R0_62;
    double R0_64;
    double R0_65;
    double R0_66;
    double R0_67;
    double R0_68;
    double R0_69;
    double R0_70;
    double R0_71;
    double R0_72;
    double R0_73;
    R0_0 = m->n00;
    R0_1 = m->n01;
    R0_2 = m->n10;
    R0_3 = m->n11;
    R0_4 = m->r;
    R0_5 = m->b;
    R0_6 = dist_bl;
    R0_7 = m->t;
    R0_8 = m->rx;
    R0_9 = m->bx;
    R0_10 = pend_bl;
    R0_11 = (double) I0_0;
    R0_11 = R0_11 * R0_5 * R0_4;
    R0_12 = R0_9 * R0_8;
    R0_13 = -R0_12;
    R0_12 = R0_4 * R0_7;
    R0_14 = -R0_12;
    R0_12 = R0_8 * R0_10;
    R0_15 = -R0_12;
    R0_12 = R0_11 + R0_13 + R0_14 + R0_15;
    R0_16 = exp(R0_12);
    R0_17 = R0_5 + R0_6;
    R0_18 = R0_17 * R0_4;
    R0_19 = exp(R0_18);
    R0_20 = -R0_6;
    R0_21 = R0_5 + R0_20 + R0_7;
    R0_22 = R0_4 * R0_21;
    R0_23 = exp(R0_22);
    R0_24 = R0_9 + R0_10;
    R0_25 = R0_8 * R0_24;
    R0_26 = exp(R0_25);
    R0_27 = (double) I0_1;
    R0_27 = R0_27 * R0_5 * R0_4;
    R0_28 = R0_9 * R0_8;
    R0_29 = R0_4 * R0_7;
    R0_30 = R0_8 * R0_10;
    R0_31 = R0_27 + R0_28 + R0_29 + R0_30;
    R0_32 = exp(R0_31);
    R0_33 = -R0_26;
    R0_34 = -R0_19;
    R0_35 = -R0_23;
    R0_36 = R0_34 + R0_35 + R0_26 + R0_32;
    R0_37 = R0_19 + R0_35 + R0_33 + R0_32;
    R0_38 = R0_34 + R0_23 + R0_33 + R0_32;
    R0_39 = R0_19 + R0_23;
    R0_40 = (double) I0_1;
    R0_40 = R0_40 * R0_26;
    R0_41 = R0_19 + R0_23 + R0_26 + R0_32;
    R0_42 = R0_34 + R0_23;
    R0_43 = R0_6 * R0_42;
    R0_44 = R0_19 + R0_26;
    R0_45 = R0_44 * R0_7;
    R0_46 = R0_19 + R0_23 + R0_40;
    R0_47 = 1 / R0_41;
    R0_48 = R0_19 + R0_35 + R0_40;
    R0_49 = 1 / R0_38;
    R0_50 = R0_34 + R0_23 + R0_40;
    R0_51 = 1 / R0_37;
    R0_52 = (double) I0_0;
    R0_52 = R0_52 * R0_26;
    R0_53 = R0_19 + R0_23 + R0_52;
    R0_54 = 1 / R0_36;
    R0_55 = R0_34 + R0_26;
    R0_56 = R0_19 + R0_33;
    R0_57 = R0_39 * R0_47 * R0_0;
    R0_58 = -R0_57;
    R0_57 = R0_19 + R0_35;
    R0_59 = R0_57 * R0_49 * R0_1;
    R0_60 = R0_42 * R0_51 * R0_2;
    R0_61 = R0_39 * R0_54 * R0_3;
    R0_62 = R0_58 + R0_59 + R0_60 + R0_61;
    R0_64 = R0_63 * R0_16 * R0_41;
    R0_65 = log(R0_64);
    R0_64 = R0_63 * R0_16 * R0_38;
    R0_66 = log(R0_64);
    R0_64 = R0_63 * R0_16 * R0_37;
    R0_67 = log(R0_64);
    R0_64 = R0_63 * R0_16 * R0_36;
    R0_68 = log(R0_64);
    R0_64 = R0_5 * R0_53;
    R0_69 = R0_56 * R0_7;
    R0_70 = R0_43 + R0_64 + R0_69;
    R0_64 = R0_54 * R0_3 * R0_70;
    R0_70 = R0_6 * R0_39;
    R0_69 = R0_5 * R0_50;
    R0_71 = R0_55 * R0_7;
    R0_70 = R0_70 + R0_69 + R0_71;
    R0_69 = R0_51 * R0_2 * R0_70;
    R0_70 = R0_6 * R0_39;
    R0_71 = -R0_70;
    R0_70 = R0_5 * R0_48;
    R0_71 = R0_71 + R0_70 + R0_45;
    R0_70 = R0_49 * R0_1 * R0_71;
    R0_71 = R0_5 * R0_46;
    R0_72 = R0_43 + R0_71 + R0_45;
    R0_71 = R0_47 * R0_0 * R0_72;
    R0_72 = -R0_71;
    R0_64 = R0_64 + R0_69 + R0_70 + R0_72;
    R0_69 = R0_46 * R0_47 * R0_0;
    R0_70 = -R0_69;
    R0_69 = R0_48 * R0_49 * R0_1;
    R0_72 = R0_50 * R0_51 * R0_2;
    R0_71 = R0_53 * R0_54 * R0_3;
    R0_70 = R0_70 + R0_69 + R0_72 + R0_71;
    R0_70 = R0_70 * R0_4;
    R0_69 = R0_44 * R0_47 * R0_0;
    R0_72 = -R0_69;
    R0_69 = R0_44 * R0_49 * R0_1;
    R0_71 = R0_55 * R0_51 * R0_2;
    R0_73 = R0_56 * R0_54 * R0_3;
    R0_72 = R0_72 + R0_69 + R0_71 + R0_73;
    R0_72 = R0_72 * R0_4;
    R0_69 = R0_62 * R0_24;
    R0_71 = R0_62 * R0_8;

    double *result = malloc(sizeof(double) * TRIPOD_BSM_NVARYING);
    assert(result != NULL && "failed malloc");
    result[0] = R0_65;
    result[1] = R0_66;
    result[2] = R0_67;
    result[3] = R0_68;
    result[4] = R0_64;
    result[5] = R0_70;
    result[6] = R0_69;
    result[7] = R0_71;

    /* result[6] is df/dt; t is constant */
    /*result[6] = R0_72;*/
    /*result[7] = R0_69;*/
    /*result[8] = R0_71;*/
    return result;
}

/*************************/
/* GSL fitting machinery */
/*************************/
typedef struct {
    const size_t n;
    const double* dist_bl;
    const double* pend_bl;
    const double* l;
    const double t;
} tripod_data_to_fit;

/** Compute residuals */
int
lcfit_tripod_f(const gsl_vector* x, void* data, gsl_vector* f)
{
    tripod_bsm_t* m = malloc(sizeof(tripod_bsm_t));
    assert(m != NULL && "failed malloc");
    const tripod_data_to_fit* d = (tripod_data_to_fit*)data;
    tripod_bsm_of_vector(x, d->t, m);
    const double* dist_bl = d->dist_bl;
    const double* pend_bl = d->pend_bl;
    const double* l = d->l;
    size_t i;

    for(i = 0; i < d->n; i++) {
        const double residual = lcfit_tripod_ll(dist_bl[i], pend_bl[i], m) - l[i];
        gsl_vector_set(f, i, residual);
    }

    free(m);

    return GSL_SUCCESS;
}

/** Evaluate the Jacobian */
int
lcfit_tripod_df(const gsl_vector* x, void* data, gsl_matrix* J)
{
    tripod_bsm_t* m = malloc(sizeof(tripod_bsm_t));
    assert(m != NULL && "failed malloc");
    const tripod_data_to_fit* d = (tripod_data_to_fit*)data;
    tripod_bsm_of_vector(x, d->t, m);
    const double* dist_bl = d->dist_bl;
    const double* pend_bl = d->pend_bl;

    size_t i;

    double* dest = J->data;
    for(i = 0; i < d->n; i++) {
        double* jac = lcfit_tripod_jacobian(dist_bl[i], pend_bl[i], m);
        memcpy(dest, jac, sizeof(double) * TRIPOD_BSM_NVARYING);
        free(jac);
        dest += TRIPOD_BSM_NVARYING;
    }

    free(m);

    return GSL_SUCCESS;
}

/** Residuals + Jacobian */
int
lcfit_tripod_fdf(const gsl_vector* x, void* data, gsl_vector* f, gsl_matrix* J)
{
    lcfit_tripod_f(x, data, f);
    lcfit_tripod_df(x, data, J);

    return GSL_SUCCESS;
}

/*****************************/
/* End GSL fitting machinery */
/*****************************/



/** \brief Fit the BSM
 *
 *  \param n_pts    Number of points in \c dist_bl \c pend_bl, and \c l
 *  \param dist_bl  Distal branch lengths sampled
 *  \param pend_bl  Pendant branch lengths sampled
 *  \param l        Log-likelihoods evaluated at the (c, pend_bl) points provided
 *  \param model    <i>(modified)</i> Starting conditions for the model; updated with
 *                  fit parameters.
 *  \param tol      Model-fitting tolerance
 *  \param max_iter Maximum number of iterations to run
 *  \returns GSL status code; GSL_SUCCESS on success.
 */
int
lcfit_tripod_fit_bsm(const size_t n_pts,
                     const double* dist_bl,
                     const double* pend_bl,
                     const double* l,
                     tripod_bsm_t* model,
                     const double tol,
                     const size_t max_iter)
{
    tripod_data_to_fit d = {n_pts, dist_bl, pend_bl, l, model->t};

    gsl_multifit_function_fdf fdf;

    gsl_vector* mvec = gsl_vector_alloc(TRIPOD_BSM_NVARYING);
    vector_of_tripod_bsm(model, mvec);

    fdf.f = &lcfit_tripod_f;
    fdf.df = &lcfit_tripod_df;
    fdf.fdf = &lcfit_tripod_fdf;
    fdf.n = n_pts;
    fdf.p = TRIPOD_BSM_NVARYING;
    fdf.params = &d;

    const gsl_multifit_fdfsolver_type* T = gsl_multifit_fdfsolver_lmsder;
    gsl_multifit_fdfsolver* s = gsl_multifit_fdfsolver_alloc(T, n_pts, TRIPOD_BSM_NVARYING);
    gsl_multifit_fdfsolver_set(s, &fdf, mvec);

    /* Run the solver */
    size_t iter = 0;
    int status = GSL_CONTINUE;
    do {
        iter++;
        status = gsl_multifit_fdfsolver_iterate(s);

        if(status)
            break;

        status = gsl_multifit_test_delta(s->dx, s->x, tol, tol);
        /*status = gsl_multifit_test_gradient(s->dx, tol);*/
    } while(status == GSL_CONTINUE && iter < max_iter);

    /* Raise GSL_ENOPROG if reached maximum iteration count */
    if(iter == max_iter && !status)
        status = GSL_ENOPROG;

#ifdef LCFIT_DEBUG
    const double chi = gsl_blas_dnrm2(s->f),
                 dof = n_pts - TRIPOD_BSM_NVARYING;
    const double c = GSL_MAX_DBL(1, chi / sqrt(dof));
    fprintf(stderr, "chisq/dof = %g\n",  pow(chi, 2.0) / dof);
#endif

    /* Copy results into model */
    tripod_bsm_of_vector(s->x, model->t, model);

    gsl_vector_free(mvec);
    gsl_multifit_fdfsolver_free(s);

    return status;
}

/**
 * Calculate the coefficients of the mutation parameters of the model, n00, n01, n10, and n11.
 * That is:
 *
 * \f[
 *   L(M, d, p) = n00 c00(M, d, p) + n01 c01(M, d, p) + n10 c10(M, d, p) + n11 c11(M, d, p)
 * \f]
 *
 * \param m Model
 * \param dist_bl Distal branch length
 * \param pend_bl Pendant branch length
 * \param result Output - double vector with length 4. Will be filled with <c>{c00,c01,c10,c11}</c>
 */
void
lcfit_tripod_nxx_coeff(const tripod_bsm_t* m, const double dist_bl, const double pend_bl, double* result)
{
    const double e1 = exp(-(m->r*(m->b+dist_bl))),
                 e2 = exp(-(m->r*(m->b - dist_bl + m->t))),
                 e3 = exp(-(m->rx * (m->bx + pend_bl)));

    #define LCFIT_COEFF(s1, s2, s3, s4, s5, s6) \
            log(( \
                ((1 s1 e1) * (1 s2 e2) * (1 s3 e3))/8 + \
                ((1 s4 e1) * (1 s5 e2) * (1 s6 e3))/8) / 2)


/*
 *                      -((b + c) r)        -(r (b - c + t))        -(rx (bx + tx))
 *                (1 - E            ) (1 - E                ) (1 - E               )
 *Out[156]= {Log[(------------------------------------------------------------------ +
 *                                                8
 *
 *               -((b + c) r)        -(r (b - c + t))        -(rx (bx + tx))
 *         (1 + E            ) (1 + E                ) (1 + E               )
 *>        ------------------------------------------------------------------) / 2],
 *                                         8
 */

    /* n00 */
    result[0] = log((
            ((1 - e1) * (1 - e2) * (1 - e3))/8 +
            ((1 + e1) * (1 + e2) * (1 + e3))/8) / 2);

    assert(abs(result[0] - LCFIT_COEFF(-,-,-,+,+,+)) < 1e-5);

/*                -((b + c) r)        -(r (b - c + t))        -(rx (bx + tx))
 *          (1 - E            ) (1 + E                ) (1 - E               )
 *>    Log[(------------------------------------------------------------------ +
 *                                          8
 *
 *               -((b + c) r)        -(r (b - c + t))        -(rx (bx + tx))
 *         (1 + E            ) (1 - E                ) (1 + E               )
 *>        ------------------------------------------------------------------) / 2],
 *                                         8
 */
    /* n01 */
    result[1] = log((
            ((1 - e1) * (1 + e2) * (1 - e3))/8 +
            ((1 + e1) * (1 - e2) * (1 + e3))/8) / 2);

    assert(abs(result[1] - LCFIT_COEFF(-,+,-,+,-,+)) < 1e-5);
/*
 *                -((b + c) r)        -(r (b - c + t))        -(rx (bx + tx))
 *          (1 + E            ) (1 - E                ) (1 - E               )
 *>    Log[(------------------------------------------------------------------ +
 *                                          8
 *
 *               -((b + c) r)        -(r (b - c + t))        -(rx (bx + tx))
 *         (1 - E            ) (1 + E                ) (1 + E               )
 *>        ------------------------------------------------------------------) / 2],
 *                                         8
 */

    /* n10 */
    result[2] = log((
            ((1 + e1) * (1 - e2) * (1 - e3))/8 +
            ((1 - e1) * (1 + e2) * (1 + e3))/8) / 2);
    assert(abs(result[2] - LCFIT_COEFF(+,-,-,-,+,+)) < 1e-5);

/*
 *                -((b + c) r)        -(r (b - c + t))        -(rx (bx + tx))
 *          (1 + E            ) (1 + E                ) (1 - E               )
 *>    Log[(------------------------------------------------------------------ +
 *                                          8
 *
 *               -((b + c) r)        -(r (b - c + t))        -(rx (bx + tx))
 *         (1 - E            ) (1 - E                ) (1 + E               )
 *>        ------------------------------------------------------------------) / 2]}
 *                                         8
 */
    /* n11 */
    result[3] = log((
            ((1 + e1) * (1 + e2) * (1 - e3))/8 +
            ((1 - e1) * (1 - e2) * (1 + e3))/8) / 2);
    assert(abs(result[3] - LCFIT_COEFF(+,+,-,-,-,+)) < 1e-5);
    #undef LCFIT_COEFF
}

/**
 * Estimate `rx` such that m intersects with sampled point (dist_bl, pend_bl, l)
 */
double
lcfit_tripod_est_rx(const double dist_bl,
                    const double pend_bl,
                    const double l,
                    const tripod_bsm_t* m)
{
/*
                        ll00    -((b + c) r) - r (b - c + t)
                 1 - 8 E     + E
           Log[-(-------------------------------------------)]
                       -((b + c) r)    -(r (b - c + t))
                      E             + E
Out[88]= -(---------------------------------------------------)
                                 bx + tx
*/
    /* Divide by n00? */
    double log_num =   1 - 8 * exp(l) + exp(-((m->b + dist_bl) * m->r) - m->r * (m->b - dist_bl + m->t)),
           log_denom = exp(-((m->b + dist_bl) * m->r)) + exp(-(m->r * (m->b - dist_bl + m->t))),
           denom =     m->bx + pend_bl;

    /*fprintf(stderr, "EST_RX: (%f,%f,%f)\n", log_num, log_denom, denom);*/
    double res = -log(-log_num / log_denom) / denom;
    /*return res / m->n00;*/
    return res;
}

/* vim: set ts=4 sw=4 : */
