/* http://www.gnu.org/software/gsl/manual/html_node/Nonlinear-Least_002dSquares-Fitting.html */
/**
 * \file lcfit.h
 * \brief lcfit C-API
 *
 * Fit the binary-symmetric model
 */
#ifndef LCFIT_H
#define LCFIT_H

#include <stdlib.h>

#ifdef __cplusplus
extern "C" {
#endif

/** \brief Fit of the binary-symmetric model */
typedef struct {
    /** \brief Number of constant sites */
    double c;
    /** \brief Number of mutated sites */
    double m;
    /** \brief Rate of mutation */
    double r;
    /** \brief Minimum branch length */
    double b;
} bsm_t;

/** \brief Default initial conditions */
const extern bsm_t DEFAULT_INIT;

/** \brief The log likelihood for the Binary Symmetric Model at a given branch length
 *
 *  \f[
 *    l[i] = c*log((1+\exp(-r*(t[i]+b)))/2)+m*log((1-\exp(-r*(t[i]+b)))/2)
 *  \f]
 *
 *  \param t Branch length
 *  \param m Model
 *  \return Log-likelihood under \c m
 */
double lcfit_bsm_log_like(double t, const bsm_t* m);

/** \brief The ML branch length for model \c m
 *
 * \param m Model
 * \return The maximum-likelihood branch length under \c m
 */
double lcfit_bsm_ml_t(const bsm_t* m);

/** \brief Determine a scale factor for \c m to intersect with \f$(t, l)\f$
 *
 * Generates a scaling parameter for the values \c m.c and \c m.m to obtain
 * log-likelihood value \c l at branch-length \c t, keeping \c m.r and \c m.b fixed.
 *
 * \param t Branch length
 * \param l Log-likelihood
 * \param m Model
 * \return A value \c v, such that the likelihood curve for the model <c>{c / v, m / v, r, b}</c> intersects with \f$(t, l)\f$
 */
double lcfit_bsm_scale_factor(const double t, const double l, const bsm_t* m);

/** \brief Fit the BSM
 *
 * \param n Number of observations in \c t and \c l
 * \param t Branch length
 * \param l Log-likelihood values at \c t
 * \param m Initial conditions for the model.
 * Combine #DEFAULT_INIT and #lcfit_bsm_scale_factor for reasonable starting conditions.
 */
int lcfit_fit_bsm(const size_t n, const double* t, const double* l, bsm_t* m);

#ifdef __cplusplus
} // extern "C"
#endif

#endif // LCFIT_H
