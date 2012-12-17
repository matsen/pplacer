#ifndef LCFIT_TRIPOD_H_
#define LCFIT_TRIPOD_H_

#include <stdlib.h>

extern const size_t TRIPOD_BSM_NPARAM;
extern const size_t TRIPOD_BSM_NVARYING;

/** Fit of the binary symmetric model for a tripod */
typedef struct  {
    double n00,
           n01,
           n10,
           n11,
           r,
           b,
           t,
           rx,
           bx;
} tripod_bsm_t;

double
lcfit_tripod_rescale(const double, const double, const double, tripod_bsm_t*);
double
lcfit_tripod_ll(const double, const double, const tripod_bsm_t*);
double*
lcfit_tripod_jacobian(const double, const double, const tripod_bsm_t*);
int
lcfit_tripod_fit_bsm(const size_t, const double*, const double*, const double*, tripod_bsm_t*);

double*
array_of_tripod_bsm(const tripod_bsm_t*);

/* vim: set ts=4 sw=4 : */
#endif /* LCFIT_TRIPOD_H_ */
