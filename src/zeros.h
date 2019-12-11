#ifndef ZEROS_JACK_FRUSCIANTE_1903
#define ZEROS_JACK_FRUSCIANTE_1903

#include <math.h>
#include <Eigen/Dense>
#include <iostream>
#include <iterator>
#include <vector>
#include <algorithm>
#include <functional>

/*! \file
  @brief Bayesian treatment of zeros observations
  @details The main aim is to correct count zeros observations using a Bayesian
            prior in order to be able to compute the clr-transformation. See the note
            for further informations.
  @note
    Reference: "Bayesian-multiplicative treatment of count zeros in compositional data sets"
    Authors: Josep-Antoni Martin-Fernandez, Karel Hron, Matthias Templ,
              Peter Filzmoser and Javier Palarea-Albaladejo
    Periodical: Statistical Modelling 2015; 15(2): 134–158
*/

namespace help
{
  std::vector<double>
  divide
  (const std::vector<double> & vect, const double & D);

  double
  sum
  (const std::vector<double> & vect);

  std::vector<double>
  uniform
  (const unsigned int & n);

  double
  geom_mean
  (const std::vector<double> & vect);
}

/*!
@brief Different possible priors that can be used.
@details They differ in the value of the strength [s]
        associated to the prior information. [D] is the dimension of the vector where to apply BM.

        DEFAULT value select SQ if sqrt(n)>D otherwise BAYES_LAPLACE.
*/

enum class PRIOR
{
   PERKS,          /**< s = 1 */ //t = 1/D
   JEFFREYS,       /**< s = D/2 */  // t = 1/D
   BAYES_LAPLACE,  /**< s = D  */ // t = 1/D
   SQ,             /**< s = sqrt(n) */ // t = 1/D
   DEFAULT         /**< default */
};


/*!
@brief	Apply Bayesian count zero-replacement algorithm
@details Read one row and apply Bayesian-multiplicative treatment of count zeros if necessary.
@see BM()
@param numbers Vector where the result is stored.
@param data Data to be processed.
@param p Prior setting.
@see PRIOR
*/
void
BM
(std::vector<double> & numbers,
 const Eigen::Block<Eigen::Map<Eigen::Matrix<double, -1, -1>, 0, Eigen::Stride<0, 0> >, 1, -1, false> & data,
 PRIOR p = PRIOR::DEFAULT);

 /*!
 @brief	Apply Bayesian count zero-replacement algorithm (general version)
 @details Read one row and apply Bayesian-multiplicative treatment of count zeros if necessary.
 @see BM()
 @param numbers Vector where the result is stored.
 @param data Data to be processed.
 @param p Prior setting.
 @param is_strength_inverse Boolean for the strength of the prior.
 @see PRIOR
 */
void
BM
(std::vector<double> & numbers,
  const Eigen::Block<Eigen::Map<Eigen::Matrix<double, -1, -1>, 0, Eigen::Stride<0, 0> >, 1, -1, false> & data,
  const double & s, const bool is_strength_inverse = false);

#endif //ZEROS_JACK_FRUSCIANTE_1903
