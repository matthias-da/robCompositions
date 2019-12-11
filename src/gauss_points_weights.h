//
// Created by Alessia Di Blasi on 17/04/18.
//

#ifndef GAUSS_POINTS_WEIGHTS_CPP_GAUSS_POINTS_WEIGHTS_HPP
#define GAUSS_POINTS_WEIGHTS_CPP_GAUSS_POINTS_WEIGHTS_HPP
#include <vector>
/*! \file
  @brief Compute Gauss points and weights to integrate splines
*/
namespace integral{
    void grule(std::vector<double> &bp, std::vector<double> &wf);
}

#endif //GAUSS_POINTS_WEIGHTS_CPP_GAUSS_POINTS_WEIGHTS_HPP
