/*

  Class to summarise connectedness
  TODO:  template for dimensions (i.e. possibility for std::array for default grid_resolution)

*/

#ifndef CONNECTEDNESS_H
#define CONNECTEDNESS_H

#include <Rcpp.h>

template<bool T_checking>
class Connectedness
{
public:
  Connectedness() = delete;
  Connectedness(const int resolution, const Rcpp::NumericMatrix grid) :
    m_resolution(resolution), m_grid(grid)
  {
    if constexpr (T_checking)
    {
      if(m_grid.rows() != m_resolution*2L+1L) Rcpp::stop("Wrong dims in grid");
      if(m_grid.cols() != m_resolution*2L+1L) Rcpp::stop("Wrong dims in grid");
    }
  }

  // TODO: incorporate non-included points
  Rcpp::NumericVector get_connectedness(const int rows, const int cols,
                        const int target, const int resolution,
                        const Rcpp::IntegerVector points_outside,
                        const Rcpp::NumericMatrix points) const
  {
    const int npatches = points_outside.size();
    Rcpp::NumericVector output (npatches);

    Rcpp::NumericVector rn (npatches);
    Rcpp::NumericVector ra (npatches);

    for (int tr=resolution; tr<(resolution+rows); ++tr)
    {
      for (int tc=resolution; tc<(resolution+cols); ++tc)
      {
        if (points(tr,tc) == target)
        {
          for(int p=0L; p<npatches; ++p)
          {
            rn[p] = points_outside[p];
            ra[p] = 0.0;
          }

          for (int roff = -resolution; roff <= resolution ; ++roff)
          {
            for (int coff = -resolution; coff <= resolution; ++coff)
            {
              if (roff != 0L && coff != 0L)
              {
                if constexpr (T_checking)
                {
                  if (tr+roff >= points.rows()) Rcpp::stop("tr+roff exceeds rows");
                  if (tc+coff >= points.cols())
                    {
                      Rcpp::Rcout << "tc: " << tc << ", coff: " << coff << ", cols: " << points.cols() << "\n";
                      Rcpp::stop("tc+coff exceeds cols");
                    }
                  if (roff+resolution >= m_grid.rows()) Rcpp::stop("roff+resolution exceeds rows");
                  if (coff+resolution >= m_grid.cols()) Rcpp::stop("roff+resolution exceeds cols");
                }

                const int p = points(tr+roff, tc+coff);
                rn[p]++;
                ra[p] += (m_grid(roff+resolution, coff+resolution) - ra[p]) / static_cast<double>(rn[p]);
              }
            }
          }

          for(int p=0L; p<npatches; ++p)
          {
            output[p] += ra[p];
          }
        }
      }
    }

    return output;
  }

private:
  const int m_resolution;
  const Rcpp::NumericMatrix m_grid;

};

#endif // CONNECTEDNESS_H
