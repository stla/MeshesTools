#ifndef _HEADER_
#include "MeshesTools.h"
#endif

// [[Rcpp::export]]
double meshVolumeEK(const Rcpp::List rmesh, const bool triangulate) {
  Message("\u2014 Processing mesh...");
  EMesh3 mesh = makeSurfMesh<EMesh3, EPoint3>(rmesh, true, triangulate, true);
  Message("... done.\n");
  if(PMP::does_self_intersect(mesh)) {
    Rcpp::stop("The mesh self-intersects.");
  }
  const EK::FT vol = PMP::volume(mesh);
  return CGAL::to_double<EK::FT>(vol);
}

// [[Rcpp::export]]
double meshAreaEK(const Rcpp::List rmesh, const bool triangulate) {
  Message("\u2014 Processing mesh...");
  EMesh3 mesh = makeSurfMesh<EMesh3, EPoint3>(rmesh, true, triangulate, false);
  Message("... done.\n");
  if(PMP::does_self_intersect(mesh)) {
    Rcpp::stop("The mesh self-intersects.");
  }
  const EK::FT ar = PMP::area(mesh);
  return CGAL::to_double<EK::FT>(ar);
}

// [[Rcpp::export]]
Rcpp::NumericVector meshCentroidEK(
    const Rcpp::List rmesh, const bool triangulate
) {
  Message("\u2014 Processing mesh...");
  EMesh3 mesh = makeSurfMesh<EMesh3, EPoint3>(rmesh, true, triangulate, true);
  Message("... done.\n");
  const EPoint3 centroid = PMP::centroid(mesh);
  Rcpp::NumericVector out(3);
  out(0) = CGAL::to_double<EK::FT>(centroid.x());
  out(1) = CGAL::to_double<EK::FT>(centroid.y());
  out(2) = CGAL::to_double<EK::FT>(centroid.z());
  return out;
}
