#ifndef _HEADER_
#include "MeshesTools.h"
#endif

// [[Rcpp::export]]
double meshVolumeEK(const Rcpp::List rmesh, const bool triangulate) {
  Message("\u2014 Processing mesh...");
  EMesh3 mesh = makeSurfMesh<EMesh3, EPoint3>(rmesh, true, triangulate, true);
  Message("... done.\n");
  const EK::FT vol = PMP::volume(mesh);
  return CGAL::to_double<EK::FT>(vol);
}

// [[Rcpp::export]]
double meshAreaEK(const Rcpp::List rmesh, const bool triangulate) {
  Message("\u2014 Processing mesh...");
  EMesh3 mesh = makeSurfMesh<EMesh3, EPoint3>(rmesh, true, triangulate, false);
  Message("... done.\n");
  const EK::FT ar = PMP::area(mesh);
  return CGAL::to_double<EK::FT>(ar);
}