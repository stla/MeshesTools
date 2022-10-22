#ifndef _HEADER_
#include "MeshesTools.h"
#endif

// [[Rcpp::export]]
Rcpp::List clipMeshEK(const Rcpp::List rmesh,
                      const Rcpp::List rclipper,
                      const bool clipVolume,
                      const bool triangulate1,
                      const bool triangulate2,
                      const bool normals) {
  Message("\u2014 Processing mesh to be clipped...");
  EMesh3 mesh = makeSurfMesh<EMesh3, EPoint3>(rmesh, true, triangulate1, false);
  bool doNotModify = false;
  if(!clipVolume) {
    doNotModify = true;
  } else {
    if(PMP::does_self_intersect(mesh)) {
      Rcpp::stop("The mesh self-intersects.");
    }
  }
  Message("... done.\n");
  Message("\u2014 Processing the clipping mesh...");
  EMesh3 clipper = 
    makeSurfMesh<EMesh3, EPoint3>(rclipper, true, triangulate2, true);
  if(PMP::does_self_intersect(clipper)) {
    Rcpp::stop("The clipping mesh self-intersects.");
  }
  Message("... done.\n");
  Message("\u2014 Performing clipping...");
  const bool clipping = PMP::clip(
    mesh, clipper, PMP::parameters::clip_volume(clipVolume),
    PMP::parameters::clip_volume(clipVolume).do_not_modify(doNotModify)
  );
  if(!clipping) {
    Rcpp::stop("Clipping has failed.");
  }
  mesh.collect_garbage();
  Message("... done.\n");
  Rcpp::List routmesh = RSurfTEKMesh(mesh, normals);
  return routmesh;
}
