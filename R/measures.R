#' @title Mesh volume
#' @description Computes the volume bounded by a mesh.
#'
#' @param mesh a mesh given either as a list containing (at least) the two fields 
#'   \code{vertices} (numeric matrix with three columns) and \code{faces} (integer 
#'   matrix or list of integer vectors), otherwise as a \strong{rgl} mesh (i.e. a 
#'   \code{mesh3d} object)
#'
#' @return A number, the volume bounded by the mesh.
#' @export
#'
#' @examples
#' library(MeshesTools)
#' R <- 4; r <- 2
#' mesh <- torusMesh(R, r)
#' meshVolume(mesh)
#' # true volume of the torus: 
#' 2 * pi^2 * R * r^2
meshVolume <- function(mesh){
  if(inherits(mesh, "mesh3d")) {
    vft  <- getVFT(mesh, beforeCheck = TRUE)
    mesh <- vft[["rmesh"]]
  }
  vertices <- mesh[["vertices"]]
  faces    <- mesh[["faces"]]
  checkedMesh <- checkMesh(vertices, faces, gmp = FALSE, aslist = TRUE)
  vertices         <- checkedMesh[["vertices"]]
  faces            <- checkedMesh[["faces"]]
  isTriangle <- checkedMesh[["isTriangle"]]
  rmesh <- list("vertices" = vertices, "faces" = faces)
  meshVolumeEK(rmesh, !isTriangle)
}

#' @title Mesh area
#' @description Computes the surface area a mesh.
#'
#' @param mesh a mesh given either as a list containing (at least) the two fields 
#'   \code{vertices} (numeric matrix with three columns) and \code{faces} (integer 
#'   matrix or list of integer vectors), otherwise as a \strong{rgl} mesh (i.e. a 
#'   \code{mesh3d} object)
#'
#' @return A number, the surface area of the mesh.
#' @export
#'
#' @examples
#' library(MeshesTools)
#' R <- 4; r <- 2
#' mesh <- torusMesh(R, r)
#' meshArea(mesh)
#' # true area of the torus: 
#' 4 * pi^2 * R * r
meshArea <- function(mesh){
  if(inherits(mesh, "mesh3d")) {
    vft  <- getVFT(mesh, beforeCheck = TRUE)
    mesh <- vft[["rmesh"]]
  }
  vertices <- mesh[["vertices"]]
  faces    <- mesh[["faces"]]
  checkedMesh <- checkMesh(vertices, faces, gmp = FALSE, aslist = TRUE)
  vertices         <- checkedMesh[["vertices"]]
  faces            <- checkedMesh[["faces"]]
  isTriangle <- checkedMesh[["isTriangle"]]
  rmesh <- list("vertices" = vertices, "faces" = faces)
  meshAreaEK(rmesh, !isTriangle)
}

#' @title Mesh centroid
#' @description Computes the centroid of a closed mesh.
#'
#' @param mesh a mesh given either as a list containing (at least) the two fields 
#'   \code{vertices} (numeric matrix with three columns) and \code{faces} (integer 
#'   matrix or list of integer vectors), otherwise as a \strong{rgl} mesh (i.e. a 
#'   \code{mesh3d} object)
#'
#' @return The centroid of the mesh given as a numeric vector.
#' @export
#'
#' @examples
#' \donttest{library(MeshesTools)
#' mesh <- cyclideMesh(a = 97, c = 32, mu = 57)
#' meshCentroid(mesh)}
meshCentroid <- function(mesh){
  if(inherits(mesh, "mesh3d")) {
    vft  <- getVFT(mesh, beforeCheck = TRUE)
    mesh <- vft[["rmesh"]]
  }
  vertices <- mesh[["vertices"]]
  faces    <- mesh[["faces"]]
  checkedMesh <- checkMesh(vertices, faces, gmp = FALSE, aslist = TRUE)
  vertices         <- checkedMesh[["vertices"]]
  faces            <- checkedMesh[["faces"]]
  isTriangle <- checkedMesh[["isTriangle"]]
  rmesh <- list("vertices" = vertices, "faces" = faces)
  meshCentroidEK(rmesh, !isTriangle)
}
