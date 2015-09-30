slepc-hs - Haskell bindings for SLEPc 
(Scalable Library for Eigenvalue Problem Computations)
Copyright (c) 2015, Marco Zocca ( zocca at marco dot gmail at com )



## Introduction

The [SLEPc](http://slepc.upv.es/) library provides a standardized interface for eigenvalue problems of various kind (linear, nonlinear, symmetric or not, real- or complex-valued), and several solvers, along with a rich option set. It is based on the [PETSc](http://www.mcs.anl.gov/petsc/) architecture that comprises distributed numerical data-structures and algorithms, which in turn is based on MPI, to support parallelized computations.



## Motivation

Eigenproblems, i.e. equations of the form `A x = s x`, where `A` is an operator (e.g. a matrix, in the linear, finite-dimensional case), `x` an eigenvector and `s` a number (the corresponding eigenvalue), are ubiquitous in science and engineering; their interpretation is fairly intuitive: the action of `A` on `x` produces another vector `s x` which is simply proportional to the original one. In other words, eigenvectors form a set of "natural directions" of the operator `a`, a natural basis in which to express it (and the set of eigenvalues `s` can be seen as the amplitude coefficients of each element of this basis). 

"Projecting" a phenomenon onto one or more of the eigenvectors of the operator that generates it highlights its symmetries.

For enlightening examples and all the relevant theory, see e.g. the "modal expansion" in quantum mechanics/acoustics/photonics, or Principal Component Analysis in statistics, etc. 




## Vision

The grand aim of this library is manifold: to bring together functional programming and high-performance numerical computing, and in particular to bring concepts and tools from the former into the practice of the latter.

It is your humble author's opinion that dynamic languages do not completely address the needs of scientific programming: ease of design, of verification and of collaboration. Functional composition of sub-programs and rich, static types are the missing link between programmer efficiency and program expressiveness.
 




## Installation

* First of all, working installations of PETSc and SLEPc (and Haskell, of course) are required, in this order. Please refer to the respective pages for detailed instructions.

* The Haskell side is based on `inline-c`, which can be obtained from Hackage via `cabal install inline-c`.

* Once everything is setup simply run `make` from within the slepc-hs root directory, and at the end of the process you should find yourself within a GHCi interactive session.




## Notes

* The library is being developed on a Unix, with the Haskell compiler GHC 7.8.4, on top of PETSc 3.6.0 and SLEPc 3.6.1 using MPICH (installed via the PETSc installation process).

* The PETSc and SLEPc architecture directory flags within the makefile are hardcoded ("arch-darwin-c-debug"), but they depend on the actual configuration parameters you supplied when installing those libraries ; modify them to suit your case.





## License

slepc-hs is free software: you can redistribute it and/or modify it under the
terms of version 3 of the GNU Lesser General Public License as published by
the Free Software Foundation.
slepc-hs is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for
more details.
You should have received a copy of the GNU Lesser General Public License
along with slepc-hs. If not, see <http://www.gnu.org/licenses/>.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -