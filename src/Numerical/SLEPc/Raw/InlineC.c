
#include <slepceps.h>

#include <slepcsvd.h>

int inline_c_0_627c47c40bd5763762c2624b800a91b2067f705e(int c_inline_c_0, EPS * e_inline_c_1) {
return (EPSCreate(c_inline_c_0, e_inline_c_1));
}


int inline_c_1_d5381146cade6c83f39f0906c8d38eb61017d987(EPS e_inline_c_0, Mat matA_inline_c_1, Mat matB_inline_c_2) {
return (EPSSetOperators(e_inline_c_0,matA_inline_c_1,matB_inline_c_2));
}


int inline_c_2_af14208e5858c3b19af7c8457d8dd7e7c790c69d(EPS e_inline_c_0, int tyi_inline_c_1) {
return (EPSSetProblemType(e_inline_c_0,tyi_inline_c_1));
}


int inline_c_3_18857752baea722e654ddd286a9782fa8acef592(EPS e_inline_c_0) {
return (EPSSetUp(e_inline_c_0));
}


int inline_c_4_6e470584a4e9413041cd210cf52a32dd9770f171(EPS e_inline_c_0) {
return (EPSSolve(e_inline_c_0));
}


int inline_c_5_d4544c7568b86930020abfc910bb06e94b97ea2a(EPS e_inline_c_0, int * nconv_inline_c_1) {
return (EPSGetConverged(e_inline_c_0,nconv_inline_c_1));
}


int inline_c_6_fa24c877084ce3d1d89ac8e04086846720de66b8(EPS e_inline_c_0, int i_inline_c_1, PetscScalar * kr_inline_c_2, PetscScalar * ki_inline_c_3, Vec xr_inline_c_4, Vec xi_inline_c_5) {
return (EPSGetEigenpair(e_inline_c_0,i_inline_c_1,kr_inline_c_2,ki_inline_c_3,xr_inline_c_4,xi_inline_c_5));
}


int inline_c_7_84e7f276d467824f42ea951f4cb09dc0397c61b6(EPS e_inline_c_0, PetscBool * ish_inline_c_1) {
return (EPSIsHermitian(e_inline_c_0,ish_inline_c_1));
}


int inline_c_8_6ffee2e86e3f5efde4af33f5ef3e0dcacb2cb41d(EPS e_inline_c_0, PetscBool * isp_inline_c_1) {
return (EPSIsPositive(e_inline_c_0,isp_inline_c_1));
}


int inline_c_9_8df0aad0753605639f13960fb0560d60557e8224(EPS e_inline_c_0, int nevc_inline_c_1, int ncvc_inline_c_2, int mpdc_inline_c_3) {
return (EPSSetDimensions(e_inline_c_0,nevc_inline_c_1,ncvc_inline_c_2,mpdc_inline_c_3));
}


int inline_c_10_437a9b0ae9e33c00ea37214163e1e450a095a896(EPS e_inline_c_0, PetscReal smin_inline_c_1, PetscReal smax_inline_c_2) {
return (EPSSetInterval(e_inline_c_0,smin_inline_c_1,smax_inline_c_2));
}


int inline_c_11_e51ffaefd721fb6fa80f208103c82d9dc8cb8571(EPS * ep_inline_c_0) {
return (EPSDestroy(ep_inline_c_0));
}


int inline_c_12_d1427e6fb06ca008057fd49c65d204348a4799e0(EPS eps_inline_c_0, PetscViewer v_inline_c_1) {
return (EPSView(eps_inline_c_0,v_inline_c_1));
}


int inline_c_13_a937be1649c4fccb35ad7ef21b67861d7f5af123(int c_inline_c_0, SVD * s_inline_c_1) {
return (SVDCreate(c_inline_c_0,s_inline_c_1));
}


int inline_c_14_32a141c54ef09e454b90ae2bd5c849f38c916684(SVD s_inline_c_0, Mat matA_inline_c_1) {
return (SVDSetOperator(s_inline_c_0,matA_inline_c_1));
}


int inline_c_15_0ee0c1999d36c1cff94a808665a66cfcfa21ab37(SVD s_inline_c_0) {
return (SVDSolve(s_inline_c_0));
}


int inline_c_16_4cf2cc2f88989102f14326fefafdb753ce67d3fd(SVD s_inline_c_0, int * n_inline_c_1) {
return (SVDGetConverged(s_inline_c_0,n_inline_c_1));
}


int inline_c_17_62f9017dae2647812099fb5b20a63b401bfb7837(SVD s_inline_c_0, int i_inline_c_1, PetscReal * sig_inline_c_2, Vec * u_inline_c_3, Vec * v_inline_c_4) {
return (SVDGetSingularTriplet(s_inline_c_0,i_inline_c_1,sig_inline_c_2,u_inline_c_3,v_inline_c_4));
}


int inline_c_18_6f501ece586681111ec735841d3b62d92f03ed5f(SVD * sp_inline_c_0) {
return (SVDDestroy(sp_inline_c_0));
}


int inline_c_19_fbcafa3185c71387492f63f3249cd068258e028a(PetscBool * b_inline_c_0) {
return ( SlepcInitialized(b_inline_c_0) );
}


int inline_c_20_bb98414c7ac54d59a71717df0ecd0a647011bf8d() {
return ( SlepcInitializeNoArguments()  );
}


int inline_c_21_1670dcd2ad0bd0cfaec2c57cf8b93491c05407a4(int * ac_inline_c_0, char *** a_inline_c_1, char * o_inline_c_2, char * h_inline_c_3) {
return (SlepcInitialize(ac_inline_c_0, a_inline_c_1, o_inline_c_2, h_inline_c_3));
}


int inline_c_22_1496100e97d5e0da1581e3887cf5fcaea7278d0c() {
 SlepcFinalize(); 
}

