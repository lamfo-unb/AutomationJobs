#include <RcppEigen.h>
#include <Rcpp.h>
#include <RcppParallel.h>
// [[Rcpp::depends(RcppEigen)]]
// [[Rcpp::depends(RcppParallel)]]
using namespace RcppParallel;


// define both_non_NA(a, b)
inline bool both_non_NA(double a, double b) {
  return (!ISNAN(a) && !ISNAN(b));
}

struct IBSKernel : public Worker
{
  // source matrix
  const RMatrix<double> X;
  
  // source Variance Sigma2.f
  //const RVector<double> lambda;
  const RMatrix<double> sigma2f;
  
  // source Nugget Sigma2.n
  //const RVector<double> lambda;
  const RMatrix<double> sigma2n;
  
  // source vector
  //const RVector<double> lambda;
  const RMatrix<double> lambda;
  
  // destination matrix
  RMatrix<double> out;
  
  // initialize with source and destination
  IBSKernel(const Rcpp::NumericMatrix X, Rcpp::NumericMatrix out, 
            const Rcpp::NumericMatrix sigma2f,
            const Rcpp::NumericMatrix sigma2n, const Rcpp::NumericMatrix lambda) 
    : X(X), out(out), sigma2f(sigma2f), sigma2n(sigma2n), lambda(lambda) {}
  
  // calculate the IBS kernel of the range of elements requested
  void operator()(std::size_t begin, std::size_t end) {
    int p = X.ncol();
    for (std::size_t i = begin; i < end; i++) {
      for (std::size_t j = 0; j <= i; j++) {
        double val = 0;
        for (int k = 0; k < p; k++) {
          // double lam = lambda(k,0);
          double xi = X(i, k), xj = X(j, k);
          if (both_non_NA(xi, xj)) {
            val = val + (-0.5*std::pow((xi-xj)*lambda(0,0),2.0));
          }
        }
        if (i == j){
          out(j, i) = out(i, j) = sigma2f(0,0)*std::exp(val) + sigma2n(0,0);
        }
        else out(j, i) = out(i, j) = sigma2f(0,0)*std::exp(val);
      }
    }
  }
};

// [[Rcpp::export]]
Rcpp::NumericMatrix IBS_kernel_C_parallel(Rcpp::NumericMatrix X, 
                                          Rcpp::NumericMatrix sigma2f,
                                          Rcpp::NumericMatrix sigma2n,
                                          Rcpp::NumericMatrix lambdaVec) {
  
  // Rcpp::NumericMatrix outFull(X.nrow(), X.nrow());
  // allocate the output matrix
  Rcpp::NumericMatrix out(X.nrow(), X.nrow());
  
  // IBSKernel functor (pass input and output matrixes)
  IBSKernel ibskernel(X, out, sigma2f, sigma2n, lambdaVec);
  
  // call parallelFor to do the work
  parallelFor(0, X.nrow(), ibskernel);
  
  // return the output matrix
  return out;
}


// struct IBSKernel : public Worker
// {
//   // source matrix
//   const RMatrix<double> X;
//   
//   // source vector
//   //const RVector<double> lambda;
//   const RMatrix<double> lambda;
// 
//   // destination matrix
//   RMatrix<double> out;
//   
//   // initialize with source and destination
//   IBSKernel(const Rcpp::NumericMatrix X, Rcpp::NumericMatrix out, const Rcpp::NumericMatrix lambda) 
//     : X(X), out(out), lambda(lambda) {}
//   
//   // calculate the IBS kernel of the range of elements requested
//   void operator()(std::size_t begin, std::size_t end) {
//     int p = X.ncol();
//     for (std::size_t i = begin; i < end; i++) {
//       for (std::size_t j = 0; j < i; j++) {
//         double val = 0;
//         for (int k = 0; k < p; k++) {
//           double lam = lambda(k,0);
//           double xi = X(i, k), xj = X(j, k);
//           if (both_non_NA(xi, xj)) {
//             val = val + (-0.5*std::pow((xi-xj)/lam,2.0));
//           }
//         }
//         out(j, i) = out(i, j) = val;
//       }
//     }
//   }
// };
// 
// // [[Rcpp::export]]
// Rcpp::NumericMatrix IBS_kernel_C_parallel(Rcpp::NumericMatrix X, Rcpp::NumericMatrix lambdaVec) {
//   
//   Rcpp::NumericMatrix outFull(X.nrow(), X.nrow());
//     // allocate the output matrix
//     Rcpp::NumericMatrix out(X.nrow(), X.nrow());
//     
//     // IBSKernel functor (pass input and output matrixes)
//     IBSKernel ibskernel(X, out, lambdaVec);
//     
//     // call parallelFor to do the work
//     parallelFor(0, X.nrow(), ibskernel);
//     
//   // return the output matrix
//   return out;
// }



// This Kernel uses theta = .5/lambda^2 to avoid problems with lambda = 0
// Create the Kernel matrix
// @param datMat  Matrix with the data
// @param function Kernel Function
// @param parms vector of parameters fot the kernel
// @return Kernel Matrix
// [[Rcpp::export]]
Eigen::MatrixXd KernelMatrix2(Eigen::MatrixXd datMat1,
                              Eigen::MatrixXd datMat2, 
                              double sigma2f,
                              double sigma2n,
                              Eigen::RowVectorXd theta){
  //Get the number of rows
  unsigned int rows1=datMat1.rows();
  unsigned int rows2=datMat2.rows();
  unsigned int nelements = theta.size();
  //Initialize Identity matrix
  Eigen::MatrixXd identity = Eigen::MatrixXd::Identity(rows1,rows2);
  //Initialize the matrix
  Eigen::MatrixXd matKernel = Eigen::MatrixXd::Zero(rows1,rows2);
  for(unsigned int c1=0;c1<rows1;c1++){
    for(unsigned int c2=0;c2<=c1;c2++){
      double val = 0.0;
      //First column with variables
      Eigen::RowVectorXd vec1 = datMat1.row(c1);
      //Second column with variables
      Eigen::RowVectorXd vec2 = datMat2.row(c2);
      for(unsigned int p=0;p<nelements;p++){
        //Calculate the kernel value
        val = val + (-0.5*std::pow((vec1(p)-vec2(p))*theta(p),2.0));
      }
      //Store the kernel value
      matKernel(c2,c1) = matKernel(c1,c2) = sigma2f*std::exp(val);
    }
  }
  matKernel = matKernel + identity*sigma2n;
  return(matKernel);
}


// Create the Kernel matrix
// @param datMat  Matrix with the data
// @param function Kernel Function
// @param parms vector of parameters fot the kernel
// @return Kernel Matrix
// [[Rcpp::export]]
Eigen::MatrixXd KernelMatrix(Eigen::MatrixXd datMat, Eigen::RowVectorXd lambda, double sigma2f){
  //Get the number of rows
  unsigned int rows=datMat.rows();
  unsigned int nelements = lambda.size();
  //Initialize the matriz
  Eigen::MatrixXd matKernel = Eigen::MatrixXd::Zero(rows,rows);
  for(unsigned int c1=0;c1<rows;c1++){
    for(unsigned int c2=c1;c2<rows;c2++){
      double val = 0.0;
      //First column with variables
      Eigen::RowVectorXd vec1 = datMat.row(c1);
      //Second column with variables
      Eigen::RowVectorXd vec2 = datMat.row(c2);
      for(unsigned int p=0;p<nelements;p++){
        //Calculate the kernel value
        val = val + (-0.5*std::pow((vec1(p)-vec2(p))/lambda(p),2.0));
      }
      //Store the kernel value
      matKernel(c1,c2)=matKernel(c2,c1)=sigma2f*std::exp(val);
    }
  }
  
  return(matKernel);
}

// Create the Kernel matrix
// @param datMat  Matrix with the data
// @param function Kernel Function
// @param parms vector of parameters fot the kernel
// @return Kernel Matrix
// [[Rcpp::export]]
Eigen::MatrixXd KernelMatrixXX(Eigen::MatrixXd datMat1, Eigen::MatrixXd datMat2, Eigen::RowVectorXd lambda, double sigma2f){
  //Get the number of rows
  unsigned int rows1=datMat1.rows();
  unsigned int rows2=datMat2.rows();
  unsigned int nelements = lambda.size();
  //Initialize the matriz
  Eigen::MatrixXd matKernel = Eigen::MatrixXd::Zero(rows1,rows2);
  for(unsigned int c1=0;c1<rows1;c1++){
    for(unsigned int c2=0;c2<rows2;c2++){
      double val = 0.0;
      //First column with variables
      Eigen::RowVectorXd vec1 = datMat1.row(c1);
      //Second column with variables
      Eigen::RowVectorXd vec2 = datMat2.row(c2);
      for(unsigned int p=0;p<nelements;p++){
        //Calculate the kernel value
        val = val + (-0.5*std::pow((vec1(p)-vec2(p))/lambda(p),2.0));
      }
      //Store the kernel value
      matKernel(c1,c2)=sigma2f*std::exp(val);
    }
  }
  
  return(matKernel);
}


// Create the Kernel matrix
// @param datMat  Matrix with the data
// @param function Kernel Function
// @param parms vector of parameters fot the kernel
// @return Kernel Matrix
// [[Rcpp::export]]
Eigen::RowVectorXd KernelVectorXx(Eigen::MatrixXd datMat, Eigen::RowVectorXd datVec, Eigen::RowVectorXd lambda, double sigma2f){
  //Get the number of rows
  unsigned int rows=datMat.rows();
  unsigned int nelements = lambda.size();
  //Initialize the matriz
  Eigen::RowVectorXd vecKernel = Eigen::RowVectorXd::Zero(rows);
  for(unsigned int c1=0;c1<rows;c1++){
      double val = 0.0;
      for(unsigned int p=0;p<nelements;p++){
        //First column with variables
        Eigen::RowVectorXd vec1 = datMat.row(c1);
        //Calculate the kernel value
        val = val + (-0.5*std::pow((vec1(p)-datVec(p))/lambda(p),2.0));
      }
      //Store the kernel value
      vecKernel(c1)=sigma2f*std::exp(val);
  }
  return(vecKernel);
}

// Create the Kernel matrix
// @param datMat  Matrix with the data
// @param function Kernel Function
// @param parms vector of parameters fot the kernel
// @return Kernel Matrix
// [[Rcpp::export]]
double KernelVectorxx(Eigen::RowVectorXd datVec1, Eigen::RowVectorXd datVec2, Eigen::RowVectorXd lambda, double sigma2f){
  //Get the number of rows
  unsigned int nelements = lambda.size();
  //Initialize the matriz
  double dblKernel=0.0;
  double val = 0.0;
  for(unsigned int p=0;p<nelements;p++){
    //Calculate the kernel value
    val = val + (-0.5*std::pow((datVec1(p)-datVec2(p))/lambda(p),2.0));
  }
  //Store the kernel value
  dblKernel=sigma2f*std::exp(val);
  return(dblKernel);
}