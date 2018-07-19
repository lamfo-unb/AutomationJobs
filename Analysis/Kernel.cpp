#include <RcppEigen.h>
// [[Rcpp::depends(RcppEigen)]]



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