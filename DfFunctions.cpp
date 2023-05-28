#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
IntegerVector df_dim(DataFrame df) {
  IntegerVector dim(2);
  dim[0] = df.nrows();
  dim[1] = df.size();
  
  return dim;
}

void summary_df(DataFrame df) {
  CharacterVector names = df.names();
  List cols = df;
  int numCols = df.size();
  int numRows = df.nrows();
  
  Rcout << "Rows: " << numRows << ", Columns: " << numCols << "\n";
  
  for (int i = 0; i < numCols; i++) {
    Rcout << names[i] << ":\n";
    
    // Check the type of column
    switch (TYPEOF(cols[i])) {
    case INTSXP:
    {
      IntegerVector col = cols[i];
      int minValue = Rcpp::min(col);
      int maxValue = Rcpp::max(col);
      double meanValue = Rcpp::mean(col);
      
      Rcout << "  Min: " << minValue << ", Max: " << maxValue << ", Mean: " << meanValue << "\n";
      break;
    }
    case REALSXP:
    {
      NumericVector col = cols[i];
      double minValue = Rcpp::min(col);
      double maxValue = Rcpp::max(col);
      double meanValue = Rcpp::mean(col);
      
      Rcout << "  Min: " << minValue << ", Max: " << maxValue << ", Mean: " << meanValue << "\n";
      break;
    }
    case STRSXP:
    {
      CharacterVector col = cols[i];
      int uniqueValues = Rcpp::unique(col).size();
      
      Rcout << "  Unique values: " << uniqueValues << "\n";
      break;
    }
    default:
      Rcout << "  Unsupported column type\n";
    }
  }
}

CharacterVector col_names(DataFrame df) {
  return df.names();
}

RCPP_MODULE(DataFrameInfo) {
  function("summary_df", &summary_df, "Show summary of DataFrame");
  function("col_names", &col_names, "Get column names of DataFrame");
  function("df_dim", &df_dim, "Get dimensions of DataFrame");
}