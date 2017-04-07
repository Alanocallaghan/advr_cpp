#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
bool allC(LogicalVector x) {
	int n = x.size();

	for (int i = 0; i < n; i++) {
		if (!x[i]) return false;
	}
	return true;
}

// [[Rcpp::export]]
bool anyC(LogicalVector x) {
	int n = x.size();

	for (int i = 0; i < n; i++) {
		if (x[i]) return true;
	}
	return false;
}

// [[Rcpp::export]]
NumericVector cumprodC(NumericVector x) {
	int n = x.size();
	NumericVector out(n);
	out[0] = 1;
	for (int i = 1; i < n; i++) {
		out[i] = out[i - 1] * x[i];
	}
	return out;
}

// [[Rcpp::export]]
NumericVector cumminC(NumericVector x) {
	int n = x.size();
	NumericVector out(n);
	double min = x[0];
	for (int i = 0; i < n; i++) {
		if (x[i] < min) min = x[i];
		out[i] = min;
	}
	return out;
}

// [[Rcpp::export]]
NumericVector cummaxC(NumericVector x) {
	int n = x.size();
	NumericVector out(n);
	double max = x[0];
	for (int i = 0; i < n; i++) {
		if (x[i] > max) max = x[i];
		out[i] = max;
	}
	return out;
}

double minC(NumericVector x) {
	int n = x.size();
	double min = x[0];
	for (int i = 0; i < n; i++) {
		if (x[i] < min) min = x[i];
	}
	return min;
}

double maxC(NumericVector x) {
	int n = x.size();
	double max = x[0];
	for (int i = 0; i < n; i++) {
		if (x[i] > max) max = x[i];
	}
	return max;
}

// [[Rcpp::export]]
NumericVector rangeC(NumericVector x) {
	NumericVector out(2);
	out[0] = minC(x);
	out[1] = maxC(x);
	return out;
}

// [[Rcpp::export]]
double varC(NumericVector x) {
	int n = x.size();
	double sum = 0, sumsq = 0;

	for (int i = 0; i < n; i++) {
		sum +=x[i];
		sumsq += pow(x[i], 2);
	}
	return (sumsq - pow(sum, 2) / n) / n - 1;
}
