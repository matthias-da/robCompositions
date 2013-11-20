#include <R.h>
#include <math.h>

void da(double *matOrig, double *matImp, int *dims, double *rowDists, double *distance) {
	int rows = dims[0];
	int cols = dims[1];
        /* double result; */
	/* int N = rows * cols;	 */
	int i, j, k;
	/* float erg; */

	
	/*Fuer alle Zeilen */
	for (i=0; i < rows; i++) {
		rowDists[i] = 0.0;
		/* ueber die entsprechenden Spalten */
		for (j=(i*cols); j < (i*cols)+cols-1; j++) {
		  for (k=j+1; k < (i*cols)+cols; k++){
			rowDists[i] = rowDists[i] + pow((log(matOrig[j]/matOrig[k]) - log(matImp[j]/matImp[k])),2);
		  }
		}
		/* Gesamtdistanz aufsummieren */
		distance[0] = distance[0] + sqrt(rowDists[i]/cols);
	} 
        /*result = distance[rows];
        return result; */
        /* return distance[rows];  das funkt leider nicht, brauche nur einen Wert zurueckgeben! */
}
