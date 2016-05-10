var i,l = 0;
var A = [];

for (i=1; i<=21; i++){
	A[i-1] = i;
}

l = A.length;

function print (B){
	y = B.length;
    for (i=0; i<y/2; i++){
    	console.log(B[i]);
    	console.log(B[y-i-1]);
    }
}

if (l%2==1){
	print(A);
	console.log(A[parseInt(l/2)]);
	}
	else{
	print(A)
	}
