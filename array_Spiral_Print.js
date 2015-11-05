var i,l,y = 0;
var A = [];

for (i=1; i<=21; i++){
	A[i-1] = i;
}

l = A.length;

function print (y){
    for (i=1; i<=y/2; i++){
    	console.log(A[i-1]);
    	console.log(A[y-i]);
    }
}

if (l%2==1){
	print(l);
	a = Math.ceil(l/2)
	console.log(a);
	}
	else{
	print(l)
	}
